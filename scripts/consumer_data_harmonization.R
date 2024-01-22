## ------------------------------------------ ##
#       Marine CND -- Data Harmonization
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Sites: SBC, CCE, Coastal CA, FCE, MCR, NGA, PIE, VCR, 

# Data Type: Consumer

# Purpose:
## Absorbs all raw data files and combines them
## Finishes with a harmonized data file in long format

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

# Create necessary sub-folder(s)
dir.create(path = file.path("tier0"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data", "consumer"), showWarnings = F)

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Identify raw data files
# For example, here I'm pulling all the SBC consumer data from Google Drive
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")) %>%
  dplyr::filter(name %in% c("Annual_All_Species_Biomass_at_transect_20230814.csv",
                            "IV_EC_talitrid_population.csv"))

raw_FCE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1BSQSXEbjgkSBJVN0p9CxhjVmfiv82U1t")) %>%
  dplyr::filter(name %in% c("MAP_years1thru19.csv"))

raw_VCR_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1PoVGjZoE_Dlr93dt45LRp4P2Jjuez94l")) %>%
  dplyr::filter(name %in% c("VCR14232_2.csv"))

raw_coastal_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name %in% c("MLPA_fish_biomass_density_transect_raw_v2.csv",
                            "MLPA_benthic_site_means.csv"))

raw_MCR_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1bMVr5VSXD2azlwD9DioeMwy4RR94uqF5")) %>%
  dplyr::filter(name %in% c("MCR_Fish_Biomass_v3.csv"))

raw_PIE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1yAoT0RtkRf2gxtBl3MXpi6cuGji5kuEE")) %>%
  dplyr::filter(name %in% c("LTE-TIDE-NektonFlumeDensity_v5_1.csv",
                            "LTE-TIDE-NektonFlumeIndividual_v6_3.csv"))

raw_CCE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/19INhcRd1xBKgDVd1G5W1B3QI4mlBr587")) %>%
  dplyr::filter(name %in% c("CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv",
                            "sumofallbiomass.csv"))

raw_NGA_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1j8QGQR6_vD1SQnFwVnaAy0W-1c_owCRv")) %>%
  dplyr::filter(name %in% c("nga_combined_clean.csv"))

# Combine file IDs
raw_ids <- rbind(raw_SBC_ids, raw_FCE_ids, raw_VCR_ids, raw_coastal_ids, raw_MCR_ids, raw_PIE_ids, raw_CCE_ids, raw_NGA_ids)

# Identify data key file
data_key_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name %in% c("CND_Data_Key_spatial.xlsx"))

# Identify PIE and CoastalCA species code tables
sp_codes_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a")) %>%
  dplyr::filter(name %in% c("PIE_speciescode_table.xlsx", "CoastalCA_speciescode_table.csv"))

# Combine file IDs
other_ids <- rbind(data_key_id, sp_codes_ids)

# For each raw data file, download it into the consumer folder
for(k in 1:nrow(raw_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_ids[k, ]$id, overwrite = T,
                                path = file.path("tier0", "raw_data", "consumer", raw_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_ids))
}

# For the data key and species code tables, download it into the tier0 folder
for(k in 1:nrow(other_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = other_ids[k, ]$id, overwrite = T,
                                path = file.path("tier0", other_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(other_ids))
}

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
#             Data Harmonizing ----
## ------------------------------------------ ##

# Read in the key
key <- readxl::read_excel(path = file.path("tier0", "CND_Data_Key_spatial.xlsx")) 

# Identify all downloaded files
( raw_files <- dir(path = file.path("tier0", "raw_data", "consumer")) )

# Make an empty list to store re-formatted raw data
df_list <- list()

for (i in 1:length(raw_files)){
  
  # Grab its name
  raw_file_name <- raw_files[i]
  
  # Message procesing start
  message("Harmonizing '", raw_file_name, "' (file ",  i, " of ", length(raw_files), ")")
  
  # Subset the key object a bit
  key_sub <- key %>%
    # Only this file's section
    dplyr::filter(raw_filename == raw_file_name) %>%
    # And only columns that have a synonymized equivalent
    dplyr::filter(!is.na(standardized_column_name) & nchar(standardized_column_name) != 0)
  
  # Special cases for reading in certain csv files 
  if (raw_file_name == "sumofallbiomass.csv") {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "consumer", raw_file_name), na.strings = ".", skip = 2, check.names = F)
  } 
  else if (raw_file_name == "CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv") {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "consumer", raw_file_name), na.strings = ".", check.names = F)
  } 
  else if (raw_file_name == "LTE-TIDE-NektonFlumeDensity_v5_1.csv") {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "consumer", raw_file_name), na.strings = ".", check.names = F)
  } 
  else if (raw_file_name == "LTE-TIDE-NektonFlumeIndividual_v6_3.csv") {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "consumer", raw_file_name), na.strings = ".", check.names = F)
  }
  else {
  raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "consumer", raw_file_name), na.strings = ".")
  }
  
  raw_df_v2 <- raw_df_v1 %>%
    # Create a row number column and a column for the original file
    dplyr::mutate(row_num = 1:nrow(.),
                  raw_filename = raw_file_name,
                  .before = dplyr::everything()) %>%
    # Make all columns into character columns
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    # Now pivot everything into ultimate long format
    ## Note: if column class differs this step can't be done
    ## That is why we convert everything into characters in the previous step
    tidyr::pivot_longer(cols = -row_num:-raw_filename,
                        names_to = "raw_column_name",
                        values_to = "values")
  
  # Identify any columns that are in the data key but (apparently) not in the data
  missing_cols <- setdiff(x = key_sub$raw_column_name, y = unique(raw_df_v2$raw_column_name))
  
  # If any are found, print a warning for whoever is running this
  if(length(missing_cols) != 0){
    message("Not all expected columns in '", raw_file_name, "' are in *data* key!")
    message("Check (and fix if needed) raw columns: ", 
            paste0("'", missing_cols, "'", collapse = " & ")) }
  
  # Drop this object (if it exists) to avoid false warning with the next run of the loop
  if(exists("missing_cols") == T){ rm(list = "missing_cols") }
  
  # Integrate synonymized column names from key
  raw_df_v3 <- raw_df_v2 %>%
    # Attach revised column names
    dplyr::left_join(key_sub, by = c("raw_filename", "raw_column_name")) %>%
    # Drop any columns that don't have a synonymized equivalent
    dplyr::filter(!is.na(standardized_column_name)) %>%
    # Pick a standard 'not provided' entry for concentration units
    dplyr::mutate(units = ifelse(nchar(units) == 0, yes = NA, no = units)) %>%
    # Handle concentration units characters that can't be in column names
    dplyr::mutate(units_fix = gsub(pattern = "\\/| |\\-", replacement = "_", x = units)) %>%
    # Combine concentration units with column name (where conc units are provided)
    dplyr::mutate(names_actual = ifelse(test = !is.na(units_fix),
                                        yes = paste0(standardized_column_name, ".", units_fix),
                                        no = standardized_column_name)) %>%
    # Pare down to only needed columns (implicitly removes unspecified columns)
    dplyr::select(row_num, project, habitat, raw_filename, names_actual, values) %>%
    # Pivot back to wide format with revised column names
    tidyr::pivot_wider(names_from = names_actual, values_from = values, values_fill = NA) %>%
    # Drop row number column
    dplyr::select(-row_num) %>%
    # Drop non-unique rows (there shouldn't be any but better safe than sorry)
    dplyr::distinct()
  
  # Add to list
  df_list[[raw_file_name]] <- raw_df_v3
}

## -------------------------------------------- ##
#  Convert Wide Species Columns to Long Format
## -------------------------------------------- ##

# MLPA_benthic_site_means.csv --------------------------------------------------

MLPA_p1 <- df_list[["MLPA_benthic_site_means.csv"]] %>%
  # Select all relevant columns
  dplyr::select(project, habitat, raw_filename, year, site, dplyr::starts_with("wide_den")) %>%
  # Pivot all "wide_den" columns to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("wide_den"), names_to = "sp_code", values_to = "density.num_m2") %>%
  # Extract the species code
  dplyr::mutate(sp_code = stringr::str_extract(sp_code, "[A-Z]+"))

MLPA_p2 <- df_list[["MLPA_benthic_site_means.csv"]] %>%
  # Select all relevant columns
  dplyr::select(project, habitat, raw_filename, year, site, dplyr::starts_with("wide_COVER")) %>%
  # Pivot all "wide_COVER" columns to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("wide_COVER"), names_to = "sp_code", values_to = "cover.percent") %>%
  # Extract the species code
  dplyr::mutate(sp_code = stringr::str_extract(sp_code, "(?<=wide_COVER_)[A-Z]+(?=.percent)"))

# Full join both parts 
MLPA_fixed <- full_join(MLPA_p1, MLPA_p2)

# Replace the old, wide dataframe with the new, long version
df_list[["MLPA_benthic_site_means.csv"]] <- MLPA_fixed

# CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv -----------------------

CCE_PROPOOS_p1 <- df_list[["CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv"]] %>%
  # Select all relevant columns
  dplyr::select(project, habitat, raw_filename, site, date, dplyr::starts_with("wide_dens")) %>%
  # Pivot all "wide_dens" columns to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("wide_dens"), names_to = "scientific_name", values_to = "density.num_m2") %>%
  # Extract the scientific name
  dplyr::mutate(scientific_name = stringr::str_extract(scientific_name, "(?<=wide_dens_)[a-z]+_*[a-z]*(?=.num_m2)")) %>%
  # Replace the underscore in the scientific name with a space
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, "_", " "))

CCE_PROPOOS_p2 <- df_list[["CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv"]] %>%
  # Select all relevant columns
  dplyr::select(project, habitat, raw_filename, site, date, dplyr::starts_with("wide_dry_biomass")) %>%
  # Pivot all "wide_dry_biomass" columns to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("wide_dry_biomass"), names_to = "scientific_name", values_to = "drymass.mgC_m2") %>%
  # Extract the scientific name
  dplyr::mutate(scientific_name = stringr::str_extract(scientific_name, "(?<=wide_dry_biomass_)[a-z]+_*[a-z]*(?=.mgC_m2)")) %>%
  # Replace the underscore in the scientific name with a space
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, "_", " "))

# Full join both parts 
CCE_PROPOOS_fixed <- full_join(CCE_PROPOOS_p1, CCE_PROPOOS_p2)

# Replace the old, wide dataframe with the new, long version
df_list[["CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv"]] <- CCE_PROPOOS_fixed

# Unlist the list we generated from above
tidy_v0 <- df_list %>%
  purrr::list_rbind(x = .)

# Check that out
dplyr::glimpse(tidy_v0)

# Clean up environment
rm(list = setdiff(ls(), c("key", "tidy_v0")))
## -------------------------------------------- ##
#               Wrangle Dates ----
## -------------------------------------------- ##

# Check out current dates
sort(unique(tidy_v0$date))

# Look at general date format per raw file
tidy_v0 %>%
  dplyr::group_by(raw_filename) %>%
  dplyr::summarize(dates = paste(unique(date), collapse = "; ")) %>%
  tidyr::pivot_wider(names_from = raw_filename, values_from = dates) %>%
  dplyr::glimpse()

# Identify format for each file name based on **human eye/judgement**
tidy_v1a <- tidy_v0 %>%
  dplyr::mutate(date_format = dplyr::case_when(
    raw_filename == "Annual_All_Species_Biomass_at_transect_20230814.csv" ~ "YYYY-MM-DD",
    raw_filename == "CCE_PROPOOS_net_data_individual_categories_line80_90_12_08_2023.csv" ~ "MM/DD/YYYY",
    raw_filename == "IV_EC_talitrid_population.csv" ~ "MM/DD/YYYY",
    raw_filename == "LTE-TIDE-NektonFlumeDensity_v5_1.csv" ~ "YYYY-MM-DD",
    raw_filename == "LTE-TIDE-NektonFlumeIndividual_v6_3.csv" ~ "YYYY-MM-DD",
    raw_filename == "MAP_years1thru19.csv" ~ "MM/DD/YY",
    raw_filename == "MCR_Fish_Biomass_v3.csv" ~ "NA", # only has year month day
    raw_filename == "MLPA_benthic_site_means.csv" ~ "NA", # only has year
    raw_filename == "MLPA_fish_biomass_density_transect_raw_v2.csv" ~ "NA", # only has year month day
    raw_filename == "VCR14232_2.csv" ~ "MM/DD/YY",
    raw_filename == "sumofallbiomass.csv" ~ "YYYY-MM-DD",
    raw_filename == "nga_combined_clean.csv" ~ "YYYY-MM-DDTHH:MM:SS-0800",
    # raw_filename == "" ~ "",
    T ~ "UNKNOWN"))

# Check remaining date formats
tidy_v1a %>%
  dplyr::group_by(date_format) %>%
  dplyr::summarize(files = paste(unique(raw_filename), collapse = "; "))

# Remind yourself what year/month/day/date columns do each raw file already contain
key %>%
  dplyr::select(raw_filename, standardized_column_name, data_type) %>%
  dplyr::filter(data_type == "consumer") %>%
  dplyr::filter(standardized_column_name %in% c("year","month","day","date")) %>%
  View()

# Break apart the date column depending on the date format
tidy_v1b <- tidy_v1a %>%
  # If the date format is "YYYY-MM-DDTHH:MM:SS-0800", remove the rest of the date starting with "T"
  # so we actually end with up "YYYY-MM-DD"
  dplyr::mutate(date = ifelse(date_format == "YYYY-MM-DDTHH:MM:SS-0800", yes = stringr::str_replace(date, "T(.+)", ""), no = date)) %>%
  # If the date format is ""YYYY-MM-DDTHH:MM:SS-0800", change the date format column to "YYYY-MM-DD" now
  dplyr::mutate(date_format = ifelse(date_format == "YYYY-MM-DDTHH:MM:SS-0800", yes = "YYYY-MM-DD", no = date_format)) %>%
  # YYYY-MM-DD
  tidyr::separate_wider_delim(date, delim = "-", names = c("year_fix1", "month_fix1", "day_fix1"), too_few = "align_start", cols_remove = F) %>% 
  # MM/DD/YY and MM/DD/YYYY
  tidyr::separate_wider_delim(date, delim = "/", names = c("month_fix2", "day_fix2", "year_fix2"), too_few = "align_start", cols_remove = F) %>%
  # If year_fix2 only has the last two digits of the year, add "20" to the front 
  dplyr::mutate(year_fix2 = ifelse(nchar(year_fix2) == 2, yes = paste0("20", year_fix2), no = year_fix2)) %>%
  # As a result of doing separate_wider_delim() twice, rows with the YYYY-MM-DD dates have values in the month_fix2 column (which we don't want)
  dplyr::mutate(month_fix2 = ifelse(date_format == "YYYY-MM-DD", yes = NA, no = month_fix2)) %>%
  # As a result of doing separate_wider_delim() twice, rows with the MM/DD/YY or MM/DD/YYYY dates have values in the year_fix1 column (which we don't want)
  dplyr::mutate(year_fix1 = ifelse(date_format == "MM/DD/YY" | date_format == "MM/DD/YYYY", yes = NA, no = year_fix1)) 
  

# Date wrangling
tidy_v1c <- tidy_v1b %>%
  dplyr::relocate(day, .after = month) %>%
  # Coalesce the day, day_fix1, day_fix2 columns together
  dplyr::mutate(day = dplyr::coalesce(day, day_fix1, day_fix2)) %>%
  # Coalesce the month, month_fix1, month_fix2 columns together
  dplyr::mutate(month = dplyr::coalesce(month, month_fix1, month_fix2)) %>%
  # Coalesce the year, year_fix1, year_fix2 columns together
  dplyr::mutate(year = dplyr::coalesce(year, year_fix1, year_fix2)) %>%
  # Drop day, month, year columns that we don't need anymore
  dplyr::select(-day_fix1, -day_fix2, -month_fix1, -month_fix2, -year_fix1, -year_fix2) %>%
  # Remove the leading 0 in the day column
  dplyr::mutate(day = gsub(pattern = "^0",
                           replacement = "",
                           x = day)) %>%
  # Remove the leading 0 in the month column
  dplyr::mutate(month = gsub(pattern = "^0",
                           replacement = "",
                           x = month)) %>%
  # Make a real date column
  dplyr::mutate(date_v0 = paste(year, month, day, sep = "-"),
                .after = day) %>%
  dplyr::mutate(date_actual = as.Date(x = date_v0, format = "%Y-%m-%d"),
                .after = date_v0) %>%
  # Remove the preliminary date columns
  dplyr::select(-date_v0, -date, -date_format) %>%
  # Rename date_actual to date
  dplyr::rename(date = date_actual) %>%
  # Replace NA strings with actual NA values
  dplyr::mutate(dplyr::across(.cols = c(year, month, day), .fns = ~dplyr::na_if(., y = "NA"))) %>%
  # Filter out the weird row with year as "(100840) rows" -this is the last line of MCR_Fish_Biomass
  dplyr::filter(year != "(100840 rows)")

# Check overall dates
unique(tidy_v1c$year)
unique(tidy_v1c$month)
unique(tidy_v1c$day)
sort(unique(tidy_v1c$date))

# Check unique years/months/days/dates for each raw file
tidy_v1c %>%
  dplyr::group_by(raw_filename) %>%
  dplyr::summarize(years = paste(unique(year), collapse = "; "),
                   months = paste(unique(month), collapse = "; "),
                   days = paste(unique(day), collapse = "; "),
                   dates = paste(unique(date), collapse = "; ")) %>%
  dplyr::glimpse()

# Clean up environment
rm(list = setdiff(ls(), c("tidy_v1c")))

## -------------------------------------------- ##
#               Wrangle Species ----
## -------------------------------------------- ##

# Read in the tables for PIE and CoastalCA species codes (for later)
PIE_sp_codes <- readxl::read_excel(path = file.path("tier0", "PIE_speciescode_table.xlsx")) 
CoastalCA_sp_codes <- read.csv(file = file.path("tier0", "CoastalCA_speciescode_table.csv"))

# Doing some preliminary wrangling on species names
tidy_v2a <- tidy_v1c %>%
  # Replace one of the missing value codes -99999 with NA values
  dplyr::mutate(dplyr::across(.cols = c(-year, -month, -day, -date, -sp_code), .fns = ~dplyr::na_if(., y = "-99999"))) %>%
  # Replace empty strings with NA values
  dplyr::mutate(dplyr::across(.cols = c(-year, -month, -day, -date, -sp_code), .fns = ~dplyr::na_if(., y = ""))) %>%
  # Replace NA strings with actual NA values
  dplyr::mutate(dplyr::across(.cols = c(-year, -month, -day, -date, -sp_code), .fns = ~dplyr::na_if(., y = "NA"))) %>%
  # Replace any mention of -1 (missing value indicator in MCR_Fish_Biomass_v3.csv) with actual NA values
  dplyr::mutate(wetmass.g = dplyr::case_when(
    raw_filename == "MCR_Fish_Biomass_v3.csv" & wetmass.g == -1 ~ NA,
    T ~ wetmass.g
  )) %>%
  # If the species is just "spp " or "spp" or "spp." or "partial" or "No fish observed" then we can set it as NA 
  dplyr::mutate(species = dplyr::case_when(
    species == "spp " | species == "spp" | species == "spp." | species == "partial"| species == "No fish observed" ~ NA,
    T ~ species
  )) %>%
  # If the row is from the MLPA csv and the species is non-empty, put genus + species as the value in species
  dplyr::mutate(species = dplyr::case_when(
    raw_filename == "MLPA_fish_biomass_density_transect_raw_v2.csv" & !is.na(species) ~ paste(genus, species),
    T ~ species
  )) %>%
  # If the row is from the IV_EC csv and the species is non-empty, put genus + species as the value in species
  dplyr::mutate(species = dplyr::case_when(
    raw_filename == "IV_EC_talitrid_population.csv" & !is.na(species) ~ paste(genus, species),
    T ~ species
  )) %>%
  # If the species is empty and the scientific_name column contains multiple species, put the scientific name as the value in species
  dplyr::mutate(species = ifelse(is.na(species) & stringr::str_detect(scientific_name, "[:punct:]"),
                                     yes = scientific_name,
                                     no = species))

# Check unique species names
unique(tidy_v2a$species)

tidy_v2b <- tidy_v2a %>%
  # Make a new scientific name column that combines info across many taxon columns
  dplyr::mutate(new_sci_name = dplyr::coalesce(scientific_name, species, genus, family, order, class, phylum)) %>%
  # Drop old scientific_name column
  dplyr::select(-scientific_name) %>%
  # Rename our new scientific name column
  dplyr::rename(scientific_name = new_sci_name) %>%
  # Remove underscore from the species name
  dplyr::mutate(scientific_name = gsub(pattern = "_",
                                       replacement = " ",
                                       x = scientific_name)) %>%
  # Fixing names in scientific_name:
  # Remove any instance of "unidentified " + a number  
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, "unidentified [:digit:]*", "")) %>%
  # Remove any instance of "Unidentified " or "Unidentifiable " or " unidentified" 
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, "Unidentified |Unidentifiable | unidentified$", "")) %>%
  # Remove any instance of " sp." or " spp." or " sp" or " spp. "
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, " sp.$| spp.$| sp$| spp. $", "")) %>%
  # Remove any instance of " spp. (partial)" or " (partial)"
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, " spp. \\(partial\\)$| \\(partial\\)$", "")) %>%
  # Remove any instance of " (cf)" or " sp. " + a number or "small " or " others"
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, " \\(cf\\)| sp. [:digit:]*|small | others$", "")) %>%
  # Remove any instance of trailing space or trailing space + a number
  dplyr::mutate(scientific_name = stringr::str_replace(scientific_name, "[:blank:]$|[:blank:][:digit:]?$", "")) %>%
  # Doing more custom fixes in scientific_name:
  dplyr::mutate(scientific_name = dplyr::case_when(
    scientific_name == "No megalorchestia" ~ "Megalorchestia",
    scientific_name == "juvenile kelp" ~ "Lessoniaceae",
    scientific_name == "Flatfish" ~ "Bothidae",
    scientific_name == "Halymenia spp.; Schizymenia pacifica" ~ "Florideophyceae",
    scientific_name == "Branching Red Alga" ~ "Rhodophyta",
    scientific_name == "Chondracanthus corymbiferus; Chondracanthus exasperatus" ~ "Chondracanthus",
    scientific_name == "Cellaria diffusa; Cellaria mandibulata" ~ "Cellaria",
    scientific_name == "Cucumaria salma; Cucumaria miniata" ~ "Cucumaria",
    scientific_name == "Dictyota binghamiae; Dictyota flabellata; Dictyota coriacea" ~ "Dictyota",
    scientific_name == "Ulva spp.; Sponogomorpha" ~ "Ulvophyceae",
    scientific_name == "Neoptilota spp.; Ptilota spp.; Rhodoptilum" ~ "Ceramiales",
    scientific_name == "Pomaulax gibberosus; Megastraea undosa" ~ "Turbinidae",
    scientific_name == "YOY Sebastes" ~ "Sebastes",
    scientific_name == "brown blade" ~ "Phaeophyceae",
    scientific_name == "Ectopleura crocea; Eudendrium californicum; Schuchertinia milleri" ~ "Anthoathecata",
    scientific_name == "salps" ~ "Salpidae",
    scientific_name == "cichlid species" ~ "Cichlidae",
    scientific_name == "No fishes collected" ~ NA,
    scientific_name == "cyprinid" ~ "Cyprinidae",
    scientific_name == "Parrotfish" ~ "Scaridae",
    scientific_name == "Sebastes atrovirens,carnatus,chrysomelas,caurinus" ~ "Sebastes",
    scientific_name == "Sebastes serranoides,flavidus" ~ "Sebastes",
    scientific_name == "Sebastes chrysomelas/carnatus young of year" ~ "Sebastes",
    scientific_name == "Sebastes serranoides,flavidus,melanops" ~ "Sebastes",
    scientific_name == "Sebastes carnatus, caurinus" ~ "Sebastes",
    scientific_name == "fishes" ~ "Vertebrata",
    scientific_name == "eel species" ~ "Anguilliformes",
    scientific_name == "Surfperch" ~ "Embiotoca",
    scientific_name == "pyrosomes" ~ "pyrosoma",
    scientific_name == "euphausiids" ~ "euphausiacea",
    scientific_name == "doliolids" ~ "doliolida",
    scientific_name == "nauplii" ~ "crustacea",
    scientific_name == "polychaete" ~ "polychaeta",
    scientific_name == "bryozoan" ~ "Bryozoa",
    scientific_name == "cnidaria ctenophores" ~ "Coelenterata",
    scientific_name == "copepoda calanoida" ~ "Calanoida",
    scientific_name == "copepoda eucalanids" ~ "Eucalanidae",
    scientific_name == "copepoda harpacticoida" ~ "Harpacticoida",
    scientific_name == "copepoda oithona" ~ "Oithona",
    scientific_name == "copepoda poecilostomatoids" ~ "Poecilostomatoida",
    scientific_name == "Arborescent Bryozoan" ~ "Bryozoa",
    scientific_name == "crustose coralline algae" ~ "Corallinales",
    scientific_name == "Encrusting Bryozoa" ~ "Bryozoa",
    scientific_name == "ostracods" ~ "Ostracoda",
    T ~ scientific_name
  )) 

# Check unique scientific names
unique(tidy_v2b$scientific_name)

# Finding the common names that do not have scientific names
some_common_names_fix <- tidy_v2b %>%
  # Make a column indicating "keep" for rows that have a common name but not a scientific name
  dplyr::mutate(needs_fixing = ifelse(is.na(scientific_name) & !is.na(common_name),
                                      yes = "keep",
                                      no = "drop")) %>%
  # Select relevant columns
  dplyr::select(common_name, needs_fixing) %>%
  # Filter to rows that needs fixing
  dplyr::filter(needs_fixing == "keep") %>%
  # Find unique values
  dplyr::distinct() %>%
  # Drop the row where common name is "No fishes collected"
  dplyr::filter(common_name != "No fishes collected") %>%
  # Drop the indicator column
  dplyr::select(-needs_fixing) %>%
  # Make empty placeholder columns for our query results later
  dplyr::mutate(kingdom_fix = NA,
                phylum_fix = NA,
                class_fix = NA,
                order_fix = NA,
                family_fix = NA,
                genus_fix = NA,
                species_fix = NA)

for (i in 1:length(some_common_names_fix$common_name)){
  # Query species for its taxonomic information
  query_results <- taxize::tax_name(sci = some_common_names_fix[i,]$common_name,
                                    get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                                    db = "itis",
                                    accepted = T,
                                    ask = F)
  
  # Save the query results
  # In case the query returns NULL, the paste0(..., collapse = "") will coerce NULL into an empty string
  some_common_names_fix[i,]$kingdom_fix <- paste0(query_results$kingdom, collapse = "")
  some_common_names_fix[i,]$phylum_fix <- paste0(query_results$phylum, collapse = "")
  some_common_names_fix[i,]$class_fix <- paste0(query_results$class, collapse = "")
  some_common_names_fix[i,]$order_fix <- paste0(query_results$order, collapse = "")
  some_common_names_fix[i,]$family_fix <- paste0(query_results$family, collapse = "")
  some_common_names_fix[i,]$genus_fix <- paste0(query_results$genus, collapse = "")
  some_common_names_fix[i,]$species_fix <- paste0(query_results$species, collapse = "")
}

some_common_names_fix_v2 <- some_common_names_fix %>%
  # Replace the string "NA" with actual NA values
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~dplyr::na_if(., y = "NA"))) %>%
  # Manually find the scientific names for the rest of the common names that did not get automatically filled by taxize
  dplyr::mutate(species_fix = dplyr::case_when(
    common_name == "Spot" ~ "Leiostomus xanthurus",
    common_name == "Pinfish" ~ "Lagodon rhomboides",
    common_name == "Striped burrfish" ~ "Chilomycterus schoepfi",
    common_name == "Black Sea Bass" ~ "Centropristis striata",
    common_name == "Pigfish" ~ "Orthopristis chrysoptera",
    common_name == "Scup" ~ "Stenotomus chrysops",
    common_name == "Sheepshead" ~ "Archosargus probatocephalus",
    common_name == "Butterfish" ~ "Peprilus triacanthus",
    common_name == "Summer Flounder" ~ "Paralichthys dentatus",
    common_name == "Striped Blenny" ~ "Meiacanthus grammistes",
    common_name == "Mangrove Snapper" ~ "Lutjanus griseus",
    common_name == "Bluefish" ~ "Pomatomus saltatrix",
    common_name == "Tautog" ~ "Tautoga onitis",
    common_name == "Croaker" ~ "Micropogonias undulatus",
    common_name == "Speckled Trout" ~ "Cynoscion nebulosus",
    common_name == "Skilletfish" ~ "Gobiesox strumosus",
    common_name == "Menhaden" ~ "Brevoortia tyrannus",
    T ~ species_fix
  )) %>%
  dplyr::mutate(genus_fix = dplyr::case_when(
    common_name == "Seahorse" ~ "Hippocampus",
    T ~ genus_fix
  )) %>%
  dplyr::mutate(family_fix = dplyr::case_when(
    common_name == "Mojarra" ~ "Gerreidae",
    common_name == "Pipefish" ~ "Syngnathinae",
    common_name == "Unknown Sciaenid" ~ "Sciaenid",
    common_name == "Sand Mullet" ~ "Mugilidae",
    common_name == "Pufferfish" ~ "Tetraodontidae",
    common_name == "Filefish" ~ "Monacanthidae",
    common_name == "American Anchovy" ~ "Engraulidae",
    T ~ family_fix
  )) %>%
  dplyr::mutate(order_fix = dplyr::case_when(
    common_name == "Silversides" ~ "Atheriniformes",
    common_name == "Goby" ~ "Gobioidei",
    T ~ order_fix
  )) %>%
  dplyr::mutate(class_fix = dplyr::case_when(
    common_name == "Squid" ~ "Coleoidea",
    T ~ class_fix
  ))

tidy_v2c <- left_join(tidy_v2b, some_common_names_fix_v2, by = "common_name") %>% 
  # Coalesce taxonomic columns together to fill in missing taxonomic info whenever possible
  dplyr::mutate(kingdom = dplyr::coalesce(kingdom, kingdom_fix),
         phylum = dplyr::coalesce(phylum, phylum_fix),
         class = dplyr::coalesce(class, class_fix),
         order = dplyr::coalesce(order, order_fix),
         family = dplyr::coalesce(family, family_fix),
         genus = dplyr::coalesce(genus, genus_fix),
         species = dplyr::coalesce(species, species_fix)) %>%
  # Drop the rest of the columns from the taxon table
  dplyr::select(-dplyr::contains("_fix")) %>%
  # Combine info from across many taxon columns into scientific_name
  dplyr::mutate(scientific_name = dplyr::coalesce(scientific_name, species, genus, family, order, class, phylum)) 

# Check unique scientific names
unique(tidy_v2c$scientific_name)

taxon_fix <- tidy_v2c %>%
  # Grab all the species from our tidy object
  dplyr::select(scientific_name) %>%
  # Get a vector of all unique species
  dplyr::distinct() %>%
  # Make empty placeholder columns for our query results later
  dplyr::mutate(kingdom_fix = NA,
                phylum_fix = NA,
                class_fix = NA,
                order_fix = NA,
                family_fix = NA,
                genus_fix = NA,
                species_fix = NA,
                common_name_fix = NA) %>%
  # Rename species column
  dplyr::rename(scientific_name_fix = scientific_name) %>%
  # Remove rows without any value in the scientific_name_fix column
  dplyr::filter(!is.na(scientific_name_fix) & nchar(scientific_name_fix) != 0)


# Check structure
dplyr::glimpse(taxon_fix)

# For each unique species...
for (i in 1:length(taxon_fix$scientific_name_fix)){

    # Message procesing start
    message("Completing taxonomic information for row ", i, " of ", length(taxon_fix$scientific_name_fix))
    
    # Query species for its taxonomic information
    query_results <- taxize::tax_name(sci = taxon_fix[i,]$scientific_name_fix,
                                      get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                                      db = "itis",
                                      accepted = T,
                                      ask = F)
    
    # Query species for its common name 
    common_name_results <- taxize::sci2comm(sci = taxon_fix[i,]$scientific_name_fix, 
                                            db = "itis",
                                            accepted = T,
                                            ask = F)
    
    # Save the query results
    # In case the query returns NULL, the paste0(..., collapse = "") will coerce NULL into an empty string
    taxon_fix[i,]$kingdom_fix <- paste0(query_results$kingdom, collapse = "")
    taxon_fix[i,]$phylum_fix <- paste0(query_results$phylum, collapse = "")
    taxon_fix[i,]$class_fix <- paste0(query_results$class, collapse = "")
    taxon_fix[i,]$order_fix <- paste0(query_results$order, collapse = "")
    taxon_fix[i,]$family_fix <- paste0(query_results$family, collapse = "")
    taxon_fix[i,]$genus_fix <- paste0(query_results$genus, collapse = "")
    taxon_fix[i,]$species_fix <- paste0(query_results$species, collapse = "")
    taxon_fix[i,]$common_name_fix <- paste0(common_name_results[[1]], collapse = "; ")
  
}

taxon_fix_v2 <- taxon_fix %>%
  # Replace the string "NA" with actual NA values
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~dplyr::na_if(., y = "NA"))) %>%
  # Replace empty strings with NA values
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = ~dplyr::na_if(., y = "")))
  

# Left join our current tidy dataframe with the table of taxonomic info
tidy_v2d <- left_join(tidy_v2c, taxon_fix_v2, by = c("scientific_name" = "scientific_name_fix")) %>%
  # Coalesce taxonomic columns together to fill in missing taxonomic info whenever possible
  mutate(kingdom = dplyr::coalesce(kingdom, kingdom_fix),
         phylum = dplyr::coalesce(phylum, phylum_fix),
         class = dplyr::coalesce(class, class_fix),
         order = dplyr::coalesce(order, order_fix),
         family = dplyr::coalesce(family, family_fix),
         genus = dplyr::coalesce(genus, genus_fix),
         common_name = dplyr::coalesce(common_name, common_name_fix)) %>%
  # If the species column is non-empty and contains punctuation indicating multiple species, put the species as the value in species_fix
  dplyr::mutate(species_fix = ifelse(!is.na(species) & stringr::str_detect(species, "[\\;\\,\\/]"),
                                     yes = species,
                                     no = species_fix)) %>%
  dplyr::mutate(species_fix = dplyr::case_when(
    species_fix == "Sebastes atrovirens,carnatus,chrysomelas,caurinus" ~ "Sebastes atrovirens; Sebastes carnatus; Sebastes chrysomelas; Sebastes caurinus",
    species_fix == "Sebastes serranoides,flavidus" ~ "Sebastes serranoides; Sebastes flavidus",
    species_fix == "Sebastes chrysomelas/carnatus young of year" ~ "Sebastes chrysomelas; Sebastes carnatus",
    species_fix == "Sebastes serranoides,flavidus,melanops" ~ "Sebastes serranoides; Sebastes flavidus; Sebastes melanops",
    species_fix == "Sebastes carnatus, caurinus" ~ "Sebastes carnatus; Sebastes caurinus",
    T ~ species_fix
  )) %>%
  # Drop the inferior species column (some strings had numbers in them or had "(cf)")
  dplyr::select(-species) %>%
  # Keep species_fix as the superior species column
  dplyr::rename(species = species_fix) %>%
  # Drop the rest of the columns from the taxon table
  dplyr::select(-dplyr::contains("_fix")) %>%
  # Make sure the first word is capitalized in scientific_name 
  dplyr::mutate(scientific_name = stringr::str_to_sentence(scientific_name)) %>%
  # Finally, if the species column is empty but the scientific_name column contains the full species name,
  # fill in the species column with the value in scientific_name
  dplyr::mutate(species = ifelse(is.na(species) & stringr::str_detect(scientific_name, "[:blank:]"),
                                 yes = scientific_name,
                                 no = species))

# Check unique scientific names
unique(tidy_v2d$scientific_name)

# Check unique species names
unique(tidy_v2d$species)

# Check structure
dplyr::glimpse(tidy_v2d)

# Now join with the table for PIE species codes 
tidy_v2e <- tidy_v2d %>%
  # First replace the species code "XAN " with "XAN"
  dplyr::mutate(sp_code = dplyr::case_when(
    sp_code == "XAN " ~ "XAN",
    T ~ sp_code
  )) %>%
  # Join with the table
  dplyr::left_join(PIE_sp_codes, by = c("project", "sp_code")) %>%
  # Coalesce with the new columns to fill in the missing taxonomic info for PIE species 
  dplyr::mutate(common_name = dplyr::coalesce(common_name.x, common_name.y),
                scientific_name = dplyr::coalesce(scientific_name.x, scientific_name.y),
                kingdom = dplyr::coalesce(kingdom.x, kingdom.y),
                phylum = dplyr::coalesce(phylum.x, phylum.y),
                class = dplyr::coalesce(class.x, class.y),
                order = dplyr::coalesce(order.x, order.y),
                family = dplyr::coalesce(family.x, family.y),
                genus = dplyr::coalesce(genus.x, genus.y),
                species = dplyr::coalesce(species.x, species.y)) %>%
  # Drop the duplicate columns that resulted from joining
  dplyr::select(-contains(".x"), -contains(".y"))

# Now join with the table for CoastalCA species codes 
tidy_v2f <- tidy_v2e %>%
  # Join with the table
  dplyr::left_join(CoastalCA_sp_codes, by = c("project", "sp_code")) %>%
  # Coalesce with the new columns to fill in the missing taxonomic info for PIE species 
  dplyr::mutate(common_name = dplyr::coalesce(common_name.x, common_name.y),
                scientific_name = dplyr::coalesce(scientific_name.x, scientific_name.y),
                kingdom = dplyr::coalesce(kingdom.x, kingdom.y),
                phylum = dplyr::coalesce(phylum.x, phylum.y),
                class = dplyr::coalesce(class.x, class.y),
                order = dplyr::coalesce(order.x, order.y),
                family = dplyr::coalesce(family.x, family.y),
                genus = dplyr::coalesce(genus.x, genus.y),
                species = dplyr::coalesce(species.x, species.y)) %>%
  # Drop the duplicate columns that resulted from joining
  dplyr::select(-contains(".x"), -contains(".y"))

# Check structure
dplyr::glimpse(tidy_v2f)

species_table <- tidy_v2f %>%
  # Select the appropriate columns to create our species table
  dplyr::select(project, sp_code, scientific_name, common_name, kingdom, phylum, class, order, family, genus, species, taxa_group) %>%
  # Get unique species
  dplyr::distinct() %>%
  # Sort by project and scientific_name
  dplyr::arrange(project, scientific_name) %>%
  # Remove rows that have NA values for both scientific_name and sp_code
  dplyr::filter(!(is.na(scientific_name) & is.na(sp_code)))

tidy_v2g <- tidy_v2f %>%
  # Now that we have our species table, we don't need the other taxa columns in our harmonized dataset
  dplyr::select(-common_name, -kingdom, -phylum, -class, -order, -family, -genus, -species, -taxa_group)

# Clean up environment
rm(list = setdiff(ls(), c("tidy_v2g", "species_table")))

## -------------------------------------------- ##
#      Reordering & Changing Column Types ----
## -------------------------------------------- ##

# Check structure
dplyr::glimpse(tidy_v2g)

tidy_v3 <- tidy_v2g %>%
  dplyr::relocate(sp_code, .after = subsite_level1) %>%
  dplyr::relocate(subsite_level2, .after = subsite_level1) %>%
  dplyr::relocate(subsite_level3, .after = subsite_level2) %>%
  dplyr::relocate(scientific_name, .after = sp_code) %>%
  dplyr::relocate(coarse_grouping, .after = scientific_name) %>%
  dplyr::relocate(count.num, .after = coarse_grouping) %>%
  dplyr::relocate(cover.percent, .after = count.num) %>%
  dplyr::relocate(density.num_m, .after = cover.percent) %>%
  dplyr::relocate(density.num_m2, .after = density.num_m) %>%
  dplyr::relocate(density.num_m3, .after = density.num_m2) %>%
  dplyr::relocate(drymass.g_m, .after = density.num_m3) %>% 
  dplyr::relocate(drymass.g_m2, .after = drymass.g_m) %>%  
  dplyr::relocate(drymass.mgC_m2, .after = drymass.g_m2) %>%  
  dplyr::relocate(excretion_egestion.ug_m3, .after = drymass.mgC_m2) %>%  
  dplyr::relocate(length.cm, .after = excretion_egestion.ug_m3) %>%  
  dplyr::relocate(length.mm, .after = length.cm) %>%  
  dplyr::relocate(length.um, .after = length.mm) %>%  
  dplyr::relocate(transect_area.m, .after = length.um) %>%  
  dplyr::relocate(transect_area.m2, .after = transect_area.m) %>% 
  dplyr::relocate(wetmass.g, .after = transect_area.m2) %>%  
  dplyr::relocate(wetmass.g_m2, .after = wetmass.g) %>%
  dplyr::relocate(wetmass.kg, .after = wetmass.g_m2) %>% 
  dplyr::relocate(wetmass.mg_m3, .after = wetmass.kg) %>%  
  dplyr::mutate(dplyr::across(.cols = c(year:day, count.num:wetmass.mg_m3), .fns = as.numeric))

# Check structure
dplyr::glimpse(tidy_v3)

## -------------------------------------------- ##
#  Convert Measurement Columns to Long Format
## -------------------------------------------- ##

tidy_v4 <- tidy_v3 %>%
  # Pivot the measurement columns to long format
  tidyr::pivot_longer(cols = count.num:wetmass.mg_m3,
               names_to = "measurement_type",
               values_to = "measurement_value") %>%
  # Create a measurement_unit column from measurement_type
  tidyr::separate_wider_delim(measurement_type, delim = ".", names = c("measurement_type", "measurement_unit"), too_many = "merge") %>%
  # Fix the units by replacing "_" with "/"
  dplyr::mutate(measurement_unit = stringr::str_replace(measurement_unit, pattern = "_", replacement = "/")) %>%
  # Drop rows where measurement_value is NA
  dplyr::filter(!is.na(measurement_value)) %>%
  # Moving columns around
  dplyr::relocate(sp_code, .after = measurement_value) %>%
  dplyr::relocate(scientific_name, .after = sp_code) 
  
# Check structure
dplyr::glimpse(tidy_v4)

## -------------------------------------------- ##
#                   Export ----
## -------------------------------------------- ##

# Create one final tidy object
tidy_final <- tidy_v4

# Check structure
dplyr::glimpse(tidy_final)

# Grab today's date
date <- gsub(pattern = "-", replacement = "", x = Sys.Date())

# Generate a date-stamped file name for this file
( tidy_filename <- paste0("harmonized_consumer_", date, ".csv") )

# Generate a date-stamped file name for the species table
( species_filename <- paste0("harmonized_consumer_species_", date, ".csv") )

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy"), showWarnings = F)

# Export locally
write.csv(x = tidy_final, file = file.path("tidy", tidy_filename), na = 'NA', row.names = F)

# Also export species table
write.csv(x = species_table, file = file.path("tidy", species_filename), na = 'NA', row.names = F)

# Export harmonized dataset to Drive
googledrive::drive_upload(media = file.path("tidy", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# Export species table to Drive
googledrive::drive_upload(media = file.path("tidy", species_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# End ----
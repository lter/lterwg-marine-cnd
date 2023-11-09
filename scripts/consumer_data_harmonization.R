## ------------------------------------------ ##
#       Marine CND -- Data Harmonization
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Sites: SBC, FCE

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

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Identify raw data files
# For example, here I'm pulling all the SBC consumer data from Google Drive
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")) %>%
  dplyr::filter(name %in% c("Annual_All_Species_Biomass_at_transect_20230814.csv",
                            "IV_EC_talitrid_population.csv"))

# Identify raw data files
raw_FCE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1guv_ULta7dlF2rYUTYhaRQ8NldSlMO_y")) %>%
  dplyr::filter(name %in% c("MAP_years1thru19.csv"))

# Combine file IDs
raw_ids <- rbind(raw_SBC_ids, raw_FCE_ids)

# Identify and download the data key
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name == "CND_Data_Key.xlsx") %>%
  googledrive::drive_download(file = .$id, path = file.path("tier0",.$name), overwrite = T)

# For each raw data file, download it into its own site folder
for(k in 1:nrow(raw_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_ids[k, ]$id, overwrite = T,
                                path = file.path("tier0", "raw_data", raw_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_ids))
}

# Clear environment
rm(list = ls())

# Read in the key
key <- readxl::read_excel(path = file.path("tier0", "CND_Data_Key.xlsx")) 

## ------------------------------------------ ##
#             Data Harmonizing ----
## ------------------------------------------ ##

# Identify all downloaded files
( raw_files <- dir(path = file.path("tier0", "raw_data")) )

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
  
  raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", raw_file_name))
  
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
                                        yes = paste0(standardized_column_name, "_", units_fix),
                                        no = standardized_column_name)) %>%
    # Pare down to only needed columns (implicitly removes unspecified columns)
    dplyr::select(row_num, project, data_type, raw_filename, names_actual, values) %>%
    # Pivot back to wide format with revised column names
    tidyr::pivot_wider(names_from = names_actual, values_from = values, values_fill = NA) %>%
    # Drop row number column
    dplyr::select(-row_num) %>%
    # Drop non-unique rows (there shouldn't be any but better safe than sorry)
    dplyr::distinct()
  
  # Add to list
  df_list[[raw_file_name]] <- raw_df_v3
}

# Unlist the list we just generated
tidy_v0 <- df_list %>%
  purrr::list_rbind(x = .)

# Check that out
dplyr::glimpse(tidy_v0)

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
    raw_filename == "IV_EC_talitrid_population.csv" ~ "MM/DD/YYYY",
    raw_filename == "MAP_years1thru19.csv" ~ "MM/DD/YY",
    # raw_filename == "" ~ "",
    T ~ "UNKNOWN"))

# Check remaining date formats
tidy_v1a %>%
  dplyr::group_by(date_format) %>%
  dplyr::summarize(files = paste(unique(raw_filename), collapse = "; "))

# Remind yourself what year/month/day/date columns do each raw file already contain
key %>%
  dplyr::select(raw_filename, standardized_column_name) %>%
  dplyr::filter(standardized_column_name %in% c("year","month","day","date"))

# Break apart the date column depending on the date format
tidy_v1b <- tidy_v1a %>%
  tidyr::separate_wider_delim(date, delim = "-", names = c("year_fix1", "month_fix1", "day_fix1"), too_few = "align_start", cols_remove = F) %>%
  tidyr::separate_wider_delim(date, delim = "/", names = c("month_fix2", "day_fix2", "year_fix2"), too_few = "align_start", cols_remove = F) 

# Date wrangling
tidy_v1c <- tidy_v1b %>%
  dplyr::relocate(day, .after = month) %>%
  # Coalesce the day and day_fix1 columns together
  dplyr::mutate(day = dplyr::coalesce(day, day_fix1)) %>%
  # Throw away the unneeded pieces from the YYYY-MM-DD date format
  dplyr::select(-year_fix1, -month_fix1, -day_fix1) %>%
  # If the date format is MM/DD/YYYY then...
  dplyr::mutate(
    # Use the year_fix2 column for the year
    year = dplyr::case_when(
      date_format == "MM/DD/YYYY" ~ year_fix2,
      T ~ year),
    # Use the month_fix2 column for the month
    month = dplyr::case_when(
      date_format == "MM/DD/YYYY" ~ month_fix2,
      T ~ month),
    # Use the day_fix2 column for the day
    day = dplyr::case_when(
      date_format == "MM/DD/YYYY" ~ day_fix2,
      T ~ day)
    ) %>%
  # Throw away the unneeded pieces from the MM/DD/YYYY date format
  dplyr::select(-year_fix2, -month_fix2, -day_fix2) %>%
  # Remove the leading 0 in the day column
  dplyr::mutate(day = gsub(pattern = "^0",
                           replacement = "",
                           x = day)) %>%
  # Make a real date column
  dplyr::mutate(date_v0 = paste(year, month, day, sep = "-"),
                .after = day) %>%
  dplyr::mutate(date_actual = as.Date(x = date_v0, format = "%Y-%m-%d"),
                .after = date_v0) %>%
  # Remove the preliminary date columns
  dplyr::select(-date_v0, -date, -date_format) %>%
  # Rename date_actual to date
  dplyr::rename(date = date_actual)

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
#    Filling Missing Taxonomic Information ----
## -------------------------------------------- ##

# Create tidy object 
tidy_v2a <- tidy_v1c

# Doing some preliminary wrangling on species names
tidy_v2b <- tidy_v2a %>%
  # Remove underscore from the species name
  dplyr::mutate(scientific_name = gsub(pattern = "_",
                                       replacement = " ",
                                       x = scientific_name)) %>%
  # Replace one of the missing value codes -99999 with NA values
  dplyr::mutate(dplyr::across(.cols = -date, .fns = ~dplyr::na_if(., y = "-99999"))) %>%
  # Replace empty strings with NA values
  dplyr::mutate(dplyr::across(.cols = -date, .fns = ~dplyr::na_if(., y = "")))


taxon_fix <- tidy_v2b %>%
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
  # Remove any non-specific or unidentified species
  dplyr::filter(!stringr::str_detect(scientific_name_fix, pattern = "[:space:]sp.|[:space:]spp.|[:space:]sp|Unidentified")) %>%
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
                                      accepted = T)
    
    # Query species for its common name 
    common_name_results <- taxize::sci2comm(sci = taxon_fix[i,]$scientific_name_fix, 
                                            db = "itis",
                                            accepted = T)
    
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
tidy_v2c <- left_join(tidy_v2b, taxon_fix_v2, by = c("scientific_name" = "scientific_name_fix")) %>%
  # Coalesce taxonomic columns together to fill in missing taxonomic info whenever possible
  mutate(kingdom = dplyr::coalesce(kingdom, kingdom_fix),
         phylum = dplyr::coalesce(phylum, phylum_fix),
         class = dplyr::coalesce(class, class_fix),
         order = dplyr::coalesce(order, order_fix),
         family = dplyr::coalesce(family, family_fix),
         genus = dplyr::coalesce(genus, genus_fix),
         common_name = dplyr::coalesce(common_name, common_name_fix)) %>%
  # Drop the inferior species column (some entries actually have only the epithet instead of the full species)
  dplyr::select(-species) %>%
  # Keep species_fix as the superior species column
  dplyr::rename(species = species_fix) %>%
  # Drop the rest of the columns from the taxon table
  dplyr::select(-dplyr::contains("_fix")) 


## -------------------------------------------- ##
#      Reordering & Changing Column Types ----
## -------------------------------------------- ##

# Check structure
dplyr::glimpse(tidy_v2c)

tidy_v3 <- tidy_v2c %>%
  dplyr::relocate(species, .before = taxa_group) %>%
  dplyr::relocate(sp_code, .after = species) %>%
  dplyr::relocate(density_num_m, .after = subsite) %>%
  dplyr::relocate(drymass_g_m, .before = drymass_g_m2) %>%  
  dplyr::relocate(wetmass_kg, .before = scientific_name) %>%  
  dplyr::relocate(wetmass_g_m2, .before = wetmass_kg) %>%
  dplyr::mutate(dplyr::across(.cols = c(year:day, density_num_m:wetmass_kg), .fns = as.numeric))

## -------------------------------------------- ##
#                   Export ----
## -------------------------------------------- ##

# Create one final tidy object
tidy_final <- tidy_v3

# Check structure
dplyr::glimpse(tidy_final)

# Grab today's date
date <- gsub(pattern = "-", replacement = "", x = Sys.Date())

# Generate a date-stamped file name for this file
( tidy_filename <- paste0(date, "_harmonized_consumer.csv") )

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy"), showWarnings = F)

# Export locally
write.csv(x = tidy_final, file = file.path("tidy", tidy_filename), na = 'NA', row.names = F)

# Export to Drive
googledrive::drive_upload(media = file.path("tidy", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# End ----
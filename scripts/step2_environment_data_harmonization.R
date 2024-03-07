## ------------------------------------------ ##
#       Marine CND -- Data Harmonization
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Sites: SBC, FCE, CCE

# Data Type: Environment

# Purpose:
## Absorbs all raw data files and combines them
## Finishes with a harmonized data file in long format

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, stringr)

# Create necessary sub-folder(s)
dir.create(path = file.path("tier0"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data", "environmental"), showWarnings = F)

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Identify raw data files
# For example, here I'm pulling all the environmental data from Google Drive
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")) %>%
  dplyr::filter(name %in% c("Bottom_temp_all_years_20230724.csv",
                            "LTER_monthly_bottledata_20220930.txt"))

raw_FCE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ei4UmhiU87Mu7mUHdB0ByVW32UWxvVjW")) %>%
  dplyr::filter(name %in% c("bottle_creek_temperature.csv",
                            "mo215_elevation_corrected_water_levels.csv",
                            "fce_water_quality.csv"))

raw_CCE_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/19INhcRd1xBKgDVd1G5W1B3QI4mlBr587")) %>%
  dplyr::filter(name %in% c("BEUTI_monthly.csv",
                            "sdsla_monthly.csv",
                            "mei.csv",
                            "cce_temperature_raw.csv"))

# Combine file IDs
raw_ids <- rbind(raw_SBC_ids, raw_FCE_ids, raw_CCE_ids)

# Identify and download the data key
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name == "CND_Data_Key_spatial.xlsx") %>%
  googledrive::drive_download(file = .$id, path = file.path("tier0",.$name), overwrite = T)

# For each raw data file, download it into its own site folder
for(k in 1:nrow(raw_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_ids[k, ]$id, overwrite = T,
                                path = file.path("tier0", "raw_data", "environmental", raw_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_ids))
}

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
#             Data Harmonizing ----
## ------------------------------------------ ##

# Read in the key
key <- readxl::read_excel(path = file.path("tier0", "CND_Data_Key_spatial.xlsx"),
                          col_types = c(rep("text", 11))) 

# Identify all downloaded files
( raw_files <- dir(path = file.path("tier0", "raw_data", "environmental")) )

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
  
  na_indicators = unique(key_sub$na_indicator)
  # Special cases for reading in certain csv files 
  if (raw_file_name == "LTER_monthly_bottledata_20220930.txt") {
    raw_df_v1 <- read.table(file.path("tier0", "raw_data", "environmental", raw_file_name), header = T,na.strings = na_indicators, sep = ";", check.names = F)
  } 
  else if (raw_file_name == "sdsla_monthly.csv") {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "environmental", raw_file_name), na.strings = na_indicators,check.names = F)
  } 
  else {
    raw_df_v1 <- read.csv(file = file.path("tier0", "raw_data", "environmental", raw_file_name), na.strings = na_indicators)
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
                                        yes = paste0(standardized_column_name, "-", units_fix),
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

# Unlist the list we generated from above
tidy_v0 <- df_list %>%
  purrr::list_rbind(x = .)

# Check that out
dplyr::glimpse(tidy_v0)

# Clean up environment
#rm(list = setdiff(ls(), c("key", "tidy_v0")))
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
  dplyr:: mutate(date=ifelse(raw_filename=="cce_temperature_raw.csv",paste0(substr(date,1,4),"-",substr(date,5,6),"-15"),date)) %>% # cce has only year month, we modify it to fit the date format
  dplyr::mutate(date_format = dplyr::case_when(
    raw_filename == "bottle_creek_temperature.csv" ~ "YYYY-MM-DD",
    raw_filename == "Bottom_temp_all_years_20230724.csv" ~ "YYYY-MM-DD",
    raw_filename == "fce_water_quality.csv" ~ "MM/DD/YY",
    raw_filename == "LTER_monthly_bottledata_20220930.txt" ~ "YYYY-MM-DD",
    raw_filename == "mei.csv" ~ "YYYY-MM-DD",
    raw_filename == "mo215_elevation_corrected_water_levels.csv" ~ "YYYY-MM-DD",
    raw_filename == "sdsla_monthly.csv" ~ "YYYY-MM-DD", # only has year month day
    raw_filename == "cce_temperature_raw.csv" ~ "YYYYMMDD",
    T ~ "UNKNOWN"))

# Check remaining date formats
tidy_v1a %>%
  dplyr::group_by(date_format) %>%
  dplyr::summarize(files = paste(unique(raw_filename), collapse = "; "))

# Remind yourself what year/month/day/date columns do each raw file already contain
key %>%
  dplyr::select(raw_filename, standardized_column_name, data_type) %>%
  dplyr::filter(data_type == "environment") %>%
  dplyr::filter(standardized_column_name %in% c("year","month","day","date")) %>%
  View()

# Break apart the date column depending on the date format
tidy_v1b <- tidy_v1a %>%
  # YYYY-MM-DD
  tidyr::separate_wider_delim(date, delim = "-", names = c("year_fix1", "month_fix1", "day_fix1"), too_few = "align_start", cols_remove = F) %>% 
  # MM/DD/YY 
  tidyr::separate_wider_delim(date, delim = "/", names = c("month_fix2", "day_fix2", "year_fix2"), too_few = "align_start", cols_remove = F) %>%
  # If year_fix2 only has the last two digits of the year, add "20" to the front 
  dplyr::mutate(year_fix2 = ifelse(nchar(year_fix2) == 2, yes = paste0("20", year_fix2), no = year_fix2)) %>%
  # As a result of doing separate_wider_delim() twice, rows with the YYYY-MM-DD dates have values in the month_fix2 column (which we don't want)
  dplyr::mutate(month_fix2 = ifelse(date_format == "YYYY-MM-DD", yes = NA, no = month_fix2)) %>%
  # As a result of doing separate_wider_delim() twice, rows with the MM/DD/YY or MM/DD/YYYY dates have values in the year_fix1 column (which we don't want)
  dplyr::mutate(year_fix1 = ifelse(date_format == "MM/DD/YY", yes = NA, no = year_fix1)) 


# Date wrangling
tidy_v1c <- tidy_v1b %>%
  dplyr::mutate(day = NA, .after = month) %>%
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
  dplyr::mutate(dplyr::across(.cols = c(year, month, day), .fns = ~dplyr::na_if(., y = "NA")))

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
#rm(list = setdiff(ls(), c("tidy_v1c")))

## -------------------------------------------- ##
#      Reordering & Changing Column Types ----
## -------------------------------------------- ##

# Check structure
dplyr::glimpse(tidy_v1c)

tidy_v3 <- tidy_v1c %>%
  dplyr::relocate(year, .after = raw_filename) %>%
  dplyr::relocate(month, .after = year) %>%
  dplyr::relocate(day, .after = month) %>%
  dplyr::relocate(date, .after = day) %>%
  dplyr::relocate(time, .after = date) %>%
  dplyr::relocate(site, .after = time) %>%
  dplyr::relocate(subsite_level1, .after = site) %>%
  dplyr::relocate(`salinity-ppt`, .after = `subsite_level1`) %>%
  dplyr::relocate(`sea_level-mm`, .after = `salinity-ppt`) %>%
  dplyr::relocate(`temperature-celsius`, .after = `sea_level-mm`) %>%  
  dplyr::relocate(`total_nitrogen-umol_l`, .after = `temperature-celsius`) %>%  
  dplyr::relocate(`total_phosphorus-umol_l`, .after = `total_nitrogen-umol_l`) %>%
  dplyr::relocate(`water_level-cm`, .after = `total_phosphorus-umol_l`) %>%  
  dplyr::mutate(dplyr::across(.cols = c(year:day, `salinity-ppt`:`temperature-celsius`), .fns = as.numeric))

# Check structure
dplyr::glimpse(tidy_v3)

## -------------------------------------------- ##
#  Convert Measurement Columns to Long Format
## -------------------------------------------- ##

tidy_v4 <- tidy_v3 %>%
  # Pivot the measurement columns to long format
  tidyr::pivot_longer(cols = `salinity-ppt`:`water_level-cm`,
                      names_to = "measurement_type",
                      values_to = "measurement_value") %>%
  # Create a measurement_unit column from measurement_type
  tidyr::separate_wider_delim(measurement_type, delim = "-", names = c("measurement_type", "measurement_unit"), too_many = "merge") %>%
  # Fix the units by replacing "_" with "/"
  dplyr::mutate(measurement_unit = stringr::str_replace(measurement_unit, pattern = "_", replacement = "/")) %>%
  # Drop rows where measurement_value is NA
  dplyr::filter(!is.na(measurement_value)) 

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
( tidy_filename <- paste0("harmonized_environment_", date, ".csv") )

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy"), showWarnings = F)

# Export locally
write.csv(x = tidy_final, file = file.path("tidy", tidy_filename), na = 'NA', row.names = F)

# Export harmonized dataset to Drive
googledrive::drive_upload(media = file.path("tidy", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# End ----
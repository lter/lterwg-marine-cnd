## ------------------------------------------ ##
#       Marine CND -- Data Harmonization
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Site: SBC

# Data Type: Consumer

# Purpose:
## Absorbs all raw data files and combines them
## Finishes with a harmonized data file in long format

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl)

# Set project
project <- "SBC"

# Create necessary sub-folder(s)
dir.create(path = file.path("tier0"), showWarnings = F)
dir.create(path = file.path("tier0", project), showWarnings = F)

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Identify raw data files
# For example, here I'm pulling all the SBC consumer data from Google Drive
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y"), pattern = "csv|txt") 

# Identify and download the data key
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name == "CND_Data_Key.xlsx") %>%
  googledrive::drive_download(file = .$id, path = file.path("tier0",.$name), overwrite = T)
  
# For each raw data file, download it into its own site folder
for(k in 1:nrow(raw_SBC_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_SBC_ids[k, ]$id, overwrite = T,
                                path = file.path("tier0", project, raw_SBC_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_SBC_ids))
  }

# Clear environment
rm(list = setdiff(ls(), "project"))

# Read in the key
key <- readxl::read_excel(path = file.path("tier0", "CND_Data_Key.xlsx")) 

## ------------------------------------------ ##
#             Data Harmonizing ----
## ------------------------------------------ ##

# Identify all downloaded files
( raw_files <- dir(path = file.path("tier0", project)) )

# Subset the key object a bit
key_sub <- key %>%
  # Only this file's section
  dplyr::filter(raw_filename == "IV_EC_talitrid_population.csv") %>%
  # And only columns that have a synonymized equivalent
  dplyr::filter(!is.na(standardized_column_name) & nchar(standardized_column_name) != 0)

raw_df_v1 <- read.csv(file = file.path("tier0", project, "IV_EC_talitrid_population.csv"))

raw_df_v2 <- raw_df_v1 %>%
  # Create a row number column and a column for the original file
  dplyr::mutate(row_num = 1:nrow(.),
                raw_filename = "IV_EC_talitrid_population.csv",
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
  message("Not all expected columns in '", "IV_EC_talitrid_population.csv", "' are in *data* key!")
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

raw_df_v4 <- raw_df_v3 %>%
  # Separate the date column into year, month, day columns
  tidyr::separate(date, sep = "/", into = c("month", "day", "year"), remove = F) %>%
  # Reorder columns
  dplyr::relocate(year, .before = month)

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

# Identify raw data files
# For example, here I'm pulling all the SBC consumer data from Google Drive
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1TMm93o1F6YzgQ00zFE51aqe4hUwlTYfE")) 

# # Identify data format file
# data_format_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1RgCt4zAC8lHl_4WKsDu07dtfwn4-Xdst")) %>%
#   dplyr::filter(name %in% c("Consumer_data_format.xlsx"))

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

## ------------------------------------------ ##
#               Read in Data ----
## ------------------------------------------ ##

# Read in data
talitrid <- read.csv(file = file.path("tier0", project, "IV_EC_talitrid_population.csv"))
#shorebird <- read.csv(file = file.path("tier0", project, "Shorebird_count_20211012.csv"))
biomass <- read.csv(file = file.path("tier0", project, "Annual_All_Species_Biomass_at_transect_20230814.csv"))
#beach <- read.csv(file = file.path("tier0", project, "Beach_Consumer_Zerofilled_20211011 (1).csv"))

aa<-data.frame(colnames(talitrid))
bb<-data.frame(colnames(biomass))

# # Read in data format
# format <- readxl::read_excel(path = file.path("tier0", "Consumer_data_format.xlsx"))

# Read in the key
key <- readxl::read_excel(path = file.path("tier0", "CND_Data_Key.xlsx")) 


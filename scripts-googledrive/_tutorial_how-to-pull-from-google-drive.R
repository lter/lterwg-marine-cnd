## ------------------------------------------ ##
#                Marine CND 
# Example Data Acquisition from Google Drive
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Purpose:
## An example script showing how you can pull in data from Google Drive to use in your local RStudio session

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# If you don't have the "librarian" package, uncomment the next line and run it to install the package
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Set site
site <- "SBC"

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)
dir.create(path = file.path("raw_data", site), showWarnings = F)

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Identify raw data files
# For example, here I'm pulling all the SBC csv files from Google Drive
# A new window will pop up asking you to select the appropriate Google Drive account
# For more help, see: https://nceas.github.io/scicomp.github.io/tutorials.html#using-the-googledrive-r-package
raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y"),
                                     type = "csv") 

# Identify raw data files
# For example, here I'm pulling specific SBC files from Google Drive
# raw_SBC_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")) %>%
#  dplyr::filter(name %in% c("Annual_All_Species_Biomass_at_transect_20230814.csv",
#                            "IV_EC_talitrid_population.csv"))

# For each raw data file, download it into its own site folder
for(k in 1:nrow(raw_SBC_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_SBC_ids[k, ]$id, overwrite = T,
                                path = file.path("raw_data", site, raw_SBC_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_SBC_ids))
}

# Read in a csv
all_species <- read.csv(file = file.path("raw_data", site, "Annual_All_Species_Biomass_at_transect_20230814.csv"))
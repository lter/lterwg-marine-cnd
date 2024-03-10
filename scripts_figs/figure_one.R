###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN.csv"))

# strata_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
#   dplyr::filter(name %in% c("strata_class.xlsx"))
# 
# # Combine file IDs
# harmonized_ids <- rbind(exc_ids, strata_ids)
# 
# # For each raw data file, download it into the consumer folder
# for(k in 1:nrow(harmonized_ids)){
#   
#   # Download file (but silence how chatty this function is)
#   googledrive::with_drive_quiet(
#     googledrive::drive_download(file = harmonized_ids[k, ]$id, overwrite = T,
#                                 path = file.path("tier2", harmonized_ids[k, ]$name)) )
#   
#   # Print success message
#   message("Downloaded file ", k, " of ", nrow(harmonized_ids))
# }

rm(list = ls()) #cleans env

### read in clean excretion and strata data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =".") |> 
  janitor::clean_names()
glimpse(dt)
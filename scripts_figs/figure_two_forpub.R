###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel, purrr, ggtext)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN_summarized.csv"))

rm(list = ls()) #cleans env

### read in data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN_summarized.csv"),stringsAsFactors = F,na.strings =".") |> 
  mutate(sdate = as.Date(sdate))
glimpse(dt)

label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "FCE", "MCR", "PISCO-Central",
              "PISCO-South", "SBC-Beach", "SBC-Ocean", "NGA", 
              "PIE", "VCR")) 

habitat_mapping <- data.frame(
  color = unique(dt$color),
  Habitat = c("Nearshore", "Offshore","Riverine", "Bay", "Back Reef", "Fore Reef", "Fringing Reef",
              'Marine Protected Area', 'Reference', "Reference", "Seward", "Knight Island Passage",
              "Kodiak Island", "Middleton Island", "Prince William Sound", "CL", "NE", "SW", "WE", "SW", "WE",
              "Hog Island: Seagrass", "Hog Island: Sand", "South Bay: Seagrass", "South Bay: Sand")) 

cols = c("CCE" = '#00008B',
         "FCE" = '#3CB371',
         "MCR" = '#FF7F50',
         "PISCO-Central" = '#708090',
         "PISCO-South" = '#B0C4DE', 
         "SBC-Beach" = '#F4A460',
         "SBC-Ocean" = '#004953',
         "NGA" = '#1E90FF',
         "PIE" = '#556B2F',
         "VCR" = '#7CFC00')

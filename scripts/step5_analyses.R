###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): explore and analyzed harmonized excretion data
###date(s): January 2024

###########################################################################
# load necessary packages -------------------------------------------------
###########################################################################

# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

###########################################################################
# connect to google drive -------------------------------------------------
###########################################################################

# pull in the harmonized data
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) %>%
  dplyr::filter(name %in% c("harmonized_consumer_excretion.csv"))

# Combine file IDs
exc_ids <- rbind(exc_ids)

# For each raw data file, download it into the consumer folder
for(k in 1:nrow(exc_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = exc_ids[k, ]$id, overwrite = T,
                                path = file.path("tier2", exc_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(exc_ids))
}

# Clear environment
rm(list = ls())

###########################################################################
# load harmonized excretion data ------------------------------------------
###########################################################################

# read in the harmonized data and start the wrangling, by project
df <- read.csv(file.path("tier2", "harmonized_consumer_excretion.csv"),
               stringsAsFactors = F,na.strings =".") 
glimpse(df)
unique(df$project)

#FCE is longer (#obs) because individual row = individual animal

fce <- df |> 
  filter(project == "FCE")
glimpse(fce)

#pisco shorter (#obs) because individual row = multiple individuals 
#(i.e., biomass/abundance -> ind biomass -> ind excretion * abundance)

pisco <- df |> 
  filter(project == "CoastalCA")
glimpse(pisco)

#mcr shorter (#obs) because individual row = multiple individuals 
#(i.e., biomass/abundance -> ind biomass -> ind excretion * abundance)

mcr <- df |> 
  filter(project == "MCR")
glimpse(mcr)

#sbc_reef shorter (#obs) because individual row = multiple individuals 
#(i.e., biomass/abundance -> ind biomass -> ind excretion * abundance)
sbc_reef <- df |> 
  filter(project == "SBC",
         habitat == "ocean")

#sbc_beach shorter (#obs) because individual row = multiple individuals 
#(i.e., biomass/abundance -> ind biomass -> ind excretion * abundance)
sbc_beach <- df |> 
  filter(project == "SBC",
         habitat == "beach")

###########################################################################
# FCE Manipulation --------------------------------------------------------
###########################################################################
glimpse(fce)

fce_wide <- fce |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit),
              values_from = measurement_value)
glimpse(fce_wide)


## ------------------------------------------ ##
#       Marine CND -- excretion rate calculation
## ------------------------------------------ ##
# Script author(s): Li Kui

# Sites: SBC, CCE, Coastal CA, FCE, MCR, NGA, PIE, VCR, 

# Data Type: Consumer

# Purpose:
## pull into the harmonized data that is ready for excretion rate
## calculate the excretion rate

## ------------------------------------------ ##
#            User Settings -----
## ------------------------------------------ ##



## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

# Create necessary sub-folder(s)
dir.create(path = file.path("tier2"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)
## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# pull in the harmonized data
consumer_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")) %>%
  dplyr::filter(name %in% c("harmonized_consumer_ready_for_excretion.csv"))

species_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
  dplyr::filter(name %in% c("CNDWG_harmonized_consumer_species.xlsx"))

# Combine file IDs
harmonized_ids <- rbind(consumer_ids, species_ids)

# For each raw data file, download it into the consumer folder
for(k in 1:nrow(harmonized_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = harmonized_ids[k, ]$id, overwrite = T,
                                path = file.path("tier1", harmonized_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(harmonized_ids))
}

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
#             data wrangling for each project ----
## ------------------------------------------ ##

#### read data
# read in the harmonized data and start the wrangling, by project
dt <- read.csv(file.path("tier1", "harmonized_consumer_ready_for_excretion.csv"),stringsAsFactors = F,na.strings =".") 
#dt <- harmonized_clean
species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")

#### read data end 


#### calculate excretion rate

# take out the rows that are needed 

dt1 <-dt %>%
  #use pisco data as an example
  #filter(project=="CoastalCA") %>%
  filter(measurement_type %in% c("dmperind","density","temp")) 

#check the unit that match with the measurement, good to go
peace <- dt1 %>%
  distinct(project,habitat,measurement_type,measurement_unit)

dt2 <- dt1 %>%
  dplyr::select(-measurement_unit) %>%
  pivot_wider(names_from = measurement_type, values_from = measurement_value) 

# merge with the species list
dt3 <-dt2 %>%
  left_join(species_list,by=c("project","sp_code","scientific_name","species")) 

###########################
#using bradley's code below for excretion calculation
cons <- dt3

cons_np_ratio <- cons %>% 
  #N
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = 1.461 + 0.6840*(log10(dmperind)) + 0.0246*temp + N_diet_coef + N_vert_coef,
         Nexc_log10 = 1.461 + 0.6840*(log10(dmperind)) + 0.0246*temp + N_diet_coef + N_vert_coef,
         Nexc_log10 = if_else(Nexc_log10 > 0, Nexc_log10, 0),
         `nind_ug/hr` = Nexc_log10) %>%
  # p
  mutate(P_vert_coef = if_else(phylum == "Chordata", 0.7504, 0),
         P_diet_coef = if_else(diet_cat == "algae_detritus", 0.0173,
                                              if_else(diet_cat == "invert", -0.2480,
                                                      if_else(diet_cat == "fish", -0.0337,
                                                              if_else(diet_cat == "fish_invert", -0.4525, 
                                                                      if_else(diet_cat == "algae_invert",0,
                                                                              NA))))),
         Pexc_log10 = 0.6757 + 0.5656*(log10(dmperind)) + 0.0194*temp + P_diet_coef + P_vert_coef,
         Pexc_log10 = if_else(Pexc_log10 > 0, Pexc_log10, 0),
         `pind_ug/hr` = Pexc_log10) %>%
  #N:P ratio
  mutate(NtoPexc_molar = 58.526 + 13.681*(log10(dmperind)),
         NtoPexc_molar = if_else(NtoPexc_molar > 0, NtoPexc_molar, 0),
         `npind_unitless` = NtoPexc_molar)

##################### end of bradley's code #####################
##### Data clean up #######

dt_final <- cons_np_ratio %>% 
  dplyr::select(-c(common_name,kingdom,phylum,class,order,family,genus,taxa_group,N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10,NtoPexc_molar,diet_cat)) %>%
  pivot_longer(cols = -c(project,habitat,raw_filename,row_num,year,month,day,date,site,subsite_level1,subsite_level2,subsite_level3,sp_code,scientific_name,species), 
             names_to = "measurement_type",
             values_to = "measurement_value") %>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion.csv"

write.csv(dt_final, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier2",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))

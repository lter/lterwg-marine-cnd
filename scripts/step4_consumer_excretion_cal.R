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
df <- read.csv(file.path("tier1", "harmonized_consumer_ready_for_excretion.csv"),stringsAsFactors = F,na.strings =".") 
#df <- harmonized_clean
species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")

#### read data end 


#### calculate excretion rate

# take out the rows that are needed 

df1 <-df %>%
  filter(measurement_type %in% c("dmperind","density","temp")) 

# #check the unit that match with the measurement, good to go
# peace <- df1 %>%
#   distinct(project,habitat,measurement_type,measurement_unit)

df2 <- df1 %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value) 

# check species list before merging, need to check
peace2 <- species_list %>%
  group_by(project,sp_code,scientific_name,species) %>%
  summarise(n=n(),.groups='drop') %>%
  ungroup() 

# there is duplicate, we need to select the first one, temporally solution
spe2 <- species_list %>%
  group_by(project,sp_code,scientific_name,species) %>%
  slice(1) %>%
  ungroup()

# merge with the species list
df3 <-df2 %>%
  left_join(spe2,by=c("project","sp_code","scientific_name","species")) 

# check to see anything that don't have diet cat column
peace3<-df3 %>%
  filter(is.na(diet_cat)) %>%
  distinct(project,habitat,sp_code,scientific_name,species,diet_cat)

###########################
#using bradley's code below for excretion calculation
cons <- df3

cons_np_ratio <- cons %>% 
  #N
  mutate(N_vert_coef = if_else(phylum == "Chordata", 0.7804, 0),
         N_diet_coef = if_else(diet_cat == "algae_detritus", -0.0389,
                               if_else(diet_cat == "invert", -0.2013,
                                       if_else(diet_cat == "fish", -0.0537,
                                               if_else(diet_cat == "fish_invert", -0.1732, 
                                                       if_else(diet_cat == "algae_invert", 0,
                                                               NA))))),
         Nexc_log10 = ifelse(`dmperind_g/ind` > 0, 1.461 + 0.6840*(log10(`dmperind_g/ind`)) + 0.0246*temp_c + N_diet_coef + N_vert_coef,NA),
         `nind_ug/hr` = 10^Nexc_log10,
         `nind_ug/hr` = ifelse(is.na(`nind_ug/hr`),0,`nind_ug/hr`)) %>%
  # p
  mutate(P_vert_coef = if_else(phylum == "Chordata", 0.7504, 0),
         P_diet_coef = if_else(diet_cat == "algae_detritus", 0.0173,
                                              if_else(diet_cat == "invert", -0.2480,
                                                      if_else(diet_cat == "fish", -0.0337,
                                                              if_else(diet_cat == "fish_invert", -0.4525, 
                                                                      if_else(diet_cat == "algae_invert",0,
                                                                              NA))))),
         Pexc_log10 = ifelse(`dmperind_g/ind` >0, 0.6757 + 0.5656*(log10(`dmperind_g/ind`)) + 0.0194*temp_c + P_diet_coef + P_vert_coef, NA),
         `pind_ug/hr` = 10^Pexc_log10,
         `pind_ug/hr` = ifelse(is.na(`pind_ug/hr`),0,`pind_ug/hr`))  
  #N:P ratio we take out the ratio for now
  # mutate(NtoPexc_molar = ifelse(dmperind >0, 58.526 + 13.681*(log10(dmperind)), NA),
  #        NtoPexc_molar = if_else(NtoPexc_molar > 0, NtoPexc_molar, 0),
  #        `npind_unitless` = NtoPexc_molar)


##################### end of bradley's code #####################
##### Data clean up #######

df_final <- cons_np_ratio %>% 
  dplyr::select(-c(common_name,kingdom,phylum,class,order,family,genus,taxa_group,N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10,diet_cat)) %>%
  pivot_longer(cols = -c(project,habitat,raw_filename,row_num,year,month,day,date,site,subsite_level1,subsite_level2,subsite_level3,sp_code,scientific_name,species), 
             names_to = "measurement_type1",
             values_to = "measurement_value") %>%
  separate(measurement_type1, into = c("measurement_type", "measurement_unit"),sep = "_", remove = T) 

# check FCE case
peace4 <- df_final %>%
  filter(project=="FCE")

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion.csv"

write.csv(df_final, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier2",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))

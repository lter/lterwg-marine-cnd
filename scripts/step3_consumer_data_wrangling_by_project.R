## ------------------------------------------ ##
#       Marine CND -- Data wrangling after the Harmonization is done
## ------------------------------------------ ##
# Script author(s): Li Kui

# Sites: SBC, CCE, Coastal CA, FCE, MCR, NGA, PIE, VCR, 

# Data Type: Consumer

# Purpose:
## Data cleaning for each project
## Finishes with a cleaner version of the harmonized data

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
dir.create(path = file.path("tier1"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)
## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# pull in the harmonized data
consumer_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")) %>%
  dplyr::filter(name %in% c("harmonized_consumer.csv"))

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
dt <- read.csv(file.path("tier1", "harmonized_consumer.csv"),stringsAsFactors = F,na.strings =".") 

species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")


#### read data end 


#### COASTAL CA
#read in the site table to filter out the site we need
pisco_site_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name %in% c("master_site_table.xlsx"))

googledrive::with_drive_quiet(
  googledrive::drive_download(file = pisco_site_id$id, overwrite = T, path = file.path("other", pisco_site_id$name)) )

pisco_site <- readxl::read_excel(path = file.path("other", "master_site_table.xlsx")) 

# filter out the site we need
pisco_site1 <- pisco_site %>% 
  dplyr::select(site,Include_Exclude, mlpa_region)

#test to see if the sites are matched, yes, they are mached and we can start the filtering
# peace<-dt %>%
#   dplyr::filter(project=="CoastalCA") %>%
#   dplyr::filter(raw_filename=="MLPA_fish_biomass_density_transect_raw_v2.csv") %>%
#   distinct(site,habitat) %>%
#   full_join(pisco_site1, by="site")

# we want to remove the sites that were not consistently survey in the history and keep the ones that has the long term surveys
site_choose <- pisco_site1 %>% 
  dplyr::filter(Include_Exclude=="Include") %>%
  dplyr::select(site) %>%
  mutate(project="CoastalCA", keep="y") 

# filter to keep the site we need
coastalca_dt <- dt %>% 
  filter(project=="CoastalCA") %>%
  left_join(site_choose, by=c("site","project")) %>%
  filter(!(project=="CoastalCA"&is.na(keep))) %>%
  dplyr::select(-keep) %>%
  # remove the benthic survey because no biomass in the benthic survey
  filter(!(project=="CoastalCA"&raw_filename=="MLPA_benthic_site_means.csv")) 

# calculate the biomass density for coastal CA
coastalca_dt1 <_ coastalca_dt %>%
  dplyr:: select(-measurement_unit) %>%
  pivot_wider(names_from = measurement_type, values_from = measurement_value) %>%
  mutate(biomass_density=biomass/area) %>%
  dplyr:: select(-biomass,-area) 

#### COASTAL CA end


#### SBC
#remove the algae species in the dataset

sbc_species <- species_list %>%
  dplyr::filter(project=="SBC")  %>%
  filter(taxa_group %in% c("MOBILE INVERT","FISH")|is.na(taxa_group))

sbc_dt <- dt %>%
  dplyr::filter(project=="SBC") %>%
  dplyr::filter(sp_code %in% sbc_species$sp_code|is.na(sp_code))

sbc_dt1 <- sbc_dt %>%
  dplyr:: select(-measurement_unit) %>%
  pivot_wider(names_from = measurement_type, values_from = measurement_value)

#### SBC end
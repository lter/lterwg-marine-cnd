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

env_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")) %>%
  dplyr::filter(name %in% c("harmonized_environment.csv"))

species_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
  dplyr::filter(name %in% c("CNDWG_harmonized_consumer_species.xlsx"))

# Combine file IDs
harmonized_ids <- rbind(consumer_ids, env_ids, species_ids)

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

env <- read.csv(file.path("tier1", "harmonized_environment.csv"),stringsAsFactors = F,na.strings =".") 

species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")


#### read data end 


#### COASTAL CA
#read in the site table to filter out the site we need
pisco_site_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name %in% c("master_site_table.xlsx"))

sbc_temp <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")) %>%
  dplyr::filter(name %in% c("Bottom_temp_all_years_20230724.csv"))

googledrive::with_drive_quiet(
  googledrive::drive_download(file = pisco_site_id$id, overwrite = T, path = file.path("other", pisco_site_id$name)) )

googledrive::with_drive_quiet(
  googledrive::drive_download(file = sbc_temp$id, overwrite = T, path = file.path("other", sbc_temp$name)) )

pisco_site <- readxl::read_excel(path = file.path("other", "master_site_table.xlsx")) 

sbc_temp <- env %>%
  filter(project=="SBC") %>%
  filter(measurement_type=="temperature") %>%
  rename(temp=measurement_value)

###calculate temperature from SBC data for summer-fall months (Jul - Oct)
sbc_temp_summer<-sbc_temp %>% 
  filter(month %in% c(7,8,9) )

#take average, min, max of summer temperatures from all sites and years
sbc_temp_ave <- sbc_temp_summer %>%
  dplyr:: summarise(MEAN= mean(temp,  na.rm=TRUE),
                    MAX= max(temp,  na.rm=TRUE),
                    MIN= min(temp,  na.rm=TRUE))


# filter out the site we need
pisco_site1 <- pisco_site %>% 
  dplyr::select(site,Include_Exclude, mlpa_region)

#test to see if the sites are matched, yes, they are matched and we can start the filtering
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


#convert wetmass into dry mass
# calculate the biomass density for coastal CA
coastalca_dt1 <- coastalca_dt %>%
  pivot_wider(names_from = c(measurement_type, measurement_unit),values_from = measurement_value) %>%
  mutate(`drymass_g/m2`=(`wetmass_kg/transect`*0.29/`transect_area_m2`)*1000, #convert from kg to g
         `dmperind_g/ind` = `wetmass_kg/transect`*0.29*1000/`count_num`,
         `density_num/m2` = `count_num`/`transect_area_m2`,
         `temp_c` = sbc_temp_ave$MEAN) 

coastalca_dt2 <-coastalca_dt1 %>%
  pivot_longer(cols = count_num:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")

coastalca_ready <- coastalca_dt2 %>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"), sep = "_", remove = FALSE) 
  
# peace<-coastalca_ready %>%  
#   distinct(measurement_type,measurement_unit)

#### COASTAL CA end



#### SBC ocean
#remove the algae species in the dataset

sbc_species <- species_list %>%
  dplyr::filter(project=="SBC")  %>%
  filter(taxa_group %in% c("MOBILE INVERT","FISH")|is.na(taxa_group))

sbc_dt <- dt %>%
  dplyr::filter(project=="SBC") %>%
  dplyr::filter(habitat=="ocean") %>%
  dplyr::filter(sp_code %in% sbc_species$sp_code|is.na(sp_code))

sbc_dt1 <- sbc_dt %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value) %>%
  mutate(`dmperindv_g/ind`=`drymass_g/m2`/`density_num/m2`,
         temp_c = sbc_temp_ave$MEAN)  #using the ones from coastal CA chunk

sbc_ready<- sbc_dt1 %>%
    pivot_longer(cols = `density_num/m2`:temp_c, 
                 names_to = "measurement_type",
                 values_to = "measurement_value") %>%
   separate(measurement_type, into = c("measurement_type", "measurement_unit"), sep = "_", remove = FALSE) 
 
#### SBC ocean end

### MCR
### MCR Start

mcr <- dt %>%
  filter(project == "MCR")

mcr_d1 <- mcr %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value)

#subsite_level1 = Habitat (BR, FO, FR)
#subsite_level2 = Transect
#subsite_level3 = Swath 
#note the wetmass_g is a total biomass for all species of that size class. You need to divide wetmass_g by count_num to get individual biomass
#biomass calculations are based off the length of each fish 

#selecting the columns we are interested in
mcr_d2 <- mcr_d1 %>% dplyr::select(year, site, subsite_level1, subsite_level2, subsite_level3 ,scientific_name, count_num, length_mm, wetmass_g)

expand_MCR_biomass_new_col <- mcr_d2%>% dplyr::mutate(ind_bio = wetmass_g/count_num) ### obtain the individual biomass by creating a new column using total biomass/count 

View(expand_MCR_biomass_new_col)


#some fish do not have biomass estimates, need to remove those. Fish without biomass are denoted by negative numbers
#one row has an error in the swath measurement, needs to be removed 

MCR_biomass_d3 <- expand_MCR_biomass_new_col %>%
  filter(subsite_level3 != 2) %>%
  filter(wetmass_g > 0)

#need to zero fill our data. Need to divide into two swath sizes (1 = 50 m2, 5 = 250m2). Different swaths sizes look for different fish species

#swath 50m
MCR_biomass_swath1 <- MCR_biomass_d3 %>%
  filter(subsite_level3 == 1)

MCR_sw1_possiblecombos <- MCR_biomass_swath1 %>%
  distinct(year, site, scientific_name, subsite_level1, subsite_level2, subsite_level3) %>%
  tidyr::expand(year, site, scientific_name, subsite_level1, subsite_level2, subsite_level3)

MCR_sw1_NAs <- left_join(MCR_sw1_possiblecombos, MCR_biomass_swath1)

View(MCR_sw1_NAs)

#replace NAs with zeros

na_count_num_filled_s1 <- which(is.na(MCR_sw1_NAs$count_num))
MCR_sw1_NAs$count_num[is.na(MCR_sw1_NAs$count_num)]<-0 ###fill in zeros for count_num

na_length_mm_filled_s1 <- which(is.na(MCR_sw1_NAs$length_mm))
MCR_sw1_NAs$length_mm[is.na(MCR_sw1_NAs$length_mm)] <- 0 ###fill in zeros for length

na_Biomass_filled_s1 <- which(is.na(MCR_sw1_NAs$wetmass_g))
MCR_sw1_NAs$wetmass_g[is.na(MCR_sw1_NAs$wetmass_g)] <- 0 ###fill in zeros for biomass 

na_ind_bio_filled_s1 <- which(is.na(MCR_sw1_NAs$ind_bio))
MCR_sw1_NAs$ind_bio[is.na(MCR_sw1_NAs$ind_bio)]<-0 ###fill in zeros for individual biomass


#swath 250m2
MCR_biomass_swath5 <- MCR_biomass_d3 %>%
  filter(subsite_level3 == 5)

MCR_sw5_possiblecombos <- MCR_biomass_swath5 %>%
  distinct(year, site, scientific_name, subsite_level1, subsite_level2, subsite_level3) %>%
  tidyr::expand(year, site, scientific_name, subsite_level1, subsite_level2, subsite_level3)

MCR_sw5_NAs <- left_join(MCR_sw5_possiblecombos, MCR_biomass_swath5)

View(MCR_sw5_NAs)

#replace NAs with zeros

na_count_num_filled_s5 <- which(is.na(MCR_sw5_NAs$count_num))
MCR_sw5_NAs$count_num[is.na(MCR_sw5_NAs$count_num)]<-0 ###fill in zeros for count_num

na_length_mm_filled_s5 <- which(is.na(MCR_sw5_NAs$length_mm))
MCR_sw5_NAs$length_mm[is.na(MCR_sw5_NAs$length_mm)] <- 0 ###fill in zeros for length

na_Biomass_filled_s5 <- which(is.na(MCR_sw5_NAs$wetmass_g))
MCR_sw5_NAs$wetmass_g[is.na(MCR_sw5_NAs$wetmass_g)] <- 0 ###fill in zeros for biomass 

na_ind_bio_filled_s5 <- which(is.na(MCR_sw5_NAs$ind_bio))
MCR_sw5_NAs$ind_bio[is.na(MCR_sw5_NAs$ind_bio)]<-0 ###fill in zeros for individual biomass

#this includes both swaths and is zero filled 
mcr_biomass_final <- rbind(MCR_sw1_NAs, MCR_sw5_NAs)

#now we are going to attach the species list, which includes the diet information 

mcr_diet <- species_list %>%
  filter(project == "MCR")

mcr_diet_cat <- merge(mcr_biomass_final, mcr_diet, by= "scientific_name")




#### Concat all the data together again

# pick out the ones that don't need to be edited
data_original <- dt %>%
  dplyr::filter(project=="SBC"&habitat=="beach") 
  
# concat data together
harmonized_clean = rbind(data_original,coastalca_ready, sbc_ready)

#### concat end


# write it back to the google drive
# Export locally
tidy_filename <- "harmonized_consumer_ready_for_excretion.csv"

write.csv(harmonized_clean, file = file.path("tier1", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier1",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

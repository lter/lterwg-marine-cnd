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
  dplyr::filter(name %in% c("temperature_allsites.csv"))

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
dt <- read.csv(file.path("tier1", "harmonized_consumer.csv"),stringsAsFactors = F,na.strings =".") %>%
  mutate(measurement_type=gsub("_", "", measurement_type))

env <- read.csv(file.path("tier1", "temperature_allsites.csv"),stringsAsFactors = F,na.strings =".") 

species_list <- readxl::read_excel(path = file.path("tier1", "CNDWG_harmonized_consumer_species.xlsx"),na=".")


#### read data end 


#### COASTAL CA
#read in the site table to filter out the site we need
pisco_site_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name %in% c("master_site_table.xlsx"))

googledrive::with_drive_quiet(
  googledrive::drive_download(file = pisco_site_id$id, overwrite = T, path = file.path("other", pisco_site_id$name)) )

pisco_site <- readxl::read_excel(path = file.path("other", "master_site_table.xlsx"),na="N/A") 

# filter out the site we need
pisco_site1 <- pisco_site %>% 
  dplyr::select(site,Include_Exclude, mlpa_region,site_status)

#test to see if the sites are matched, yes, they are matched and we can start the filtering
# peace<-dt %>%
#   dplyr::filter(project=="CoastalCA") %>%
#   dplyr::filter(raw_filename=="MLPA_fish_biomass_density_transect_raw_v2.csv") %>%
#   distinct(site,habitat) %>%
#   full_join(pisco_site1, by="site")

# we want to remove the sites that were not consistently survey in the history and keep the ones that has the long term surveys
pisco_site_choose <- pisco_site1 %>% 
  dplyr::filter(Include_Exclude=="Include") %>%
  mutate(project="CoastalCA", keep="y") 

# filter to keep the site we need
coastalca_dt <- dt %>% 
  filter(project=="CoastalCA") %>%
  left_join(pisco_site_choose, by=c("site","project")) %>%
  filter(!(project=="CoastalCA"&is.na(keep))) %>%
  dplyr::select(-keep) %>%
  # remove the benthic survey because no biomass in the benthic survey
  filter(!(project=="CoastalCA"&raw_filename=="MLPA_benthic_site_means.csv"))

# Jenn C want to keep the region and MPA status and remove the outer/inner and top_mid_bot category, remove the top canopy because the survey was inconsistent
coastalca_dt1 <- coastalca_dt %>%
  filter(subsite_level3!="CAN") %>% # no record directly from the top canopy
  mutate(subsite_level3 = subsite_level2,
         subsite_level2 = site,
         subsite_level1 = site_status,
         site=mlpa_region) %>%
  dplyr::select(- c(Include_Exclude, mlpa_region, site_status))

#convert wetmass into dry mass
# calculate the biomass density for coastal CA
coastalca_dt2 <- coastalca_dt1 %>%
  pivot_wider(names_from = c(measurement_type, measurement_unit),values_from = measurement_value) %>%
  dplyr::select(-row_num) 


#zero fill the pisco data, because every year the specie might get added, so we only zero fill in a given year. 
coastalca_dt3 <- coastalca_dt2 %>%
  mutate(year=as.character(year), 
         campus = ifelse(subsite_level2 %in% c("POINT_VICENTE_W",  # note that these are VRG campus sites and they have different species list compared to the UCSB and UCSC campus
                                               "ROCKY_POINT_N",
                                               "LONG_POINT_E",
                                               "RIDGES_N",
                                               "ABALONE_COVE_KELP_W",
                                               "BUNKER_POINT"), "VRG","other")) %>%
  group_by(year,campus) %>%
  complete(nesting(sp_code,scientific_name,species),
           nesting(project,habitat,raw_filename,month,day,date,
                                                          site, subsite_level1, subsite_level2, subsite_level3,transectarea_m2),
         fill = list(count_num=0, length_cm=NA,`wetmass_kg/transect` = 0)) %>%
  ungroup() %>%
  dplyr::select(-campus) %>% # remove the column after zero filled
  filter(sp_code!="NO_ORG") #REMOVE THE SPECIES THAT ARE NOT ORGANISM

# extract temperature, PISCO use SBC temperature
pisco_temp <- env$temp[env$project=="SBC"]

coastalca_dt4 <- coastalca_dt3 %>%
  mutate(`drymass_g/m2`=(`wetmass_kg/transect`*0.29/`transectarea_m2`)*1000, #convert from kg to g
         `dmperind_g/ind` = ifelse(count_num>0,`wetmass_kg/transect`*0.29*1000/`count_num`,0), 
         `density_num/m2` = `count_num`/`transectarea_m2`,
         `temp_c` = pisco_temp) %>%
   mutate(row_num = paste0(raw_filename, "_", 1:nrow(.))) #adding the row_num back
 

coastalca_dt5 <-coastalca_dt4 %>%
  pivot_longer(cols = transectarea_m2:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")

coastalca_ready <- coastalca_dt5 %>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 

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
  #if there is a shell-free drymass we used it ,otherwise, we use dry mass
  mutate(`drymass_g/m2` = ifelse(!is.na(`sfdrymass_g/m2`),`sfdrymass_g/m2`,`drymass_g/m2`)) %>%
  mutate(`dmperind_g/ind`=ifelse(`density_num/m2`>0,`drymass_g/m2`/`density_num/m2`,0),
         temp_c = pisco_temp)  #pisco uses SBC temp, so we assign the value here again. 

sbc_ready<- sbc_dt1 %>%
    pivot_longer(cols = `density_num/m2`:temp_c, 
                 names_to = "measurement_type",
                 values_to = "measurement_value") %>%
   separate(measurement_type, into = c("measurement_type", "measurement_unit"), sep = "_",remove = FALSE)
  
sbc_ready <-filter(sbc_ready,measurement_type!="sfdrymass") #drop the sfdrymass_g/m2 value
 
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
#keep all columns for now, might want to select later on
#mcr_d2 <- mcr_d1 %>% dplyr::select(project, year, site, subsite_level1, subsite_level2, subsite_level3 ,scientific_name, count_num, length_mm, wetmass_g)

expand_MCR_biomass_new_col <- mcr_d1%>% dplyr::mutate(ind_bio = wetmass_g/count_num) ### obtain the individual biomass by creating a new column using total biomass/count 

#View(expand_MCR_biomass_new_col)

#some fish do not have biomass estimates, need to remove those. Fish without biomass are denoted by negative numbers
#one row has an error in the swath measurement, needs to be removed 

MCR_biomass_d3 <- expand_MCR_biomass_new_col %>%
  filter(subsite_level3 != 2) %>%
  filter(wetmass_g > 0)

#need to zero fill our data. Need to divide into two swath sizes (1 = 50 m2, 5 = 250m2). Different swaths sizes look for different fish species

#subsite swath 50m
MCR_biomass_swath1 <- MCR_biomass_d3 %>%
  filter(subsite_level3 == 1)


#zero fill 
MCR_sw1_final <- MCR_biomass_swath1 %>%
  complete(nesting(scientific_name, species, sp_code),
           nesting(project, habitat, raw_filename, year, month, day, date, site, subsite_level1, subsite_level2, subsite_level3),
           fill = list(count_num=0, length_mm= NA, wetmass_g=0, ind_bio = 0))


#swath 250m2
MCR_biomass_swath5 <- MCR_biomass_d3 %>%
  filter(subsite_level3 == 5)


#zero fill 
MCR_sw5_final <- MCR_biomass_swath5 %>%
  complete(nesting(scientific_name, species, sp_code),
           nesting(project, habitat, raw_filename, year, month, day, date, site, subsite_level1, subsite_level2, subsite_level3),
           fill = list(count_num=0, length_mm= NA, wetmass_g=0, ind_bio = 0))


#this includes both swaths and is zero filled 
mcr_biomass_final <- rbind(MCR_sw1_final, MCR_sw5_final)

#now we are going to attach the species list, which includes the diet information 

mcr_diet <- species_list %>%
  filter(project == "MCR")

mcr_diet_cat <- merge(mcr_biomass_final, mcr_diet, by= c("scientific_name", "species", "sp_code", "project"))

# dm conversion download from google drive

dm_con_sr <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a")) %>%
  dplyr::filter(name %in% c("dm_conversions_cndwg.xlsx"))

googledrive::with_drive_quiet(
  googledrive::drive_download(file = dm_con_sr$id, overwrite = T, path = file.path("other", dm_con_sr$name)) )

dm_conv1 <- readxl::read_excel(path = file.path("other", "dm_conversions_cndwg.xlsx"),na="NA") 
##

dm_conv <- dm_conv1 |> #read_csv("other/dm_conversions_cndwg.csv") |> 
  select(-level) |> #removes level for simplicity
  filter(kingdom == "Animalia",
         dm_wm_mean < 1) #only want animals and no coefficients greater than 1 (thats wrong)

dm_coeff <- dm_conv |> 
  group_by(class) |> 
  summarise(dm_coeff= mean(as.numeric(dm_wm_mean), na.rm = T),.groups = "drop") |>
  ungroup()

mcr_dm_coeff <- left_join(mcr_diet_cat, dm_coeff, by = "class")
#glimpse(mcr_dm_coeff)
na_coeff_result <- which(is.na(mcr_dm_coeff$dm_coeff)) #yay

mcr_all_dm <- mcr_dm_coeff |> 
  mutate(`dmperind_g/ind` = ind_bio*dm_coeff,
         subsite_level3 = as.numeric(subsite_level3),
         `transectarea_m2` = subsite_level3*50,
         `density_num/m2` = count_num/transectarea_m2,
        `wetmass_g/m2` = wetmass_g/`transectarea_m2`,
         temp_c = 26.5) # mcr assign temp here

mcr_all_dm1 <- mcr_all_dm |> 
  mutate(row_num = paste0(raw_filename, "_", 1:nrow(mcr_all_dm))) |>
  dplyr::select(project,habitat,raw_filename,row_num,year,month,day,date,site,subsite_level1,subsite_level2,subsite_level3,sp_code,scientific_name,species,
                count_num,length_mm,`wetmass_g/m2`,`dmperind_g/ind`,`transectarea_m2`,`density_num/m2`,temp_c) 
 
mcr_ready <-mcr_all_dm1 %>%
  pivot_longer(cols = count_num:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")%>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 

#### MCR end
# rm(mcr, mcr_all_dm, mcr_all_dm1, mcr_biomass_final, mcr_d1, mcr_diet, mcr_diet_cat,
#    mcr_dm_coeff, MCR_sw1_final, MCR_sw5_final, MCR_biomass_d3, MCR_biomass_swath1,
#    MCR_biomass_swath5, expand_MCR_biomass_new_col)

# GCE start ---------------------------------------------------------------
## touched base w Amanda Spivak & Jimmy Nelson re: data questions and have yet
### to hear back from them.. move forward without them for time being
# GCE end -----------------------------------------------------------------

# NGA start ---------------------------------------------------------------

nga <- dt %>%
  filter(project == "NGA")

nga_d1 <- nga %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value)

#site = site
#subsite_level1 = transect
#note the wetmass_mg/m3 is a total biomass for all species of that size class. 
#you need to divide wetmass_mg/m3 by density_num/m3 to get individual biomass and 
# multiply by "0.001" to get wetmass/ind in g

expand_NGA_biomass_new_col <- nga_d1 %>%
  ### obtain the individual biomass by creating a new column
  dplyr::mutate(ind_bio = (`wetmass_mg/m3`/`density_num/m3`*0.001)) 
glimpse(expand_NGA_biomass_new_col) #missing small # of ind_bio estimates

###calculate avg species ind_biom
nga_sp_avg_ind_bio <- expand_NGA_biomass_new_col |> 
  filter(ind_bio != 0) |> 
  group_by(scientific_name) |> 
  summarize(avg_bio = mean(ind_bio))

###replace zeros with average and rename similar to mcr dataset
expand_NGA_biomass_new_col <- expand_NGA_biomass_new_col |> 
  mutate(count = count_num) |> 
  dplyr::select(-count_num) |> 
  left_join(nga_sp_avg_ind_bio, by = "scientific_name") |> 
  mutate(ind_bio = ifelse(ind_bio == 0, avg_bio, ind_bio),
         wetmass_g = `wetmass_mg/m3`*0.001,
         count_num = `density_num/m3`) |> 
  dplyr::select(-avg_bio, -`wetmass_mg/m3`, `density_num/m3`)


###zero-fill the data
nga_zerofill <- expand_NGA_biomass_new_col |> 
  complete(nesting(scientific_name, species, sp_code),
           nesting(project, habitat, raw_filename, year, month, 
                   day, date, site, subsite_level1),
           fill = list(count_num = 0, wetmass_g = 0, ind_bio = 0,
                       length_um = NA))

###join with species list
nga_diet <- species_list |> 
  filter(project == "NGA")

nga_diet_cat <- merge(nga_zerofill, nga_diet, 
                      by= c("scientific_name", "species", "sp_code", "project"))

###dry mass conversions
dm_conv <- dm_conv1 |> #read_csv("other/dm_conversions_cndwg.csv") |> 
  select(-level) |> #removes level for simplicity
  filter(kingdom == "Animalia",
         dm_wm_mean < 1) #only want animals and no coefficients greater than 1 (thats wrong)

dm_coeff <- dm_conv |> 
  group_by(class) |> 
  summarise(dm_coeff= mean(as.numeric(dm_wm_mean), na.rm = T),.groups = "drop") |>
  ungroup()

nga_dm_coeff <- left_join(nga_diet_cat, dm_coeff, by = "class")
# glimpse(nga_dm_coeff)
na_coeff_result <- which(is.na(nga_dm_coeff$dm_coeff)) 

### for small number of NAs take average coefficient
nga_dm <- nga_dm_coeff |> 
  mutate(dm_coeff = replace(dm_coeff, is.na(dm_coeff), 0.1916655))
# na_coeff_result <- which(is.na(nga_dm$dm_coeff)) 

nga_all_dm <- nga_dm |> 
  mutate(`dmperind_g/ind` = ind_bio*dm_coeff,
         `density_num/m3` = count_num,
         `wetmass_g/m3` = wetmass_g,
         `drymass_g/m3` = `wetmass_g/m3`*dm_coeff,
         temp_c = 11.2)
glimpse(nga_all_dm)

nga_all_dm1 <- nga_all_dm |> 
  mutate(row_num = paste0(raw_filename, "_", 1:nrow(nga_all_dm)),
         count_num = count) |>
  dplyr::select(project,habitat,raw_filename,row_num,year,month,
                day,date,site,subsite_level1,subsite_level2,subsite_level3,
                sp_code,scientific_name,species,
                count_num,length_um,`wetmass_g/m3`,`dmperind_g/ind`,
                `density_num/m3`,`excretionegestion_ug/m3`,
                `drymass_g/m3`,temp_c) 

nga_ready <- nga_all_dm1 %>%
  pivot_longer(cols = count_num:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")%>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 


# NGA end -----------------------------------------------------------------
# rm(nga, nga_all_dm, nga_all_dm1, nga_d1, nga_diet, nga_diet_cat, nga_dm,
#    nga_dm_coeff, nga_sp_avg_ind_bio, nga_zerofill, expand_NGA_biomass_new_col)

# PIE start ---------------------------------------------------------------

pie <- dt %>%
  filter(project == "PIE") |> 
  filter(year != 1900)

pie_d1 <- pie %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value)
glimpse(pie_d1)
#site = site
#subsite_level1 = transect
#note the wetmass_g is a total biomass for all species of that size class. 
#you need to divide wetmass_g by density_num/m2 to get individual biomass 
#to get wetmass/ind in g

test <- pie_d1 |> 
  group_by(year) |> 
  mutate(transect = count_num/`density_num/m2`) |> 
  summarise(avg_transect = mean(transect, na.rm = TRUE))

pie_d2 <- left_join(pie_d1, test, by = "year")
glimpse(pie_d2)
pie_d2$avg_transect <- floor(pie_d2$avg_transect)

expand_PIE_biomass_new_col <- pie_d2 %>%
  select(-transectarea_m2, -`density_num/m2`) |> 
  ### obtain the individual biomass by creating a new column
  dplyr::mutate(`density_num/m2` = count_num/avg_transect,
                ind_bio = wetmass_g/`density_num/m2`) 
glimpse(expand_PIE_biomass_new_col) #missing small # of ind_bio estimates

###calculate avg species ind_bio
pie_sp_avg_ind_bio <- expand_PIE_biomass_new_col |> 
  filter(!is.na(ind_bio)) |> 
  group_by(scientific_name) |> 
  summarize(avg_bio = mean(ind_bio, na.rm = TRUE))

###replace zeros with average and rename similar to mcr dataset
expand_PIE_biomass_new_col <- expand_PIE_biomass_new_col |> 
  mutate(count = count_num) |> 
  dplyr::select(-count_num) |> 
  left_join(pie_sp_avg_ind_bio, by = "scientific_name") |> 
  mutate(ind_bio = ifelse(is.na(ind_bio), avg_bio, ind_bio)) |> 
  dplyr::select(-avg_bio)
glimpse(expand_PIE_biomass_new_col)

###zero-fill the data
pie_zero_fill <- expand_PIE_biomass_new_col |> 
  complete(nesting(scientific_name, species, sp_code),
           nesting(project, habitat, raw_filename, year, month, 
                   day, date, site, subsite_level1, subsite_level2),
           fill = list(count = 0, ind_bio = 0, `density_num/m2` = 0,
                       length_mm = NA))

###join with species list
pie_diet <- species_list |> 
  filter(project == "PIE")

pie_diet_cat <- merge(pie_zero_fill, pie_diet, 
                      by= c("scientific_name", "species", "sp_code", "project"))

###dry mass conversions
dm_conv <- dm_conv1 |> #read_csv("other/dm_conversions_cndwg.csv") |> 
  select(-level) |> #removes level for simplicity
  filter(kingdom == "Animalia",
         dm_wm_mean < 1) #only want animals and no coefficients greater than 1 (thats wrong)

dm_coeff <- dm_conv |> 
  group_by(class) |> 
  summarise(dm_coeff= mean(as.numeric(dm_wm_mean), na.rm = T),.groups = "drop") |>
  ungroup()

pie_dm_coeff <- left_join(pie_diet_cat, dm_coeff, by = "class")
# glimpse(nga_dm_coeff)
na_coeff_result <- which(is.na(pie_dm_coeff$dm_coeff)) #yay

# ### for small number of NAs take average coefficient
# pie_dm <- pie_dm_coeff |> 
#   mutate(dm_coeff = replace(dm_coeff, is.na(dm_coeff), 0.1916655))
# # na_coeff_result <- which(is.na(nga_dm$dm_coeff)) 

pie_all_dm <- pie_dm_coeff |> 
  mutate(`dmperind_g/ind` = ind_bio*dm_coeff,
         `density_num/m2` = `density_num/m2`,
         temp_c = 15.2)
glimpse(pie_all_dm)

### na check
na_count_per_column <- sapply(pie_all_dm, function(x) sum(is.na(x)))
print(na_count_per_column)

### missing some site data it appears
site_test <- pie_all_dm |> 
  select(site, subsite_level1, subsite_level2, subsite_level3) |> 
  distinct() #missing some of the site information
### not terrible - so just filter out below - can't fix this

pie_all_dm1 <- pie_all_dm |> 
  filter(!is.na(subsite_level1))
na_count_per_column <- sapply(pie_all_dm1, function(x) sum(is.na(x)))
print(na_count_per_column)
glimpse(pie_all_dm1)

pie_all_dm2 <- pie_all_dm1 |> 
  mutate(row_num = paste0(raw_filename, "_", 1:nrow(pie_all_dm1)),
         count_num = count,
         `dmperind_g/ind` = ind_bio) |>
  dplyr::select(project,habitat,raw_filename,row_num,year,month,
                day,date,site,subsite_level1,subsite_level2,subsite_level3,
                sp_code,scientific_name,species,
                count_num,length_mm,`dmperind_g/ind`,
                `density_num/m2`,temp_c) 

pie_ready <- pie_all_dm2 %>%
  pivot_longer(cols = count_num:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")%>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 

# PIE end -----------------------------------------------------------------
# rm(pie, pie_all_dm, pie_all_dm1, pie_all_dm2, pie_d1, pie_d2, pie_diet, pie_diet_cat,
#    pie_dm_coeff, pie_sp_avg_ind_bio, pie_zero_fill, expand_PIE_biomass_new_col,
#    site_test,test)

# VCR start ------------------------------------------------------------
### NA in species column with count zero represents no fishes collected @ site

vcr <- dt %>%
  filter(project == "VCR")

vcr_d1 <- vcr %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value)

###split site into site and subsite_level1
vcr_d2 <- vcr_d1 |> 
  separate(site, into = c("site", "subsite_level1"), sep = "_", extra = "merge", fill = "right")
glimpse(vcr_d2)

expand_VCR_biomass_new_col <- vcr_d2 %>%
  ### obtain the individual biomass by creating a new column
  dplyr::mutate(`density_num/m2` = count_num/transectarea_m2,
                ind_bio = wetmass_g/count_num,
                ind_bio = replace(ind_bio, is.na(ind_bio), 0),
                wetmass_g = replace(wetmass_g, is.na(wetmass_g),0),
                `wetmass/m2` = wetmass_g/transectarea_m2,
                scientific_name = ifelse(is.na(scientific_name), "Gerreidae", scientific_name)) #fill in w real species so we dont lose true zeros in dataset
glimpse(expand_VCR_biomass_new_col) #missing small # of ind_bio estimates

###zero-fill the data
vcr_zerofill <- expand_VCR_biomass_new_col |> 
  complete(nesting(scientific_name, species, sp_code),
           nesting(project, habitat, raw_filename, year, month, 
                   day, date, site, subsite_level1),
           fill = list(count_num = 0, wetmass_g = 0, ind_bio = 0,
                       `density_num/m2` = 0, `wetmass/m2` = 0,
                       length_cm = NA))
glimpse(vcr_zerofill)

### check to see if we need to remove south bay bare sites after 2017 according to Max C
# test <- vcr_zerofill |> 
#   filter(year > 2017) |> 
#   mutate(rm = paste(site, subsite_level1, sep = "_"))
# unique(test$rm)

### nope, zero-fill accounted for missing data 2017 and after at South Bay Bare Sites


###join with species list
vcr_diet <- species_list |> 
  filter(project == "VCR")

vcr_diet_cat <- merge(vcr_zerofill, vcr_diet, 
                      by= c("scientific_name", "species", "sp_code", "project"))

###dry mass conversions
dm_conv <- dm_conv1 |> #read_csv("other/dm_conversions_cndwg.csv") |> 
  select(-level) |> #removes level for simplicity
  filter(kingdom == "Animalia",
         dm_wm_mean < 1) #only want animals and no coefficients greater than 1 (thats wrong)

dm_coeff <- dm_conv |> 
  group_by(class) |> 
  summarise(dm_coeff= mean(as.numeric(dm_wm_mean), na.rm = T),.groups = "drop") |>
  ungroup()

vcr_dm_coeff <- left_join(vcr_diet_cat, dm_coeff, by = "class")
# glimpse(nga_dm_coeff)
na_coeff_result <- which(is.na(vcr_dm_coeff$dm_coeff)) #yay

# ### for small number of NAs take average coefficient
# vcr_dm <- vcr_dm_coeff |> 
#   mutate(dm_coeff = replace(dm_coeff, is.na(dm_coeff), 0.1916655))
# # na_coeff_result <- which(is.na(nga_dm$dm_coeff)) 
glimpse(vcr_dm_coeff)

vcr_all_dm <- vcr_dm_coeff |> 
  mutate(`dmperind_g/ind` = ind_bio*dm_coeff,
         temp_c = 24.0)
glimpse(vcr_all_dm)

vcr_all_dm1 <- vcr_all_dm |> 
  mutate(row_num = paste0(raw_filename, "_", 1:nrow(vcr_all_dm)),
         `drymass_m2` = `wetmass/m2`*dm_coeff) |>
  dplyr::select(project,habitat,raw_filename,row_num,year,month,
                day,date,site,subsite_level1,subsite_level2,subsite_level3,
                sp_code,scientific_name,species,
                count_num,length_cm,`density_num/m2`,
                `dmperind_g/ind`, `drymass_m2`, temp_c) 

na_count_per_column <- sapply(vcr_all_dm1, function(x) sum(is.na(x)))
print(na_count_per_column)

vcr_ready <- vcr_all_dm1 %>%
  pivot_longer(cols = count_num:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value")%>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"),sep = "_", remove = FALSE) 

# VCR end -----------------------------------------------------------------
# rm(vcr, vcr_all_dm, vcr_all_dm1, vcr_d1, vcr_d2, vcr_diet, vcr_diet_cat,
#    vcr_dm_coeff, vcr_zerofill, expand_VCR_biomass_new_col)

#### CCE start

#extract temp data
cce_mean_temp <- env$temp[env$project=="CCE"]
         
# calculate the dmperind dry biomass 
cce <- dt %>%
  dplyr::filter(project=="CCE") %>%
  filter(!(is.na(scientific_name)|scientific_name=="others")) %>% # there was a species named "others" in the data that don't have scientific name, Dante suggests we removed it from the list 
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value) %>%
  mutate(`dmperind_g/ind`=ifelse(`density_num/m2`>0,`drymass_g/m2`/`density_num/m2`,0),
         temp_c = cce_mean_temp) 

cce_ready<- cce %>%
  pivot_longer(cols = `density_num/m2`:temp_c, 
               names_to = "measurement_type",
               values_to = "measurement_value") %>%
  separate(measurement_type, into = c("measurement_type", "measurement_unit"), sep = "_",remove = FALSE) 


#### CCE end


#### Concat all the data together again

# pick out the ones that don't need to be edited
data_original <- dt %>%
  dplyr::filter((project=="SBC"&habitat=="beach") |
                  project=="FCE")
  
# concat data together
harmonized_clean = rbind(data_original,
                         coastalca_ready,sbc_ready,mcr_ready,nga_ready,
                         pie_ready, sbc_ready, coastalca_ready, vcr_ready,
                         cce_ready)

na_count_per_column <- sapply(harmonized_clean, function(x) sum(is.na(x)))
print(na_count_per_column)

month_nas <- na_count_per_column <- harmonized_clean |> 
  filter(is.na(month))
unique(month_nas$year)

fce_original <- dt |> 
  filter(project == "FCE") |> 
  filter(year == 2020)

fce_monthtest <- fce_original |> 
  group_by(month) |> 
  summarise(n = n())

### went back to og data and determined it was december 2020 that was improperly 
### code for a subset

harmonized_clean$month[is.na(harmonized_clean$month)] <- 12
na_count_per_column <- sapply(harmonized_clean, function(x) sum(is.na(x)))
print(na_count_per_column)

value_nas <- harmonized_clean |> 
  filter(is.na(measurement_value)) |> 
  group_by(measurement_type) |> 
  summarize(n = n())
### check to see what dmperind data we are missing (missing total of 1002)
value_nas <- harmonized_clean |> 
  filter(is.na(measurement_value)) |> 
  filter(measurement_type == "dmperind")
### missing 1002, all from CCE, which I have already talked to them about

# # check to see the measurement type and unit are the same
# peace <- harmonized_clean %>%
#   distinct(project,habitat,measurement_type,measurement_unit)

#### concat end


# write it back to the google drive
# Export locally
tidy_filename <- "harmonized_consumer_ready_for_excretion.csv"

write.csv(harmonized_clean, file = file.path("tier1", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier1",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

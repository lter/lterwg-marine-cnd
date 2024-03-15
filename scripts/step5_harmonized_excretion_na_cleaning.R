###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): clean up missing data in harmonized_consumer_excretion.csv where possible
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion.csv"))
rm(list = ls()) #cleans env

### read in data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion.csv"),stringsAsFactors = F,na.strings =".") 
glimpse(dt)

# data manipulation -------------------------------------------------------

dt_wide <- dt |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit), 
              values_from = measurement_value) 
glimpse(dt_wide)  

# FCE hydroyear to year fix -----------------------------------------------
dt_wide <- dt_wide |> 
  mutate(year = if_else(project == "FCE" & month < 10, year + 1, year))

### check for NAs
na_count_per_column <- sapply(dt_wide, function(x) sum(is.na(x)))
print(na_count_per_column) #1002 observations of NAs in dmperind_g/ind

### check out NAs in dmperind_g/ind column
NA_dmperind_g <- dt_wide |> 
  filter(is.na(`dmperind_g/ind`))
unique(NA_dmperind_g$project) #CCE only project
unique(NA_dmperind_g$scientific_name) #number of species

### try to find average for each species
cce_avg_dm_species <- dt_wide |> 
  filter(project == "CCE") |> 
  group_by(project, scientific_name) |> 
  summarize(avg_dmperind = mean(`dmperind_g/ind`, na.rm = TRUE),
            n = n())
print(cce_avg_dm_species)

#Copepoda does not have any estimate...pyrsomes missing estimates for 2006-2009
dt_wide1 <- dt_wide |> 
  filter(!(project == "CCE" & scientific_name == "Copepoda"))

NA_dmperind_g <- dt_wide1 |> 
  filter(is.na(`dmperind_g/ind`))
unique(NA_dmperind_g$scientific_name) #Copepoda removed :)

### add species average to remainder of NAs
cce_spp_avg <- dt_wide1 |> 
  filter(project == "CCE", !is.na(`dmperind_g/ind`)) |> 
  group_by(scientific_name) |> 
  summarise(avg_dmperind = mean(`dmperind_g/ind`, na.rm = TRUE))

dt_wide2 <- dt_wide1 |> 
  left_join(cce_spp_avg, by = "scientific_name") |> 
  mutate(`dmperind_g/ind` = if_else(project == "CCE" & is.na(`dmperind_g/ind`), avg_dmperind, `dmperind_g/ind`)) |> 
  dplyr::select(-avg_dmperind)  #remove the temporary avg_dmperind column

### re-check to ensure NAs are gone
na_count_per_column <- sapply(dt_wide2, function(x) sum(is.na(x)))
print(na_count_per_column) #looks good :)

### check on nas in subsite_level1
NA_subsite_level1 <- dt_wide2 |> 
  filter(is.na(subsite_level1))
unique(NA_subsite_level1$project) #"CoastalCA"
unique(NA_subsite_level1$year) #2002-2021
unique(NA_subsite_level1$subsite_level2)#"SCI_YELLOWBANKS_W" "HORSESHOE_REEF_E"  "HORSESHOE_REEF_W" 
unique(NA_subsite_level1$subsite_level3) #"1" "2" "3" "4"

### lets look at regular CoastalCA setup - i.e., why are these missing
CoastalCA_test <- dt_wide2 |> 
  filter(project == "CoastalCA") |> 
  dplyr::select(site, subsite_level1, subsite_level2, subsite_level3) |> 
  distinct() #should be mpa or reference

### id mpa vs reference using - https://caseagrant.ucsd.edu/sites/default/files/27B_Caselle-appendicies.pdf
# "SCI_YELLOWBANKS_W" = reference
# "HORSESHOE_REEF_E" = reference
# "HORSESHOE_REEF_W" = reference

### replace nas here with "reference"
dt_wide3 <- dt_wide2 |> 
  mutate(subsite_level1 = if_else(project == "CoastalCA" & is.na(subsite_level1), "reference", subsite_level1))

CoastalCA_check <- dt_wide3 |> 
  filter(project == "CoastalCA") |> 
  dplyr::select(site, subsite_level1, subsite_level2, subsite_level3) |> 
  distinct()

### check nas
na_count_per_column <- sapply(dt_wide3, function(x) sum(is.na(x)))
print(na_count_per_column)

### tidy environment
all_objects <- ls() #list all objects in the environment
object_to_keep <- "dt_wide3" #specify the object you want to keep
rm(list = all_objects[all_objects != object_to_keep])
rm(all_objects, object_to_keep)

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion_CLEAN.csv"

write.csv(dt_wide3, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier2",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))


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

# peacek<-df6%>%
#   filter(project=="NGA")%>%
#   select(sp_code,scientific_name,species)
# #check to see if there is repeated species
# peace1 <- species_list %>%
#   group_by(project,sp_code,scientific_name,species) %>%
#   summarise(freq=n(),.groups='drop') %>%
#   ungroup()
#### read data end 


#### calculate excretion rate

# take out the rows that are needed 

df1 <-df %>%
  filter(measurement_type %in% c("dmperind","density","temp")) 

# #check the unit that match with the measurement, good to go
 # peace <- df1 %>%
 #   distinct(project,habitat,measurement_type,measurement_unit)
#glimpse(df1)

### pivot_wider indicates duplicates and generating lists instead of columns in dbl
### check for replicates in data. fixed the issue because it was concat twice in the earlier codes
# df1_replicates <- df1 |> 
#   group_by(across(everything())) |> 
#   filter(n() > 1) |> 
#   ungroup()
# 
# unique(df1_replicates$project) #[1] "CoastalCA" "SBC"  
# unique(df1_replicates$habitat) #[1] "ocean"
# 
# df1_replicates_distinct <- df1_replicates |> 
#   distinct()

df2 <- df1 |> 
  distinct()


df3 <- df2 %>%
  pivot_wider(names_from = c(measurement_type,measurement_unit), values_from = measurement_value) 

glimpse(df3)

# check df3# check species list before merging, need to check
# peace2 <- species_list %>%
#   group_by(project,sp_code,scientific_name,species) %>%
#   summarise(n=n(),.groups='drop') %>%
#   ungroup() 

# there is duplicate, we need to select the first one; FCE fixed the issue, we are good now. but keep here in case
 spe2 <- species_list %>%
   group_by(project,sp_code,scientific_name,species) %>%
   slice(1) %>%
   ungroup()

# merge with the species list
df4 <- df3 %>%
  left_join(spe2,by=c("project","sp_code","scientific_name","species")) 


# fix nas that dont make sense --------------------------------------------

# check to see anything that don't have diet cat column
 peace3<-df4 %>%
   filter(is.na(diet_cat)) %>%
   distinct(project,habitat,sp_code,scientific_name,species,diet_cat)

na_count_per_column <- sapply(df4, function(x) sum(is.na(x)))
print(na_count_per_column)

### examine small # of data w missing scientific name column
value_nas <- df4 |> 
  filter(is.na(scientific_name)) #all from PISCO and are anchovies/sardines

### fill NAs in scientific_name with order 'Clupeiformes' since most resolved portion of shared taxonomy

df5 <- df4 |> 
  mutate(scientific_name = ifelse(is.na(scientific_name), "Clupeiformes", scientific_name))

na_count_per_column <- sapply(df5, function(x) sum(is.na(x)))
print(na_count_per_column)

### examine data without family classification

# ## FCE 
# value_nas <- df5 |> 
#   filter(is.na(family)) |> 
#   filter(project == "FCE") 
# unique(value_nas$scientific_name)
# 
# ## VCR
# value_nas <- df5 |> 
#   filter(is.na(family)) |> 
#   filter(project == "VCR") 
# unique(value_nas$scientific_name)

# ## CoastalCA
# value_nas <- df5 |> 
#   filter(is.na(family)) |> 
#   filter(project == "CoastalCA") 
# unique(value_nas$scientific_name)

df6 <- df5 %>%
  mutate(family = case_when(
    scientific_name == "Cichlasoma urophthalmus" ~ "Cichlidae",
    scientific_name == "Aphrododerus sayanus" ~ "Aphrododeridae",
    scientific_name == "Farfantepenaeus duorarum" ~ "Penaeidae",
    scientific_name == "Gambusia holbrooki" ~ "Poeciliidae",
    scientific_name == "Kleptolebias marmoratus" ~ "Rivulidae",
    scientific_name == "Palaemonetes pugio" ~ "Palaemonidae",
    TRUE ~ family  # Keep the existing family if none of the above conditions are met
  ))

# value_nas <- df6 |> 
#   filter(is.na(family)) |> 
#   filter(project == "FCE") 
# unique(value_nas$scientific_name)
# glimpse(df6)

### tidy up environment
 rm(df1_replicates, 
    df1_replicates_distinct, value_nas, na_count_per_column,
    rep, rep_distinct)

###########################
#using bradley's code below for excretion calculation
cons <- df6

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
  dplyr::select(-c(N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10)) %>%
  pivot_longer(cols = -c(project,habitat,raw_filename,row_num,year,month,day,date,site,subsite_level1,subsite_level2,subsite_level3,sp_code,scientific_name,species,common_name,kingdom,phylum,class,order,family,genus,taxa_group,diet_cat), 
             names_to = "measurement_type1",
             values_to = "measurement_value") %>%
  separate(measurement_type1, into = c("measurement_type", "measurement_unit"),sep = "_", remove = T) 

# check FCE case
# peace4 <- df_final %>%
#   filter(project=="FCE")

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion.csv"

write.csv(df_final, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier2",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))

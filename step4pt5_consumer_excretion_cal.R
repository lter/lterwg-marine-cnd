## ------------------------------------------ ##
#       Marine CND -- excretion rate calculation
## ------------------------------------------ ##
# Script author(s): Li Kui (Mack White revisions and clean up)

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
librarian::shelf(tidyverse, googledrive, readxl, ropensci/taxize, stringr)

# Create necessary sub-folder(s)
dir.create(path = file.path("tier2"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)

## -------------------------------------------- ##
#             Data Acquisition ----
## -------------------------------------------- ##

# Raw data are stored in a Shared Google Drive that is only accessible by team members
# If you need the raw data, run the relevant portion of the following script:
file.path("scripts-googledrive", "step4pt5_gdrive-interactions.R")

## ------------------------------------------ ##
#             data wrangling for each project ----
## ------------------------------------------ ##

#### read data
# read in the harmonized data and start the wrangling, by project
df <- read.csv(file.path("tier1", "harmonized_consumer_ready_for_excretion_V2.csv"),stringsAsFactors = F,na.strings =".") 
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

df1 <-df |> 
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
### fixed duplicates in pisco and sbc above in previous scripts, but continue to run for posterity 

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

### tidy environment (optional but helps with memory)---
rm(cons,df,df1,df2,df3,df4,df5,df6,peace3,spe2,species_list) 

### skipping pivot longer below because my machine can't handle it... haha
exc_df <- cons_np_ratio |> 
  dplyr::select(-c(N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10))

# FCE hydroyear to year fix -----------------------------------------------
### initially used substring to select the hydroyear, instead of calendar year such that
### hydroyear 2023-2024 is equal to 2023, so need to reverse by starting with earliest sample
### on record for a given 'hydroyear' as we tend to think of it in terms of a sampling season
### in the map database - need to reverse and then fix such it appropriately handles year-month combinations
### for hydroyear

fce_wide <- exc_df |>
  filter(project == "FCE") |> 
  ### reverse what I did to qualify hydroyear in intitial dataset
  mutate(year = if_else(
    month >= 10,
    year,
    year + 1
  )) |>
  ### set so that hydroyear corresponds to correct year-month combination
  mutate(year = if_else(month >=7, year + 1, year))

other_wide <- exc_df |> filter(project != "FCE")

df_wide <- rbind(fce_wide, other_wide)

# rm(exc_df,cons_np_ratio)

### check for NAs
na_count_per_column <- sapply(df_wide, function(x) sum(is.na(x)))
print(na_count_per_column) #1002 observations of NAs in dmperind_g/ind fixed in step3

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion_CLEAN_V4.csv"

write.csv(df_wide, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

##### left behind this section on october 29 due to system memory lacking
##### Data clean up #######

# df_final <- cons_np_ratio %>% 
#   dplyr::select(-c(N_vert_coef,N_diet_coef,Nexc_log10,P_vert_coef,P_diet_coef,Pexc_log10)) %>%
#   pivot_longer(cols = -c(project,habitat,raw_filename,row_num,year,month,day,date,site,subsite_level1,subsite_level2,subsite_level3,sp_code,scientific_name,species,common_name,kingdom,phylum,class,order,family,genus,taxa_group,diet_cat), 
#                names_to = "measurement_type1",
#                values_to = "measurement_value") %>%
#   separate(measurement_type1, into = c("measurement_type", "measurement_unit"),sep = "_", remove = T) 

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion_V2.csv"

write.csv(df_final, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Tidied data are also stored in Google Drive
# To upload the most current versions (that you just created locally), 
## run the relevant portion of the following script:
file.path("scripts-googledrive", "step4pt5_gdrive-interactions.R")


# End ----


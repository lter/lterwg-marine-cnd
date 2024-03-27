###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): GLMM to determine explore stability in nutrient supply
###date(s): March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, nlme)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN.csv"))

strata_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
  dplyr::filter(name %in% c("strata_class.xlsx"))

# Combine file IDs
harmonized_ids <- rbind(exc_ids, strata_ids)

# For each raw data file, download it into the consumer folder
for(k in 1:nrow(harmonized_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = harmonized_ids[k, ]$id, overwrite = T,
                                path = file.path("tier2", harmonized_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(harmonized_ids))
}

rm(list = ls()) #cleans env


### read in clean excretion and strata data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN.csv"),stringsAsFactors = F,na.strings =".") |> 
  janitor::clean_names()
glimpse(dt)

strata_list <- readxl::read_excel(path = file.path("tier2", "strata_class.xlsx"),na=".") |> 
  ### remove decimals from numbered sites
  mutate(site = str_remove(site, "\\.0$"),
         subsite_level1 = str_remove(subsite_level1, "\\.0$"),
         subsite_level2 = str_remove(subsite_level2, "\\.0$"),
         subsite_level3 = str_remove(subsite_level3, "\\.0$"))

# set up data for summary statistics --------------------------------------

### replace NAs in subsite_level2 and subsite_level3 columns with "Not Available"
### to allow group_by function to go as far in sequence as it can for 
### each project without throwing NAs

dt <- dt %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

### check to see NA fixes incorporated
na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column)

# look into outliers for project-species combinations -----------

dt_og <- dt |> 
  group_by(project, habitat) |> 
  ### filtering out sharks and rays that are considered "biomass busters"
  mutate(mean_dmperind = mean(dmperind_g_ind, na.rm = TRUE),  
         sd_dmperind = sd(dmperind_g_ind, na.rm = TRUE),  
         lower_bound = mean_dmperind - 5 * sd_dmperind,  
         upper_bound = mean_dmperind + 5 * sd_dmperind,
         ### +/- 5 SD cutoff... rest of sharks and rays included
         outlier = dmperind_g_ind < lower_bound | dmperind_g_ind > upper_bound,
         sharkray = grepl("\\bshark\\b|\\bray\\b", common_name, ignore.case = TRUE),
         elasmo = class %in% c("Chondrichthyes", "Elasmobranchii")) |> 
  ungroup() |> 
  filter(!(outlier & sharkray & elasmo)) |> #lose 251 sharks and rays from MCR/PISCO datasets
  dplyr::select(-lower_bound, -upper_bound, -outlier, -sharkray)

glimpse(dt_og)


# summarize data for plotting ---------------------------------------------

dt_total <- dt_og |> 
  ### classify each individual as either being a vertebrate or invertebrate
  mutate(vert_1 = if_else(phylum == "Chordata", "vertebrate", "invertebrate")) |> 
  mutate(vert2 = if_else(is.na(vert_1) & project == "CoastalCA", "vertebrate", vert_1)) |> 
  mutate(vert = ifelse(is.na(vert2), "invertebrate", vert2)) |> 
  dplyr::select(-vert_1, -vert2) |> 
  mutate(vertebrate_n = if_else(vert == "vertebrate" & dmperind_g_ind != 0, 1, 0),
  invertebrate_n = if_else(vert == "invertebrate" & dmperind_g_ind != 0, 1, 0)) |> 
  ### calculate max size of community at this resolution so we can calculate mean max size of species within community
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, vert, scientific_name) |>
  mutate(max_size = max(dmperind_g_ind, na.rm = TRUE)) |> 
  ungroup() |>
  ### group to highest resolution for each site - why set to "Not Available" earlier in script
  ### added "vert" column in case we need to piece out vertebrate and invertebrate differences
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3) |> 
  summarise(total_nitrogen_m = sum(nind_ug_hr * density_num_m, na.rm = TRUE),
            total_nitrogen_m2 = sum(nind_ug_hr * density_num_m2, na.rm = TRUE),
            total_nitrogen_m3 = sum(nind_ug_hr * density_num_m3, na.rm = TRUE),
            total_nitrogen = sum(total_nitrogen_m + total_nitrogen_m2 + total_nitrogen_m3, na.rm = TRUE),
            nitrogen_ind_sd_nozeros = sd(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            nitrogen_ind_mean_nozeros = mean(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            nitrogen_ind_cv_nozeros = (nitrogen_ind_sd_nozeros/nitrogen_ind_mean_nozeros)*100,
            total_phosphorus_m = sum(pind_ug_hr * density_num_m, na.rm = TRUE),
            total_phosphorus_m2 = sum(pind_ug_hr * density_num_m2, na.rm = TRUE),
            total_phosphorus_m3 = sum(pind_ug_hr * density_num_m3, na.rm = TRUE),
            total_phosphorus = sum(total_phosphorus_m + total_phosphorus_m2 + total_phosphorus_m3, na.rm = TRUE),
            phosphorus_ind_sd_nozeros = sd(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            phosphorus_ind_mean_nozeros = mean(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            phosphorus_ind_cv_nozeros = (phosphorus_ind_sd_nozeros/phosphorus_ind_mean_nozeros)*100,
            total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
            total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
            total_bm_m3 = sum(dmperind_g_ind*density_num_m3, na.rm = TRUE),
            total_biomass = sum(total_bm_m + total_bm_m2 + total_bm_m3, na.rm = TRUE),
            bm_ind_sd_nozeros = sd(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            bm_ind_mean_nozeros = mean(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            bm_ind_cv_nozeros = (bm_ind_sd_nozeros/bm_ind_mean_nozeros)*100,
            n_obs = n(),
            diet_algae_detritus_n = sum(diet_cat == "algae_detritus", na.rm = TRUE),
            diet_invert_n = sum(diet_cat == "invert", na.rm = TRUE),
            diet_algae_invert_n = sum(diet_cat == "algae_invert", na.rm = TRUE),
            diet_fish_invert_n = sum(diet_cat == "fish_invert", na.rm = TRUE),
            diet_fish_n = sum(diet_cat == "fish", na.rm = TRUE),
            prop_algae_detritus = diet_algae_detritus_n / n_obs,
            prop_invert = diet_invert_n / n_obs,
            prop_algae_invert = diet_algae_invert_n / n_obs,
            prop_fish_invert = diet_fish_invert_n / n_obs,
            prop_fish = diet_fish_n / n_obs,
            n_spp = n_distinct(scientific_name[dmperind_g_ind != 0]),
            mean_max_community_size = mean(max_size),
            vertebrate_count = sum(vertebrate_n),
            invertebrate_count = sum(invertebrate_n),
            total_count = vertebrate_count + invertebrate_count,
            vertebrate_prop = if_else(total_count > 0, vertebrate_count / total_count, 0),
            invertebrate_prop = if_else(total_count > 0, invertebrate_count / total_count, 0)) |>
  ungroup() |>
  dplyr::select(-total_nitrogen_m, -total_nitrogen_m2, -total_nitrogen_m3,
                -total_phosphorus_m, -total_phosphorus_m2, -total_phosphorus_m3,
                -total_bm_m, -total_bm_m2, -total_bm_m3,
                -n_obs,
                -diet_algae_detritus_n, -diet_invert_n, -diet_algae_invert_n,
                -diet_fish_invert_n, -diet_fish_n)

### check for NAs 
na_count_per_column <- sapply(dt_total, function(x) sum(is.na(x)))
print(na_count_per_column) #dont understand NAs, but small fraction of dataset

dt_total_clean <- dt_total |> 
  ### replace NAs with zeros in mean columns
  mutate(nitrogen_ind_mean_nozeros = replace_na(nitrogen_ind_mean_nozeros, 0),
         phosphorus_ind_mean_nozeros = replace_na(phosphorus_ind_mean_nozeros, 0),
         bm_ind_mean_nozeros = replace_na(bm_ind_mean_nozeros, 0)) |> 
  ### set sd/cv columns to zero since instance when 1 individual collected
  mutate(nitrogen_ind_sd_nozeros = replace_na(nitrogen_ind_sd_nozeros, 0),
         nitrogen_ind_cv_nozeros = replace_na(nitrogen_ind_cv_nozeros, 0),
         phosphorus_ind_sd_nozeros = replace_na(phosphorus_ind_sd_nozeros, 0),
         phosphorus_ind_cv_nozeros = replace_na(phosphorus_ind_cv_nozeros, 0),
         bm_ind_sd_nozeros = replace_na(bm_ind_sd_nozeros, 0),
         bm_ind_cv_nozeros = replace_na(bm_ind_cv_nozeros, 0))

glimpse(dt_total_clean)

na_count_per_column <- sapply(dt_total_clean, function(x) sum(is.na(x)))
print(na_count_per_column)

summary(dt_total_clean)

### 
strata_list <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

dt_calcs_strata <- left_join(dt_total_clean, 
                             strata_list, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2",
                                    "subsite_level3")) |> 
  mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
  dplyr::select(-ecoregion_habitat)

na_count_per_column <- sapply(dt_calcs_strata, function(x) sum(is.na(x)))
print(na_count_per_column)

# generate project-habitat + date column ----------------------------------

dt_calcs_strata_1 <- dt_calcs_strata |>
  ### create project-habitat column since some projects sample multiple habitats (i.e., SBC ocean & beach)
  unite("projecthabitat", project, habitat, sep = "-", remove = FALSE) |> 
  ### create date columns for timeseries plotting
  mutate(sdate = ymd(paste(year, month, "01", sep = "-")))

na_count_per_column <- sapply(dt_calcs_strata_1, function(x) sum(is.na(x)))
print(na_count_per_column)

### tidy up environment
dat <- dt_calcs_strata_1
# all_objects <- ls() #list all objects in the environment
# object_to_keep <- "dat" #specify the object you want to keep
# rm(list = all_objects[all_objects != object_to_keep])
# rm(all_objects, object_to_keep)

# setup individual 'projecthabitats' for plotting -------------------------
### Below I have seperated each unique projecthabitat out to mutate new columns based on either
### the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
### their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
### whereas SBC wants their data summarized at the site level. This approach sets up
### an easy way to map plots across all unique projecthabitats, instead of doing them
### individually

### CCE-oceanic
cce <- dat |> 
  filter(projecthabitat == "CCE-oceanic") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')

### CoastalCA-ocean
pisco_central <- dat |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "CENTRAL") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-CENTRAL")

pisco_south <- dat |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "SOUTH") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-SOUTH") |> 
  group_by(subsite_level2, year) |> 
  mutate(test = mean(total_nitrogen)) |>
  ungroup() |> 
  filter(!test > 75000) |> 
  dplyr::select(-test)

### FCE-estuary
fce <- dat |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') #group at subsite_level1

### MCR-ocean
mcr <- dat |> 
  filter(projecthabitat == "MCR-ocean") |> 
  ### join site and subsite_level1 according DB request for grouping variable
  unite("group", site, subsite_level1, sep = "-", remove = FALSE) |>
  mutate(group = group,
         color = subsite_level1,
         units = 'm2')

### NGA-oceanic
nga <- dat |> 
  filter(projecthabitat == "NGA-oceanic") |> 
  mutate(group = site,
         color = strata,
         units = 'm3')

### PIE-estuary
pie <- dat |> 
  filter(projecthabitat == "PIE-estuary") |> 
  mutate(group = site,
         color = strata,
         units = 'm2') 

### SBC-beach
sbc_beach <- dat |> 
  filter(projecthabitat == "SBC-beach") |> 
  mutate(group = site,
         color = strata,
         units = 'm') 
### SBC-ocean
sbc_reef <- dat |> 
  filter(projecthabitat == "SBC-ocean") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')

### VCR-estuary
vcr <- dat |> 
  filter(projecthabitat == "VCR-estuary") |> 
  mutate(group = subsite_level1,
         color = strata,
         units = 'm2')

#Binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
plotting_dat_ready <- bind_rows(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef,
                                nga, pie, vcr)

## tidy up environment
# all_objects <- ls() #list all objects in the environment
# object_to_keep <- c("plotting_dat_ready", "dt_og") #specify the object you want to keep
# rm(list = all_objects[all_objects != object_to_keep])
# rm(all_objects, object_to_keep)

na_count_per_column <- sapply(plotting_dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column)

dt <- plotting_dat_ready

### update sites with new "color" according to how Deron thinks we should group
cce_plotting_annual <- dt |> 
  filter(projecthabitat == "CCE-oceanic") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

pisco_central_plotting_annual <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean-CENTRAL") |> 
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

pisco_south_plotting_annual <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean-SOUTH") |> 
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

sbc_beach_plotting_annual <- dt |> 
  filter(projecthabitat == "SBC-beach") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

nga_plotting_annual <- dt |> 
  filter(projecthabitat == "NGA-oceanic") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

pie_plotting_annual <- dt |> 
  filter(projecthabitat == "PIE-estuary") |> 
  mutate(color2 = site)

vcr_plotting_annual <- dt |> 
  filter(projecthabitat == "VCR-estuary") |> 
  unite(color2, c(subsite_level1, color), sep = "-", remove = FALSE)

fce_plotting_annual <- dt |>
  filter(projecthabitat == "FCE-estuary") |>
  unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE) |> 
  ### revert back to hydrologic year to make more sense of dataset
  mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

mcr_plotting_annual <- dt |>
  filter(projecthabitat == "MCR-ocean") |>
  unite(color2, c(subsite_level1, site), sep = "-", remove = FALSE)

sbc_ocean_plotting_annual <- dt |>
  filter(projecthabitat == "SBC-ocean") |>
  unite(color2, c(site, color), sep = "-", remove = FALSE)

dt <- bind_rows(cce_plotting_annual, pisco_central_plotting_annual, pisco_south_plotting_annual,
                fce_plotting_annual, mcr_plotting_annual, sbc_ocean_plotting_annual,
                sbc_beach_plotting_annual, nga_plotting_annual, pie_plotting_annual, 
                vcr_plotting_annual)

rm(cce_plotting_annual, pisco_central_plotting_annual, pisco_south_plotting_annual,
   fce_plotting_annual, mcr_plotting_annual, sbc_ocean_plotting_annual,
   sbc_beach_plotting_annual, nga_plotting_annual, pie_plotting_annual, 
   vcr_plotting_annual)

unique(dt$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "PISCO-Central", "PISCO-South", "FCE", "MCR",
              "SBC-Ocean", "SBC-Beach", "NGA", 
              "PIE", "VCR")) 
print(label_mapping)

unique(dt$color)
habitat_mapping <- data.frame(
  color = unique(dt$color),
  Habitat = c("Nearshore", "Offshore","Marine Protected Area", "Reference", "Riverine", "Bay", 
              "Back Reef", "Fore Reef", "Fringing Reef",'Reference', "Seward", 
              "Knight Island Passage", "Kodiak Island", "Middleton Island", "Prince William Sound", "Fertilized",
              "Natural", "Fertilized", "Natural", "Seagrass", "Sand"))
print(habitat_mapping)

data_step2 <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  select(-projecthabitat, -habitat, -project, -color) |> 
  rename(color = color2,
         project = Project, 
         habitat = Habitat)

# filter <- data_step2 |> 
#   filter(invertebrate_prop == 0 & vertebrate_prop == 0) #only ones are those with nothing caught!

# write_csv(data_step2, "glmm.csv")

dt_glmm <- data_step2 |> 
  group_by(project, color, year) |> 
  mutate(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  filter(!is.na(total_nitrogen_cv)) |> #~30 obs with NAs, omitted
  ungroup() 

### go to allgeier et al 2014 and think of metrics to associate with cv nitrogen supply
### primarily metrics related to community structure
### data needs to be grouped by site and then average/cv metrics gleaned
### probably need to go back and determine how we want to handle vert vs invert... maybe calculate for both
### also probably want to get rid of all the extra metrics in that initial "dt_total" df as to not muddy things up




model <- lme(total_nitrogen_cv ~ total_biomass + n_spp + mean_max_community_size + n_spp_cv + mean_max_community_size_cv + vertebrate_prop + invertebrate_prop, 
             data = dt_glmm,
             random = ~ 1 | project/color)
summary(model)

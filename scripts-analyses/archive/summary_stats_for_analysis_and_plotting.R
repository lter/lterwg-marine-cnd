###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): calculate summary stats needed for analysis/figures
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

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

dt <- dt |> 
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
  filter(!(outlier & sharkray & elasmo)) #lose 251 sharks and rays from MCR/PISCO datasets

# summarize data for plotting ---------------------------------------------
glimpse(dt)

dt_calcs <- dt |> 
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
  mutate(max_size = max(dmperind_g_ind, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3) |> 
  summarise(total_n_ug_hr_m = sum(nind_ug_hr * density_num_m, na.rm = TRUE),
            total_n_ug_hr_m2 = sum(nind_ug_hr * density_num_m2, na.rm = TRUE),
            total_n_ug_hr_m3 = sum(nind_ug_hr * density_num_m3, na.rm = TRUE),
            total_n = sum(total_n_ug_hr_m + total_n_ug_hr_m2 + total_n_ug_hr_m3, na.rm = TRUE),
            # n_ind_sd = sd(nind_ug_hr, na.rm = TRUE),
            n_ind_sd_nozeros = sd(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            # n_ind_mean = mean(nind_ug_hr, na.rm = TRUE),
            n_ind_mean_nozeros = mean(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            # n_ind_cv = (n_ind_sd/n_ind_mean)*100,
            n_ind_cv_nozeros = (n_ind_sd_nozeros/n_ind_mean_nozeros)*100,
            total_p_ug_hr_m = sum(pind_ug_hr * density_num_m, na.rm = TRUE),
            total_p_ug_hr_m2 = sum(pind_ug_hr * density_num_m2, na.rm = TRUE),
            total_p_ug_hr_m3 = sum(pind_ug_hr * density_num_m3, na.rm = TRUE),
            total_p = sum(total_p_ug_hr_m + total_p_ug_hr_m2 + total_p_ug_hr_m3, na.rm = TRUE),
            # p_ind_sd = sd(pind_ug_hr, na.rm = TRUE),
            p_ind_sd_nozeros = sd(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            # p_ind_mean = mean(pind_ug_hr, na.rm = TRUE),
            p_ind_mean_nozeros = mean(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            # p_ind_cv = (p_ind_sd/p_ind_mean)*100,
            p_ind_cv_nozeros = (p_ind_sd_nozeros/p_ind_mean_nozeros)*100,
            total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
            total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
            total_bm_m3 = sum(dmperind_g_ind*density_num_m3, na.rm = TRUE),
            total_bm = sum(total_bm_m + total_bm_m2 + total_bm_m3, na.rm = TRUE),
            # bm_ind_sd = sd(dmperind_g_ind, na.rm = TRUE),
            bm_ind_sd_nozeros = sd(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            # bm_ind_mean = mean(dmperind_g_ind, na.rm = TRUE),
            bm_ind_mean_nozeros = mean(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            # bm_ind_cv = (bm_ind_sd/bm_ind_mean)*100,
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
            mean_max_community_size = mean(max_size)) |> 
  ungroup() |>
  dplyr::select(-total_n_ug_hr_m, -total_n_ug_hr_m2, -total_n_ug_hr_m3,
                -total_p_ug_hr_m, -total_p_ug_hr_m2, -total_p_ug_hr_m3,
                -total_bm_m, -total_bm_m2, -total_bm_m3,
                -n_obs,
                -diet_algae_detritus_n, -diet_invert_n, -diet_algae_invert_n,
                -diet_fish_invert_n, -diet_fish_n)

# ### check to make sure mean max size makes sense
# test <- dt_calcs |>
#   group_by(project, habitat) |>
#   summarize(mean_WeightMax = mean(mean_max_community_size))

### check for NAs 
na_count_per_column <- sapply(dt_calcs, function(x) sum(is.na(x)))
print(na_count_per_column) #dont understand NAs, but small fraction of dataset

dt_calcs_clean <- dt_calcs |> 
  ### replace NAs with zeros in mean columns
  mutate(n_ind_mean_nozeros = replace_na(n_ind_mean_nozeros, 0),
         p_ind_mean_nozeros = replace_na(p_ind_mean_nozeros, 0),
         bm_ind_mean_nozeros = replace_na(bm_ind_mean_nozeros, 0)) |> 
  ### set sd/cv columns to zero since instance when 1 individual collected
  mutate(n_ind_sd_nozeros = 0,
         n_ind_cv_nozeros = 0,
         p_ind_sd_nozeros = 0,
         p_ind_cv_nozeros = 0,
         bm_ind_sd_nozeros = 0,
         bm_ind_cv_nozeros = 0)
glimpse(dt_calcs_clean)

na_count_per_column <- sapply(dt_calcs_clean, function(x) sum(is.na(x)))
print(na_count_per_column)

# join strata and prepare data for plotting -------------------------------

### 
strata_list <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

dt_calcs_strata <- left_join(dt_calcs_clean, 
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
dt <- dt_calcs_strata_1
all_objects <- ls() #list all objects in the environment
object_to_keep <- "dt" #specify the object you want to keep
rm(list = all_objects[all_objects != object_to_keep])
rm(all_objects, object_to_keep)

# setup individual 'projecthabitats' for plotting -------------------------
### Below I have seperated each unique projecthabitat out to mutate new columns based on either
### the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
### their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
### whereas SBC wants their data summarized at the site level. This approach sets up
### an easy way to map plots across all unique projecthabitats, instead of doing them
### individually

### CCE-oceanic
cce <- dt |> 
  filter(projecthabitat == "CCE-oceanic") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')

### CoastalCA-ocean
pisco_central <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "CENTRAL") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-CENTRAL")

pisco_south <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "SOUTH") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-SOUTH") |> 
  group_by(subsite_level2, year) |> 
  mutate(test = mean(total_n)) |>
  ungroup() |> 
  filter(!test > 75000) |> 
  dplyr::select(-test)

### FCE-estuary
fce <- dt |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') #group at subsite_level1

### MCR-ocean
mcr <- dt |> 
  filter(projecthabitat == "MCR-ocean") |> 
  ### join site and subsite_level1 according DB request for grouping variable
  unite("group", site, subsite_level1, sep = "-", remove = FALSE) |>
  mutate(group = group,
         color = subsite_level1,
         units = 'm2')

### NGA-oceanic
nga <- dt |> 
  filter(projecthabitat == "NGA-oceanic") |> 
  mutate(group = site,
         color = strata,
         units = 'm3')

### PIE-estuary
pie <- dt |> 
  filter(projecthabitat == "PIE-estuary") |> 
  mutate(group = site,
         color = strata,
         units = 'm2') 

### SBC-beach
sbc_beach <- dt |> 
  filter(projecthabitat == "SBC-beach") |> 
  mutate(group = site,
         color = strata,
         units = 'm') 
### SBC-ocean
sbc_reef <- dt |> 
  filter(projecthabitat == "SBC-ocean") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')

### VCR-estuary
vcr <- dt |> 
  filter(projecthabitat == "VCR-estuary") |> 
  mutate(group = subsite_level1,
         color = strata,
         units = 'm2')

#Binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
plotting_dat_ready <- bind_rows(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef,
                                nga, pie, vcr)

### tidy up environment
all_objects <- ls() #list all objects in the environment
object_to_keep <- "plotting_dat_ready" #specify the object you want to keep
rm(list = all_objects[all_objects != object_to_keep])
rm(all_objects, object_to_keep)

na_count_per_column <- sapply(plotting_dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column)

### check out data to see if anything jumps out
# test <- plotting_dat_ready |> 
#   group_by(projecthabitat, strata) |> 
#   summarize(mean_n = mean(total_n),
#             mean_p = mean(total_p),
#             mean_bm = mean(total_bm))

dt <- plotting_dat_ready

rm(plotting_dat_ready, na_count_per_column)

#### export and write to the drive
# Export locally
tidy_filename <- "harmonized_consumer_excretion_CLEAN_summarized.csv"

write.csv(dt, file = file.path("tier2", tidy_filename), na = '.', row.names = F)

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media= file.path("tier2",tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))




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
  dplyr::select(-...9,-...10)
glimpse(strata_list)

# set up data for summary statistics --------------------------------------

### replace NAs in subsite_level2 and subsite_level3 columns with "Not Available"
### to allow group_by function to go as far in sequence as it can for 
### each project without throwing NAs

dt <- dt %>%
  mutate(subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

### check to see NA fixes incorporated
# na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
# print(na_count_per_column)

# summarize data for plotting ---------------------------------------------
glimpse(dt)

dt_calcs <- dt |> 
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
            n_spp = n_distinct(scientific_name[dmperind_g_ind != 0]),
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
            prop_fish = diet_fish_n / n_obs) |>
  ungroup() |> 
  dplyr::select(-total_n_ug_hr_m, -total_n_ug_hr_m2, -total_n_ug_hr_m3,
                -total_p_ug_hr_m, -total_p_ug_hr_m2, -total_p_ug_hr_m3,
                -total_bm_m, -total_bm_m2, -total_bm_m3,
                -n_obs,
                -diet_algae_detritus_n, -diet_invert_n, -diet_algae_invert_n,
                -diet_fish_invert_n, -diet_fish_n)

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

na_count_per_column <- sapply(dt_calcs_clean, function(x) sum(is.na(x)))
print(na_count_per_column)



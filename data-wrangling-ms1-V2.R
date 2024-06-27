###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): Last Step Data Prep for Analyses & Figures
###date(s): March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, vegan, readxl, e1071)

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

dt <- dt |> 
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

### check to see NA fixes incorporated
na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column)

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
  dplyr::select(-lower_bound, -upper_bound, -outlier, -sharkray, -elasmo)

glimpse(dt_og)

###########################################################################
# add vertebrate and invertebrate column ~ phylum -------------------------
###########################################################################

dt_mutate <- dt_og |> 
  ### classify each individual as either being a vertebrate or invertebrate
  mutate(vert_1 = if_else(phylum == "Chordata", "vertebrate", "invertebrate")) |> 
  mutate(vert2 = if_else(is.na(vert_1) & project == "CoastalCA", "vertebrate", vert_1)) |> 
  mutate(vert = ifelse(is.na(vert2), "invertebrate", vert2)) |> 
  dplyr::select(-vert_1, -vert2) |> 
  mutate(vertebrate_n = if_else(vert == "vertebrate" & dmperind_g_ind != 0, 1, 0),
         invertebrate_n = if_else(vert == "invertebrate" & dmperind_g_ind != 0, 1, 0)) |> 
  ## calculate max size of community at this resolution so we can calculate mean max size of species within community
  group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
  mutate(max_size = max(dmperind_g_ind, na.rm = TRUE),
         min_size = min(dmperind_g_ind, na.rm = TRUE),
         mean_size = mean(dmperind_g_ind, na.rm = TRUE))

###########################################################################
# add vertebrate and invertebrate column ~ phylum -------------------------
###########################################################################

dt_mutate <- dt_og |> 
  ### classify each individual as either being a vertebrate or invertebrate
  mutate(vert_1 = if_else(phylum == "Chordata", "vertebrate", "invertebrate")) |> 
  mutate(vert2 = if_else(is.na(vert_1) & project == "CoastalCA", "vertebrate", vert_1)) |> 
  mutate(vert = ifelse(is.na(vert2), "invertebrate", vert2)) |> 
  dplyr::select(-vert_1, -vert2) |> 
  mutate(vertebrate_n = if_else(vert == "vertebrate" & dmperind_g_ind != 0, 1, 0),
         invertebrate_n = if_else(vert == "invertebrate" & dmperind_g_ind != 0, 1, 0)) |> 
  ## calculate max size of community at this resolution so we can calculate mean max size of species within community
  group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
  mutate(max_size = max(dmperind_g_ind, na.rm = TRUE),
         min_size = min(dmperind_g_ind, na.rm = TRUE),
         mean_size = mean(dmperind_g_ind, na.rm = TRUE))

### step one getting rid of inverts
dt_mutate_filter <- dt_mutate |> 
  filter(!(project %in% c("FCE", "VCR") & vert == "invertebrate")) |> 
  mutate(vert = ifelse(project %in% c("CCE", "NGA"), "invertebrate", vert))

dt_mutate_filter_2 <- dt_mutate_filter |> 
  filter(vert == "vertebrate",
         !project %in% c("NGA", "CCE"))

# calculating the nutrient metrics ----------------------------------------

dt_nitrogen_totals <- dt_mutate_filter_2 |> 
  group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3) |> 
  summarise(### calculate total phosphorus supply at each sampling unit and then sum to get column with all totals
    total_nitrogen_m = sum(nind_ug_hr * density_num_m, na.rm = TRUE),
    total_nitrogen_m2 = sum(nind_ug_hr * density_num_m2, na.rm = TRUE),
    total_nitrogen_m3 = sum(nind_ug_hr * density_num_m3, na.rm = TRUE),
    total_nitrogen = sum(total_nitrogen_m + total_nitrogen_m2 + total_nitrogen_m3, na.rm = TRUE),
    ### calculate total phosphorus supply at each sampling unit and then sum to get column with all totals
    total_phosphorus_m = sum(pind_ug_hr * density_num_m, na.rm = TRUE),
    total_phosphorus_m2 = sum(pind_ug_hr * density_num_m2, na.rm = TRUE),
    total_phosphorus_m3 = sum(pind_ug_hr * density_num_m3, na.rm = TRUE),
    total_phosphorus = sum(total_phosphorus_m + total_phosphorus_m2 + total_phosphorus_m3, na.rm = TRUE),
    ### calculate total biomass at each sampling unit and then sum to get column with all totals
    total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
    total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
    total_bm_m3 = sum(dmperind_g_ind*density_num_m3, na.rm = TRUE),
    total_biomass = sum(total_bm_m + total_bm_m2 + total_bm_m3, na.rm = TRUE),
    ### calculate species richness
    n_spp = n_distinct(scientific_name[dmperind_g_ind != 0]),
    ### calculate average community size metrics
    max_size = mean(max_size, na.rm = TRUE),
    mean_size = mean(mean_size, na.rm = TRUE),
    min_size = mean(min_size, na.rm = TRUE)) |> 
  ungroup() |>
  dplyr::select(-total_nitrogen_m, -total_nitrogen_m2, -total_nitrogen_m3,
                -total_phosphorus_m, -total_phosphorus_m2, -total_phosphorus_m3,
                -total_bm_m, -total_bm_m2, -total_bm_m3) |> 
  arrange(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3)

# calculating version one of the diversity metrics ------------------------
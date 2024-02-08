###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): explore and analyzed harmonized excretion data
###date(s): January 2024

###########################################################################
# load necessary packages -------------------------------------------------
###########################################################################

# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)
# dir.create(path = file.path("tier2"), showWarnings = F)
###########################################################################
# connect to google drive -------------------------------------------------
###########################################################################
# ONLY NEED TO BE DONE ONCE
# # pull in the harmonized data
# exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) %>%
#   dplyr::filter(name %in% c("harmonized_consumer_excretion.csv"))
# 
# # Combine file IDs
# exc_ids <- rbind(exc_ids)
# 
# # For each raw data file, download it into the consumer folder
# for(k in 1:nrow(exc_ids)){
# 
#   # Download file (but silence how chatty this function is)
#   googledrive::with_drive_quiet(
#     googledrive::drive_download(file = exc_ids[k, ]$id, overwrite = T,
#                                 path = file.path("tier2", exc_ids[k, ]$name)) )
# 
#   # Print success message
#   message("Downloaded file ", k, " of ", nrow(exc_ids))
# }
# 
# # Clear environment
# rm(list = ls())

###########################################################################
# load harmonized excretion data ------------------------------------------
###########################################################################

# read in the harmonized data and start the wrangling, by project
df <- read.csv(file.path("tier2", "harmonized_consumer_excretion.csv"),
               stringsAsFactors = F,na.strings =".")
glimpse(df)
unique(df$project)

# check out individual projects -------------------------------------------

fce <- df |> 
  filter(project == "FCE")
glimpse(fce)

pisco <- df |> 
  filter(project == "CoastalCA")
glimpse(pisco)

mcr <- df |> 
  filter(project == "MCR")
glimpse(mcr)

sbc_reef <- df |> 
  filter(project == "SBC",
         habitat == "ocean")

sbc_beach <- df |> 
  filter(project == "SBC",
         habitat == "beach")

# rm(fce, pisco, mcr, sbc_reef, sbc_beach)

###########################################################################
# Fix Untrue Zeros in FCE Dataset -----------------------------------------
###########################################################################

fce <- df |> 
  filter(project == "FCE")
glimpse(fce)

fce_wide <- fce |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit),
              values_from = measurement_value)
glimpse(fce_wide)
summary(fce_wide)
sum(is.infinite(fce_wide$`density_num/m`))#2 infinites - going to simply remove
fce_wide_noINF <- fce_wide[!is.infinite(fce_wide$`density_num/m`), ]

fce_summ <- fce_wide_noINF |> 
  group_by(year, month, site, subsite_level1, subsite_level2) |>
  mutate(bm_tot_m = sum(`dmperind_g/ind`*`density_num/m`))

fce_summ_clean <- fce_summ |> 
  filter(!is.na(subsite_level1)) |> #removes weird NAs in subsite_level1 -still kicking around
  filter(!(site == "TB" & year %in% c(2004:2007))) #removes years 2004 through 2007 for TB - we werent sampling then

#seems like a lot of artifical zeros 
#below shows legitimate zeros - need to recode columns and select() then join to link up with above and get rid of artifical zeros
master_map <- read_csv("../mw_dissertation/MAP_database_maintanence/mastermap_yrs1thru19.csv")
map1 <- master_map |> 
  select(HYDROYEAR, s.mo, DRAINAGE, SITE, BOUT, SPECIESCODE) |> 
  separate(HYDROYEAR, into = c("year", "void")) |> 
  select(-void) |> 
  rename(
    year = year,
    month = s.mo,
    site = DRAINAGE,
    subsite_level1 = SITE,
    subsite_level2 = BOUT,
    spp_code = SPECIESCODE
  ) |> 
  mutate(year = as.integer(year),
         month = as.integer(month),
         subsite_level1 = as.character(subsite_level1),
         subsite_level2 = as.character(subsite_level2)) |> 
  filter(spp_code == 13) |> 
  na.omit()

fce_join <- left_join(fce_summ_clean, map1, by = c("year", "month", "site", "subsite_level1", "subsite_level2"))

fce_true_zeros <- fce_join |> 
  filter(bm_tot_m != 0 | (bm_tot_m == 0 & spp_code == 13)) |> 
  select(-spp_code) #this is right - gets rid of made-up site combinations - double checked before removing spp_code column

# pivot back + bind with full dataset -------------------------------------

fce_long <- fce_true_zeros %>%
  select(-bm_tot_m) |> 
  pivot_longer(
    cols = c(`density_num/m`, `dmperind_g/ind`, temp_c, `density_num/m2`, `nind_ug/hr`, `pind_ug/hr`),
    names_to = c("measurement_type", "measurement_unit"), 
    names_sep = "_",
    values_to = "measurement_value"
  )

df_4join <- df |> 
  filter(project != "FCE")

df_clean <- bind_rows(df_4join, fce_long)

# Pivot Full Dataset Wider ------------------------------------------------

df_wide <- df_clean |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit),
              values_from = measurement_value)

write_csv(df_wide, "data/exc_clean_02082024.csv")

###########################################################################
# Summarizing Data --------------------------------------------------------
################################################################

fce_summ <- fce_wide |> 
  rename(
    density_num_m = `density_num/m`,
    ind_drymass_g = `dmperind_g/ind`,
    ind_n_ug_hr = `nind_ug/hr`,
    ind_p_ug_hr = `pind_ug/hr`
  ) |> 
  select(
    -project, -habitat, -raw_filename, -row_num, -subsite_level3, -species, -`density_num/m2`
  ) |> 
  group_by(
    year, month, site, subsite_level1, subsite_level2
    ) |>
  summarize(
    n_tot_ug_m_hr = sum(ind_n_ug_hr*density_num_m, na.rm = TRUE),
    n_ind_sd = sd(ind_n_ug_hr, na.rm = TRUE),
    n_ind_sd_nozeros = sd(ind_n_ug_hr[ind_n_ug_hr != 0], na.rm = TRUE),
    n_ind_mean = mean(ind_n_ug_hr, na.rm = TRUE),
    n_ind_mean_nozeros = mean(ind_n_ug_hr[ind_n_ug_hr != 0], na.rm = TRUE),
    n_ind_cv = (n_ind_sd/n_ind_mean)*100,
    n_ind_cv_nozeros = (n_ind_sd_nozeros/n_ind_mean_nozeros)*100,
    p_tot_ug_m_hr = sum(ind_p_ug_hr*density_num_m, na.rm = TRUE),
    p_ind_sd = sd(ind_p_ug_hr, na.rm = TRUE),
    p_ind_sd_nozeros = sd(ind_p_ug_hr[ind_p_ug_hr != 0], na.rm = TRUE),
    p_ind_mean = mean(ind_p_ug_hr, na.rm = TRUE),
    p_ind_mean_nozeros = mean(ind_p_ug_hr[ind_p_ug_hr != 0], na.rm = TRUE),
    p_ind_cv = (p_ind_sd/p_ind_mean)*100,
    p_ind_cv_nozeros = (p_ind_sd_nozeros/p_ind_mean_nozeros)*100,
    bm_tot_m = sum(ind_drymass_g*density_num_m),
    bm_ind_sd = sd(ind_drymass_g, na.rm = TRUE),
    bm_ind_sd_nozeros = sd(ind_drymass_g[ind_drymass_g != 0], na.rm = TRUE),
    bm_ind_mean = mean(ind_drymass_g, na.rm = TRUE),
    bm_ind_mean_nozeros = mean(ind_drymass_g[ind_drymass_g != 0], na.rm = TRUE),
    bm_ind_cv = (bm_ind_sd/bm_ind_mean)*100,
    bm_ind_cv_nozeros = (bm_ind_sd_nozeros/bm_ind_mean_nozeros)*100,
    n_spp = n_distinct(scientific_name[ind_drymass_g != 0])
    ) 
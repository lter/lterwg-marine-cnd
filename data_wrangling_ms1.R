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
         invertebrate_n = if_else(vert == "invertebrate" & dmperind_g_ind != 0, 1, 0))

### remove all invertebrate data from FCE & VCR - makes up very small fraction and neither project poised at
### to monitor invertebrate populations/communities
dt_mutate_filter <- dt_mutate |> 
  filter(!(project %in% c("FCE", "VCR") & vert == "invertebrate")) |> 
  mutate(vert = ifelse(project %in% c("CCE", "NGA"), "invertebrate", vert))

dt_mutate_filter_2 <- dt_mutate_filter |> 
  filter(vert == "vertebrate",
         !project %in% c("NGA", "CCE", "PIE"))

# write_csv(dt_mutate_filter_2, "local_data/filtered_dataset.csv")

### calculate max size of community at this resolution so we can calculate mean max size of species within community
dt_mutate_filter_3 <- dt_mutate_filter_2 |>   
  group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
  mutate(max_size = max(dmperind_g_ind, na.rm = TRUE),
         min_size = min(dmperind_g_ind, na.rm = TRUE),
         mean_size = mean(dmperind_g_ind, na.rm = TRUE))

dt_total <- dt_mutate_filter_3 |> 
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
  

na_count_per_column <- sapply(dt_total, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

strata_list1 <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available")) |> 
  select(-subsite_level3) |> 
  distinct()

dt_total_strata <- left_join(dt_total, 
                             strata_list1, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2")) |> 
  mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
  dplyr::select(-ecoregion_habitat)

na_count_per_column <- sapply(dt_total_strata, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

###########################################################################
# generate pseudo date column for each project ----------------------------
###########################################################################

dt_total_strata_date <- dt_total_strata |>
  ### create project-habitat column since some projects sample multiple habitats (i.e., SBC ocean & beach)
  unite("projecthabitat", project, habitat, sep = "-", remove = FALSE) |> 
  ### create date columns for timeseries plotting
  mutate(sdate = ymd(paste(year, month, "01", sep = "-"))) 

###########################################################################
# set up individual projects/habitats for analyses and plotting -----------
###########################################################################

### Below I have separated each unique projecthabitat out to mutate new columns based on either
# the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
# their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
# whereas SBC wants their data summarized at the site level. This approach sets up
# an easy way to map plots across all unique projecthabitats, instead of doing them
# individually

### CCE-oceanic - NOT IN FIRST MANUSCRIPT
# cce <- dt_total_strata_date |> 
#   filter(projecthabitat == "CCE-oceanic") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm2') |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   unite(color2, c(site, color), sep = "-", remove = FALSE)

### CoastalCA-ocean
pisco_central <- dt_total_strata_date |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "CENTRAL") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-CENTRAL") |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

pisco_south <- dt_total_strata_date |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "SOUTH") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = strata,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-SOUTH") |> 
  group_by(subsite_level2, year) |> 
  ### removing three insane outlier in dataset that wasn't capture by initial filtering
  mutate(test = mean(total_nitrogen)) |>
  ungroup() |> 
  filter(!test > 75000) |> 
  dplyr::select(-test) |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

### FCE-estuary
fce <- dt_total_strata_date |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') |> #grouped at subsite_level1
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE)
  ### reverts back to hydrologic year to make more sense of dataset - data is collected across calendar years but considered sequential (i.e., November - June)
  ### this was done in step5
  # mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

### MCR-ocean
mcr <- dt_total_strata_date |> 
  filter(projecthabitat == "MCR-ocean") |> 
  ### join site and subsite_level1 according DB request for grouping variable
  unite("group", site, subsite_level1, sep = "-", remove = FALSE) |>
  mutate(group = group,
         color = subsite_level1,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level1, site), sep = "-", remove = FALSE)

### NGA-oceanic - NOT IN FIRST MANUSCRIPT
# nga <- dt_total_strata_date |> 
#   filter(projecthabitat == "NGA-oceanic") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm3') |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   unite(color2, c(site, color), sep = "-", remove = FALSE)

### PIE-estuary - NOT IN FIRST MANUSCRIPT
# pie <- dt_total_strata_date |> 
#   filter(projecthabitat == "PIE-estuary") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm2')  |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   mutate(color2 = site) # no unite function needed here to generate new 'color2' column

### SBC-beach - NOT IN FIRST MANUSCRIPT
# sbc_beach <- dt_total_strata_date |> 
#   filter(projecthabitat == "SBC-beach") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm') |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   unite(color2, c(site, color), sep = "-", remove = FALSE)

### SBC-ocean
sbc_reef <- dt_total_strata_date |> 
  filter(projecthabitat == "SBC-ocean") |> 
  mutate(group = site,
         color = strata,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(site, color), sep = "-", remove = FALSE)

### VCR-estuary
vcr <- dt_total_strata_date |> 
  filter(projecthabitat == "VCR-estuary") |> 
  mutate(group = subsite_level1,
         color = strata,
         units = 'm2') |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level1, color), sep = "-", remove = FALSE)

### binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
dat_ready <- bind_rows(fce, mcr, pisco_central, pisco_south, sbc_reef, vcr)

na_count_per_column <- sapply(dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

### tidy up working environment
rm(fce, mcr, pisco_central, pisco_south, sbc_reef, vcr)

###########################################################################
# clean up dataset names for plotting and analysis ------------------------
###########################################################################

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dat_ready$projecthabitat),
  Project = c("FCE", "MCR", "PCCC", "PCCS",
              "SBCO", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
  color = unique(dat_ready$color),
  Habitat = c(
              "Riverine", "Bay", #FCE
              "Back Reef", "Fore Reef", "Fringing Reef", #MCR
              "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, SBC-Beach, & SBC-Ocean
              "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

dat_ready_2 <- dat_ready |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  ### remove columns needed for joins up to this point
  select(-projecthabitat, -habitat, -project, -color, -site) |> 
  ### rename columns to be more representative/clean
  rename(site = color2,
         program = Project, 
         habitat = Habitat,
         date = sdate) |> 
  dplyr::select(program, habitat, year, month, date, vert, everything())

glimpse(dat_ready_2)
unique(dat_ready_2$habitat)

dat_ready_3 <- dat_ready_2 |> 
  filter(site != "RB-17")

# write_csv(dat_ready_3, "local_data/model_data_all_final_07032024.csv")

# step 2 ------------------------------------------------------------------

# exc <- read_csv("local_data/model_data_all_final_07032024.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv") |> 
  rename(program = project)
comm <- read_csv("local_data/community_data_filtered.csv") |> 
  select(-date) |> 
  rename(program = project)

### join data for modeling with site characteristic information
dt <- left_join(dat_ready_2, sc, join_by(program)) |> 
  select(program, site, year, month, everything()) |> 
  select(-n_spp, -date)

glimpse(dt)
unique(dt$program)

dt1 <- left_join(dt, comm, relationship = "many-to-many")
unique(dt$program)

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 

testna <- dt1 |> 
  filter(is.na(Species_Richness))
### these are instances where programs-sites observed zero fishes
### do we keep these or get rid of them? I initially got rid of them since we are interested
### in aspects of community but maybe should keep and make zeros?
### omit NAs - ran both ways and doesn't change the story at all, just the coefficients slightly
### the NAs make up ~1% of all data going into the models

# dt1[is.na(dt1)] <- 0
# dt2 <- dt1

dt2 <- na.omit(dt1)
dt3 <- dt2 |> 
  filter(total_nitrogen > 0)

na_count_per_column <- sapply(dt3, function(x) sum(is.na(x)))
print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt3 |> 
  group_by(program, habitat, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            cv_p = (sd(total_phosphorus, na.rm = TRUE) / mean(total_phosphorus, na.rm = TRUE)),
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            bm_stability = 1/cv_bm,
            mean_max_ss = mean(max_size),
            cv_max_ss = (sd(max_size, na.rm = TRUE) / mean(max_size, na.rm = TRUE)),
            max_size_stability = 1/cv_max_ss,
            mean_spp_rich = mean(Species_Richness),
            cv_spp_rich = (sd(Species_Richness, na.rm = TRUE) / mean(Species_Richness, na.rm = TRUE)),
            spp_rich_stability = 1/cv_spp_rich,
            mean_fam_rich = mean(Family_Richness),
            cv_fam_rich = (sd(Family_Richness, na.rm = TRUE) / mean(Family_Richness, na.rm = TRUE)),
            fam_rich_stability = 1/cv_fam_rich,
            mean_SppInvSimpDivInd = mean(Species_Inverse_Simpson_Diversity_Index),
            cv_SppInvSimpDivInd = (sd(Species_Inverse_Simpson_Diversity_Index, na.rm = TRUE) / mean(Species_Inverse_Simpson_Diversity_Index, na.rm = TRUE)),
            SppInvSimpDivInd_stability = 1/cv_SppInvSimpDivInd,
            mean_TrophInvSimpDivInd = mean(Trophic_Inverse_Simpson_Diversity_Index),
            cv_TrophInvSimpDivInd = (sd(Trophic_Inverse_Simpson_Diversity_Index, na.rm = TRUE) / mean(Trophic_Inverse_Simpson_Diversity_Index, na.rm = TRUE)),
            TrophInvSimpDivInd_stability = 1/cv_TrophInvSimpDivInd)|> 
  ### omit three sites with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
  na.omit() |> 
  ungroup()
unique(model_dt$program)

glimpse(model_dt)

write_csv(model_dt, "local_data/model_data_clean_final.csv")

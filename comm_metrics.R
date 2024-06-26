###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): Community calculations for stability GLMMs
###date(s): March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, vegan)

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
  dplyr::select(-lower_bound, -elasmo, -upper_bound, -outlier, -sharkray)

glimpse(dt_og)

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

strata_list1 <- dt_og |> 
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

dt_total_strata <- left_join(results, 
                             strata_list1, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2",
                                    "subsite_level3")) |> 
  mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
  dplyr::select(-ecoregion_habitat) |> 
  ### remove 172 duplicated rows since dt_total_strata should not be longer than dt_total
  distinct() 

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

na_count_per_column <- sapply(dt_total_strata_date, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

###########################################################################
# clean up dataset names for plotting and analysis ------------------------
###########################################################################

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dat_ready$projecthabitat),
  Project = c("FCE", "MCR", "PISCO-Central", "PISCO-South",
              "SBC-Ocean", "PIE", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
  color = unique(dat_ready$color),
  Habitat = c(
    "Riverine", "Bay", #FCE
    "Back Reef", "Fore Reef", "Fringing Reef", #MCR
    "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, & SBC-Ocean
    "Fertilized", "Natural", "Fertilized", "Natural", #PIE
    "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

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
  ### calculate max size of community at this resolution so we can calculate mean max size of species within community
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
  # mutate(max_size = max(dmperind_g_ind, na.rm = TRUE),
  #        min_size = min(dmperind_g_ind, na.rm = TRUE),
  #        mean_size = mean(dmperind_g_ind, na.rm = TRUE)) |> 
  ungroup()

### remove all invertebrate data from FCE & VCR - makes up very small fraction and neither project poised at
### to monitor invertebrate populations/communities
dt_mutate_filter <- dt_mutate |> 
  ### remove invertebrate datae from FCE & VCR
  filter(!(project %in% c("FCE", "VCR") & vert == "invertebrate")) |>
  ### remove all invertebrate data because going to focus on vertebrates 
  filter(!vert == "invertebrate") |> 
  ### remove CCE and NGA as they are not taxonomically resolved
  filter(!project %in% c("CCE", "NGA"))

glimpse(dt_mutate_filter)

rm(dt, dt_mutate, dt_og)

results <- dt_mutate_filter |> 
  filter(dmperind_g_ind != 0) |> 
  # group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3) |> 
  group_by(project, habitat, vert, year, month, site) |> 
  summarize(
    Species_Richness = length(unique(scientific_name)),
    Family_Richness = length(unique(family)),
    Species_Shannon_Diversity_Index = diversity(x = table(scientific_name), index = "shannon"),
    Species_Inverse_Simpson_Diversity_Index = diversity(x = table(scientific_name), index = "invsimpson"),
    Trophic_Shannon_Diversity_Index = diversity(x = table(diet_cat), index = "shannon"),
    Trophic_Inverse_Simpson_Diversity_Index = diversity(x = table(diet_cat), index = "invsimpson"),
    Fisher_Alpha_Div = fisher.alpha(table(scientific_name))
  ) |> 
  ungroup()

na_count_per_column <- sapply(results, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

gamma_diversity <- dt_mutate_filter |> 
  filter(dmperind_g_ind != 0) |> 
  group_by(project, vert, year, month) %>%
  summarize(
    gamma_div = length(unique(scientific_name))
  )

na_count_per_column <- sapply(gamma_diversity, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

# Calculating Beta Diversity (β) using Bray-Curtis method
beta_diversity <- dt_mutate_filter |> 
  filter(dmperind_g_ind != 0) |> 
  group_by(project, habitat, vert, year, month) |> 
  summarise(
    beta_jaccard_div = betadiver(as.matrix(table(site, scientific_name)), method = "jaccard"),
    beta_gower_div = betadiver(as.matrix(table(site, scientific_name)), method = "Gower")
  )

test <- dt_mutate_filter |> 
  filter(project == "MCR")

bdt <- test |> 
  mutate(site_full = paste(subsite_level1, site, sep = "")) |> 
  group_by(site_full, habitat, year, scientific_name) |> 
  summarise(count = n()) |> 
  ungroup()

matrix_view <- bdt |> 
  unite("site_habitat_year", site_full, strata, year, sep = "") |> 
  pivot_wider(names_from = scientific_name, values_from = count, values_fill = list(count = 0))

final_matrix <- as.matrix(matrix_view[-1])
rownames(final_matrix) <- matrix_view$site_habitat_year

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

strata_list1 <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

dt_total_strata <- left_join(results, 
                             strata_list1, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2",
                                    "subsite_level3")) |> 
  mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
  dplyr::select(-ecoregion_habitat) |> 
  ### remove 172 duplicated rows since dt_total_strata should not be longer than dt_total
  distinct() 

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

na_count_per_column <- sapply(dt_total_strata_date, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

###########################################################################
# clean up dataset names for plotting and analysis ------------------------
###########################################################################

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dat_ready$projecthabitat),
  Project = c("FCE", "MCR", "PISCO-Central", "PISCO-South",
              "SBC-Ocean", "PIE", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
  color = unique(dat_ready$color),
  Habitat = c(
    "Riverine", "Bay", #FCE
    "Back Reef", "Fore Reef", "Fringing Reef", #MCR
    "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, & SBC-Ocean
    "Fertilized", "Natural", "Fertilized", "Natural", #PIE
    "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

dat_ready_2 <- dat_ready |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  ### remove columns needed for joins up to this point
  select(-projecthabitat, -habitat, -project, -color, -site) |> 
  ### rename columns to be more representative/clean
  rename(site = color2,
         project = Project, 
         habitat = Habitat,
         date = sdate) |> 
  dplyr::select(project, habitat, year, month, date, vert, everything())
# write_csv(dat_ready_2, "local_data/community_data_filtered.csv")

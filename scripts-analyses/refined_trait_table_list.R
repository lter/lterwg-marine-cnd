###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): pulling out site-species-max size-diet_cat-information
###date(s): March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, vegan)

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
# ### calculate max size of community at this resolution so we can calculate mean max size of species within community
# group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |>
# mutate(max_size = max(dmperind_g_ind, na.rm = TRUE),
#        min_size = min(dmperind_g_ind, na.rm = TRUE),
#        mean_size = mean(dmperind_g_ind, na.rm = TRUE)) |> 
# ungroup()

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

# results <- dt_mutate_filter |> 
#   filter(dmperind_g_ind != 0) |> 
#   group_by(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3) %>%
#   summarize(
#     Species_Richness = length(unique(scientific_name)),
#     Family_Richness = length(unique(family)),
#     Species_Shannon_Diversity_Index = diversity(x = table(scientific_name), index = "shannon"),
#     Species_Inverse_Simpson_Diversity_Index = diversity(x = table(scientific_name), index = "invsimpson"),
#     Trophic_Shannon_Diversity_Index = diversity(x = table(diet_cat), index = "shannon"),
#     Trophic_Inverse_Simpson_Diversity_Index = diversity(x = table(diet_cat), index = "invsimpson")
#   ) |> 
#   ungroup()
# 
# na_count_per_column <- sapply(results, function(x) sum(is.na(x)))
# print(na_count_per_column) #yay

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

strata_list1 <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

dt_total_strata <- left_join(dt_mutate_filter, 
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

#####
sites <- dt_mutate_filter |> 
  mutate(project_site = paste(project, site, sep = "-")) |> 
  filter(project %in% c("MCR", "FCE", "CoastalCA"))
  # mutate(project = if_else(project == "CoastalCA", project_site, 
  #                          project)) |> 
  # mutate(project = case_when(
  #   project == "CoastalCA-CENTRAL" ~ "PCCC",
  #   project == "CoastalCA-SOUTH" ~ "PCCS",
  #   TRUE ~ project  # This line keeps all other values unchanged
  # )) 
glimpse(sites)  
unique(sites$project)

### for jenn casselle
CoastalCA <- sites |> 
  filter(project == "CoastalCA") |> 
  select(project, site, subsite_level2) |> 
  distinct()

write_csv(CoastalCA, "local_data/jenn-site-table-pisco.csv")

unique_site_table <- sites |> 
  group_by(project, scientific_name, diet_cat) |> 
  summarize(max_ind_size = max(dmperind_g_ind, na.rm = TRUE)) |> 
  filter(max_ind_size > 0)  
  # filter(project != "FCE")

write_csv(unique_site_table, "local_data/case_study_species_table.csv")
  
  
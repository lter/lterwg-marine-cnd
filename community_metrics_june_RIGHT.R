###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): Community calculations for stability GLMMs
###date(s): March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, vegan, codyn)

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
# set up individual projects/habitats for analyses and plotting -----------
###########################################################################

### Below I have separated each unique projecthabitat out to mutate new columns based on either
# the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
# their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
# whereas SBC wants their data summarized at the site level. This approach sets up
# an easy way to map plots across all unique projecthabitats, instead of doing them
# individually

# ### CCE-oceanic
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
  # mutate(test = mean(total_nitrogen)) |>
  # ungroup() |> 
  # filter(!test > 75000) |> 
  # dplyr::select(-test) |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

### FCE-estuary
fce <- dt_total_strata_date |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') |> #grouped at subsite_level1
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE) |> 
  ### reverts back to hydrologic year to make more sense of dataset - data is collected across calendar years but considered sequential (i.e., November - June)
  mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

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

# ### NGA-oceanic
# nga <- dt_total_strata_date |> 
#   filter(projecthabitat == "NGA-oceanic") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm3') |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   unite(color2, c(site, color), sep = "-", remove = FALSE)

### PIE-estuary
# pie <- dt_total_strata_date |> 
#   filter(projecthabitat == "PIE-estuary") |> 
#   mutate(group = site,
#          color = strata,
#          units = 'm2')  |> 
#   ### added new resolution group wants considered for examination -> functionally the "site" for each project
#   mutate(color2 = site) # no unite function needed here to generate new 'color2' column

### SBC-beach
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
    "Fringing Reef", "Back Reef", "Fore Reef", #MCR
    "Marine Protected Area", "Reference", #PISCO-Central, PISCO-South, & SBC-Ocean
    "Seagrass", "Sand")) #VCR
print(habitat_mapping) #yayayay

dat_ready_2 <- dat_ready |> 
  select(-date) |> 
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

na_count_per_column <- sapply(dat_ready_2, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

unique(dat_ready_2$habitat)
unique(dat_ready_2$site)

dat_ready_3 <- dat_ready_2 |> 
  filter(site != "RB-17")
# write_csv(dat_ready_2, "local_data/community_data_filtered.csv")

glimpse(dat_ready_3)

# DIVERSITY METRICS -------------------------------------------------------

dat_ready_4 <- dat_ready_3 |> 
  mutate(year_month = paste(year, month, sep = "-")) |> 
  select(year_month, everything())

step_1_div <- dat_ready_3 |>
  filter(dmperind_g_ind != 0) |>
  group_by(project, habitat, year_month, site) |> 
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

na_count_per_column <- sapply(step_1_div, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

# turnover metrics --------------------------------------------------------

species_presence <- dat_ready_4 |> 
  # filter(site != "RB-17") |> #remove earlier in process
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> 
  mutate(total_bm_m = sum(dmperind_g_ind*density_num_m, na.rm = TRUE),
    total_bm_m2 = sum(dmperind_g_ind*density_num_m2, na.rm = TRUE),
    total_bm_m3 = sum(dmperind_g_ind*density_num_m3, na.rm = TRUE),
    total_biomass = sum(total_bm_m + total_bm_m2 + total_bm_m3, na.rm = TRUE),
    total_density_m = sum(density_num_m, na.rm = TRUE),
    total_density_m2 = sum(density_num_m2, na.rm = TRUE),
    total_density_m3 = sum(density_num_m3, na.rm = TRUE),
    total_density = sum(total_density_m + total_density_m2 + total_density_m3, na.rm = TRUE)) |> 
  ungroup() |>
  dplyr::select(
                -total_bm_m, -total_bm_m2, -total_bm_m3) |> 
  arrange(project, habitat, vert, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) |> 
  group_by(project, habitat, year, site, scientific_name) |> 
  summarize(mean_total_bm = mean(total_biomass),
            mean_total_dens = mean(total_density)) |> 
  ungroup() |> 
  mutate(mean_total_dens_1p = ifelse(
    mean_total_dens > 0, mean_total_dens + 1, 0), 
    incidence = ifelse(
    mean_total_dens > 0, 1, 0))

test_2006 <- species_presence |> 
  filter(project == "MCR",
         habitat == "Fore Reef",
         site == "FO-1",
         year == 2006)

test_wider2006 <- test_2006 |> 
  select(scientific_name, incidence) |> 
  pivot_wider(names_from = "scientific_name", values_from = "incidence")

test_2007 <- species_presence |> 
  filter(project == "MCR",
         habitat == "Fore Reef",
         site == "FO-1",
         year == 2007)

sync_test <- synchrony (df=test,
                        time.var="year",
                        species.var="scientific_name",
                        abundance.var ="mean_total_bm",
                        metric = "Loreau",
                        replicate.var=NA)

test_wider2007 <- test_2007 |> 
  select(scientific_name, incidence) |> 
  pivot_wider(names_from = "scientific_name", values_from = "incidence")

mcr20062007 <- rbind(test_wider2006,test_wider2007)
beta <- vegdist(mcr20062007, method = 'jaccard')

turnover_test <- turnover(df = test, time.var = "year", 
                          abundance.var = "mean_total_dens_1p", 
                          species.var = "scientific_name",
                          metric = "total")

testy <- species_presence |> 
  mutate(psh = paste(project, habitat, site, sep = "-")) |> 
  filter(site != "TB-5")

#try to make accumulation curves

#for each phs randomly pull 1 through n species 
#for each random pull, calculate the stability of that comm
#replicate 1000 times 

mcr_test <- species_presence |> 
  mutate(psh = paste(project, habitat, site, sep = "-")) |> 
  filter(
    project == "MCR",
    habitat == "Fore Reef",
    site == "FO-1",
    mean_total_bm > 0
  )


unique(mcr_test$scientific_name)

library(dplyr)
library(purrr)

library(dplyr)
library(dplyr)

# Assume 'mcr_test' is your dataset already loaded into R
# Parameters for the analysis
num_replicates = 100

# Initialize a data frame to store issues
issues <- data.frame(site = character(), habitat = character(), num_species = integer(), replicate = integer(), count = integer())

# Function to perform the analysis for a given site and habitat
perform_analysis <- function(data) {
  results <- list()
  all_species <- unique(data$scientific_name)
  num_species <- length(all_species)
  
  for (replicate in 1:num_replicates) {
    replicate_results <- list()
    
    for (num_sampled in 1:num_species) {
      sampled_species <- sample(all_species, size = num_sampled)
      species_data <- data %>% filter(scientific_name %in% sampled_species)
      
      yearly_data <- species_data %>%
        group_by(year) %>%
        summarise(total_biomass = sum(mean_total_bm, na.rm = TRUE)) %>%
        pull(total_biomass)
      
      # Calculate stability as 1/CV
      tryCatch({
        mean_biomass <- mean(yearly_data)
        sd_biomass <- sd(yearly_data)
        if(mean_biomass > 0 && !is.na(sd_biomass)) {
          cv_biomass <- sd_biomass / mean_biomass
          if(cv_biomass > 0) {
            stability_score <- 1 / cv_biomass
          } else {
            stability_score <- NA
          }
        } else {
          stability_score <- NA
        }
      }, error = function(e) {
        stability_score <- NA
        issues <<- rbind(issues, data.frame(site = unique(data$site), habitat = unique(data$habitat), num_species = num_sampled, replicate = replicate, count = 1))
      })
      
      replicate_results[[num_sampled]] <- list(
        num_species = num_sampled,
        stability = stability_score,
        sampled_species = sampled_species
      )
    }
    
    results[[replicate]] <- replicate_results
  }
  
  return(results)
}

# Filter data for the project "MCR" and iterate over each site and habitat combination
project_data <- mcr_test %>% filter(project == "MCR")
unique_sites_habitats <- unique(project_data %>% select(site, habitat))

# Process each site and habitat
all_results <- list()
for (i in seq_along(unique_sites_habitats$site)) {
  site_data <- project_data %>% filter(site == unique_sites_habitats$site[i], habitat == unique_sites_habitats$habitat[i])
  all_results[[paste(unique_sites_habitats$site[i], unique_sites_habitats$habitat[i], sep = "_")]] <- perform_analysis(site_data)
}

# View issues
print(issues)

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming 'all_results' is your final output containing all the replicate lists
results_df <- data.frame(site_habitat = character(), replicate = integer(), num_species = integer(), stability = numeric())

for (site_habitat in names(all_results)) {
  site_habitat_results <- all_results[[site_habitat]]
  
  for (replicate in seq_along(site_habitat_results)) {
    replicate_data <- site_habitat_results[[replicate]]
    
    for (num_sampled in seq_along(replicate_data)) {
      species_data <- replicate_data[[num_sampled]]
      
      results_df <- rbind(results_df, data.frame(
        site_habitat = site_habitat,
        replicate = replicate,
        num_species = species_data$num_species,
        stability = species_data$stability
      ))
    }
  }
}












psh_vec = unique(testy$psh)
df_temp <- data.frame(matrix(ncol=3,nrow=length(psh_vec)))
df_temp[,1] <- psh_vec
names(df_temp)<-c("psh_vec","beta_time","synch")

for (i in 1:length(psh_vec)){
  temp <- testy |> 
  filter(psh == psh_vec[i])
  beta_temp <- turnover(df = temp, time.var = "year", 
           abundance.var = "mean_total_dens_1p", 
           species.var = "scientific_name",
           metric = "total")
  
  df_temp[i,2]<-mean(beta_temp[,1])
  
  
  synch_temp <- synchrony (df=temp,
             time.var="year",
             species.var="scientific_name",
             abundance.var ="mean_total_bm",
             metric = "Loreau",
             replicate.var=NA)
  df_temp[i,3]<-synch_temp
}

df_temp_final <- df_temp |> 
  separate(col = psh_vec, into = c("program", "habitat", "site", "number"), sep = "-")
  
  

write_csv(df_temp, "local_data/beta_synchrony_table.csv")



gamma_diversity <- dat_ready_4 |> 
  filter(dmperind_g_ind != 0) |> 
  group_by(project, year_month) |> 
  summarize(
    gamma_div = length(unique(scientific_name))
  )

na_count_per_column <- sapply(gamma_diversity, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

# ### beta diversity - got it working, but how do I get tied back in now?
# test <- dat_ready_4 |> 
#   filter(project == "MCR")
# 
# species_presence <- test |> 
#   filter(dmperind_g_ind != 0) |> 
#   group_by(year_month, habitat, site, scientific_name) |> 
#   summarise(count = n()) |> 
#   ungroup() |> 
#   pivot_wider(names_from = scientific_name, 
#               values_from = count, 
#               values_fill = list(count = 0)) 
# 
# # calculate Jaccard dissimilarities
# jaccard_diss <- vegdist(species_presence[, 4:ncol(species_presence)], method = "jaccard")
# 
# # compute beta diversity
# beta_div <- betadisper(jaccard_diss, group = species_presence$habitat)
# 
# distances <- beta_div$distances
# beta_df <- data.frame(year_month = species_presence$year_month,
#                       habitat = species_presence$habitat,
#                       dist = distances)
# 
# join_test <- left_join(test, beta_df)
# 
# beta_dftest <- data.frame(year_month = species_presence$year_month,
#                       habitat = species_presence$habitat,
#                       dist = distances) |> 
#   group_by(year_month, habitat) |> 
#   mutate(mean_dist = mean(dist))

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

dt_total_strata <- left_join(dt_og, 
                             strata_list, 
                             by = c("project", "habitat", "site",
                                    "subsite_level1", "subsite_level2",
                                    "subsite_level3")) |> 
  mutate(strata = if_else(is.na(ecoregion_habitat), site, ecoregion_habitat)) |> 
  dplyr::select(-ecoregion_habitat)

na_count_per_column <- sapply(dt_total_strata, function(x) sum(is.na(x)))
print(na_count_per_column) #yayay

### add psuedo-date to df

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
pie <- dt_total_strata_date |> 
  filter(projecthabitat == "PIE-estuary") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')  |> 
  ### added new resolution group wants considered for examination -> functionally the "site" for each project
  mutate(color2 = site) # no unite function needed here to generate new 'color2' column

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
dat_ready <- bind_rows(fce, mcr, pisco_central, pisco_south, sbc_reef,
                       pie, vcr)

na_count_per_column <- sapply(dat_ready, function(x) sum(is.na(x)))
print(na_count_per_column) #yay

### tidy up working environment
rm(fce, mcr, pisco_central, pisco_south, sbc_reef, pie, vcr)

unique(dat_ready$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dat_ready$projecthabitat),
  Project = c("FCE", "MCR", "PCCC", "PCCS",
              "SBCO", "PIE", "VCR")) 
print(label_mapping) #looks good

unique(dat_ready$color)
habitat_mapping <- data.frame(
  color = unique(dat_ready$color),
  Habitat = c(
    "Riverine", "Bay", #FCE
    "Fringing Reef", "Back Reef", "Fore Reef", #MCR
    "MPA", "Reference", #PISCO-Central, PISCO-South, & SBC-Ocean
    "Reference", "MPA", "Reference", "Reference", "Reference",
    "MPA", "Reference", "Reference", "Reference", "Reference",
    "Reference",
    "Fertilized", "Natural", "Natural", "Fertilized", "Fertilized", "" #PIE
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

### read in clean excretion and strata data from google drive
filter_dt <-read_csv("local_data/filtered_dataset.csv")

strata_list <- readxl::read_excel(path = file.path("tier2", "strata_class.xlsx"),na=".") |> 
  ### remove decimals from numbered sites
  mutate(site = str_remove(site, "\\.0$"),
         subsite_level1 = str_remove(subsite_level1, "\\.0$"),
         subsite_level2 = str_remove(subsite_level2, "\\.0$"),
         subsite_level3 = str_remove(subsite_level3, "\\.0$"))

###########################################################################
# add strata of interest to each project ----------------------------------
###########################################################################

strata_list1 <- strata_list %>%
  mutate(subsite_level1 = replace_na(subsite_level1, "Not Available"),
         subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available")) |> 
  select(-subsite_level3) |> 
  distinct()

dt_total_strata <- left_join(filter_dt, 
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
  filter(site != "RB-17") |> 
  rename(program = project)

glimpse(dat_ready_3)

# dat_ready_4 <- dat_ready_3 |> 
#   mutate(year_month = paste(year, month, sep = "-")) |> 
#   select(year_month, everything())

step_test <- exc |> 
  group_by(program, habitat, year, month, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            mean_p = mean(total_phosphorus),
            mean_bm = mean(total_biomass))

step_1_div <- dat_ready_3 |>
  filter(dmperind_g_ind != 0) |>
  group_by(program, habitat, year, month, site) |> 
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
print(na_count_per_column)

###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): preliminary changepoint analyses
###date(s): January - March 2024
###note(s): 
# fce data may make more sense if grouped by hydrologic year, for data averaged at that resolution
# df <- df |> 
# mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readr, Rbeast, forecast, changepoint)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN_summarized.csv"))

rm(list = ls()) #cleans env

### read in data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN_summarized.csv"),stringsAsFactors = F,na.strings =".") |> 
  mutate(sdate = as.Date(sdate))
glimpse(dt)


### update sites with new "color" according to how Deron thinks we should group
cce_plotting_annual <- dt |> 
  filter(projecthabitat == "CCE-oceanic") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

pisco_central_plotting_annual <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean-CENTRAL") |> 
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

pisco_south_plotting_annual <- dt |> 
  filter(projecthabitat == "CoastalCA-ocean-SOUTH") |> 
  unite(color2, c(subsite_level2, color), sep = "-", remove = FALSE)

sbc_beach_plotting_annual <- dt |> 
  filter(projecthabitat == "SBC-beach") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

nga_plotting_annual <- dt |> 
  filter(projecthabitat == "NGA-oceanic") |> 
  unite(color2, c(site, color), sep = "-", remove = FALSE)

pie_plotting_annual <- dt |> 
  filter(projecthabitat == "PIE-estuary") |> 
  mutate(color2 = site)

vcr_plotting_annual <- dt |> 
  filter(projecthabitat == "VCR-estuary") |> 
  unite(color2, c(subsite_level1, color), sep = "-", remove = FALSE)

fce_plotting_annual <- dt |>
  filter(projecthabitat == "FCE-estuary") |>
  unite(color2, c(site, subsite_level1), sep = "-", remove = FALSE) |> 
  mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

mcr_plotting_annual <- dt |>
  filter(projecthabitat == "MCR-ocean") |>
  unite(color2, c(subsite_level1, site), sep = "-", remove = FALSE)

sbc_ocean_plotting_annual <- dt |>
  filter(projecthabitat == "SBC-ocean") |>
  unite(color2, c(site, color), sep = "-", remove = FALSE)

dt <- bind_rows(cce_plotting_annual, pisco_central_plotting_annual, pisco_south_plotting_annual,
                fce_plotting_annual, mcr_plotting_annual, sbc_ocean_plotting_annual,
                sbc_beach_plotting_annual, nga_plotting_annual, pie_plotting_annual, 
                vcr_plotting_annual)

rm(cce_plotting_annual, pisco_central_plotting_annual, pisco_south_plotting_annual,
   fce_plotting_annual, mcr_plotting_annual, sbc_ocean_plotting_annual,
   sbc_beach_plotting_annual, nga_plotting_annual, pie_plotting_annual, 
   vcr_plotting_annual)

label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "PISCO-Central", "PISCO-South", "FCE", "MCR",
              "SBC-Ocean", "SBC-Beach", "NGA", 
              "PIE", "VCR")) 

habitat_mapping <- data.frame(
  color = unique(dt$color),
  Habitat = c("Nearshore", "Offshore","Marine Protected Area", "Reference", "Riverine", "Bay", 
              "Back Reef",
              "Fore Reef", "Fringing Reef",'Reference', "Seward", "Knight Island Passage",
              "Kodiak Island", "Middleton Island", "Prince William Sound", "Fertilized",
              "Natural", "Fertilized", "Natural", "Seagrass", "Sand")) 

dt_annual <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, Habitat, color2, year) |> 
  summarize(n_mean_annual = mean(total_n),
         p_mean_annual = mean(total_p),
         bm_mean_annual = mean(total_bm),
         ss_mean_annual = mean(mean_max_community_size)) |> 
  ungroup() |> 
  group_by(Project, Habitat, color2) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  filter(n > 10)

unique(dt_annual$Project)

# na_count_per_column <- sapply(dt_annual, function(x) sum(is.na(x)))
# print(na_count_per_column)

# Step 1: Detrend the time series for each site using LOESS
detrended_data <- dt_annual %>%
  group_by(color2) %>%
  do({
    loess_model <- loess(n_mean_annual ~ year, data = ., span = 0.75)  # Adjust span as needed
    trend <- predict(loess_model)
    detrended_values <- .$n_mean_annual - trend
    data.frame(year = .$year, n_mean_annual = detrended_values, color2 = unique(.$color2), Project = unique(.$Project), Habitat = unique(.$Habitat))
  }) %>%
  ungroup()

# Step 2: Calculate the slope for each site
slopes <- detrended_data %>%
  group_by(color2) %>%
  do({
    model <- lm(n_mean_annual ~ year, data = .)
    slope <- coef(model)["year"]
    data.frame(site = unique(.$color2), Project = unique(.$Project), Habitat = unique(.$Habitat), slope = slope)
  }) %>%
  ungroup()

# Step 3: Aggregate slope data by project and habitat
slope_summary <- slopes %>%
  group_by(Project, color2) %>%
  summarise(mean_slope = mean(slope), sd_slope = sd(slope), .groups = 'drop')

# Step 4: Plot the results
ggplot(slope_summary, aes(x = color2, y = mean_slope, fill = Project)) +
  geom_bar(stat = "identity", position = "dodge")
  # geom_errorbar(aes(ymin = mean_slope - sd_slope, ymax = mean_slope + sd_slope), position = position_dodge(width = 0.9), width = 0.25)

data <- dt_annual |> 
  group_by(Project, Habitat, year) |> 
  summarize(mean_n = mean(n_mean_annual))

ggplot(data, aes(x = year, y = mean_n, group = Habitat, color = Habitat)) +
  geom_line() +
  facet_wrap(~ Project, scales = 'free_y') +
  labs(title = "Time Series for Each Project-Habitat Combination",
       x = "Year",
       y = "Mean Annual Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Function to detect change points in a given time series
detect_change_points <- function(data) {
  # Assuming the data is already ordered by time (year)
  # Apply the cpt.mean function to detect changes in mean
  # You can also use cpt.var or cpt.meanvar depending on your specific needs
  cp_model <- cpt.mean(data$mean_n, method = "BinSeg", penalty = "SIC", Q = 3)#only grabbing first three, not most intense 3
  cpts <- cpts(cp_model)
  return(data.frame(change_point = data$year[cpts], Project = unique(data$Project), Habitat = unique(data$Habitat)))
}

# Apply the function to each project-habitat group
change_points <- data %>%
  group_by(Project, Habitat) %>%
  do(detect_change_points(.))

# View the change points
print(change_points)


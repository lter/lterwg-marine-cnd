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
librarian::shelf(tidyverse, readr, Rbeast, changepoint)

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

data <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  select(-projecthabitat, -habitat, -project) |> 
  group_by(Project, Habitat, year) |>
  summarize(n_mean= mean(total_n),
            n_sd = sd(total_n),
            p_mean = mean(total_p),
            p_sd = sd(total_p),
            bm_mean = mean(total_bm),
            bm_sd = sd(total_bm),
            ss_mean = mean(mean_max_community_size),
            ss_sd = sd(mean_max_community_size)) |>
  ungroup() |>
  group_by(Project, Habitat) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n > 10)

###########################################################################
# +/- 2 SD Approach -------------------------------------------------------
###########################################################################

data1 <- data |> 
  # filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  group_by(Project, Habitat) |> 
  arrange(Project, Habitat, year) |> 
  mutate(delta_n = n_mean - lag(n_mean),
         lag_year = year - 1,
         row_id = row_number()) |> 
  ungroup() |> 
  filter(row_id != 1) |> 
  group_by(Project, Habitat) |> 
  mutate(mean_delta_n = mean(delta_n),
         sd_delta_n = sd(delta_n),
         lower = mean_delta_n - 1.5 * sd_delta_n,
         upper = mean_delta_n + 1.5 * sd_delta_n) |> 
  ungroup() |> 
  dplyr::select(Project, Habitat, year, lag_year, delta_n, mean_delta_n, sd_delta_n, lower, upper)

data_og <- data |> 
  # filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  group_by(Project, Habitat) |> 
  arrange(Project, Habitat, year) |> 
  mutate(row_id = row_number()) |> 
  ungroup() |> 
  filter(row_id != 1) |> 
  dplyr::select(Project, Habitat, year, n_mean)

data_final <- left_join(data_og, data1) |> 
  mutate(abrupt = delta_n > upper | delta_n < lower)
glimpse(data_final)

data_final |> 
  # rename(ChangeVelocity = abrupt) |> 
  # filter(Project == "FCE", Habitat == "Riverine") |> 
  unite(site, Project, Habitat, sep = "-") |> 
  ggplot(aes(x = year, y = delta_n)) +
  geom_line(aes(group = 1), color = "blue") +  # Add a line connecting the points
  geom_point(aes(color = abrupt), size = 3) +  # Add points colored by 'abrupt'
  geom_hline(aes(yintercept = mean_delta_n), linetype = "dashed", color = "black") +  # Add mean_delta_n as a dashed line
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.2) +  # Highlight the area between 'lower' and 'upper'
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                     labels = c("TRUE" = "Abrupt Change", "FALSE" = "Stable")) +
  labs(title = "Mean Annual Change in Nitrogen Supply (1.5 +/- SD)",
       x = "Year",
       y = "n_mean") +
  facet_wrap(~site, scales = "free_y")+
  theme_minimal(legend)

###########################################################################
# Using changepoint package
###########################################################################
df <- data

### using "SegNeigh" method with Q = 4 and penalty value of 0.05
for (project in unique(df$Project)) {
  for (habitat in unique(df$Habitat[df$Project == project])) {
    subset_df <- df %>% filter(Project == project, Habitat == habitat)
    
    # Apply change point detection
    result <- cpt.mean(subset_df$n_mean, penalty = "Manual", pen.value = 0.05, Q=4,
                       method = "SegNeigh", test.stat = "Normal", class = TRUE)
    
    # Plot the results
    plot(subset_df$year, subset_df$n_mean, type = "l", main = paste("Change Points for", project, habitat),
         xlab = "Year", ylab = "n_mean")
    abline(v = subset_df$year[cpts(result)], col = "red", lwd = 2) # Add vertical lines for change points
  }
}

### using "SegNeigh" method with Q = 4 and penalty of BIC
for (project in unique(df$Project)) {
  for (habitat in unique(df$Habitat[df$Project == project])) {
    subset_df <- df %>% filter(Project == project, Habitat == habitat)
    
    # Apply change point detection
    result <- cpt.mean(subset_df$n_mean, penalty = "BIC", Q=4,
                       method = "SegNeigh", test.stat = "CUSUM", class = TRUE)
    
    # Plot the results
    plot(subset_df$year, subset_df$n_mean, type = "l", main = paste("Change Points for", project, habitat),
         xlab = "Year", ylab = "n_mean")
    abline(v = subset_df$year[cpts(result)], col = "red", lwd = 2) # Add vertical lines for change points
  }
}

###

for (project in unique(df$Project)) {
  for (habitat in unique(df$Habitat[df$Project == project])) {
    # Filter data for the current project-habitat combination
    subset_df <- df[df$Project == project & df$Habitat == habitat,]
    
    # Check if the subset is not empty
    if (nrow(subset_df) > 0) {
      # Apply beast function
      result <- beast(subset_df$n_mean, time = subset_df$year)
      
      # Plot the results
      plot(result, main = paste("Change Points for Project:", project, "Habitat:", habitat))
    }
  }
}

###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure two for manuscript
###date(s): January - March 2024
###note(s): 
# fce data may make more sense if grouped by hydrologic year, for data averaged at that resolution
# df <- df |> 
# mutate(year = if_else(project == "FCE" & month < 10, year - 1, year))

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel, purrr)

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

# set up labels/habitats for plotting -------------------------------------

label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "PISCO-Central", "PISCO-South", "FCE",
                 "MCR", "SBC-Ocean", "SBC-Beach", "NGA", 
                 "PIE", "VCR") # Replace with actual labels
)

habitat_mapping <- data.frame(
  color = unique(dt$color),
  Habitat = c("Nearshore", "Offshore","Riverine", "Bay", "Back Reef", "Fore Reef", "Fringing Reef",
              'Marine Protected Area', 'Reference', "Reference", "Seward", "Knight Island Passage",
              "Kodiak Island", "Middleton Island", "Prince William Sound", "Fertilized",
              "Natural", "Fertilized", "Natural", "Seagrass", "Sand")) 

cols = c("CCE" = '#00008B',
         "FCE" = '#3CB371',
         "MCR" = '#FF7F50',
         "PISCO-Central" = '#708090',
         "PISCO-South" = '#B0C4DE', 
         "SBC-Beach" = '#F4A460',
         "SBC-Ocean" = '#004953',
         "NGA" = '#1E90FF',
         "PIE" = '#556B2F',
         "VCR" = '#7CFC00')

# linear models to pull slopes --------------------------------------------

model_results <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  # filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  group_by(Project, color2) |> 
  do({
    model <- lm(sd_nitrogen ~ mean_nitrogen, data = .)
    data.frame(slope = coef(model)[2])
  }) |> 
  ungroup() |> 
  group_by(Project) |> 
  na.omit() |> 
  mutate(median_slope = median(slope)) |> 
  ungroup()

model_results |> 
  mutate(Project_label = factor(Project, levels = unique(Project[order(median_slope)]))) |> 
  ggplot(aes(x = Project_label, y = slope)) +
  geom_boxplot() +
  labs(x = "Project", y = "Slope: SD Nitrogen Supply ~ Mean Nitrogen Supply", title = "Box plot of Slopes by Project Habitat") +
  # scale_fill_manual("CCE" = '#00008B',
  #                   "FCE" = '#3CB371',
  #                   "MCR" = '#FF7F50',
  #                   "PISCO-Central" = '#708090',
  #                   "PISCO-South" = '#B0C4DE', 
  #                   "SBC-Beach" = '#F4A460',
  #                   "SBC-Ocean" = '#004953',
  #                   "NGA" = '#1E90FF',
  #                   "PIE" = '#556B2F',
  #                   "VCR" = '#7CFC00')+
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 14),
        plot.title = element_blank(),
        axis.text.y = element_text(face = "bold", size = 14, colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(
  filename = "slope_figtwo_nitrogen.tiff",
  path = "plots/",
  width = 14, height = 7
)

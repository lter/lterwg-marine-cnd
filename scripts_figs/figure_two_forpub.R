###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel, purrr, ggtext, svglite)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN_summarized.csv"))

rm(list = ls()) #cleans env

### read in data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN_summarized.csv"),stringsAsFactors = F,na.strings =".") |> 
  mutate(sdate = as.Date(sdate))
glimpse(dt)

label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "FCE", "MCR", "PISCO-Central",
              "PISCO-South", "SBC-Beach", "SBC-Ocean", "NGA", 
              "PIE", "VCR")) 

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
  ### revert back to hydrologic year to make more sense of dataset
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


###########################################################################
###########################################################################
# nitrogen ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# nitrogen supply ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2, year) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(mean_nitrogen_z, na.rm = TRUE),
            sd_total_nitrogen_z = mean(sd_nitrogen_z, na.rm = TRUE)) |>
  ungroup() |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = Project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Nitrogen Supply",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14),
        legend.position = "none")
        # axis.title.y = element_text(face = "bold", size = 28),
        # axis.title = element_text(face = "bold", size = 28),
        # legend.title.align = 0.5,
        # legend.title = element_text(face = "bold", size = 36, hjust = 0.5),
        # legend.text = element_text(face = "bold", size = 36),
        # legend.position = "bottom")
        # plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

# ggsave(
#   filename = "figure2a_z_nitrogen.tiff",
#   path = "plots/figure2/zscore/nitrogen/",
#   width = 12, height = 9
# )

###########################################################################
# nitrogen supply ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  group_by(Project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(mean_nitrogen_z, na.rm = TRUE),
            sd_total_nitrogen_z = mean(sd_nitrogen_z, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = Project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Nitrogen Supply",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
        # axis.title.y = element_text(face = "bold", size = 28),
        # axis.title = element_text(face = "bold", size = 28),
        # legend.title.align = 0.5,
        # legend.title = element_text(face = "bold", size = 14),
        # legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2b_z_nitrogen.tiff",
  path = "plots/figure2/zscore/nitrogen/",
  width = 12, height = 9
)

###########################################################################
###########################################################################
# phosphorus ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# phosphorus supply ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2, year) |> 
  mutate(mean_phosphorus = mean(total_p, na.rm = TRUE),
         sd_phosphorus = sd(total_p, na.rm = TRUE),
         sd_phosphorus = replace_na(sd_phosphorus, 0)) |> 
  ungroup() |> 
  group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2, year) |> #keeping project for plotting
  summarise(mean_total_phosphorus_z = mean(mean_phosphorus_z, na.rm = TRUE),
            sd_total_phosphorus_z = mean(sd_phosphorus_z, na.rm = TRUE)) |>
  ungroup() |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_phosphorus_z, y = sd_total_phosphorus_z, 
             color = Project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Phosphorus Supply",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2a_z_phosphorus.tiff",
  path = "plots/figure2/zscore/phosphorus/",
  width = 12, height = 9
)

###########################################################################
# phosphorus supply ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2) |> 
  mutate(mean_phosphorus = mean(total_p, na.rm = TRUE),
         sd_phosphorus = sd(total_p, na.rm = TRUE),
         sd_phosphorus = replace_na(sd_phosphorus, 0)) |> 
  ungroup() |> 
  group_by(Project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2) |> #keeping project for plotting
  summarise(mean_total_phosphorus_z = mean(mean_phosphorus_z, na.rm = TRUE),
            sd_total_phosphorus_z = mean(sd_phosphorus_z, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = mean_total_phosphorus_z, y = sd_total_phosphorus_z, 
             color = Project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Phosphorus Supply",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2b_z_phosphorus.tiff",
  path = "plots/figure2/zscore/phosphorus/",
  width = 12, height = 9
)


###########################################################################
###########################################################################
# biomass ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# biomass ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2, year) |> 
  mutate(mean_biomass = mean(total_bm, na.rm = TRUE),
         sd_biomass = sd(total_bm, na.rm = TRUE),
         sd_biomass = replace_na(sd_biomass, 0)) |> 
  ungroup() |> 
  group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2, year) |> #keeping project for plotting
  summarise(mean_total_biomass_z = mean(mean_biomass_z, na.rm = TRUE),
            sd_total_biomass_z = mean(sd_biomass_z, na.rm = TRUE)) |>
  ungroup() |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_biomass_z, y = sd_total_biomass_z, 
             color = Project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Total Dry Biomass",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2a_z_biomass.tiff",
  path = "plots/figure2/zscore/biomass/",
  width = 12, height = 9
)

###########################################################################
# biomass ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2) |> 
  mutate(mean_biomass = mean(total_bm, na.rm = TRUE),
         sd_biomass = sd(total_bm, na.rm = TRUE),
         sd_biomass = replace_na(sd_biomass, 0)) |> 
  ungroup() |> 
  group_by(Project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2) |> #keeping project for plotting
  summarise(mean_total_biomass_z = mean(mean_biomass_z, na.rm = TRUE),
            sd_total_biomass_z = mean(sd_biomass_z, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = mean_total_biomass_z, y = sd_total_biomass_z, 
             color = Project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Total Dry Biomass",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2b_z_biomass.tiff",
  path = "plots/figure2/zscore/biomass/",
  width = 12, height = 9
)


###########################################################################
###########################################################################
# MaxSize ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# MaxSize ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2, year) |> 
  mutate(mean_MaxSize = mean(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = sd(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = replace_na(sd_MaxSize, 0)) |> 
  ungroup() |> 
  group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2, year) |> #keeping project for plotting
  summarise(mean_total_MaxSize_z = mean(mean_MaxSize_z, na.rm = TRUE),
            sd_total_MaxSize_z = mean(sd_MaxSize_z, na.rm = TRUE)) |>
  ungroup() |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_MaxSize_z, y = sd_total_MaxSize_z, 
             color = Project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Size Structure",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2a_z_MaxSize.tiff",
  path = "plots/figure2/zscore/MaxSize/",
  width = 12, height = 9
)

###########################################################################
# MaxSize ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |>
  group_by(Project, color2) |> 
  mutate(mean_MaxSize = mean(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = sd(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = replace_na(sd_MaxSize, 0)) |> 
  ungroup() |> 
  group_by(Project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(Project, color2) |> #keeping project for plotting
  summarise(mean_total_MaxSize_z = mean(mean_MaxSize_z, na.rm = TRUE),
            sd_total_MaxSize_z = mean(sd_MaxSize_z, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = mean_total_MaxSize_z, y = sd_total_MaxSize_z, 
             color = Project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  scale_color_manual(values = cols) +
  theme_classic() +
  # labs(title = "Size Structure",
  #      x = 'Z-Scored Mean',
  #      y = 'Z-Scored Std. Dev.') +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14))
# axis.title.y = element_text(face = "bold", size = 28),
# axis.title = element_text(face = "bold", size = 28),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 14),
# legend.text = element_text(face = "bold", size = 14))
# plot.title = element_text(face = "bold", size = 48, hjust = 0.5))

ggsave(
  filename = "figure2b_z_MaxSize.tiff",
  path = "plots/figure2/zscore/MaxSize/",
  width = 12, height = 9
)

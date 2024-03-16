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

###########################################################################
###########################################################################
# nitrogen ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# nitrogen supply ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  group_by(projecthabitat, color2, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2, year) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(mean_nitrogen_z, na.rm = TRUE),
            sd_total_nitrogen_z = mean(sd_nitrogen_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2A: Z-Scored Nitrogen ~ Space Per Year (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Annual Areal Nitrogen Supply (ug/hr/m_m2)',
       y = 'Z-Scored SD Annual Areal Nitrogen Supply (ug/hr/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_z_nitrogen.png",
#   path = "plots/figure2/zscore/nitrogen/",
#   width = 15, height = 9
# )

###########################################################################
# nitrogen supply ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(project, color2) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  group_by(project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(mean_nitrogen_z, na.rm = TRUE),
            sd_total_nitrogen_z = mean(sd_nitrogen_z, na.rm = TRUE)) |>
              ungroup() |>
              # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
              # unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
              ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
                         color = project, label = color2)) + 
              geom_point(alpha = 0.9, size = 4) +
              geom_text_repel() +
              geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
              theme_classic() +
              labs(title = "Figure 2B: Z-Scored Nitrogen ~ Space (FCE, MCR, SBC)",
                   x = 'Z-Scored Mean Areal Nitrogen Supply (ug/hr/m_m2)',
                   y = 'Z-Scored SD Areal Nitrogen Supply (ug/hr/m_m2)') +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.background = element_rect(fill = "white"),
                    axis.line = element_line("black"),
                    axis.text = element_text(face = "bold"),
                    axis.title = element_text(face = "bold"),
                    legend.position = "right")

# ggsave(
#   filename = "figure2b_z_nitrogen.png",
#   path = "plots/figure2/zscore/nitrogen/",
#   width = 15, height = 9
# )

###########################################################################
###########################################################################
# phosphorus ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# phosphorus supply ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  group_by(projecthabitat, color2, year) |> 
  mutate(mean_phosphorus = mean(total_p, na.rm = TRUE),
         sd_phosphorus = sd(total_p, na.rm = TRUE),
         sd_phosphorus = replace_na(sd_phosphorus, 0)) |> 
  ungroup() |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2, year) |> #keeping project for plotting
  summarise(mean_total_phosphorus_z = mean(mean_phosphorus_z, na.rm = TRUE),
            sd_total_phosphorus_z = mean(sd_phosphorus_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_phosphorus_z, y = sd_total_phosphorus_z, 
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2A: Z-Scored Phosphorus ~ Space Per Year (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Annual Areal Phosphorus Supply (ug/hr/m_m2)',
       y = 'Z-Scored SD Annual Areal Phosphorus Supply (ug/hr/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_z_phosphorus.png",
#   path = "plots/figure2/zscore/phosphorus/",
#   width = 15, height = 9
# )

###########################################################################
# phosphorus supply ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(project, color2) |> 
  mutate(mean_phosphorus = mean(total_p, na.rm = TRUE),
         sd_phosphorus = sd(total_p, na.rm = TRUE),
         sd_phosphorus = replace_na(sd_phosphorus, 0)) |> 
  ungroup() |> 
  group_by(project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2) |> #keeping project for plotting
  summarise(mean_total_phosphorus_z = mean(mean_phosphorus_z, na.rm = TRUE),
            sd_total_phosphorus_z = mean(sd_phosphorus_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  # unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_phosphorus_z, y = sd_total_phosphorus_z, 
             color = project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2B: Z-Scored Phosphorus Supply ~ Space (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Areal Phosphorus Supply (ug/hr/m_m2)',
       y = 'Z-Scored SD Areal Phosphorus Supply (ug/hr/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_z_phosphorus.png",
#   path = "plots/figure2/zscore/phosphorus/",
#   width = 15, height = 9
# )


###########################################################################
###########################################################################
# biomass ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# biomass ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  group_by(projecthabitat, color2, year) |> 
  mutate(mean_biomass = mean(total_bm, na.rm = TRUE),
         sd_biomass = sd(total_bm, na.rm = TRUE),
         sd_biomass = replace_na(sd_biomass, 0)) |> 
  ungroup() |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2, year) |> #keeping project for plotting
  summarise(mean_total_biomass_z = mean(mean_biomass_z, na.rm = TRUE),
            sd_total_biomass_z = mean(sd_biomass_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_biomass_z, y = sd_total_biomass_z, 
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2A: Z-Scored Total Dry Biomass ~ Space Per Year (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'Z-Scored SD Annual Total Dry Biomass (g/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_z_biomass.png",
#   path = "plots/figure2/zscore/biomass/",
#   width = 15, height = 9
# )

###########################################################################
# biomass ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(project, color2) |> 
  mutate(mean_biomass = mean(total_bm, na.rm = TRUE),
         sd_biomass = sd(total_bm, na.rm = TRUE),
         sd_biomass = replace_na(sd_biomass, 0)) |> 
  ungroup() |> 
  group_by(project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2) |> #keeping project for plotting
  summarise(mean_total_biomass_z = mean(mean_biomass_z, na.rm = TRUE),
            sd_total_biomass_z = mean(sd_biomass_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  # unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_biomass_z, y = sd_total_biomass_z, 
             color = project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2B: Z-Scored Total Dry Biomass ~ Space (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Total Dry Biomass (g/m_m2)',
       y = 'Z-Scored SD Total Dry Biomass (g/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_z_biomass.png",
#   path = "plots/figure2/zscore/biomass/",
#   width = 15, height = 9
# )


###########################################################################
###########################################################################
# MaxSize ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# MaxSize ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  group_by(projecthabitat, color2, year) |> 
  mutate(mean_MaxSize = mean(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = sd(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = replace_na(sd_MaxSize, 0)) |> 
  ungroup() |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2, year) |> #keeping project for plotting
  summarise(mean_total_MaxSize_z = mean(mean_MaxSize_z, na.rm = TRUE),
            sd_total_MaxSize_z = mean(sd_MaxSize_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_MaxSize_z, y = sd_total_MaxSize_z, 
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2A: Z-Scored Max Size ~ Space Per Year (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Annual Max Size (g)',
       y = 'Z-Scored SD Annual Max Size (g)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_z_MaxSize.png",
#   path = "plots/figure2/zscore/MaxSize/",
#   width = 15, height = 9
# )

###########################################################################
# MaxSize ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(project, color2) |> 
  mutate(mean_MaxSize = mean(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = sd(mean_max_community_size, na.rm = TRUE),
         sd_MaxSize = replace_na(sd_MaxSize, 0)) |> 
  ungroup() |> 
  group_by(project) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(project, color2) |> #keeping project for plotting
  summarise(mean_total_MaxSize_z = mean(mean_MaxSize_z, na.rm = TRUE),
            sd_total_MaxSize_z = mean(sd_MaxSize_z, na.rm = TRUE)) |>
  ungroup() |>
  # unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  # unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = mean_total_MaxSize_z, y = sd_total_MaxSize_z, 
             color = project, label = color2)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2B: Z-Scored Max Size ~ Space (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Max Size (g)',
       y = 'Z-Scored SD Max Size (g)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_z_MaxSize.png",
#   path = "plots/figure2/zscore/MaxSize/",
#   width = 15, height = 9
# )
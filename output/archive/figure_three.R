###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure three for manuscript
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
###########################################################################
###########################################################################
# cv of both nutrient supply and biomass ----------------------------------
###########################################################################
###########################################################################
###########################################################################
###########################################################################


###########################################################################
###########################################################################
# nitrogen ----------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# cv mean annual nitrogen supply ~ cv mean annual biomass (subset) --------
###########################################################################

dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  group_by(project, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_n_cv,
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 3A: CV of Mean Annual N Supply ~ CV of Mean Annual Biomass",
       x = 'CV Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'CV Mean Annual Areal Nitrogen Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "n_bm_cv_figure3_subset.png",
#   path = "plots/figure3",
#   width = 15, height = 9
# )

###########################################################################
# cv mean annual nitrogen supply ~ cv mean annual biomass (all sites) -----
###########################################################################

dt |> 
  # filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  group_by(projecthabitat, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_n_cv,
             color = projecthabitat, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 3A: CV of Mean Annual N Supply ~ CV of Mean Annual Biomass",
       x = 'CV Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'CV Mean Annual Areal Nitrogen Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "n_bm_cv_figure3_all.png",
#   path = "plots/figure3",
#   width = 15, height = 9
# )

###########################################################################
###########################################################################
# phosphorus --------------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# cv mean annual phosphorus supply ~ cv mean annual biomass (subset)-------
###########################################################################

dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  group_by(project, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_p_cv,
             color = project, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 3A: CV of Mean Annual P Supply ~ CV of Mean Annual Biomass",
       x = 'CV Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'CV Mean Annual Areal Phosphorus Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "p_bm_cv_figure3_subset.png",
#   path = "plots/figure3",
#   width = 15, height = 9
# )

###########################################################################
# cv mean annual phosphorus supply ~ cv mean annual biomass (all sites)----
###########################################################################

dt |> 
  # filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  group_by(projecthabitat, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_p_cv,
             color = projecthabitat, label = year_site)) + 
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 3A: CV of Mean Annual P Supply ~ CV of Mean Annual Biomass",
       x = 'CV Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'CV Mean Annual Areal Phosphorus Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "p_bm_cv_figure3_all.png",
#   path = "plots/figure3",
#   width = 15, height = 9
# )
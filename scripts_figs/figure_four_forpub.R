###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure four for manuscript
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


# add labels and colors for figures ---------------------------------------

unique(dt$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "PISCO-Central", "PISCO-South", "FCE", "MCR", 
              "SBC-Ocean", "SBC-Beach", "NGA", "PIE", "VCR")) 
print(label_mapping)

unique(dt$color)
habitat_mapping <- data.frame(
  color = unique(dt$color),
  Habitat = c("Nearshore", "Offshore", 'Marine Protected Area', 'Reference', 
              "Riverine", "Bay", "Back Reef", "Fore Reef", "Fringing Reef",
               "Reference", "Seward", "Knight Island Passage",
              "Kodiak Island", "Middleton Island", "Prince William Sound", "Fertilized",
              "Natural", "Fertilized", "Natural", "Seagrass", "Sand")) 
print(habitat_mapping)

unique(label_mapping$Project)

cols = c("CCE" = '#00008B',
         "PISCO-Central" = '#708090',
         "PISCO-South" = '#B0C4DE', 
         "FCE" = '#3CB371',
         "MCR" = '#FF7F50',
         "SBC-Ocean" = '#004953',
         "SBC-Beach" = '#F4A460',
         "NGA" = '#1E90FF',
         "PIE" = '#556B2F',
         "VCR" = '#7CFC00')


###########################################################################
# residuals for cv n supply ~ cv biomass ----------------------------------
###########################################################################

dt_cv <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, color2) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  filter(!is.na(total_n_cv)) |> #~30 obs with NAs, omitted
  ungroup() 

dt_residuals <- dt_cv |>
  mutate(residual = resid(lm(total_n_cv ~ total_bm_cv, data = dt_cv)))

dt_residuals |> 
# filter(!Project == "CCE") |> #for plot 1b
# ggplot(aes(x = reorder(Project, -residual), y = log(residual), fill = Project)) +
ggplot(aes(x = reorder(Project, residual), y = residual, fill = Project)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  theme_classic() +
  labs(y = "Residuals", x = "Project",
       title = "CV data residuals: nitrogen supply ~ biomass") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        title = element_text(face = "bold", size = 18))

# ggsave(
#   filename = "fig_4_example1a.png",
#   path = "plots/",
#   width = 15, height = 9
# )

# ggsave(
#   filename = "fig_4_example1b.png",
#   path = "plots/",
#   width = 15, height = 9
# )

###########################################################################
# residuals for zscored sd n supply ~ mean n supply -----------------------
###########################################################################

dt_z <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, Habitat, color2, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |>
  ungroup() |>
  group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  summarize(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |> 
  dplyr::select(Project, color2, mean_nitrogen_z, sd_nitrogen_z) |> 
  distinct() |> 
  filter(!is.na(mean_nitrogen_z),
         !is.na(sd_nitrogen_z))

dt_residuals_z <- dt_z |>
  mutate(residual = resid(lm(sd_nitrogen_z ~ mean_nitrogen_z, data = dt_z)))

dt_residuals_z |> 
  ggplot(aes(x = reorder(Project, -residual), y = residual, fill = Project)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  theme_classic() +
  labs(y = "Residuals", x = "Project",
       title = "z-scored data residuals: sd nitrogen supply ~ mean nitrogen supply") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        title = element_text(face = "bold", size = 18))

# ggsave(
#   filename = "fig_4_example2.png",
#   path = "plots/",
#   width = 15, height = 9
# )

###########################################################################
# residuals for raw sd n supply ~ mean n supply ---------------------------
###########################################################################

dt_raw <- dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, Habitat, color2, year) |> 
  summarize(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |>
  # group_by(Project, color2) |> #grouping by specific project and strata of interest to generate z-score calculations
  # summarize(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  # ungroup() |> 
  filter(!is.na(mean_nitrogen),
         !is.na(sd_nitrogen))

dt_residuals_raw <- dt_raw |>
  mutate(residual = resid(lm(sd_nitrogen ~ mean_nitrogen, data = dt_raw)))

dt_residuals_raw |> 
  # filter(!Project == "SBC-Beach") |>
  ggplot(aes(x = reorder(Project, -residual), y = residual, fill = Project)) +
  # ggplot(aes(x = reorder(Project, -residual), y = log(residual), fill = Project)) +
  geom_boxplot() +
  scale_fill_manual(values = cols) +
  theme_classic() +
  labs(y = "Residuals", x = "Project",
       title = "raw data residuals: sd nitrogen supply ~ mean nitrogen supply") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        title = element_text(face = "bold", size = 18))

# ggsave(
#   filename = "fig_4_example3.png",
#   path = "plots/",
#   width = 15, height = 9
# )

# ggsave(
#   filename = "fig_4_example4.png",
#   path = "plots/",
#   width = 15, height = 9
# )

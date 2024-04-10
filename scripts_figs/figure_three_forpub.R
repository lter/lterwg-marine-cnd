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

rm(cce_plotting_annual, pisco_central_plotting_annual, pisco_south_plotting_annual,
   fce_plotting_annual, mcr_plotting_annual, sbc_ocean_plotting_annual,
   sbc_beach_plotting_annual, nga_plotting_annual, pie_plotting_annual, 
   vcr_plotting_annual)


# add labels and colors for figures ---------------------------------------

unique(dt$projecthabitat)
label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  Project = c("CCE", "PISCO-Central", "PISCO-South", "FCE",
              "MCR","SBC-Ocean", "SBC-Beach", "NGA", 
              "PIE", "VCR")) 
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
# cv mean annual nitrogen supply ~ cv mean annual biomass (all sites) -----
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_n_cv,
             color = Project)) + 
  geom_point(alpha = 0.9, size = 4) +
  scale_color_manual(values = cols) +
  # geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(x = expression('CV Mean Annual Total Dry Biomass (g/m-m'^2~'-m'^3~')'),
       y = expression('CV Mean Annual Areal Nitrogen Supply (ug/h/m-m'^2~'-m'^3~')')) +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 14))
        # axis.title.x = element_text(face = "bold", size = 20),
        # axis.title.y = element_text(face = "bold", size = 20),
        # # axis.title = element_text(face = "bold", size = 20),
        # legend.title.align = 0.5,
        # legend.title = element_text(face = "bold", size = 20, hjust = 0.5),
        # legend.text = element_text(face = "bold", size = 20),
        # legend.position = "bottom")


ggsave(
  filename = "n_bm_cv_figure3_all.tiff",
  path = "plots/figure3",
  width = 15, height = 9
)

###########################################################################
# cv mean annual phosphorus supply ~ cv mean annual biomass (all sites)----
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |>
  left_join(habitat_mapping, by = "color") |>
  group_by(Project, color2, year) |> 
  summarise(across(where(is.numeric), 
                   ~ (sd(., na.rm = TRUE) / mean(., na.rm = TRUE)) * 100, 
                   .names = "{.col}_cv")) |> 
  ungroup() |> 
  unite(year_site, c(year, color2), sep = "-", remove = FALSE) |>
  ggplot(aes(x = total_bm_cv, y = total_p_cv,
             color = Project)) + 
  geom_point(alpha = 0.9, size = 4) +
  scale_color_manual(values = cols) +
  # geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(x = expression('CV Mean Annual Total Dry Biomass (g/m-m'^2~'-m'^3~')'),
       y = expression('CV Mean Annual Areal Nitrogen Supply (ug/h/m-m'^2~'-m'^3~')')) +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))
# axis.title.x = element_text(face = "bold", size = 20),
# axis.title.y = element_text(face = "bold", size = 20),
# # axis.title = element_text(face = "bold", size = 20),
# legend.title.align = 0.5,
# legend.title = element_text(face = "bold", size = 20, hjust = 0.5),
# legend.text = element_text(face = "bold", size = 20),
# legend.position = "bottom")


ggsave(
  filename = "p_bm_cv_figure3_all_.tiff",
  path = "plots/figure3",
  width = 15, height = 9
)

###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure two for manuscript
###date(s): January - March 2024
###note(s): 

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

###########################################################################
# nitrogen supply ~ space and time (z-scored by project and strata) -------
###########################################################################

dt |> 
  group_by(projecthabitat, color, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  ungroup() |>
  group_by(projecthabitat, color, year) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(mean_nitrogen_z), na.rm = TRUE,
            sd_total_nitrogen_z = mean(sd_nitrogen_z), na.rm = TRUE) |>
  ungroup() |>
  unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> #only needed for all together figure
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = p_strata, label = year)) + #change label to only year for facet_wrapped plot
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2A: Z-Scored Nitrogen Across Space Per Year (FCE, MCR, SBC)",
       x = 'Z-Scored Mean Annual Areal Nitrogen Supply (ug/hr/m_m2)',
       y = 'Z-Scored SD Annual Nitrogen Supply (ug/hr/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(
  filename = "figure2a_z_nitrogen.png",
  path = "plots/figure2/zscore/nitrogen/",
  width = 15, height = 9
)

dt %>%
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) %>%
  group_by(projecthabitat, color, year) %>%
  arrange(year) %>%
  mutate(change_total_n = total_n - lag(total_n)) |> 
  summarise(mean_change_total_n = mean(change_total_n, na.rm = TRUE),
            sd_change_total_n = sd(change_total_n, na.rm = TRUE)) %>%
  mutate(mean_change_total_n_z = (mean_change_total_n - mean(mean_change_total_n)) / sd(mean_change_total_n),
         sd_change_total_n_z = (sd_change_total_n - mean(sd_change_total_n)) / sd(sd_change_total_n)) %>%
  mutate(row_id = row_number()) |> 
  ungroup() |> 
  filter(row_id != 1) |> 
  unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> #only needed for all together figure
  ggplot(aes(x = mean_change_total_n_z, y = sd_change_total_n_z, color = p_strata, label = year)) +
  geom_point() +
  geom_text_repel() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "Z-Scored Annual Change in total_n",
       x = "Z-Scored Mean Annual Change in Areal Nitrogen Supply (ug/hr/m_m2)",
       y = "Z-Scored SD Annual Change in Areal Nitrogen Supply (ug/hr/m_m2)")

ggsave(
  filename = "figure2b_z_nitrogen.png",
  path = "plots/figure2/zscore/nitrogen/",
  width = 15, height = 9
)

###########################################################################
# nitrogen supply ~ space (raw by project and strata)-----------------
###########################################################################

dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |>
  group_by(projecthabitat, color) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(total_n), na.rm = TRUE,
            sd_total_nitrogen_z = sd(total_n), na.rm = TRUE) |>
  ungroup() |>
  unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> #only needed for all together figure
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = p_strata, label = p_strata)) + #change label to only year for facet_wrapped plot
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2C: Nitrogen Supply Across Space (FCE, MCR, SBC)",
       x = 'Mean Annual Areal Nitrogen Supply (ug/hr/m_m2)',
       y = 'SD Annual Areal Nitrogen Supply (ug/hr/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(
  filename = "figure2c_nitrogen.png",
  path = "plots/figure2/zscore/nitrogen/",
  width = 15, height = 9
)

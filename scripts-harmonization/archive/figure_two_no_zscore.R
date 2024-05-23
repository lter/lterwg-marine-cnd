###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate plots for figure two of ms one
###date(s): January 2024
###note(s): 
###### 02-10-2024: 
## THIS IS VERSION WHERE DATA IS NOT Z-SCORED FOR FIGURES

###########################################################################
# load necessary packages -------------------------------------------------
###########################################################################

# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel)

dat <- read_csv("data/exc_clean_02082024.csv")
glimpse(dat)

# Replace NA in subsite_level2 and subsite_level3 with a placeholder (e.g., "Not Available")
# Making a character column instead of true 'NA', allows for the group_by function to go as far as
## it can in sequence for grouping, which is what we want here
df <- dat %>%
  mutate(subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available"))

# Group by the specified columns and summarize
df_summarized <- df %>%
  group_by(project, habitat, year, month, site, subsite_level1, subsite_level2, subsite_level3) %>%
  summarise(total_n_ug_hr_m = sum(nind_ug_hr * density_num_m, na.rm = TRUE),
            total_n_ug_hr_m2 = sum(nind_ug_hr * density_num_m2, na.rm = TRUE),
            total_n = sum(total_n_ug_hr_m + total_n_ug_hr_m2, na.rm = TRUE),
            n_ind_sd = sd(nind_ug_hr, na.rm = TRUE),
            n_ind_sd_nozeros = sd(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            n_ind_mean = mean(nind_ug_hr, na.rm = TRUE),
            n_ind_mean_nozeros = mean(nind_ug_hr[nind_ug_hr != 0], na.rm = TRUE),
            n_ind_cv = (n_ind_sd/n_ind_mean)*100,
            n_ind_cv_nozeros = (n_ind_sd_nozeros/n_ind_mean_nozeros)*100,
            total_p_ug_hr_m = sum(pind_ug_hr * density_num_m, na.rm = TRUE),
            total_p_ug_hr_m2 = sum(pind_ug_hr * density_num_m2, na.rm = TRUE),
            total_p = sum(total_p_ug_hr_m + total_p_ug_hr_m2, na.rm = TRUE),
            p_ind_sd = sd(pind_ug_hr, na.rm = TRUE),
            p_ind_sd_nozeros = sd(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            p_ind_mean = mean(pind_ug_hr, na.rm = TRUE),
            p_ind_mean_nozeros = mean(pind_ug_hr[pind_ug_hr != 0], na.rm = TRUE),
            p_ind_cv = (p_ind_sd/p_ind_mean)*100,
            p_ind_cv_nozeros = (p_ind_sd_nozeros/p_ind_mean_nozeros)*100,
            total_bm_m = sum(dmperind_g_ind*density_num_m),
            total_bm_m2 = sum(dmperind_g_ind*density_num_m2),
            total_bm = total_bm_m + total_bm_m2,
            bm_ind_sd = sd(dmperind_g_ind, na.rm = TRUE),
            bm_ind_sd_nozeros = sd(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            bm_ind_mean = mean(dmperind_g_ind, na.rm = TRUE),
            bm_ind_mean_nozeros = mean(dmperind_g_ind[dmperind_g_ind != 0], na.rm = TRUE),
            bm_ind_cv = (bm_ind_sd/bm_ind_mean)*100,
            bm_ind_cv_nozeros = (bm_ind_sd_nozeros/bm_ind_mean_nozeros)*100,
            n_spp = n_distinct(scientific_name[dmperind_g_ind != 0])
  )

# read in strata file from online -----------------------------------------
# this allows us to determine the spatial/otherwise unit folks want their data grouped by
strata <- read_csv("data/strata_mcr_sbc_fce.csv") |> 
  distinct() |> 
  filter(project %in% c("MCR", "SBC", "FCE")) #as of 2/09/2024 these are only projects who have filled out

#left join with summarized dataset to ensure strata information is there for plotting
df_summarized1 <- left_join(df_summarized, strata, by = c("project", "habitat", "site"))

# write.csv(df_summarized1, "data/summarized_lter_exc_clean.csv") #this is copy of summarized dataset with strata information

# lets do some plotting ---------------------------------------------------

dat <- read_csv("data/summarized_lter_exc_clean.csv") |> 
  unite("projecthabitat", project, habitat, sep = "-", remove = FALSE) |> #since SBC has two habitats monitored, joining here for distinguishing column for plotting
  mutate(sdate = ymd(paste(year, month, "01", sep = "-"))) #generating date for plotting timeseries

# Setup individual 'projecthabitats' for plotting -------------------------
# Below I have seperated each unique projecthabitat out to mutate new columns based on either
## the strata they want their data colored by (i.e., color = XXXX)and the level to which they want
## their data summarized (i.e., for FCE-estuary, I want summarized at subsite_level1..
## whereas SBC wants their data summarized at the site level. This approach sets up
## an easy way to map plots across all unique projecthabitats, instead of doing them
## individually

fce <- dat |> 
  filter(projecthabitat == "FCE-estuary") |>
  mutate(group = subsite_level1,
         color = strata,
         units = 'm') #group at subsite_level1

mcr <- dat |> 
  filter(projecthabitat == "MCR-ocean") |> 
  mutate(group = subsite_level1,
         color = subsite_level1,
         units = 'm2')

sbc_reef <- dat |> 
  filter(projecthabitat == "SBC-ocean") |> 
  mutate(group = site,
         color = strata,
         units = 'm2')

pisco_central <- dat |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "CENTRAL") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = subsite_level1,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-CENTRAL") #split pisco into central and southern

pisco_south <- dat |> 
  filter(projecthabitat == "CoastalCA-ocean",
         site == "SOUTH") |> #split pisco into central and southern
  mutate(group = subsite_level2,
         color = subsite_level1,
         units = 'm2',
         projecthabitat = "CoastalCA-ocean-SOUTH") #split pisco into central and southern

sbc_beach <- dat |> 
  filter(projecthabitat == "SBC-beach") |> 
  mutate(strata = case_when(
    site == "Isla_Vista" ~ "mpa",
    site == "East_Campus" ~ "reference",
    TRUE ~ NA_character_),
    group = site,
    color = strata,
    units = 'm') #noticed that Kyle had added mpa and reference strata, so brought in here with a case_when() statement

cce <- dat |> 
  filter(projecthabitat == "CCE-oceanic") |> 
  mutate(group = site,
         color = site,
         units = 'm2')

#Binding everything back together, removing index row generated when saving out of R
## and arranging the data by date
plotting_dat_ready <- bind_rows(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef) |> 
  dplyr::select(-...1) |> 
  arrange(sdate)

rm(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef)

# FIGURE TWO --------------------------------------------------------------
###########################################################################
# set up so I can go through and search and replace for each variable (i.e., 
## start with 'nitrogen', then for 'phosphorus', just find and replace) but
## will need to manually change columns I am summing by because not being read
## in as such (e.g., can't search and replace "total_n")

# NITROGEN PLOTS ----------------------------------------------------------

# Nitrogen Supply ~ Space + Time ------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_nitrogen = mean(total_n, na.rm = TRUE),
            sd_total_nitrogen = sd(total_n), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color))  |> 
  ggplot(aes(x = mean_total_nitrogen, y = sd_total_nitrogen, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Nitrogen Supply ~ Space + Time",
       x = 'Mean Annual Total Nitrogen Supply (ug/h/m_m2)',
       y = 'SD Annual Total Nitrogen Supply (ug/h/m_m2)') +
  facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_nitrogen_facet.png",
#   path = "plots/figure2/nitrogen/",
#   width = 15, height = 9
# )

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_nitrogen = mean(total_n, na.rm = TRUE),
            sd_total_nitrogen = sd(total_n), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color)) |>  #removes weird NA point in PISCO dataset
  filter(mean_total_nitrogen <= 50000,
         sd_total_nitrogen <= 50000) |> 
  ggplot(aes(x = mean_total_nitrogen, y = sd_total_nitrogen, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5)+
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2A: Nitrogen Supply ~ Space + Time",
       x = 'Mean Annual Total Nitrogen Supply (ug/h/m_m2)',
       y = 'SD Annual Total Nitrogen Supply (ug/h/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_nitrogen.png",
#   path = "plots/figure2/nitrogen/",
#   width = 15, height = 9
# )

# Nitrogen Supply ~ Space -------------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, color) |> 
  summarise(mean_total_nitrogen = mean(total_n, na.rm = TRUE),
            sd_total_nitrogen = sd(total_n), na.rm = TRUE) |> 
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  ungroup() |> 
  filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
  filter(mean_total_nitrogen <= 30000,
         sd_total_nitrogen <= 30000) |> 
  ggplot(aes(x = mean_total_nitrogen, y = sd_total_nitrogen, 
             color = projecthabitat, label = color, group = color)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.9, size = 4) +
  # geom_text_repel() +
  geom_text(vjust = -0.9, hjust = 0.9) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2B: Nitrogen Supply ~ Space",
       x = 'Mean Total Nitrogen Supply (ug/h/m_m2)',
       y = 'SD Total Nitrogen Supply (ug/h/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_nitrogen.png",
#   path = "plots/figure2/nitrogen/",
#   width = 15, height = 9
# )

# Phosphorus PLOTS ----------------------------------------------------------

# Phosphorus Supply ~ Space + Time ------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_phosphorus = mean(total_p, na.rm = TRUE),
            sd_total_phosphorus = sd(total_p), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color))  |> 
  ggplot(aes(x = mean_total_phosphorus, y = sd_total_phosphorus, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Phosphorus Supply ~ Space + Time",
       x = 'Mean Annual Total Phosphorus Supply (ug/h/m_m2)',
       y = 'SD Annual Total Phosphorus Supply (ug/h/m_m2)') +
  facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_phosphorus_facet.png",
#   path = "plots/figure2/phosphorus/",
#   width = 15, height = 9
# )

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_phosphorus = mean(total_p, na.rm = TRUE),
            sd_total_phosphorus = sd(total_p), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color)) |>  #removes weird NA point in PISCO dataset
  filter(mean_total_phosphorus <= 7500,
         sd_total_phosphorus <= 7500) |> 
  ggplot(aes(x = mean_total_phosphorus, y = sd_total_phosphorus, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5)+
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2A: Phosphorus Supply ~ Space + Time",
       x = 'Mean Annual Total Phosphorus Supply (ug/h/m_m2)',
       y = 'SD Annual Total Phosphorus Supply (ug/h/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_phosphorus.png",
#   path = "plots/figure2/phosphorus/",
#   width = 15, height = 9
# )

# phosphorus Supply ~ Space -------------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, color) |> 
  summarise(mean_total_phosphorus = mean(total_p, na.rm = TRUE),
            sd_total_phosphorus = sd(total_p), na.rm = TRUE) |> 
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  ungroup() |> 
  filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
  filter(mean_total_phosphorus <= 7500,
         sd_total_phosphorus <= 7500) |> 
  ggplot(aes(x = mean_total_phosphorus, y = sd_total_phosphorus, 
             color = projecthabitat, label = color, group = color)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.9, size = 4) +
  # geom_text_repel() +
  geom_text(vjust = -0.9, hjust = 0.9) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2B: Phosphorus Supply ~ Space",
       x = 'Mean Total Phosphorus Supply (ug/h/m_m2)',
       y = 'SD Total Phosphorus Supply (ug/h/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_phosphorus.png",
#   path = "plots/figure2/phosphorus/",
#   width = 15, height = 9
# )

# Biomass PLOTS ----------------------------------------------------------

#tried below code to fix NAs in CCE data, but did not work... Not certain where to go from here
## need to look at OG dataframe/talk to CCE + Li
plotting_dat_ready <- plotting_dat_ready %>%
  mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
         total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
         bm_sum = total_bm_m + total_bm_m2)

# Biomass Supply ~ Space + Time ------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_biomass = mean(bm_sum, na.rm = TRUE),
            sd_total_biomass = sd(bm_sum), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color))  |> 
  ggplot(aes(x = mean_total_biomass, y = sd_total_biomass, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Dry Biomass ~ Space + Time",
       x = 'Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'SD Annual Total Dry Biomass (g/m_m2)') +
  facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_biomass_facet.png",
#   path = "plots/figure2/biomass/",
#   width = 15, height = 9
# )

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_biomass = mean(bm_sum, na.rm = TRUE),
            sd_total_biomass = sd(bm_sum), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color)) |>  #removes weird NA point in PISCO dataset
  filter(mean_total_biomass <= 7500,
         sd_total_biomass <= 7500) |> 
  ggplot(aes(x = mean_total_biomass, y = sd_total_biomass, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5)+
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2A: Dry Biomass ~ Space + Time",
       x = 'Mean Annual Total Dry Biomass (g/m_m2)',
       y = 'SD Annual Total Dry Biomass (g/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_biomass.png",
#   path = "plots/figure2/biomass/",
#   width = 15, height = 9
# )

# biomass Supply ~ Space -------------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, color) |> 
  summarise(mean_total_biomass = mean(bm_sum, na.rm = TRUE),
            sd_total_biomass = sd(bm_sum), na.rm = TRUE) |> 
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  ungroup() |> 
  filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
  filter(mean_total_biomass <= 100,
         sd_total_biomass <= 100) |> 
  ggplot(aes(x = mean_total_biomass, y = sd_total_biomass, 
             color = projecthabitat, label = color, group = color)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  # geom_text(vjust = -0.9, hjust = 0.9) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2B: Dry Biomass ~ Space",
       x = 'Mean Total Dry Biomass (g/m_m2)',
       y = 'SD Total Dry Biomass (g/m_m2)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_biomass.png",
#   path = "plots/figure2/biomass/",
#   width = 15, height = 9
# )

# Size Structure PLOTS ----------------------------------------------------------

# Size Structure ~ Space + Time ------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_sizestructure = mean(bm_ind_mean_nozeros, na.rm = TRUE),
            sd_total_sizestructure = sd(bm_ind_mean_nozeros), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color))  |> 
  ggplot(aes(x = mean_total_sizestructure, y = sd_total_sizestructure, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Size Structure ~ Space + Time",
       x = 'Mean Annual Individual Dry Mass (g)',
       y = 'SD Annual Individual Dry Mass (g)') +
  facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_sizestructure_facet.png",
#   path = "plots/figure2/sizestructure/",
#   width = 15, height = 9
# )

plotting_dat_ready |> 
  group_by(projecthabitat, year, color, group) |> 
  summarise(mean_total_sizestructure = mean(bm_ind_mean_nozeros, na.rm = TRUE),
            sd_total_sizestructure = sd(bm_ind_mean_nozeros), na.rm = TRUE) |>
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> 
  ungroup() |> 
  filter(!is.na(color)) |>  #removes weird NA point in PISCO dataset
  filter(mean_total_sizestructure <= 7500,
         sd_total_sizestructure <= 7500) |> 
  ggplot(aes(x = mean_total_sizestructure, y = sd_total_sizestructure, 
             color = ph_strata, group = group)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.5)+
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2A: Size Structure ~ Space + Time",
       x = 'Mean Annual Individual Dry Mass (g)',
       y = 'SD Annual Individual Dry Mass (g)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2a_sizestructure.png",
#   path = "plots/figure2/sizestructure/",
#   width = 15, height = 9
# )

# Size Structure ~ Space -------------------------------------------------

plotting_dat_ready |> 
  group_by(projecthabitat, color) |> 
  summarise(mean_total_sizestructure = mean(bm_ind_mean_nozeros, na.rm = TRUE),
            sd_total_sizestructure = sd(bm_ind_mean_nozeros), na.rm = TRUE) |> 
  unite(ph_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |>
  ungroup() |> 
  filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
  filter(mean_total_sizestructure <= 100,
         sd_total_sizestructure <= 100) |> 
  ggplot(aes(x = mean_total_sizestructure, y = sd_total_sizestructure, 
             color = projecthabitat, label = color, group = color)) +
  # geom_line(alpha = 0.6) + #makes lines more transparent
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  # geom_text(vjust = -0.9, hjust = 0.9) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(title = "Figure 2B: Size Structure ~ Space",
       x = 'Mean Total Individual Dry Mass (g)',
       y = 'SD Total Individual Dry Mass (g)') +
  # facet_wrap(~projecthabitat, scales = "free") +
  # scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "figure2b_sizestructure.png",
#   path = "plots/figure2/sizestructure/",
#   width = 15, height = 9
# )
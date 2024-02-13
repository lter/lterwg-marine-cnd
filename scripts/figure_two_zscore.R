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
df_summarized_strata <- left_join(df_summarized, strata, by = c("project", "habitat", "site"))

# write.csv(df_summarized_strata, "data/summarized_lter_exc_clean.csv") #this is copy of summarized dataset with strata information

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

rm(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef,
   strata, df_summarized)

###########################################################################
# Plotting for Figure Two -------------------------------------------------
###########################################################################

# N Supply ~ Space + Time -------------------------------------------------

plotting_dat_ready |> 
  filter(!is.na(color)) |> #remove weird NAs from PISCO South (only a few)
  # group_by(projecthabitat, color) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  # ungroup() |>
  group_by(project, projecthabitat, color, year) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(total_n_z), na.rm = TRUE,
            sd_total_nitrogen_z = sd(total_n_z), na.rm = TRUE) |> 
  ungroup() |> 
  unite(p_strata, c(project, color), sep = "-", remove = FALSE) |> #only needed for all together figure
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |> 
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = p_strata, label = year)) + #change label to only year for facet_wrapped plot
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  # geom_smooth(method = 'rlm', se = FALSE) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2B: Z-Scored Nitrogen Supply ~ Space + Time",
       x = 'Mean Annual Nitrogen Supply (ug/h/m_m2)',
       y = 'SD Annual Nitrogen Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# N Supply ~ Time --------------------------------------------------------

plotting_dat_ready |> 
  filter(!is.na(color)) |> #remove weird NAs from PISCO South (only a few)
  # group_by(projecthabitat, color) |> #grouping by specific project and strata of interest to generate z-score calculations
  mutate(across(where(is.numeric), ~ (.-mean(.)) / sd(.), .names = "{.col}_z")) |> #z-scores all numeric data and generates new column for the zscored variable
  # ungroup() |>
  group_by(project, projecthabitat, year) |> #keeping project for plotting
  summarise(mean_total_nitrogen_z = mean(total_n_z), na.rm = TRUE,
            sd_total_nitrogen_z = sd(total_n_z), na.rm = TRUE) |> 
  ungroup() |> 
  unite(p_year, c(project, year), sep = "-", remove = FALSE) |> #only needed for all together figure
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) |> 
  ggplot(aes(x = mean_total_nitrogen_z, y = sd_total_nitrogen_z, 
             color = project, label = year)) + #change label to only year for facet_wrapped plot
  geom_point(alpha = 0.9, size = 4) +
  geom_text_repel() +
  # geom_smooth(method = 'rlm', se = FALSE) +
  geom_smooth(aes(group = 1), method = "rlm", se = FALSE, color = "black") +
  theme_classic() +
  labs(title = "Figure 2B: Z-Scored Nitrogen Supply ~ Time",
       x = 'Mean Annual Nitrogen Supply (ug/h/m_m2)',
       y = 'SD Annual Nitrogen Supply (ug/h/m_m2)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# Individual Site N Supply ~ Space + Time ---------------------------------



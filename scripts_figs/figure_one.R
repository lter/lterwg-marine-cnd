###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
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
###########################################################################
# NITROGEN FIGURE ONE -----------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# areal nitrogen supply ~ space and time with strata smoother -------------
###########################################################################

nitrogen_supply_smoothed <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    ungroup() |>  
    ### code below sets stage to map() plot across all sites
    filter(projecthabitat == f) |> 
    ggplot(aes(x = sdate, y = mean_total_n, group = group)) +
    geom_line(alpha = 0.4, color = 'black') +
    ### separates the aesthetics of the smoothing term from the actual sites being plotted
    ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
    geom_smooth(aes(color = color, group = color, fill = color), 
                method = "loess", span = 0.2, se = FALSE, linewidth = 3.0, alpha = 0.2) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Areal Nitrogen Supply (ug/hr/m_m2_m3)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  # legend.position = "none")
}

nitrogen_supply_smoothed_fig1 <- map(unique(dt$projecthabitat), nitrogen_supply_smoothed)

# ggsave(
#   filename = "nitrogen_supply_smoothed_fig1.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nitrogen_supply_smoothed_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
# areal nitrogen supply ~ space and time with strata boxplot --------------
###########################################################################

nitrogen_supply_boxplot <- function(f) {
  dt |> 
    group_by(projecthabitat, units, year, color, group) |>  # Group by site and year now, not sdate
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    filter(projecthabitat == f) |> 
    ggplot(aes(x = factor(year), y = mean_total_n, fill = color)) +  # X-axis is now the year
    geom_boxplot(aes(group = interaction(year, color)), alpha = 0.6) +  # Grouping within each year by site
    theme_classic() +
    labs(title = f,
         x = 'Year',
         y = 'Mean Areal Nitrogen Supply (ug/hr/m_m2_m3)') +
    scale_fill_brewer(palette = "Set1") +  # Optional: Improves color distinction
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
nitrogen_supply_boxplot_fig1 <- map(unique(dt$projecthabitat), nitrogen_supply_boxplot)

# ggsave(
#   filename = "nitrogen_supply_boxplot_fig1.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nitrogen_supply_boxplot_fig1 , nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
###########################################################################
# PHOSPHORUS FIGURE ONE ---------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# areal phosphorus supply ~ space and time with strata smoother -----------
###########################################################################

phosphorus_supply_smoothed <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_p = mean(total_p, na.rm = TRUE)) |> 
    ungroup() |>  
    ### code below sets stage to map() plot across all sites
    filter(projecthabitat == f) |> 
    ggplot(aes(x = sdate, y = mean_total_p, group = group)) +
    geom_line(alpha = 0.4, color = 'black') +
    ### separates the aesthetics of the smoothing term from the actual sites being plotted
    ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
    geom_smooth(aes(color = color, group = color, fill = color), 
                method = "loess", span = 0.2, se = FALSE, linewidth = 3.0, alpha = 0.2) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Areal Phosphorus Supply (ug/hr/m_m2_m3)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  # legend.position = "none")
}

phosphorus_supply_smoothed_fig1 <- map(unique(dt$projecthabitat), phosphorus_supply_smoothed)

# ggsave(
#   filename = "phosphorus_supply_smoothed_fig1.pdf",
#   path = "plots/figure1/phosphorus/",
#   plot = marrangeGrob(phosphorus_supply_smoothed_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
# areal phosphorus supply ~ space and time with strata boxplot ------------
###########################################################################

phosphorus_supply_boxplot <- function(f) {
  dt |> 
    group_by(projecthabitat, units, year, color, group) |>  # Group by site and year now, not sdate
    summarise(mean_total_p = mean(total_p, na.rm = TRUE)) |> 
    filter(projecthabitat == f) |> 
    ggplot(aes(x = factor(year), y = mean_total_p, fill = color)) +  # X-axis is now the year
    geom_boxplot(aes(group = interaction(year, color)), alpha = 0.6) +  # Grouping within each year by site
    theme_classic() +
    labs(title = f,
         x = 'Year',
         y = 'Mean Areal Phosphorus Supply (ug/hr/m_m2_m3)') +
    scale_fill_brewer(palette = "Set1") +  # Optional: Improves color distinction
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
phosphorus_supply_boxplot_fig1 <- map(unique(dt$projecthabitat), phosphorus_supply_boxplot)

# ggsave(
#   filename = "phosphorus_supply_boxplot_fig1.pdf",
#   path = "plots/figure1/phosphorus/",
#   plot = marrangeGrob(phosphorus_supply_boxplot_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )


###########################################################################
###########################################################################
# BIOMASS FIGURE ONE ------------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# total biomass ~ space and time with strata smoother ---------------------
###########################################################################

total_biomass_smoothed <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_bm = mean(total_bm, na.rm = TRUE)) |> 
    ungroup() |>  
    ### code below sets stage to map() plot across all sites
    filter(projecthabitat == f) |> 
    ggplot(aes(x = sdate, y = mean_total_bm, group = group)) +
    geom_line(alpha = 0.4, color = 'black') +
    ### separates the aesthetics of the smoothing term from the actual sites being plotted
    ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
    geom_smooth(aes(color = color, group = color, fill = color), 
                method = "loess", span = 0.2, se = FALSE, linewidth = 3.0, alpha = 0.2) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Total Dry Biomass (g/m_m2_m3)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  # legend.position = "none")
}

total_biomass_smoothed_fig1 <- map(unique(dt$projecthabitat), total_biomass_smoothed)

# ggsave(
#   filename = "total_biomass_smoothed_fig1.pdf",
#   path = "plots/figure1/biomass/",
#   plot = marrangeGrob(total_biomass_smoothed_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
# total biomass ~ space and time with strata boxplot ----------------------
###########################################################################

total_biomass_boxplot <- function(f) {
  dt |> 
    group_by(projecthabitat, units, year, color, group) |>  # Group by site and year now, not sdate
    summarise(mean_total_bm = mean(total_bm, na.rm = TRUE)) |> 
    filter(projecthabitat == f) |> 
    ggplot(aes(x = factor(year), y = mean_total_bm, fill = color)) +  # X-axis is now the year
    geom_boxplot(aes(group = interaction(year, color)), alpha = 0.6) +  # Grouping within each year by site
    theme_classic() +
    labs(title = f,
         x = 'Year',
         y = 'Total Dry Biomass (g/m_m2_m3)') +
    scale_fill_brewer(palette = "Set1") +  # Optional: Improves color distinction
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
total_biomass_boxplot_fig1 <- map(unique(dt$projecthabitat), total_biomass_boxplot)

# ggsave(
#   filename = "total_biomass_boxplot_fig1.pdf",
#   path = "plots/figure1/biomass/",
#   plot = marrangeGrob(total_biomass_boxplot_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9)

###########################################################################
###########################################################################
# SIZE STRUCTURE FIGURE ONE -----------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# size structure ~ space and time with strata smoother --------------------
###########################################################################

dt_size_structure <- dt |> 
  filter(!(projecthabitat == "SBC-ocean" & mean_max_community_size > 70))

size_structure_smoothed <- function(f) {
  dt_size_structure |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(AvgSppMaxDryMass = mean(mean_max_community_size, na.rm = TRUE)) |> 
    ungroup() |>  
    ### code below sets stage to map() plot across all sites
    filter(projecthabitat == f) |> 
    ggplot(aes(x = sdate, y = AvgSppMaxDryMass, group = group)) +
    geom_line(alpha = 0.4, color = 'black') +
    ### separates the aesthetics of the smoothing term from the actual sites being plotted
    ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
    geom_smooth(aes(color = color, group = color, fill = color), 
                method = "loess", span = 0.2, se = FALSE, linewidth = 3.0, alpha = 0.2) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Max Size of Individual Species in Community Average (g)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  # legend.position = "none")
}

size_structure_smoothed_fig1 <- map(unique(dt_size_structure$projecthabitat), size_structure_smoothed)

# ggsave(
#   filename = "size_structure_smoothed_fig1.pdf",
#   path = "plots/figure1/size structure/",
#   plot = marrangeGrob(size_structure_smoothed_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
# size structure ~ space and time with strata boxplot ---------------------
###########################################################################

size_structure_boxplot <- function(f) {
  dt_size_structure |> 
    group_by(projecthabitat, units, year, color, group) |>  # Group by site and year now, not sdate
    summarise(AvgSppMaxDryMass = mean(mean_max_community_size, na.rm = TRUE)) |> 
    filter(projecthabitat == f) |> 
    ggplot(aes(x = factor(year), y = AvgSppMaxDryMass, fill = color)) +  # X-axis is now the year
    geom_boxplot(aes(group = interaction(year, color)), alpha = 0.6) +  # Grouping within each year by site
    theme_classic() +
    labs(title = f,
         x = 'Year',
         y = 'Max Size of Individual Species in Community Average (g)') +
    scale_fill_brewer(palette = "Set1") +  # Optional: Improves color distinction
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
size_structure_boxplot_fig1 <- map(unique(dt_size_structure$projecthabitat), size_structure_boxplot)

# ggsave(
#   filename = "size_structure_boxplot_fig1.pdf",
#   path = "plots/figure1/size structure/",
#   plot = marrangeGrob(size_structure_boxplot_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9)

###########################################################################
###########################################################################
# DIET PROPORTIONS FIGURE ONE ---------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# diet proportions ~ space and time stacked bar chart ---------------------
###########################################################################

dt_diet_prop <- dt |> 
  dplyr::select(projecthabitat, year, color, n_spp, prop_algae_detritus, prop_invert, 
         prop_algae_invert, prop_fish_invert, prop_fish) |> 
  pivot_longer(cols = starts_with("prop_"), names_to = "diet", values_to = "proportion") |> 
  mutate(diet = str_replace(diet, "prop_", "")) |> 
  distinct()

diet_prop_stacked_bar <- function(f) {
  dt_diet_prop |> 
    group_by(projecthabitat, diet, year, color) |>
    summarise(avg_prop = mean(proportion, na.rm = TRUE)) |>
    filter(projecthabitat == f) |> 
    ggplot(aes(x = year, y = avg_prop, fill = diet)) +
    geom_bar(position = "fill", stat = "identity") +
    theme_classic() +
    labs(title = f,
         x = 'Year',
         y = 'Proportion of Community Diet') +
    scale_fill_brewer(palette = "Set1") +  # Optional: Improves color distinction
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

diet_prop_stacked_bar_fig1 <- map(unique(dt_diet_prop$projecthabitat), diet_prop_stacked_bar)

# ggsave(
#   filename = "diet_prop_stacked_bar_fig1.pdf",
#   path = "plots/figure1/",
#   plot = marrangeGrob(diet_prop_stacked_bar_fig1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
###########################################################################
# HISTOGRAMS FIGURE ONE ---------------------------------------------------
###########################################################################
###########################################################################

###########################################################################
# nitrogen: raw & log-transformed -----------------------------------------
###########################################################################

### raw nitrogen histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = total_n, fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Areal Nitrogen Supply (ug/hr/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "raw_total_nitrogen_freq.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )

### log-transformed nitrogen histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = (log(total_n)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Log-Transformed Areal Nitrogen Supply (ug/hr/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "log_total_nitrogen_freq.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )

###########################################################################
# phosphorus: raw & log-transformed ---------------------------------------
###########################################################################

### raw phosphorus histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = total_p, fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Areal Phosphorus Supply (ug/hr/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "raw_total_phosphorus_freq.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )

### log-transformed phosphorus histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = (log(total_n)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) + 
  labs(x = "Log-Transformed Areal Phosphorus Supply (ug/hr/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "log_total_phosphorus_freq.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )


###########################################################################
# total biomass: raw & log-transformed ---------------------------------------
###########################################################################

### raw total biomass histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = total_bm, fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Total Dry Biomass (g/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "raw_total_biomass_freq.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )

### log-transformed total biomass histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = (log(total_bm)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) + 
  labs(x = "Log-Transformed Total Dry Biomass (g/m_m2_m3)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "log_total_biomass_freq.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )

###########################################################################
# size structure: raw & log-transformed ---------------------------------------
###########################################################################

### raw size structure histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = mean_max_community_size, fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  
  labs(x = "Max Size of Individual Species in Community Average (g)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "raw_size_structure_freq.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )

### log-transformed size structure histogram
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  ggplot(aes(x = (log(mean_max_community_size)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) + 
  labs(x = "Max Size of Individual Species in Community Average (g)",
       y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ggsave(
#   filename = "log_size_structure_freq.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )
# 
# ###########################################################################
# ###########################################################################
# # HISTOGRAMS FIGURE ONE: FCE, MCR, & PISCO South w/ annual data
# ###########################################################################
# ###########################################################################
# 
# ###########################################################################
# # nitrogen: raw & log-transformed -----------------------------------------
# ###########################################################################
# 
# ### raw nitrogen histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'CoastalCA-ocean-SOUTH')) |> 
#   group_by(projecthabitat, year) |> 
#   mutate(avg_n = mean(total_n)) |> 
#   ggplot(aes(x = avg_n, fill = projecthabitat)) +
#   geom_density(alpha = 0.5) +  
#   labs(x = "Areal Nitrogen Supply (ug/hr/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "raw_total_nitrogen_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )
# 
# ### log-transformed nitrogen histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = (log(total_n)), fill = projecthabitat)) +
#   geom_density(alpha = 0.5) +  
#   labs(x = "Log-Transformed Areal Nitrogen Supply (ug/hr/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         # legend.position = "none",
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "log_total_nitrogen_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )
# 
# ###########################################################################
# # phosphorus: raw & log-transformed ---------------------------------------
# ###########################################################################
# 
# ### raw phosphorus histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = total_p, fill = projecthabitat)) +
#   geom_density(alpha = 0.5) +  
#   labs(x = "Areal Phosphorus Supply (ug/hr/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "raw_total_phosphorus_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )
# 
# ### log-transformed phosphorus histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = (log(total_n)), fill = projecthabitat)) +
#   geom_density(alpha = 0.5) + 
#   labs(x = "Log-Transformed Areal Phosphorus Supply (ug/hr/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "log_total_phosphorus_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )
# 
# ###########################################################################
# # total biomass: raw & log-transformed ---------------------------------------
# ###########################################################################
# 
# ### raw total biomass histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = total_bm, fill = projecthabitat)) +
#   geom_density(alpha = 0.5) +  
#   labs(x = "Total Dry Biomass (g/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "raw_total_biomass_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )
# 
# ### log-transformed total biomass histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = (log(total_bm)), fill = projecthabitat)) +
#   geom_density(alpha = 0.5) + 
#   labs(x = "Log-Transformed Total Dry Biomass (g/m_m2_m3)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "log_total_biomass_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )
# 
# ###########################################################################
# # size structure: raw & log-transformed ---------------------------------------
# ###########################################################################
# 
# ### raw size structure histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = mean_max_community_size, fill = projecthabitat)) +
#   geom_density(alpha = 0.5) +  
#   labs(x = "Max Size of Individual Species in Community Average (g)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "raw_size_structure_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/raw/",
#   width = 15, height = 9
# )
# 
# ### log-transformed size structure histogram
# dt |> 
#   filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
#   ggplot(aes(x = (log(mean_max_community_size)), fill = projecthabitat)) +
#   geom_density(alpha = 0.5) + 
#   labs(x = "Max Size of Individual Species in Community Average (g)",
#        y = "Frequency") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.background = element_rect(fill = "white"),
#         axis.line = element_line("black"),
#         axis.text = element_text(face = "bold"),
#         axis.title = element_text(face = "bold"))
# 
# ggsave(
#   filename = "log_size_structure_freq_fce_mcr_piscosouth.png",
#   path = "plots/figure1/histogram/log_transformed/",
#   width = 15, height = 9
# )
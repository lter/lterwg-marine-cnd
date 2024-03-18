###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel, purrr, ggtext)

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

habitat_mapping <- dt |> 
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean"))
habitat_mapping <- data.frame(
  color = unique(habitat_mapping$color),
  Habitat = c("Riverine", "Bay", "Back Reef", "Fore Reef", "Fringing Reef",
                 'Reference', 'Marine Protected Area')) 

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

###########################################################################
# NITROGEN --------------------------------------------------------------
###########################################################################

dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  # mutate(Project = factor(Project, levels = c("Bay", "Riverine", "Back Reef", "Fore Reef",
  #                                             "Fringing Reef", "Marine Protected Area", "Reference"))) |> 
  group_by(Project, units, sdate, year, month, Habitat, group) |> 
  summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
  ungroup() |>  
  ### code below sets stage to map() plot across all sites
  # filter(projecthabitat == f) |> 
  ggplot(aes(x = sdate, y = mean_total_n, group = group)) +
  geom_line(alpha = 0.4, color = 'black') +
  ### separates the aesthetics of the smoothing term from the actual sites being plotted
  ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
  geom_smooth(aes(color = Habitat, group = Habitat, fill = Habitat), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 2.5, alpha = 0.2) +
  theme_classic() +
  facet_wrap(~Project, scales = "free") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold", colour = "black", size = 24)) +
  labs(x = 'Date',
       y = 'Nitrogen Supply') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 24),
        axis.title = element_text(face = "bold", size = 14),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "figure1_nsupply.tiff",
  path = "plots/",
  width = 21, height = 7
)


###########################################################################
# PHOSPHORUS --------------------------------------------------------------
###########################################################################


dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  # mutate(Project = factor(Project, levels = c("Bay", "Riverine", "Back Reef", "Fore Reef",
  #                                             "Fringing Reef", "Marine Protected Area", "Reference"))) |> 
  group_by(Project, units, sdate, year, month, Habitat, group) |> 
  summarise(mean_total_p = mean(total_p, na.rm = TRUE)) |> 
  ungroup() |>  
  ### code below sets stage to map() plot across all sites
  # filter(projecthabitat == f) |> 
  ggplot(aes(x = sdate, y = mean_total_p, group = group)) +
  geom_line(alpha = 0.4, color = 'black') +
  ### separates the aesthetics of the smoothing term from the actual sites being plotted
  ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
  geom_smooth(aes(color = Habitat, group = Habitat, fill = Habitat), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 2.5, alpha = 0.2) +
  theme_classic() +
  facet_wrap(~Project, scales = "free") +
  theme(strip.text = element_blank()) +
  labs(x = 'Date',
       y = 'Phosphorus Supply') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.title.y = element_text(face = "bold", size = 24),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 14),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "figure1_psupply.tiff",
  path = "plots/",
  width = 21, height = 7
)


###########################################################################
# BIOMASS --------------------------------------------------------------
###########################################################################


dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  # mutate(Project = factor(Project, levels = c("Bay", "Riverine", "Back Reef", "Fore Reef",
  #                                             "Fringing Reef", "Marine Protected Area", "Reference"))) |> 
  group_by(Project, units, sdate, year, month, Habitat, group) |> 
  summarise(mean_total_bm = mean(total_bm, na.rm = TRUE)) |> 
  ungroup() |>  
  ### code below sets stage to map() plot across all sites
  # filter(projecthabitat == f) |> 
  ggplot(aes(x = sdate, y = mean_total_bm, group = group)) +
  geom_line(alpha = 0.4, color = 'black') +
  ### separates the aesthetics of the smoothing term from the actual sites being plotted
  ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
  geom_smooth(aes(color = Habitat, group = Habitat, fill = Habitat), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 2.5, alpha = 0.2) +
  theme_classic() +
  facet_wrap(~Project, scales = "free") +
  theme(strip.text = element_blank()) +
  labs(x = 'Date',
       y = 'Total Dry Biomass') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.title.y = element_text(face = "bold", size = 24),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 14),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "figure1_bm.tiff",
  path = "plots/",
  width = 21, height = 7
)

###########################################################################
# SIZE STRUCTURE --------------------------------------------------------------
###########################################################################


dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c("FCE", "MCR", "SBC-Ocean")) |> 
  filter(!(Project == "SBC-Ocean" & mean_max_community_size > 200)) |> 
  # mutate(Project = factor(Project, levels = c("Bay", "Riverine", "Back Reef", "Fore Reef",
  #                                             "Fringing Reef", "Marine Protected Area", "Reference"))) |> 
  group_by(Project, units, sdate, year, month, Habitat, group) |> 
  summarise(mean_total_ss = mean(mean_max_community_size, na.rm = TRUE)) |> 
  ungroup() |>  
  ### code below sets stage to map() plot across all sites
  # filter(projecthabitat == f) |> 
  ggplot(aes(x = sdate, y = mean_total_ss, group = group)) +
  geom_line(alpha = 0.4, color = 'black') +
  ### separates the aesthetics of the smoothing term from the actual sites being plotted
  ### small number of data points for NGA results in loss of smoothers where span is less than 0.7
  geom_smooth(aes(color = Habitat, group = Habitat, fill = Habitat), 
              method = "loess", span = 0.2, se = FALSE, linewidth = 2.5, alpha = 0.2) +
  theme_classic() +
  facet_wrap(~Project, scales = "free") +
  theme(strip.text = element_blank()) +
  labs(x = 'Date',
       y = 'Size Structure') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.title.y = element_text(face = "bold", size = 24),
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 14),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "figure1_ss.tiff",
  path = "plots/",
  width = 21, height = 7
)


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
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = total_n, fill = Project)) +
  geom_density(alpha = 0.5) +  
  # labs(x = "Areal Nitrogen Supply (ug/hr/m_m2_m3)",
  #      y = "Frequency") +
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "raw_total_nitrogen_freq.tiff",
  path = "plots/figure1/histogram/raw/",
  width = 7, height = 7
)

### log-transformed nitrogen histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = log(total_n), fill = Project)) +
  geom_density(alpha = 0.5) +  
  # labs(x = "Areal Nitrogen Supply (ug/hr/m_m2_m3)",
  #      y = "Frequency") +
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "log_total_nitrogen_freq.tiff",
  path = "plots/figure1/histogram/log_transformed/",
  width = 7, height = 7
)

###########################################################################
# phosphorus: raw & log-transformed ---------------------------------------
###########################################################################

### raw histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = total_p, fill = Project)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "raw_total_phosphorus_freq.tiff",
  path = "plots/figure1/histogram/raw/",
  width = 7, height = 7
)

### log-transformed histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = log(total_p), fill = Project)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "log_total_phosphorus_freq.tiff",
  path = "plots/figure1/histogram/log_transformed/",
  width = 7, height = 7
)

###########################################################################
# total biomass: raw & log-transformed ---------------------------------------
###########################################################################

### raw histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = total_bm, fill = Project)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "raw_total_bm_freq.tiff",
  path = "plots/figure1/histogram/raw/",
  width = 7, height = 7
)

### log-transformed histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  ggplot(aes(x = log(total_bm), fill = Project)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "log_total_bm_freq.tiff",
  path = "plots/figure1/histogram/log_transformed/",
  width = 7, height = 7
)

###########################################################################
# size structure: raw & log-transformed ---------------------------------------
###########################################################################
### raw histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |> 
  filter(!(Project == "SBC-Ocean" & mean_max_community_size > 200)) |> 
  ggplot(aes(x = mean_max_community_size, fill = Project)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "raw_ss_freq.tiff",
  path = "plots/figure1/histogram/raw/",
  width = 7, height = 7
)

### log-transformed histogram
dt |> 
  left_join(label_mapping, by = "projecthabitat") |> 
  left_join(habitat_mapping, by = "color") |> 
  filter(Project %in% c('FCE', 'MCR', 'SBC-Ocean')) |>
  filter(!(Project == "SBC-Ocean" & mean_max_community_size > 200)) |> 
  ggplot(aes(x = log(mean_max_community_size), fill = Project)) +
  geom_density(alpha = 0.5) +  
  scale_fill_manual(values = cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(face = "bold", size = 14))

ggsave(
  filename = "log_ss_freq.tiff",
  path = "plots/figure1/histogram/log_transformed/",
  width = 7, height = 7
)

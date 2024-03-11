###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): generate figure one for manuscript
###date(s): January - March 2024
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel)

### set google drive path
exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) |> 
  dplyr::filter(name %in% c("harmonized_consumer_excretion_CLEAN_summarized.csv"))

rm(list = ls()) #cleans env

### read in data from google drive
dt <- read.csv(file.path("tier2", "harmonized_consumer_excretion_CLEAN_summarized.csv"),stringsAsFactors = F,na.strings =".") |> 
  mutate(sdate = as.Date(sdate))
glimpse(dt)

# nitrogen supply ~ project sites + time --------------------------------------------------

nsupply_sitelevel <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_n, color = color, group = group)) +
    geom_line() +
    geom_point() +
    # geom_smooth(method = "loess", se = TRUE) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Nitrogen Supply (ug/hr/m_m2)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

nsupply_figure1 <- map(unique(dt$projecthabitat), nsupply_sitelevel)

ggsave(
  filename = "nsupply_sitelevel_seperate.pdf",
  path = "plots/figure1/nitrogen/",
  plot = marrangeGrob(nsupply_figure1, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# N Supply ~ Time + Site + Strata (smoothed) ------------------------------

nsupply_sitelevel_2 <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_n, color = group, group = group)) +
    geom_line(alpha = 0.6) + #makes lines more transparent
    # geom_point() +
    geom_smooth(aes(linetype = color, group = color), 
                method = "loess", span = 0.9, se = TRUE, color = "black") + #separates the aesthetics of the smoothing term from the actual sites being plotted
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Nitrogen Supply (ug/hr/m_m2)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none")
}

nsupply_figure2 <- map(unique(dt$projecthabitat), nsupply_sitelevel_2)

ggsave(
  filename = "nsupply_sitelevel2_seperate_02162024_mcrsplit.pdf",
  path = "plots/figure1/nitrogen/",
  plot = marrangeGrob(nsupply_figure2, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# N Supply ~ Time + Strata (smoothed) -------------------------------------

nsupply_strata_smoothed <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color) |> 
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_n, color = color, group = color)) +
    # geom_line() +
    geom_point(size = 3) +
    geom_smooth(method = "loess", span = 0.9, se = TRUE) +
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Nitrogen Supply (ug/hr/m_m2)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

nsupply_figure3 <- map(unique(dt$projecthabitat), nsupply_strata_smoothed)

ggsave(
  filename = "nsupply_strata_smoothed_seperate_02162024.pdf",
  path = "plots/figure1/nitrogen/",
  plot = marrangeGrob(nsupply_figure3, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# N Supply ~ Time + Strata (stacked bar chart) ----------------------------

nsupply_annual_stacked <- function(f) {
  dt |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_total_n = mean(total_n, na.rm = TRUE)) |> 
    ungroup() |> 
    filter(!is.na(color)) |>  #removes weird NA from PISCO 
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(fill = color, x = as.factor(year), 
               y = as.numeric(mean_total_n))) + #makes year a factor, important for showing each year on x axis
    geom_bar(position = "stack", stat = "identity") +
    labs(title = f, 
         x = "Year", 
         y = "Mean Annual Nitrogen Supply (ug/hr/m_m2)") +
    scale_x_discrete(name = "Year") + #plots each year on x axis for ease of interpretation
    theme(panel.grid.major = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
          panel.grid.minor = element_blank(),legend.position = "top")
}

nsupply_figure4 <- map(unique(dt$projecthabitat), nsupply_annual_stacked)

ggsave(
  filename = "nsupply_annual_stacked_seperate_02162024.pdf",
  path = "plots/figure1/nitrogen/",
  plot = marrangeGrob(nsupply_figure4, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# PHOSPHORUS PLOTS --------------------------------------------------------
# Will only be plotting versions 2 and 4 of figures above, because I believe
## those are the most informative - true for remainder - P, Biomass, & Size Structure

# P Supply ~ Time + Site + Strata (smoothed) ------------------------------

psupply_sitelevel_2 <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_p = mean(total_p, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_p, color = group, group = group)) +
    geom_line(alpha = 0.6) + #makes lines more transparent
    # geom_point() +
    geom_smooth(aes(linetype = color, group = color), 
                method = "loess", span = 0.9, se = TRUE, color = "black") + #separates the aesthetics of the smoothing term from the actual sites being plotted
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Phosphorus Supply (ug/hr/m_m2)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none")
}

psupply_figure2 <- map(unique(dt$projecthabitat), psupply_sitelevel_2)

ggsave(
  filename = "psupply_sitelevel2_seperate_02162024.pdf",
  path = "plots/figure1/phosphorus/",
  plot = marrangeGrob(psupply_figure2, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# P Supply ~ Time + Strata (stacked bar chart) ----------------------------

psupply_annual_stacked <- function(f) {
  dt |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_total_p = mean(total_p, na.rm = TRUE)) |> 
    ungroup() |> 
    filter(!is.na(color)) |>  #removes weird NA from PISCO 
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(fill = color, x = as.factor(year), 
               y = as.numeric(mean_total_p))) + #makes year a factor, important for showing each year on x axis
    geom_bar(position = "stack", stat = "identity") +
    labs(title = f, 
         x = "Year", 
         y = "Mean Annual Phosphorus Supply (ug/hr/m_m2)") +
    scale_x_discrete(name = "Year") + #plots each year on x axis for ease of interpretation
    theme(panel.grid.major = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
          panel.grid.minor = element_blank(),legend.position = "top")
}

psupply_figure4 <- map(unique(dt$projecthabitat), psupply_annual_stacked)

ggsave(
  filename = "psupply_annual_stacked_seperate_02162024.pdf",
  path = "plots/figure1/phosphorus/",
  plot = marrangeGrob(psupply_figure4, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# BIOMASS PLOTS -----------------------------------------------------------

glimpse(dt)
#forgot that initial shot at summing these rows did not work and resulted in all NAs... 
##so going to use the rowSums function to "treat" NAs as zeros, 
##initially summed calculated two columns based on whether the sampling unit was in
##m or m2, then summed across with idea that summing would functionally remove
##the units from the equation and result in a single column for analysis/plotting
## for example, FCE units are 'm' - so if I summed total_bm_m and total_bm_m2, the
## column bm_sum should just be the biomass bc total_bm_m2 is completely blank
## for the FCE

#tried below code to fix NAs in CCE data, but did not work... Not certain where to go from here
## need to look at OG dataframe/talk to CCE + Li
# dt <- dt %>%
#   mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
#          total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
#          bm_sum = total_bm_m + total_bm_m2)

# BM ~ Time + Site + Strata (smoothed) ------------------------------------

bmsupply_sitelevel_2 <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_bm = mean(total_bm, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_bm, color = group, group = group)) +
    geom_line(alpha = 0.6) + #makes lines more transparent
    # geom_point() +
    geom_smooth(aes(linetype = color, group = color), 
                method = "loess", span = 0.9, se = TRUE, color = "black") + #separates the aesthetics of the smoothing term from the actual sites being plotted
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Dry Mass (g/m_m2)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none")
}

bmsupply_figure2 <- map(unique(dt$projecthabitat), bmsupply_sitelevel_2)

ggsave(
  filename = "bm_sitelevel2_seperate_02162024.pdf",
  path = "plots/figure1/biomass/",
  plot = marrangeGrob(bmsupply_figure2, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# BM ~ Time + Strata (stacked bar chart) ---------------------------------

bmsupply_annual_stacked <- function(f) {
  dt |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_total_bm = mean(total_bm, na.rm = TRUE)) |> 
    ungroup() |> 
    filter(!is.na(color)) |>  #removes weird NA from PISCO 
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(fill = color, x = as.factor(year), 
               y = as.numeric(mean_total_bm))) + #makes year a factor, important for showing each year on x axis
    geom_bar(position = "stack", stat = "identity") +
    labs(title = f, 
         x = "Year", 
         y = "Mean Annual Dry Mass (g/m_m2)") +
    scale_x_discrete(name = "Year") + #plots each year on x axis for ease of interpretation
    theme(panel.grid.major = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
          panel.grid.minor = element_blank(),legend.position = "top")
}

bmsupply_figure4 <- map(unique(dt$projecthabitat), bmsupply_annual_stacked)

ggsave(
  filename = "bm_annual_stacked_seperate_02162024.pdf",
  path = "plots/figure1/biomass/",
  plot = marrangeGrob(bmsupply_figure4, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# Size Structure Plots ----------------------------------------------------

# BM ~ Time + Site + Strata (smoothed) ------------------------------------

bmindsupply_sitelevel_2 <- function(f) {
  dt |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_MaxWeight = mean(mean_max_community_size, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_MaxWeight, color = group, group = group)) +
    geom_line(alpha = 0.6) + #makes lines more transparent
    # geom_point() +
    geom_smooth(aes(linetype = color, group = color), 
                method = "loess", span = 0.9, se = TRUE, color = "black") + #separates the aesthetics of the smoothing term from the actual sites being plotted
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Max Species Dry Mass in Community (g)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none")
}

bmindsupply_figure2 <- map(unique(dt$projecthabitat), bmindsupply_sitelevel_2)

ggsave(
  filename = "ind_bm_sitelevel2_seperate_02162024.pdf",
  path = "plots/figure1/size structure/",
  plot = marrangeGrob(bmindsupply_figure2, nrow = 1, ncol = 1),
  width = 15, height = 9
)

# BM ~ Time + Strata (stacked bar chart) ---------------------------------

bmindsupply_annual_stacked <- function(f) {
  dt |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_MaxWeight = mean(mean_max_community_size, na.rm = TRUE)) |> 
    ungroup() |> 
    filter(!is.na(color)) |>  #removes weird NA from PISCO 
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(fill = color, x = as.factor(year), 
               y = as.numeric(mean_MaxWeight))) + #makes year a factor, important for showing each year on x axis
    geom_bar(position = "stack", stat = "identity") +
    labs(title = f, 
         x = "Year", 
         y = "Mean Max Species Dry Mass in Community (g)") +
    scale_x_discrete(name = "Year") + #plots each year on x axis for ease of interpretation
    theme(panel.grid.major = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
          panel.grid.minor = element_blank(),legend.position = "top")
}

bmindsupply_figure4 <- map(unique(dt$projecthabitat), bmindsupply_annual_stacked)

ggsave(
  filename = "ind_bm_annual_stacked_seperate_02162024.pdf",
  path = "plots/figure1/size structure/",
  plot = marrangeGrob(bmindsupply_figure4, nrow = 1, ncol = 1),
  width = 15, height = 9
)


###########################################################################
# Figure One Histograms ---------------------------------------------------
###########################################################################

# log nitrogen ------------------------------------------------------------
dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  # mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
  #        total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
  #        bm_sum = total_bm_m + total_bm_m2) |> 
  ggplot(aes(x = (log(total_n)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  # Adjust alpha for fill transparency
  # scale_fill_manual(values = c("FCE" = "red", "MCR" = "green", "SBC" = "blue")) +
  labs(title = "Frequency of Total Nitrogen Supply ~ Site",
       x = "Log Nitrogen Supply (ug/hr/m_m2)",
       y = "Frequency") +
  theme_classic() +
  # theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(
  filename = "log_total_n_freq.png",
  path = "plots/figure1/histogram/",
  width = 15, height = 9
)

# log phosphorus ----------------------------------------------------------

dt |> 
  # filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  # mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
  #        total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
  #        bm_sum = total_bm_m + total_bm_m2) |> 
  ggplot(aes(x = (log(total_p)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  # Adjust alpha for fill transparency
  # scale_fill_manual(values = c("FCE" = "red", "MCR" = "green", "SBC" = "blue")) +
  labs(title = "Frequency of Total Phosphorus Supply ~ Site",
       x = "Log Phosphorus Supply (ug/hr/m_m2)",
       y = "Frequency") +
  theme_classic() +
  # theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(
  filename = "log_total_p_freq.png",
  path = "plots/figure1/histogram/",
  width = 15, height = 9
)

# log total biomass -------------------------------------------------------

dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  # mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
  #        total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
  #        bm_sum = total_bm_m + total_bm_m2) |> 
  ggplot(aes(x = (log(total_bm)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  # Adjust alpha for fill transparency
  # scale_fill_manual(values = c("FCE" = "red", "MCR" = "green", "SBC" = "blue")) +
  labs(title = "Frequency of Total Dry Biomass ~ Site",
       x = "Log Total Dry Biomass (g/m_m2)",
       y = "Frequency") +
  theme_classic() +
  # theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(
  filename = "log_total_biomass_freq.png",
  path = "plots/figure1/histogram/",
  width = 15, height = 9
)

# log size structure ------------------------------------------------------

dt |> 
  filter(projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> 
  # mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
  #        total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
  #        bm_sum = total_bm_m + total_bm_m2) |> 
  ggplot(aes(x = log((mean_max_community_size)), fill = projecthabitat)) +
  geom_density(alpha = 0.5) +  # Adjust alpha for fill transparency
  # scale_fill_manual(values = c("FCE" = "red", "MCR" = "green", "SBC" = "blue")) +
  labs(title = "Frequency of Individual Dry Biomass ~ Site",
       x = "Log Total Individual Dry Biomass (g)",
       y = "Frequency") +
  theme_classic() +
  # theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

ggsave(
  filename = "log_individual_biomass_freq.png",
  path = "plots/figure1/histogram/",
  width = 15, height = 9
)



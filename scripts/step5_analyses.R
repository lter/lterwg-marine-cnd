###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): explore and analyzed harmonized excretion data
###date(s): January 2024
###note(s): 
###### 02-10-2024: 
## Something weird going on with CCE - saying that there are NAs in
## dmperind_g_ind column and influencing the size structure summing, which is
## no bueno

###########################################################################
# load necessary packages -------------------------------------------------
###########################################################################

# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra)
# dir.create(path = file.path("tier2"), showWarnings = F)
###########################################################################
# connect to google drive -------------------------------------------------
###########################################################################
# ONLY NEED TO BE DONE ONCE
# # pull in the harmonized data
# exc_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) %>%
#   dplyr::filter(name %in% c("harmonized_consumer_excretion.csv"))
# 
# # Combine file IDs
# exc_ids <- rbind(exc_ids)
# 
# # For each raw data file, download it into the consumer folder
# for(k in 1:nrow(exc_ids)){
# 
#   # Download file (but silence how chatty this function is)
#   googledrive::with_drive_quiet(
#     googledrive::drive_download(file = exc_ids[k, ]$id, overwrite = T,
#                                 path = file.path("tier2", exc_ids[k, ]$name)) )
# 
#   # Print success message
#   message("Downloaded file ", k, " of ", nrow(exc_ids))
# }
# 
# # Clear environment
# rm(list = ls())

###########################################################################
# load harmonized excretion data ------------------------------------------
###########################################################################

# read in the harmonized data and start the wrangling, by project
df <- read.csv(file.path("tier2", "harmonized_consumer_excretion.csv"),
               stringsAsFactors = F,na.strings =".")
glimpse(df)
unique(df$project)

###########################################################################
# Fix Untrue Zeros in FCE Dataset -----------------------------------------
###########################################################################

fce <- df |> 
  filter(project == "FCE")
glimpse(fce)

fce_wide <- fce |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit),
              values_from = measurement_value)
glimpse(fce_wide)
summary(fce_wide)
sum(is.infinite(fce_wide$`density_num/m`))#2 infinites - going to simply remove
fce_wide_noINF <- fce_wide[!is.infinite(fce_wide$`density_num/m`), ]

fce_summ <- fce_wide_noINF |> 
  group_by(year, month, site, subsite_level1, subsite_level2) |>
  mutate(bm_tot_m = sum(`dmperind_g/ind`*`density_num/m`))

fce_summ_clean <- fce_summ |> 
  filter(!is.na(subsite_level1)) |> #removes weird NAs in subsite_level1 -still kicking around
  filter(!(site == "TB" & year %in% c(2004:2007))) #removes years 2004 through 2007 for TB - we werent sampling then

#seems like a lot of artifical zeros 
#below shows legitimate zeros - need to recode columns and select() then join to link up with above and get rid of artifical zeros
master_map <- read_csv("../mw_dissertation/MAP_database_maintanence/mastermap_yrs1thru19.csv")
map1 <- master_map |> 
  select(HYDROYEAR, s.mo, DRAINAGE, SITE, BOUT, SPECIESCODE) |> 
  separate(HYDROYEAR, into = c("year", "void")) |> 
  select(-void) |> 
  rename(
    year = year,
    month = s.mo,
    site = DRAINAGE,
    subsite_level1 = SITE,
    subsite_level2 = BOUT,
    spp_code = SPECIESCODE
  ) |> 
  mutate(year = as.integer(year),
         month = as.integer(month),
         subsite_level1 = as.character(subsite_level1),
         subsite_level2 = as.character(subsite_level2)) |> 
  filter(spp_code == 13) |> 
  na.omit()

fce_join <- left_join(fce_summ_clean, map1, by = c("year", "month", "site", "subsite_level1", "subsite_level2"))

fce_true_zeros <- fce_join |> 
  filter(bm_tot_m != 0 | (bm_tot_m == 0 & spp_code == 13)) |> 
  select(-spp_code) #this is right - gets rid of made-up site combinations - double checked before removing spp_code column

# pivot back + bind with full dataset -------------------------------------

fce_long <- fce_true_zeros %>%
  select(-bm_tot_m) |> 
  pivot_longer(
    cols = c(`density_num/m`, `dmperind_g/ind`, temp_c, `density_num/m2`, `nind_ug/hr`, `pind_ug/hr`),
    names_to = c("measurement_type", "measurement_unit"), 
    names_sep = "_",
    values_to = "measurement_value"
  )

df_4join <- df |> 
  filter(project != "FCE")

df_clean <- bind_rows(df_4join, fce_long)

# Pivot Full Dataset Wider ------------------------------------------------

df_wide <- df_clean |> 
  pivot_wider(names_from = c(measurement_type, measurement_unit),
              values_from = measurement_value) |> 
  janitor::clean_names() |> 
  mutate(projecthabitat = paste(project, habitat))
  
# write.csv(dat, "data/exc_clean_02082024.csv") #this is the full dataset from google drive with FCE fixed

###########################################################################
# Summarizing Data --------------------------------------------------------
###########################################################################

# clear environment + read in cleaned data --------------------------------

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
  select(-...1) |> 
  arrange(sdate)

rm(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef)

###########################################################################
# plotting ----------------------------------------------------------------
###########################################################################

# NITROGEN PLOTS ----------------------------------------------------------

# N Supply ~ Time + Site --------------------------------------------------

nsupply_sitelevel <- function(f) {
  plotting_dat_ready |> 
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

nsupply_figure1 <- map(unique(plotting_dat_ready$projecthabitat), nsupply_sitelevel)

# ggsave(
#   filename = "nsupply_sitelevel_seperate_02102024.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nsupply_figure1, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# N Supply ~ Time + Site + Strata (smoothed) ------------------------------

nsupply_sitelevel_2 <- function(f) {
  plotting_dat_ready |> 
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

nsupply_figure2 <- map(unique(plotting_dat_ready$projecthabitat), nsupply_sitelevel_2)

# ggsave(
#   filename = "nsupply_sitelevel2_seperate_02102024.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nsupply_figure2, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# N Supply ~ Time + Strata (smoothed) -------------------------------------

nsupply_strata_smoothed <- function(f) {
  plotting_dat_ready |> 
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

nsupply_figure3 <- map(unique(plotting_dat_ready$projecthabitat), nsupply_strata_smoothed)

# ggsave(
#   filename = "nsupply_strata_smoothed_seperate_02102024.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nsupply_figure3, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# N Supply ~ Time + Strata (stacked bar chart) ----------------------------

nsupply_annual_stacked <- function(f) {
  plotting_dat_ready |>
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

nsupply_figure4 <- map(unique(plotting_dat_ready$projecthabitat), nsupply_annual_stacked)

# ggsave(
#   filename = "nsupply_annual_stacked_seperate_02102024.pdf",
#   path = "plots/figure1/nitrogen/",
#   plot = marrangeGrob(nsupply_figure4, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# PHOSPHORUS PLOTS --------------------------------------------------------
# Will only be plotting versions 2 and 4 of figures above, because I believe
## those are the most informative - true for remainder - P, Biomass, & Size Structure

# P Supply ~ Time + Site + Strata (smoothed) ------------------------------

psupply_sitelevel_2 <- function(f) {
  plotting_dat_ready |> 
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

psupply_figure2 <- map(unique(plotting_dat_ready$projecthabitat), psupply_sitelevel_2)

# ggsave(
#   filename = "psupply_sitelevel2_seperate_02102024.pdf",
#   path = "plots/figure1/phosphorus/",
#   plot = marrangeGrob(psupply_figure2, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# P Supply ~ Time + Strata (stacked bar chart) ----------------------------

psupply_annual_stacked <- function(f) {
  plotting_dat_ready |>
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

psupply_figure4 <- map(unique(plotting_dat_ready$projecthabitat), psupply_annual_stacked)

# ggsave(
#   filename = "psupply_annual_stacked_seperate_02102024.pdf",
#   path = "plots/figure1/phosphorus/",
#   plot = marrangeGrob(psupply_figure4, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# BIOMASS PLOTS -----------------------------------------------------------

glimpse(plotting_dat_ready)
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
plotting_dat_ready <- plotting_dat_ready %>%
  mutate(total_bm_m = ifelse(is.na(total_bm_m), 0, total_bm_m),
         total_bm_m2 = ifelse(is.na(total_bm_m2), 0, total_bm_m2),
         bm_sum = total_bm_m + total_bm_m2)

# BM ~ Time + Site + Strata (smoothed) ------------------------------------

bmsupply_sitelevel_2 <- function(f) {
  plotting_dat_ready |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_bm = mean(bm_sum, na.rm = TRUE)) |> 
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

bmsupply_figure2 <- map(unique(plotting_dat_ready$projecthabitat), bmsupply_sitelevel_2)

# ggsave(
#   filename = "bm_sitelevel2_seperate_02102024.pdf",
#   path = "plots/figure1/biomass/",
#   plot = marrangeGrob(bmsupply_figure2, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# BM ~ Time + Strata (stacked bar chart) ---------------------------------

bmsupply_annual_stacked <- function(f) {
  plotting_dat_ready |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_total_bm = mean(bm_sum, na.rm = TRUE)) |> 
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

bmsupply_figure4 <- map(unique(plotting_dat_ready$projecthabitat), bmsupply_annual_stacked)

# ggsave(
#   filename = "bm_annual_stacked_seperate_02102024.pdf",
#   path = "plots/figure1/biomass/",
#   plot = marrangeGrob(bmsupply_figure4, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# Size Structure Plots ----------------------------------------------------

# BM ~ Time + Site + Strata (smoothed) ------------------------------------

bmindsupply_sitelevel_2 <- function(f) {
  plotting_dat_ready |> 
    group_by(projecthabitat, units, sdate, year, month, color, group) |> 
    summarise(mean_total_bm_ind = mean(bm_ind_mean_nozeros, na.rm = TRUE)) |> 
    ungroup() |>  
    filter(!is.na(color)) |> #removes weird NA point in PISCO dataset
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(x = sdate, y = mean_total_bm_ind, color = group, group = group)) +
    geom_line(alpha = 0.6) + #makes lines more transparent
    # geom_point() +
    geom_smooth(aes(linetype = color, group = color), 
                method = "loess", span = 0.9, se = TRUE, color = "black") + #separates the aesthetics of the smoothing term from the actual sites being plotted
    theme_classic() +
    labs(title = f,
         x = 'Date',
         y = 'Mean Individual Dry Mass (g)') +
    scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          # legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "none")
}

bmindsupply_figure2 <- map(unique(plotting_dat_ready$projecthabitat), bmindsupply_sitelevel_2)

# ggsave(
#   filename = "ind_bm_sitelevel2_seperate_02102024.pdf",
#   path = "plots/figure1/size structure/",
#   plot = marrangeGrob(bmindsupply_figure2, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

# BM ~ Time + Strata (stacked bar chart) ---------------------------------

bmindsupply_annual_stacked <- function(f) {
  plotting_dat_ready |>
    group_by(projecthabitat, year, color) |> 
    summarise(mean_total_bm_ind = mean(bm_ind_mean_nozeros, na.rm = TRUE)) |> 
    ungroup() |> 
    filter(!is.na(color)) |>  #removes weird NA from PISCO 
    filter(projecthabitat == f) |> #this is what we will use to map() plot across
    ggplot(aes(fill = color, x = as.factor(year), 
               y = as.numeric(mean_total_bm_ind))) + #makes year a factor, important for showing each year on x axis
    geom_bar(position = "stack", stat = "identity") +
    labs(title = f, 
         x = "Year", 
         y = "Mean Annual Individual Dry Mass (g)") +
    scale_x_discrete(name = "Year") + #plots each year on x axis for ease of interpretation
    theme(panel.grid.major = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
          panel.grid.minor = element_blank(),legend.position = "top")
}

bmindsupply_figure4 <- map(unique(plotting_dat_ready$projecthabitat), bmindsupply_annual_stacked)

# ggsave(
#   filename = "ind_bm_annual_stacked_seperate_02102024.pdf",
#   path = "plots/figure1/size structure/",
#   plot = marrangeGrob(bmindsupply_figure4, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

###########################################################################
# FIGURE TWO --------------------------------------------------------------
###########################################################################
# set up so I can go through and search and replace for each variable (i.e., 
## start with 'nitrogen', then for 'phosphorus', just find and replace) but
## will need to manually change columns I am summing by because not being read
## in as such (e.g., can't search and replace "total_n")

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

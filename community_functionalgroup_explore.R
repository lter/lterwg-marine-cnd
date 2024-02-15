###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): explore community and feeding group effects
# figure sketches: https://docs.google.com/document/d/1gySJodTrvOqhDMPKxPJD8np4Y2vbTwrXwXys5CLCYHI/edit
# figure notes: https://docs.google.com/document/d/1URPbBolZv9jcXS57kS4WzwYOvNge5aQKezemz-OoLlg/edit
###date(s): February 2024
###note(s): 

###########################################################################
# load necessary packages -------------------------------------------------
###########################################################################

# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr, gridExtra, 
                 MASS, ggrepel, vegan)

# Read in clean excretion data and set up for grouping --------------------

data <- read_csv("data/exc_clean_02082024.csv") |> 
  dplyr::select(-...1, -species, -raw_filename, -row_num)
glimpse(data)

# Replace NA in subsite_level2 and subsite_level3 with a placeholder (e.g., "Not Available")
# Making a character column instead of true 'NA', allows for the group_by function to go as far as
## it can in sequence for grouping, which is what we want here
df <- data %>%
  mutate(subsite_level2 = replace_na(subsite_level2, "Not Available"),
         subsite_level3 = replace_na(subsite_level3, "Not Available")) |> 
  unite("projecthabitat", project, habitat, sep = "-", remove = FALSE) |> #since SBC has two habitats monitored, joining here for distinguishing column for plotting
  mutate(sdate = ymd(paste(year, month, "01", sep = "-"))) |> #generating date for plotting timeseries
  dplyr::select(-day, -date)

strata <- read_csv("data/strata_mcr_sbc_fce.csv") |> 
  distinct() |> 
  filter(project %in% c("MCR", "SBC", "FCE"))

dat <- left_join(df, strata, by = c("project", "habitat", "site"))

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
  arrange(sdate)

#clean up environment
rm(cce, fce, mcr, pisco_central, pisco_south, sbc_beach, sbc_reef, 
   dat, data, df, strata)

# df_aggregated <- plotting_dat_ready |> 
#   group_by(projecthabitat, year, month, site, subsite_level1, subsite_level2, subsite_level3, scientific_name) %>%
#   summarize(density_num_m = ifelse(is.na(density_num_m), 0, density_num_m),
#          density_num_m2 = ifelse(is.na(density_num_m2), 0, density_num_m2),
#          dens = density_num_m + density_num_m2) |> 
#   ungroup()
# glimpse(df_aggregated)

df_strata_spp <- plotting_dat_ready |> 
  mutate(density_num_m = ifelse(is.na(density_num_m), 0, density_num_m),
         density_num_m2 = ifelse(is.na(density_num_m2), 0, density_num_m2),
         dens = density_num_m + density_num_m2) |> 
  group_by(projecthabitat, year, color, scientific_name) |> 
  summarize(total_dens = sum(dens, na.rm = TRUE)) |> 
  ungroup()


# Convert aggregated data to a species matrix required by vegan's diversity function
# Each row represents a site-year combination, and columns represent species
# Cells contain the total density of each species for each site-year combination
df_matrix <- pivot_wider(df_strata_spp, names_from = scientific_name, values_from = total_dens, values_fill = list(total_dens = 0))

# Extract the matrix of counts, excluding year and site columns for diversity calculation
species_matrix <- as.matrix(df_matrix[,-c(1:3)])

# Calculate Shannon diversity (H') and Pielou's evenness (J') for each site-year
diversity_indices <- apply(species_matrix, 1, function(x) {
  H <- diversity(x, index = "shannon")
  S <- specnumber(x)
  J <- H / log(S)
  return(c(H = H, J = J))
})

# Convert results to a data frame
evenness_df <- as.data.frame(t(diversity_indices), row.names = NULL)
colnames(evenness_df) <- c("Shannon_Diversity", "Pielous_Evenness")

# Add year and site information back to the results
evenness_df$projecthabitat <- df_matrix$projecthabitat
evenness_df$year <- df_matrix$year
evenness_df$color <- df_matrix$color


evenness_df |>
  filter(projecthabitat == "MCR") |> 
  ggplot(aes(x = year, y = Pielous_Evenness, group = color, color = color)) +
  geom_line() + # Add lines
  geom_point() + # Add points
  theme_minimal() + # Use a minimal theme
  labs(title = "Species Evenness Across Sites Over Years",
       x = "Year",
       y = "Pielou's Evenness",
       color = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve x-axis label readability


evenness_df |> 
  mutate(year = ymd(paste0(year, "0101"))) |> 
  filter(!is.na(color),
         projecthabitat %in% c('FCE-estuary', 'MCR-ocean', 'SBC-ocean')) |> #remove weird NAs from PISCO South (only a few)
  ggplot(aes(x = year, y = Shannon_Diversity,
             color = color)) + #change label to only year for facet_wrapped plot
  geom_line() +
  geom_point(alpha = 0.9, size = 4) +
  # geom_text_repel() +
  # geom_smooth(method = 'rlm', se = FALSE) +
  geom_smooth(method = "rlm", se = FALSE) +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ projecthabitat, scales = 'free') +
  theme_classic() +
  labs(title = "Shannon Diversity Indices ~ Space + Time",
       x = 'Year',
       y = 'Shannon Diversity') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")

# ggsave(
#   filename = "temporal_spp_eve_ShDiv_fig4_subset.png",
#   path = "plots/",
#   width = 15, height = 9
# )

# Shannon Diversity ~ Space + Time (all sites seperate) -------------------

spp_eve_ShDiv <- function(f) { 
evenness_df |> 
  mutate(year = ymd(paste0(year, "0101"))) |> 
  filter(!is.na(color)) |> #remove weird NAs from PISCO South (only a few)
  filter(projecthabitat == f) |> 
  ggplot(aes(x = year, y = Shannon_Diversity,
             color = color)) + #change label to only year for facet_wrapped plot
  geom_line() +
  geom_point(alpha = 0.9, size = 4) +
  # geom_text_repel() +
  # geom_smooth(method = 'rlm', se = FALSE) +
  geom_smooth(method = "rlm", se = FALSE) +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  # facet_wrap(~ projecthabitat, scales = 'free') +
  theme_classic() +
  labs(title = paste("Shannon Diversity Indices ~ Space + Time:", f),
       x = 'Year',
       y = 'Shannon Diversity') +
  scale_x_date(date_breaks = '1 year', date_labels = "%Y") + #adds all years to x axis for ease of interpetations
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line("black"),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "right")
}

spp_eve_ShDiv_fig4 <- map(unique(plotting_dat_ready$projecthabitat), spp_eve_ShDiv)

# ggsave(
#   filename = "temporal_spp_eve_ShDiv_fig4_seperate.pdf",
#   path = "plots/",
#   plot = marrangeGrob(spp_eve_ShDiv_fig4, nrow = 1, ncol = 1),
#   width = 15, height = 9
# )

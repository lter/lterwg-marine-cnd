### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")
### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  dplyr::select(project, ecosystem, ocean, latitude, site, year, month, vert, everything()) |> 
  dplyr::select(-n_spp)

dt1 <- left_join(dt, add, by = "project")

vert_colors <- c("vertebrate" = "black", "invertebrate" = "#ED6464")
ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")
dt1$axis_name_5 <- factor(dt1$axis_name_5, 
                          levels = c("VCR", "SBCO", "SBCB",
                                     "PCCS", "PCCC", "MCR",
                                     "PIE", "FCE",
                                     "NGA", "CCE"))

### summarize all sites measured within the dataset annualy
plot_dt <- dt1 |> 
  ### https://nga.lternet.edu/wp-content/uploads/2022/06/SKQ202210S_CruisePlan.pdf
  ### kodiak island and middleton data is terrible, so remove
  ### link at top indicates KIP is part of PWS sampling program, so join
  mutate(habitat = fct_recode(habitat, 
                              "Prince William Sound" = "Knight Island Passage",
                              "MPA" = "Marine Protected Area")) |> 
  filter(!habitat %in% c("Kodiak Island", "Middleton Island")) |> 
  group_by(axis_name_5, vert, ecosystem_2, strata, habitat, year, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            mean_p = mean(total_phosphorus),
            mean_bm = mean(total_biomass),
            mean_max_ss = mean(max_size)) |> 
  na.omit() |> 
  ungroup() |> 
  unite(project_habitat, c(axis_name_5, habitat), sep = " ", remove = FALSE) |> 
  unite(site_vert, c(site, vert), sep = " ", remove = FALSE) |> 
  filter(site != "RB-17")

glimpse(plot_dt)

test <- plot_dt |> 
  filter(axis_name_5 == "FCE")

ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")

# number_of_ts <- plot_dt |> 
#   group_by(axis_name_5) |> 
#   summarize(ts_trends = n_distinct(site_vert))

plot_dt |> 
  group_by(project_habitat, vert, year, site, site_vert) |> 
  summarise(mean_nitrogen = mean(mean_n, na.rm = TRUE)) |> 
  ungroup() |>
  ggplot(aes(x = year, y = mean_nitrogen, group = site_vert, color = vert)) +
  geom_line(alpha = 0.8, linewidth = 0.3) +
  scale_color_manual(values = vert_colors) +
  labs(y = "Aggregate Nitrogen Supply Rate Trend") +
  theme_classic() +
  facet_wrap(~project_habitat, scales = "free", ncol = 4) +
  theme(
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.title.y = element_text(face = "bold", size = 18, color = "black"),
        axis.title.x = element_blank(),
        axis.line = element_line("black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", size = 14, color = "black"),
        legend.title = element_text(face = "bold", size = 14, color = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12, color = "black"))  # Customize facet label text)

# ggsave(
#   filename = "output/ms first round/plots/program_habitat_ts_multipanel.tiff",
#   width = 12, height = 12
# )

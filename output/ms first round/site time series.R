### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled)

exc <- read_csv("../../local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("../../local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  dplyr::select(project, ecosystem, ocean, latitude, site, year, month, vert, everything()) |> 
  dplyr::select(-n_spp)

### summarize all sites measured within the dataset annualy
plot_dt <- dt |> 
  ### https://nga.lternet.edu/wp-content/uploads/2022/06/SKQ202210S_CruisePlan.pdf
  ### kodiak island and middleton data is terrible, so remove
  ### link at top indicates KIP is part of PWS sampling program, so join
  mutate(habitat = fct_recode(habitat, 
                              "Prince William Sound" = "Knight Island Passage",
                              "MPA" = "Marine Protected Area")) |> 
  filter(!habitat %in% c("Kodiak Island", "Middleton Island")) |> 
  group_by(project, units, vert, ecosystem, latitude, strata, habitat, year, month, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            mean_p = mean(total_phosphorus),
            mean_bm = mean(total_biomass),
            mean_max_ss = mean(mean_max_size)) |> 
  na.omit() |> 
  ungroup() |> 
  unite(project_vert, c(project, vert), sep = "-", remove = FALSE) |> 
  unite(project_units, c(project, units), sep = " ", remove = FALSE) |> 
  unite(project_habitat, c(project, habitat), sep = " ", remove = FALSE) |> 
  unite(site_vert, c(site, vert), sep = " ", remove = FALSE) |> 
  unite(project_site_vert, c(project, site_vert), sep = " ", remove = FALSE)
  
glimpse(plot_dt)

plot_dt |> 
  group_by(project_habitat, units, site_vert, vert, year, habitat) |> 
  summarise(mean_nitrogen = mean(mean_n, na.rm = TRUE)) |> 
  ungroup() |>
  ggplot(aes(x = year, y = mean_nitrogen, group = site_vert, color = vert)) +
  geom_line(alpha = 0.8, linewidth = 0.3) +
  theme_classic() +
  facet_wrap(~project_habitat, scales = "free") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_text(face = "bold", size = 12, color = "black"),
        axis.line = element_line("black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", size = 14, color = "black"),
        legend.title = element_text(face = "bold", size = 14, color = "black"),
        strip.background = element_rect(fill = "white", color = NA),  # Remove border around facet labels
        strip.text = element_text(face = "bold", size = 11, color = "black"))  # Customize facet label text)
# 
# ggsave(
#   filename = "test_timeseries_05062024.tiff",
#   path = "plots/",
#   width = 14, height = 7
# )

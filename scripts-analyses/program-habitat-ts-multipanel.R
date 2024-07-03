### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled)

exc <- read_csv("local_data/model_data_all_final_07032024.csv") #all sites, no 10 year cutoff

# dt <- exc |> 
#   filter(site != "RB-17",
#          program != "PIE")



### summarize all sites measured within the dataset annualy
plot_dt <- exc |> 
  ### https://nga.lternet.edu/wp-content/uploads/2022/06/SKQ202210S_CruisePlan.pdf
  ### kodiak island and middleton data is terrible, so remove
  ### link at top indicates KIP is part of PWS sampling program, so join
  # mutate(habitat = fct_recode(habitat, 
  #                             "Prince William Sound" = "Knight Island Passage",
  #                             "MPA" = "Marine Protected Area")) |> 
  # filter(!habitat %in% c("Kodiak Island", "Middleton Island")) |> 
  group_by(program, habitat, year, site) |>
  summarize(mean_n = mean(total_nitrogen),
            mean_p = mean(total_phosphorus),
            mean_bm = mean(total_biomass),
            mean_max_ss = mean(max_size)) |> 
  na.omit() |> 
  ungroup() |> 
  mutate(project_habitat = paste(program, habitat, sep = "-"))


plot_dt |> 
  ggplot(aes(x = year, y = mean_n, group = site)) +
  geom_line(alpha = 0.8, linewidth = 0.3) +
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

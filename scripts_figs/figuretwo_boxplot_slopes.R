label_mapping <- data.frame(
  projecthabitat = unique(dt$projecthabitat),
  label_name = c("CCE", "PISCO-Central", "PISCO-South", "FCE",
                 "MCR", "SBC-Ocean", "SBC-Beach", "NGA", 
                 "PIE", "VCR") # Replace with actual labels
)

cols

model_results <- dt |> 
  group_by(projecthabitat, color2, year) |> 
  mutate(mean_nitrogen = mean(total_n, na.rm = TRUE),
         sd_nitrogen = sd(total_n, na.rm = TRUE),
         sd_nitrogen = replace_na(sd_nitrogen, 0)) |> 
  ungroup() |> 
  # model_results <- test %>%
  # filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) %>%
  group_by(projecthabitat, color2) %>%
  do({
    model <- lm(sd_nitrogen ~ mean_nitrogen, data = .)
    data.frame(slope = coef(model)[2])
  }) %>%
  ungroup() |> 
  group_by(projecthabitat) %>%
  na.omit() |> 
  mutate(median_slope = median(slope)) %>%
  ungroup()

model_results |> 
  left_join(label_mapping, by = "projecthabitat") %>%
  mutate(projecthabitat_label = factor(label_name, levels = unique(label_name[order(median_slope)]))) |> 
  ggplot(aes(x = projecthabitat_label, y = slope)) +
  geom_boxplot() +
  labs(x = "Project", y = "Slope: SD Nitrogen Supply ~ Mean Nitrogen Supply", title = "Box plot of Slopes by Project Habitat") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(size = 14, hjust=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # legend.position = 'right',
        # legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave(
  filename = "slope_figtwo_nitrogen.tiff",
  path = "plots/",
  width = 21, height = 7
)

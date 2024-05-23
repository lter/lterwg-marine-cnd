dt %>%
  filter(projecthabitat %in% c("FCE-estuary", "MCR-ocean", "SBC-ocean")) %>%
  group_by(projecthabitat, color, year) %>%
  arrange(year) %>%
  mutate(change_total_n = total_n - lag(total_n)) |> 
  summarise(mean_change_total_n = mean(change_total_n, na.rm = TRUE),
            sd_change_total_n = sd(change_total_n, na.rm = TRUE)) %>%
  mutate(mean_change_total_n_z = (mean_change_total_n - mean(mean_change_total_n)) / sd(mean_change_total_n),
         sd_change_total_n_z = (sd_change_total_n - mean(sd_change_total_n)) / sd(sd_change_total_n)) %>%
  mutate(row_id = row_number()) |> 
  ungroup() |> 
  filter(row_id != 1) |> 
  unite(p_strata, c(projecthabitat, color), sep = "-", remove = FALSE) |> #only needed for all together figure
  ggplot(aes(x = mean_change_total_n_z, y = sd_change_total_n_z, color = p_strata, label = year)) +
  geom_point() +
  geom_text_repel() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(title = "Z-Scored Annual Change in total_n",
       x = "Z-Scored Mean Annual Change in Areal Nitrogen Supply (ug/hr/m_m2)",
       y = "Z-Scored SD Annual Change in Areal Nitrogen Supply (ug/hr/m_m2)")

ggsave(
  filename = "figure2b_z_nitrogen.png",
  path = "plots/figure2/zscore/nitrogen/",
  width = 15, height = 9
)
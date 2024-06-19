###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White, Li Kui, Angel Chen
###goal(s): nutrient supply stability correlation figure
###date(s): May 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, ggpmisc, corrplot, performance, ggeffects, ggpubr, parameters)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  select(-n_spp)

na_count_per_column <- sapply(dt, function(x) sum(is.na(x)))
print(na_count_per_column) 
dt1 <- na.omit(dt)

### summarize all sites measured within the dataset annualy
model_dt <- dt1 |> 
  group_by(project, ecosystem, vert, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            bm_stability = 1/cv_bm,
            min_ss = mean(mean_min_size),
            mean_ss = mean(mean_mean_size),
            max_ss = mean(mean_max_size)) |> 
  na.omit() |> 
  ungroup() |> 
  filter(n_stability <= 2.5)

glimpse(model_dt)

dat <- left_join(model_dt, add, relationship = "many-to-many")

vert_colors <- c("vertebrate" = "black", "invertebrate" = "#ED6464")
ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")

# Generate the scatter plot with separate regression lines for 'vertebrate' and 'invertebrate'
my_formula <- y~x

dat |> 
ggplot(aes(x = bm_stability, y = n_stability, color = vert)) +
  geom_point(size = 4) +
  geom_smooth(data = dat[dat$vert == "vertebrate",], method = "lm", se = FALSE, size = 1.5) +
  geom_smooth(data = dat[dat$vert == "invertebrate",], method = "lm", se = FALSE, size = 1.5) +
  # stat_regline_equation(aes(label = ..rr.label..),
  #   formula = my_formula) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "black", size = 2) +
  scale_color_manual(values = vert_colors) +
  labs(x = "Aggregate Biomass Stability", y = "Aggregate Nitrogen Supply Rate Stability") +
  scale_x_continuous(limits = c(0.3,2.0), breaks = c(0.5,1,1.5,2))+
  scale_y_continuous(limits = c(0.3,2.5), breaks = c(0.5,1,1.5,2,2.5))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(color = "black", face = "bold", size = 18),
        axis.title.y = element_text(color = "black", face = "bold", size = 18),
        axis.line = element_line("black"),
        axis.text.x = element_text(color = "black", face = "bold", size = 16),
        axis.text.y = element_text(color = "black", face = "bold", size = 16),
        # axis.text.y = element_text(color = "black", face = "bold", size = 18),
        plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        legend.position = "none")

# ggsave(
#   filename = "output/ms first round/plots/stability_correlation.tiff",
#   width = 10, height = 10
# )

###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): hierarchical models for species evenness and richness
###date(s): June 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters)

sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

dat <- read.csv("local_data/model_data_clean.csv") |> 
  select(program, ecosystem, climate, habitat, site, n_stability, everything()) |> 
  # rename(program = project,
  #        spp_diversity = SppInvSimpDivInd,
  #        trophic_diversity = TrophInvSimpDivInd) |> 
  mutate(program = as.factor(program))
glimpse(dat)

model_data_scaled <- dat |> 
  group_by(program) |> 
  ### this is a function syntax
  mutate(across(latitude:trophic_div,\(x) scale(x, center = TRUE))) |> 
  ungroup() |> 
  mutate(n_stability_scaled = scale(n_stability))

###########################################################################
# diversity-stability plots at program ------------------------------------
###########################################################################

# Species Richness Plots --------------------------------------------------

rich_scaled <- model_data_scaled |> 
  ggplot(aes(x = species_rich, y = n_stability, color = program)) +
  geom_point() +  # Adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
  labs(x = "Scaled Species Richness",
       y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        axis.title.y = element_text(face = "bold", color = "black"),
        legend.position = "none",
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))


# Species Evenness Plots --------------------------------------------------

div_scaled <- model_data_scaled |> 
  rename(Program = program) |> 
  ggplot(aes(x = species_div, y = n_stability, color = Program)) +
  geom_point() +  # Adds the scatter plot points
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear model lines for each program
  labs(x = "Scaled Species Diversity (Inverse Simpson)",
       y = "Scaled Aggregate Nitrogen Supply Stability (1/CV)") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(face = "bold", color = "black"),
        legend.title = element_text(face = "bold", color = "black"))

ggarrange(rich_scaled, div_scaled,
          labels = c('a)','b)'),
          ncol = 2, nrow = 1, align = "h")

ggsave("output/ms first round/plots/two-panel-dsr-meeting.tiff", units = "in", width = 10,
       height = 6, dpi =  600, compression = "lzw")

# ggsave("output/ms first round/plots/two-panel-dsr-meeting-legend.tiff", units = "in", width = 10,
#        height = 6, dpi =  600, compression = "lzw")
# hierarchical models -----------------------------------------------------

m1 <- glmmTMB(n_stability ~ spp_rich + (spp_rich|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m1)
# summary(m1)

m2 <- glmmTMB(n_stability ~ spp_rich + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m2)
# summary(m2)

m3 <- glmmTMB(n_stability ~ (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m3)
# summary(m3)

m4 <- glmmTMB(n_stability ~ spp_diversity + (spp_diversity|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m4)
# summary(m4)

m5 <- glmmTMB(n_stability ~ spp_diversity + (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m5)
# summary(m5)

m6 <- glmmTMB(n_stability ~ (1|program), data = model_data_scaled,
              # family = gaussian(link = "log"),
              control = glmmTMBControl(optimizer=optim,
                                       optArgs = list(method = 'CG')),
              REML = FALSE)
# model_performance(m6)
# summary(m6)

model_table_richness <- performance::compare_performance(m1,m2,m3)
model_table_evenness <- performance::compare_performance(m4,m5,m6)

aicc_richness <- model_table_richness |> 
  mutate(delta_aicc = AICc - min(AICc))

aicc_evenness <- model_table_evenness |> 
  mutate(delta_aicc = AICc - min(AICc))

# full global models ------------------------------------------------------

global_model_N <- glmmTMB(
  n_stability ~ 
    # ecosystem +
    # latitude +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    spp_diversity + trophic_diversity + (1|program),
  data = model_data_scaled,
  na.action = "na.fail",
  # family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N)
performance::check_model(global_model_N)

model_set_N <- dredge(global_model_N,
                      subset = !(`cond(spp_diversity)`&&`cond(spp_rich)`) & !(`cond(mean_bm)` && `cond(max_ss)`) & !(`cond(spp_rich)` && `cond(fam_richness)`)) |>
  filter(delta < 2)



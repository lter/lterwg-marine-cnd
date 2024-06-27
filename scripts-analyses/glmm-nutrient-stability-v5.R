###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): nutrient supply stability models
###date(s): June 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, corrplot, performance, ggeffects, ggpubr, parameters)

data <- read_csv("local_data/model_data_clean_final.csv")
glimpse(data)

model_dt <- data|>
  ### mutates character class columns as factors
  mutate_at(c("program", "habitat", "site"), as.factor)
glimpse(model_dt)

model_data <- model_dt |> 
  ### selection of variables we want for regression models
  select(program, site, habitat, 
         n_stability, p_stability, bm_stability,
         max_size_stability, spp_rich_stability, fam_rich_stability,
         SppInvSimpDivInd_stability, TrophInvSimpDivInd_stability,
         mean_bm, mean_max_ss, mean_spp_rich, mean_fam_rich,
         mean_SppInvSimpDivInd, mean_TrophInvSimpDivInd)
glimpse(model_data)

# explore collinearity ----------------------------------------------------

numeric_data <- model_data |> 
  ### filter for only numeric data
  select(where(is.numeric))

### generate correlation matrix and visually assess collinearity
cor_matrix <- cor(numeric_data)
### visually assess
corrplot(cor_matrix, method = "number", tl.cex = 0.4)

###########################################################################
# create global model -----------------------------------------------------
###########################################################################

model_data_scaled <- model_data |> 
  mutate(across(mean_bm:mean_TrophInvSimpDivInd,\(x) scale(x, center = FALSE)))

glimpse(model_data_scaled)

global_model_N <- glmmTMB(
  n_stability ~ 
    mean_bm + mean_max_ss +
    mean_fam_rich + mean_spp_rich +
    mean_SppInvSimpDivInd + mean_TrophInvSimpDivInd + (1|program),
  data = model_data_scaled,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N)
performance::check_model(global_model_N)

model_set_N <- dredge(global_model_N,
                      subset = !(`cond(mean_SppInvSimpDivInd)`&&`cond(mean_spp_rich)`) & !(`cond(mean_bm)` && `cond(mean_max_ss)`) & !(`cond(mean_spp_rich)` && `cond(mean_fam_rich)`)) |>
  filter(delta < 4)

# lets plot some relationships --------------------------------------------

###n_stability ~ mean_bm
model <- lm(n_stability ~ mean_bm, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_bm, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_bm
model <- lm(n_stability ~ mean_bm, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_bm, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_max_ss
model <- lm(n_stability ~ mean_max_ss, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_max_ss, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_spp_rich
model <- lm(n_stability ~ mean_spp_rich, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_spp_rich, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_fam_rich
model <- lm(n_stability ~ mean_fam_rich, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_fam_rich, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_SppInvSimpDivInd
model <- lm(n_stability ~ mean_SppInvSimpDivInd, data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_SppInvSimpDivInd, y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()

###n_stability ~ mean_TrophInvSimpDivInd 
model <- lm(n_stability ~ mean_TrophInvSimpDivInd , data = model_data)
r_squared <- summary(model)$r.squared
ggplot(model_data, aes(x = mean_TrophInvSimpDivInd , y = n_stability)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +  # Add a blue linear regression line
  annotate("text", x = Inf, y = Inf, label = sprintf("R² = %.2f", r_squared), 
           hjust = 1.1, vjust = 1.1, size = 5, color = "black") +
  theme_classic()


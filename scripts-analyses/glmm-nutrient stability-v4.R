###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): nutrient supply stability models
###date(s): March-April 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, corrplot, performance, ggeffects, ggpubr, parameters)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")

### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  filter(vert == "vertebrate") |> 
  ### if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  filter(!project %in% c("CCE", "NGA")) |> 
  ### recalculated species richness in community dataset prep and wanted to ensure it matched up
  ### however, can remove now
  select(-n_spp, -date)

### read in community metrics for analysis
comm <- read_csv("local_data/community_data_filtered.csv") |> 
  select(-date)

dt1 <- left_join(dt, comm, relationship = "many-to-many")

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 

testna <- dt1 |> 
  filter(is.na(Species_Richness))
### these are instances where programs-sites observed zero fishes
### do we keep these or get rid of them? I initially got rid of them since we are interested
### in aspects of community but maybe should keep and make zeros?
### omit NAs - ran both ways and doesn't change the story at all, just the coefficients slightly
### the NAs make up ~1% of all data going into the models

# dt1[is.na(dt1)] <- 0
# dt2 <- dt1

dt2 <- na.omit(dt1)

na_count_per_column <- sapply(dt2, function(x) sum(is.na(x)))
print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt2 |> 
  group_by(project, ecosystem, climate, latitude, strata, site) |> 
  summarize(mean_n = mean(total_nitrogen),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            ### calculating coefficient of variation for biomass across time for each site
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            ### calculating stability of biomass (i.e., 1/cv)
            bm_stability = 1/cv_bm,
            min_ss = mean(min_size),
            mean_ss = mean(mean_size),
            max_ss = mean(max_size),
            ### binned data from dives makes this metric hard to get after
            # size_skew = mean(mean_skew_size),
            spp_rich = mean(Species_Richness),
            fam_richness = mean(Family_Richness),
            SppShDivInd = mean(Species_Shannon_Diversity_Index),
            SppInvSimpDivInd = mean(Species_Inverse_Simpson_Diversity_Index),
            TrophShDivInd = mean(Trophic_Shannon_Diversity_Index),
            TrophInvSimpDivInd = mean(Trophic_Inverse_Simpson_Diversity_Index))|> 
  ### omit three sites with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
  na.omit() |> 
  ungroup()

# glimpse(model_dt)

# write_csv(model_dt, "local_data/model_data_clean.csv")
###########################################################################
# recode factors as such --------------------------------------------------
###########################################################################

data <- model_dt|>
  ### mutates character class columns as factors
  mutate_at(c("project", "ecosystem", "climate", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  select(n_stability, p_stability, bm_stability, project, site, strata, ecosystem, climate, 
         latitude, mean_bm, min_ss, mean_ss, max_ss,
         spp_rich, fam_richness, SppShDivInd, SppInvSimpDivInd,
         TrophShDivInd, TrophInvSimpDivInd)
glimpse(model_data)

###########################################################################
# explore collinearity ----------------------------------------------------
###########################################################################

numeric_data <- model_data |> 
  ### filter for only numeric data
  select(where(is.numeric))

### generate correlation matrix and visually assess collinearity
cor_matrix <- cor(numeric_data)
### visually assess
corrplot(cor_matrix, method = "number")

### checking for normality in dependent variable - first pull out dependent variable
normality_check <- model_data$bm_stability
### run shapiro-wilks test
result <- shapiro.test(normality_check)
### visually assess
hist(normality_check)
### data right-skewed, so check shapiro-wilks with log transformation
result <- shapiro.test(log(normality_check)) #use log link function

###########################################################################
# create global model -----------------------------------------------------
###########################################################################

model_data_scaled <- model_data |> 
  ### this is a function syntax
  mutate(across(latitude:TrophInvSimpDivInd,\(x) scale(x, center = FALSE)))

glimpse(model_data_scaled)

### all variables

global_model_N <- glmmTMB(
  n_stability ~ 
    # ecosystem +
    # latitude +
    mean_bm + max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data_scaled,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N)
performance::check_model(global_model_N)

model_set_N <- dredge(global_model_N,
                      subset = !(`cond(SppInvSimpDivInd)`&&`cond(spp_rich)`) & !(`cond(mean_bm)` && `cond(max_ss)`) & !(`cond(spp_rich)` && `cond(fam_richness)`)) |>
  filter(delta < 4)

### removing mean_biomass and family_richness

global_model_N3 <- glmmTMB(
  n_stability ~ 
    # ecosystem +
    # latitude +
    max_ss +
    spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data_scaled,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N3)
performance::check_model(global_model_N3)

model_set_N3 <- dredge(global_model_N3,
                      subset = !(`cond(SppInvSimpDivInd)`&&`cond(spp_rich)`)) |>
  filter(delta < 4)

### removing mean biomass

global_model_N2 <- glmmTMB(
  n_stability ~ 
    # ecosystem +
    # latitude +
    max_ss +
    fam_richness + spp_rich +
    SppInvSimpDivInd + TrophInvSimpDivInd + (1|project),
  data = model_data_scaled,
  na.action = "na.fail",
  family = gaussian(link = "log"),
  REML = FALSE
)
# 
diagnose(global_model_N2)
performance::check_model(global_model_N2)

model_set_N2 <- dredge(global_model_N2,
                       subset = !(`cond(SppInvSimpDivInd)`&&`cond(spp_rich)`) & !(`cond(spp_rich)` && `cond(fam_richness)`)) |>
  filter(delta < 4)

###########################################################################
# run models with delta AICc <4 -------------------------------------------
###########################################################################
m1 <- glmmTMB(n_stability ~  max_ss + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m1)

m2 <- glmmTMB(n_stability ~ fam_richness + max_ss + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m2)

m3 <- glmmTMB(n_stability ~ max_ss + spp_rich + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m3)

m4 <- glmmTMB(n_stability ~ fam_richness + max_ss + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m4) 

m5 <- glmmTMB(n_stability ~ max_ss + SppInvSimpDivInd + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m5) 

m6 <- glmmTMB(n_stability ~ max_ss + spp_rich + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m6) 

m7 <- glmmTMB(n_stability ~ fam_richness + max_ss + spp_rich + TrophInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m7) 

m8 <- glmmTMB(n_stability ~ fam_richness + max_ss + SppInvSimpDivInd + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m8) 

m9 <- glmmTMB(n_stability ~ 1 + (1|project), data = model_data_scaled,
              family = gaussian(link = "log"),
              REML = FALSE)
performance::check_model(m9)

# ### compare models and save for publication
model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6,m7,m8,m9)
# write_csv(model_table, "output/ms first round/tables/stability_model_comparison.csv")

# ### compare models and save for publication
model_set_N$weight <- as.numeric(model_set_N$weight)
glimpse(model_set_N)
# write_csv(model_set_N, "output/ms first round/tables/stability_model_set_N.csv")

###########################################################################
# plot marginal effects ---------------------------------------------------
###########################################################################

#max size structure - min = 0.003; max = 2.614
pred_max_ss <- ggeffect(m1, "max_ss[0:2.7 by=0.1]", type = "re")
#troph inv simp div ind - min = 0.537; max = 1.275
pred_TrophInvSimpDivInd <- ggeffect(m1, "TrophInvSimpDivInd[0:1.3 by=0.1]", type = "re")

### rescale to original values for plotting
# sd(model_data$max_ss)#30.85014
# sd(model_data$TrophInvSimpDivInd)#0.3127963

### plotting predicted effects of size structure
max_ss <- ggplot(pred_max_ss, aes(x = x*48.09204, y = predicted)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Max Size", y = "Predicted Aggregate Nitrogen Supply Rate Stability") +
  scale_x_continuous(limits = c(0,135), breaks = c(0,25,50,75,100,125))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

# ggsave(
#   filename = "size_structure_me.tiff",
#   path = "output/ms first round/plots/",
#   width = 10, height = 10
# )

### plotting predicted effects of trophic inverse simpson diversity
troph_simp <- ggplot(pred_TrophInvSimpDivInd, aes(x = x*2.020464, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Trophic Diversity", y = "Predicted Aggregate Nitrogen Supply Rate Stability") +
  scale_x_continuous(limits = c(0,2.7), breaks = c(0,0.5,1,1.5,2,2.5))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

# ggsave(
#   filename = "trophic_inv_simp_me.tiff",
#   path = "output/ms first round/plots/",
#   width = 10, height = 10
# )

#Plot of predicted stability~fixed effects
ggarrange(max_ss, troph_simp,
          labels = c('a)','b)'),
          ncol = 2, vjust = 1, align = "h")
### make this a horizontal figure

# saving for publication
# ggsave("output/ms first round/plots/combined_me.tiff", units = "in", width = 12,
#        height = 6, dpi =  600, compression = "lzw")

###########################################################################
# exploratory analysis ----------------------------------------------------
###########################################################################

plot_model(m1, type = 'pred', terms = c('max_ss', "project"),
           pred.type = "re", ci.lvl = NA)

model_data |> 
  filter(project %in% c("PISCO-Central", "PISCO-South")) |> 
  ggplot(aes(strata, n_stability, group = strata, color = strata))+
  geom_boxplot() + 
  facet_wrap(~project)

test <- model_data |> 
  filter(project %in% c("MCR")) |> 
  separate(site, into = c("Habitat", "SiteNumber"), sep = "-")

ggplot(test, aes(Habitat, n_stability)) +
  geom_boxplot()

model_data |> 
  ggplot(aes(project, n_stability)) + 
  geom_boxplot()


### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, vert, year, month, vert, everything()) |> 
  # filter(vert == "vertebrate") |> 
  ### if species richness is important in models, remove CCE & NGA as they are not taxonomically resolved species, but instead "groups"
  # filter(!project %in% c("CCE", "NGA")) |> 
  ### recalculated species richness in community dataset prep and wanted to ensure it matched up
  ### however, can remove now
  select(-n_spp)

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 
### 195 NAs within community dataset - because I removed sites instances where nothing was caught
### set community metrics to zero here, since it represents periods of time where nothing was collected

dt2 <- na.omit(dt1)

na_count_per_column <- sapply(dt2, function(x) sum(is.na(x)))
print(na_count_per_column) 

### summarize all sites measured within the dataset annualy
model_dt <- dt |> 
  group_by(project, ecosystem, climate, latitude, strata, site, vert) |> 
  summarize(mean_n = mean(total_nitrogen),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_n = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            n_stability = 1/cv_n,
            mean_p = mean(total_phosphorus),
            ### calculating coefficient of variation for nutrient supply across time for each site
            cv_p = (sd(total_nitrogen, na.rm = TRUE) / mean(total_nitrogen, na.rm = TRUE)),
            ### calculating stability of nutrient supply (i.e., 1/cv)
            p_stability = 1/cv_p,
            mean_bm = mean(total_biomass),
            ### calculating coefficient of variation for biomass across time for each site
            cv_bm = (sd(total_biomass, na.rm = TRUE) / mean(total_biomass, na.rm = TRUE)),
            ### calculating stability of biomass (i.e., 1/cv)
            bm_stability = 1/cv_bm,
            min_ss = mean(mean_min_size),
            mean_ss = mean(mean_mean_size),
            max_ss = mean(mean_max_size)) |> 
  ### omit three sites with NA here - it appears because there was no replication of the sites (i.e., one-offs in datasets)
  na.omit() |> 
  ungroup()

glimpse(model_dt)
###########################################################################
# recode factors as such --------------------------------------------------
###########################################################################

data <- model_dt|>
  ### mutates character class columns as factors
  mutate_at(c("project", "ecosystem", "climate", "strata", "site"), as.factor)
glimpse(data)

###########################################################################
# select covariates -------------------------------------------------------
###########################################################################

model_data <- data |> 
  ### selection of variables we want for regression models
  select(n_stability, p_stability, bm_stability, project, site, vert, strata, ecosystem, climate, 
         latitude, mean_bm, min_ss, mean_ss, max_ss)
glimpse(model_data)



one <- model_data |> 
  filter(n_stability <= 3) |> 
  ggplot(aes(project, n_stability)) + 
  geom_boxplot() +
  labs(x = "LTER Program", y = "Nitrogen Supply Stability") +
  # scale_x_continuous(limits = c(0,2.7), breaks = c(0,0.5,1,1.5,2,2.5))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.title.y = element_text(face = "bold", size = 18),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18))

# ggsave(
#   filename = "project_n_stability.tiff",
#   path = "output/ms first round/plots/",
#   width = 22, height = 10
# )

three <- model_data |> 
  filter(n_stability <= 3) |> 
  ggplot(aes(vert, n_stability, fill = vert)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Community", y = "Nitrogen Supply Stability") +
  # scale_x_continuous(limits = c(0,2.7), breaks = c(0,0.5,1,1.5,2,2.5))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

# ggsave(
#   filename = "vertebrate_vs_invertebrate_n_stability.tiff",
#   path = "output/ms first round/plots/",
#   width = 10, height = 10
# )

two<- model_data |> 
  filter(n_stability <= 3) |> 
  filter(ecosystem != "onshore") |> 
  ggplot(aes(ecosystem, n_stability, fill = ecosystem)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Ecosystem", y = "Nitrogen Supply Stability") +
  # scale_x_continuous(limits = c(0,2.7), breaks = c(0,0.5,1,1.5,2,2.5))+
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line = element_line("black"),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14))

# ggsave(
#   filename = "vertebrate_vs_invertebrate_n_stability.tiff",
#   path = "output/ms first round/plots/",
#   width = 10, height = 10
# )
###rerun biomass stability - see if it is showing same thing or not
###two types of nutrient dynamics - supply and storage

#Plot of fitted model q1.m1
ggarrange(two,three,
          labels = c('a)','b)'),
          ncol = 2, vjust = 1, align = "h")
### make this a horizontal figure

#saving for publication
ggsave("output/ms first round/plots/supp_plot.tiff", units = "in", width = 10,
       height = 6, dpi =  600, compression = "lzw")

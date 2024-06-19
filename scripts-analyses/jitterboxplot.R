###project: LTER Marine Consumer Nutrient Dynamic Synthesis Working Group
###author(s): Mack White
###goal(s): jitterbox plot(s) summmarizing program-level data
###date(s): March-April 2024
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, corrplot, performance, ggeffects, sjlabelled, ggpubr)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/jitter_axis_titles.csv")
### join data for modeling with site characteristic information
dt <- left_join(exc, sc) |> 
  dplyr::select(project, ecosystem, ocean, site, year, month, vert, everything()) |> 
  dplyr::select(-n_spp)

dt1 <- left_join(dt, add, by = "project")

vert_colors <- c("vertebrate" = "black", "invertebrate" = "#ED6464")
ecosystem_colors <- c("Coastal" = "#7fcdff", "Pelagic" = "#064273", "Estuarine" = "#76b6c4")
dt1$axis_name_5 <- factor(dt1$axis_name_5, 
                          levels = c("VCR", "SBCO", "SBCB",
                                     "PCCS", "PCCC", "MCR",
                                     "PIE", "FCE",
                                     "NGA", "CCE"))

# nitrogen jitterbox ------------------------------------------------------

b <- dt1 |> 
  group_by(axis_name_5, ecosystem_2, vert, site, year) |> 
  summarize(mean_n = mean(total_nitrogen)) |> 
  ggplot(aes(y = axis_name_5, x = log1p(mean_n), fill = ecosystem_2)) +
  geom_jitter(aes(color = vert), position = position_jitter(width = 0.2), size = 1.0) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA,  alpha = 0.75) +
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  scale_color_manual(values = vert_colors) + # Apply the color palette for jitter
  labs(title = "Aggregate Nitrogen Supply Rate", x = "log+1(μg/hr/per unit area)", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "italic", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(color = "black", face = "bold", size = 18),
        axis.text.y = element_blank(),
        # axis.text.y = element_text(color = "black", face = "bold", size = 18),
        plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        legend.position = "none")

# ggsave(
#   filename = "output/ms first round/plots/total_n_jitter_boxplot_log_short.tiff",
#   width = 16, height = 8
# )

# phosphorus jitterbox ------------------------------------------------------

c <- dt1 |> 
  group_by(axis_name_5, ecosystem_2, vert, site, year) |> 
  summarize(mean_p = mean(total_phosphorus)) |> 
  ggplot(aes(y = axis_name_5, x = log1p(mean_p), fill = ecosystem_2)) +
  geom_jitter(aes(color = vert), position = position_jitter(width = 0.2), size = 1.0) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA,  alpha = 0.75) +
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  scale_color_manual(values = vert_colors) + # Apply the color palette for jitter
  labs(title = "Aggregate Phosphorus Supply Rate", x = "log+1(μg/hr/per unit area)", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "italic", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(color = "black", face = "bold", size = 18),
        axis.text.y = element_blank(),
        # axis.text.y = element_text(color = "black", face = "bold", size = 18),
        plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        legend.position = "none")

# ggsave(
#   filename = "output/ms first round/plots/total_p_jitter_boxplot_log_short.tiff",
#   width = 16, height = 8
# )

# mean max size structure jitterbox ---------------------------------------

d <- dt1 |> 
  group_by(axis_name_5, ecosystem_2, vert, site, year) |> 
  summarize(mean_ss = mean(mean_max_size)) |> 
  ggplot(aes(y = axis_name_5, x = log1p(mean_ss), fill = ecosystem_2)) +
  geom_jitter(aes(color = vert), position = position_jitter(width = 0.2), size = 1.0) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA,  alpha = 0.75) +
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  scale_color_manual(values = vert_colors) + # Apply the color palette for jitter
  labs(title = "Mean Max Size of Community", x = "log+1(g dry weight)", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "italic", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(color = "black", face = "bold", size = 18),
        axis.text.y = element_blank(),
        # axis.text.y = element_text(color = "black", face = "bold", size = 18),
        plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        legend.position = "none")

# ggsave(
#   filename = "output/ms first round/plots/max_size_jitter_boxplot_log_short.tiff",
#   width = 16, height = 8
# )

# biomass jitterbox ---------------------------------------

a <- dt1 |> 
  group_by(axis_name_5, ecosystem_2, vert, site, year) |> 
  summarize(mean_bm = mean(total_biomass)) |> 
  ggplot(aes(y = axis_name_5, x = log1p(mean_bm), fill = ecosystem_2)) +
  geom_jitter(aes(color = vert), position = position_jitter(width = 0.2), size = 1.0) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA,  alpha = 0.75) +
  scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
  scale_color_manual(values = vert_colors) + # Apply the color palette for jitter
  labs(title = "Aggregate Biomass", x = "log+1(g dry biomass/per unit area)", y = "Project") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "italic", size = 16),
        axis.title.y = element_blank(),
        axis.line = element_line("black"),
        axis.text.x = element_text(color = "black", face = "bold", size = 18),
        # axis.text.y = element_blank(),
        axis.text.y = element_text(color = "black", face = "bold", size = 18),
        plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
        legend.position = "none")

# ggsave(
#   filename = "output/ms first round/plots/biomass_jitter_boxplot_log_short.tiff",
#   width = 16, height = 8
# )

###########################################################################

### saving three-panel figure
# #Plot of biomass, N supply, and P supply
ggarrange(a,b,c,
          labels = c('a)', 'b)', 'c)'),
          ncol = 3, vjust = 1, align = "h")

# #saving for publication - the meat of the figure
# ggsave("output/ms first round/plots/combined_jitterbox.tiff", units = "in", width = 18,
#        height = 6, dpi =  600, compression = "lzw")

###########################################################################

### saving four-panel figure
# #Plot of biomass, N supply, and P supply, and size structure
ggarrange(a,d,b,c,
          labels = c('a)', 'b)', 'c)', 'd)',
          ncol = 2, vjust = 1, align = "hv"))

#saving for publication - the meat of the figure
# ggsave("output/ms first round/plots/combined_jitterbox_fourpanel.tiff", units = "in", width = 12,
#        height = 10, dpi =  600, compression = "lzw")

###########################################################################

### saving two-panel figure
# #Plot of biomass and N supply
ggarrange(a,b,
          labels = c('a)', 'b)',
          ncol = 2, vjust = 1, align = "hv"))

#saving two=panel figure
ggsave("output/ms first round/plots/combined_jitterbox_twopanel.tiff", units = "in", width = 16,
       height = 10, dpi =  600, compression = "lzw")

###########################################################################
# additional plotting code for reference ----------------------------------
###########################################################################

# dt1 |> 
#   rename(Community = vert,
#          Ecosystem = ecosystem_2) |> 
#   group_by(axis_name_5, Ecosystem, Community, site, year) |> 
#   summarize(mean_bm = mean(total_biomass)) |> 
#   ggplot(aes(y = axis_name_5, x = log1p(mean_bm), fill = Ecosystem)) +
#   geom_jitter(aes(color = Community), position = position_jitter(width = 0.2), size = 1.0) +
#   geom_boxplot(outlier.shape = NA,  alpha = 0.75) +
#   scale_fill_manual(values = ecosystem_colors) + # Apply the color palette
#   scale_color_manual(values = vert_colors) + # Apply the color palette for jitter
#   labs(title = "Total Biomass", x = "log+1(g dry biomass/per unit area)", y = "Project") +
#   theme_classic() +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.title.x = element_text(face = "italic", size = 16),
#         axis.title.y = element_blank(),
#         axis.line = element_line("black"),
#         axis.text.x = element_text(color = "black", face = "bold", size = 18),
#         # axis.text.y = element_blank(),
#         axis.text.y = element_text(color = "black", face = "bold", size = 18),
#         plot.title = element_text(color = "black", face = "bold", size = 18, hjust = 0.5),
#         legend.position = "right",
#         legend.spacing.x = unit(0.25, "cm"), # Horizontal spacing between legend items
#         legend.spacing.y = unit(0.25, "cm"), # Vertical spacing between legend items
#         legend.text = element_text(color = "black", face = "bold", size = 12), # Font size of legend text
#         legend.title = element_text(color = "black", face = "bold", size = 14)) # Font size and style of legend title)

# #saving for publication - the y axis title + legend
# ggsave("output/ms first round/plots/combined_jitterbox_ADD.tiff", units = "in", width = 6,
#        height = 6, dpi =  600, compression = "lzw")

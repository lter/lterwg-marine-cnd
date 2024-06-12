librarian::shelf(tidyverse, readxl, glmmTMB, MuMIn, sjPlot, ggpmisc, corrplot, performance, ggeffects, ggpubr, parameters)

exc <- read_csv("local_data/model_data_all.csv") #all sites, no 10 year cutoff
sc <- read_csv("local_data/site_characteristics.csv")
add <- read_csv("local_data/timeseries_axis_titles.csv")

dt <- left_join(exc, sc) |> 
  select(project, ecosystem, ocean, climate, latitude, site, year, month, vert, everything()) |> 
  select(-n_spp)

dt1 <- left_join(dt, add)

na_count_per_column <- sapply(dt1, function(x) sum(is.na(x)))
print(na_count_per_column) 

comm <- read_csv("local_data/community_data_filtered.csv")

test <- dt1 |> 
  filter(project %in% c("FCE", "MCR", "PISCO-South", "PISCO-Central",
                        "SBC-Ocean", "PIE", "VCR")) |> 
  filter(vert == "vertebrate")

test2 <- left_join(comm, test)

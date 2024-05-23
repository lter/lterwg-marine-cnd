# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, readxl, taxize, stringr)

aLTER_SKQF20_CVQ_150_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQF20_CVQ_150_V4_BioRespExcre.csv")
bLTER_SKQF20_MNT_500_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQF20_MNT_500_V4_BioRespExcre.csv")
cLTER_SKQS20_CVQ_150_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQS20_CVQ_150_V4_BioRespExcre.csv")
dLTER_SKQS20_MNT_500_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQS20_MNT_500_V4_BioRespExcre.csv")
eLTER_SKQS21_CVQ_150_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQS21_CVQ_150_V4_BioRespExcre.csv")
fLTER_SKQS21_MNT_500_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_SKQS21_MNT_500_V4_BioRespExcre.csv")
gLTER_TGXF19_CVQ_150_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_TGXF19_CVQ_150_V4_BioRespExcre.csv")
hLTER_TGXF19_MNT_500_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_TGXF19_MNT_500_V4_BioRespExcre.csv")
iLTER_TGXF21_CVQ_150_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_TGXF21_CVQ_150_V4_BioRespExcre.csv")
jLTER_TGXF21_MNT_500_V4_BioRespExcre <- read_csv("data/other/NGA_2024/LTER_TGXF21_MNT_500_V4_BioRespExcre.csv")
kLTER_TGXS18_CVQ_150_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXS18_CVQ_150__V4_BioResp.csv")
lLTER_TGXS18_MNT_500_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXS18_MNT_500__V4_BioResp.csv")
mLTER_TGXS19_CVQ_150_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXS19_CVQ_150_V4_BioResp.csv")
nLTER_TGXS19_MNT_500_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXS19_MNT_500_V4_BioResp.csv")
oLTER_TGXF18_CVQ_150_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXF18_CVQ_150_V4_BioResp.csv")
pLTER_TGXF18_MNT_500_V4_BioResp <- read_csv("data/other/NGA_2024/LTER_TGXF18_MNT_500_V4_BioResp.csv")

# create list of data frames ----------------------------------------------

# Set the path to the folder containing CSV files
folder_path <- "data/other/NGA_2024"

# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store data frames
data_frames <- list()

# Loop through each CSV file and read it into a data frame
for (file in csv_files) {
  data <- read.csv(file)
  data_frames[[file]] <- data
}

# check to see if all data frames have identical column names -------------

if(all(sapply(data_frames[-1], function(df) identical(colnames(data_frames[[1]]), colnames(df))))) {
  print("Column names match across all data frames.")
} else {
  print("Column names do not match across all data frames.")
}

### column names do not match


# bind together rows ------------------------------------------------------

library(dplyr)
combined_df <- bind_rows(data_frames) #of observations equal that of all other ind sets combined

glimpse(combined_df)

dat <- combined_df |> 
  janitor::clean_names()
glimpse(dat)

write_csv(dat, "data/tier0/nga_combined_clean.csv")

column_names <- colnames(dat)
column_names_df <- data.frame(ColumnNames = column_names)
write_csv(column_names_df, "data/tier0/nga_combined_clean_column_names.csv")

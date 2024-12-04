## ------------------------------------------ ##
# Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step2_..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier0"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data", "environmental"), showWarnings = F)

## ------------------------------------------ ##
# Download (Before Script) ----
## ------------------------------------------ ##

# Identify raw data files
raw_ids <- dplyr::bind_rows(
  ## SBC
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")),
  ## FCE
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ei4UmhiU87Mu7mUHdB0ByVW32UWxvVjW")),
  ## CCE
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/19INhcRd1xBKgDVd1G5W1B3QI4mlBr587"))
) %>% 
  # Filter to only desired files
  dplyr::filter(name %in% c("Bottom_temp_all_years_20230724.csv", # SBC
                            "LTER_monthly_bottledata_20220930.txt", # SBC
                            "bottle_creek_temperature.csv", # FCE
                            "mo215_elevation_corrected_water_levels.csv", # FCE
                            "fce_water_quality.csv", # FCE
                            "BEUTI_monthly.csv", # CCE
                            "sdsla_monthly.csv", # CCE
                            "mei.csv", # CCE
                            "cce_temperature_raw.csv" # CCE
  ))

# Check it
raw_ids

# Download all of these files
purrr::walk2(.x = raw_ids$id, .y = raw_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier0", "raw_data", "environmental", .y)))

# Identify and download the data key
googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name == "CND_Data_Key_spatial.xlsx") %>%
  googledrive::drive_download(file = .$id, path = file.path("tier0",.$name), overwrite = T)

# Clear environment
rm(list = c("raw_ids"))

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized dataset to Drive
googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          name = "harmonized_environment.csv",
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

googledrive::drive_upload(media = file.path("tier1", temperature_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

googledrive::drive_upload(media = file.path("tier1", temperature_filename), overwrite = T,
                          name = "temperature_allsites.csv",
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))


# End ----

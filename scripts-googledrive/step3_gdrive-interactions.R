## ------------------------------------------ ##
# Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step3_..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier1"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)

## ------------------------------------------ ##
# Download (Before Script) ----
## ------------------------------------------ ##

# pull in the harmonized data
consumer_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")) %>%
  dplyr::filter(name %in% c("harmonized_consumer.csv"))

env_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")) %>%
  dplyr::filter(name %in% c("temperature_allsites.csv"))

species_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) %>%
  dplyr::filter(name %in% c("CNDWG_harmonized_consumer_species.xlsx"))

# Combine file IDs
harmonized_ids <- rbind(consumer_ids, env_ids, species_ids)

# Download the 'other' files into a different folder
purrr::walk2(.x = harmonized_ids$id, .y = harmonized_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier1", .y)))

# Download Coastal CA site table
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name == "master_site_table.xlsx") %>% 
  googledrive::drive_download(file = .$id, overwrite = T, path = file.path("other", .$name))

# Download "dm conversion"
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a")) %>%
  dplyr::filter(name == "dm_conversions_cndwg.xlsx") %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("other", .$name))

# Download another dm conversion (for NGA this time)
ng_dm_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1j8QGQR6_vD1SQnFwVnaAy0W-1c_owCRv")) %>%
  dplyr::filter(name == "Group mesh conversion.csv") %>% 
  googledrive::drive_download(file = .$id, overwrite = T, 
                              path = file.path("other", .$name))

# Clear these objects from the environment
rm(list = ls())

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))




# End ----

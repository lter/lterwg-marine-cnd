## ------------------------------------------ ##
# Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step3pt5_..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier1"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)

## ------------------------------------------ ##
# Download (Before Script) ----
## ------------------------------------------ ##

# Identify harmonized data
harmonized_ids <- dplyr::bind_rows(
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")),
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4"))
  ) %>%
  # Filter to only needed files
  dplyr::filter(name %in% c("harmonized_consumer_ready_for_excretion.csv",
                            "CNDWG_harmonized_consumer_species.xlsx",
                            "temperature_allsites.csv"))

# Check it
harmonized_ids

# Download them locally
purrr::walk2(.x = harmonized_ids$id, .y = harmonized_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier1", .y)))

# Download the Coastal CA site table
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")) %>%
  dplyr::filter(name == "master_site_table.xlsx") %>% 
  googledrive::drive_download(file = .$id, overwrite = T, 
                              path = file.path("other", .$name))

# Download "dm conversion"
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a")) %>%
  dplyr::filter(name == "dm_conversions_cndwg.xlsx") %>%
  googledrive::drive_download(file = .$id, overwrite = T, 
                              path = file.path("other", .$name))

# Download "dm conversion" for NGA this time
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1j8QGQR6_vD1SQnFwVnaAy0W-1c_owCRv")) %>%
  dplyr::filter(name == "Group mesh conversion.csv") %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("other", .$name))

# Clear these objects from the environment
rm(list = setdiff(x = ls(), y = c("harmonized_ids")))

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# End ----

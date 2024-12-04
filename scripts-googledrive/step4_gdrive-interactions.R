## ------------------------------------------ ##
# Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step4..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier2"), showWarnings = F)
dir.create(path = file.path("other"), showWarnings = F)

## ------------------------------------------ ##
# Download (Before Script) ----
## ------------------------------------------ ##

# Pull in the harmonized data
harmonized_ids <- dplyr::bind_rows(
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH")),
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1CEgNtAnk4DuPNpR3lJN9IqpjWq0cM8F4")) ) %>%
  # Filter to only needed files
  dplyr::filter(name %in% c("harmonized_consumer_ready_for_excretion.csv",
                            "CNDWG_harmonized_consumer_species.xlsx"))

# Download the 'other' files into a different folder
purrr::walk2(.x = harmonized_ids$id, .y = harmonized_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier1", .y)))

# Clear these objects from the environment
rm(list = setdiff(x = ls(), y = c("harmonized_ids")))

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media = file.path("tier2", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))

# End ----

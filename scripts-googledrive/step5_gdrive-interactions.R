## ------------------------------------------ ##
# Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step5_..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier2"), showWarnings = F)

## ------------------------------------------ ##
# Download (Before Script) ----
## ------------------------------------------ ##

# Get harmonized consumer excretion data
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX")) %>% 
  dplyr::filter(name == "harmonized_consumer_excretion.csv") %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("tier2", .$name))

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized clean dataset to Drive
googledrive::drive_upload(media = file.path("tier2", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1VakpcnFVckAYNggv_zNyfDRfkcGTjZxX"))

# End ----

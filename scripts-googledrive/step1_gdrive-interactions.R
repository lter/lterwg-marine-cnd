## ------------------------------------------ ##
  # Marine CND -- Google Drive Interaction
## ------------------------------------------ ##
# Script author(s): Nick Lyon

# Purpose:
## Does all Google Drive interactions related to the "step1_..." harmonization script

# Create necessary sub-folder(s)
dir.create(path = file.path("tier0"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data"), showWarnings = F)
dir.create(path = file.path("tier0", "raw_data", "consumer"), showWarnings = F)
dir.create(path = file.path("tier1"), showWarnings = F)

## ------------------------------------------ ##
      # Download (Before Script) ----
## ------------------------------------------ ##

# Identify raw files
raw_ids <- dplyr::bind_rows(
  ### SBC
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1ycKkpiURLVclobAdCmZx2s_ewcaFAV9Y")),
  ### FCE 
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1BSQSXEbjgkSBJVN0p9CxhjVmfiv82U1t")),
  ### VCR
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1PoVGjZoE_Dlr93dt45LRp4P2Jjuez94l")),
  ## Coastal
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1vT-u9EFsssA8t9_y1A163BTr6ENGBelC")),
  ## MCR
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1bMVr5VSXD2azlwD9DioeMwy4RR94uqF5")),
  ## PIE
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1yAoT0RtkRf2gxtBl3MXpi6cuGji5kuEE")),
  ## CCE
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/19INhcRd1xBKgDVd1G5W1B3QI4mlBr587")),
  ## NGA
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1j8QGQR6_vD1SQnFwVnaAy0W-1c_owCRv"))
) %>% 
  ## Filter to only desired file names
  dplyr::filter(name %in% c("Annual_All_Species_Biomass_at_transect_20240307.csv", # SBC
                            "IV_EC_talitrid_population_v3.csv",  # SBC
                            "map_revised02052024.csv", # FCE
                            "VCR14232_2.csv", # VCR
                            "MLPA_fish_biomass_density_transect_raw_v2.csv", # Coastal
                            "MLPA_benthic_site_means.csv", # Coastal
                            "MCR_LTER_Annual_Fish_Survey_20230615.csv", # MCR
                            "LTE-TIDE-NektonFlumeDensity_v5_1.csv", # PIE
                            "LTE-TIDE-NektonFlumeIndividual_v6_3.csv", # PIE
                            "cce_wdrymass.csv", # CCE
                            "Excretion_update_1997-2021.csv" # NGA
  ) )

# Check that looks right
raw_ids

# Identify data key
datakey_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-FDBq0jtEm3bJOfiyIkyxD0JftJ6qExe")) %>%
  dplyr::filter(name %in% c("CND_Data_Key_spatial.xlsx"))

# Check it
datakey_id

# Get taxonomic information too
taxoninfo_ids <- dplyr::bind_rows(
  ## PIE / CoastalCA species code tables
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a")),
  ## MCR species fix table
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1LYffjtQdLcNYkStrf_FukihQ6tPKlw1a"))
) %>% 
  dplyr::filter(name %in% c("PIE_CoastalCA_codes.csv", "MCR_species_fix")) %>% 
  dplyr::distinct()

# Check it
taxoninfo_ids

# Combine non-data file IDs
other_ids <- rbind(datakey_id, taxoninfo_ids)

# Download the raw data
purrr::walk2(.x = raw_ids$id, .y = raw_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier0", "raw_data", "consumer", .y)))

# Download the 'other' files into a different folder
purrr::walk2(.x = other_ids$id, .y = other_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tier0", .y)))

# Clear these objects from the environment
rm(list = c("datakey_id", "other_ids", "raw_ids", "taxoninfo_ids"))

## ------------------------------------------ ##
# Upload (After Script) ----
## ------------------------------------------ ##

# Export harmonized dataset to Drive
googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1A-DCrOlyq6IZKvN9d3OHFXj_XFJ5L7HN"))

# Export harmonized dataset to Drive under the general dataset name
googledrive::drive_upload(media = file.path("tier1", tidy_filename), overwrite = T,
                          name = "harmonized_consumer.csv",
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))

# If you've specified that you want to upload the tidy data...
if (species_update_flag == 1){
  
  # Generate a date-stamped file name for the species table
  ( species_filename <- paste0("harmonized_consumer_species_", date, ".csv") )
  
  # Also export species table
  write.csv(x = species_table, file = file.path("tier1", species_filename), na = '.', row.names = F)
  
  # Export species table to Drive
  googledrive::drive_upload(media = file.path("tier1", species_filename), overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1A-DCrOlyq6IZKvN9d3OHFXj_XFJ5L7HN"))
  
  # Export species table to Drive under the general dataset name
  googledrive::drive_upload(media = file.path("tier1", species_filename), overwrite = T,
                            name = "harmonized_consumer_species.csv",
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1iw3JIgFN9AuINyJD98LBNeIMeHCBo8jH"))
  
}
# End ----

## Development Version

## `version 2.0.0`

Changes from the preceding version will be listed as they are made.

- All interactions with Google Drive separated into dedicated scripts (one for each harmonization script)
- Streamlined text of README and removed some superseded content
- Expanded the `.gitignore` with content from the [LTER working group template `gitignore`](https://github.com/lter/lterwg-template/blob/main/.gitignore)

- PISCO wrangling was revised in step 3 to generate estimates of biomass and density, important for nutrient supply calculations. Data was previously being calculated at transect level (i.e., per 60 m2) instead of per m2 (which is resolution of other visual survey data)
- FCE hyrologic year correction was revised in step 4 to account for fact it was corrected for when initially submitted. Now all FCE data should be corrected for sampling season. This is important since monthly data collected during a hydrologic (i.e., dry) season
- Outlier and NA handling revised to be more stringent for CCE and PIE (e.g., should no longer be shrimp greater than 1 lb)


## `version 1.0.0`

Title: "Initial Release of LTER Synthesis Working Group: Consumer-Mediated Nutrient Dynamics Project, v1.0.0"

We are excited to announce the initial official release of the "LTER Synthesis Working Group -- Consumer-Mediated Nutrient Dynamics of Marine Ecosystems Under the Wake of Global Change" project, v1.0.0! This release marks the start of our journey to integrate the projects' datasets.

Primary Folder Structure & Contents include:
scripts-harmonization: This folder contains code designed to wrangle, harmonize, and clean datasets from various long-term research programs. The scripts are organized into five sequential steps, executed in order from step 1 to step 5.

A huge thank you to everyone who has contributed to this release.

If you have any questions, encounter any issues, or want to contribute, please visit our [issue tracker](https://github.com/lter/lterwg-marine-cnd/issues).

The Project Team

# LTER Synthesis working group -- Consumer-mediated nutrient dynamics of marine ecosystems under the wake of global change


## PIs: 

- Mack White, Graduate Student, FIU
- Joseph Peters, Graduate Student, UCSB
- Bradley Strickland, Postdoctoral Research Associate, VIMS
- Deron Burkepile, Professor, FIU
- Jennifer Rehage, Associate Professor, FIU


## Project Summary

- https://lternet.edu/working-groups/marine-consumer-nutrient-dynamics/


## Guidelines for sharing scripts

- All code should include sufficient annotation and documentation so that other users can understand and run the scripts 
- Use RStudio projects to manage working directory
- Write your scripts such that other users can run the code without modifications. Keep file paths and other variables that might need to be changed at the beginning of the script, just after attaching the necessary libraries
- Check out the Scientific Computing Team’s best practice tips for [storing file paths](https://nceas.github.io/scicomp.github.io/best_practices.html#file-paths)
- Include an attribution header to your scripts or Rmarkdown documents

Example:

```r
## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: 
##
## Email: 
##
## ---------------------------

library(tidyverse)

```


## Supplementary Resources

NCEAS Scientific Computing Support Team page [link](https://nceas.github.io/scicomp.github.io)

## Folder Structure & Contents

scripts-harmonization: This folder contains code designed to wrangle, harmonize, and clean datasets from various long-term research programs. The scripts are organized into five sequential steps, executed in order from step 1 to step 5. 

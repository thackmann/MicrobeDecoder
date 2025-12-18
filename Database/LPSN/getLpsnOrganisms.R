# Get LPSN Organisms for App
# This script gets names of organisms from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
# - Data from getLpsnNonCyanobacteria.R script
# - Data from getLpsnCyanobacteria.R script
# Author: Timothy Hackmann
# Date: 3 December 2025

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()
  subdirectory <- "/LPSN"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)
  source("LPSN\\functions.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)

  # From https://lpsn.dsmz.de/downloads
  lpsn_non_cyanobacteria <- read.csv("LPSN\\data\\lpsn_non_cyanobacteria.csv")
  lpsn_cyanobacteria <- read.csv("LPSN\\data\\lpsn_cyanobacteria.csv")
  
# === Format data ===
  # Combine data for non-cyanobacteria and cyanobacteria
  lpsn_organisms <- dplyr::bind_rows(
    lpsn_non_cyanobacteria,
    lpsn_cyanobacteria
  ) %>%
  dplyr::distinct(LPSN_ID, .keep_all = TRUE)
  
# === Export ===
  write.csv(lpsn_organisms, paste0(database_directory, "\\LPSN\\data\\lpsn_organisms.csv"), row.names = FALSE)
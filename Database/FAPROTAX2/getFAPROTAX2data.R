# Get Data from FAPROTAX2
# This script formats data from the FAPROTAX2 database for the app
# It concatenates traits from FAPROTAX2 into a single column and formats the LPSN ID
# It is not called during app execution
# Requirements:
# - Packages in install/installPackages.R
# - Data from FAPROTAX (http://www.loucalab.com/archive/FAPROTAX2/lib/php/index.php?section=Download)
# Author: Timothy Hackmann
# Date: 29 Jun 2026

# === Get database directory ===
  database_directory <- this.path::this.dir()
  subdirectory <- "/FAPROTAX2"
  database_directory <- gsub(paste0(subdirectory, "$"), "", database_directory)
  
# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE)

# === Read in data ===
  setwd(database_directory)
  
  # From http://www.loucalab.com/archive/FAPROTAX2/lib/php/index.php?section=Download
  FAPROTAX2_data <- readr::read_tsv(
    file = "FAPROTAX2\\data\\FAPROTAX2-db_main.zip",
    comment = "#",
    col_types = readr::cols(.default = readr::col_character())
  )
  
# === Format data  ===
  # Concatenate traits
  value_cols <- names(FAPROTAX2_data)[grepl("\\.value$", names(FAPROTAX2_data))]
  
  FAPROTAX2_data <- FAPROTAX2_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      FAPROTAX2_Type_of_metabolism = {
        values <- c(dplyr::pick(dplyr::all_of(value_cols)))
        
        traits <- value_cols[which(values == "P")]
        traits <- sub("\\.value$", "", traits)
        
        if (length(traits) == 0) {
          NA_character_
        } else {
          paste(traits, collapse = ";")
        }
      }
    ) |>
    dplyr::ungroup()

  # Format LPSN ID
  FAPROTAX2_data <- FAPROTAX2_data |>
    dplyr::mutate(
      LPSN_accession = stringr::str_remove(LPSN_accession, "^LPSN")
    )
  
  # Select relevant columns
  FAPROTAX2_data <- FAPROTAX2_data |>
    dplyr::select(LPSN_accession, FAPROTAX2_Type_of_metabolism) |>
    dplyr::rename(LPSN_ID = LPSN_accession)
  
# === Export  ===
  setwd(database_directory)
  write.csv(FAPROTAX2_data, file = "FAPROTAX2\\data\\FAPROTAX2_data.csv")

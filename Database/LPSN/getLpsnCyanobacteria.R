# Get Cyanobacteria for App
# This script gets names of cyanobacteria from the LPSN database.
# Requirements:
# - Packages in install/installPackages.R
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

# === Retrieve data ===
taxon_url <- "https://lpsn.dsmz.de/phylum/cyanobacteriota"

lpsn_data <- get_information_on_child_taxa(
  taxon_url = taxon_url
  # taxon_url = taxon_url,
  # max_taxa = 1000
)

# === Format data ===
  # Select organisms with correct name
  df <- lpsn_data %>%
    dplyr::filter(
      grepl("correct name", status, ignore.case = TRUE) &
        !grepl("not correct name", status, ignore.case = TRUE)
    ) %>%
    dplyr::filter(sp_epithet != "")
  
  # Rename columns
  # df = df %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species, subsp_epithet, nomenclatural_type, status, record_no, address) # debug
  df = df %>% dplyr::select(Genus, Species, subsp_epithet, nomenclatural_type, status, record_no, address)
  df = df %>% dplyr::rename(Subspecies = "subsp_epithet", Strain = nomenclatural_type, Status = status, LPSN_ID = record_no)
  
  # Replace blank values with NA
  df = df %>% dplyr::mutate_all(~ifelse(. == "", NA, .))
  
  # Keep only entries with both genus and species specified
  df <- df %>% dplyr::filter(!is.na(Genus)) %>% dplyr::filter(!is.na(Species))
  
  # For subspecies, keep only entries that have subspecies specified (e.g., keep Selenomonas ruminantium lactilytica but not Selenomonas ruminantium)
  df <- df %>% dplyr::group_by(Genus, Species) %>% dplyr::filter(!(dplyr::n_distinct(Subspecies) > 1 & Subspecies == "")) %>% dplyr::ungroup()

# === Export ===
write.csv(df, paste0(database_directory, "\\LPSN\\data\\lpsn_cyanobacteria.csv"), row.names = FALSE)

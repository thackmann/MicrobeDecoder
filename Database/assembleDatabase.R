# Assemble Database
# This script assembles the database for the app
# It draws on data from LPSN, Bergey's Manual, BacDive, NCBI, GOLD, and IMG.
# It is not called during app execution.
# Requirements:
# - Packages in install/installPackages.R
# - Data from LPSN from LPSN/getLpsnOrganisms.R script
# - Additional data from LPSN from LPSN/getLpsnPhylogeny.R script
# - Additional data from LPSN from LPSN/getLpsnRibosomalSequences.R script
# - Data downloaded from BacDive at links below
# - Data from Bergey's Manual from Bergey/addBergeyLabels.R script
# - Data downloaded from NCBI at links below
# - Data downloaded from GOLD at links below
# - Data downloaded from IMG following instructions below
# - Data from FAPROTAX from FAPROTAX/getFAPROTAXpredictions.R script
# - Data from primary literature
# - Data from VPI Anaerobe Manual
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Get database directory ===
  database_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(database_directory)
  source("functions\\helperFunctions.R", local = TRUE) 
  
# === Read in data ===
  setwd(database_directory)

  # Read in data from LPSN
    # From LPSN/getLpsnPhylogeny.R script
    lpsn_organisms =  readr::read_csv("LPSN\\data\\lpsn_organisms.csv")
    # From LPSN/getLpsnPhylogeny.R script
    lpsn_phylogeny =  readr::read_csv("LPSN\\data\\lpsn_phylogeny.csv")
    # From getLpsnRibosomalSequencs.R script
    lpsn_ribosomal_sequences =  readr::read_csv("LPSN\\data\\lpsn_ribosomal_sequences.csv")

  # Read in data from GTDB
    # From https://data.gtdb.ecogenomic.org/releases/latest/
    gtdb_bacteria_data =  readr::read_tsv("GTDB\\data\\bac120_metadata.tsv.gz")
    gtdb_archaea_data =  readr::read_tsv("GTDB\\data\\ar53_metadata.tsv.gz")
    
  # Read in data from GOLD
    # From https://gold.jgi.doe.gov/downloads
    # After downloading, open main *.xlsx file and resave tabs as *csv with names below
    GOLD_organism_data <- readr::read_csv("GOLD\\data\\goldDataOrganism.csv",
                                          locale = readr::locale(encoding = "ISO-8859-1"),
                                          na = "",
                                          guess_max = 1000000,
                                          name_repair = "minimal")

    GOLD_sequencing_data <- readr::read_csv("GOLD\\data\\goldDataSequencing.csv",
                                            locale = readr::locale(encoding = "ISO-8859-1"),
                                            na = "",
                                            guess_max = 1000000,
                                            name_repair = "minimal")

  # Read in data from IMG
    # From https://img.jgi.doe.gov/ (follow instructions below to download)
    IMG_data <- openxlsx::read.xlsx("IMG\\data\\IMG.xlsx")

  # Read in data from NCBI
    # From https://ftp.ncbi.nih.gov/pub/taxonomy/taxdmp.zip
    NCBI_nodes <- load_nodes_dmp(file_path = "NCBI\\data\\nodes.dmp")
    NCBI_names <- load_names_dmp(file_path = "NCBI\\data\\names.dmp")

  # Read in data from BacDive
    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1
    BacDive_data <- readr::read_csv("BacDive\\data\\advsearch_bacdive_2024-11-07.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Antibiotic+resistance&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=met_antibiotica-ab_resistant-4&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Metabolite+%28antibiotic%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_antibiotica-metabolite_antib-4
    Antibiotic_resistance <- readr::read_csv("BacDive\\data\\antibiotic_resistance.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Antibiotic+sensitivity&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=met_antibiotica-ab_sensitive-4&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Metabolite+%28antibiotic%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_antibiotica-metabolite_antib-4
    Antibiotic_sensitivity <- readr::read_csv("BacDive\\data\\antibiotic_sensitivity.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Cell+length&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=cell_morphology-cell_len-2
    Cell_length <- readr::read_csv("BacDive\\data\\cell_length.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Cell+shape&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-cell_shape-2
    Cell_shape <- readr::read_csv("BacDive\\data\\cell_shape.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Cell+width&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-cell_width-2
    Cell_width <- readr::read_csv("BacDive\\data\\cell_width.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Colony+size&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=colony_morphology-colony_len-2
    Colony_size <- readr::read_csv("BacDive\\data\\colony_size.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Enzyme+activity&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=%2B&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=enzymes-activity-4&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Enzyme&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=enzymes-enzyme-4
    Enzyme_activity <- readr::read_csv("BacDive\\data\\enzyme_activity.csv")
    
    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Flagellum+arrangement&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-flagellum_arrangement-2
    Flagellum_arrangement <- readr::read_csv("BacDive\\data\\flagellum_arrangement.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Gram+stain&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=cell_morphology-gram_stain-2
    Gram_stain <- readr::read_csv("BacDive\\data\\gram_stain.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Incubation+period+%28in+days%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=colony_morphology-incubation_period-2
    Incubation_period <- readr::read_csv("BacDive\\data\\incubation_period.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Indole+test+%28Indole%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=met_test-indole_test-4
    Indole_test <- readr::read_csv("BacDive\\data\\indole_test.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Utilization+activity&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=%2B&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=met_util-ability-4&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Metabolite+%28production%29&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=met_production-metabolite_prod-4
    Metabolite_production <- readr::read_csv("BacDive\\data\\metabolite_production.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B1%5D%5Bgc%5D=OR&fg%5B1%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B1%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B1%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B1%5D%5Bfl%5D%5B2%5D=AND&fg%5B1%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Utilization+activity&fg%5B1%5D%5Bfl%5D%5B3%5D%5Bfv%5D=%2B&fg%5B1%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=met_util-ability-4&fg%5B1%5D%5Bfl%5D%5B4%5D=AND&fg%5B1%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Metabolite+%28utilization%29&fg%5B1%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B1%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B1%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=met_util-metabolite_util-4
    Metabolite_utilization <- readr::read_csv("BacDive\\data\\metabolite_utilization.csv")
    
    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Motility&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=cell_morphology-motility-2
    Motility <- readr::read_csv("BacDive\\data\\motility.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Oxygen+tolerance&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=oxygen_tolerance-oxygen_tol-4
    Oxygen_tolerance <- readr::read_csv("BacDive\\data\\oxygen_tolerance.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Kind+of+pH&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=growth&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=culture_pH-test_type-3&fg%5B0%5D%5Bfl%5D%5B10%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Testresult+%28pH%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=positive&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=culture_pH-ability-3&fg%5B0%5D%5Bfl%5D%5B12%5D=AND&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfd%5D=pH&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfvd%5D=culture_pH-pH-3
    pH_for_growth <- readr::read_csv("BacDive\\data\\pH_for_growth.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28animal%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_animal-6
    Pathogenicity_animal <- readr::read_csv("BacDive\\data\\pathogenicity_animal.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28human%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_human-6
    Pathogenicity_human <- readr::read_csv("BacDive\\data\\pathogenicity_human.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Pathogenicity+%28plant%29&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=risk_assessment-pathogenicity_plant-6
    Pathogenicity_plant <- readr::read_csv("BacDive\\data\\pathogenicity_plant.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Salt+conc.&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=halophily-salt_concentration-4
    Salt_concentration <- readr::read_csv("BacDive\\data\\salt_concentration.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=salt+concentration+unit&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=halophily-salt_concentration_unit-4
    Salt_concentration_unit <- readr::read_csv("BacDive\\data\\salt_concentration_unit.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B5%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B6%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Ability+of+spore+formation&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=spore_formation-ability-4
    Spore_formation <- readr::read_csv("BacDive\\data\\spore_formation.csv")

    # https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfd%5D=Kind+of+temperature&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfv%5D=growth&fg%5B0%5D%5Bfl%5D%5B7%5D%5Bfvd%5D=culture_temp-test_type-3&fg%5B0%5D%5Bfl%5D%5B8%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Testresult+%28temperature%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=positive&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=culture_temp-ability-3&fg%5B0%5D%5Bfl%5D%5B12%5D=AND&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfd%5D=Temperature&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfo%5D=equal&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B13%5D%5Bfvd%5D=culture_temp-temp-3
    Temperature_for_growth <- readr::read_csv("BacDive\\data\\temperature_for_growth.csv")

    # From https://bacdive.dsmz.de/advsearch?fg%5B0%5D%5Bgc%5D=OR&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfd%5D=Type+strain&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfv%5D=1&fg%5B0%5D%5Bfl%5D%5B1%5D%5Bfvd%5D=strains-is_type_strain-1&fg%5B0%5D%5Bfl%5D%5B2%5D=AND&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfd%5D=Genus&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B3%5D%5Bfvd%5D=strains-genus-1&fg%5B0%5D%5Bfl%5D%5B4%5D=AND&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfd%5D=Species+epithet&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfo%5D=contains&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B9%5D%5Bfvd%5D=strains-species_epithet-1&fg%5B0%5D%5Bfl%5D%5B10%5D=AND&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfd%5D=Voges-Proskauer-test+%28Acetoin%29&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfv%5D=*&fg%5B0%5D%5Bfl%5D%5B11%5D%5Bfvd%5D=met_test-voges_proskauer-4
    Voges_proskauer <- readr::read_csv("BacDive\\data\\voges_proskauer.csv")

    # From https://bacdive.dsmz.de/isolation-sources
    Isolation_sources <- readr::read_csv("BacDive\\data\\isolation_sources.csv")

  # Read in data from Bergey's Manual
    # From Bergey/addBergeyLabels.R script
    Bergey_data <- readr::read_csv("Bergey\\data\\Bergey_data_with_labels.csv")

  # Read in data from VPI Anaerobe Laboratory Manual
    VPI_data <- readr::read_csv("VPI\\data\\VPI_data.csv")

  # Read in data from primary literature
    primary_literature_data <- readr::read_csv("primary_literature\\data\\primary_literature_data.csv")

  # Read in data from FAPROTAX
    # From FAPROTAX/getFAPROTAXpredictions.R script
    FAPROTAX_data <- readr::read_csv("FAPROTAX\\data\\FAPROTAX_data.csv")

# === Start database using data from LPSN ===
  database = lpsn_organisms

  # Add phylogeny
  matches_LPSN = match(x = lpsn_phylogeny$LPSN_ID, table = database$LPSN_ID)

  # Use indices to add phylogeny to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = lpsn_phylogeny,
    target_index = matches_LPSN,
    source_index = seq_along(matches_LPSN),
    source_col_names = c(
      "Domain",
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus...1",
      "Species",
      "Subspecies",
      "Strain"
    ),
    target_col_names = c(
      "LPSN_Domain",
      "LPSN_Phylum",
      "LPSN_Class",
      "LPSN_Order",
      "LPSN_Family",
      "LPSN_Genus",
      "LPSN_Species",
      "LPSN_Subspecies",
      "LPSN_Strain"
    )
  )

  # Add 16S ribosomal sequence
  matches_LPSN = match(x = lpsn_ribosomal_sequences$LPSN_ID, table = database$LPSN_ID)

  # Use indices to add sequences to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = lpsn_ribosomal_sequences,
    target_index = matches_LPSN,
    source_index = seq_along(matches_LPSN),
    source_col_names = c(
      "16S_ribosomal_sequence"
    ),
    target_col_names = c(
      "LPSN_16S_Ribosomal_sequence"
    )
  )

  # Rename columns
  database <- database %>% dplyr::rename(LPSN_Page_link = address)
  database <- database %>% dplyr::select(-LPSN_Page_link,LPSN_Page_link)
  database <- database %>% dplyr::rename(LPSN_status = Status)
  database <- database %>% dplyr::select(-LPSN_status,LPSN_status)

# === Get names of culture collections ====
  # Names are used in matching strains in database to other sources of data
  collection_names = get_collection_names(strains = database$Strain, n = 32)

# === Add data from GTDB ===
  # Format GTDB data
  # Combine data for bacteria and archaea
  gtdb_data <- rbind(gtdb_bacteria_data, gtdb_archaea_data)
  
  # Keep only records for type strains, and select only essential columns
  gtdb_data <- gtdb_data %>%
    dplyr::filter((gtdb_type_designation_ncbi_taxa_sources == "LPSN")) %>%
    dplyr::select(accession, gtdb_taxonomy, ncbi_strain_identifiers)
  
  # Split taxonomy into ranks
  gtdb_data <- gtdb_data %>%
    dplyr::mutate(
      taxonomy_split = lapply(gtdb_taxonomy, split_taxonomy)
    ) %>%
    tidyr::unnest_wider(taxonomy_split)
  
  # Find matches between database and GOLD
  matches_GTDB = perform_matching(
    table_data = gtdb_data,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_strain_col = "ncbi_strain_identifiers",
    table_delim = ",",
    x_data = database,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )
  
  # Filter indices to keep only the best matches
  matches_filtered <- matches_GTDB %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  # Remove rank 7 and 8 matches (likely to contain non-type strains)
  matches_filtered <- matches_filtered %>% dplyr::filter(!Rank %in% c(7, 8))
  # Remove multiple matches
  matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  
  # Use indices to add GTDB data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = gtdb_data,
    target_index = matches_filtered$x_index, 
    source_index = matches_filtered$table_index,
    source_col_names = c(
      "Domain",
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus",
      "Species",
      "accession"
    ),
    target_col_names = c(
      "GTDB_Domain",
      "GTDB_Phylum",
      "GTDB_Class",
      "GTDB_Order",
      "GTDB_Family",
      "GTDB_Genus",
      "GTDB_Species",
      "GTDB_ID"
    )
  )

# === Add data from GOLD database ===
  # Format GOLD organism data
    # Select only bacteria and archaea
    GOLD_organism_data <- GOLD_organism_data %>%
      dplyr::filter(`ORGANISM NCBI SUPERKINGDOM` == "Bacteria" | `ORGANISM NCBI SUPERKINGDOM` == "Archaea")
    # Format genus names
    GOLD_organism_data$`ORGANISM SPECIES` <- stringr::str_remove(GOLD_organism_data$`ORGANISM SPECIES`, "^\\S+\\s+")

    # Add sequencing project
    GOLD_sequencing_data <- GOLD_sequencing_data %>% dplyr::select(`ORGANISM GOLD ID`, `PROJECT GOLD ID`) %>% dplyr::group_by(`ORGANISM GOLD ID`) %>%
      dplyr::summarize(`PROJECT GOLD ID` = paste(`PROJECT GOLD ID`, collapse = ", "))
    GOLD_organism_data <- dplyr::left_join(x = GOLD_organism_data, y = GOLD_sequencing_data, by = "ORGANISM GOLD ID")

  # Find matches between database and GOLD
  matches_GOLD = perform_matching(
    table_data = GOLD_organism_data,
    table_genus_col = "ORGANISM GENUS",
    table_species_col = "ORGANISM SPECIES",
    table_strain_col = "ORGANISM STRAIN",
    table_delim = ",",
    x_data = database,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
    matches_filtered =  matches_GOLD
    matches_filtered$Project = GOLD_organism_data$'PROJECT GOLD ID'[matches_GOLD$table_index]
    # Remove rank 7 and 8 matches (likely to contain non-type strains)
    matches_filtered <- matches_filtered %>% dplyr::filter(!Rank %in% c(7, 8))
    
    # Pick the highest rank with a sequencing project
    matches_filtered <- matches_filtered %>%
      dplyr::group_by(x_index) %>%
      dplyr::arrange(Rank) %>%
      dplyr::mutate(Project_NA = all(is.na(Project))) %>%
      dplyr::filter(ifelse(Project_NA, Rank == min(Rank), Rank == min(Rank[!is.na(Project)], na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-Project_NA)  # Remove the helper column

  # Use indices to add GOLD data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = GOLD_organism_data,
      target_index = matches_filtered$x_index,
      source_index = matches_filtered$table_index,
      source_col_names = c(
        "ORGANISM GOLD ID",
        "PROJECT GOLD ID",
        "ORGANISM NCBI TAX ID"
      ),
      target_col_names = c(
        "GOLD_Organism_ID",
        "GOLD_Project_ID",
        "NCBI_Taxonomy_ID"
      )
    )

# === Add data from IMG ===
  # Instructions for downloading data (IMG.xlsx) from IMG
    ## Get GOLD organism IDs from database and output to file using command below (uncomment to run)
    # write_project_ids_to_files(project_IDs = database$'GOLD_Project_ID', batch_size = 500, file_prefix = "project_IDs_batch")
    ## Then navigate to IMG, log on, and then navigate to Find Genomes (https://img.jgi.doe.gov/cgi-bin/mer/main.cgi?section=GenomeSearch&page=searchForm)
    ## Paste contents of each file outputted above (one at a time in search bar).  In "Search by ID (list)" field, choose "GOLD Sequencing Project ID".  Click "Search".
    ## In the screen that appears, click "Select All" and "Add Selected to Genome Cart".  Repeat for remaining files.
    ## In the Genome Cart screen that appears, click "GOLD Sequencing Project ID", "CheckM2 Completeness", and "CheckM2 Contamination".
    ## Then click  "Redisplay" below the table.
    ## Then click the check box in the left corner (to select all genomes), then click "Export".
    ## Save as "IMG.xlsx" in the IMG/data folder.

    # Get IMG genome IDs
    x <- IMG_data$`GOLD.Sequencing.Project.ID`
    table <- database$`GOLD_Project_ID`

    # Find matches
    matches_IMG <- find_match_indices(x, table)
    IMG_data$match_indices <- matches_IMG

    # Add IMG genome IDs to database
    database$`IMG_Genome_ID` <- NA
    database$`IMG_Genome_ID` <- update_database_column(
      database_col = database$`IMG_Genome_ID`,
      match_indices = IMG_data$match_indices,
      values = IMG_data$`IMG.Genome.ID`
    )

    # Get IMG genome ID for genome with max quality score
    IMG_data$CheckM2.Contamination <- as.numeric(IMG_data$CheckM2.Contamination)
    IMG_data$Quality_score <- IMG_data$`CheckM2.Completeness` - 5*IMG_data$`CheckM2.Contamination`
    IMG_data_filtered <- IMG_data %>%
      dplyr::group_by(match_indices) %>%
      dplyr::slice_max(`Quality_score`, with_ties = FALSE)

    # Add genome IDs with max quality to database
    database$`IMG_Genome_ID_max_quality` <- NA
    database$`IMG_Genome_ID_max_quality` <- update_database_column(
      database_col = database$`IMG_Genome_ID_max_quality`,
      match_indices = IMG_data_filtered$match_indices,
      values = IMG_data_filtered$`IMG.Genome.ID`
    )

# === Add data from NCBI ===
    # Format data
    tax_ids <- extract_first_value(vec = database$`NCBI_Taxonomy_ID`)
    NCBI_data = get_multiple_lineages(tax_ids = tax_ids, nodes_df = NCBI_nodes, names_df = NCBI_names)

    # Find matches between database and NCBI
    matches_NCBI = match(x =  NCBI_data$tax_id, table = database$`NCBI_Taxonomy_ID`)

    # Add NCBI data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = NCBI_data,
      target_index = seq_along(matches_NCBI),
      source_index = matches_NCBI,
      source_col_names = c(
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species"
      ),
      target_col_names = c(
        "NCBI_Phylum",
        "NCBI_Class",
        "NCBI_Order",
        "NCBI_Family",
        "NCBI_Genus",
        "NCBI_Species"
      )
    )

# === Add data from BacDive ===
    # Format BacDive data
      BacDive_data <- BacDive_data %>%
        tidyr::separate(col = species, into = c("genus", "species"), sep = " ", extra = "merge")

    # Find matches between database and BacDive
      matches_BacDive = perform_matching(
        table_data = BacDive_data,
        table_genus_col = "genus",
        table_species_col = "species",
        table_strain_col = "strain_number_header",
        table_delim = ", ",
        x_data = database,
        x_genus_col = "Genus",
        x_species_col = "Species",
        x_strain_col = "Strain",
        x_delim = ";",
        collection_names = collection_names,
        output_file = NULL
      )

    # Filter indices to keep only the best matches
      matches_filtered = matches_BacDive %>% dplyr::group_by(x_index) %>%
        dplyr::slice_min(`Rank`, with_ties = FALSE)

    # Use indices to add BacDive data to database
      database <- add_columns_based_on_indices(
        target_df =  database,
        source_df = BacDive_data,
        target_index = matches_filtered$x_index,
        source_index = matches_filtered$table_index,
        source_col_names = c(
          "ID"
        ),
        target_col_names = c(
          "BacDive_ID"
        )
      )

    # Use BacDive IDs to add more data
      # Add most phenotypes
      phenotype_names <- c("Antibiotic_resistance", "Antibiotic_sensitivity",
                           "Cell_length", "Cell_shape", "Cell_width",
                           "Colony_size", "Enzyme_activity",                            
                           "Flagellum_arrangement", "Gram_stain",
                           "Incubation_period", "Indole_test", 
                           "Metabolite_production", "Metabolite_utilization",
                           "Motility",
                           "Oxygen_tolerance", "pH_for_growth",
                           "Pathogenicity_animal", "Pathogenicity_human", "Pathogenicity_plant",
                           "Salt_concentration", "Salt_concentration_unit",
                           "Spore_formation",
                           "Temperature_for_growth",
                           "Voges_proskauer")

      for (phenotype in phenotype_names) {
        source_df <- get(phenotype)
        database <- add_BacDive_phenotypes(
          target_df = database,
          source_df = source_df,
          source_col_names = NULL,
          target_col_names = paste0("BacDive_", phenotype)
        )
      }

      # Add isolation categories
      source_df <- Isolation_sources
      database <- add_BacDive_phenotypes(
        target_df = database,
        source_df = source_df,
        source_col_names = c(
          "Category 1",
          "Category 2",
          "Category 3"
          ),
        target_col_names = c(
          "BacDive_Isolation_category_1",
          "BacDive_Isolation_category_2",
          "BacDive_Isolation_category_3"
          )
      )

# === Add data from Bergey's Manual ===
  # Clean strain IDs in Bergey data
  Bergey_data$Strain <- gsub(pattern = "DSMZ", replacement = "DSM", x = Bergey_data$Strain)

  # Find matches between database and Bergey's Manual
  matches_Bergey = perform_matching(
    table_data = database,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_subspecies_col = "Subspecies",
    table_strain_col = "Strain",
    x_data = Bergey_data,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_subspecies_col = "Subspecies",
    x_strain_col = "Strain",
    table_delim = ";",
    x_delim = ";",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
  matches_filtered = matches_Bergey %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)

  # Remove any multiple matches still remaining
  # (caused by organisms appearing in multiple articles in Bergey's Manual or 
  # or organisms have subspecies names with incorrect name)
  matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  
  # Use indices to add data from Bergey's Manual to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = Bergey_data,
    target_index = matches_filtered$table_index,
    source_index = matches_filtered$x_index,
    source_col_names = c(
      "Phylum",
      "Class",
      "Order",
      "Family",
      "Genus",
      "Species",
      "Subspecies",
      "Strain",
      "Article_link",
      "Type_of_metabolism",
      "Text_for_end_products",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_substrates",
      "Substrates_for_end_products"
    ),
    target_col_names = c(
      "Bergey_Phylum",
      "Bergey_Class",
      "Bergey_Order",
      "Bergey_Family",
      "Bergey_Genus",
      "Bergey_Species",
      "Bergey_Subspecies",
      "Bergey_Strain",
      "Bergey_Article_link",
      "Bergey_Type_of_metabolism",
      "Bergey_Text_for_end_products",
      "Bergey_Major_end_products",
      "Bergey_Minor_end_products",
      "Bergey_Text_for_substrates",
      "Bergey_Substrates_for_end_products"
    )
  )

# === Add data from VPI Anaerobe Manual ===
    # Find matches between database and primary literature
    matches_VPI = perform_matching(
      table_data = database,
      table_genus_col = "Genus",
      table_species_col = "Species",
      table_strain_col = "Strain",
      x_data = VPI_data,
      x_genus_col = "Genus",
      x_species_col = "Species",
      x_strain_col = "Strains",
      table_delim = ";",
      x_delim = ",",
      collection_names = collection_names,
      output_file = NULL
    )

    # Filter indices to keep only the best matches
    matches_filtered <- matches_VPI %>% dplyr::group_by(x_index) %>%
      dplyr::slice_min(`Rank`, with_ties = FALSE)
    # Remove rank 7 and 8 matches (likely to contain non-type strains)
    matches_filtered <- matches_filtered %>% dplyr::filter(!Rank %in% c(7, 8))
    # Remove multiple matches (caused by strains appearing multiple times in VPI Anaerobe Manual)
    matches_filtered <- matches_filtered %>% dplyr::group_by(table_index) %>%
      dplyr::slice_min(`Rank`, with_ties = FALSE)
    
    # Use indices to add VPI data to database
    database <- add_columns_based_on_indices(
      target_df =  database,
      source_df = VPI_data,
      target_index = matches_filtered$table_index,
      source_index = matches_filtered$x_index,
      source_col_names = c(
        "Genus",
        "Species",
        "Species",
        "Type_of_metabolism",
        "Major_end_products",
        "Minor_end_products"
      ),
      target_col_names = c(
        "VPI_Genus",
        "VPI_Species",
        "VPI_Subspecies",
        "VPI_Type_of_metabolism",
        "VPI_Major_end_products",
        "VPI_Minor_end_products"
      )
    )

# === Add data from primary literature ===
  # Format names of columns
  selected_columns <- c(
    "Formate", "Acetate", "Propionate", "Butyrate", "Isobutyrate", "Valerate",
    "Isovalerate", "Caproate", "Isocaproate", "Heptanoate", "Octanoate", "Pyruvate", "Lactate",
    "Succinate", "Glycerol", "Malate", "Ornithine", "Ethanol", "Butanol", "CO2",
    "H2", "NH3", "CH4"
  )

  primary_literature_data =
    primary_literature_data %>% unite_with_names(
      col_name = "Text_for_end_products",
      selected_col = selected_columns,
      name_first = FALSE
    )

  # Find matches between database and primary literature
  matches_literature = perform_matching(
    table_data = database,
    table_genus_col = "Genus",
    table_species_col = "Species",
    table_strain_col = "Strain",
    x_data = primary_literature_data,
    x_genus_col = "Genus",
    x_species_col = "Species",
    x_strain_col = "Strain",
    table_delim = ";",
    x_delim = ",",
    collection_names = collection_names,
    output_file = NULL
  )

  # Filter indices to keep only the best matches
  matches_filtered <- matches_literature %>% dplyr::group_by(x_index) %>%
    dplyr::slice_min(`Rank`, with_ties = FALSE)
  # Remove rank 7 and 8 matches (likely to contain non-type strains)
  matches_filtered <- matches_filtered %>% dplyr::filter(!Rank %in% c(7, 8))
  
  # Use indices to add primary literature data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = primary_literature_data,
    target_index = matches_filtered$table_index,
    source_index = matches_filtered$x_index,
    source_col_names = c(
      "Genus",
      "Species",
      "Subspecies",
      "Strain",
      "Citation",
      "Type_of_metabolism",
      "Major_end_products",
      "Minor_end_products",
      "Text_for_end_products",
      "Substrates_for_end_products"
    ),
    target_col_names = c(
      "Literature_Genus",
      "Literature_Species",
      "Literature_Subspecies",
      "Literature_Strain",
      "Literature_Citation",
      "Literature_Type_of_metabolism",
      "Literature_Major_end_products",
      "Literature_Minor_end_products",
      "Literature_Text_for_end_products",
      "Literature_Substrates_for_end_products"
    )
  )

# === Add data from FAPROTAX ===
  # Format names of columns
  FAPROTAX_data$Genus = sapply(FAPROTAX_data$taxonomy, get_nth_element, delimiter = ";", n = 5)
  FAPROTAX_data$Species = sapply(FAPROTAX_data$taxonomy, get_nth_element, delimiter = ";", n = 6)

  # Find matches between database and FAPROTAX
  matches_FAPROTAX = FAPROTAX_data %>%
    dplyr::mutate(
      index = match(
        paste(Genus, Species),
        paste(database$Genus, database$Species)
      )
    ) %>%
    dplyr::pull(index)

  # Use indices to add FAPROTAX data to database
  database <- add_columns_based_on_indices(
    target_df =  database,
    source_df = FAPROTAX_data,
    target_index = matches_FAPROTAX,
    source_index = seq_along(matches_FAPROTAX),
    source_col_names = c(
      "group"
    ),
    target_col_names = c(
      "FAPROTAX_Type_of_metabolism"
    )
  )

# === Format data in database ===
  # Format all missing values consistently
      database[database == ""] <- NA
      database[database == "NA"] <- NA

# === Export database ===
    save_as_zip(database, "database.csv")
    
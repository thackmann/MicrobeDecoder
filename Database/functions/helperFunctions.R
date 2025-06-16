# Utility Functions for Database
# This script contains various utility functions for use in the constructing the 
# database, including operators, dataframe manipulation, and web scraping.
# Author: Timothy Hackmann
# Date: 14 February 2025

# === Define functions ===
  #' Pipe Operator
  #'
  #' This operator is imported from the magrittr package and is used to chain operations together.
  #'
  import::from(magrittr, "%>%")

  #' Install Missing CRAN Packages
  #'
  #' This function checks for missing CRAN packages and installs them if they are not already installed.
  #'
  #' @param packages A character vector of CRAN package names to check and install if missing.
  #' @return None. The function installs missing packages and provides a message if installation occurs.
  #' @examples
  #' cran_packages <- c("dplyr", "ggplot2")
  #' install_missing_cran_packages(cran_packages)
  install_missing_cran_packages <- function(packages) {
    # Identify missing packages
    missing_cran <- packages[!(packages %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_cran) > 0) {
      message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
      install.packages(missing_cran)
    } else {
      message("All packages are already installed.")
    }
  }
  
  #' Install Missing Bioconductor Packages
  #'
  #' This function checks for missing Bioconductor packages and installs them using BiocManager if they are not already installed.
  #'
  #' @param packages A character vector of Bioconductor package names to check and install if missing.
  #' @return None. The function installs missing packages and provides a message if installation occurs.
  #' @examples
  #' bioc_packages <- c("Biostrings", "GenomicRanges")
  #' install_missing_bioc_packages(bioc_packages)
  install_missing_bioc_packages <- function(packages) {
    # Ensure BiocManager is installed
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    
    # Identify missing packages
    missing_bioc <- packages[!(packages %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_bioc) > 0) {
      message("Installing missing Bioconductor packages: ", paste(missing_bioc, collapse = ", "))
      BiocManager::install(missing_bioc)
    } else {
      message("All Bioconductor packages are already installed.")
    }
  }
  
  #' Install Missing GitHub Packages
  #'
  #' This function checks for missing GitHub packages and installs them if they are not already installed.
  #'
  #' @param packages A named character vector of GitHub repository names in the format "username/repository".
  #'        If a package requires installation from a subdirectory, specify it as a named element where
  #'        the name is the repo and the value is the subdirectory.
  #' @return None. The function installs missing GitHub packages and provides a message if installation occurs.
  #' @examples
  #' github_packages <- c("r-lib/remotes", "thackmann/FileLocator" = "FileLocator")
  #' install_missing_github_packages(github_packages)
  install_missing_github_packages <- function(packages) {
    # Ensure remotes package is installed
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    
    # Extract package names from repo paths
    repo_names <- ifelse(names(packages) != "", names(packages), sub(".*/", "", packages))
    
    # Identify missing packages
    missing_github <- packages[!(repo_names %in% installed.packages()[, "Package"])]
    
    # Check if there are any missing packages
    if (length(missing_github) > 0) {
      message("Installing missing GitHub packages: ", paste(names(missing_github), collapse = ", "))
      
      for (repo in names(missing_github)) {
        subdir <- missing_github[repo]
        if (subdir == "") {
          remotes::install_github(repo)
        } else {
          remotes::install_github(repo, subdir = subdir)
        }
      }
    } else {
      message("All GitHub packages are already installed.")
    }
  }

  #' Add columns to a target dataframe based on indices from vectors.
  #'
  #' This function adds columns from a source dataframe to a target dataframe
  #' based on indices from two vectors. It allows adding values from multiple
  #' columns in the source dataframe to specified columns in the target dataframe.
  #'
  #' @param target_df Dataframe. The target dataframe to which the new columns will be added.
  #' @param source_df Dataframe. The source dataframe containing the values to add.
  #' @param target_index Integer vector. The indices of the target dataframe where the new values will be placed.
  #' @param source_index Integer vector. The indices of the source dataframe corresponding to the values to be added.
  #' @param source_col_names Character vector. A vector of column names in the source dataframe to be added.
  #' @param target_col_names Character vector. A vector of column names in the target dataframe where values will be placed.
  #'   This should be the same length as `source_col_names`.
  #' @param sep Character. A separator to use when concatenating values from the source dataframe. Default is ','.
  #'
  #' @return The target dataframe with the new columns added.
  #' @export
  #' @examples
  add_columns_based_on_indices <- function(target_df, source_df, target_index, source_index, source_col_names, target_col_names = source_col_names, sep = ",") {
    # Check if the lengths of target_index and source_index match
    if (length(target_index) != length(source_index)) {
      stop("Length of target_index and source_index must be the same.")
    }
    
    # Check if the lengths of source_col_names and target_col_names match
    if (length(source_col_names) != length(target_col_names)) {
      stop("Length of source_col_names and target_col_names must be the same.")
    }
    
    # Convert target_index and source_index into a dataframe for easy grouping
    index_df <- data.frame(target_index = target_index, source_index = source_index)
    
    # Loop over each column name pair and add the corresponding values from the source dataframe to the target dataframe
    for (i in seq_along(source_col_names)) {
      source_col <- source_col_names[i]
      target_col <- target_col_names[i]
      
      # Ensure the target dataframe has the new column initialized with NA
      target_df[[target_col]] <- NA
      
      # Loop over each unique target index
      for (t_index in unique(index_df$target_index)) {
        # Get the source indices that correspond to this target index
        matching_source_indices <- index_df$source_index[index_df$target_index == t_index]
        
        # Extract unique values from the source dataframe and concatenate them
        unique_values <- unique(source_df[[source_col]][matching_source_indices])
        
        # Remove NA values from unique values (unless all values are NA)
        if (!all(is.na(unique_values))) {
          unique_values <- unique_values[!is.na(unique_values)]
        }
        
        # Concatenate the unique values with the specified separator
        concatenated_values <- paste(unique_values, collapse = sep)
        
        # Assign the concatenated values to the corresponding target index in the target column
        target_df[[target_col]][t_index] <- concatenated_values
      }
    }
    
    return(target_df)
  }
  
  #' Retrieve the Body of a Web Page
  #'
  #' This function retrieves the body content of a web page using the `polite` package.
  #'
  #' @param url A character string representing the URL of the web page to scrape.
  #' @param user_agent A character string specifying the user agent to use for the request. 
  #'        Default is `"me"`.
  #'
  #' @return An object containing the HTML body of the web page.
  #' @export
  #'
  #' @examples
  #' # Example usage:
  #' body <- get_web_page_body("https://example.com", user_agent = "me")
  #'
  get_web_page_body <- function(url, user_agent = "me") {
    # Initiate a session with the specified URL
    session <- polite::bow(url, user_agent = user_agent)
    
    # Scrape the body of the page
    body <- polite::scrape(bow = session)
    
    return(body)
  }
  
  # --- Clean database file and add links ---
  #' Save Data in Original Format and Zip It
  #'
  #' This function saves a dataframe in its original format (e.g., CSV, RDS), 
  #' compresses it into a ZIP archive, and optionally removes the original file.
  #'
  #' @param data A dataframe to be saved.
  #' @param fp A character string specifying the full file path, including the original extension (e.g., "data/file.csv").
  #' @param remove_original A logical value indicating whether to delete the original file after zipping. Default is `TRUE`.
  #' @param overwrite A logical value indicating whether to overwrite an existing ZIP file. Default is `TRUE`.
  #' 
  #' @return The function does not return anything. It writes the file to disk.
  #' 
  #' @export
  save_as_zip <- function(data, fp, remove_original = TRUE, overwrite = TRUE) {
    # Ensure the zip package is available
    if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required. Install it with install.packages('zip')")
    
    # Normalize the file path
    fp <- normalizePath(fp, winslash = "/", mustWork = FALSE)
    
    # Extract file name, extension, and directory
    ext <- tools::file_ext(fp)
    if (ext == "") stop("File path must include an extension (e.g., .csv, .rds).")
    
    file_name <- basename(fp)  # e.g., "database_clean.csv"
    dir_name <- dirname(fp)    # e.g., "data"
    
    # Create a folder with the same name as the object
    object_name <- tools::file_path_sans_ext(file_name)  # e.g., "database_clean"
    zip_folder <- file.path(dir_name, object_name)
    
    # Ensure the folder is clean
    if (dir.exists(zip_folder)) unlink(zip_folder, recursive = TRUE)
    dir.create(zip_folder)
    
    # Define paths
    save_fp <- file.path(zip_folder, file_name)  # Save inside the new folder
    zip_fp <- file.path(dir_name, paste0(object_name, ".zip"))  # ZIP file in same directory
    
    # Save file based on extension
    switch(ext,
           "csv" = write.csv(data, save_fp, row.names = FALSE),
           "rds" = saveRDS(data, save_fp),
           stop("Unsupported file format: ", ext)
    )
    
    # Overwrite existing ZIP file if specified
    if (overwrite && file.exists(zip_fp)) file.remove(zip_fp)
    
    # Create ZIP file using zip::zipr()
    zip::zipr(zip_fp, files = save_fp, recurse = FALSE, compression_level = 9)
    
    # Ensure the ZIP file was created before deleting the original
    if (file.exists(zip_fp) && remove_original) unlink(zip_folder, recursive = TRUE)
  }
 
# === Assemble database ====
  #' Extract Culture Collection Names
  #'
  #' This function extracts names of culture collections from a vector of strain names.
  #' The names are prefixes in strain names (e.g., "DSM" in "DSM 1").
  #'
  #' @param strains A character vector where each element contains strain information,
  #'                with strain names separated by semicolons.
  #' @param pattern A regular expression pattern used to identify culture collection prefixes.
  #' @param n An integer specifying the number of top collections to return (default is 32).
  #'
  #' @return A character vector of the top `n` culture collection names sorted by frequency.
  #' @examples
  #' strains <- c("DSM 1234; ATCC 5678", "JCM 9101; DSM 2234")
  #' get_collection_names(strains, pattern = "^([A-Za-z]+)\\s\\d+", n = 5)
  #' @export
  get_collection_names <- function(strains, pattern = "^([A-Za-z]+)\\s\\d+", n = 32) {
    # Ensure the input is a character vector
    if (!is.character(strains)) {
      stop("The input must be a character vector.")
    }

    # Initialize an empty vector to store collection_names
    collection_names <- c()

    # Loop over each strain entry in the vector
    for (line in strains) {
      # Split each line by semicolons and trim whitespace
      strain_list <- strsplit(line, ";")[[1]]
      strain_list <- trimws(strain_list)

      # Extract the prefix if it matches the pattern
      match <- sapply(strain_list, function(x) {
        match <- regmatches(x, regexec(pattern, x))
        if (length(match[[1]]) > 1) {
          return(match[[1]][2])  # The first capture group (prefix) is in the second element
        } else {
          return(NA)
        }
      })

      # Remove NA values and add matched collections to the list
      collection_names <- c(collection_names, na.omit(match))
    }

    # Format the names as a sorted table
    collection_names <- table(collection_names)
    collection_names <- sort(collection_names, decreasing = TRUE)

    # Return the top `n` names
    return(names(collection_names)[1:n])
  }

  #' Split Taxonomy String into Taxonomic Ranks
  #'
  #' This function processes a single Greengenes-like taxonomy string and splits it into
  #' individual taxonomic ranks. By default, it extracts the species epithet
  #' (e.g., "coli" in "Escherichia coli").
  #'
  #' @param taxonomy A character string representing a GTDB taxonomy, e.g.,
  #'   "d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Burkholderiales;f__Burkholderiaceae;g__Bordetella;s__Bordetella pseudohinzii".
  #' @param extract_species_epithet Logical. If `TRUE` (default), the species
  #'   column contains only the epithet (e.g., "coli" from "Escherichia coli").
  #'   If `FALSE`, the full species name is returned.
  #'
  #' @return A named character vector with elements for each taxonomic rank:
  #'   `Domain`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, and `Species`.
  #'
  #' @examples
  #' taxonomy <- "d__Bacteria;p__Bacillota;c__Bacilli;o__Staphylococcales;f__Staphylococcaceae;g__Staphylococcus;s__Staphylococcus epidermidis"
  #' split_taxonomy(taxonomy)
  #' split_taxonomy(taxonomy, extract_species_epithet = FALSE)
  #' Split Taxonomy String into Taxonomic Ranks
  #'
  #' This function processes a single Greengenes-like taxonomy string and splits it into
  #' individual taxonomic ranks. By default, it extracts the species epithet
  #' (e.g., "coli" in "Escherichia coli").
  #'
  #' @param taxonomy A character string representing a GTDB taxonomy, e.g.,
  #'   "d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Burkholderiales;f__Burkholderiaceae;g__Bordetella;s__Bordetella pseudohinzii".
  #' @param extract_species_epithet Logical. If `TRUE` (default), the species
  #'   column contains only the epithet (e.g., "coli" from "Escherichia coli").
  #'   If `FALSE`, the full species name is returned.
  #'
  #' @return A named character vector with elements for each taxonomic rank:
  #'   `Domain`, `Phylum`, `Class`, `Order`, `Family`, `Genus`, and `Species`.
  #'
  #' @examples
  #' taxonomy <- "d__Bacteria;p__Bacillota;c__Bacilli;o__Staphylococcales;f__Staphylococcaceae;g__Staphylococcus;s__Staphylococcus epidermidis"
  #' split_taxonomy(taxonomy)
  #' split_taxonomy(taxonomy, extract_species_epithet = FALSE)
  split_taxonomy <- function(
    taxonomy,
    ranks = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    prefixes = paste0(substr(tolower(ranks), 1, 1), "__"),
    extract_species_epithet = TRUE
  ) {
    # Split the taxonomy string by semicolon
    taxon_split <- strsplit(taxonomy, ";")[[1]]
    
    # Create a named vector from prefix to taxon
    taxon_named <- setNames(rep(NA_character_, length(ranks)), ranks)
    for (item in taxon_split) {
      for (i in seq_along(prefixes)) {
        prefix <- prefixes[i]
        if (startsWith(item, prefix)) {
          taxon_named[ranks[i]] <- sub(paste0("^", prefix), "", item)
          break
        }
      }
    }
    
    # Optionally extract the species epithet
    if (extract_species_epithet && "Species" %in% ranks && !is.na(taxon_named["Species"])) {
      taxon_named["Species"] <- sub(".*\\s", "", taxon_named["Species"])
    }
    
    return(taxon_named)
  }

  #' Match an organism to a table based on genus, species, subspecies, and strain.
  #'
  #' This function matches a query organism, defined by its genus, species, subspecies, and strain,
  #' to an organism in a reference table. It optionally considers strain delimiters and can return
  #' the best matches based on ranking criteria.  Subspecies are matched only if present in a reference table.
  #'
  #' @param x_genus Character. Genus of the query organism.
  #' @param x_species Character. Species of the query organism.
  #' @param x_subspecies Character. Subspecies of the query organism (optional, can be NULL).
  #' @param x_strain Character. Strain of the query organism.
  #' @param x_delim Character. Delimiter used to split the query strain string. Default is NULL.
  #' @param table_genus Character vector. Genus column from the reference table.
  #' @param table_species Character vector. Species column from the reference table.
  #' @param table_subspecies Character vector. Subspecies column from the reference table (optional, can be NULL).
  #' @param table_strain Character vector. Strain column from the reference table.
  #' @param table_delim Character. Delimiter used to split the strain strings in the reference table. Default is NULL.
  #' @param collection_names Character vector. List of prefixes of large culture collection names (e.g., DSM, ATCC).
  #' @param best_matches Logical. If TRUE, only the highest-ranked matches are returned. Default is FALSE.
  #'
  #' @return A dataframe with matched entries including a calculated rank.
  #' @import dplyr stringr
  #' @export
  #' @examples
  #' match_organism("Escherichia", "coli", NULL, "ATCC 25922", table_genus, table_species, NULL, table_strain)
  match_organism <- function(x_genus, x_species, x_subspecies = NULL, x_strain, x_delim = NULL,
                             table_genus, table_species, table_subspecies = NULL, table_strain, table_delim = NULL,
                             collection_names = c(
                               "DSM", "JCM", "KCTC", "LMG", "NBRC", "ATCC", "CCUG", "CIP", "CGMCC", "KACC",
                               "IFO", "CECT", "NCTC", "NCIMB", "MCCC", "BCRC", "CCM", "GDMCC", "HAMBI",
                               "NCIB", "CBS", "CCRC", "RIA", "CFBP", "NCCB", "YIM", "IAM", "KCCM",
                               "MTCC", "ICMP", "KMM", "TBRC"
                             ),
                             best_matches = FALSE) {
    # Step 1: Find matches for genus, species, and subspecies separately
    match_genus <- which(x_genus == table_genus)
    match_species <- which(x_species == table_species)
    # match_subspecies <- if (!is.null(x_subspecies) && !is.null(table_subspecies)) which(x_subspecies == table_subspecies) else NULL
    match_subspecies <- if (!is.null(table_subspecies)) which(x_subspecies == table_subspecies) else NULL
    
    # Step 2: Split strain data if delimiters are provided
    if(is.null(x_delim)) {
      x = x_strain
    } else {
      x <- stringr::str_split(x_strain, paste0(x_delim, "\\s*"))[[1]]
    }
    
    if(is.null(table_delim)) {
      y = table_strain
    } else {
      y <- lapply(table_strain, function(str) stringr::str_split(str, paste0(table_delim, "\\s*"))[[1]])
    }
    
    # Step 3: Find matches for strain
    match_strain <- c()
    for (i in seq_along(y)) {
      if (any(x %in% y[[i]])) {
        match_strain <- c(match_strain, i)
      }
    }
    
    # Step 4: Find matches for culture collection (e.g., DSM, ATCC, etc.)
    pattern <- paste0(collection_names, collapse = "|")
    pattern <- paste0("^(", pattern, ")\\s\\d+")
    z <- x[grepl(pattern, x)]
    
    match_culture_collection <- c()
    for (i in seq_along(y)) {
      if (any(z %in% y[[i]])) {
        match_culture_collection <- c(match_culture_collection, i)
      }
    }
    
    # Step 5: Create match dataframe
    match_dataframe <- data.frame(
      Index = seq_along(table_genus),
      Genus_Match = seq_along(table_genus) %in% match_genus,
      Species_Match = seq_along(table_genus) %in% match_species,
      Subspecies_Match = if (!is.null(match_subspecies)) seq_along(table_genus) %in% match_subspecies else NA,
      Strain_Match = seq_along(table_genus) %in% match_strain,
      Culture_Collection_Match = seq_along(table_genus) %in% match_culture_collection
    )
    
    # Step 6: Assign rank based on matching criteria
    match_dataframe <- match_dataframe %>%
      dplyr::mutate(Rank = dplyr::case_when(
        Genus_Match & Species_Match & Subspecies_Match & Strain_Match ~ 1,
        Genus_Match & Species_Match & Strain_Match ~ 2,
        Species_Match & Subspecies_Match & Strain_Match ~ 3,
        Species_Match & Strain_Match ~ 4,
        Genus_Match & Strain_Match ~ 5,
        Culture_Collection_Match ~ 6,
        Genus_Match & Species_Match & Subspecies_Match ~ 7,
        Genus_Match & Species_Match ~ 8,
        TRUE ~ NA_real_
      )) %>%
      dplyr::filter(!is.na(Rank))
    
    # Step 7: Return best matches if specified
    if (nrow(match_dataframe) > 0) {
      match_dataframe <- match_dataframe %>%
        dplyr::arrange(Rank)
      
      if(best_matches){
        match_dataframe = match_dataframe %>% dplyr::filter(Rank == min(Rank))
      }
      return(match_dataframe)
    } else {
      return(data.frame(
        Index = NA_integer_,
        Genus_Match = NA,
        Species_Match = NA,
        Subspecies_Match = NA,
        Strain_Match = NA,
        Culture_Collection_Match = NA,
        Rank = NA_integer_
      ))
    }
  }
  
  #' Perform matching for a set of organisms.
  #'
  #' This function performs organism matching for multiple organisms based on genus, species, subspecies, and strain.
  #' It iterates through a dataset and calls the `match_organism` function to find the best matches.
  #'
  #' @param table_data Dataframe. The reference table containing the genus, species, and strain information.
  #' @param table_genus_col Character. The name of the genus column in the reference table.
  #' @param table_species_col Character. The name of the species column in the reference table.
  #' @param table_subspecies_col Character. The name of the subspecies column in the reference table.
  #' @param table_strain_col Character. The name of the strain column in the reference table.
  #' @param x_data Dataframe. The dataset containing the query organisms.
  #' @param x_genus_col Character. The name of the genus column in the query dataset.
  #' @param x_species_col Character. The name of the species column in the query dataset.
  #' @param x_subspecies_col Character. The name of the subspecies column in the query dataset.
  #' @param x_strain_col Character. The name of the strain column in the query dataset.
  #' @param table_delim Character. Delimiter used to split the strain strings in the reference table. Default is ",".
  #' @param x_delim Character. Delimiter used to split the strain strings in the query dataset. Default is ";".
  #' @param collection_names Character vector. List of names of culture collections (e.g., DSM, ATCC).
  #' @param output_file Character. File path for saving the output as CSV. Default is NULL.
  #'
  #' @return A dataframe of matched organisms with genus, species, strain, and culture collection matches, along with ranks.
  #' @import dplyr stringr
  #' @export
  #' @examples
  #' perform_matching(reference_table, "Genus", "Species", "Strain", query_data, "Genus", "Species", "Strain")
  perform_matching <- function(table_data, table_genus_col, table_species_col, table_subspecies_col = NULL, table_strain_col,
                               x_data, x_genus_col, x_species_col, x_subspecies_col = NULL, x_strain_col,
                               table_delim = ",", x_delim = ";",
                               collection_names = collection_names,
                               output_file=NULL) {
    # Initialize an empty dataframe to store matches
    matches <- data.frame(
      x_index = integer(),
      table_index = integer(),
      Genus_Match = character(),
      Species_Match = character(),
      Subspecies_Match = character(),
      Strain_Match = character(),
      Culture_Collection_Match = character(),
      Rank = character()
    )
    
    table_genus <- table_data[[table_genus_col]]
    table_species <- table_data[[table_species_col]]
    table_subspecies <- if (!is.null(table_subspecies_col)) table_data[[table_subspecies_col]] else NULL
    table_strain <- table_data[[table_strain_col]]
    table_strain <- lapply(table_strain, function(str) stringr::str_split(str, paste0(table_delim, "\\s*"))[[1]])
    
    for (i in 1:nrow(x_data)) {
      x_genus <- x_data[[x_genus_col]][i]
      x_species <- x_data[[x_species_col]][i]
      x_subspecies <- if (!is.null(x_subspecies_col)) x_data[[x_subspecies_col]][i] else NA
      x_strain <- x_data[[x_strain_col]][i]
      x_strain <- stringr::str_split(x_strain, paste0(x_delim, "\\s*"))[[1]]
      
      result <- match_organism(
        x_genus = x_genus,
        x_species = x_species,
        x_subspecies = if (!is.na(x_subspecies)) x_subspecies else NULL,
        x_strain = x_strain,
        x_delim = NULL,
        table_genus = table_genus,
        table_species = table_species,
        table_subspecies = if (!is.null(table_subspecies)) table_subspecies else NULL,
        table_strain = table_strain,
        collection_names = collection_names,
        table_delim = NULL
      )
      
      if (nrow(result) > 0) {
        colnames(result)[colnames(result) == "Index"] <- "table_index"
        result <- cbind(x_index = i, result)
        matches <- rbind(matches, result)
      }
      
      if (!is.null(output_file)) {
        write.csv(matches, output_file, row.names = FALSE)
      }
      
      svMisc::progress(value = i, max = nrow(x_data))
    }
    
    return(matches)
  }

  #' Add Data on BacDive Phenotypes
  #'
  #' This function adds phenotypic data from BacDive to a target dataframe.
  #'
  #' @param target_df Dataframe. The target dataframe to which the new columns will be added.
  #' @param source_df Dataframe. The source dataframe containing the values to add.
  #' @param source_col_names Character vector. A vector of column names in the source dataframe to be added.
  #' @param target_col_names Character vector. A vector of column names in the target dataframe where values will be placed.
  #'
  #' @return The target dataframe with the new columns added.
  #' @export
  add_BacDive_phenotypes <- function(target_df, source_df, source_col_names = NULL, target_col_names = source_col_names) {
    # Fill in missing ID cells with the value above
    source_df$ID <- zoo::na.locf(source_df$ID, na.rm = FALSE)
    source_df$ID <- as.character(source_df$ID)
    
    # Ensure that missing fields are filled for each ID and concatenate unique values
    source_df <- source_df %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ paste(unique(stats::na.omit(.)), collapse = ";")), .groups = "drop")
    
    # Get names of columns to add
    if (is.null(source_col_names)) {
      source_col_names <- names(source_df)[ncol(source_df)]
    }
    
    # Remove existing columns in target_df if they match target_col_names
    if (!is.null(target_col_names)) {
      target_df <- target_df %>%
        dplyr::select(-dplyr::any_of(target_col_names), everything()) # Removes columns before re-adding
    }
    
    # Join the columns to the target data
    for (col_name in source_col_names) {
      target_df <- target_df %>%
        dplyr::left_join(source_df %>% dplyr::select(ID, dplyr::all_of(col_name)), by = c("BacDive_ID" = "ID"))
    }
    
    # Rename columns if new names are provided
    if (!is.null(target_col_names)) {
      names(target_df)[match(source_col_names, names(target_df))] <- target_col_names
    }
    
    return(target_df)
  }
  

  #' Load Names Data from names.dmp File
  #'
  #' This function reads and processes the `names.dmp` file from the NCBI taxonomy database.
  #' It extracts taxonomic information and filters for scientific names only.
  #'
  #' @param file_path A character string specifying the file path to the `names.dmp` file.
  #' @return A data frame with columns `tax_id` and `name_txt`, containing scientific names.
  #' @importFrom dplyr filter select
  #' @examples
  #' \dontrun{
  #' scientific_names <- load_names_dmp("path/to/names.dmp")
  #' head(scientific_names)
  #' }
  load_names_dmp <- function(file_path) {
    # Read lines from names.dmp file
    names_data <- readLines(file_path)

    # Split each line by the separator "\t|\t"
    names_split <- strsplit(names_data, "\t\\|\t")

    # Convert the split list to a dataframe
    names_df <- do.call(rbind, lapply(names_split, function(x) x[1:4]))
    names_df[,4] <- gsub(pattern = "\t\\|", replacement = "", x = names_df[,4])
    colnames(names_df) <- c("tax_id", "name_txt", "unique_name", "name_class")

    # Convert columns to appropriate types
    names_df <- as.data.frame(names_df, stringsAsFactors = FALSE)
    names_df$tax_id <- as.integer(names_df$tax_id)

    # Filter for scientific names
    scientific_names <- names_df %>%
      dplyr::filter(name_class == "scientific name") %>%
      dplyr::select(tax_id, name_txt)

    return(scientific_names)
  }


  #' Load Nodes Data from nodes.dmp File
  #'
  #' This function reads and processes the `nodes.dmp` file from the NCBI taxonomy database.
  #' It extracts hierarchical relationships between taxonomic IDs.
  #'
  #' @param file_path A character string specifying the file path to the `nodes.dmp` file.
  #' @return A data frame containing columns for taxonomic relationships, such as `tax_id`, `parent_tax_id`, and `rank`.
  #' @examples
  #' \dontrun{
  #' nodes_df <- load_nodes_dmp("path/to/nodes.dmp")
  #' head(nodes_df)
  #' }
  load_nodes_dmp <- function(file_path) {
    # Read lines from nodes.dmp file
    nodes_data <- readLines(file_path)

    # Split each line by the separator "\t|\t"
    nodes_split <- strsplit(nodes_data, "\t\\|\t")

    # Convert the split list to a dataframe
    nodes_df <- do.call(rbind, lapply(nodes_split, function(x) x[1:13]))
    nodes_df[,13] <- gsub(pattern = "\t\\|", replacement = "", x = nodes_df[,13])
    colnames(nodes_df) <- c("tax_id", "parent_tax_id", "rank", "embl_code", "division_id",
                            "inherited_div_flag", "genetic_code_id", "inherited_GC_flag",
                            "mitochondrial_genetic_code_id", "inherited_MGC_flag",
                            "GenBank_hidden_flag", "hidden_subtree_root_flag", "comments")

    # Convert columns to appropriate types
    nodes_df <- as.data.frame(nodes_df, stringsAsFactors = FALSE)
    nodes_df$tax_id <- as.integer(nodes_df$tax_id)
    nodes_df$parent_tax_id <- as.integer(nodes_df$parent_tax_id)

    return(nodes_df)
  }

  #' Extract First Value from Delimited Elements in a Vector
  #'
  #' This function takes a vector of elements, some of which may contain multiple values separated by a specified delimiter,
  #' and returns a vector containing only the first value from each element.
  #'
  #' @param vec A character vector, where some elements may contain delimited values.
  #' @param sep A character string specifying the delimiter. Defaults to a comma.
  #' @return A character vector with only the first value from each delimited element, without names.
  #' @examples
  #' tax_ids <- c("1120917,291968", "2691583", "1960156")
  #' first_tax_ids <- extract_first_value(tax_ids, sep = ",")
  #' print(first_tax_ids) # Should return c("1120917", "2691583", "1960156")
  extract_first_value <- function(vec, sep = ",") {
    # Use sapply to split by the specified separator and take the first part, then remove names
    unname(sapply(vec, function(x) strsplit(as.character(x), sep)[[1]][1]))
  }

  #' Retrieve Taxonomic Lineage Up to Phylum Level
  #'
  #' This function retrieves the lineage for a given taxonomic ID up to the "phylum" level.
  #' It uses data from the `nodes.dmp` and `names.dmp` files to trace the lineage.
  #'
  #' @param tax_id An integer representing the taxonomic ID for the species.
  #' @param nodes_df A data frame from `load_nodes_dmp` containing taxonomic hierarchy information.
  #' @param names_df A data frame from `load_names_dmp` containing scientific names.
  #' @return A data frame with the lineage of the specified taxonomic ID up to the "phylum" level.
  #'         Columns include `tax_id`, `rank`, and `name`. If `tax_id` is `NA` or not found, it returns NA values.
  #' @examples
  #' \dontrun{
  #' lineage <- get_lineage(515635, nodes_df, scientific_names)
  #' print(lineage)
  #' }
  get_lineage <- function(tax_id, nodes_df, names_df) {
    # Return NA if tax_id is NA or if not found in nodes_df
    if (is.na(tax_id) || !(tax_id %in% nodes_df$tax_id)) {
      return(data.frame(
        tax_id = NA_integer_,
        rank = c("phylum", "class", "order", "family", "genus", "species"),
        name = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    # Initialize an empty data frame to store the lineage with rank, tax_id, and name columns
    lineage_df <- data.frame(
      tax_id = integer(),
      rank = character(),
      name = character(),
      stringsAsFactors = FALSE
    )

    # Loop to trace lineage until reaching "phylum" or the root (no parent)
    current_tax_id <- tax_id
    while(TRUE) {
      # Find the current node in nodes_df
      node <- nodes_df[nodes_df$tax_id == current_tax_id, ]
      if (nrow(node) == 0) break  # Stop if the tax_id is not found

      # Get the rank, tax_id, and parent_tax_id information
      rank <- node$rank
      parent_tax_id <- node$parent_tax_id

      # Find the scientific name for this tax_id in names_df
      name <- names_df[names_df$tax_id == current_tax_id, "name_txt"]
      name <- ifelse(length(name) > 0, name, NA) # Handle missing names

      # Append the current rank information as a new row in lineage_df
      lineage_df <- rbind(lineage_df, data.frame(
        tax_id = current_tax_id,
        rank = rank,
        name = name,
        stringsAsFactors = FALSE
      ))

      # Break if we reached the "phylum" level or if there's no parent
      if (rank == "phylum" || current_tax_id == parent_tax_id) break

      # Move up to the parent tax_id for the next iteration
      current_tax_id <- parent_tax_id
    }

    # Ensure the lineage has all expected ranks even if some are missing
    required_ranks <- c("phylum", "class", "order", "family", "genus", "species")
    missing_ranks <- setdiff(required_ranks, lineage_df$rank)

    if (length(missing_ranks) > 0) {
      # Add missing ranks with NA values for tax_id and name
      missing_df <- data.frame(
        tax_id = NA_integer_,
        rank = missing_ranks,
        name = NA_character_,
        stringsAsFactors = FALSE
      )
      lineage_df <- rbind(lineage_df, missing_df)
    }

    # Arrange the data frame by the order of ranks
    lineage_df <- lineage_df[match(required_ranks, lineage_df$rank), ]

    return(lineage_df)
  }

  #' Retrieve Lineage for Multiple Taxonomy IDs
  #'
  #' This function applies `get_lineage` over a vector of `tax_id`s, returning a data frame
  #' containing taxonomic names for `phylum`, `class`, `order`, `family`, `genus`, and `species` ranks.
  #' It includes a progress indicator in the console for each processed tax_id.
  #'
  #' @param tax_ids A vector of integers representing taxonomy IDs for the species.
  #' @param nodes_df A data frame from `load_nodes_dmp` containing taxonomic hierarchy information.
  #' @param names_df A data frame from `load_names_dmp` containing scientific names.
  #' @return A data frame with columns `tax_id`, `phylum`, `class`, `order`, `family`, `genus`, and `species`.
  #' @importFrom svMisc progress
  #' @examples
  #' \dontrun{
  #' tax_ids <- c(515635, 513050, 13)
  #' lineage_df <- get_multiple_lineages(tax_ids, nodes_df, scientific_names)
  #' print(lineage_df)
  #' }
  get_multiple_lineages <- function(tax_ids, nodes_df, names_df) {
    # Initialize an empty data frame with the necessary columns
    n <- length(tax_ids)
    lineage_df <- data.frame(
      tax_id = tax_ids,
      phylum = rep(NA_character_, n),
      class = rep(NA_character_, n),
      order = rep(NA_character_, n),
      family = rep(NA_character_, n),
      genus = rep(NA_character_, n),
      species = rep(NA_character_, n),
      stringsAsFactors = FALSE
    )

    # Loop through each tax_id and populate the data frame
    for (i in seq_along(tax_ids)) {
      # Retrieve the current tax_id
      tax_id <- tax_ids[i]

      # Get lineage information for the current tax_id
      lineage <- get_lineage(tax_id, nodes_df, names_df)

      # Extract the relevant taxonomic levels
      ranks <- c("phylum", "class", "order", "family", "genus", "species")
      for (rank in ranks) {
        if (rank %in% lineage$rank) {
          lineage_df[i, rank] <- lineage$name[lineage$rank == rank]
        }
      }

      # Show progress
      svMisc::progress(value = i, max = n)
    }

    return(lineage_df)
  }

  #' Write Project IDs to Batch Files
  #'
  #' This function takes a vector of `GOLD_Project_ID`s, removes missing or invalid entries,
  #' splits the IDs into batches of a specified size, and writes each batch to a separate file.
  #'
  #' @param project_IDs A character vector of `PROJECT GOLD ID`s.
  #' @param batch_size An integer specifying the number of IDs per batch. Default is 500.
  #' @param file_prefix A character string to use as the prefix for output files. Default is "project_IDs_batch".
  #'
  #' @return This function does not return a value; it creates files in the working directory.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' project_IDs <- database$`GOLD_Project_ID`
  #' write_project_ids_to_files(project_IDs, batch_size = 500, file_prefix = "project_IDs_batch")
  #' }
  write_project_ids_to_files <- function(project_IDs, batch_size = 500, file_prefix = "project_IDs_batch") {
    # Remove missing or invalid entries
    project_IDs <- project_IDs[!is.na(project_IDs) & project_IDs != "NA"]

    # Split IDs into batches
    batches <- split(project_IDs, ceiling(seq_along(project_IDs) / batch_size))

    # Write each batch to a separate file
    for (i in seq_along(batches)) {
      comma_delimited_string <- paste(batches[[i]], collapse = ", ")
      file_path <- paste0(file_prefix, i, ".txt")
      write(comma_delimited_string, file = file_path)
    }
  }

  #' Find Match Indices
  #'
  #' This function finds the first matching index for each element in a vector \code{x} within a table \code{table}.
  #' The matching uses regular expressions to ensure whole-word matching.
  #'
  #' @param x A character vector of elements to search for.
  #' @param table A character vector in which to search for matches.
  #'
  #' @return An integer vector of the same length as \code{x}, containing the indices of the first matches in \code{table},
  #' or \code{NA_integer_} if no match is found.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' x <- c("ID1", "ID2", "ID3")
  #' table <- c("ID1", "ID4", "ID3")
  #' find_match_indices(x, table)
  #' }
  find_match_indices <- function(x, table) {
    match_indices <- integer(length(x))
    for (i in seq_along(x)) {
      match_idx <- which(grepl(paste0("\\b", x[i], "\\b"), table))
      match_indices[i] <- if (length(match_idx) > 0) match_idx[1] else NA_integer_

      svMisc::progress(value = i, max = length(x))
    }
    match_indices
  }

  #' Update Database Column
  #'
  #' This function updates a column in a database using match indices and corresponding values.
  #' For each match index, the corresponding value is either set or appended to the existing value in the column.
  #'
  #' @param database_col A character vector representing the column to update.
  #' @param match_indices An integer vector of indices indicating where to update values in \code{database_col}.
  #' @param values A character vector of values to insert or append to \code{database_col}.
  #'
  #' @return A character vector with updated values.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' database_col <- c(NA, NA, "Existing")
  #' match_indices <- c(1, 2, 3)
  #' values <- c("Value1", "Value2", "Value3")
  #' update_database_column(database_col, match_indices, values)
  #' }
  update_database_column <- function(database_col, match_indices, values) {
    for (i in seq_along(values)) {
      match_idx <- match_indices[i]

      if (!is.na(match_idx)) {
        if (is.na(database_col[match_idx])) {
          database_col[match_idx] <- values[i]
        } else {
          database_col[match_idx] <- paste(database_col[match_idx], values[i], sep = ",")
        }
      }
    }
    database_col
  }

  #' Concatenate Column Names and Values in a Single Column
  #'
  #' This function combines specified columns into a single column, similar to `tidyr::unite`,
  #' but with the added option of concatenating the column names with their respective values.
  #' `NA` values are ignored when concatenating.
  #'
  #' @param data A data frame or tibble.
  #' @param col_name The name of the new column to store the combined text.
  #' @param selected_col A character vector of column names to be united.
  #' @param name_value_sep Separator used between the column name and its value.
  #' @param pair_sep Separator used between each column name-value pair.
  #' @param name_first Logical, if `TRUE`, the column name appears before the value; if `FALSE`, the value appears first.
  #' @return A tibble with the new combined column and without the original columns specified in `selected_col`.
  #' @examples
  #' df <- data.frame(
  #'   cluster = LETTERS[1:4],
  #'   group = c(rep("m", 2), rep("f", 2)),
  #'   point = rnorm(4),
  #'   err = c(NA, 0.142428504256532, NA, 0.125111019192263)
  #' )
  #' unite_with_names(df, "text", selected_col = c("cluster", "group", "point", "err"),
  #'                  name_value_sep = ": ", pair_sep = "\n", name_first = TRUE)
  #'
  #' @export
  unite_with_names <- function(data, col_name, selected_col, name_value_sep = " ", pair_sep = ";", name_first = TRUE) {
    # Select specified columns
    columns <- dplyr::select(data, dplyr::all_of(selected_col))

    # Concatenate column names with values, skipping NAs, and based on name_first
    columns <- purrr::imap(columns, ~ ifelse(
      is.na(.x),
      NA,
      if (name_first) paste(.y, .x, sep = name_value_sep) else paste(.x, .y, sep = name_value_sep)
    ))

    # Convert to tibble and apply unite with pair_sep between each column's name-value combination
    combined <- columns %>%
      tidyr::as_tibble() %>%
      tidyr::unite({{ col_name }}, dplyr::everything(), sep = pair_sep, na.rm = TRUE)

    # Return data with combined column
    dplyr::bind_cols(dplyr::select(data, -dplyr::all_of(selected_col)), combined)
  }

  #' Get the nth Element from a Delimited String
  #'
  #' This function splits a string by a specified delimiter and returns the nth element.
  #'
  #' @param string A character string to be split.
  #' @param delimiter A character string representing the delimiter to split the string by.
  #' @param n An integer specifying the position of the element to retrieve after splitting.
  #'
  #' @return The nth element as a character string if it exists; otherwise, \code{NA}.
  #'
  #' @examples
  #' get_nth_element("apple,banana,cherry", ",", 2)  # Returns "banana"
  #' get_nth_element("dog,cat", ",", 3)  # Returns NA, since there is no 3rd element
  #'
  #' @importFrom stringr str_split
  #'
  get_nth_element <- function(string, delimiter, n) {
    elements <- stringr::str_split(string, delimiter)[[1]]
    if (n <= length(elements)) {
      return(elements[[n]])
    } else {
      return(NA)  # Return NA if n is out of bounds
    }
  }

  #' Coalesce and Drop Columns
  #'
  #' This function coalesces values from one or more columns into a single column
  #' and removes the original columns afterward. It works similarly to
  #' `dplyr::coalesce()` but extends the functionality to handle multiple columns
  #' and streamline the process of column removal.
  #'
  #' @param df A dataframe containing the columns to be coalesced.
  #' @param columns A character vector specifying the names of the columns to coalesce.
  #'   The first non-NA value in each row across these columns will be retained.
  #' @param new_col A string specifying the name of the new column to create
  #'   from the coalesced values. Defaults to the name of the first column in `columns`.
  #'
  #' @return A dataframe with `new_col` as the coalesced column, and the original
  #'   columns in `columns` removed.
  #'
  #' @details This function provides a convenient way to combine values from multiple
  #' columns into a single column while ensuring that the original columns are
  #' dropped from the dataframe. It leverages `dplyr` for mutation and column selection.
  #'
  #' @examples
  #' df <- data.frame(
  #'   col1 = c(NA, "B", NA, "D"),
  #'   col2 = c("A", NA, "C", NA),
  #'   col3 = c("X", NA, NA, "Y")
  #' )
  #'
  #' # Coalesce three columns into one
  #' coalesce_and_drop(df, columns = c("col1", "col2", "col3"), new_col = "final_col")
  #'
  #' # Coalesce two columns with a default new column name
  #' coalesce_and_drop(df, columns = c("col1", "col2"))
  #'
  #' @export
  coalesce_and_drop <- function(df, columns, new_col = columns[1]) {
    df %>%
      dplyr::mutate(!!new_col := dplyr::coalesce(!!!rlang::syms(columns))) %>%
      dplyr::select(-dplyr::all_of(columns))
  } 
# Define Functions for Predictions from Taxonomy Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === Functions for predicting traits ===
  #' Extract Unique Values from a Delimited Vector
  #'
  #' This function takes a character vector where each element may contain 
  #' multiple values separated by a specified delimiter. It extracts and 
  #' returns a unique list of all values.
  #'
  #' @param x A vector with delimited values (will be coerced to character if needed).
  #' @param delimiter A string indicating the delimiter used to separate values. Default is `";"`.
  #' @return A character vector of unique values sorted alphabetically, or NULL if coercion fails.
  #' @examples
  #' vec <- c("lactate", "acetate;ethanol;H2;CO2", "acetate;butyrate", "acetate", NA)
  #' extract_unique_values(vec)
  #' extract_unique_values(vec, delimiter = ",")
  #' @export
  extract_unique_values <- function(x, delimiter = ";") {
    if (!is.character(x)) {
      x <- tryCatch(as.character(x), error = function(e) NULL)
      
      if (is.null(x)) {
        warning("Unable to coerce input to character. Returning NULL.")
        return(NULL)
      }
    }
    
    unique_values <- unique(unlist(strsplit(na.omit(x), delimiter)))
    sorted_values <- sort(unique_values)
    
    return(sorted_values)
  }
  
  #' Convert a Query into a Regular Expression for Matching
  #'
  #' This function converts a query string into a regular expression for matching,
  #' padding strings with "^" and "$" to prevent partial matches.
  #'
  #' @param x A character string or NA value to be formatted.
  #' @return A formatted regular expression string.
  #' @export
  format_query_element <- function(x) {
      return(paste0("^", x, "$"))
  }
  
  #' Get Reference Table of Organisms
  #'
  #' This function takes the database and formats into a reference table of organisms
  #' and their traits.  It can optionally add a custom trait based on a query string.  
  #'
  #' @param data A dataframe of the database
  #' @param system_taxonomy A character value of the taxonomy system to use.  Default is 'LPSN'. 
  #' @param traits_to_predict A character vector of trait column names.
  #' @param query_string A string representing the query used to filter the organisms.
  #' @param ignore_NA Logical; whether to remove rows with all NA in trait columns.
  #'
  #' @return A reference table of organisms and their traits
  #' @export
  get_reference_table <- function(data, 
                                  system_taxonomy = "LPSN",
                                  traits_to_predict = NULL, 
                                  query_string = NULL, 
                                  ignore_NA = TRUE) {
    # Get data
    table <- data
    
    # Add taxonomy
    col_name <- paste0(system_taxonomy, " Taxonomy")
    table <- expand_and_merge_taxonomy(data = table, col_name = col_name)
    
    # Add custom traits
    if(!is.null(query_string)) {
      trait_name <- "Custom trait"
      table <- add_custom_traits(data = table, query_string = query_string, trait_name = trait_name, ignore_NA = ignore_NA)
      traits_to_predict <- trait_name
    }
    
    # Select relevant columns
    table <- table %>%
      dplyr::select(Phylum, Class, Order, Family, Genus, Species, dplyr::all_of(traits_to_predict))
    
    if (ignore_NA) {
      table <- table %>%
        dplyr::filter(!dplyr::if_all(dplyr::all_of(traits_to_predict), is.na))
    }
    
    return(table)
  }
  
  #' Filter a Table by Taxon
  #'
  #' This function filters a table to return rows matching a specified query taxon.
  #' It starts at the most specific level (Genus + Species) and progressively moves up 
  #' the taxonomic ranks until a match is found. There is an option to require 
  #' all ranks to match (strict mode). Ranks that are NA (^NA$) are ignored.
  #'
  #' @param table A dataframe to be filtered.
  #' @param query_taxon A dataframe containing the taxon query to match
  #' @param ignore_species Logical; whether to ignore the rank of species in taxa
  #' @param match_all_ranks Logical; if TRUE, all ranks must match. Default is FALSE
  #'
  #' @return A filtered dataframe containing only rows that match the query taxon.
  #' @export
  filter_table_by_taxon <- function(table, query_taxon, ignore_species = TRUE, match_all_ranks = FALSE) {
    # Set levels of taxonomic ranks to match
    rank_levels <- list(
      c("Genus", "Species"),
      "Genus",
      "Family",
      "Order",
      "Class",
      "Phylum"
    )

    if (ignore_species) {
      rank_levels <- lapply(rank_levels, setdiff, "Species")
    }

    valid_levels <- names(query_taxon)[
      names(query_taxon) %in% unlist(rank_levels) & !sapply(query_taxon, identical, "^NA$")
    ]
    
    rank_levels <- lapply(rank_levels, function(ranks) {
      intersect(ranks, valid_levels)
    })
    
    rank_levels <- Filter(length, rank_levels)
    rank_levels <- rank_levels[!duplicated(rank_levels)]
    
    # Option 1 (strict): match across all taxonomic ranks at once
    if (match_all_ranks) {
      match <- table
      
      all_ranks_to_match <- unique(unlist(rank_levels))
      
      for (rank in all_ranks_to_match) {
        pattern <- query_taxon[[rank]]

        if (!identical(pattern, "^.*$")) {
          match <- dplyr::filter(match, grepl(pattern, .data[[rank]]))
        }
      }
      
      return(dplyr::select(as.data.frame(match), -all_of(colnames(query_taxon))))
    }
    
    # Option 2 (permissive): start matching at low taxonomic levels, then move up if no match
    for (ranks in rank_levels) {
      match <- table
      for (rank in ranks) {
        pattern <- query_taxon[[rank]]

        if (!identical(pattern, "^.*$")) {
          match <- dplyr::filter(match, grepl(pattern, .data[[rank]]))
        }
      }
      if (nrow(match) > 0) {
        return(dplyr::select(as.data.frame(match), -all_of(colnames(query_taxon))))
      }
    }
    
    # No match found at any level
    return(dplyr::select(as.data.frame(table[0, ]), -all_of(colnames(query_taxon))))
  }
  
  #' Convert a Trait into a Regular Expression for Matching
  #'
  #' This function converts a trait string into a regular expression for matching,
  #' padding strings with "(?<=^|;)" and "(?=;|$)" to prevent partial matches. 
  #' Specifically, it will match traits at the start of string, end of a string, 
  #' and those containing ";".
  #'
  #' @param x A character string or NA value to be formatted.
  #' @return A formatted regular expression string.
  #' @export
  format_trait_element <- function(x) {
    return(paste0("(?<=^|;)", x, "(?=;|$)"))
  }
  
  #' Calculate the Fraction of Matching Elements in a Vector
  #'
  #' This function calculates the fraction of elements in a character vector that match 
  #' a given regular expression, with options to ignore NA values or replace them with 
  #' a specified value.
  #'
  #' @param x A character vector.
  #' @param pattern A regular expression pattern to match within elements of `x`.
  #' @param ignore_NA A logical value indicating whether to ignore NA values (`TRUE`, default) 
  #'        or replace them with `replace_value` (`FALSE`).
  #' @param replace_value A numeric value used to replace NA values when `ignore_NA = FALSE`. 
  #'        Default is 0.
  #'
  #' @return The fraction of elements in `x` that match the pattern.
  #' @examples
  #' vec <- c("acetate", "butyrate;acetate", "acetate;propionate", "butyrate", NA)
  #' match_fraction(vec, pattern = "(?<=^|;)acetate(?=;|$)")  # Default: ignores NA
  #' match_fraction(vec, pattern = "(?<=^|;)acetate(?=;|$)", ignore_NA = FALSE, replace_value = 0)
  #'
  #' @export
  match_fraction <- function(x, pattern, ignore_NA = TRUE, replace_value = 0) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(0)  # Return 0 for empty, NULL, or all-NA cases
    }
    
    if (!is.character(x)) {
      x <- tryCatch(as.character(x), error = function(e) NULL)
      if (is.null(x)) {
        warning("Unable to coerce input to character. Returning 0.")
        return(0)
      }
    }
    
    # Convert matches into binary (0 or 1)
    match_binary <- sapply(gregexpr(pattern, x, perl = TRUE), function(m) as.numeric(any(m != -1)))
    
    # Ensure match_binary is numeric/logical before taking mean
    if (!is.numeric(match_binary) & !is.logical(match_binary)) {
      stop("Error: match_binary is not numeric or logical. Check input x and pattern.")
    }
    
    # Compute fraction
    if (ignore_NA) {
      return(mean(match_binary, na.rm = TRUE))
    } else {
      match_binary[is.na(match_binary)] <- replace_value
      return(mean(match_binary))
    }
  }
  
  #' Get Mapping of Original Rows to Unique Rows
  #'
  #' This function identifies unique rows in a dataframe based on specified columns 
  #' and returns a mapping of each original row to the first occurrence of its unique value.
  #'
  #' @param df A dataframe containing the data to be processed.
  #' @param unique_cols A character vector of column names to determine uniqueness. 
  #'   Defaults to all columns in `df`.
  #'
  #' @return A dataframe with two columns:
  #'   - `x`: The row index in the original dataframe.
  #'   - `y`: The corresponding row index of the first unique occurrence.
  #'
  #' @examples
  #' # Example dataframe
  #' df <- data.frame(
  #'   Phylum = c(NA, NA, NA, NA, NA, NA),
  #'   Class = c(NA, NA, NA, NA, NA, NA),
  #'   Order = c(NA, NA, NA, NA, NA, NA),
  #'   Family = c(NA, NA, "Lachnospiraceae", "Porphyromonadaceae", NA, NA),
  #'   Genus = c("Clostridium", "Corynebacterium", NA, NA, "Treponema", "Treponema"),
  #'   Species = c(NA, NA, NA, NA, NA, NA)
  #' )
  #' 
  #' # Get mapping of original rows to first unique occurrence
  #' get_unique_mapping(df, c("Phylum", "Class", "Order", "Family", "Genus", "Species"))
  #'
  #' @export
  get_unique_mapping <- function(df, unique_cols = colnames(df)) {
    # Add an index column before deduplication
    df$`x` <- seq_len(nrow(df))
    
    # Find unique rows based on specified columns and assign a unique index
    df_unique <- df %>%
      dplyr::distinct(across(all_of(unique_cols)), .keep_all = TRUE) %>%
      dplyr::mutate(`y` = seq_len(dplyr::n()))
    
    # Create a mapping of each original row to its unique row
    df_map <- df %>%
      dplyr::left_join(df_unique %>% dplyr::select(-`x`), by = unique_cols) %>%
      dplyr::select(`x`, `y`)
    
    return(df_map)
  }
  
  #' Identify Unique Queries and Create Mapping
  #'
  #' This function identifies unique queries based on specified taxonomic columns 
  #' and creates a mapping to restore duplicate results later.
  #'
  #' @param query A dataframe containing query data.
  #' @param unique_cols A character vector specifying the taxonomic columns 
  #'   to identify unique queries.
  #'
  #' @return A list containing:
  #'   - `query_map`: A mapping dataframe to restore duplicate results.
  #'   - `query_unique`: A dataframe with distinct queries.
  #' 
  #' @export
  precompute_unique_queries <- function(query, unique_cols) {
    query_map <- get_unique_mapping(df = query, unique_cols = unique_cols)
    query_unique <- query %>% dplyr::distinct(across(all_of(unique_cols)), .keep_all = TRUE)
    list(query_map = query_map, query_unique = query_unique)
  }
  
  #' Precompute Patterns for Query and Traits
  #'
  #' This function precomputes patterns for unique queries and trait categories
  #' to speed up subsequent matching.
  #'
  #' @param query_unique A dataframe containing unique queries.
  #' @param traits_to_predict A character vector of trait categories to predict.
  #' @param table A dataframe containing reference data.
  #'
  #' @return A list containing:
  #'   - `unique_traits`: A list of unique values for each trait.
  #'   - `num_unique_traits`: A numeric vector with counts of unique traits per category.
  #'   - `query_patterns`: A list of formatted query patterns.
  #'   - `trait_patterns`: A list of formatted trait patterns.
  #'
  #' @export
  precompute_patterns <- function(query_unique, traits_to_predict, table) {
    unique_traits <- setNames(lapply(traits_to_predict, function(cat) extract_unique_values(table[[cat]])), 
                              traits_to_predict)
    num_unique_traits <- sapply(unique_traits, length)
    
    query_patterns <- lapply(seq_len(nrow(query_unique)), function(i) {
      as.data.frame(apply(query_unique[i, ], c(1, 2), format_query_element))
    })
    
    trait_patterns <- setNames(lapply(traits_to_predict, function(trait) {
      sapply(unique_traits[[trait]], format_trait_element, USE.NAMES = FALSE)
    }), traits_to_predict)
    
    list(unique_traits = unique_traits, num_unique_traits = num_unique_traits, 
         query_patterns = query_patterns, trait_patterns = trait_patterns)
  }
  
  #' Compute Matching Organisms for Each Query
  #'
  #' This function finds matching organisms in the reference table for each unique query.
  #'
  #' @param query_unique A dataframe containing unique queries.
  #' @param table A dataframe containing reference data.
  #' @param query_patterns A list of formatted query patterns.
  #' @param ignore_species Logical; whether to ignore the rank of species in taxa
  #' @param match_all_ranks Logical; if TRUE, all ranks must match. Default is FALSE
  #'
  #' @return A list of dataframes, where each element corresponds to matching organisms 
  #'   for a unique query.
  #'
  #' @export
  compute_matching_organisms <- function(query_unique, table, query_patterns, ignore_species = TRUE, match_all_ranks = FALSE) {
    matching_organisms <- vector("list", length = nrow(query_unique))
    for (i in seq_len(nrow(query_unique))) {
      matching_organisms[[i]] <- filter_table_by_taxon(table = table, 
                                                       query_taxon = query_patterns[[i]], 
                                                       ignore_species = ignore_species, 
                                                       match_all_ranks = match_all_ranks)
      
    }
    
    return(matching_organisms)
  }
  
  #' Compute Probabilities of Traits for Unique Queries
  #'
  #' This function computes the probability of traits for each unique query,
  #' updating the progress bar in a Shiny session.
  #'
  #' @param query_unique A dataframe of unique queries.
  #' @param traits_to_predict A character vector of trait categories to predict.
  #' @param unique_traits A list of unique traits per category.
  #' @param trait_patterns A list of formatted trait patterns.
  #' @param matching_organisms A list of dataframes with matching organisms.
  #' @param ignore_NA A logical indicating whether to ignore NA values (default = TRUE).
  #' @param session The Shiny session object (optional; required for progress bar updates).
  #' @param ns A namespace function for modular Shiny apps (optional; required for progress bar updates).
  #'
  #' @return A dataframe containing computed probabilities for each trait-category-query combination.
  #'
  #' @export
  compute_probabilities <- function(query_unique, traits_to_predict, unique_traits, 
                              trait_patterns, matching_organisms, ignore_NA = TRUE,
                              session = NULL, 
                              ns = NULL) {
    # Initialize values
    results_list <- vector("list", sum(sapply(unique_traits, length)) * nrow(query_unique) * length(traits_to_predict))
    idx <- 1
    
    # Loop through each unique query and compute probabilities
    for (i in seq_len(nrow(query_unique))) {
      match <- matching_organisms[[i]]
      query_taxon <- query_unique[i, ]
      
      for (trait_col in traits_to_predict) {
        pattern <- trait_patterns[[trait_col]]
        matching_traits <- match %>% dplyr::pull(trait_col)
        
        for (k in seq_along(unique_traits[[trait_col]])) {
          results_list[[idx]] <- list(
            `y` = i,
            `Phylum` = query_taxon["Phylum"],
            `Class` = query_taxon["Class"],
            `Order` = query_taxon["Order"],
            `Family` = query_taxon["Family"],
            `Genus` = query_taxon["Genus"],
            `Species` = query_taxon["Species"],
            `Trait category` = trait_col,
            `Trait name` = unique_traits[[trait_col]][k],
            `Probability` = match_fraction(matching_traits, pattern[k], ignore_NA = ignore_NA)
          )
          idx <- idx + 1
        }
      }
      
      # Update progress
      if (!is.null(ns)) display_modal(ns = ns, message = "Prediction in progress", value = (i / nrow(query_unique)) * 100)
    }
    
    # Bind results and flatten any nested dataframes
    results_df <- dplyr::bind_rows(results_list) %>%
      dplyr::mutate(dplyr::across(where(is.data.frame), ~ .[[1]]))
    
    return(results_df)
    
  }
  
  #' Restore Duplicate Rows from Unique Queries
  #'
  #' This function restores duplicate rows that were removed when identifying unique queries.
  #' It maps computed results back to the original queries.
  #'
  #' @param df_unique A dataframe containing computed results for unique queries.
  #' @param query_map A mapping dataframe that links original rows to unique query rows.
  #'
  #' @return A dataframe with duplicate rows restored.
  #'
  #' @export
  restore_duplicates <- function(df_unique, query_map) {
    query_map %>%
      dplyr::left_join(df_unique, by = "y", relationship = "many-to-many") %>%
      dplyr::select(-`y`) %>%
      dplyr::rename(`Organism number` = `x`)
  }
  
  #' Predict Trait Probabilities from Query and Reference Table
  #'
  #' This function computes probabilities of traits for a given query taxa table
  #' using a reference table and pre-specified trait categories.
  #'
  #' @param query A formatted query dataframe.
  #' @param table A cleaned reference table.
  #' @param traits_to_predict A character vector of trait names to predict.
  #' @param ignore_NA Logical; whether to ignore NAs when computing probabilities.
  #' @param ignore_species Logical; whether to ignore the rank of species in taxa
  #' @param match_all_ranks Logical; if TRUE, all ranks must match. Default is FALSE
  #' @param ns A namespace function for Shiny progress bar updates.
  #' @param session The current Shiny session object.
  #'
  #' @return A dataframe of predicted trait probabilities.
  #' @export
  predict_traits_taxonomy <- function(query, table, traits_to_predict, ignore_NA = TRUE,
                            ignore_species = TRUE, match_all_ranks = FALSE,
                             ns = NULL, session = shiny::getDefaultReactiveDomain()) {
    unique_cols <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    query_data <- precompute_unique_queries(query, unique_cols)
    
    patterns <- precompute_patterns(query_data$query_unique, traits_to_predict, table)
    
    matching_organisms <- compute_matching_organisms(query_unique = query_data$query_unique, 
                                                     table = table, 
                                                     query_patterns = patterns$query_patterns, 
                                                     ignore_species = ignore_species,
                                                     match_all_ranks = match_all_ranks)
    
    results_unique <- compute_probabilities(
      query_unique = query_data$query_unique,
      traits_to_predict = traits_to_predict,
      unique_traits = patterns$unique_traits,
      trait_patterns = patterns$trait_patterns,
      matching_organisms = matching_organisms,
      ignore_NA = ignore_NA,
      ns = ns,
      session = session
    )
    
    restore_duplicates(results_unique, query_data$query_map)
  }
  
  #' Compute Predictions from Taxonomy
  #'
  #' This is the main function for predicting traits in the module
  #'
  #' @param data Database loaded with organism metadata
  #' @param query_taxa A list of taxonomic inputs
  #' @param query_string A query string used to filter the database
  #' @param traits_to_predict Character vector of trait names
  #' @param ignore_NA Logical, whether to ignore NA values
  #' @param match_all_ranks Logical; if TRUE, all ranks must match. Default is FALSE
  #' @param ignore_species Logical, whether to ignore species rank in taxa
  #' @param system_taxonomy Character, taxonomy system to use (e.g., "LPSN")
  #' @param ns The namespace function for the Shiny module.
  #'
  #' @return A named list with trait probabilities.
  #' @export
  compute_taxonomy_predictions <- function(data, query_taxa, query_string, traits_to_predict, ignore_NA, match_all_ranks, ignore_species, system_taxonomy, ns = NULL) {
    # Update progress
    if (!is.null(ns)) display_modal(ns = ns, message = "Getting data", value = 0)  
    
    # Get reference table
    table <- get_reference_table(data, system_taxonomy, traits_to_predict, query_string, ignore_NA)
    
    # Get query taxa
    query_taxa <- format_query_taxa(query_taxa)
    
    # Update progress
    if (!is.null(ns)) display_modal(ns = ns, message = "Prediction in progress", value = 0)  
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))  
    
    # Get trait probabilities
    probabilities <- predict_traits_taxonomy(query_taxa, table, traits_to_predict, ignore_NA = ignore_NA, ignore_species = ignore_species, match_all_ranks = match_all_ranks, ns = ns)
    
    # Update progress
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    return(list(probabilities = probabilities))
  }
  
# === Processing taxonomy ===
  #' Check if String has Taxonomy in QIIME2 or MetaPhlAn Format
  #' 
  #' This helper function is used to detect if a string follows QIIME2 or
  #' MetaPhlAn format.  It checks for presence of taxonomic prefixes 
  #' (like k__, p__), which both formats use.  It also checks for presence 
  #' of semicolon seperators (used by QIIME2) and pipe separators 
  #' (used by MetaPhlAn).
  #'
  #' @param x A single character string.
  #' @return One of "QIIME2", "MetaPhlAn", or "Unknown".
  #' @examples
  #' classify_taxonomy_string("k__Bacteria; p__Firmicutes; c__Bacilli")
  #' classify_taxonomy_string("k__Bacteria|p__Firmicutes|c__Bacilli")
  #' classify_taxonomy_string("UNCLASSIFIED")
  classify_taxonomy_string <- function(x) {
    x <- as.character(x)
    
    has_prefix <- grepl("k__|p__|c__|o__|f__|g__|s__", x, perl = TRUE)
    has_semicolon <- grepl(";", x, fixed = TRUE)
    has_pipe <- grepl("|", x, fixed = TRUE)
    
    if (has_prefix && has_semicolon && !has_pipe) {
      return("qiime2")
    } else if (has_prefix && has_pipe && !has_semicolon) {
      return("metaphlan")
    } else {
      return("unknown")
    }
  }
  
  #' Split Taxonomy String into Taxonomic Ranks
  #'
  #' This function splits a string containing an organism's full taxonomy into
  #' individual ranks. It can accommodate both QIIME2 and MetaPhlAn-style
  #' strings. By default, it extracts the species epithet
  #' (e.g., "coli" in "Escherichia coli" or "epidermidis" from "Staphylococcus_epidermidis").
  #'
  #' @param taxonomy A character string with the organism's taxonomy, e.g.,
  #'   "d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Burkholderiales;f__Burkholderiaceae;g__Bordetella;s__Bordetella pseudohinzii".
  #' @param rank_sep The delimiter between ranks (e.g., ";" for QIIME2, "|" for MetaPhlAn).
  #' @param ranks Vector of rank names.
  #' @param prefixes Vector of corresponding prefixes.
  #' @param extract_species_epithet Logical. If `TRUE` (default), the species
  #'   column contains only the epithet (e.g., "coli" from "Escherichia coli").
  #'   If `FALSE`, the full species name is returned.
  #' @param sp_sep The delimiter between genus and species (e.g., " " for QIIME2, "_" for MetaPhlAn).
  #' 
  #' @return A named character vector with taxonomic ranks.
  #'
  #' @examples
  #' split_taxonomy_string("k__Bacteria; p__Firmicutes; c__Bacilli; o__Lactobacillales; f__Streptococcaceae; g__Streptococcus; s__Streptococcus pneumoniae", rank_sep = ";", sp_sep = " ")
  #' split_taxonomy_string("k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae|g__Streptococcus|s__Streptococcus_pneumoniae", rank_sep = "|", sp_sep = "_")
  split_taxonomy_string <- function(
    taxonomy,
    rank_sep = ";",
    ranks = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
    prefixes = paste0(substr(tolower(ranks), 1, 1), "__"),
    extract_species_epithet = TRUE,
    sp_sep = " "
  ) {
    parts <- strsplit(taxonomy, rank_sep, fixed = TRUE)[[1]]
    out <- setNames(rep(NA_character_, length(ranks)), ranks)
    
    for (item in parts) {
      item <- trimws(item)  # remove leading/trailing spaces
      for (i in seq_along(prefixes)) {
        prefix <- prefixes[i]
        if (startsWith(item, prefix)) {
          val <- sub(paste0("^", prefix), "", item)
          if (ranks[i] == "Species" && extract_species_epithet) {
            val <- sub(paste0(".*", sp_sep), "", val)
          }
          out[ranks[i]] <- val
          break
        }
      }
    }
    
    return(out)
  }
  
  #' Check if Data Follows DADA2 Format
  #'
  #' This function checks if the data contains columns expected in the DADA2 format.
  #'
  #' @param data Dataframe. The data to check.
  #' 
  #' @return Logical. TRUE if the data follows DADA2 format, FALSE otherwise.
  #' @export
  is_dada2_format <- function(data) {
    all(c("Phylum", "Class", "Order", "Family", "Genus", "Species") %in% colnames(data))
  }
  
  #' Check if Data Follows DADA2 Format
  #'
  #' This function checks if the data contains taxonomoy in DADA2-format.
  #' It does this by scanning all columns for the DADA2 taxonomy pattern.
  #'
  #' @param data Dataframe. The data to check.
  #' 
  #' @return Logical. TRUE if the data follows DADA2 format, FALSE otherwise.
  #' @export
  is_qiime2_format <- function(data) {
    if (!is.data.frame(data)) return(FALSE)
    
    any(
      sapply(data, function(col) {
        any(sapply(col, classify_taxonomy_string) == "qiime2", na.rm = TRUE)
      })
    )
  }
  
  #' Check if Data Follows MetaPhlAn Format
  #'
  #' This function checks if the data contains taxonomoy in MetaPhlAn-format.
  #' It does this by scanning all columns for the MetaPhlAn taxonomy pattern.
  #'
  #' @param data A dataframe to check.
  #' @return Logical. TRUE if any column contains MetaPhlAn-formatted taxonomy, FALSE otherwise.
  #' @export
  is_metaphlan_format <- function(data) {
    if (!is.data.frame(data)) return(FALSE)
    
    any(
      sapply(data, function(col) {
        any(sapply(col, classify_taxonomy_string) == "metaphlan", na.rm = TRUE)
      })
    )
  }
  
  #' Check if Data Follows IMG Genome Format
  #'
  #' This function checks if the data contains taxonomic columns expected in the IMG genome format.
  #' It detects either the NCBI taxonomy columns or the GTDB taxonomy columns.
  #'
  #' @param data Dataframe. The data to check.
  #'
  #' @return Logical. TRUE if the data follows the IMG genome format, FALSE otherwise.
  #' @export
  is_img_format <- function(data) {
    ncbi_cols <- c("NCBI Phylum", "NCBI Class", "NCBI Order", "NCBI Family", "NCBI Genus", "NCBI Species")
    gtdb_cols <- c("GTDB Phylum", "GTDB Class", "GTDB Order", "GTDB Family", "GTDB Genus", "GTDB Species")
    
    has_ncbi <- all(ncbi_cols %in% colnames(data))
    has_gtdb <- all(gtdb_cols %in% colnames(data))
    
    return(has_ncbi || has_gtdb)
  }
  
  #' Process DADA2 Format
  #'
  #' Selects the expected taxonomy columns from a DADA2-style dataframe.
  #'
  #' @param data Dataframe in DADA2 format.
  #'
  #' @return A dataframe with `Phylum` to `Species` columns.
  #' @export
  process_dada2_format <- function(data) {
    data <- data %>% dplyr::select(Phylum, Class, Order, Family, Genus, Species)
    
    return(data)
  }
  
  #' Process QIIME2 Format
  #'
  #' Splits QIIME2-style taxonomy strings into separate columns.
  #'
  #' @param data Dataframe that contains a column with QIIME2-formatted taxonomy.
  #'
  #' @return A dataframe with columns `Phylum`, `Class`, `Order`, `Family`, `Genus`, `Species`.
  #' @export
  process_qiime2_format <- function(data) {
    # Find the column that contains QIIME2-format taxonomy
    taxonomy_col <- names(data)[
      which.max(
        sapply(data, function(col) any(sapply(col, classify_taxonomy_string) == "qiime2", na.rm = TRUE))
      )
    ]
    
    if (is.na(taxonomy_col) || taxonomy_col == "") {
      stop("No QIIME2-formatted column found.")
    }
    
    # Split the strings into separate columns
    taxonomy_vec <- as.character(data[[taxonomy_col]])
    
    out_list <- vector("list", length(taxonomy_vec))
    for (i in seq_along(taxonomy_vec)) {
      out_list[[i]] <- split_taxonomy_string(
        taxonomy = taxonomy_vec[i],
        rank_sep = ";",
        sp_sep = " ",
        extract_species_epithet = TRUE
      )
    }
    
    # Combine results
    df <- do.call(rbind, out_list)
    rownames(df) <- NULL
    df <- (as.data.frame(df))
    
    return(df)
  }
  
  
  #' Process MetaPhlAn Format
  #'
  #' Splits MetaPhlAn-style taxonomy strings into separate columns.
  #' Only rows beginning with `k__` are included; others like `UNCLASSIFIED` are excluded.
  #'
  #' @param data Dataframe that contains a column with MetaPhlAn-formatted taxonomy.
  #'
  #' @return A dataframe with columns `Phylum`, `Class`, `Order`, `Family`, `Genus`, `Species`.
  #' @export
  process_metaphlan_format <- function(data) {
    # Find the column that contains MetaPhlAn-format taxonomy
    taxonomy_col <- names(data)[
      which.max(
        sapply(data, function(col) any(sapply(col, classify_taxonomy_string) == "metaphlan", na.rm = TRUE))
      )
    ]
    
    if (is.na(taxonomy_col) || taxonomy_col == "") {
      stop("No MetaPhlAn-formatted column found.")
    }
    
    # Split the strings into separate columns
    taxonomy_vec <- as.character(data[[taxonomy_col]])
    taxonomy_vec <- gsub("\\t.*$", "", taxonomy_vec)         # Strip extra fields
    taxonomy_vec <- taxonomy_vec[grepl("^k__", taxonomy_vec)] # Keep valid rows
    
    out_list <- vector("list", length(taxonomy_vec))
    for (i in seq_along(taxonomy_vec)) {
      out_list[[i]] <- split_taxonomy_string(
        taxonomy = taxonomy_vec[i],
        rank_sep = "|",
        sp_sep = "_",
        extract_species_epithet = TRUE
      )
    }
    
    # Combine results
    df <- do.call(rbind, out_list)
    rownames(df) <- NULL
    df <- (as.data.frame(df))
    
    return(df)
  }
  
  #' Process IMG Genome Format
  #'
  #' Selects and renames GTDB or NCBI taxonomy columns from IMG genome tables.
  #' By default, it prioritizes NCBI taxonomy columns unless `prefer = "GTDB"` is specified.
  #' It can also optionally extract just the species epithet (e.g., "coli" from "Escherichia coli").
  #'
  #' @param data Dataframe containing GTDB or NCBI taxonomy columns.
  #' @param prefer Character. Which taxonomy to prioritize if both are present: "NCBI" (default) or "GTDB".
  #' @param extract_species_epithet Logical. If `TRUE` (default), returns only the species epithet
  #'   in the `Species` column; if `FALSE`, returns full species name.
  #'
  #' @return A dataframe with columns `Phylum` to `Species`, or NULL if neither taxonomy is available.
  #' @export
  process_img_genome_format <- function(data, prefer = c("NCBI", "GTDB"), extract_species_epithet = TRUE) {
    prefer <- match.arg(prefer)
    
    gtdb_cols <- c("GTDB Phylum", "GTDB Class", "GTDB Order", "GTDB Family", "GTDB Genus", "GTDB Species")
    ncbi_cols <- c("NCBI Phylum", "NCBI Class", "NCBI Order", "NCBI Family", "NCBI Genus", "NCBI Species")
    
    has_gtdb <- all(gtdb_cols %in% colnames(data))
    has_ncbi <- all(ncbi_cols %in% colnames(data))
    
    select_and_rename <- function(cols, prefix) {
      df <- data %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::rename_with(~ gsub(paste0("^", prefix, " "), "", .x))
      
      if (extract_species_epithet && "Species" %in% colnames(df)) {
        df <- df %>%
          dplyr::mutate(Species = sub(".*\\s", "", Species))
      }
      
      return(df)
    }
    
    if (prefer == "NCBI" && has_ncbi) {
      return(select_and_rename(ncbi_cols, "NCBI"))
    } else if (prefer == "GTDB" && has_gtdb) {
      return(select_and_rename(gtdb_cols, "GTDB"))
    } else if (has_ncbi) {
      return(select_and_rename(ncbi_cols, "NCBI"))
    } else if (has_gtdb) {
      return(select_and_rename(gtdb_cols, "GTDB"))
    } else {
      return(NULL)
    }
  }
  
  #' Process Uploaded Taxonomy File
  #'
  #' This function processes an uploaded taxonomy file, detecting whether it follows
  #' the DADA2, QIIME2, MetaPhlan, or IMG genome format, and reformats the data 
  #' accordingly.
  #'
  #' @param query Dataframe. The uploaded taxonomy data.
  #'
  #' @return A processed dataframe with standard taxonomic ranks or NULL if format is unrecognized.
  #' @export
  process_uploaded_taxonomy <- function(query) {
    if (!is.data.frame(query)) return(NULL)
    
    if (is_dada2_format(query)) {
      return(process_dada2_format(query))
    } else if (is_qiime2_format(query)) {
      return(process_qiime2_format(query))
    } else if (is_metaphlan_format(query)) {
      return(process_metaphlan_format(query))
    } else if (is_img_format(query)) {
      return(process_img_genome_format(query))
    } else {
      return(NULL)
    }
  }
                
  #' Process IMG Genome Format
  #'
  #' Selects and renames GTDB or NCBI taxonomy columns from IMG genome tables.
  #' By default, it prioritizes NCBI taxonomy columns unless `prefer = "GTDB"` is specified.
  #' It can also optionally extract just the species epithet (e.g., "coli" from "Escherichia coli").
  #'
  #' @param data Dataframe containing GTDB or NCBI taxonomy columns.
  #' @param prefer Character. Which taxonomy to prioritize if both are present: "NCBI" (default) or "GTDB".
  #' @param extract_species_epithet Logical. If `TRUE` (default), returns only the species epithet
  #'   in the `Species` column; if `FALSE`, returns full species name.
  #'
  #' @return A dataframe with columns `Phylum` to `Species`, or NULL if neither taxonomy is available.
  #' @export
  process_img_genome_format <- function(data, prefer = c("NCBI", "GTDB"), extract_species_epithet = TRUE) {
    prefer <- match.arg(prefer)
    
    gtdb_cols <- c("GTDB Phylum", "GTDB Class", "GTDB Order", "GTDB Family", "GTDB Genus", "GTDB Species")
    ncbi_cols <- c("NCBI Phylum", "NCBI Class", "NCBI Order", "NCBI Family", "NCBI Genus", "NCBI Species")
    
    has_gtdb <- all(gtdb_cols %in% colnames(data))
    has_ncbi <- all(ncbi_cols %in% colnames(data))
    
    select_and_rename <- function(cols, prefix) {
      df <- data %>%
        dplyr::select(dplyr::all_of(cols)) %>%
        dplyr::rename_with(~ gsub(paste0("^", prefix, " "), "", .x))
      
      if (extract_species_epithet && "Species" %in% colnames(df)) {
        df <- df %>%
          dplyr::mutate(Species = sub(".*\\s", "", Species))
      }
      
      return(df)
    }
    
    if (prefer == "NCBI" && has_ncbi) {
      return(select_and_rename(ncbi_cols, "NCBI"))
    } else if (prefer == "GTDB" && has_gtdb) {
      return(select_and_rename(gtdb_cols, "GTDB"))
    } else if (has_ncbi) {
      return(select_and_rename(ncbi_cols, "NCBI"))
    } else if (has_gtdb) {
      return(select_and_rename(gtdb_cols, "GTDB"))
    } else {
      return(NULL)
    }
  }
  
  #' Process Uploaded Taxonomy File
  #'
  #' This function processes an uploaded taxonomy file, detecting whether it follows
  #' the DADA2, QIIME2, MetaPhlan, or IMG genome format, and reformats the data 
  #' accordingly.
  #'
  #' @param query Dataframe. The uploaded taxonomy data.
  #'
  #' @return A processed dataframe with standard taxonomic ranks or NULL if format is unrecognized.
  #' @export
  process_uploaded_taxonomy <- function(query) {
    if (!is.data.frame(query)) return(NULL)
    
    if (is_dada2_format(query)) {
      return(process_dada2_format(query))
    } else if (is_qiime2_format(query)) {
      return(process_qiime2_format(query))
    } else if (is_metaphlan_format(query)) {
      return(process_metaphlan_format(query))
    } else if (is_img_format(query)) {
      return(process_img_genome_format(query))
    } else {
      return(NULL)
    }
  }
  
# === Get inputs ===
  #' Get Query Taxa
  #'
  #' Processes the query taxa input from either a database selection or uploaded file.
  #'
  #' @param taxonomy_from_database Logical. Use database input?
  #' @param taxonomy_from_upload Logical. Use uploaded file?
  #' @param selected_organisms Character vector of selected taxa (if from database).
  #' @param taxonomy_upload_path File path to uploaded taxonomy file.
  #'
  #' @return A processed `query_taxa` dataframe.
  get_query_taxa <- function(taxonomy_from_database,
                             taxonomy_from_upload,
                             selected_organisms = NULL,
                             taxonomy_upload_path = NULL) {
    if (taxonomy_from_database) {
      query_taxa <- process_query_taxa(selected_organisms)
      runValidationModal(need(!is.null(query_taxa) && nrow(query_taxa) > 0, "Please choose a taxon"))
    } else if (taxonomy_from_upload) {
      query_taxa <- validate_and_read_file(file_path = taxonomy_upload_path)
      
      query_taxa <- process_uploaded_taxonomy(query_taxa)
      
      runValidationModal(need(!is.null(query_taxa) && nrow(query_taxa) > 0, "Please check the format of the taxonomy file and try again."))
    } else {
      stop("No valid taxonomy input specified.")
    }
    return(query_taxa)
  }
  
  #' Get Traits Input
  #'
  #' Validates trait inputs and processes query string for custom traits.
  #'
  #' @param traits_from_standard Logical. Is this a standard trait?
  #' @param traits_from_other Logical. Is this a custom trait?
  #' @param traits_to_predict Character vector of traits (if standard).
  #' @param query_string Raw query string (if custom trait).
  #'
  #' @return A named list with `traits_to_predict` and `query_string`.
  get_traits_input <- function(traits_from_standard,
                               traits_from_other,
                               traits_to_predict = NULL,
                               query_string = NULL) {
    if (traits_from_standard) {
      runValidationModal(need(!is.null(traits_to_predict) && length(traits_to_predict) > 0, "Please choose a trait"))
      return(list(traits_to_predict = traits_to_predict, query_string = NULL))
    } else if (traits_from_other) {
      query_string <- process_query_string(query_string)
      runValidationModal(need(query_string != "", "Please build a valid query."))
      return(list(traits_to_predict = "Custom trait", query_string = query_string))
    } else {
      stop("No valid trait source specified.")
    }
  }
  
  #' Get Inputs for Taxonomy Module
  #'
  #' This is the main function for getting all inputs for the module
  #'
  #' @param taxonomy_from_database Logical. Whether taxonomy is selected from database.
  #' @param taxonomy_from_upload Logical. Whether taxonomy is uploaded from a file.
  #' @param traits_from_standard Logical. Whether traits are standard.
  #' @param traits_from_other Logical. Whether trait is defined by a custom query.
  #' @param selected_organisms Character vector of selected taxa (if from database).
  #' @param taxonomy_upload_path File path to uploaded taxonomy file.
  #' @param traits_to_predict Character vector of traits to predict.
  #' @param query_string Raw query string for custom trait.
  #' @param ignore_NA Logical. Ignore NA in response variable?
  #' @param match_all_ranks Logical; if TRUE, all ranks must match. Default is FALSE
  #' @param ignore_species Logical. Collapse species to genus?
  #' @param system_taxonomy Logical. Use system taxonomy hierarchy?
  #'
  #' @return A named list of inputs for downstream processing.
  get_taxonomy_inputs <- function(
    taxonomy_from_database,
    taxonomy_from_upload,
    traits_from_standard,
    traits_from_other,
    selected_organisms = NULL,
    taxonomy_upload_path = NULL,
    traits_to_predict = NULL,
    query_string = NULL,
    ignore_NA,
    match_all_ranks,
    ignore_species,
    system_taxonomy
  ) {
    query_taxa <- get_query_taxa(
      taxonomy_from_database = taxonomy_from_database,
      taxonomy_from_upload = taxonomy_from_upload,
      selected_organisms = selected_organisms,
      taxonomy_upload_path = taxonomy_upload_path
    )
    
    traits_info <- get_traits_input(
      traits_from_standard = traits_from_standard,
      traits_from_other = traits_from_other,
      traits_to_predict = traits_to_predict,
      query_string = query_string
    )
    
    list(
      query_taxa = query_taxa,
      traits_to_predict = traits_info$traits_to_predict,
      query_string = traits_info$query_string,
      ignore_NA = ignore_NA,
      match_all_ranks = match_all_ranks,
      ignore_species = ignore_species,
      system_taxonomy = system_taxonomy
    )
  }
  
# === Other functions ===
  #' Create Organism Name from Taxonomy
  #'
  #' This helper function creates an organism name based on its taxonomy, using genus and species 
  #' or other taxonomic levels if genus and species are unavailable.
  #'
  #' @param phylum A character string specifying the phylum.
  #' @param class A character string specifying the class.
  #' @param order A character string specifying the order.
  #' @param family A character string specifying the family.
  #' @param genus A character string specifying the genus.
  #' @param species A character string specifying the species.
  #' @return A character string representing the organism name.
  #' @export
  create_organism_name <- function(phylum, class, order, family, genus, species) {
    if (!is.na(genus) & genus!="NA" & !is.na(species) & species!="NA") {
      return(paste(genus, species, sep = " "))
    } else if (!is.na(genus) & genus!="NA") {
      return(paste(genus, "spp.", sep = " "))
    } else if (!is.na(family) & family!="NA") {
      return(paste(family, "spp.", sep = " "))
    } else if (!is.na(order) & order!="NA") {
      return(paste(order, "spp.", sep = " "))
    } else if (!is.na(class) & class!="NA") {
      return(paste(family, "spp.", sep = " "))
    } else if (!is.na(phylum) & phylum!="NA") {
      return(paste(family, "spp.", sep = " "))
    } else {
      return(NA)
    }
  }
  
  #' Add Custom Traits Based on Query
  #'
  #' This function processes a data table by applying a query to identify organisms with 
  #' positive traits, then adds a column (default: 'Custom trait') to indicate which 
  #' organisms have these traits.
  #'
  #' @param data A dataframe containing the organism data.
  #' @param query_string A string representing the query used to filter the organisms.
  #' @param ignore_NA Logical. If TRUE, organisms with NA values are excluded from the final result. Default is FALSE.
  #' @param trait_name A string specifying the name of the new trait column. Default is "Custom trait".
  #' @return A dataframe with the trait column added, indicating positive traits.
  #' @export
  #' @importFrom dplyr mutate if_else left_join filter select
  #' @importFrom tidyr replace_na
  add_custom_traits <- function(data, query_string, ignore_NA = FALSE, trait_name = "Custom trait") {
    # Validate that the query string is not empty
    if(query_string == "") {
      stop("Please build a valid query.")
    }
    
    # Add an index column to the data
    data$index <- seq_len(nrow(data))
    
    # Get data for organisms with positive traits
    data_positive <- filter_data_by_query(data, query_string)

    # Add a new column 'Custom trait' to data_positive
    data_positive <- data_positive %>%
      dplyr::mutate(`Custom trait` = dplyr::if_else(dplyr::n() > 0, "positive", NA_character_))
    
    # Get data for all organisms (excluding those with NA values if specified)
    data_all <- if (ignore_NA) {
      filter_data_excluding_na(data, query_string)
    } else {
      data
    }

    # Merge the positive trait data back using the index column
    data <- dplyr::left_join(
      data_all,
      data_positive[, c("index", trait_name)],
      by = "index"
    )
    
    # Fill NA values in 'Custom trait' column with "-"
    data[[trait_name]] <- tidyr::replace_na(data[[trait_name]], "negative")

    return(data)
  }
  
  #' Get Organism and Taxonomy Information
  #'
  #' This function extracts organism names and their corresponding taxonomic classification
  #' from a cleaned database. The organism names are formatted as "Genus Species Subspecies",
  #' with missing subspecies values properly handled.
  #'
  #' @param database A dataframe containing organism data, including columns for taxonomy and organism names.
  #' @param phylum_col A character string specifying the column name that contains phylum information. Defaults to `"Phylum"`.
  #' @param class_col A character string specifying the column name that contains class information. Defaults to `"Class"`.
  #' @param order_col A character string specifying the column name that contains order information. Defaults to `"Order"`.
  #' @param family_col A character string specifying the column name that contains family information. Defaults to `"Family"`.
  #' @param genus_col A character string specifying the column name that contains genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains subspecies information. Defaults to `"Subspecies"`.
  #'
  #' @return A dataframe with columns: `Phylum`, `Class`, `Order`, `Family`, `Genus`, `Species`, and `Organism`.
  #'
  #' @examples
  #' # Example usage
  #' get_organism_by_taxonomy(database, 
  #'                          phylum_col = "Phylum",
  #'                          class_col = "Class",
  #'                          order_col = "Order",
  #'                          family_col = "Family",
  #'                          genus_col = "Genus_Name",
  #'                          species_col = "Species_Name",
  #'                          subspecies_col = "Subspecies_Name")
  #'
  #' @importFrom dplyr mutate rename select
  #' @export
  get_organism_by_taxonomy <- function(database, 
                                       phylum_col = "Phylum",
                                       class_col = "Class",
                                       order_col = "Order",
                                       family_col = "Family",
                                       genus_col = "Genus",
                                       species_col = "Species",
                                       subspecies_col = "Subspecies") {
    
    result <- database %>%
      dplyr::mutate(Organism = paste(.data[[genus_col]], .data[[species_col]], .data[[subspecies_col]], sep = " ")) %>%
      dplyr::mutate(Organism = gsub(" NA$", "", Organism)) %>%
      dplyr::select(.data[[phylum_col]], .data[[class_col]], .data[[order_col]], .data[[family_col]], 
                    .data[[genus_col]], .data[[species_col]], Organism)
    
    return(result)
  }
  
  #' Get Taxonomy Information for Selected Organisms
  #'
  #' This function retrieves the taxonomy information for the selected organisms
  #' from a provided organism-to-taxonomy mapping dataframe.
  #'
  #' @param organism_by_taxonomy Dataframe. The dataframe mapping organisms to their taxonomy.
  #' @param selected_organisms Character vector. The organisms selected by the user.
  #' 
  #' @return A dataframe containing taxonomy information (Phylum, Class, Order, Family, Genus, Species) 
  #'         for the selected organisms.
  #' @export
  #' @importFrom dplyr filter select
  get_taxonomy_for_selected_organisms <- function(organism_by_taxonomy, selected_organisms) {
    taxonomy_data <- organism_by_taxonomy %>%
      dplyr::filter(Organism %in% selected_organisms) %>%
      dplyr::select(Phylum, Class, Order, Family, Genus, Species)
    
    return(taxonomy_data)
  }
  
  #' Process Selected Taxa into a Query Dataframe
  #'
  #' This function converts a vector of taxon strings (formatted as "Taxon_Name (Rank)") 
  #' into a dataframe where the taxon is assigned to the correct rank column, with all 
  #' other ranks filled with `NA`.
  #'
  #' @param selected_organisms A character vector of taxon strings, where each entry 
  #'   follows the format "Taxon_Name (Rank)".
  #'
  #' @return A dataframe with columns "Phylum", "Class", "Order", "Family", "Genus", "Species", 
  #'   with the provided taxon names placed in the appropriate rank columns and all other 
  #'   values set to `NA`.
  #'
  #' @examples
  #' selected_taxa <- c("Abditibacteriota (Phylum)", "Bacilli (Class)", "Lactobacillales (Order)")
  #' process_query_taxa(selected_taxa)
  #'
  #' @export
  process_query_taxa <- function(selected_organisms) {
    # Define the fixed column names
    ranks <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    
    # Initialize an empty dataframe
    query <- as.data.frame(matrix(NA, nrow = length(selected_organisms), ncol = length(ranks)))
    colnames(query) <- ranks
    
    # Loop through each value in selected_organisms
    for (i in seq_along(selected_organisms)) {
      extracted <- gsub(" \\(.*\\)", "", selected_organisms[i])  # Remove the parenthetical part
      rank <- gsub(".*\\((.*)\\)", "\\1", selected_organisms[i]) # Extract the rank
      
      # Assign the extracted value to the correct column, if the rank exists
      if (rank %in% ranks) {
        query[i, rank] <- extracted
      } else {
        warning(sprintf("Invalid rank: '%s' in '%s'. Expected one of %s", 
                        rank, selected_organisms[i], paste(ranks, collapse = ", ")))
      }
    }
    
    return(query)
  }
  
  #' Clean taxon data by replacing placeholders, removing suffixes, and stripping brackets
  #'
  #' This function replaces common placeholder values (e.g., "", "?", "unclassified") with `NA`,
  #' removes specified suffixes (e.g., "_unclassified"), and strips characters from specified brackets.
  #'
  #' @param df A data frame of taxonomic data.
  #' @param missing_values A character vector of values to treat as missing. Defaults to "", "?", and "unclassified".
  #' @param suffixes_to_remove A character vector of suffixes to strip from values (e.g., "_unclassified").
  #' @param brackets_to_remove A string of bracket characters to remove (e.g., "[]{}()"). Defaults to "[]".
  #'
  #' @return A cleaned data frame.
  #' @export
  #'
  #' @examples
  #' cleaned <- clean_taxon_data(taxon_df)
  clean_taxon_data <- function(
    df,
    missing_values = c("", "?", "unclassified"),
    suffixes_to_remove = c("_unclassified"),
    brackets_to_remove = "[]"
  ) {
    # Escape bracket characters for regex
    brackets_regex <- paste0("[", gsub("([][(){}.^$*+?|\\\\])", "\\\\\\1", brackets_to_remove), "]")
    
    df[] <- lapply(df, function(x) {
      if (is.character(x) || is.factor(x)) {
        x <- as.character(x)
        x[x %in% missing_values] <- NA
        x <- gsub(brackets_regex, "", x)  # Remove bracket characters
        
        for (suffix in suffixes_to_remove) {
          pattern <- paste0(suffix, "$")
          x <- gsub(pattern, "", x)
        }
      }
      return(x)
    })
    return(as.data.frame(df))
  }
  
  #' Format Query Taxa Table
  #'
  #' This function cleans and formats a query taxa dataframe by replacing missing values.
  #'
  #' @param query A dataframe of taxonomic ranks for query organisms.
  #'
  #' @return A formatted version of the query dataframe.
  #' @export
  format_query_taxa <- function(query) {
    query <- clean_taxon_data(query)

    return(query)
  }

  #' Get Choices for Taxa
  #'
  #' This function extracts unique taxonomic names from a dataframe and appends 
  #' the corresponding rank in parentheses.
  #'
  #' @param df A dataframe containing taxonomic ranks as columns (e.g., "Phylum", "Class", 
  #'   "Order", "Family", "Genus", "Species"), with taxon names as values.
  #'
  #' @return A character vector of unique taxon names formatted as "Taxon_Name (Rank)".
  #'
  #' @examples
  #' taxon_df <- data.frame(
  #'   Phylum = c("Abditibacteriota", "Bacillota", "Bacillota"),
  #'   Class = c("Abditibacteriia", "Bacilli", "Erysipelotrichia"),
  #'   Order = c("Abditibacteriales", "Lactobacillales", "Erysipelotrichales")
  #' )
  #' 
  #' get_taxon_choices(taxon_df)
  #'
  #' @export
  get_taxon_choices <- function(data) {
    # Get column names as taxonomic ranks
    ranks <- c("Phylum", "Class", "Order", "Family", "Genus")
    
    data <- data %>% dplyr::select(dplyr::all_of(ranks))
    
    # Convert the dataframe into a vector with the required format
    choices <- unlist(mapply(function(col, rank) {
      paste(data[[col]], sprintf("(%s)", rank))
    }, colnames(data), ranks, SIMPLIFY = FALSE))
    
    # Ensure unique values and remove names
    choices <- unique(unname(choices))
    
    return(choices)
  }
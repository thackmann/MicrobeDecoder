# Functions for Loading Data for Shiny App
# This script contains functions for loading files and objects used by the app.
# These functions are called within modules of the app.  Loading with functions
# on demand speeds up app execution.  
# Author: Timothy Hackmann
# Date: 26 February 2025

# === Define functions ===
  # --- Utility functions for loading data ---
  #' Check if an Object Exists and Load if Not Present
  #'
  #' This function checks if an object exists in the environment and loads it from a file if it is not present.
  #' It supports loading from CSV, TSV, TXT, XLS, XLSX, RDS, and ZIP files containing these formats. 
  #' Optionally, it can force reload even if the object is already in the environment.
  #'
  #' @param file_path A character string specifying the path to the file.
  #' @param name The name of the object to load. If NULL, the object name is inferred from the file name.
  #' @param load_function The function to use for loading the file. If NULL, it is inferred from the file extension.
  #' @param envir The environment where the object should be loaded. Default is the global environment.
  #' @param force_reload Logical; if TRUE, reloads the object even if it exists in the environment.
  #' @param ... Additional arguments passed to the load function.
  #' @return The loaded object.
  #' @export
  #' @importFrom tools file_ext
  check_and_load <- function(file_path, name = NULL, load_function = NULL, envir = globalenv(), force_reload = FALSE, ...) {
    # Extract the object name if not provided
    if (is.null(name)) {
      file_name <- basename(file_path)  # Get the file name from the path
      name <- sub("\\..*$", "", file_name)  # Remove the extension to get the object name
    }
    
    # Check if the object exists and return it if force_reload is FALSE
    if (!force_reload && exists(name, envir = envir)) {
      return(get(name, envir = envir))
    }
    
    # Determine the file extension
    extension <- tools::file_ext(file_path)
    
    # Handle ZIP files
    if (tolower(extension) == "zip") {
      z <- extract_from_zip(file_path)
      # Ensure temp_dir gets cleaned up even if downstream code errors
      on.exit(unlink(z$temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
      file_path <- z$path
      extension <- z$extension
    }
    
    # Determine the load function
    if (is.null(load_function)) {
      load_function <- switch(extension,
                              "rds" = readRDS,
                              "csv" = function(file, ...) readr::read_csv(file, show_col_types = FALSE, ...),
                              "tsv" = function(file, ...) readr::read_tsv(file, show_col_types = FALSE, ...),
                              "txt" = function(file, ...) readr::read_delim(file, delim = "\t", show_col_types = FALSE, ...),
                              "xlsx" = function(file, ...) readxl::read_excel(path = file, ...),
                              "xls"  = function(file, ...) readxl::read_excel(path = file, ...),
                              "ko"  = function(file, ...) read.table(file, sep = "\t", header = FALSE, fill = TRUE, ...),
                              stop("Unsupported file extension"))
    }
    
    # Load the object
    obj <- do.call(load_function, c(list(file_path), list(...)))  # Pass additional arguments dynamically
    assign(name, obj, envir = envir)
    
    return(obj)
  }
  
  #' Load a Data File by Key
  #'
  #' This function looks up a key in `data_registry` and loads the corresponding
  #' file with `check_and_load`.
  #'
  #' @param key A character string naming an entry in `data_registry`.
  #' @param force_reload Logical; if TRUE, reloads the object even if it exists in the environment.
  #' @return The loaded object.
  #' @export
  load_data <- function(key, force_reload = FALSE) {
    if (!key %in% names(data_registry)) {
      stop("Unknown data key: ", key)
    }

    check_and_load(file_path = data_registry[[key]], force_reload = force_reload)
  }

  # --- Functions for loading specific objects or files ---
  #' Load the Database
  #'
  #' This function loads the database from a zipped CSV file. The clean
  #' (formatted) database is loaded by default; the raw (unformatted) database is
  #' loaded with `type = "raw"`. Each type has its own column types. The data is
  #' stored in the environment if it is not already present, unless
  #' `force_reload = TRUE` is specified.
  #'
  #' @param type The database to load, either "clean" (default) or "raw".
  #' @param force_reload Logical; if TRUE, reloads the database even if it exists in the environment.
  #' @return A data frame containing the database.
  #' @export
  #' @importFrom readr cols col_character
  load_database <- function(type = c("clean", "raw"), force_reload = FALSE) {
    type <- match.arg(type)

    path <- switch(type,
                   clean = "data/database/database_clean.zip",
                   raw   = "data/database/database.zip")

    col_types <- switch(type,
                        clean = readr::cols(
                          `IMG Genome ID` = readr::col_character(),
                          `NCBI Taxonomy ID` = readr::col_character()
                        ),
                        raw = readr::cols(
                          IMG_Genome_ID = readr::col_character(),
                          NCBI_Taxonomy_ID = readr::col_character()
                        ))

    check_and_load(file_path = path, force_reload = force_reload, col_types = col_types)
  }

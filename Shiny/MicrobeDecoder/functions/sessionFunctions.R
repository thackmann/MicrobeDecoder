# Initialize Session for App
# This defines a functions for initializing a session in the app.  It includes
# functions for loading functions and plugins.  The functions are loaded by
# sourcing R files and plugins by loading package name spaces/
# Timothy Hackmann
# Date: 14 May 25

#' Source R Files in Specified Subdirectories
#'
#' Recursively sources all `.R` or `.r` files from subdirectories,
#' excluding files in paths that contain any of the `exclude` folder names.
#'
#' @param base_dir Parent directory to search. Defaults to `getwd()`.
#' @param subdirs Subdirectories to include. Defaults to common folders.
#' @param exclude Character vector of folder names to exclude (e.g., "old", "deprecated").
#' @param verbose Whether to print each file being sourced. Default: TRUE.
#' @param local Whether to source in local environment. Default: TRUE.
#'
#' @return Invisibly returns a vector of sourced file paths.
source_r_files <- function(base_dir = getwd(),
                           subdirs = c("install", "functions", "variables", "modules"),
                           exclude = c("old"),
                           verbose = TRUE,
                           local = FALSE) {
  sourced_files <- c()
  file_timings <- list()
  
  for (dir in subdirs) {
    full_dir <- file.path(base_dir, dir)
    if (!dir.exists(full_dir)) {
      warning("Directory does not exist: ", full_dir)
      next
    }
    
    files <- list.files(path = full_dir, pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
    
    if (!is.null(exclude)) {
      # Exclude files that contain any of the folder names in their full path
      exclude_patterns <- paste0("([/\\\\])", exclude, "([/\\\\]|$)")
      combined_pattern <- paste0(exclude_patterns, collapse = "|")
      files <- files[!grepl(combined_pattern, files)]
    }
    
    for (file in files) {
      if (verbose) message("Sourcing: ", file)
      t_file <- Sys.time()
      source(file, local = local)
      file_timings[[file]] <- as.numeric(difftime(Sys.time(), t_file, units = "secs"))
      sourced_files <- c(sourced_files, file)
    }
  }
  
  # Attach the per-file source times so callers can find slow files.
  attr(sourced_files, "file_timings") <- unlist(file_timings)
  
  invisible(sourced_files)
}

#' Load package namespaces
#'
#' This function loads the namespaces for installed R packages without
#' attaching the packages to the search path. Packages that are not installed
#' are skipped.
#'
#' @param packages A character vector of package names to load.
#' @param verbose Logical. If \code{TRUE}, print a message for each namespace
#'   loaded and each package skipped. Default \code{FALSE}.
#' @return Invisibly, a character vector of the package namespaces that were
#'   loaded.
#' @export
load_namespaces <- function(packages, verbose = FALSE) {
  loaded <- character(0)
  
  for (package in packages) {
    if (requireNamespace(package, quietly = TRUE)) {
      loaded <- c(loaded, package)
      
      if (verbose) {
        message("Loaded namespace: ", package)
      }
    } else if (verbose) {
      message("Package not installed, skipping: ", package)
    }
  }
  
  invisible(loaded)
}

#' Initialize an R session for the app
#'
#' This functions sets up the R session needs before it can run app
#' code.  It loads all functions, loads plugins for the solver, and serves
#' example files for download.  The main session and each background worker 
#' are separate R sessions, so each must call this. 
#'
#' Assumes \code{source_r_files} is already available (from sourceFunctions.R).
#'
#' @param app_dir The app's working directory.
#' @param verbose Logical; passed to \code{source_r_files}. Default \code{FALSE}.
#' @return Invisibly \code{TRUE}.
#' @export
initialize_session <- function(app_dir = getwd(), verbose = FALSE) {
  subdirs <- c("install", "variables", "functions", "modules")
  
  # Load functions
  sourced <- source_r_files(
    base_dir = app_dir,
    subdirs  = subdirs,
    exclude  = c("old"),
    verbose  = verbose,
    local    = FALSE
  )

  # Load plugins
  t_plugins <- Sys.time()
  # plugins <- c("ROI.plugin.ecos", "ROI.plugin.glpk")
  plugins <- c("ROI.plugin.glpk")
  load_namespaces(plugins)
  
  # Serve example files
  shiny::addResourcePath("examples_data", "data/examples")
}
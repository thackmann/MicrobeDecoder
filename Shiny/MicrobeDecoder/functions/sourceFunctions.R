# Read R Files from Source 
# This defines a single function for sourcing R files from specified subdirectories
# After calling it, files with all remaining functions can be sourced.
# Timothy Hackmann
# 14 May 25

# Read R Files from Source 
# This defines a single function for sourcing R files from specified subdirectories
# After calling it, files with all remaining functions can be sourced.
# Timothy Hackmann
# Modified: 14 May 25

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
      source(file, local = local)
      sourced_files <- c(sourced_files, file)
    }
  }
  
  invisible(sourced_files)
}

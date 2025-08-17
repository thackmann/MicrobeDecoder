# Run Tests for Shiny App
# This script tests the Shiny app by running it and comparing outputs to
# expected values.  It is run according to an external test script.  
# The expected values are those recorded from a previous run, where outputs were 
# manually reviewed for accuracy.  
# Requirements
# -External test script at testthat/test-shinytest2.R
# -Expected results at testthat/_snaps/shinytest2
# Author: Timothy Hackmann
# Date: 14 Jul 2025

# === Get app directory ===
  app_directory <- FileLocator::getCurrentFileLocation()
  app_directory <- dirname(app_directory)

# === Run tests ===
  setwd(app_directory)
  shinytest2::test_app(path_to_app = app_directory) # Runs external test script
  
# === Record new tests ===  
  # Uncomment to run; do this if only needing to record new tests
  # shinytest2::record_test(app_directory)
    
# === Record expected results ===
  # Uncomment to run; do this if only needing to replace previous results
  # results_directory <- FileLocator::getCurrentFileLocation()
  # results_directory <- paste0(results_directory, "tests/testthat/_snaps/windows-4.4/shinytest2")
  # files_to_delete <- list.files(results_directory, full.names = TRUE)
  # file.remove(files_to_delete)
  # shinytest2::test_app(path_to_app = app_directory)
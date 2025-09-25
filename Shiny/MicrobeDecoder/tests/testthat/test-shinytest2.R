# Define Tests for Shiny App
# This script defines tests to run on the Shiny app.  It is used by runTests.R to
# to run the app and comparing outputs to expected values.  Tests are defined
# for each of the modules and consist of setting inputs, performing actions 
# (e.g., making predictions), and recording outputs.  
# Author: Timothy Hackmann
# Date: 14 Jul 2025

# Predictions from taxonomy
testthat::test_that("predictionsTaxonomyResults", {
  # Initialize test
  app <- AppDriver$new(name = "predictionsTaxonomyResults", variant = platform_variant())
  app$set_window_size(width = 1619, height = 945)

  # Launch browser (comment to make execution hidden)
  app$view()

  # Set inputs
  app$set_inputs(tabs = "predictionsTaxonomy")
  app$set_inputs(`predictionsTaxonomy-set_traits` = c("Type of metabolism (FAPROTAX)",
      "Type of metabolism (Fermentation Explorer)", "Metabolites utilized (BacDive)",
      "Metabolites produced (BacDive)", "Metabolites utilized (Fermentation Explorer)",
      "Metabolites produced (Fermentation Explorer)"))
  app$set_inputs(`predictionsTaxonomy-taxonomy_database` = "Escherichia (Genus)")

  # Perform actions
  Sys.sleep(10)
  app$click("predictionsTaxonomy-make_predictions")
  Sys.sleep(30)

  # Record output
  app$set_inputs(`predictionsTaxonomy-results_tabs` = "Heatmap")
  Sys.sleep(5)
  app$expect_values(output = "predictionsTaxonomy-heatmap_plot")
})

# Predictions with metabolic networks
testthat::test_that("predictionsNetworkResults", {
  # Initialize test
  app <- AppDriver$new(name = "predictionsNetworkResults", variant = platform_variant())
  app$set_window_size(width = 1619, height = 945)

  # Launch browser (comment to make execution hidden)
  app$view()

  # Set inputs
  app$set_inputs(tabs = "predictionsNetwork")
  app$set_inputs(`predictionsNetwork-reference_network_database` = "Fermentation of glucose")
  app$click("predictionsNetwork-update_gene_function_choices")

  # Perform actions
  Sys.sleep(10)
  app$click("predictionsNetwork-make_predictions")
  Sys.sleep(30)

  # Record output
  app$set_inputs(`predictionsNetwork-results_tabs` = "Heatmap")
  Sys.sleep(5)
  app$expect_values(output = "predictionsNetwork-heatmap_plot")
})

# Predictions with machine learning
testthat::test_that("predictionsMachineLearningResults", {
  # Initialize test
  app <- AppDriver$new(name = "predictionsMachineLearningResults", variant = platform_variant())
  app$set_window_size(width = 1619, height = 945)

  # Launch browser (comment to make execution hidden)
  app$view()

  # Set inputs
  app$set_inputs(tabs = "predictionsMachineLearning")
  # app$set_inputs(`predictionsMachineLearning-gene_functions_database` = "Escherichia coli")
  app$set_inputs(`predictionsMachineLearning-model_names` = c("Fermentation (type of metabolism)",
      "Methanogenesis (type of metabolism)"))

  # Perform actions
  Sys.sleep(10)
  app$click("predictionsMachineLearning-make_predictions")
  Sys.sleep(30)

  # Record output
  app$set_inputs(`predictionsMachineLearning-results_tabs` = "Heatmap")
  Sys.sleep(5)
  app$expect_values(output = "predictionsMachineLearning-heatmap_plot")
})

# Database search
testthat::test_that("databaseSearchResults", {
  # Initialize test
  app <- AppDriver$new(name = "databaseSearchResults", variant = platform_variant())
  app$set_window_size(width = 1619, height = 945)

  # Launch browser (comment to make execution hidden)
  app$view()

  # Set inputs
  app$set_inputs(tabs = "databaseSearch")

  # Perform actions
  Sys.sleep(10)
  app$click("databaseSearch-make_predictions")
  Sys.sleep(30)

  # Record output
  app$expect_values(output = "databaseSearch-treemap_plot")
})
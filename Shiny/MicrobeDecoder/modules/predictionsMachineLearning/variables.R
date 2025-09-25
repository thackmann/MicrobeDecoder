# Define Variables for Predictions Using Machine Learning Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# Choices for variables
choices_traits_ML = c(
  metabolism_var, 
  physiology_var,
  growth_var,
  morphology_var,
  isolation_var
)

# File paths for random forest models
model_path_config <- list(
  # Type of metabolism
  `Aerobic chemoheterotrophy (type of metabolism)` = "data/random_forest_models/aerobic_chemoheterotrophy.rds",
  `Fermentation (type of metabolism)` = "data/random_forest_models/fermentation.rds",
  `Nitrate reduction (type of metabolism)` = "data/random_forest_models/nitrate_reduction.rds",
  `Sulfur compound respiration (type of metabolism)` = "data/random_forest_models/sulfur_compound_respiration.rds",
  `Sulfate respiration (type of metabolism)` = "data/random_forest_models/sulfate_respiration.rds",
  `Sulfur respiration (type of metabolism)` = "data/random_forest_models/sulfur_respiration.rds",
  `Phototrophy (type of metabolism)` = "data/random_forest_models/phototrophy.rds",
  `Methanogenesis (type of metabolism)` = "data/random_forest_models/methanogenesis.rds",
  
  # Metabolites produced
  `Acetate (end product)` = "data/random_forest_models/acetate.rds",
  `Butyrate (end product)` = "data/random_forest_models/butyrate.rds",
  `CH4 (end product)` = "data/random_forest_models/CH4.rds",
  `Ethanol (end product)` = "data/random_forest_models/ethanol.rds",
  `Formate (end product)` = "data/random_forest_models/formate.rds",
  `H2 (end product)` = "data/random_forest_models/H2.rds",
  `Isobutyrate (end product)` = "data/random_forest_models/isobutyrate.rds",
  `Isovalerate (end product)` = "data/random_forest_models/isovalerate.rds",
  `Lactate (end product)` = "data/random_forest_models/lactate.rds",
  `Propionate (end product)` = "data/random_forest_models/propionate.rds",
  `Pyruvate (end product)` = "data/random_forest_models/pyruvate.rds",
  `Succinate (end product)` = "data/random_forest_models/succinate.rds",
  
  # Physiology/morphology
  `Anaerobe (oxygen tolerance)` = "data/random_forest_models/anaerobe.rds",
  `Gram positive (gram stain)` = "data/random_forest_models/gram_positive.rds",
  `Spore positive (spore formation)` = "data/random_forest_models/spore_formation.rds",
  `Motility positive (motility)` = "data/random_forest_models/motility.rds",
  
  # Growth
  `Slow growth (incubation period)` = "data/random_forest_models/slow_growth.rds"
)

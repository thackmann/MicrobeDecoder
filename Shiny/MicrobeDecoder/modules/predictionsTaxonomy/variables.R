# Define Variables for Predictions from Taxonomy Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# Choices for variables
  choices_traits_taxonomy <- c(
    metabolism_var, 
    physiology_var, 
    morphology_var, 
    growth_var, 
    isolation_var
  )
  
  choices_system_taxonomy <- c("LPSN", "GTDB", "NCBI", "Bergey")
  
  poor_traits_taxonomy <- c(
      # "Type of metabolism (FAPROTAX)", "Type of metabolism (Fermentation Explorer)", 
      "Metabolites produced (BacDive)", "Metabolites utilized (BacDive)",
      # "Metabolites produced (Fermentation Explorer)", 
      "Major metabolites produced (Fermentation Explorer)", 
      "Minor metabolites produced (Fermentation Explorer)", 
      # "Metabolites utilized (Fermentation Explorer)",
      "Enzyme activity (BacDive)",
      "Indole test (BacDive)", "Voges Proskauer (BacDive)", 
      # "Motility (BacDive)", 
      "Antibiotic resistance (BacDive)", 
      "Antibiotic sensitivity (BacDive)",
      "Cell shape (BacDive)", "Cell length in microns (BacDive)", "Cell width in microns (BacDive)",
      "Flagellum arrangement (BacDive)",
      # "Gram stain (BacDive)", "Spore formation (BacDive)"
    growth_var,
    isolation_var
  )
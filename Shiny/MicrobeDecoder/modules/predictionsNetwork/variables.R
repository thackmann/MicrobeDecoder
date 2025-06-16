# Define Variables for Predictions with Metabolic Networks Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 17 Apr 2025

# Common enzyme cofactors in metabolic networks
enzyme_cofactors <- c(
  "NAD+", "NADH", "NADP+", "NADPH", "FAD", "FADH2", "Reduced ferredoxin", "Oxidized ferredoxin",
  "Oxidized flavodoxin", "Reduced flavodoxin", 
  "Ubiquinone", "Ubiquinol", "Quinone", "Hydroquinone", "Menaquinone", "Menaquinol",
  "Oxidized hydrogenase", "Reduced hydrogenase",
  "Acceptor", "Reduced acceptor",
  "Ferricytochrome c", "Ferrocytochrome c", "Ferricytochrome cL", "Ferrocytochrome cL",
  "ATP", "ADP", "AMP", "GTP", "GDP", "NDP", "Nucleoside triphosphate",
  "Orthophosphate", "Diphosphate", "Polyphosphate",
  
  "Protein histidine", "Protein N(tau)-phospho-L-histidine", "Protein N(pi)-phospho-L-histidine", 
  
  "Glutathione",
  
  "CoA", 
  "Thiamin diphosphate",
  "Enzyme N6-(dihydrolipoyl)lysine",
  # "2-(alpha-Hydroxyethyl)thiamine diphosphate",
  # "[Dihydrolipoyllysine-residue acetyltransferase] S-acetyldihydrolipoyllysine",
   
  "Enzyme N6-(lipoyl)lysine",
  # "3-Carboxy-1-hydroxypropyl-ThPP",
  # "[Dihydrolipoyllysine-residue succinyltransferase] S-succinyldihydrolipoyllysine"
  
  "H+", "H2O"
)
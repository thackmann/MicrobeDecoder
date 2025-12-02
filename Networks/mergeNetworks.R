# Merge Networks
# This script takes individual metabolic networks and merges them.  The 
# individual metabolic networks are from KEGG maps (e.g., map00010) and other 
# KEGG reactions.  Any errors are resolved manually 
# with by hand-coded exceptions.  
# Requirements
# -Individual networks from keggPathwaysToNetworks.R and otherReactionsToNetworks.R.
# Author: Timothy Hackmann
# Date:  25 April 2025

# === Set directory ===
  network_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(network_directory)
  source("functions/helperFunctions.R", local = TRUE)

# === Load network files ===
  fp <- paste0(network_directory, "/data/keggNetworks/")
  network_files <- list.files(path = fp, pattern = ".csv$", full.names = TRUE)
  
  if (length(network_files) == 0) {
    stop("No network files containing 'map' were found in the directory.")
  }
  
  # Read and combine all network files
  merged_network <- lapply(network_files, function(file) {
    message("Reading: ", basename(file))
    readr::read_csv(file, show_col_types = FALSE)
  }) %>%
    dplyr::bind_rows()

# === Manually fix errors ===  
  # Fill in missing information (for most reactions)
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K14086",
      new_eq = "2 Reduced ferredoxin + 2 H+ + 2 H+[side 1] <=> Hydrogen + 2 Oxidized ferredoxin + 2 H+[side 2]",
      new_ec = "7.1.1.B",
      new_rn = "R7.1.1.B"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "1.6.1.4",
      new_eq = "NADH + H+ + 2 NADP+ + 2 Reduced ferredoxin <=> NAD+ + 2 NADPH + 2 Oxidized ferredoxin",
      new_ec = "1.6.1.4",
      new_rn = "R1.6.1.4"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.2.1.1",
      new_eq = c(
        "NADH + H+ + Ubiquinone + Na+[side 1] <=> NAD+ + Ubiquinol + Na+[side 2]",
        "NADH + H+ + Quinone + Na+[side 1] <=> NAD+ + Hydroquinone + Na+[side 2]"
      ),
      new_rn = c(
        "R7.2.1.1",
        "R7.2.1.A"
      )
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.2.4.3",
      new_eq = "(S)-Methylmalonyl-CoA + Na+[side 1] + H+[side 2] <=> Propanoyl-CoA + CO2 + Na+[side 2]",
      new_rn = "R7.2.4.3"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.2.2.1",
      new_eq = "ATP + H2O + Na+[side 1] <=> ADP + Orthophosphate + Na+[side 2]",
      new_rn = "R7.2.2.1"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K02784",
      new_eq = "Phosphoenolpyruvate + Protein histidine <=> Pyruvate + Protein N(pi)-phospho-L-histidine",
      new_ec = "2.7.3.9",
      new_rn = "R02628"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.2.1.2",
      new_eq = "2 Reduced ferredoxin + NAD+ + H+ + Na+[side 1] <=> 2 Oxidized ferredoxin + NADH + Na+[side 2]",
      new_rn = "R7.2.1.2"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.2.3.1",
      new_eq = "Diphosphate + H2O + Na+[side 1] <=> 2 Orthophosphate + Na+[side 2]",
      new_rn = "R7.2.3.1"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "1.16.9.1",
      new_eq = "Fe2+ + Rusticyanin <=> Fe3+ + Reduced rusticyanin",
      new_rn = "R1.16.9.1"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K03532",
      new_eq = "Trimethylamine + 2 Quinone + H2O <=> Trimethylamine N-oxide + 2 Hydroquinone + 2 H+",
      new_ec = "1.7.2.A",
      new_rn = "R1.7.2.A"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ec_match = "7.1.1.10",
      new_eq = "2 Reduced ferredoxin + Plastoquinone + 6 H+[side 1] <=> 2 Oxidized ferredoxin + Plastoquinol + 7 H+[side 2]",
      new_ec = "7.1.1.10",
      new_rn = "R7.1.1.10"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K08928",
      new_eq = c(
        "2 Ferrocytochrome c + Quinone + 2 H+[side 1] + hn <=> 2 Ferricytochrome c + Hydroquinone",
        "Hydroquinone + 2 Ferricytochrome c <=> Quinone + 2 Ferrocytochrome c + 2 H+[side 2]"
      ),
      new_ec = c(
        "1.97.1.A",
        "7.1.1.B"
      ),
      new_rn = c(
        "R1.97.1.A",
        "R7.1.1.C"
      )
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K08940",
      new_eq = c(
        "Ferrocytochrome c + Oxidized ferredoxin + hn <=> Ferricytochrome c + Reduced ferredoxin",
        "Reduced ferredoxin + Ferricytochrome c + H+[side 1] <=> Oxidized ferredoxin + Ferrocytochrome c + H+[side 2]"
      ),
      new_ec = c(
        "1.97.1.B",
        "7.1.1.C"
      ),
      new_rn = c(
        "R1.97.1.B",
        "R7.1.1.D"
      )
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K04641",
      new_eq = "H+[side 1] + hn <=> H+[side 2]",
      new_ec = "R7.1.A.A",
      new_rn = "R7.1.A.A"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K16293",
      new_eq = "Hydrogen + 2 Polysulfide sulfur <=> Hydrogen sulfide + Polysulfide sulfur",
      new_ec = "1.12.98.4",
      new_rn = "R10390"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K16294",
      new_eq = "Hydrogen + 2 Polysulfide sulfur <=> Hydrogen sulfide + Polysulfide sulfur",
      new_ec = "1.12.98.4",
      new_rn = "R10390"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K16933",
      new_eq = "2 Hydroquinone + Oxygen + n H+[side 1] <=> 2 Quinone + 2 H2O + n H+[side 2]",
      new_ec = "7.1.1.A",
      new_rn = "R7.1.1.A"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K16934",
      new_eq = "2 Hydroquinone + Oxygen + n H+[side 1] <=> 2 Quinone + 2 H2O + n H+[side 2]",
      new_ec = "7.1.1.A",
      new_rn = "R7.1.1.A"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K21566",
      new_eq = "(4-Hydroxyphenyl)acetate + Cl- + Quinone + H+ <=> 3-Chloro-4-hydroxyphenylacetate + Hydroquinone",
      new_ec = "1.21.99.A",
      new_rn = "R1.21.99.A"
    )
  
  # Correct wrong information
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "k21566",
      new_eq = "2 Reduced ferredoxin + 2 H+ + Na+[side 1] <=> Hydrogen + 2 Oxidized ferredoxin + Na+[side 2]",
      new_ec = "7.2.1.B",
      new_rn = "R7.2.1.B"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K11942",
      new_eq = "(R)-Methylmalonyl-CoA <=> Succinyl-CoA",
      new_ec = "5.4.99.2",
      new_rn = "R00833"
    )
  
  merged_network <- merged_network %>% 
    fill_info(
      ko_match = "K02567",
      new_eq = "2 Ferrocytochrome c + Nitrate <=> 2 Ferricytochrome c + Nitrite + H2O",
      new_rn = "R03071",
      new_ec = "1.9.6.1"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      ko_match = "K08352",
      old_eq = "Thiosulfate + Menaquinol <=> Sulfite + Hydrogen sulfide + Menaquinone",
      new_eq = c(
        "Thiosulfate + Menaquinol <=> Sulfite + Hydrogen sulfide + Menaquinone",
        "Hydrogen + 2 Polysulfide sulfur <=> Hydrogen sulfide + Polysulfide sulfur"
      ),
      new_ec = c(
        "1.8.5.5",
        "1.12.98.4"
      ),
      new_rn = c(
        "R10149",
        "R10390")
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "n Hydrogen sulfide + n Quinone <=> Polysulfide + n Hydroquinone",
      new_eq = "Hydrogen sulfide + Quinone <=> Polysulfide sulfur + Hydroquinone",
      new_ec = "1.8.5.4",
      new_rn = "R1.8.5.4"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      ko_match = "K00234",
      old_eq = "Quinone + Succinate <=> Hydroquinone + Fumarate",
      new_eq = "Ubiquinone + Succinate <=> Ubiquinol + Fumarate",
      new_ec = "1.3.5.1",
      new_rn = "R13223"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      rn_match = "R00923",
      new_ec = "4.1.1.A"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Selenite + H2O + Ferricytochrome c <=> Selenate + Ferrocytochrome c",
      new_eq = "Selenite + H2O + 2 Ferricytochrome c <=> Selenate + 2 Ferrocytochrome c",
      new_ec = "1.97.1.9",
      new_rn = "R1.97.1.9"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "ATP + H2O <=> ADP + Orthophosphate",
      new_eq = "ATP + H2O + 4 H+[side 1] <=> ADP + Orthophosphate + 4 H+[side 2]",
      new_ec = "7.1.2.2",
      new_rn = "R7.1.2.2"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Reduced coenzyme F420 + Methanophenazine <=> Coenzyme F420 + Dihydromethanophenazine",
      new_eq = "Reduced coenzyme F420 + Methanophenazine + 2 H+[side 1] <=> Coenzyme F420 + Dihydromethanophenazine + 2 H+[side 2]",
      new_ec = "1.5.98.3",
      new_rn = "R1.5.98.3"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Coenzyme M 7-mercaptoheptanoylthreonine-phosphate heterodisulfide + Dihydromethanophenazine <=> Coenzyme B + Coenzyme M + Methanophenazine",
      new_eq = "Coenzyme M 7-mercaptoheptanoylthreonine-phosphate heterodisulfide + Dihydromethanophenazine + 2 H+[side 1] <=> Coenzyme B + Coenzyme M + Methanophenazine + 2 H+[side 2]",
      new_ec = "1.8.98.1",
      new_rn = "R1.8.98.1"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Plastoquinol + 2 Oxidized plastocyanin + 2 H+ <=> Plastoquinone + 2 Reduced plastocyanin + 4 H+",
      new_eq = "Plastoquinol + 2 Oxidized plastocyanin + 2 H+[side 1] <=> Plastoquinone + 2 Reduced plastocyanin + 4 H+[side 2]",
      new_ec = "7.1.1.6",
      new_rn = "R7.1.1.6"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Ubiquinone + NADH + H+ + 4 H+ <=> Ubiquinol + NAD+ + 4 H+",
      new_eq = "Ubiquinone + NADH + H+ + 4 H+[side 2] <=> Ubiquinol + NAD+ + 4 H+[side 2]",
      new_ec = "7.1.1.2",
      new_rn = "R7.1.1.2"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Ubiquinol + 2 Ferricytochrome c <=> Ubiquinone + 2 Ferrocytochrome c + 2 H+",
      new_eq = "Ubiquinol + 2 Ferricytochrome c <=> Ubiquinone + 2 Ferrocytochrome c + 2 H+[side 2]",
      new_ec = "7.1.1.8",
      new_rn = "R7.1.1.8.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Hydroquinone + 2 Ferricytochrome c <=> Quinone + 2 Ferrocytochrome c + 2 H+",
      new_eq = "Hydroquinone + 2 Ferricytochrome c <=> Quinone + 2 Ferrocytochrome c + 2 H+[side 2]",
      new_ec = "7.1.1.8",
      new_rn = "R7.1.1.8.B"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Oxygen + 4 Ferrocytochrome c + 8 H+ <=> 4 Ferricytochrome c + 2 H2O + 4 H+",
      new_eq = "Oxygen + 4 Ferrocytochrome c + 8 H+[side 1] <=> 4 Ferricytochrome c + 2 H2O + 4 H+[side 2]",
      new_ec = "7.1.1.9",
      new_rn = "R7.1.1.9"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Ubiquinol + Oxygen + 8 H+ <=> 2 Ubiquinone + 2 H2O + 8 H+",
      new_eq = "2 Ubiquinol + Oxygen + 8 H+[side 1] <=> 2 Ubiquinone + 2 H2O + 8 H+[side 2]",
      new_ec = "7.1.1.3",
      new_rn = "R7.1.1.3"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Ubiquinol + Oxygen + 4 H+ <=> 2 Ubiquinone + 2 H2O + 4 H+",
      new_eq = "2 Ubiquinol + Oxygen + 4 H+[side 1] <=> 2 Ubiquinone + 2 H2O + 4 H+[side 2]",
      new_ec = "7.1.1.7",
      new_rn = "R7.1.1.7"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Trichloroethene + Cl- + Acceptor <=> Tetrachloroethene + Reduced acceptor",
      new_eq = "Tetrachloroethene + Hydroquinone <=> Trichloroethene + Cl- + H+ + Quinone",
      new_ec = "1.21.99.5.A",
      new_rn = "R1.21.99.5.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Trichloroethene + 2 H+ <=> trans-1,2-Dichloroethene + Hydrochloric acid",
      new_eq = "Trichloroethene + Hydroquinone <=> trans-1,2-Dichloroethene + Cl- + Quinone + H+",
      new_ec = "1.21.99.5.B",
      new_rn = "R1.21.99.5.B"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Trichloroethene + 2 H+ <=> cis-1,2-Dichloroethene + Hydrochloric acid",
      new_eq = "Trichloroethene + Hydroquinone <=> cis-1,2-Dichloroethene + Cl- + Quinone + H+",
      new_ec = "1.21.99.5.C",
      new_rn = "R1.21.99.5.C"
    )
  
  merged_network <- merged_network %>%
    fill_info(
      ko_match = "K18016",
      new_eq = "2 Reduced ferredoxin + 2 H+ + Na+[side 1] <=> Hydrogen + 2 Oxidized ferredoxin + Na+[side 2]",
      new_ec = "7.2.1.B",
      new_rn = "R7.2.1.B"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Butanoyl-CoA + FAD <=> FADH2 + Crotonoyl-CoA",
      new_eq = "Butanoyl-CoA + 2 NAD+ + 2 Reduced ferredoxin <=> Crotonoyl-CoA + 2 NADH + 2 Oxidized ferredoxin",
      new_ec = "1.3.1.109",
      new_rn = "R11675"
    )
  
  # Replace generic or wrong cofactors with others
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Hydrogen + Acceptor <=> Reduced acceptor",
      new_eq = c(
        "Ubiquinone + Hydrogen <=> Ubiquinol",
        "Quinone + Hydrogen <=> Hydroquinone"
      ),
      new_ec = c(
        "1.12.99.6.A",
        "1.12.99.6.B"
      ),
      new_rn = c(
        "R1.12.99.6.A",
        "R1.12.99.6.B"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Sulfite + Acceptor + AMP <=> Adenylyl sulfate + Reduced acceptor",
      new_eq = c(
        "Sulfite + NAD+ + AMP <=> Adenylyl sulfate + NADH + H+",
        "Sulfite + 2 Oxidized ferredoxin + AMP <=> Adenylyl sulfate + 2 Reduced ferredoxin + 2 H+"
      ),
      new_ec = c(
        "1.8.99.2.A",
        "1.8.99.2.B"
      ),
      new_rn = c(
        "R1.8.99.2.A",
        "R1.8.99.2.B"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Hydrogen sulfide + Protein dithiol + 2 p-Benzoquinone <=> Protein-trisulfide + 2 Hydroquinone",
      new_eq = c(
        "Hydrogen sulfide + Protein dithiol + 2 NAD+ <=> Protein-trisulfide + 2 NADH + 2 H+",
        "Hydrogen sulfide + Protein dithiol + 4 Oxidized ferredoxin <=> Protein-trisulfide + 4 Reduced ferredoxin + 4 H+"
      ),
      new_ec = c(
        "1.8.5.10.A",
        "1.8.5.10.B"
      ),
      new_rn = c(
        "R1.8.5.10.A",
        "R1.8.5.10.B"
      )
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Sulfur + Reduced acceptor <=> Hydrogen sulfide + Acceptor",
      new_eq = "Sulfur + Hydrogen <=> Hydrogen sulfide",
      new_ec = "1.8.98.A",
      new_rn = "R1.8.98.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Tetrathionate + 2 e- <=> 2 Thiosulfate",
      new_eq = "Tetrathionate + Hydroquinone <=> 2 Thiosulfate + Quinone",
      new_ec = "1.8.5.A",
      new_rn = "R1.8.5.A"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Nitrite + Acceptor + H2O <=> Nitrate + Reduced acceptor",
      new_eq = c(
        "2 Ferrocytochrome c + Nitrate <=> 2 Ferricytochrome c + Nitrite + H2O",
        "Nitrate + Hydroquinone <=> Nitrite + Quinone + H2O"
      ),
      new_ec = c(
        "1.9.6.1",
        "1.7.5.1"
      ),
      new_rn = c(
        "R03071",
        "R09497"
      )
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "(R)-Lactate + Acceptor <=> Pyruvate + Reduced acceptor",
      new_eq = "(R)-Lactate + NAD+ <=> Pyruvate + NADH + H+",
      new_ec = "1.1.1.28",
      new_rn = "R00704"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Formate + Oxidized hydrogenase <=> CO2 + Reduced hydrogenase",
      new_eq = "Formate + 2 H+ <=> CO2 + Hydrogen"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Hydrogen + Acceptor <=> Reduced acceptor",
      new_eq = c(
        "Ubiquinone + Hydrogen <=> Ubiquinol",
        "Quinone + Hydrogen <=> Hydroquinone"
      ),
      new_ec = c(
        "1.12.99.6.A",
        "1.12.99.6.B"
      ),
      new_rn = c(
        "R1.12.99.6.A",
        "R1.12.99.6.B"
      )
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Chloric acid + Reduced acceptor <=> Chlorous acid + Acceptor + H2O",
      new_eq = "Chloric acid + Ferrocytochrome c <=> Chlorous acid + Ferricytochrome c + H2O + H+",
      new_ec = "1.97.1.1",
      new_rn = "R1.97.1.1"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Glycolate + Acceptor <=> Glyoxylate + Reduced acceptor",
      new_eq = "Glycolate + Quinone <=> Glyoxylate + Hydroquinone",
      new_ec = "1.1.99.14.A",
      new_rn = "R1.1.99.14.A"
    )
  
  # Replace menaquinone, decylubiquinone, p-benzoquinone, or semiquinone (rare cofactors) with quinone (a common one)
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Menaquinone + Hydrogen <=> Menaquinol",
      new_eq = "Quinone + Hydrogen <=> Hydroquinone",
      new_ec = "1.12.99.6.B",
      new_rn = "R1.12.99.6.B"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Menaquinol + Oxygen + n H+ <=> 2 Menaquinone + 2 H2O + n H+",
      new_eq = "2 Hydroquinone + Oxygen + n H+[side 1] <=> 2 Quinone + 2 H2O + n H+[side 2]",
      new_ec = "7.1.1.A",
      new_rn = "R7.1.1.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Dimethyl sulfide + Menaquinone + H2O <=> Dimethyl sulfoxide + Menaquinol",
      new_eq = "Dimethyl sulfide + Quinone + H2O <=> Dimethyl sulfoxide + Hydroquinone",
      new_ec = "1.8.5.3.A",
      new_rn = "R1.8.5.3.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Thiosulfate + Menaquinol <=> Sulfite + Hydrogen sulfide + Menaquinone",
      new_eq = "Sulfite + Hydrogen sulfide + Quinone <=> Thiosulfate + Hydroquinone",
      new_ec = "1.8.5.5",
      new_rn = "R11488"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Thiosulfate + Decylubiquinone <=> Tetrathionate + Decylubiquinol",
      new_eq = "2 Thiosulfate + Quinone <=> Tetrathionate + Hydroquinone",
      new_ec = "1.8.5.2.A",
      new_rn = "R1.8.5.2.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "p-Benzoquinone + Selenite + H2O + 2 H+ <=> Hydroquinone + Selenate",
      new_eq = "Quinone + Selenite + H2O + 2 H+ <=> Hydroquinone + Selenate",
      new_ec = "1.97.1.14.A",
      new_rn = "R1.97.1.14.A"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Quinone + NADPH + H+ <=> 2 Semiquinone + NADP+",
      new_eq = "NADPH + H+ + Quinone <=> NADP+ + Hydroquinone",
      new_ec = "1.6.5.10",
      new_rn = "R07359"
    )
  
  # Replace names of metabolites inconsistent with those in other reactions
  replacements <- c(
    "Reduced \\[2Fe-2S\\] ferredoxin" = "Reduced ferredoxin",
    "Oxidized \\[2Fe-2S\\] ferredoxin" = "Oxidized ferredoxin",
    "Sodium cation\\(side 1\\)" = "Na+[side 1]",
    "Sodium cation\\(side 2\\)" = "Na+[side 2]",
    "Ferricytochrome cL" = "Ferricytochrome c",
    "Ferrocytochrome cL" = "Ferrocytochrome c",
    "Ferricytochrome c3" = "Ferricytochrome c",
    "Ferrocytochrome c3" = "Ferrocytochrome c",
    "Polysulfide\\(n\\)" = "2 Polysulfide sulfur",
    "Polysulfide\\(n-1\\)" = "Polysulfide sulfur"
  )
  
  merged_network <- merged_network %>%
    dplyr::mutate(eq = stringr::str_replace_all(eq, replacements))
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Oxaloacetate + 2 Sodium cation <=> Pyruvate + CO2 + 2 Sodium cation",
      new_eq = "Oxaloacetate + 2 Na+[side 1] <=> Pyruvate + CO2 + 2 Na+[side 2]"
    )
  
  # Combine KO IDs for subunits not detected automatically
  merged_network <- merged_network %>% combine_ko(
    list(c("K02111, K02112", "K02117")),
    exact_match = TRUE
  )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K08483", "K02784")
    )
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K02793", "K02795", "K02796"),
      c("K02794", "K02795", "K02796")
    )
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K02768", "K02769", "K02770"),
      c("K11194", "K11195", "K11196", "K02771")
    )
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K02777", "K02779"),
      c("K02777", "K02791")
    )
    )
  
  merged_network <- merged_network %>%
    combine_ko(
      ko_groups = list(
        c("K00202", "K00203", "K00204"),
        c("K00202", "K00203", "K00205"),
        c("K00202", "K00203", "K11260")
      ),
      rn_match = "R11743"
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K08352", "K16293", "K16294")
    )
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K03416", "K17489")
    )
    )
  
  merged_network <- merged_network %>% 
    combine_ko(list(
      c("K22338", "K22341")
    )
    )
  
  merged_network <- merged_network %>%
    combine_ko(list(
      c("K17222", "K17223")
    )
    )
  
  
  merged_network <- merged_network %>%
    combine_ko(list(
      c("K14068", "K14070")
    )
    )
  
  merged_network <- merged_network %>% combine_ko(
    list(c("K08940", "K08941", "K08942", "K08943"))
  )
  
  merged_network <- merged_network %>% combine_ko(
    list(c("K16933", "K16934"))
  )
  
  # Split and recombine KO IDs for subunits not combined properly
  merged_network <- merged_network %>%
    split_ko(
      ko_match = c("K01652"),
      rn_match = "R00014"
    )
  
  merged_network <- merged_network %>%
    combine_ko(
      ko_groups = list(
        c("K01652", "K01653"),
        c("K01652", "K11258")
      ),
      rn_match = "R00014"
    )
  
  merged_network <- merged_network %>%
    split_ko(
      ko_match = c("K00200"),
      rn_match = c("R08060")
    )
  
  merged_network <- merged_network %>%
    combine_ko(
      ko_groups = list(c("K00200", "K00201", "K00202")),
      rn_match = "R08060"
    )
  
  # Add missing reactions for promiscuous enzymes
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Protein N(pi)-phospho-L-histidine + D-Glucose <=> Protein histidine + alpha-D-Glucose 6-phosphate",
      new_eq = c(
        "Protein N(pi)-phospho-L-histidine + D-Glucose <=> Protein histidine + alpha-D-Glucose 6-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Mannose <=> Protein histidine + D-Mannose 6-phosphate"
      ),
      new_ec = c(
        "2.7.1.199",
        "2.7.1.191"
      ), 
      new_rn = c(
        "R02738",
        "R02630"
      ),
      ko_match ="K02779"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Protein N(pi)-phospho-L-histidine + D-Mannose <=> Protein histidine + D-Mannose 6-phosphate",
      new_eq = c(
        "Protein N(pi)-phospho-L-histidine + D-Mannose <=> Protein histidine + D-Mannose 6-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Fructose <=> Protein histidine + D-Fructose 1-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Glucose <=> Protein histidine + alpha-D-Glucose 6-phosphate"
      ),
      new_ec = c(
        "2.7.1.191",
        "2.7.1.202",
        "2.7.1.199"
      ), 
      new_rn = c(
        "R02630",
        "R03232",
        "R02738"
      ),
      ko_match ="K02793"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Protein N(pi)-phospho-L-histidine + D-Fructose <=> Protein histidine + D-Fructose 1-phosphate",
      new_eq = c(
        "Protein N(pi)-phospho-L-histidine + D-Fructose <=> Protein histidine + D-Fructose 1-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Glucose <=> Protein histidine + alpha-D-Glucose 6-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Mannose <=> Protein histidine + D-Mannose 6-phosphate"
      ),
      new_ec = c(
        "2.7.1.202",
        "2.7.1.199",
        "2.7.1.191"
      ), 
      new_rn = c(
        "R03232",
        "R02738",
        "R02630"
      ),
      ko_match = "K02770"
    )
  
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Protein N(pi)-phospho-L-histidine + D-Fructose <=> Protein histidine + D-Fructose 1-phosphate",
      new_eq = c(
        "Protein N(pi)-phospho-L-histidine + D-Fructose <=> Protein histidine + D-Fructose 1-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Glucose <=> Protein histidine + alpha-D-Glucose 6-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Mannose <=> Protein histidine + D-Mannose 6-phosphate"
      ),
      new_ec = c(
        "2.7.1.202",
        "2.7.1.199",
        "2.7.1.191"
      ), 
      new_rn = c(
        "R03232",
        "R02738",
        "R02630"
      ),
      ko_match = "K02770"
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Protein N(pi)-phospho-L-histidine + Lactose <=> Protein histidine + Lactose 6'-phosphate",
      new_eq = c(
        "Protein N(pi)-phospho-L-histidine + Lactose <=> Protein histidine + Lactose 6'-phosphate",
        "Protein N(pi)-phospho-L-histidine + D-Galactose <=> Protein histidine + D-Galactose 6-phosphate"
      ),
      new_ec = c(
        "2.7.1.207",
        "2.7.1.204"
      ), 
      new_rn = c(
        "R04393",
        "R11171"
      ),
      ko_match = "K02786"
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "Succinyl-CoA + Acetate <=> Acetyl-CoA + Succinate",
      new_eq = c(
        "Succinyl-CoA + Acetate <=> Acetyl-CoA + Succinate",
        "Propanoyl-CoA + Succinate <=> Succinyl-CoA + Propanoate"
      ),
      new_ec = c(
        "2.8.3.18",
        "2.8.3.27"
      ),
      new_rn = c(
        "R10343",
        "R11773"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "D-Galactose + NAD+ <=> D-Galactono-1,5-lactone + NADH + H+",
      new_eq = c(
        "D-Galactose + NAD+ <=> D-Galactono-1,5-lactone + NADH + H+",
        "D-Glucose + NAD+ <=> D-Glucono-1,5-lactone + NADH + H+"
      ),
      new_ec = c(
        "1.1.1.359",
        "1.1.1.359"
      ),
      new_rn = c(
        "R1.1.1.1.359.A",
        "R1.1.1.1.359.B"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Formate + NAD+ <=> H+ + CO2 + NADH",
      new_eq = c(
        "Formate + NAD+ <=> H+ + CO2 + NADH",
        "Formate + Quinone <=> CO2 + Hydroquinone"
      ),
      new_rn = c(
        "R00519",
        "R09494"
      ),
      new_ec = c(
        "1.17.1.9",
        "1.17.5.3"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "[SoxY protein]-S-sulfanyl-L-cysteine + Thiosulfate + 2 Ferricytochrome c <=> [SoxY protein]-S-(2-sulfodisulfanyl)-L-cysteine + 2 Ferrocytochrome c + 2 H+",
      new_eq = c(
        "[SoxY protein]-S-sulfanyl-L-cysteine + Thiosulfate + 2 Ferricytochrome c <=> [SoxY protein]-S-(2-sulfodisulfanyl)-L-cysteine + 2 Ferrocytochrome c + 2 H+",
        "[SoxY protein]-S-sulfanyl-L-cysteine + Hydrogen sulfide + 2 Ferricytochrome c <=> [SoxY protein]-S-disulfanyl-L-cysteine + 2 Ferrocytochrome c + 2 H+",
        "[SoxY protein]-S-sulfanyl-L-cysteine + Sulfur <=> [SoxY protein]-S-disulfanyl-L-cysteine"
      ),
      new_rn = c(
        "R12164",
        "R1.8.2.A",
        "R2.8.5.A"
      ),
      new_ec = c(
        "2.8.5.2",
        "1.8.2.A",
        "2.8.5.A"
        
      )
    )
  
  merged_network <- merged_network %>%
    replace_equation(
      old_eq = "2 Reduced ferredoxin + NAD+ + H+ + Na+[side 1] <=> 2 Oxidized ferredoxin + NADH + Na+[side 2]",
      new_eq = c(
        "2 Reduced ferredoxin + NAD+ + H+ + Na+[side 1] <=> 2 Oxidized ferredoxin + NADH + Na+[side 2]",
        "2 Reduced ferredoxin + Methanophenazine + Na+[side 1] <=> 2 Oxidized ferredoxin + Dihydromethanophenazine + Na+[side 2]"
      ),
      new_ec = c(
        "7.2.1.2",
        "7.2.1.C"
      ),
      new_rn = c(
        "R7.2.1.2",
        "R7.2.1.C"
      )
    )
  
  merged_network <- merged_network %>% 
    replace_equation(
      old_eq = "Secondary alcohol + Coenzyme F420 <=> Ketone + Reduced coenzyme F420",
      new_eq = c(
        "Propan-2-ol + Coenzyme F420 <=> Acetone + Reduced coenzyme F420",
        "Butan-2-ol + Coenzyme F420 <=> Methyl ethyl ketone + Reduced coenzyme F420",
        "Cyclopentanol + Coenzyme F420 <=> Cyclopentanone + Reduced coenzyme F420"
      ),
      new_ec = c(
        "1.1.98.5",
        "1.1.98.5",
        "1.1.98.5"
      ),
      new_rn = c(
        "R1.1.98.5.A",
        "R1.1.98.5.B",
        "R1.1.98.5.C"
      )
    )  
  
  # Remove KO IDs that should not be present in enzymes
  # Not present in Bacillus subtilis
  merged_network <- merged_network %>%
    remove_ko(
      ko = "K00425, K00426, K00424",
      ko_remove = "K00424"
    )
  
  # Not present in Synechococcus elongatus
  merged_network <- merged_network %>%
    remove_ko(
      ko = "K02635, K02637, K02634, K02636, K02642, K02643, K03689, K02640",
      ko_remove = "K02642"
    )
  
  # Add missing KO IDs to reactions
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12164",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12164",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12097",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12097",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R11971",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R11971",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12096",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12096",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R1.8.2.A",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R1.8.2.A",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R2.8.5.A",
      ko_to_add = "K17226"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R2.8.5.A",
      ko_to_add = "K17227"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R1.7.2.A",
      ko_to_add = "K07811"
    )
  
  merged_network <- merged_network %>% 
    add_ko(
      rn_match = "R12164",
      ko_to_add = "K17226"
    )
  
  # Denote H2O formed during reduction of oxygen as H2O* 
  merged_network <- merged_network %>%
    replace_water(
      rns = c("R7.1.1.9", "R7.1.1.3", "R7.1.1.7", "R7.1.1.A", "R00148")
    )
  
  # Add reactions
  merged_network <- merged_network %>%
    add_rn(
      ko = "NA",
      rn = "R1.16.9.A",
      eq = "Rusticyanin + Ferrocytochrome c <=> Reduced rusticyanin + Ferricytochrome c",
      ec = "1.16.9.A",
      symbol = "NA",
      name = "NA",
      md = "NA"
    )
  
  merged_network <- merged_network %>%
    add_rn(
      ko = "NA",
      rn = "R00112",
      eq = "NADPH + NAD+ <=> NADP+ + NADH",
      ec = "NA",
      symbol = "NA",
      name = "NA",
      md = "NA"
    )
  
  merged_network <- merged_network %>%
    add_rn(
      ko = "NA",
      rn = "R.SA",
      eq = "Sulfur + Hydrogen sulfide <=> 2 Polysulfide sulfur + 2 H+",
      ec = "NA",
      symbol = "NA",
      name = "NA",
      md = "NA"
    )  
  
  merged_network <- merged_network %>%
    add_rn(
      ko = "NA",
      rn = "R.SB",
      eq = "2 Polysulfide sulfur + Glutathione <=> S-Sulfanylglutathione + Polysulfide sulfur",
      ec = "NA",
      symbol = "NA",
      name = "NA",
      md = "NA"
    )  
  
  # Remove extra entries (for subunits not matched to proteins or extra reactions)
  merged_network <- merged_network %>% remove_rn(ko = "K08484")
  merged_network <- merged_network %>% remove_rn(ko = "K02778")
  merged_network <- merged_network %>% remove_rn(ko = "K23991")
  merged_network <- merged_network %>% remove_rn(ko = "K23993")
  merged_network <- merged_network %>% remove_rn(ko = "K11261")
  merged_network <- merged_network %>% remove_rn(ko = "K02164")
  merged_network <- merged_network %>% remove_rn(ko = "K02448")
  merged_network <- merged_network %>% remove_rn(ko = "K04747")
  merged_network <- merged_network %>% remove_rn(ko = "K04748")
  merged_network <- merged_network %>% remove_rn(ko = "K04016")
  merged_network <- merged_network %>% remove_rn(ko = "K00126")
  merged_network <- merged_network %>% remove_rn(ko = "K22515")
  merged_network <- merged_network %>% remove_rn(ko = "K00203")
  merged_network <- merged_network %>% remove_rn(ko = "K00204")
  merged_network <- merged_network %>% remove_rn(ko = "K00205")
  merged_network <- merged_network %>% remove_rn(ko = "K11260")
  merged_network <- merged_network %>% remove_rn(ko = "K00196")
  merged_network <- merged_network %>% remove_rn(ko = "K00192, K00195")
  merged_network <- merged_network %>% remove_rn(ko = "K21648")
  merged_network <- merged_network %>% remove_rn(rn = "R08034")

# === Final processing ===    
  # Merge nearly identical rows 
  merged_network$map <- remove_or_replace_target(merged_network$map)
  merged_network$md  <- remove_or_replace_target(merged_network$md)
  
  merged_network <- merged_network %>%
    dplyr::group_by(name, eq, way, ec, ko, rn, symbol) %>%
    dplyr::summarise(
      map = clean_and_collapse(map),
      md  = clean_and_collapse(md),
      .groups = "drop"
    )
  
  # Remove extra reactions (duplicated across maps or modules)
  merged_network <- merged_network %>%
    # Pyruvate metabolism
    dplyr::filter(!(rn == "R01196" & !grepl("map00620", map))) %>%
    
    # TCA cycle
    dplyr::filter(!(rn == "R02164" & !grepl("map00020", map))) %>%
    dplyr::filter(!(rn == "R13223" & !grepl("map00020", map))) %>%
    dplyr::filter(!(rn == "R00352" & !grepl("map00720", map))) %>%
    dplyr::filter(!(rn == "R01197" & !grepl("M00009", md))) %>%
    
    # Oxidative phosphorylation
    dplyr::filter(!(rn == "R02161" & !grepl("M00151", md))) %>%
    
    # Nitrogen metabolism
    dplyr::filter(!(rn == "R09497" & !grepl("M00530", md))) %>%
    dplyr::filter(!(rn == "R00787" & !grepl("M00530", md))) %>%
    dplyr::filter(!(rn == "R03071" & !grepl("M00529", md))) %>%
    
    # Propionate
    dplyr::filter(!(rn == "R11773" & !grepl("map00620", map))) %>%
    
    # Acetogenesis
    dplyr::filter(!(rn == "R00134" & !grepl("map00720", map))) %>%
    
    # Methane metabolism
    dplyr::filter(!(rn == "R12754" & !grepl("map00720", map)))

# === Save network ===
  fp <- paste0(network_directory, "/data/mergedNetwork/merged_network.csv")
  write.csv(merged_network, fp, row.names = FALSE)
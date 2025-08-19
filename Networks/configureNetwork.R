# Configure Network
# This script takes a previously-merged metabolic network and configures it 
# for the app.  Specifically, it assigns pathways to each of the reactions, 
# which the app will then use to generate individual networks. The result is 
# known as a master network.  
# Requirements
# - merged_network.csv: A file that contains the master metabolic network with all reactions.
# - reaction_config.csv: A file that specifies which reactions belong to modules and their direction.
# Author: Timothy Hackmann
# Date:  25 April 2025

# === Set directory ===
  network_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(network_directory)
  source("functions/helperFunctions.R", local = TRUE)

# === Load network files ===
  fp <- paste0(network_directory, "/data/mergedNetwork/merged_network.csv")
  merged_network <- readr::read_csv(fp)
  
# === Load configuration files ===
  fp <- paste0(network_directory, "/data/config/reaction_config.csv")
  reaction_config <- readr::read_csv(fp)

# === Define networks and specific reactions to keep/remove ===
  network_definitions <- list(
      "Fermentation of glucose" = c(
        "Glycolysis", 
        "Pentose phosphate", 
        "Methylglyoxal shunt",
        "Malate shunt",
        "Pyruvate decarboxylation",
        "Lactate formation", 
        "Acetate formation", 
        "Ethanol formation",
        "Propionate formation", 
        "Butyrate formation",
        "Hydrogen formation",
        "NADH reduction by ferredoxin",
        "Quinone reduction by NADH",
        "Quinone reduction by other donors",
        "Fumarate reduction by electron carriers",
        "ATP formation",
        "Redox balance"
      ),
    "Fermentation of other hexoses" = c(
      "Fructose utilization",
      "Galactose utilization",
      "Mannose utilization",
      "Glycolysis", 
      "Pentose phosphate", 
      "Methylglyoxal shunt",
      "Malate shunt",
      "Pyruvate decarboxylation",
      "Lactate formation", 
      "Acetate formation", 
      "Ethanol formation",
      "Propionate formation", 
      "Butyrate formation",
      "Hydrogen formation",
      "NADH reduction by ferredoxin",
      "Quinone reduction by NADH",
      "Quinone reduction by other donors",
      "Fumarate reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Fermentation of pentoses" = c(
      "Arabinose utilization",
      "Ribose utilization",
      "Xylose utilization",
      "Pentose phosphate",
      "Glycolysis", 
      "Pyruvate decarboxylation",
      "Lactate formation", 
      "Acetate formation", 
      "Ethanol formation",
      "Propionate formation", 
      "Butyrate formation",
      "Hydrogen formation",
      "NADH reduction by ferredoxin",
      "Quinone reduction by NADH",
      "Quinone reduction by other donors",
      "Fumarate reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration (generic)" = c(
      "Quinone reduction by NADH",
      "Quinone reduction by succinate",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration of glucose" = c(
      "Glycolysis", 
      "Pentose phosphate",
      "Methylglyoxal shunt",
      "Malate shunt",
      "Pyruvate decarboxylation",
      "TCA cycle",
      "Quinone reduction by NADH",
      "Quinone reduction by succinate",
      # "Quinone reduction by other donors",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration of other hexoses" = c(
      "Fructose utilization",
      "Galactose utilization",
      "Mannose utilization",
      "Glycolysis", 
      "Pentose phosphate",
      "Methylglyoxal shunt",
      "Malate shunt",
      "Pyruvate decarboxylation",
      "TCA cycle",
      "Quinone reduction by NADH",
      "Quinone reduction by succinate",
      # "Quinone reduction by other donors",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
   "Aerobic respiration of pentoses" = c(
      "Arabinose utilization",
      "Ribose utilization",
      "Xylose utilization",
      "Pentose phosphate",
      "Glycolysis", 
      "Pyruvate decarboxylation",
      "TCA cycle",
      "Quinone reduction by NADH",
      "Quinone reduction by succinate",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration of ammonia (nitrification)" = c(
      "Ammonia oxidation",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration of nitrite (nitrification)" = c(
      "Nitrite oxidation",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Aerobic respiration of sulfur compounds" = c(
      "Sulfur oxidation",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
   "Aerobic respiration of methane and methanol" = c(
      "Methane oxidation",
      "Methanol oxidation",
      "Formate oxidation",
      "NADH reduction by ferredoxin",
      "Quinone reduction by NADH",
      "Quinone reduction by other donors",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"),
   "Aerobic respiration of carbon monoxide" = c(
     "Carbon monoxide oxidation",
     "Quinone reduction by NADH",
     "Cytochrome c reduction by quinone",
     "Oxygen reduction by electron carriers",
     "ATP formation",
     "Redox balance"),
   "Aerobic respiration of iron compounds" = c(
     "Iron oxidation",
     "Oxygen reduction by electron carriers",
     "ATP formation",
     "Redox balance"
   ),
   "Aerobic respiration of arsenic compounds" = c(
     "Arsenic oxidation",
     "Oxygen reduction by electron carriers",
     "ATP formation",
     "Redox balance"
   ),
   "Aerobic respiration of hydrogen" = c(
      "Hydrogen oxidation",
      "Cytochrome c reduction by quinone",
      "Oxygen reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
    "Nitrate respiration (generic) (dissimilatory nitrate reduction)" = c(
      "Nitrate reduction by electron carriers",
      "Redox balance"
    ),
   "Nitrate respiration of glucose (dissimilatory nitrate reduction)" = c(
      "Glycolysis",
      "Pentose phosphate",
      "Methylglyoxal shunt",
      "Malate shunt",
      "Pyruvate decarboxylation",
      "TCA cycle",
      "NADH reduction by ferredoxin",
      "Quinone reduction by NADH",
      "Quinone reduction by succinate",
      "Quinone reduction by other donors",
      "Cytochrome c reduction by quinone",
      "Nitrate reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
   "Sulfur compound respiration (generic)" = c(
     "Sulfate and sulfite reduction by electron carriers",
     "Sulfur reduction by electron carriers",
     "Tetrathionate reduction by electron carriers",
     "Thiosulfate reduction by electron carriers",
     "Redox balance"
   ),
    "Sulfate respiration (generic) (dissimilatory sulfate reduction)" = c(
      "Sulfate and sulfite reduction by electron carriers",
      "Redox balance"
    ),
    "Sulfate respiration of pyruvate (dissimilatory sulfate reduction)" = c(
      "Lactate oxidation",
      "Pyruvate synthesis",
      "Ethanol oxidation",
      "Pyruvate decarboxylation",
      "TCA cycle",
      "Wood Ljungdahl",
      "NADH reduction by ferredoxin",
      "Quinone reduction by NADH",
      "Quinone reduction by other donors",
      "Quinone reduction by succinate",
      "Cytochrome c reduction by quinone",
      "Sulfate and sulfite reduction by electron carriers",
      "ATP formation",
      "Redox balance"
    ),
   "Sulfate respiration of phosphite" = c(
     "Phosphite oxidation",
     "Quinone reduction by NADH",
     "Cytochrome c reduction by quinone",
     "Sulfate and sulfite reduction by electron carriers",
     "ATP formation",
     "Redox balance"
   ),
   "Sulfite respiration (generic)" = c(
     "Sulfate and sulfite reduction by electron carriers",
     "Redox balance"
   ),
   "Sulfur respiration (generic)" = c(
     "Sulfur reduction by electron carriers",
     "Redox balance"
   ),
   "Sulfur respiration of glucose" = c(
     "Glycolysis", 
     "Pentose phosphate", 
     "Methylglyoxal shunt",
     "Malate shunt",
     "Pyruvate decarboxylation",
     "Lactate formation", 
     "Acetate formation", 
     "Ethanol formation",
     "Propionate formation", 
     "Butyrate formation",
     "Hydrogen formation",
     "NADH reduction by ferredoxin",
     "Quinone reduction by NADH",
     "Quinone reduction by other donors",
     "Fumarate reduction by electron carriers",
     "Sulfur reduction by electron carriers",
     "ATP formation",
     "Redox balance"
   ),
   "Tetrathionate respiration (generic)" = c(
     "Tetrathionate reduction by electron carriers",
     "Redox balance"
   ),
   "Thiosulfate respiration (generic)" = c(
     "Thiosulfate reduction by electron carriers",
     "Redox balance"
   ),
   "Iron respiration (generic)" = c(
     "Iron reduction by electron carriers",
     "Redox balance"
   ),
   "Chlorate respiration (generic)" = c(
     "Chlorate reduction by electron carriers",
     "Redox balance"
   ),
   "Selenate respiration (generic)" = c(
     "Selenate reduction by electron carriers",
     "Redox balance"
   ),
   "Fumarate respiration (generic)" = c(
     "Fumarate reduction by electron carriers",
     "Redox balance"
   ),
   "Fumarate respiration of hydrogen" = c(
     "Hydrogen oxidation",
     "Fumarate reduction by electron carriers",
     "Redox balance"
   ),
   "Trimethylamine N-oxide (TMAO) respiration (generic)" = c(
     "Trimethylamine N-oxide reduction by electron carriers",
     "Redox balance"
   ),
   "Trimethylamine N-oxide (TMAO) respiration of hydrogen" = c(
     "Hydrogen oxidation",
     "Trimethylamine N-oxide reduction by electron carriers",
     "Redox balance"
   ),
   "Dimethyl sulfoxide (DMSO) respiration (generic)" = c(
     "Dimethyl sulfoxide reduction by electron carriers",
     "Redox balance"
   ),
   "Dimethyl sulfoxide (DMSO) respiration of hydrogen" = c(
     "Hydrogen oxidation",
     "Dimethyl sulfoxide reduction by electron carriers",
     "Redox balance"
   ),
   "Organohalide respiration (generic)" = c(
     "Tetrachloroethene reduction by electron carriers",
     "3-Chloro-4-hydroxyphenylacetate reduction by electron carriers",
     "Redox balance"
   ),
   "Sulfur disproportionation" = c(
     "Sulfur disproportionation",
     "Redox balance"
   ),
    "Methanogenesis" = c(
      "Methanogenesis with CO2",
      "Methanogenesis with formate", 
      "Methanogenesis with methanol",
      "Methanogenesis with acetate", 
      "Methanogenesis with methyamines",
      "Methanogenesis with alcohols",
      "Redox balance"
    ),
    "Acetogenesis" = c(
      "Wood Ljungdahl",
      "Acetate formation",
      "NADH reduction by ferredoxin",
      "Hydrogen oxidation",
      "Redox balance"
    ),
    "Nitrogen fixation" = c(
      "Nitrogen fixation"
    ),
    "Nitrite respiration of ammonia (anammox)" = c(
      "Anammox"
    ),
   "Phototrophy" = c(
     "Light reactions of cyanobacteria",
     "Light reactions of purple bacteria",
     "Light reactions of green bacteria",
     "Light reactions of archaea",
     "ATP formation"
   ),
   "Oxygenic photosynthesis" = c(
     "Light reactions of cyanobacteria",
     "ATP formation",
     "Calvin cycle"
   )
  )
  
# === Combine all configured networks ===
  all_networks <- lapply(names(network_definitions), function(name) {
    mds <- network_definitions[[name]]
    configure_network(
      merged_network = merged_network,
      mds = mds,
      reaction_config = reaction_config
    )
  })
  names(all_networks) <- names(network_definitions)

  main_network <- dplyr::bind_rows(all_networks, .id = "nt")

# === Save networks ===
  fp <- paste0(network_directory, "/data/mainNetwork/main.csv")
  write.csv(main_network, fp, row.names = FALSE)
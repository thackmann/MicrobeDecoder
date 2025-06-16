# Create App Networks
# This script takes a master metabolic network and configures it to create smaller 
# networks the app.  For example, it can create a network for glucose fermentation.
# Requirements
# - master_network.csv: A file that contains the master metabolic network with all reactions.
# - reaction_config.csv: A file that specifies which reactions belong to mds and their direction.
# Author: Timothy Hackmann
# Date:  25 April 2025

# === Set directory ===
  network_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(network_directory)
  source("functions/helperFunctions.R", local = TRUE)

# === Load network files ===
  fp <- paste0(network_directory, "/data/masterNetwork/master_network.csv")
  master_network <- readr::read_csv(fp)
  
# === Load configuration files ===
  fp <- paste0(network_directory, "/data/config/reaction_config.csv")
  reaction_config <- readr::read_csv(fp)

# === Define maps and specific reactions to keep/remove ===
  # Fermentation of glucose
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  fermentation_of_glucose <- configured_network
  
  head(master_network)
  head(fermentation_of_glucose)
  
  # Fermentation of hexoses
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  fermentation_of_hexoses <- configured_network
  
  # Fermentation of pentoses
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  fermentation_of_pentoses <- configured_network
  
  # Aerobic respiration of glucose
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_glucose <- configured_network
  
  # Aerobic respiration of hexoses
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_hexoses <- configured_network
  
  # Aerobic respiration of pentoses
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_pentoses <- configured_network
  
  # Aerobic respiration of ammonia (nitrification)
  mds <- c(
    "Ammonia oxidation",
    "Cytochrome c reduction by quinone",
    "Oxygen reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_ammonia <- configured_network
  
  # Aerobic respiration of nitrite (nitrification)
  mds <- c(
    "Nitrite oxidation",
    "Cytochrome c reduction by quinone",
    "Oxygen reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_nitrite <- configured_network

  # Aerobic respiration of sulfur compounds
  mds <- c(
    "Sulfur oxidation",
    "Cytochrome c reduction by quinone",
    "Oxygen reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_sulfur <- configured_network
  
  # Aerobic respiration of methane
  mds <- c(
    "Methane oxidation",
    "Formaldehyde oxidation",
    "Formate oxidation",
    "NADH reduction by ferredoxin",
    "Quinone reduction by NADH",
    "Quinone reduction by other donors",
    "Cytochrome c reduction by quinone",
    "Oxygen reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_methane <- configured_network
  
  # Aerobic respiration of hydrogen
  mds <- c(
    "Hydrogen oxidation",
    "Cytochrome c reduction by quinone",
    "Oxygen reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  aerobic_respiration_of_hydrogen <- configured_network
  
  # Nitrate reduction by electron carriers (dissimilatory nitrate reduction)
  mds <- c(
    "Nitrate reduction by electron carriers",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  nitrate_reduction_by_electron_carriers <- configured_network
  
  # Nitrate reduction by glucose (dissimilatory nitrate reduction)
  mds <- c(
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
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  nitrate_reduction_by_glucose <- configured_network
  
  # Sulfate reduction by electron carriers (dissimilatory sulfate reduction)
  mds <- c(
    "Sulfate reduction by electron carriers",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  sulfate_reduction_by_electron_carriers <- configured_network
  
  # Sulfate reduction by pyruvate (dissimilatory sulfate reduction)
  mds <- c(
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
    "Sulfate reduction by electron carriers",
    "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  sulfate_reduction_by_pyruvate <- configured_network
  
  
  # Methanogenesis
  mds <- c(
    "Methanogenesis  with CO2",
    "Methanogenesis  with formate", 
    "Methanogenesis with methanol",
    "Methanogenesis with acetate", 
    "Methanogenesis with methyamines",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  methanogenesis <- configured_network
  
  # Acetogenesis
  mds <- c(
    "Wood Ljungdahl",
    "Acetate formation",
    "NADH reduction by ferredoxin",
    "Hydrogen oxidation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  acetogenesis <- configured_network
  
  # Nitrogen fixation
  mds <- c(
    "Nitrogen fixation"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  nitrogen_fixation <- configured_network
  
  
  # Anammox
  mds <- c(
    "Anammox"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  anammox <- configured_network
  
  # Sulfur reduction by electron carriers
  mds <- c(
    # "Lactate oxidation", 
    # "Pyruvate synthesis",
    # "Ethanol oxidation",
    # "Pyruvate decarboxylation",
    # "TCA cycle",
    # "NADH reduction by ferredoxin",
    # "Quinone reduction by NADH",
    # "Quinone reduction by other donors",
    # "Cytochrome c reduction by quinone",
    "Sulfur reduction by electron carriers",
    # "ATP formation",
    "Redox balance"
  )
  
  configured_network <- configure_network(
    master_network = master_network, 
    mds = mds, 
    reaction_config = reaction_config
  )
  
  sulfur_reduction_by_electron_carriers <- configured_network
  
# === Save networks ===
  fp <- paste0(network_directory, "/data/appNetworks")
  object_to_csv(fermentation_of_glucose, fp)
  object_to_csv(fermentation_of_hexoses, fp)
  object_to_csv(fermentation_of_pentoses, fp)
  object_to_csv(aerobic_respiration_of_glucose, fp)
  object_to_csv(aerobic_respiration_of_hexoses, fp)
  object_to_csv(aerobic_respiration_of_pentoses, fp)
  object_to_csv(aerobic_respiration_of_ammonia, fp)
  object_to_csv(aerobic_respiration_of_nitrite, fp)
  object_to_csv(aerobic_respiration_of_sulfur, fp)
  object_to_csv(aerobic_respiration_of_methane, fp)
  object_to_csv(aerobic_respiration_of_hydrogen, fp)
  object_to_csv(nitrate_reduction_by_electron_carriers, fp)
  object_to_csv(nitrate_reduction_by_glucose, fp)
  object_to_csv(sulfate_reduction_by_electron_carriers, fp)
  object_to_csv(sulfate_reduction_by_pyruvate, fp)
  object_to_csv(methanogenesis, fp)
  object_to_csv(acetogenesis, fp)
  object_to_csv(nitrogen_fixation, fp)
  object_to_csv(anammox, fp)
  # object_to_csv(sulfur_respiration, fp)
# Translate Other KEGG Reactions to Networks
# This script translates reactions from KEGG into reference network models for 
# the app.  This script is similar to keggPathwaystoNetworks, except it includes
# reactions not in KEGG pathways.  These reactions were hand-picked.  They are 
# frequently missing metadata, which must be provided by hand.  
# Author: Timothy Hackmann
# Date:  25 April 2025

# === Get database directory ===
  network_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(network_directory)
  source("functions/helperFunctions.R", local = TRUE)

# === Get metadata for KEGG pathways ===
  map <- c("other_reactions")
  
  # Define reactions, EC, and other variables for retrieving metadata
  calls <- list(
      # Glycolysis
      list(ec = "2.7.4.1"),
      list(ec = "1.2.7.6"),
      list(ec = "2.7.1.63"),
      list(rn = "R05804"),
      
      # Pentose phosphate pathway
      list(rn = "R00835"),
      list(rn = "R01827"),
      list(rn = "R00761"),
      
      # Pyruvate metabolism
      list(rn = "R01735"),
      list(rn = "R01699"),
      
      # PTS
      list(ec = "2.7.3.9"),
      list(ko = "K02784"),
      list(rn = "R02738"),
      list(rn = "R02630"),
      list(rn = "R04393"),
      list(rn = "R11171"),
      
      # Propanoate metabolism
      list(ec = "7.2.4.3"),
      list(ko = "K11942"),
      list(ec = "7.2.4.3"),
      
      # Formate formation
      list(ec = "1.17.5.3"),

      # Ethanol formation
      list(rn = "R00755"),
      
      # Sulfate respiration
      list(rn = "R00860"),
      
      # Sulfur respiration
      list(ko = "K16293"),
      list(ko = "K16294"),
      
      # Sulfur disproportionation
      list(rn = "R00864"),
      
      # Aerobic respiration
      list(ko="K16933"),
      list(ko="K16934"),
      
      # Iron respiration
      list(ec ="1.16.2.1"),
      
      # Chlorate respiration
      list(ec ="1.97.1.1"),
      list(ec ="1.13.11.49"),
      
      # Selenite respiration
      list(rn = "R07229"),
      list(rn = "R13388"),
         
      # TMAO respiration
      list(ko ="K03532"),
         
      # Organohalide respiration
      list(ec = "1.21.99.5"),
      list(ko ="k21566"),
      
      # Sulfur oxidation
      list(rn = "R12164"),
      list(rn = "R12097"),
      list(rn = "R11971"),
      list(rn = "R12096"),
      list(ko = "K17226"),
      list(ko = "K17227"),
      list(rn = "R00029"),
      
      # Phosphite oxidation
      list(ec = "1.20.1.1"),
      
      # Hydrogen formation
      list(ec = "1.12.5.1"),
      list(ec = "1.12.99.6"),
      list(ec = "1.12.7.2"),
      list(ec = "1.12.1.4"),
      list(ec = "1.12.1.2"),
      list(ec = "1.12.1.3"),
      list(ec = "1.17.98.4"),
      list(ko = c("K14086", "K14087", "K14088", "K14089", "K14090", "K14091")),
      
      # Hydrogen oxidation
      list(ec = "1.12.2.1"),
      
      # Methane formation
      list(rn = "R11743"),
      list(ec = "1.5.98.3"),
      list(rn = "R09095"),
      list(ec = "1.1.98.5"),
      
      # NADH reduction by ferredoxin
      list(ec = "1.6.1.4"),
      list(ec = "7.2.1.2"),
      list(rn = "R05875"),
      
      # Quinone reduction by NADH
      list(ec = "1.6.5.9"),
      list(ec = "7.2.1.1"),
      
      # NADPH reduction by electron carriers
      list(ec = "1.6.5.2"),
      list(ec = "1.6.5.10"),
      list(ec = "1.6.5.5"),
      list(ec = "1.18.1.2"),
      
      # ATP formation
      list(ec = "7.2.2.1"),
      list(ec = "7.2.3.1"),
      
      # Formate metabolism
      list(ec = "1.17.1.11"),
      
      # Formaldehyde metabolism
      list(ec ="1.5.1.15"),
      list(ec ="3.5.1.10"),
      list(ec = "1.5.1.6"),
      list(rn ="R09093"),
      
      # Galactose metabolism
      list(rn = "R03034"),
      
      # Acetogenesis
      list(rn = "R08433"),
      
      # Iron oxidation
      list(ec ="1.16.9.1"),
      
      # Arsenic oxidation
      list(ec = "1.20.2.1"),
      
      # Carbon monoxide oxidation
      list(rn ="R11168"),
      
      # Photosynthesis
      list(ec = "7.1.1.10"),
      list(md = "M00597"),
      list(md = "M00598"),
      list(ko = "K04641")
  )
  
  # Initialize empty list to store results
  kegg_metadata <- list()
  
  # Loop over calls
  for (i in seq_along(calls)) {
    args <- calls[[i]]
    kegg_metadata[[i]] <- do.call(get_kegg_metadata, args)
    svMisc::progress(i, max.value = length(calls))
  }
  
  # Bind all results together
  kegg_metadata <- dplyr::bind_rows(kegg_metadata)
  
# === Format enzymes ===
  # Find subunits in enzyme_combined
  enzyme_table <- format_nonmodule_enzymes(kegg_metadata)
  
  # Get sets of KO IDs that give complete enzymes
  enzyme_combined <- purrr::imap(enzyme_table, function(df, md) {
    if (!is.null(df)) {
      get_complete_enzyme_combinations(df)
    } else {
      NULL
    }
  })
  
  # Remove NULL entries
  enzyme_combined <- purrr::compact(enzyme_combined)
  
  # Convert into long dataframe
  enzyme_combined <- expand_enzyme_ko_df(enzyme_combined)
  
  # Join with metadata and summarize
  enzyme_combined  <- summarize_enzyme_annotations(enzyme_combined, kegg_metadata, remove_na = FALSE)
  
  # Collapse across shared ko-rn mappings
  enzyme_combined <- collapse_by_ko_set(enzyme_combined)
  
# === Add reactions with missing KO IDs ===
  # Identify rows in kegg_metadata where ko is NA
  kegg_missing_ko <- kegg_metadata %>%
    dplyr::filter(is.na(ko))
  
  # Format columns to match enzyme_combined
  kegg_missing_ko <- kegg_missing_ko %>%
    dplyr::mutate(
      ko_set = NA_character_,  # explicitly NA to match type
      md = "NA"   
    ) %>%
    dplyr::select(colnames(enzyme_combined))  # ensure same column order
  
  # Add to enzyme_combined
  enzyme_combined <- dplyr::bind_rows(enzyme_combined, kegg_missing_ko)
  
# === Format as reference model for app  ===  
  model <- dplyr::tibble(
    rn        = enzyme_combined$rn,
    ec        = enzyme_combined$ec,
    eq        = enzyme_combined$eq,
    way       = "Bidirectional",
    ko        = enzyme_combined$ko_set,
    symbol    = enzyme_combined$symbol,
    name      = enzyme_combined$name,
    map       = map,
    md        = enzyme_combined$md
  )
  
#===  Export ===  
  fp <- paste0(network_directory, "\\data\\keggNetworks\\", map, ".csv")
  write.csv(model, fp, row.names = FALSE)
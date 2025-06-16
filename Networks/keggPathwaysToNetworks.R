# Translate KEGG Pathways to Networks
# This script translates KEGG metabolic pathways into reference network models for 
# the app.  For a given pathway map, it downloads data from KEGG, including the reaction 
# equation, reaction ID, EC number, and KO name, and KO IDs.  It downloads data 
# for all modules from the map, including the md definition.  From the module
# definition, it finds sets of KO IDs that make functional enzymes.  If enzymes
# are not in modules, an algorithm is used to identify subunits required 
# to make a functional enzyme.  The final output is a reference network with rows 
# for reactions and columns for reaction equation and other metadata.  
# Author: Timothy Hackmann
# Date:  25 April 2025

# === Set directory ===
  network_directory <- FileLocator::getCurrentFileLocation()

# === Load external R files ===
  setwd(network_directory)
  source("functions/helperFunctions.R", local = TRUE)

# === Define KEGG map IDs to process ===
  map_ids <- c("map00010", "map00620", "map00020", "map00190", "map00910", 
               "map00195", "map00710", "map00920", "map00720", "map00650",
               "map00640", "map00030", "map00051", "map00052", "map00053", 
               "map00500", "map00680", "map00040", "map00630"
               )  
  
# === Build reference models ===
  for (map in map_ids) {
    # === Get all reactions for KEGG pathway ===
    message("Processing: ", map)
    
    # Get modules or reactions
      if(map=="map00190"|map=="map00195")
      {
        md <- map_to_md(map)
      }else{
        rn <- map_to_rn(map, use_modules = FALSE)
      }
    
    # Get metadata
      kegg_metadata <- list()
      if(map=="map00190"|map=="map00195")
      {
        for (i in seq_along(md)) {
          kegg_metadata[[i]] <- get_kegg_metadata(md = md[i])
          svMisc::progress(i, max.value = length(md))
        }
      }else{
        for (i in seq_along(rn)) {
          kegg_metadata[[i]] <- get_kegg_metadata(rn = rn[i])
          svMisc::progress(i, max.value = length(rn))
        }
      }
      kegg_metadata <- dplyr::bind_rows(kegg_metadata)
  
  # === Format enzymes in KEGG modules ===
    # Get module IDs for the pathway
    md <- map_to_md(map)
  
    # Get metadata for each module
    module_metadata <- purrr::set_names(md) %>%
      purrr::map(get_module_metadata)
    
    # Convert module definitions to tables
    module_table <- purrr::map(module_metadata, function(meta) {
      if (!is.null(meta$definition)) {
        parse_module_definition(meta$definition)
      } else {
        NULL
      }
    })
    
    # Get sets of KO IDs that give complete enzymes
    module_enzymes <- purrr::imap(module_table, function(df, md) {
      if (!is.null(df)) {
        get_complete_enzyme_combinations(df)
      } else {
        NULL
      }
    })
    
    # Remove NULL entries
    module_enzymes <- purrr::compact(module_enzymes)
    
    # Convert into long dataframe
    module_enzymes <- expand_enzyme_ko_df(module_enzymes)
    
    # Join with metadata and summarize
    module_enzymes  <- summarize_enzyme_annotations(module_enzymes, kegg_metadata, remove_na = TRUE)

    # Collapse across shared ko-rn mappings
    module_enzymes <- collapse_by_ko_set(module_enzymes)
    
  # === Format enzymes not in modules ===
    # Get KO IDs not in modules
    kegg_metadata_cleaned <- filter_nonmodule_kos(kegg_metadata, module_enzymes, module_metadata)
    
    if(length(kegg_metadata_cleaned)>0 & nrow(kegg_metadata_cleaned)>0)
    {
      # Find subunits in other_enzymes
      other_enzyme_table <- format_nonmodule_enzymes(kegg_metadata_cleaned)
          
      # Get sets of KO IDs that give complete enzymes
      other_enzymes <- purrr::imap(other_enzyme_table, function(df, md) {
        if (!is.null(df)) {
          get_complete_enzyme_combinations(df)
        } else {
          NULL
        }
      })
      
      # Remove NULL entries
      other_enzymes <- purrr::compact(other_enzymes)
      
      # Convert into long dataframe
      other_enzymes <- expand_enzyme_ko_df(other_enzymes)
      
      # Join with metadata and summarize
      other_enzymes  <- summarize_enzyme_annotations(other_enzymes, kegg_metadata)
      
      # Collapse across shared ko-rn mappings
      other_enzymes <- collapse_by_ko_set(other_enzymes)
    }
    
  # === Combine module enzymes and non-module enzymes ===  
    if(length(kegg_metadata_cleaned>0))
    {
      enzyme_combined <- dplyr::bind_rows(module_enzymes, other_enzymes) %>%
        dplyr::arrange(is.na(md), md, rn)
    }else{
      enzyme_combined <- module_enzymes
    }
    
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
  }
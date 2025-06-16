# Preprocess Data for Shiny App
# This script generates data files for the app
# It is not called during app execution
# Requirements
# - Packages in install//installPackages.R
# Author: Timothy Hackmann
# Date: 13 June 2025

# === Set system locale ===
Sys.setlocale("LC_ALL", "C")

# === Get app directory ===
  app_directory <- FileLocator::getCurrentFileLocation()
  app_directory <- dirname(app_directory)
  
# === Load external R files ===
  # Load external R files
  setwd(app_directory)
  source("functions//helperFunctions.R", local = TRUE) 
  source("functions//loadDataFunctions.R", local = TRUE)
  source("functions//plotFunctions.R", local = TRUE) 
  source("modules//predictionsMachineLearning//functions.R", local = TRUE) 
  source("preprocessing//functions.R", local = TRUE) 
  
# === Preprocess data ===
  # --- Clean database file and add links ---
    # Get data
      data <- load_raw_database(force_reload = TRUE)

    # Clean data
      data[] <- lapply(data, as.character)

      data_vars <- c("BacDive_Antibiotic_resistance", "BacDive_Antibiotic_sensitivity", "BacDive_Cell_length",
                     "BacDive_Cell_shape", "BacDive_Cell_width", "BacDive_Colony_size", 
                     "BacDive_Enzyme_activity", "BacDive_Flagellum_arrangement", "BacDive_Gram_stain", 
                     "BacDive_Incubation_period", "BacDive_Indole_test", "BacDive_Metabolite_production", 
                     "BacDive_Metabolite_utilization", "BacDive_Motility", "BacDive_Oxygen_tolerance",
                     "BacDive_pH_for_growth", "BacDive_Pathogenicity_animal", "BacDive_Pathogenicity_human",
                     "BacDive_Pathogenicity_plant", "BacDive_Salt_concentration", "BacDive_Salt_concentration_unit",
                     "BacDive_Spore_formation", "BacDive_Temperature_for_growth", "BacDive_Voges_proskauer",
                     "BacDive_Isolation_category_1", "BacDive_Isolation_category_2", "BacDive_Isolation_category_3",
                     "FAPROTAX_Type_of_metabolism")

      is_numeric_vars <- c(FALSE, FALSE, TRUE,
                           FALSE, TRUE, TRUE,
                           FALSE, FALSE, FALSE,
                           TRUE, FALSE, FALSE,
                           FALSE, FALSE, FALSE,
                           TRUE, FALSE, FALSE,
                           FALSE, TRUE, FALSE,
                           FALSE, TRUE, FALSE,
                           FALSE, FALSE, FALSE,
                           FALSE)

      data[data_vars] <- mapply(clean_external_data, x = data[data_vars], is_numeric = is_numeric_vars, SIMPLIFY = FALSE)

    # Convert salt concentration to uniform units (mol L-1)
      data$BacDive_Salt_concentration <- convert_salt_concentration(concentration = data$BacDive_Salt_concentration, unit = data$BacDive_Salt_concentration_unit)

    # Replace a complex antibiotic name with a simpler one (makes easier to match with regex)
      data$BacDive_Antibiotic_resistance = gsub(pattern="0129 \\(2,4-Diamino-6,7-di-iso-propylpteridine phosphate\\)", replacement = "2,4-Diamino-6,7-diisopropylpteridine", x = data$BacDive_Antibiotic_resistance)
      data$BacDive_Antibiotic_sensitivity = gsub(pattern="0129 \\(2,4-Diamino-6,7-di-iso-propylpteridine phosphate\\)", replacement = "2,4-Diamino-6,7-diisopropylpteridine", x = data$BacDive_Antibiotic_sensitivity)

    # Combine columns
      # Pathogenicity
      data = collapse_columns(df = data,
                              cols=c("BacDive_Pathogenicity_animal", "BacDive_Pathogenicity_human", "BacDive_Pathogenicity_plant"),
                              new_col_name = "BacDive_Pathogenicity",
                              delete = "BacDive_Pathogenicity_",
                              positive_value ="positive",
                              negative_value="negative")

      # Type of metabolism (Fermentation Explorer)
        data <- data %>%
        dplyr::mutate(Fermentation_Explorer_Type_of_metabolism = dplyr::coalesce(
          Literature_Type_of_metabolism,
          VPI_Type_of_metabolism,
          Bergey_Type_of_metabolism
        ))

      # Metabolites produced (Fermentation Explorer)
      data <- data %>%
        dplyr::mutate(
          Fermentation_Explorer_Major_metabolites_produced = dplyr::coalesce(Literature_Major_end_products, VPI_Major_end_products, Bergey_Major_end_products),
          Fermentation_Explorer_Minor_metabolites_produced = dplyr::if_else(
            !is.na(Literature_Major_end_products),  # If Literature_Major_end_products exists
            Literature_Minor_end_products,         # Pick Literature_Minor_end_products only
            dplyr::coalesce(VPI_Minor_end_products, Bergey_Minor_end_products)  # Otherwise, use coalesce for others
          )
        )

      data$Fermentation_Explorer_Major_metabolites_produced[data$Fermentation_Explorer_Major_metabolites_produced == "NA"] <- NA
      data$Fermentation_Explorer_Minor_metabolites_produced[data$Fermentation_Explorer_Minor_metabolites_produced == "NA"] <- NA
      data <- data %>%
        dplyr::mutate(
          Fermentation_Explorer_Metabolites_produced = dplyr::case_when(
            is.na(Fermentation_Explorer_Major_metabolites_produced) ~ Fermentation_Explorer_Minor_metabolites_produced,
            is.na(Fermentation_Explorer_Minor_metabolites_produced) ~ Fermentation_Explorer_Major_metabolites_produced,
            TRUE ~ paste(Fermentation_Explorer_Major_metabolites_produced, Fermentation_Explorer_Minor_metabolites_produced, sep = ";")
          )
        )
      

      # Metabolites utilized (Fermentation Explorer)
      data <- data %>%
        dplyr::mutate(Fermentation_Explorer_Metabolites_utilized = dplyr::coalesce(
          Literature_Substrates_for_end_products,
          Bergey_Substrates_for_end_products
        ))
      
      # LPSN taxonomy
      data$LPSN_Taxonomy <- apply(
        data[c("LPSN_Domain", "LPSN_Phylum", "LPSN_Class", "LPSN_Order", "LPSN_Family", "LPSN_Genus", "LPSN_Species")],
        1,
        collapse_taxonomy
      )

      # GTDB taxonomy
      data$GTDB_Taxonomy <- apply(
        data[c("GTDB_Domain", "GTDB_Phylum", "GTDB_Class", "GTDB_Order", "GTDB_Family", "GTDB_Genus", "GTDB_Species")],
        1,
        collapse_taxonomy
      )

      # NCBI taxonomy
      data$NCBI_Domain = NA
      data$NCBI_Taxonomy <- apply(
        data[c("NCBI_Domain", "NCBI_Phylum", "NCBI_Class", "NCBI_Order", "NCBI_Family", "NCBI_Genus", "NCBI_Species")],
        1,
        collapse_taxonomy
      )

      # Bergey taxonomy
      data$Bergey_Domain = NA
      data$Bergey_Taxonomy <- apply(
        data[c("Bergey_Domain", "Bergey_Phylum", "Bergey_Class", "Bergey_Order", "Bergey_Family", "Bergey_Genus", "Bergey_Species")],
        1,
        collapse_taxonomy
      )

    # Add links
      data$LPSN_Page_link <- createLinkButton(data$LPSN_Page_link)
      data$NCBI_Taxonomy_ID_link <- createLink(data$NCBI_Taxonomy_ID, "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=")
      data$GOLD_Organism_ID_link <- createLink(data$GOLD_Organism_ID, "https://gold.jgi.doe.gov/organism?id=")
      data$GOLD_Project_ID_link <- createLink(data$GOLD_Project_ID, "https://gold.jgi.doe.gov/project?id=")
      data$IMG_Genome_ID_link <- createLink(data$IMG_Genome_ID, "https://img.jgi.doe.gov/cgi-bin/m/main.cgi?section=TaxonDetail&page=taxonDetail&taxon_oid=")
      data$BacDive_ID_link <- createLink(data$BacDive_ID, "https://bacdive.dsmz.de/strain/")
      data$GTDB_ID_link <- createLink(data$GTDB_ID, "https://gtdb.ecogenomic.org/searches?s=al&q=")
      data$Bergey_Article_link <- createLinkButton(data$Bergey_Article_link)

    # Keep only columns used in app
      data <- data %>% dplyr::select(
        "Genus", "Species", "Subspecies", "Strain",
        "LPSN_ID", "LPSN_Page_link", "LPSN_Taxonomy",
        "GTDB_ID", "GTDB_ID_link", "GTDB_Taxonomy",
        "GOLD_Organism_ID", "GOLD_Organism_ID_link", "GOLD_Project_ID", "GOLD_Project_ID_link",
        "NCBI_Taxonomy_ID", "NCBI_Taxonomy_ID_link", "NCBI_Taxonomy",
        "IMG_Genome_ID", "IMG_Genome_ID_link", "IMG_Genome_ID_max_quality",
        "BacDive_ID", "BacDive_ID_link",
        "BacDive_Antibiotic_resistance", "BacDive_Antibiotic_sensitivity", "BacDive_Cell_length", "BacDive_Cell_shape",
        "BacDive_Cell_width", "BacDive_Colony_size", "BacDive_Enzyme_activity", 
        "BacDive_Flagellum_arrangement", "BacDive_Gram_stain", "BacDive_Incubation_period",
        "BacDive_Indole_test", "BacDive_Metabolite_production", "BacDive_Metabolite_utilization",  
        "BacDive_Motility", "BacDive_Oxygen_tolerance", "BacDive_pH_for_growth", "BacDive_Pathogenicity",
        "BacDive_Salt_concentration", "BacDive_Spore_formation", "BacDive_Temperature_for_growth", "BacDive_Voges_proskauer",
        "BacDive_Isolation_category_1", "BacDive_Isolation_category_2", "BacDive_Isolation_category_3",
        "Bergey_Article_link", "Bergey_Taxonomy",
        "Fermentation_Explorer_Type_of_metabolism", "Fermentation_Explorer_Major_metabolites_produced", 
        "Fermentation_Explorer_Minor_metabolites_produced", "Fermentation_Explorer_Metabolites_produced", 
        "Fermentation_Explorer_Metabolites_utilized",
        "FAPROTAX_Type_of_metabolism"
      )

      # Simplify column names
      data <- dplyr::rename(data,
                            BacDive_Cell_length_in_microns = "BacDive_Cell_length",
                            BacDive_Cell_width_in_microns = "BacDive_Cell_width",
                            BacDive_Incubation_period_in_days = "BacDive_Incubation_period",
                            BacDive_Temperature_for_growth_in_degrees = "BacDive_Temperature_for_growth",
                            BacDive_Incubation_period_in_days = "BacDive_Incubation_period",
                            BacDive_Salt_for_growth_in_moles_per_liter = "BacDive_Salt_concentration",
                            BacDive_Voges_Proskauer = "BacDive_Voges_proskauer",
                            BacDive_Metabolites_produced = "BacDive_Metabolite_production",
                            BacDive_Metabolites_utilized = "BacDive_Metabolite_utilization",
      )
      
    # Move database names in column names to the end
    colnames(data) <- move_prefix_to_end(colnames = colnames(data))

    # Remove underscores in column names
    colnames(data) <- gsub(pattern = "_", replacement = " ", x = colnames(data))
    
    # Convert character columns to factor
      data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

    # Save file
      fp <- "data/database/database_clean.csv"
      save_as_zip(data = data, fp = fp)

  # --- Generate query filters ---
    # Get data
      # Load layout
      data <- load_database(force_reload = TRUE)

      # Add taxonomy (LPSN)
      col_name <- "LPSN Taxonomy"
      data <- expand_and_merge_taxonomy(data = data, col_name = col_name)

      # Generate filters
      query_filters <- list(
        # Taxonomy
        create_query_filter("Phylum", data),
        create_query_filter("Class", data),
        create_query_filter("Order", data),
        create_query_filter("Family", data),
        create_query_filter("Genus", data),
        create_query_filter("Species", data),
        create_query_filter("Subspecies", data),
        create_query_filter("Strain", data),
        create_query_filter("NCBI Taxonomy ID", data),
        create_query_filter("GOLD Organism ID", data, delimited = TRUE, delimiter = ","),
        create_query_filter("GOLD Project ID", data, delimited = TRUE, delimiter = ","),
        create_query_filter("IMG Genome ID", data, delimited = TRUE, delimiter = ","),
        create_query_filter("IMG Genome ID max quality", data),
        create_query_filter("BacDive ID", data),
        
        # Physiology/Function
        create_query_filter("Type of metabolism (FAPROTAX)", data, delimited = TRUE),
        create_query_filter("Type of metabolism (Fermentation Explorer)", data, delimited = TRUE),
        create_query_filter("Metabolites produced (Fermentation Explorer)", data, delimited = TRUE),
        create_query_filter("Major metabolites produced (Fermentation Explorer)", data, delimited = TRUE),
        create_query_filter("Minor metabolites produced (Fermentation Explorer)", data, delimited = TRUE),
        create_query_filter("Metabolites utilized (Fermentation Explorer)", data, delimited = TRUE),
        create_query_filter("Metabolites produced (BacDive)", data, delimited = TRUE),
        create_query_filter("Metabolites utilized (BacDive)", data, delimited = TRUE),
        create_query_filter("Enzyme activity (BacDive)", data, delimited = TRUE),
        create_query_filter("Oxygen tolerance (BacDive)", data, delimited = TRUE),
        create_query_filter("Pathogenicity (BacDive)", data, delimited = TRUE),
        
        create_query_filter("Temperature for growth in degrees (BacDive)", data, type = "double", label = "Temperature for growth in degrees"),
        create_query_filter("Salt for growth in moles per liter (BacDive)", data, type = "double", label = "Salt for growth in moles per liter"),
        create_query_filter("pH for growth (BacDive)", data, type = "double"),
        create_query_filter("Incubation period in days (BacDive)", data, type = "double"),
        
        create_query_filter("Indole test (BacDive)", data, delimited = TRUE),
        create_query_filter("Voges Proskauer (BacDive)", data, delimited = TRUE, label = "Voges Proskauer"),
        create_query_filter("Motility (BacDive)", data, delimited = TRUE),
        create_query_filter("Antibiotic resistance (BacDive)", data, delimited = TRUE),
        create_query_filter("Antibiotic sensitivity (BacDive)", data, delimited = TRUE),
        
        # Morphology
        create_query_filter("Cell length in microns (BacDive)", data, type = "double"),
        create_query_filter("Cell width in microns (BacDive)", data, type = "double"),
        create_query_filter("Cell shape (BacDive)", data, delimited = TRUE),
        create_query_filter("Colony size (BacDive)", data, type = "double"),
        create_query_filter("Flagellum arrangement (BacDive)", data, delimited = TRUE),
        create_query_filter("Gram stain (BacDive)", data, delimited = TRUE),
        create_query_filter("Spore formation (BacDive)", data, delimited = TRUE),
        
        # Isolation traits
        create_query_filter("Isolation category 1 (BacDive)", data, delimited = TRUE),
        create_query_filter("Isolation category 2 (BacDive)", data, delimited = TRUE),
        create_query_filter("Isolation category 3 (BacDive)", data, delimited = TRUE)
      )  

    # Save object to file
    data_fp = paste0("data/query_filters/query_filters.rds")
    saveRDS(query_filters, file = data_fp)

    # Generate placeholder filters (simple filters to load at app startup)
    query_filters_simple <- list(
      create_query_filter("Type of metabolism (FAPROTAX)", data, delimited = TRUE)
    )
    
    # Save object to file
    data_fp = paste0("data/query_filters/query_filters_simple.rds")
    saveRDS(query_filters_simple, file = data_fp)
    
  # --- Generate files for phylogenetic tree ---
    # Load tree
    data_fp <- "data/tree/tree.zip"
    unzip(data_fp, exdir = tempdir())
    tree_file <- list.files(tempdir(), pattern = "\\.tre$|\\.tree$|\\.nwk$", full.names = TRUE)
    tree <- ape::read.tree(tree_file)

    # Get layouts for phylogenetic tree
      layout_types <- c("rectangular", "daylight", "equal_angle")

      for (layout_type in layout_types) {
        layout <- get_tree_layout(tree, layout_type = layout_type)
        data_fp <- paste0("data/tree/layout_tree_", layout_type, ".rds")
        saveRDS(layout, data_fp)
      }

    # Get nodes from tips to root
        nodes_to_root = get_nodes_to_root(tree = tree)
        data_fp = "data/tree/nodes_to_root.rds"
        saveRDS(nodes_to_root, file = data_fp)

    # Plot branches for all organisms
      for (layout_type in layout_types) {
        # Load layout
        data_fp = paste0("data/tree/layout_tree_", layout_type, ".rds")
        layout <- readRDS(data_fp)

        # Plot branches
        if(layout_type=="rectangular")
        {
          plot <- ggtree_to_plotly(layout = layout, type = layout_type, color = "#e5e5e5", coord_fixed = FALSE)
        }else{
          plot <- ggtree_to_plotly(layout = layout, type = layout_type, color = "#e5e5e5", coord_fixed = TRUE, x_to_y_ratio = 0.8)
        }

        # Save plot to file
        data_fp = paste0("data/tree/plot_branches_all_", layout_type, ".rds")
        saveRDS(plot, file = data_fp)
      }

    # Plot tip points for all organisms
      for (layout_type in layout_types) {
        # Load layout and data
        data_fp = paste0("data/tree/layout_tree_", layout_type, ".rds")
        layout <- readRDS(data_fp)
        data <- load_database()

        # Format layout
        layout_tips <- layout %>% dplyr::filter(isTip == TRUE)
        layout_tips <- add_taxonomy_to_layout(layout = layout_tips, layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG_Genome_ID_max_quality")
        layout_tips <- add_fill_to_layout(layout = layout_tips, group = "Phylum", lighten_amount = 0.95)
        layout_tips <- add_color_to_layout(layout = layout_tips, group = "Phylum", lighten_amount = 0.9)

        # Create scatter plot
        # Plot branches
        if(layout_type=="rectangular")
        {
          plot <- plot_scatterplot(df = layout_tips,
                                   color = layout_tips$color, fill =  layout_tips$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                   label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                   coord_fixed=FALSE, x_to_y_ratio=NULL)
        }else{
          plot <- plot_scatterplot(df = layout_tips,
                                   color = layout_tips$color, fill =  layout_tips$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                                   label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                   coord_fixed=TRUE, x_to_y_ratio=0.8)
        }

        # Save plot to file
        data_fp = paste0("data/tree/plot_tips_all_", layout_type, ".rds")
        saveRDS(plot, file = data_fp)
      }

  # --- Generate files for t-SNE plot ---
      # Plot scatter plot for all organisms
        # Load layout and data
        layout <- load_layout_tsne()
        data <- load_database()

        #Format layout
        layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "IMG_Genome_ID_max_quality", taxonomy = data, taxonomy_ID = "IMG_Genome_ID_max_quality")
        layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.95)
        layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.9)

        #Create scatter plot
        plot = plot_scatterplot(df = layout,
                         color = layout$color, fill =  layout$fill, stroke = 1, size = 5, shape = "circle", alpha=1,
                         label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                         ticklen.x = 4, ticklen.y = 4, showticklabels.x = TRUE, showticklabels.y = TRUE, title.x = "Dimension 1", title.y = "Dimension 2",
                         coord_fixed=TRUE, x_to_y_ratio=1)

        # Save plot to file
        data_fp = paste0("data/tnse/plot_tsne_all.rds")
        saveRDS(plot, file = data_fp)

  # --- Generate random forest models ---
        # Define variables and query strings
        variables <- list(
          
          # Type of metabolism
          "fermentation" = "grepl(\"(?<=^|;)Fermentation(?=;|$)\", `Type of metabolism (Fermentation Explorer)`, perl = TRUE)",
          "methanogenesis" = "grepl(\"(?<=^|;)Methanogenesis(?=;|$)\", `Type of metabolism (Fermentation Explorer)`, perl = TRUE)",

          # Metabolites utilized
          "nitrate" = "grepl(\"(?<=^|;)nitrate(?=;|$)\", `Metabolites utilized (BacDive)`, perl = TRUE)",
          
          # Metabolites produced
          "acetate" = "grepl(\"(?<=^|;)acetate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "butyrate" = "grepl(\"(?<=^|;)butyrate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "CO2" = "grepl(\"(?<=^|;)CO2(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "CH4" = "grepl(\"(?<=^|;)CH4(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "ethanol" = "grepl(\"(?<=^|;)ethanol(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "formate" = "grepl(\"(?<=^|;)formate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "H2" = "grepl(\"(?<=^|;)H2(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "isobutyrate" = "grepl(\"(?<=^|;)isobutyrate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "isovalerate" = "grepl(\"(?<=^|;)isovalerate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "lactate" = "grepl(\"(?<=^|;)D-lactate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE) | grepl(\"(?<=^|;)D-lactate(?=;|$)\", `Major metabolites produced (Fermentation Explorer)`, perl = TRUE) | grepl(\"(?<=^|;)lactate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "propionate" = "grepl(\"(?<=^|;)propionate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "pyruvate" = "grepl(\"(?<=^|;)pyruvate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          "succinate" = "grepl(\"(?<=^|;)succinate(?=;|$)\", `Metabolites produced (Fermentation Explorer)`, perl = TRUE)",
          
          # Physiology/morphology
          "anaerobe" = "grepl(\"(?<=^|;)anaerobe(?=;|$)\", `Oxygen tolerance (BacDive)`, perl = TRUE) | grepl(\"(?<=^|;)obligate anaerobe(?=;|$)\", `Oxygen tolerance (BacDive)`, perl = TRUE)",
          "gram_positive" = "grepl(\"(?<=^|;)positive(?=;|$)\", `Gram stain (BacDive)`, perl = TRUE)",
          "spore_formation" = "grepl(\"(?<=^|;)positive(?=;|$)\", `Spore formation (BacDive)`, perl = TRUE)",
          "motility" = "grepl(\"(?<=^|;)positive(?=;|$)\", `Motility (BacDive)`, perl = TRUE)",
          "motility_non_gliding" = "grepl(\"(?<=^|;)positive(?=;|$)\", `Motility (BacDive)`, perl = TRUE) & grepl(\"(?<=^|;)gliding(?=;|$)\", `Flagellum arrangement (BacDive)`, perl = TRUE)",
          
          # Growth
          "thermophile" = "`Temperature for growth in degrees (BacDive)`> 45",
          "halophile" = "`Salt for growth in moles per liter (BacDive)` > 3",
          "slow_growth" = "`Incubation period in days (BacDive)` > 7",
          
          # Pathogenecity
          "animal_pathogen" = "grepl(\"(?<=^|;)animal(?=;|$)\", `Pathogenicity (BacDive)`, perl = TRUE)",
          "plant_pathogen" = "grepl(\"(?<=^|;)plant(?=;|$)\", `Pathogenicity (BacDive)`, perl = TRUE)"
        )
        
      # Process each variable
      for (i in seq_along(variables)) {
        var_name <- names(variables)[i]
        query_string <- variables[[i]]
        
        generate_rf(
          var_name = var_name,
          query_string = query_string,
          predictors_to_keep = 1
        )
        
        svMisc::progress(i, max.value = length(variables))
      }
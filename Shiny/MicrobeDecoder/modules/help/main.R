# Define the Help Module in Shiny App
# This script defines the user interface (UI) and server for the help module.  
# Author: Timothy Hackmann
# Date: 26 February 25

# === Define user interface (UI) ===
helpUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Title
    create_title_div("Help"),
    
    bslib::layout_sidebar(
      
    #Sidebar
    sidebar = bslib::navset_pill(
                          id = ns("subtabs"),
                          bslib::nav_panel( 
                                    title = "Overview",
                                    value = "Overview",
                                    id = "Overview"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits from taxonomy",
                                    value = "Predict traits from taxonomy",
                                    id = "Predict traits from taxonomy"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits with metabolic networks",
                                    value = "Predict traits with metabolic networks",
                                    id = "Predict traits with metabolic networks"
                          ),
                          bslib::nav_panel(
                                    title = "Predict traits with machine learning",
                                    value = "Predict traits with machine learning",
                                    id = "Predict traits with machine learning"
                          ),
                          bslib::nav_panel(
                            title = "Search database",
                            value = "Search database",
                            id = "Search database"
                          ),
                          bslib::nav_panel(
                            title = "Download database",
                            value = "Download database",
                            id = "Download database"
                          ),
                          bslib::nav_panel(
                            title = "History",
                            value = "History",
                            id = "History"
                          )
                        ),
    #Main content area
    div(
        shiny::uiOutput(ns("main_content"))
      )
    )
  )
}

# === Define server ===
helpServer <- function(input, output, session, x, selected_tab) {
  ns <- session$ns
  
  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "help")
  
  # --- Render user interface (UI) ---
  output$main_content <- shiny::renderUI({
    switch(input$subtabs,
           "Overview" = div(
             h3("Overview of Microbe Decoder"),
             p(shiny::uiOutput(ns("video_overview")))
           ),
           
           "Predict traits from taxonomy" = div(
             tags$h3("Predict traits from taxonomy"),
             h4("Overview"),
             p("This tool predicts traits for organisms given their taxonomy. After the user selects organisms for prediction, the tool finds organisms with matching taxonomy in the internal database. The tool then calculates the fraction (0 to 1) of matching taxa positive for a trait.  This fraction is the probability of the trait.  This approach is similar to that used by ", url_FAPROTAX, ", except the latter reports only traits with probability of 1."),
             
             h4("Use"),
             
             tags$img(
               src = "screenshots/taxonomy.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
               
               tags$h5("Organisms (taxa)"),
               p("Choose organisms from the database or by uploading a file."),
               p("An uploaded file should be a .csv, .txt, .xlsx, or .zip and follow one of the formats below."),
               p("Example files:"),
               
               tags$ol(
                 class = "circled-letter-list",
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_1"), label = "E. coli (generic format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_2"), label = "Bacterial isolates from the rumen (IMG/M format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_3"), label = "Metagenomic species from the infant gut (generic format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_4"), label = "ASVs from the Winogradsky columns (DADA2 format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_5"), label = "MAGs from Black Sea (GTDB format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_6"), label = "Bacteria from QIIME2 tutorial (QIIME2 format)")),
                 tags$li(shiny::downloadLink(outputId = ns("downloadTaxa_7"), label = "Bacteria from MetaPhlAn tutorial (MetaPhlAn format)"))
               ),
               
               tags$h5("Traits"),
               p("Choose traits to predict here.  For the Other traits tab, detailed traits can be specified using a query builder."),
               
               tags$h5("Show advanced settings"),
               p("Click here to show additional settings (optional)."),
               tags$i("Hide poorly predicted traits"),
               p("When turned on, traits that are known to be predicted poorly are hidden and cannot be selected by the user.  This applies to standard traits only."),
               tags$i("Probability threshold"),
               p("When this slider is set to 0.5, only traits with predicted probability of at least 0.5 are shown."),
               tags$i("All taxonomic ranks must match"),
               p("When turned on, query organisms must match database organisms at all ranks (species to phylum).  When turned off, the tool is less strict; it starts matching at the most specific level (genus and species), then moves up ranks until a match is found.  Ranks in query organisms that are \"NA\" are ignored.  Turning it off leads to more matches."),
               tags$i("Ignore species"),
               p("When turned on, the rank of species is ignored when matching.  Turning it off leads to more matches"),
               tags$i("Ignore missing values in database"),
               p("When turned on, matching organisms with \"NA\" for a trait are ignored.  This leads to more traits being predicted."),
               tags$i("Taxonomy"),
               p("This switch controls the taxonomy in the internal database used for matching."),
               
               
               tags$h5("Make predictions"),
               p("After selecting organisms and traits, click here to make predictions."),
               
               tags$h5("Download results"),
               p("Click here to download a .csv of the results (probabilities of predicted traits)."),
               
               tags$h5("Treemap|Heatmap"),
               p("Click these tabs to explore results in interactive plots.  Only traits with values above the probability threshold (Advanced settings) are shown."),
               # tags$i("Summary"),
               # p("This plot shows the percentage of organisms positive for each trait."),
               tags$i("Treemap"),
               p("This plot shows the relative abundance of each trait (percentage of organisms positive for a given trait relative to all traits)."),
               tags$i("Heatmap"),
               p("This plot shows the probability of each trait for each organism.")
             ),
             
             h4("Interactive example"),
             tags$ol(class = "circled-letter-list",
                     tags$li(
                       shiny::actionLink(
                         ns("open_demo_taxonomy_rumen"),
                         "Bacterial isolates from the rumen"
                       )
                     )
             ),
             p(
               HTML("Results are for metabolites produced by previously uncharacterized bacteria from the rumen. Some metabolites (e.g., acetate) are predicted with high probability across bacteria, while others (e.g., H<sub>2</sub>) are predicted across fewer organisms. The specific pattern closely matches "),
               tags$a(
                 href = "https://www.science.org/doi/10.1126/sciadv.adg8687",
                 target = "_blank",
                 "observed values"
               ),
               "."
             )
           ),
           
           "Predict traits with metabolic networks" = div(
             h3("Predict traits with metabolic networks"),
             h4("Overview"),
             p("This tool predicts traits for an organism by building a metabolic network from the genome. After the user selects gene functions for a genome, the tool builds a network of biochemical reactions.  It then uses flux balance analysis (FBA) to determine if the network is complete and can metabolize a chosen substrate to end products."),
             
             h4("Use"),
             
             tags$img(
               src = "screenshots/networks.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
               
             tags$h5("Organisms (gene functions)"),
             p("Choose gene functions from the database or by uploading a file."),  
             p("An uploaded file should be a .csv, .tsv, .txt, .ko, or .zip and follow one of the formats below."),
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli (generic format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_2"), label = "B. subtilis (eggNOG format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_3"), label = "P. aeruginosa (KAAS format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_4"), label = "Bacterial isolates from the rumen (IMG/M format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_5"), label = "ASVs from the Winogradsky columns (PICRUSt2 format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_6"), label = "MAGs from Black Sea (generic format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_7"), label = "Bacteria from HUMAnN tutorial (HUMAnN format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_8"), label = "Ancestral bacteria (generic format)"))
             ),
             
             tags$i("Load examples"),
             p("Click here to load organisms from the database known to carry out the type of metabolism chosen below.  The user can select these to check the sensitivity of model predictions."),
             
             tags$i("Choose with file"),
             p("To make a large number of selections from the database, click here to upload a file with names of organisms.  The file should be a .csv or .zip and follow the format below."),
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadNames_1"), label = "Metagenomic species from the infant gut")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadNames_2"), label = "Model isolates of bacteria and archaea"))
             ),
             
             tags$h5("Type of metabolism (reference network)"),
             p("Choose a reference network from the database or by uploading a file.  The tool will check if these reactions are in the genome and if so add them to the biochemical network.  All reactions needed to metabolize a chosen substrate to end products should be included."),
             p(shiny::tagList("An uploaded file should be a .csv or .zip and follow the format below.  Information can come from ", url_KEGG, ".")), 
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadReference_1"), label = "Glucose fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadReference_3"), label = "Methanogenesis"))
             ),
             
             tags$h5("Substrates"),
             p("Choose one or more substrates for the metabolic network here. Any metabolite in the reference network can be chosen."),
             
             tags$h5("End products"),
             p("Choose end products to check here. Any metabolite in the reference network can be chosen."),
             
             tags$h5("Show advanced settings"),
             p("Click here to show additional settings (optional)."),
             tags$i("Unbalanced intermediates"),
             p("Metabolites chosen here are allowed to be produced (or consumed) in infinite quantities. NADH and ATP are examples of metabolites usually chosen to be unbalanced. In the metabolic model, these can accumulate without needing to be regenerated to NAD+ or ADP. This simplifies the model, as reactions for consuming NADH and ATP do not have to be included."),
             tags$i("Flux threshold"),
             p("When this slider is set to 1, only end products with a flux of at least 1 are shown."),
             tags$i("Enzymes must have all subunits"),
             p("When turned on, a biochemical reaction is included in the network only if its enzyme has all subunits (KO IDs).  Turning it off will lead to more reactions being included."),
             
             
             tags$h5("Make predictions"),
             p("After selecting organisms and other settings, click here to make predictions."),
             
             tags$h5("Download results"),
             p("Click here to download a .csv of the results (fluxes for network model).  The higher the fluxes, the faster the reaction or more product that is formed. The flux of substrate is initially set to -1000."),
             
             tags$h5("Treemap|Heatmap|Metabolic network"),
             p("Click these tabs to explore results in interactive plots.  Only fluxes with values above the flux threshold (Advanced settings) are shown."),
             # tags$i("Summary"),
             # p("This plot shows the percentage of organisms with positive fluxes for each end product."),
             tags$i("Treemap"),
             p("This plot shows the relative abundance of each end product (percentage of organisms with positive fluxes for the end product relative to all end products)."),
             tags$i("Heatmap"),
             p("This plot shows the flux of each end product for each organism."),
             tags$i("Metabolic network"),
             p("This plot shows the predicted metabolic network for the organism.   Positive fluxes are highlighted.")
           ),
             
             h4("Interactive example"),
             tags$ol(class = "circled-letter-list",
                     tags$li(
                       shiny::actionLink(
                         ns("open_demo_network_rumen"),
                         "Bacterial isolates from the rumen"
                       )
                     )
             ),
             p("Results are for pathways of glucose fermentation for previously uncharacterized bacteria from the rumen.  Some end products (e.g., acetate) are predicted with high probability across bacteria, while others (e.g., hydrogen) are predicted across fewer organisms.  The specific pattern closely matches ", tags$a(href = "https://www.science.org/doi/10.1126/sciadv.adg8687", target = "_blank", "observed values"), ".")
           ),
           
           "Predict traits with machine learning" = div(
             h3("Predict traits with machine learning"),
             h4("Overview"),
             p("This tool predicts traits for an organism from its genome using machine learning.  After the user selects gene functions for a genome, the tool uses a machine learning algorithm (random forest model) to predict traits.  The tool calculates the fraction (0 to 1) of trees giving a positive prediction.  This fraction is the probability of the trait."),
             p("The user can predict simple traits using pre-trained models.  They can also train their own models to predict more complex traits."),

             h4("Use"),
             
             tags$img(
               src = "screenshots/machineLearning.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
               
             tags$h5("Organisms (gene functions)"),
             p("Choose gene functions from the database or by uploading a file."),
             p("An uploaded file should be a .csv, .tsv, .txt, or .zip and follow one of the formats below."),
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_1"), label = "E. coli (generic format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_2"), label = "B. subtilis (eggNOG format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_3"), label = "P. aeruginosa (KAAS format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_4"), label = "Bacterial isolates from the rumen (IMG/M format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_5"), label = "ASVs from the Winogradsky columns (PICRUSt2 format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_6"), label = "MAGs from Black Sea (generic format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_7"), label = "Bacteria from HUMAnN tutorial (HUMAnN format)")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadFunctions_8"), label = "Ancestral bacteria (generic format)"))
             ),
             
             tags$i("Choose with file"),
             p("To make a large number of selections from the database, click here to upload a file with names of organisms.  The file should be a .csv or .zip and follow the format below."),
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadNames_1"), label = "Metagenomic species from the infant gut")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadNames_2"), label = "Model isolates of bacteria and archaea"))
             ),
             
             tags$h5("Traits or models"),
             p("Choose one of several options for predicting traits."),
             tags$i("Standard traits"),
             p("Choose a trait from a list, and the tool loads a pre-trained random forest model for it."),
             tags$i("Other traits"),
             p("Specify a trait using a query builder, and the tool will train a random forest model for it."),
             tags$i("Model upload"),
             p("Upload one or more .rds files of random forest models.  These files typically come from other tabs."),
             p("Example files:"),
             tags$ol(class = "circled-letter-list",
                     tags$li(shiny::downloadLink(outputId = ns("downloadModel_1"), label = "Fermentation")),
                     tags$li(shiny::downloadLink(outputId = ns("downloadModel_2"), label = "Methanogenesis"))
             ),
             
             tags$h5("Show advanced settings"),
             p("Click here to show additional settings (optional)."),
             tags$i("Probability threshold"),
             p("When this slider is set to 0.5, only traits with predicted probability of at least 0.5 are shown."),
             tags$i("Enable saving of models"),
             p("When turned on, random forest models are saved and available for download.  For speed, this is turned off by default."),
             tags$i("Keep models in cache"),
             p("When turned on, random forest models that were previously loaded are kept in cache.  To preserve memory, this is turned off by default."),
             tags$i("Ignore missing values in database"),
             p("When turned on, matching organisms with \"NA\" for a trait are ignored.  This leads to more traits being predicted."),
             tags$i("Proportion of predictors to keep."),
             p("When this slider is set to 0.1, a random subsample of 10% of the predictors is kept for model training.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Proportion of responses to keep."),
             p("When this slider is set to 0.25, a random subsample of 25% of the responses is kept for model training.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Proportion of data for model training."),
             p("When this slider is set to 0.7, a random subsample of 70% of data is used for training and 30% for evaluation.  The data include both responses and predictors."),
             tags$i("Set seed for subsampling."),
             p("This sets the seed for randomly subsampling predictors and responses.  If kept at the default (123), subsampling will be identical each time the model is trained."),
             tags$i("Set number of trees"),
             p("This sets the number of trees in the random forest model.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Set maximum nodes"),
             p("This sets the number of maximum nodes in the random forest model.  Higher values will increase training time but may improve predictive performance."),
             tags$i("Weight for positive classes of responses."),
             p("When this slider is set to 0.5, positive and negative responses receive equal weight during training.  Increasing it will give more weight to positive responses."),
             tags$i("Name of trait"),
             p("This sets the name of trait in the output, and it does not affect predictive performance.  Only alphanumeric characters are allowed."),
             
             tags$h5("Make predictions"),
             p("After selecting organisms and traits, click here to make predictions."),
             
             tags$h5("Download results"),
             p("Click here to download a .csv of the results (probabilities of predicted traits). Additionally, an .rds file for the random forest can be downloaded. It can be re-uploaded using the Model upload tab."),
             
             tags$h5("Treemap|Heatmap"),
             p("Click these tabs to explore results in interactive plots.  Only traits with values above the probability threshold (Advanced settings) are shown."),
             # tags$i("Summary"),
             # p("This plot shows the percentage of organisms positive for each trait."),
             tags$i("Treemap"),
             p("This plot shows the relative abundance of each trait (percentage of organisms positive for a given trait relative to all traits)."),
             tags$i("Heatmap"),
             p("This plot shows the probability of each trait for each organism."),
             
             tags$h5("Download model"),
             p("Click here to download an .rds file for the random forest. It can be re-uploaded using the Model upload tab."),
             
             tags$h5("Download evaluation metrics"),
             p("Click here to download an .csv file for evaluation metrics for the machine learning model.  The metrics are also shown in plots above."),
             ),

             h4("Interactive example"),
             tags$ol(class = "circled-letter-list",
                     tags$li(
                       shiny::actionLink(
                         ns("open_demo_ML_rumen"),
                         "Bacterial isolates from the rumen"
                       )
                     )
             ),
             p(
               HTML("Results are for metabolites produced by previously uncharacterized bacteria from the rumen. Some metabolites (e.g., acetate) are predicted with high probability across bacteria, while others (e.g., H<sub>2</sub>) are predicted across fewer organisms. The specific pattern closely matches "),
               tags$a(
                 href = "https://www.science.org/doi/10.1126/sciadv.adg8687",
                 target = "_blank",
                 "observed values"
               ),
               "."
             )
           ),
           
           "Search database" = div(
             h3("Search database"),
             h4("Overview"),
             p("This tool allows the user to search the internal database."),
             
             h4("Use"),
             
             tags$img(
               src = "screenshots/databaseSearch.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
               
               tags$h5("Build query"),
               p("Specify simple or complex queries."),
               
               tags$h5("Perform search"),
               p("After specifying the query, click here to search the database."),
               
               tags$h5("Download results"),
               p("Click here to download a .csv of the results (matching organisms)."),
               
               tags$h5("Treemap|Tree|t-SNE"),
               p("Click these tabs to explore results in interactive plots."),
               tags$i("Treemap"),
               p("This plot shows the relative abundance of phyla of matching organisms (percentage of organisms belonging to a given phylum relative to all phyla).  Phylum is the default variable and can be changed."),
               tags$i("Tree"),
               p("This plot shows a phylogenetic tree of organisms.  Matching organisms are highlighted.  The phylogenetic tree was constructed with n = 14 ribosomal genes from n = 3,822 prokaryotes."),
               tags$i("t-SNE"),
               p("This plot shows a t-SNE plot of gene functions.  Matching organisms are highlighted.  The t-SNE plot was constructed with n = 30,805 gene functions from n = 4,301 prokaryotes. Organisms that cluster together in this plot have similar functions.")
             )
           ),

           "Download database" = div(
             h3("Download database"),
             h4("Overview"),
             p("This tool allows the user to download the internal database. The .csv includes all organisms and all information available."),
             
             h4("Use"),
             
             tags$img(
               src = "screenshots/databaseDownload.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
                      
               tags$h5("Download"),
               p("Click to download the database as a .csv file.  Two versions are available.")
            )
           ),
           
           "History" = div(
             h3("History"),
             h4("Overview"),
             p("This tab allows users to view past computation jobs (searches or predictions)."), 
             p("Jobs are saved for 30 days.  Any job file over 50 Mb may be deleted immediately."),
             p("Only jobs submitted under the user's IP address are shown."),
             
             h4("Use"),
             
             tags$img(
               src = "screenshots/history.svg",
               width = 750
             ),
             
             tags$div(
               class = "circled-number-list",
               
               tags$h5("Past jobs"),
               p("Click on past jobs to see computation results or to select a job for deletion."),
               
               tags$h5("Select all/Delete selected"),
               p("Click to select and delete jobs.  Deletion cannot be undone.")
             
             )
           )
    )
  })
  
  # --- Generate outputs ---
  shiny::observeEvent({tab_selected_trigger()},
  {
    # Output videos for tutorial
    output$video_overview <- shiny::renderUI({
      shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/W1_e6f9_7x4" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
    
    output$video_predictionsTaxonomy <- shiny::renderUI({
      shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Lhlk-4vRmL4" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
    
    output$video_predictionsNetwork <- shiny::renderUI({
      shiny::HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/MOubZwqIW4I" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    })
    
    # Output example data for download
    output$downloadTaxa_1 <- create_download_handler("taxa_e_coli", function() load_taxa_e_coli())
    output$downloadTaxa_2 <- create_download_handler("taxa_rumen", function() load_taxa_rumen())
    output$downloadTaxa_3 <- create_download_handler("taxa_infant", function() load_taxa_infant())
    output$downloadTaxa_4 <- create_download_handler("taxa_winogradsky", function() load_taxa_winogradsky())
    output$downloadTaxa_5 <- create_download_handler("taxa_sea", function() load_taxa_sea())
    output$downloadTaxa_6 <- create_download_handler("taxa_qiime2", function() load_taxa_qiime2())
    output$downloadTaxa_7 <- create_download_handler("taxa_metaphlan", function() load_taxa_metaphlan())
    
    output$downloadFunctions_1 <- create_download_handler("gene_functions_e_coli", function() load_gene_functions_e_coli())
    output$downloadFunctions_2 <- create_download_handler("gene_functions_b_subtilis", function() load_gene_functions_b_subtilis())
    output$downloadFunctions_3 <- create_download_handler("gene_functions_p_aeruginosa", function() load_gene_functions_p_aeruginosa())
    output$downloadFunctions_4 <- create_download_handler("gene_functions_rumen", function() load_gene_functions_rumen())
    output$downloadFunctions_5 <- create_download_handler("gene_functions_winogradsky", function() load_gene_functions_winogradsky())
    output$downloadFunctions_6 <- create_download_handler("gene_functions_sea", function() load_gene_functions_sea())
    output$downloadFunctions_7 <- create_download_handler("gene_functions_humann", function() load_gene_functions_humann())
    output$downloadFunctions_8 <- create_download_handler("gene_functions_ancentral", function() load_gene_functions_ancestral())
    
    output$downloadNames_1 <- create_download_handler("names_infant", function() load_names_infant())
    output$downloadNames_2 <- create_download_handler("names_model", function() load_names_model())
    
    output$downloadReference_1 <- create_download_handler("reference_network_glucose_fermentation", function() load_reference_network_glucose_fermentation())
    output$downloadReference_3 <- create_download_handler("reference_network_methanogenesis", function() load_reference_network_methanogenesis())

    output$downloadModel_1 <- create_download_handler("random_forest_fermentation", function() load_model_fermentation())
    output$downloadModel_2 <- create_download_handler("random_forest_methanogenesis", function() load_model_methanogenesis())
  })
  
  # Open links to demo jobs
  shiny::observeEvent(input$open_demo_taxonomy_rumen, {
    open_demo_job(
      session,
      demo_tab  = "predictionsTaxonomy",
      demo_job  = "rumen"
    )
  })
  
  shiny::observeEvent(input$open_demo_network_rumen, {
    open_demo_job(
      session,
      demo_tab  = "predictionsNetwork",
      demo_job  = "rumen"
    )
  })
  
  shiny::observeEvent(input$open_demo_ML_rumen, {
    open_demo_job(
      session,
      demo_tab  = "predictionsMachineLearning",
      demo_job  = "rumen"
    )
  })
}

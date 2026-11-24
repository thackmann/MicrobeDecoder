# Load Internal Data for Shiny App
# This script defines variables for the app.  These are
# variables used by multiple modules.  
# Author: Timothy Hackmann
# Date: 26 February 25

# Colors for plots
  # From Powerpoint
  red_color=rgb(red=255, green=0, blue=0, maxColorValue = 255)
  gold_color=rgb(red=255, green=192, blue=0, maxColorValue = 255)
  green_color=rgb(red=0, green=176, blue=80, maxColorValue = 255)
  blue_color=rgb(red=0, green=112, blue=192, maxColorValue = 255)
  purple_color=rgb(red=112, green=48, blue=160, maxColorValue = 255)
  grey_color = rgb(red=127, green=127, blue=127, maxColorValue = 255) 
  
  # Others
  na_grey    = rgb(red=221, green=221, blue=221, maxColorValue = 255)
  muted_fill = rgb(red=217, green=224, blue=232, maxColorValue = 255)
  muted_text = rgb(red=75,  green=99,  blue=105, maxColorValue = 255)

# Registry for loading modules
  # Each entry describes how to load one module
  # `ui_fn`     : name of the module's UI constructor (resolved with get() at mount time)
  # `server_fn` : name of the module's server function (resolved with get() at mount time)
  # `needs`     : extra arguments open_module() should pass to the server function
  #               beyond the module id. `selected_tab` and `x` are filled in at
  #               mount time so they have access to `session` and `input` from
  #               the running server scope.
  #' @export
  module_registry <- list(
    home = list(
      ui_fn     = "homeUI",
      server_fn = "homeServer",
      needs     = c("x")
    ),
    predictionsTaxonomy = list(
      ui_fn     = "predictionsTaxonomyUI",
      server_fn = "predictionsTaxonomyServer",
      needs     = c("selected_tab", "on_ready")
    ),
    predictionsNetwork = list(
      ui_fn     = "predictionsNetworkUI",
      server_fn = "predictionsNetworkServer",
      needs     = c("selected_tab", "on_ready")
    ),
    predictionsMachineLearning = list(
      ui_fn     = "predictionsMachineLearningUI",
      server_fn = "predictionsMachineLearningServer",
      needs     = c("selected_tab", "on_ready")
    ),
    history = list(
      ui_fn     = "historyUI",
      server_fn = "historyServer",
      needs     = c("selected_tab", "on_ready")
    ),
    examples = list(
      ui_fn     = "examplesUI",
      server_fn = "examplesServer",
      needs     = character(0)
    ),
    databaseSearch = list(
      ui_fn     = "databaseSearchUI",
      server_fn = "databaseSearchServer",
      needs     = c("x", "selected_tab", "on_ready")
    ),
    databaseDownload = list(
      ui_fn     = "databaseDownloadUI",
      server_fn = "databaseDownloadServer",
      needs     = character(0)
    ),
    help = list(
      ui_fn     = "helpUI",
      server_fn = "helpServer",
      needs     = c("x", "selected_tab")
    ),
    about = list(
      ui_fn     = "aboutUI",
      server_fn = "aboutServer",
      needs     = character(0)
    )
  )

#' Registry for loading data files
  #'
  #' This named character vector maps a key to the file path for each data file
  #' loaded generically by `load_data()`. The database is loaded separately by
  #' `load_database()`, which has its own paths and column types.
  #'
  #' @export
  data_registry <- c(
    gene_functions                = "data/gene_functions/gene_functions_database.rds",
    query_filters                 = "data/query_filters/query_filters.rds",
    placeholder_filters           = "data/query_filters/query_filters_simple.rds",
    main_reference_network        = "data/reference_networks/main.zip",
    layout_tree_daylight          = "data/tree/layout_tree_daylight.rds",
    layout_tree_equal_angle       = "data/tree/layout_tree_equal_angle.rds",
    layout_tree_rectangular       = "data/tree/layout_tree_rectangular.rds",
    plot_branches_all_daylight    = "data/tree/plot_branches_all_daylight.rds",
    plot_branches_all_equal_angle = "data/tree/plot_branches_all_equal_angle.rds",
    plot_branches_all_rectangular = "data/tree/plot_branches_all_rectangular.rds",
    plot_tips_all_daylight        = "data/tree/plot_tips_all_daylight.rds",
    plot_tips_all_equal_angle     = "data/tree/plot_tips_all_equal_angle.rds",
    plot_tips_all_rectangular     = "data/tree/plot_tips_all_rectangular.rds",
    layout_tsne                   = "data/tsne/layout_tsne.rds",
    plot_tsne_all                 = "data/tsne/plot_tsne_all.rds",
    nodes_to_root                 = "data/tree/nodes_to_root.rds",
    config_reference_networks     = "data/config/reference_networks.csv",
    config_network_layout         = "data/config/network_layout.csv",
    config_plots                  = "data/config/plot_var.csv"
  )  
 
# Lists of example files
  # Lists links to example files for download.
  # These are used as example files for the prediction tabs and the Help page.
  # Files are served from data/examples (see addResourcePath in app.R).
  example_file_links <- list(
    taxa = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_e_coli.zip",      download = NA, "E. coli (generic format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_rumen.zip",       download = NA, "Bacterial isolates from the rumen (IMG/M format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_infant.zip",      download = NA, "Metagenomic species from the infant gut (MetaPhlAn format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_winogradsky.zip", download = NA, "ASVs from the Winogradsky columns (DADA2 format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_sea.zip",         download = NA, "MAGs from Black Sea (GTDB format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_qiime2.zip",      download = NA, "Bacteria from QIIME2 tutorial (QIIME2 format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/taxa/taxa_metaphlan.zip",   download = NA, "Bacteria from MetaPhlAn tutorial (MetaPhlAn format)"))
    ),
    gene_functions = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_e_coli.zip",       download = NA, "E. coli (generic format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_b_subtilis.zip",   download = NA, "B. subtilis (eggNOG format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_p_aeruginosa.zip", download = NA, "P. aeruginosa (KAAS format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_rumen.zip",        download = NA, "Bacterial isolates from the rumen (IMG/M format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_winogradsky.zip",  download = NA, "ASVs from the Winogradsky columns (PICRUSt2 format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_sea.zip",          download = NA, "MAGs from Black Sea (generic format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_humann.zip",       download = NA, "Bacteria from HUMAnN tutorial (HUMAnN format)")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/gene_functions/gene_functions_ancestral.zip",    download = NA, "Ancestral microbes (generic format)"))
    ),
    names = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/names/names_infant.zip", download = NA, "Metagenomic species from the infant gut")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/names/names_model.zip",  download = NA, "Model isolates of bacteria and archaea"))
    ),
    reference_network = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/reference_network/reference_network_fermentation_of_glucose.zip", download = NA, "Fermentation of glucose")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/reference_network/reference_network_methanogenesis.zip",          download = NA, "Methanogenesis"))
    ),
    model = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/model/model_fermentation.zip",   download = NA, "Fermentation")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/model/model_methanogenesis.zip", download = NA, "Methanogenesis"))
    ),
    tree = shiny::tags$ol(
      class = "circled-letter-list",
      # shiny::tags$li(shiny::tags$a(href = "examples_data/tree/tree_rumen.zip", download = NA, "Bacterial isolates from the rumen")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/tree/tree_ancestral.zip", download = NA, "Ancestral microbes"))
    ),
    metadata = shiny::tags$ol(
      class = "circled-letter-list",
      shiny::tags$li(shiny::tags$a(href = "examples_data/metadata/metadata_rumen.zip",     download = NA, "Bacterial isolates from the rumen")),
      shiny::tags$li(shiny::tags$a(href = "examples_data/metadata/metadata_ancestral.zip", download = NA, "Ancestral microbes"))
    )
  )  
  
# State of computation workers
  #' Used for tracking whether the shared worker pool has been started
  .worker_state <- new.env(parent = emptyenv())
  .worker_state$started <- FALSE  
     
# Names of variables in database
  taxonomy_var <- c("Phylum", "Class", "Order", "Family", "Genus", 
                    "Species", "Subspecies"
                    # , "Strain" # commenting this out speeds up execution by ~5 s
                    )
  
  database_var <- c("LPSN Page", "Bergey Article", "GTDB ID",
                    "NCBI Taxonomy ID", "GOLD Organism ID", 
                    "GOLD Project ID", "IMG Genome ID", "BacDive ID")
  
  metabolism_var <- c(
    "Type of metabolism (FAPROTAX)", "Type of metabolism (FAPROTAX2)", 
    "Type of metabolism (Fermentation Explorer)", 
    "Metabolites produced (BacDive)", "Metabolites utilized (BacDive)",
    "Metabolites produced (Fermentation Explorer)", 
    "Major metabolites produced (Fermentation Explorer)", 
    "Minor metabolites produced (Fermentation Explorer)", 
    "Metabolites utilized (Fermentation Explorer)",
    "Enzyme activity (BacDive)"
  )
  
  physiology_var <- c(
    "Oxygen tolerance (BacDive)", "Pathogenicity (BacDive)", 
    "Indole test (BacDive)", "Voges Proskauer (BacDive)", 
    "Motility (BacDive)", "Antibiotic resistance (BacDive)", 
    "Antibiotic sensitivity (BacDive)"
  )

  growth_var <- c(
  "Temperature for growth in degrees (BacDive)", "Salt for growth in moles per liter (BacDive)", 
  "pH for growth (BacDive)", "Incubation period in days (BacDive)"
  )
    
  morphology_var <- c(
    "Cell shape (BacDive)", "Cell length in microns (BacDive)", "Cell width in microns (BacDive)",
    "Flagellum arrangement (BacDive)", "Gram stain (BacDive)", "Spore formation (BacDive)"
  )
  
  isolation_var <- c(
    "Isolation category 1 (BacDive)", "Isolation category 2 (BacDive)", "Isolation category 3 (BacDive)"
  )

# Status types for computation jobs
  job_final_statuses <- c(
    "completed",
    "error",
    "cancelled",
    "interrupted"
  )  

# Links to external websites
  url_AnaerobeManual <- shiny::a("Anaerobe Laboratory Manual", href="https://search.worldcat.org/title/anaerobe-laboratory-manual/oclc/2546699", target="_blank")
  url_BacDive <- shiny::a("BacDive", href="https://bacdive.dsmz.de/", target="_blank")
  url_Bergey <- shiny::a("Bergey's Manual of Systematics of Archaea and Bacteria", href="https://onlinelibrary.wiley.com/doi/book/10.1002/9781118960608", target="_blank")
  url_CC <- shiny::a("CC by 4.0 license", href="https://creativecommons.org/licenses/by/4.0/", target="_blank")
  url_DADA2 <- shiny::a("DADA2", href="https://benjjneb.github.io/dada2/", target="_blank")
  url_FAPROTAX <- shiny::a("FAPROTAX", href="http://www.loucalab.com/archive/FAPROTAX", target="_blank")
  url_FAPROTAX_license <- shiny::a("this license", href="http://www.loucalab.com/archive/FAPROTAX/lib/php/index.php?section=License", target="_blank")
  url_FAPROTAX2 <- shiny::a("FAPROTAX2", href="http://www.loucalab.com/archive/FAPROTAX2/", target="_blank")
  url_FermentationExplorer <- shiny::a("Fermentation Explorer", href="https://www.science.org/doi/10.1126/sciadv.adg8687", target="_blank")
  url_GitHub <- shiny::a("GitHub", href="https://github.com/thackmann/microbedecoder", target="_blank")
  url_fairuse <- shiny::a("fair use", href="https://www.copyright.gov/fair-use/", target="_blank")
  url_fbar <- shiny::a("fbar", href="https://cran.r-project.org/web/packages/fbar/index.html", target="_blank")
  url_GOLD <- shiny::a("GOLD", href="https://gold.jgi.doe.gov/", target="_blank")
  url_GTDB <- shiny::a("GTDB", href="https://gtdb.ecogenomic.org/", target="_blank")
  url_IMG <- shiny::a("IMG/M", href="https://img.jgi.doe.gov/m/", target="_blank")
  url_JGI <- shiny::a("this notice", href="https://jgi.doe.gov/disclaimer/", target="_blank")
  url_KAAS <- shiny::a("KAAS", href="https://www.genome.jp/kegg/kaas/", target="_blank")
  url_KEGG <- shiny::a("KEGG pathways", href="https://www.genome.jp/kegg/pathway.html", target="_blank") 
  url_LPSN <- shiny::a("LPSN", href="https://www.bacterio.net/", target="_blank")
  url_MicrobeDecoderPublication <- shiny::a("Microbe Decoder uncovers functional traits of microbes in microbiome datasets", href="https://doi.org/10.1093/nar/gkag515", target="_blank")
  url_MicrobeDecoderPublicationPMID <- shiny::a("PMID: 42163722", href="https://pubmed.ncbi.nlm.nih.gov/42163722/", target="_blank")
  url_MIT <- shiny::a("MIT license", href="https://opensource.org/license/mit", target="_blank")
  url_NCBI <- shiny::a("NCBI", href="https://www.ncbi.nlm.nih.gov/taxonomy", target="_blank")
  url_QIIME2 <- shiny::a("QIIME2", href="https://qiime2.org/", target="_blank")
# Main Shiny App Script
# This script sets up the system locale, loads external R scripts, and defines the user interface (UI)
# and server components for the Shiny app. The app includes modules for 
# database searching, predictions, and user help.  Elements are organized 
# within a Bootstrap-based layout.
# Author: Timothy Hackmann
# Date: 15 April 2025

# === Set system locale ===
  Sys.setlocale("LC_ALL", "C")

# === Set CRAN mirror ===
  options(repos = c(CRAN = "https://cloud.r-project.org"))

# === Initialize session ===
  source("functions/sessionFunctions.R")   
  initialize_session(app_dir = getwd(), verbose = TRUE)
  
# === Define user interface (UI) ===
  ui <- bslib::page_fluid(
    title = "Microbe Decoder",
    
    # --- Set style ---
    # Set Bootstrap version and theme
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    
    # Set theme for query builder
    jqbr::useQueryBuilder(bs_version = "5"),
    
    # Activate JavaScript
    shinyjs::useShinyjs(),
    
    # Load files from /www folder and custom fonts
    tags$head(
      tags$script(src = "custom.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "shortcut icon", href = "favicon.svg"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto+Flex:wdth,wght@100..151,100..1000&display=swap")
    ),
      
    # --- Loading screen ---
    create_loading_screen("app-loading-screen", navbar_height_px = 0),
    
    # --- Main app UI (initially hidden ) ---
    shinyjs::hidden(
      div(id = "app-wrapper",
          # Create navigation bar
          bslib::page_navbar(
            id = "tabs",
            selected = "home",  # placeholder, gets overridden by query
            
            # Home
            bslib::nav_panel(
              value = "home",
              title = tagList(icon("home"), "Home"),
              initialize_module("home")
            ),
            
            # Predict
            bslib::nav_menu(
              title = tagList(icon("desktop"), "Predict"),
              # Predict from taxonomy
              bslib::nav_panel(
                value = "predictionsTaxonomy",
                title = "From taxonomy",
                initialize_module("predictionsTaxonomy")
              ),
              # Predict from metabolic networks
              bslib::nav_panel(
                value = "predictionsNetwork",
                title = "With metabolic networks",
                initialize_module("predictionsNetwork")
              ),
              # Predict with machine learning
              bslib::nav_panel(
                value = "predictionsMachineLearning",
                title = "With machine learning",
                initialize_module("predictionsMachineLearning")
              )
            ),
            
            # Database
            bslib::nav_menu(
              title = tagList(icon("database"), "Database"),
              # Search database
              bslib::nav_panel(
                value = "databaseSearch",
                title = "Search",
                initialize_module("databaseSearch")
              ),
              # Download database
              bslib::nav_panel(
                value = "databaseDownload",
                title = "Download",
                initialize_module("databaseDownload")
              )
            ),
            
            # Interactive examples
            bslib::nav_panel(
              value = "examples",
              title = tagList(icon("lightbulb"), "Examples"),
              initialize_module("examples")
            ),
            
            # Help
            bslib::nav_panel(
              value = "help",
              title = tagList(icon("question-circle"), "Help"),
              initialize_module("help")
            ),
            
            # Prediction history
            bslib::nav_panel(
              value = "history",
              title = tagList(icon("clock-rotate-left"), "History"),
              initialize_module("history")
            ),
            
            # About (right-aligned)
            bslib::nav_spacer(),
            bslib::nav_panel(
              value = "about",
              title = tagList(icon("circle-info"), "About"),
              initialize_module("about")
            )
          )
      )
    )
    
  )

# === Define server ===
server <- function(input, output, session) {
  
  # Uncomment to adjust theming
  # bslib::bs_themer()
  
  # Set maximum file upload size
  options(shiny.maxRequestSize = 250*1024^2)
  
  # Set variables
  session$userData$modal_open <- reactiveVal(FALSE) # For tracking if modals are open
  session$userData$job_id <- reactive(get_query_param(param_name = "job")) # For job id
  session$userData$user_ip <- get_user_ip() # For user IP address
  session$userData$user_id <- get_user_id(session$userData$user_ip) # For user id
  
  # Load modules
  setup_module_loading(
    persistent_tabs = c(
      "home"
    ),
    transient_tabs = c(
      "predictionsTaxonomy",
      "predictionsNetwork",
      "predictionsMachineLearning",
      "databaseSearch",
      "databaseDownload",
      "history",
      "examples",
      "help",
      "about"
    )
  )
  
  # Navigate to correct tab when app loads
  init_navigation()
  
  # Trigger animation for logo text
  trigger_typing()
  
  # Clear old/large computation jobs
  setup_auto_cleanup()
  
  # Catch and stop computation jobs interrupted by user
  catch_interrupted_jobs()
  
}

# Uncomment to enable reactlog
# options(shiny.reactlog = TRUE)

# === Run app ===
shiny::shinyApp(ui = ui, server = server)
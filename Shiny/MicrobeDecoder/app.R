# Main Shiny App Script
# This script sets up the system locale, loads external R scripts, and defines the user interface (UI)
# and server components for the Shiny app. The app includes modules for 
# database searching, predictions, and user help, all organized within a Bootstrap-based layout.
# Author: Timothy Hackmann
# Date: 15 April 2025

# === Set system locale ===
  Sys.setlocale("LC_ALL", "C")

# === Set CRAN mirror ===
  options(repos = c(CRAN = "https://cloud.r-project.org"))

# === Load external R files ===
  # Load file with function for loading additional files
  source("functions/sourceFunctions.R")
  
  # Load all remaining files
  source_r_files(
    subdirs = c("install", "variables", "functions", "modules"),
    exclude = c("old"),
    verbose = TRUE,
    local = FALSE
  )
  
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
      
    # Call additional JavaScript functions
    shiny::tagList(
      inject_ip_js()
    ),
    
    # --- Loading screen ---
    create_loading_screen("app-loading-screen", navbar_height_px = 0),
    
    # --- Main app UI (initially hidden ) ---
    shinyjs::hidden(
      div(id = "app-wrapper",
          # Navigation bar
          bslib::page_navbar(
            id = "tabs",
            selected = "home",  # placeholder, gets overridden by query
            
            # Home
            bslib::nav_panel(
              value = "home",
              title = tagList(icon("home"), "Home"),
              homeUI("home")
            ),
            
            # Predict
            bslib::nav_menu(
              title = tagList(icon("desktop"), "Predict"),
              # Predict from taxonomy
              bslib::nav_panel(
                value = "predictionsTaxonomy",
                title = "From taxonomy",
                predictionsTaxonomyUI("predictionsTaxonomy")
              ),
              # Predict from metabolic networks
              bslib::nav_panel(
                value = "predictionsNetwork",
                title = "With metabolic networks",
                predictionsNetworkUI("predictionsNetwork")
              ),
              # Predict with machine learning
              bslib::nav_panel(
                value = "predictionsMachineLearning",
                title = "With machine learning",
                predictionsMachineLearningUI("predictionsMachineLearning")
              )
            ),
            
            # Prediction history
            bslib::nav_panel(
              value = "history",
              title = tagList(icon("clock-rotate-left"), "History"),
              historyUI("history")
            ),
            
            # Database
            bslib::nav_menu(
              title = tagList(icon("database"), "Database"),
              # Search database
              bslib::nav_panel(
                value = "databaseSearch",
                title = "Search",
                databaseSearchUI("databaseSearch")
              ),
              # Download database
              bslib::nav_panel(
                value = "databaseDownload",
                title = "Download",
                databaseDownloadUI("databaseDownload")
              )
            ),
            
            # Help
            bslib::nav_panel(
              value = "help",
              title = tagList(icon("question-circle"), "Help"),
              helpUI("help")
            ),
            
            # About (right-aligned)
            bslib::nav_spacer(),
            bslib::nav_panel(
              value = "about",
              title = tagList(icon("circle-info"), "About"),
              aboutUI("about")
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
      options(shiny.maxRequestSize=100*1024^2)

      # Set variables
      session$userData$modal_open <- reactiveVal(FALSE) # For tracking if modals are open
      session$userData$job_id <- reactive(get_query_param(param_name = "job")) # For job id
      session$userData$user_ip <- get_user_ip() # For user IP address
      session$userData$user_id <- get_user_id(session$userData$user_ip) # For user id
      
      # Call server modules
      shiny::callModule(homeServer, "home", x=session)
      shiny::callModule(predictionsTaxonomyServer, "predictionsTaxonomy", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(predictionsNetworkServer, "predictionsNetwork", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(predictionsMachineLearningServer, "predictionsMachineLearning", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(historyServer, "history", selected_tab = reactive(input$tabs))
      shiny::callModule(databaseSearchServer, "databaseSearch", x = session, selected_tab = reactive(input$tabs))
      shiny::callModule(databaseDownloadServer, "databaseDownload")
      shiny::callModule(helpServer, "help", selected_tab = reactive(input$tabs))
      shiny::callModule(aboutServer, "about")

      # Update URL based on selected tab
      sync_tabs_with_query()

      # Trigger animation for logo text
      trigger_typing()
      
      # Clear old/large computation jobs
      setup_auto_cleanup()
  }
  
  # Uncomment to enable reactlog
  # options(shiny.reactlog = TRUE)
  
# === Run app ===
  shiny::shinyApp(ui = ui, server = server)
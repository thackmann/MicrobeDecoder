# Define the Home Module in Shiny App
# This script defines the user interface (UI) and server for the home module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === Define user interface (UI) ===
homeUI <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # Outer wrapper that will be flex column
    div(
      class = "page-wrapper",
      
      # Main content (takes up remaining height)
      div(
        class = "page-content",
        
        # Logo
        div(
          class = "home-header",
          tags$img(
            src   = "MicrobeDecoderLogo.svg",
            width = 100,
            class = "home-logo"
          )
        ),
        
        # Logo text
        div(class = "logo-text", "Microbe Decoder"),
        
        # Subtitle
        div(
          id         = ns("animated-subtitle"),
          class      = "logo-subtitle",
          `data-text` = "Uncover what microbes are doing in your system"
        ),
        
        # Buttons
        div(
          class = "home-grid",

          # --- Group 1: Predict ---
          div(class = "home-section-label", "Predict"),
          home_button(
            button_name          = ns('jump_predictionsTaxonomy'),
            icon_background_color = "#6d54a3",
            image_name           = "predictionsTaxonomy",
            title                = "Predict traits from taxonomy",
            subtitle             = "Just provide names of taxa"
          ),
          home_button(
            button_name          = ns('jump_predictionsNetwork'),
            icon_background_color = "#ef4146",
            image_name           = "predictionsNetwork",
            title                = "Predict traits with metabolic networks",
            subtitle             = "Build networks on the fly"
          ),
          home_button(
            button_name          = ns('jump_predictionsMachineLearning'),
            icon_background_color = "#f3a73f",
            image_name           = "predictionsMachineLearning",
            title                = "Predict traits with machine learning",
            subtitle             = "Use random forests"
          ),

          # --- Group 2: Explore database ---
          div(class = "home-section-label", "Explore database"),
            home_button(
              button_name          = ns('jump_databaseSearch'),
              icon_background_color = "#bb65a8",
              image_name           = "databaseSearch",
              title                = "Search database",
              subtitle             = "Find data for thousands of organisms"
            ),
            home_button(
              button_name          = ns('jump_databaseDownload'),
              icon_background_color = "#26b784",
              image_name           = "databaseDownload",
              title                = "Download database",
              subtitle             = "For use in Excel or other programs"
            ),
        
          # --- Group 3: Utilities ---
          div(class = "home-section-label", "Utilities"),
          home_button(
            button_name          = ns('jump_history'),
            icon_background_color = "#808285",
            image_name           = "history",
            title                = "History",
            subtitle             = "View past searches and predictions"
          ),
          home_button(
            button_name          = ns('jump_examples'),
            icon_background_color = "#6d54a3",
            image_name           = "examples",
            title                = "Interactive examples",
            subtitle             = "See predictions for example datasets"
          ),
          home_button(
            button_name          = ns('jump_help'),
            icon_background_color = "#26b784",
            image_name           = "help",
            title                = "Help",
            subtitle             = "Tutorials and documentation"
          ),
          home_button(
            button_name          = ns('jump_about'),
            icon_background_color = "#ef4146",
            image_name           = "about",
            title                = "About",
            subtitle             = "How to cite and source code"
          )
        )
      ),
      
      # Footer
      div(
        class = "app-footer",
        a(
          href = "https://creativecommons.org/licenses/by/4.0/",
          target = "_blank",
          rel = "noopener noreferrer",
          tags$span(
            tags$img(src = "https://mirrors.creativecommons.org/presskit/icons/cc.svg", class = "cc-icon"),
            " CC BY 4.0 license"
          )
        )
        
      )
    )
  )
}


# === Define server ===
  homeServer <- function(id, x) {
    shiny::moduleServer(id, function(input, output, session) {
    #Set namespace
    ns <- session$ns

    
    #Navigate to tab selected by navigation button
    observeEvent(input$jump_databaseSearch, {
      shinyjs::runjs("shinyjs.goToTab('databaseSearch');")
    })
    observeEvent(input$jump_databaseDownload, {
      shinyjs::runjs("shinyjs.goToTab('databaseDownload');")
    })
    observeEvent(input$jump_predictionsTaxonomy, {
      shinyjs::runjs("shinyjs.goToTab('predictionsTaxonomy');")
    })
    observeEvent(input$jump_predictionsNetwork, {
      shinyjs::runjs("shinyjs.goToTab('predictionsNetwork');")
    })
    observeEvent(input$jump_predictionsMachineLearning, {
      shinyjs::runjs("shinyjs.goToTab('predictionsMachineLearning');")
    })
    observeEvent(input$jump_history, {
      shinyjs::runjs("shinyjs.goToTab('history');")
    })
    observeEvent(input$jump_examples, {
      shinyjs::runjs("shinyjs.goToTab('examples');")
    })
    observeEvent(input$jump_help, {
      shinyjs::runjs("shinyjs.goToTab('help');")
    })
    observeEvent(input$jump_about, {
      shinyjs::runjs("shinyjs.goToTab('about');")
    })
      })
  }

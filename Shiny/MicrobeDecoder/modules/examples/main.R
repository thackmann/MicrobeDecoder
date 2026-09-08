# Define the Examples Module in Shiny App
# This script defines the user interface (UI) and server for the examples module
# Author: Timothy Hackmann
# Date: 2 Aug 2026

# === Define user interface (UI) ===
examplesUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Title
    create_title_div("Examples"),

    bslib::layout_sidebar(

      # Sidebar
      sidebar = bslib::navset_pill(
                            id = ns("subtabs"),
                            bslib::nav_panel(
                              title = "Ancestral microbes",
                              value = "Ancestral microbes",
                              id = "Ancestral microbes"
                            ),
                            bslib::nav_panel(
                                      title = "Bacterial isolates from the rumen",
                                      value = "Bacterial isolates from the rumen",
                                      id = "Bacterial isolates from the rumen"
                            )
                          ),
      # Main content area
      div(
          shiny::uiOutput(ns("main_content"))
        )
    )
  )
}

# === Define server ===
examplesServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # --- Render user interface (UI) ---
    # Links open a saved job in a new browser tab
    output$main_content <- shiny::renderUI({
      switch(input$subtabs,
             
             "Ancestral microbes" = div(
               h3("Ancestral microbes"),
               p(
                 HTML("Results are for n = 700 modern bacteria and archaea and their ancestors.  More details on this analysis will be made available in a publication.")
               ),
               
               tags$h5("Predict traits with metabolic networks"),
               tags$ol(class = "circled-letter-list",
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=fermentation_of_glucose", target = "_blank", rel = "noopener", "Fermentation of glucose")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=aerobic_respiration", target = "_blank", rel = "noopener", "Aerobic respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=nitrate_respiration", target = "_blank", rel = "noopener", "Nitrate respiration (generic) (dissimilatory nitrate reduction)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=sulfur_compound_respiration", target = "_blank", rel = "noopener", "Sulfur compound respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=chlorate_respiration", target = "_blank", rel = "noopener", "Chlorate respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=selenate_respiration", target = "_blank", rel = "noopener", "Selenate respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=TMAO_respiration", target = "_blank", rel = "noopener", "Trimethylamine N-oxide (TMAO) respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=organohalide_respiration", target = "_blank", rel = "noopener", "Organohalide respiration (generic)")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=methanogenesis", target = "_blank", rel = "noopener", "Methanogenesis")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=acetogenesis", target = "_blank", rel = "noopener", "Acetogenesis")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=nitrogen_fixation", target = "_blank", rel = "noopener", "Nitrogen fixation")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=phototrophy_with_sulfur_compounds", target = "_blank", rel = "noopener", "Phototrophy with sulfur compounds")),
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/ancestral&job=oxygenic_photosynthesis", target = "_blank", rel = "noopener", "Oxygenic photosynthesis"))
               ),
               
               tags$h5("Predict traits with machine learning"),
               tags$ol(class = "circled-letter-list",
                       tags$li(tags$a(href = "?tab=predictionsMachineLearning&user=examples/ancestral&job=all", target = "_blank", rel = "noopener", "All traits"))
               )
             ),
             
             "Bacterial isolates from the rumen" = div(
               h3("Bacterial isolates from the rumen"),
               p(
                 HTML("Results are for previously uncharacterized bacteria from the rumen.  Predictions closely match "),
                 tags$a(
                   href = "https://www.science.org/doi/10.1126/sciadv.adg8687",
                   target = "_blank",
                   "observed values"
                 ),
                 "."
               ),
                 
               tags$h5("Predict traits from taxonomy"),
               tags$ol(class = "circled-letter-list",
                       tags$li(tags$a(href = "?tab=predictionsTaxonomy&user=examples/rumen&job=all", target = "_blank", rel = "noopener", "All traits"))
               ),
               
               tags$h5("Predict traits with metabolic networks"),
               tags$ol(class = "circled-letter-list",
                       tags$li(tags$a(href = "?tab=predictionsNetwork&user=examples/rumen&job=fermentation", target = "_blank", rel = "noopener", "Fermentation"))
               ),
               
               tags$h5("Predict traits with machine learning"),
               tags$ol(class = "circled-letter-list",
                       tags$li(tags$a(href = "?tab=predictionsMachineLearning&user=examples/rumen&job=all", target = "_blank", rel = "noopener", "All traits"))
               )
             )
      )
    })
  })
}

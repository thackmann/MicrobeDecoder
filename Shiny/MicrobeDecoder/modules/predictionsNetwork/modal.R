# This script defines the user interface (UI) and server for the build networks modal
# Author: Timothy Hackmann
# Date: 26 June 2025

modalUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Set title
    create_title_div("Build custom network"),
    
    # Set parameters
    div(
      style = "display: flex; gap: 0.5em; margin-top: 0em;",
      create_selectize_input(
        inputId = ns("network_configuration"), 
        label = "Component", 
        width = "100%",
        options = list(
          `actions-box` = TRUE, 
          `live-search` = TRUE,
          plugins = list("remove_button")
        )
      )
    ),
    
    # Advanced inputs
    shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
    shiny::conditionalPanel(
      condition = "input.show_advanced",
      ns = ns,
      create_switch_input(ns("hide_cofactors"), "Hide cofactors", label_position = "above")
    ),
    
    # Main content area
    create_plot_panel(ns, "network", "Network", use_spinner = TRUE, full_screen = FALSE),
    
    div(
      style = "display: flex; gap: 0.5em; margin-top: 0em;",
      actionButton(ns("use_network"), "Use network", class = "btn btn-primary"),
      actionButton(ns("cancel_network"), "Cancel", class = "btn btn-danger")
    )
  )
}

modalServer <- function(input, output, session) {
  ns <- session$ns
  
  # --- Update user interface (UI) elements ---
  # Update choices and selected variable for modules
  shiny::observe({
    main_network <- load_main_reference_network()
    
    vars_to_label <- c("nt" = "network", "md" = "module", "rn" = "reaction", "ko" = "KO", "eq" = "equation")
    choices <- create_labeled_choices(main_network, vars_to_label)
    
    update_select_input(
      inputId = "network_configuration",
      choices = choices,
      selected = session$userData$builder_selected()
    )
    
  }, label = "update_network_configuration")
  
  # --- Process results ---
  configure_network <- shiny::eventReactive(input$network_configuration, {
    main_network <- load_main_reference_network()
    
    configured_network <- main_network %>% 
      filter_network(input$network_configuration)
  }, 
  label = "configure_network") %>% debounce(500)
  
  # --- Generate outputs ---
  # Output plot for network
  output$network_plot <- plotly::renderPlotly({
    configured_network <- configure_network()
    
    to_remove <- get_metabolites_to_remove(
      hide_cofactors = input$hide_cofactors,
      enzyme_cofactors = enzyme_cofactors
    )
    
    g <- make_network_graph(configured_network, add_flux = FALSE, to_remove = to_remove, add_underscore = TRUE)
    layout <- set_network_layout(g, type = "FR", dimensions = 2)
    g <- format_network_graph(g, show_flux = FALSE, show_modules = TRUE, vertex_default_size = 12)
    
    plot_network(
      g,
      layout,
      network_legend_key = get_network_legend_key(2),
      spread = 0.05,
      showlabels = FALSE
    )
  })
  
  # Output network and configuration to app
  observeEvent(input$use_network, {
    # Store configuration
    session$userData$builder_selected(input$network_configuration)
    
    # Store network
    session$userData$network_data(configure_network())
    
    # Remove modal
    shiny::removeModal()
  })
  
  # Close modal without outputting network
  observeEvent(input$cancel_network, {
    shiny::removeModal()
  })
}
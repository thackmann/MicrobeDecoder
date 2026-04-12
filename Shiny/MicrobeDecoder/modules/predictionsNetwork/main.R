# This script defines the user interface (UI) and server for the predictions with metabolic networks module.
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === Define user interface (UI) ===
  predictionsNetworkUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      #Call JavaScript functions
      inject_js_resize(ns, "treemap-container"),

      # --- Loading screen ---
      create_loading_screen("network-loading-screen"), 
      
      # --- Main UI (initially hidden ) ---
      #Title
      shinyjs::hidden(
        div(id = "network-wrapper",
        
        # Title
        create_title_div("Predict traits with metabolic networks"),
      
        # Content
        bslib::layout_sidebar(
          # Sidebar
          sidebar = bslib::sidebar(
                id = ns("sidebar"), 
                width = "30%",
                
                # Select data
                div("Organisms (gene functions)", class = "tight-heading"),
                bslib::navset_tab(id = ns("function_tabs"),
                                  bslib::nav_panel(title = "Database",
                                                   div(
                                                     create_selectize_input(inputId = ns("gene_functions_database")),
                                                     div(
                                                       style = "display: flex; gap: 10px; align-items: baseline; margin-top: -8px;",
                                                       span(
                                                         style = "margin-top: -8px; padding: 0;",
                                                         shiny::actionLink(ns("update_gene_function_choices"), label = "Load examples")
                                                       ),
                                                       span(
                                                         style = "margin-top: -8px; padding: 0;",
                                                         fileInput_link(ns("upload_names"), label = "Choose with file")
                                                       )
                                                     )
                                                   )
                                  ),
                                  bslib::nav_panel(title = "File upload",
                                                   fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
                                  )
                ),
                div("Type of metabolism (reference network)", class = "tight-heading"),
                bslib::navset_tab(id = ns("reaction_tabs"),
                                  bslib::nav_panel(title = "Database",
                                                   create_selectize_input(inputId = ns("reference_network_database"), multiple = FALSE),
                                   ),
                                  bslib::nav_panel(title = "Build",
                                                   div(
                                                     create_network_input(ns("network_input")),
                                                     uiOutput(ns("download_network_ui")),
                                                     style = "margin-top: -6px;"
                                                   )
                                  ),
                                  bslib::nav_panel(title = "File upload",
                                                   fileInput_modal(ns("reference_network_upload"), modalId = ns("reference_network_modal"))
                                   )
                  ),
                
                # Set parameters
                create_selectize_input(inputId = ns("substrates"), label = "Substrates"), 
                create_selectize_input(inputId = ns("products"), label = "End products"),
                 
                # Advanced inputs
                shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
                shiny::conditionalPanel(
                  condition = "input.show_advanced",
                  ns = ns,
                  create_selectize_input(inputId = ns("unbalanced_intermediates"), label = "Unbalanced intermediates"),
                  shiny::sliderInput(ns("threshold"), "Flux threshold", min = 0, max = 1000, value = 1),
                  create_switch_input(inputId = ns("all_subunits"), label = "Enzymes must have all subunits")
                ),
                
                # Make predictions
                shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
            ),
          
          # Main content area
          div(
             id = ns("results_page"),
             
             # Message for missing selections
             shiny::conditionalPanel(
               condition = "!output.flag_results",
               ns = ns,
               shiny::h4("Please make selections at left")
             ),
             
             # Results panel
             shiny::conditionalPanel(
               condition = "output.flag_results",
               ns = ns,
  
               # Summary and download button
                 bslib::card(
                   bslib::card_header(shiny::textOutput(ns("summary_text"))),
                   create_download_button(ns('download_data'))
                 ),
                 
                  # Tabs for plots
                  bslib::navset_card_underline(
                      id = ns("results_tabs"),
                      
                      # Title
                      bslib::nav_item(
                        tags$span("Prediction results", class = "nav-title")
                      ),
                      bslib::nav_spacer(),
                      
                      # Plot options
                      div(
                        class = "flex-container plot-options-container",
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network' && output.flag_multiple_organisms",
                                                     create_picker_input(ns("organism_to_display"), "Organism")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "output.flag_multiple_substrates",
                                                     create_picker_input(ns("substrate_to_display"), "Substrate")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network' && output.flag_multiple_products",
                                                     create_picker_input(ns("product_to_display"), "End product")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network'",
                                                     create_picker_input(ns("set_network_layout"), "Layout")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network'",
                                                     create_picker_input(ns("set_network_dimensions"), "Dimensions", choices = c("2", "3"), selected = "2")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network'",
                                                     create_switch_input(ns("hide_cofactors"), "Hide cofactors", label_position = "above")
                        ),
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network'",
                                                     create_switch_input(ns("hide_unbalanced_intermediates"), "Hide unbalanced", label_position = "above")
                        )
                      ),
                      div(
                        class = "flex-container plot-options-container",
                        create_conditional_flex_item(ns, 
                                                     "input.results_tabs == 'Metabolic network'",
                                                     create_download_button(ns("download_network_model"), "Download network model")
                        )
                      ),
                      
                      # Plot panels
                      create_plot_panel(ns, "heatmap", "Heatmap"),
                      create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                      create_plot_panel(ns, "network", "Metabolic network", use_spinner = TRUE)
                      # create_plot_panel(ns, "summary", "Summary")
                  )
              )
            )
         )
        )
      )
    )
  }
  
  # === Define server ===
  predictionsNetworkServer <- function(input, output, session, x, selected_tab) {
    # --- Set namespace ----
    ns <- session$ns
    
    # --- Set variables ----
    session$userData$builder_selected <- reactiveVal("Glycolysis") # For storing the selected values in the builder
    session$userData$network_data <- reactiveVal(NULL) # For storing the user-built network
    network_filename <- reactiveVal(NULL) # For storing network's file name
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsNetwork")
    
    tab_loaded_trigger <- make_tab_trigger(
      selected_tab, "predictionsNetwork", input, "gene_functions_database"
    )
    
    make_predictions_trigger <- make_action_button_trigger("make_predictions")
    
    updated_reference_network_trigger <- make_other_trigger(
      input$reference_network_database,
      session$userData$network_data(),
      input$reference_network_upload
    )
    
    url_change_trigger <- make_url_trigger(tab_name = "predictionsNetwork")

    update_metabolite_choices_trigger <- or_trigger(
      tab_selected_trigger, 
      updated_reference_network_trigger
    )
    
    get_graph_trigger <- make_other_trigger(
      url_change_trigger(),
      input$substrate_to_display, 
      input$product_to_display,
      input$set_network_layout, 
      input$set_network_dimensions,
      input$organism_to_display,
      input$hide_cofactors,
      input$hide_unbalanced_intermediates
    )

    # --- Get user input (events) ---
    get_inputs <- shiny::eventReactive({ make_predictions_trigger() }, {
      # Set flags
      functions_from_database <- isTRUE(input$function_tabs == "Database")
      functions_from_upload <- isTRUE(input$function_tabs == "File upload")
      network_from_database <- isTRUE(input$reaction_tabs == "Database")
      network_from_builder <- isTRUE(input$reaction_tabs == "Build")
      network_from_upload <- isTRUE(input$reaction_tabs == "File upload")

      # Launch modal
      display_modal(ns = ns, message = "Getting inputs")
      
      # Compile inputs
      get_network_inputs(
        functions_from_database = functions_from_database,
        functions_from_upload = functions_from_upload,
        network_from_database = network_from_database,
        network_from_builder = network_from_builder,
        network_from_upload = network_from_upload,
        selected_organisms = input$gene_functions_database,
        gene_functions_upload_path = input$gene_functions_upload$datapath,
        reference_network_upload_path = input$reference_network_upload$datapath,
        selected_reference_network = input$reference_network_database,
        substrates = input$substrates,
        products = input$products,
        unbalanced_intermediates = input$unbalanced_intermediates,
        all_subunits = input$all_subunits
      )
    }, label = "get_inputs")
    
    # --- Process input ---
    # Create job for computation
    create_job <- shiny::eventReactive(make_predictions_trigger(), {
      # Create job ID
      job_id <- create_job_id()
      
      # Update URL with the  ID
      url <- create_job_url(job_id = job_id, tab = "predictionsNetwork")
      shiny::updateQueryString(sub(".*\\?", "?", url), mode = "push")
      
      # Update progress   
      display_modal(ns = ns, message = "Creating job for computation", value = 0, url = url)
      cat("Job created:", job_id)
      
      return(job_id)
      
    }, label = "create_job")
    
    # --- Perform computations ---
    compute_job <- shiny::eventReactive({make_predictions_trigger()},
    {
      results <- compute_network_predictions(
        reference_network = get_inputs()$reference_network,
        gene_functions = get_inputs()$gene_functions,
        substrates = get_inputs()$substrates,
        products = get_inputs()$products,
        unbalanced_intermediates = get_inputs()$unbalanced_intermediates,
        all_subunits = get_inputs()$all_subunits,
        ns = ns
      )

      return(results)
      
    }, label = "compute_job")

    # --- Save and get results ---
    # Save results
    shiny::observeEvent({make_predictions_trigger()},
    {
      job_id <- create_job()
      job_dir <- get_job_dir(tab = "predictionsNetwork")
      
      results <-
        list(
          get_organism_names = get_inputs()$organism_names,
          get_input_substrates = get_inputs()$substrates,
          get_input_products = get_inputs()$products,
          get_unbalanced_intermediates = get_inputs()$unbalanced_intermediates,
          get_solved_models = compute_job()$solved_models,
          predict_fluxes = compute_job()$fluxes
        )
      
      # Update progress
      display_modal(ns = ns, message = "Saving results", value = 100)
      
      # Save result
      save_job_result(job_id = job_id, result = results, job_dir = job_dir)
      
      # Update progress
      hide_modal_with_progress()
    },
    label="save_results")
    
    # Get results
    get_results <- eventReactive({ url_change_trigger() }, {
      job_id <- get_query_param()
      user_id <- get_query_param(param_name = "user")
      job_dir <- get_job_dir(tab = "predictionsNetwork", user_id = user_id)
      
      load_job_result(job_id, job_dir)
    })
    
    # --- Process results ---
    # Make network graph
    get_network_graph <- shiny::eventReactive({get_graph_trigger()},
    {
      # Get inputs
      s <- get_results()$get_solved_models
      organism <- input$organism_to_display
      substrate <- input$substrate_to_display
      product <- input$product_to_display
      unbalanced_intermediates <- get_results()$get_unbalanced_intermediates
      threshold <- input$threshold
      hide_cofactors <- input$hide_cofactors
      hide_unbalanced_intermediates <- input$hide_unbalanced_intermediates
      
      # Get model
      s = s[[organism]][[substrate]][[product]]
      
      # Change fluxes to 0 if product has flux less than threshold
      if (s$flux[which(s$name == "Ending_metabolite")] < threshold) {
        s$flux = 0
      }
      
      # Set metabolites to remove
      to_remove <- get_metabolites_to_remove(
        hide_unbalanced_intermediates = input$hide_unbalanced_intermediates,
        hide_cofactors = hide_cofactors,
        unbalanced_intermediates = unbalanced_intermediates,
        enzyme_cofactors = enzyme_cofactors,
        exceptions = c(product, substrate)
      )
      
      # Make graph
      g = make_network_graph(s = s, to_remove = to_remove)
      
      return(g)
    }, 
    label="get_network_graph")
  
    # Set layout for graph
    get_network_layout <- shiny::eventReactive(get_graph_trigger(),
    {
      g <- get_network_graph()

      layout <- set_network_layout(graph = g, type = input$set_network_layout, dimensions = input$set_network_dimensions)
      
      return(layout)
    },
    label="get_network_layout")

    # --- Update user interface (UI) elements ---
    # Update choices for gene functions (organisms)
    shiny::observeEvent({list(tab_selected_trigger(), input$update_gene_function_choices)}, {
      # Load data
      database <- load_database()
      
      # Get choices
      choices <- get_organism_choices(database = database)
      selected <- get_default_organism_selections(input$reference_network_database)
      selected <- assign_if_invalid(selected, c("Escherichia coli"))
      
      update_select_input(inputId = "gene_functions_database", choices = choices, selected = selected)
    }, 
    label = "update_gene_function_choices_init", ignoreInit = TRUE)
    
    shiny::observeEvent(input$upload_names, {
      req(input$upload_names)
      
      # Load data
      database <- load_database()
      
      # Get choices
      database <- load_database()
      choices <- get_organism_choices(database = database)
      selected <- get_uploaded_organism_selections(input$upload_names$datapath, choices)
      selected <- assign_if_invalid(selected, c("Escherichia coli"))
      
      # Update UI
      update_select_input(inputId = "gene_functions_database", choices = choices, selected = selected)
    }, 
    label = "update_gene_function_choices_from_upload", ignoreInit = TRUE)
    
    # Hide loading screen
    shiny::observeEvent({tab_loaded_trigger()},
    {
      shinyjs::runjs("shinyjs.hide('network-loading-screen'); shinyjs.show('network-wrapper');")
    },
    once = TRUE, label = "hide_loading_screen")
    
    # Update choices for reference network
    observeEvent(tab_selected_trigger(), {
      choices <- get_choices_reference_network_in_database()
      
      update_select_input(inputId = "reference_network_database", choices = choices)
    }, label = "update_reference_network_choices")
    
    # Update choices for metabolites (substrates, products, and unbalanced intermediates)
    observeEvent({update_metabolite_choices_trigger()}, {
      # Get choices for metabolites
      selected_network <- if (input$reaction_tabs == "Database") input$reference_network_database else "Other"
      req(selected_network)

      choices <- get_metabolite_choices(
          network_from_database = isTRUE(input$reaction_tabs == "Database"),
          network_from_builder = isTRUE(input$reaction_tabs == "Build"),
          network_from_upload = isTRUE(input$reaction_tabs == "File upload"),
          selected_network = selected_network,
          upload_path = input$reference_network_upload$datapath
      ) 

      # Get choices for substrates
      selected <- get_metabolite_selections(selected_network = selected_network, 
                                            metabolite_col = "default_substrates")
      update_select_input(inputId = "substrates", choices = choices, selected = selected)
      
      # Get choices for products
      selected <- get_metabolite_selections(selected_network = selected_network, 
                                            metabolite_col = "default_products")
      update_select_input(inputId = "products", choices = choices, selected = selected)
      
      # Get choices for unbalanced intermediates
      selected <- get_metabolite_selections(selected_network = selected_network, 
                                            metabolite_col = "default_unbalanced_intermediates")
      selected <- selected[selected %in% choices]

      update_select_input(inputId = "unbalanced_intermediates", choices = choices, selected = selected)
    }, label = "update_metabolite_choices")
  
    # Update choices for substrates, products, and organisms to display
    shiny::observeEvent({list(url_change_trigger(), input$threshold)}, {
      results <- get_results()
      df      <- results$predict_fluxes
      
      all_substrates <- results$get_input_substrates
      unpredicted    <- get_unpredicted_choices(df, choices_col = "Substrate", 
                                                value_col = "Flux", threshold = input$threshold)
      fmt <- format_picker_choices(all_substrates, unpredicted, label = "substrate not predicted")
      
      update_picker_input(inputId = "substrate_to_display", choices = fmt$choices, 
                          choicesOpt = fmt$choicesOpt)
      update_picker_input(inputId = "product_to_display",   choices = results$get_input_products)
      update_picker_input(inputId = "organism_to_display",  choices = results$get_organism_names)
    }, label = "update_display_inputs")
    
    # Update choices for network layout
    shiny::observeEvent({input$set_network_dimensions},
    {
      choices <- switch(input$set_network_dimensions,
                        "3" = c("FR", "KK", "DRL", "MDS"),
                        "2" = c("FR", "KK", "DH", "GEM", "DRL", "MDS", "Graphopt"),
                        NULL)
      update_picker_input(inputId = "set_network_layout", choices = choices)
    },
    label="update_layout_display")

    # Update text for file input
    observeEvent(session$userData$network_data(), {
      reference_network <- session$userData$network_data()
      
      # Generate filename based on current time
      file_name <- paste0("network_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
      network_filename(file_name)
      
      # Create display text for the input box
      display <- if (nrow(reference_network) == 0) "Custom network" else file_name
      update_text_input(inputId = "network_input-network_name_display", value = display)
    })
    
    # Toggle sidebar closed (when loading saved job)
    shiny::observeEvent(tab_selected_trigger(), 
    {
      if (isTRUE(session$userData$loaded_job_on_init)) {
        # Toggle side bar closed
        bslib::sidebar_toggle("sidebar")
        
        # Reset restore so it does not toggle side bar closed again
        session$userData$loaded_job_on_init <- FALSE
      }
    })
    
  # --- Generate outputs ---
  # Output modals with example data
    output_download_modal(
      input_id = "gene_functions_modal",
      object_ids = c(
        "gene_functions_e_coli",
        "gene_functions_b_subtilis",
        "gene_functions_p_aeruginosa",
        "gene_functions_rumen",
        "gene_functions_winogradsky",
        "gene_functions_sea",
        "gene_functions_humann",
        "gene_functions_ancestral"
      ),
      labels = c(
        "E. coli (generic format)",
        "B. subtilis (eggNOG format)",
        "P. aeruginosa (KAAS format)",
        "Bacterial isolates from the rumen (IMG/M format)",
        "ASVs from the Winogradsky columns (PICRUSt2 format)",
        "MAGs from Black Sea (generic format)",
        "Bacteria from HUMAnN tutorial (HUMAnN format)",
        "Ancestral bacteria (generic format)"
      ),
    ns = ns,
    label = "show_gene_functions_modal"
  )

  output_download_modal(
    input_id = "reference_network_modal",
    object_ids = c(
      "reference_network_glucose_fermentation",
      "reference_network_methanogenesis"
    ),
    labels = c(
      "Glucose fermentation",
      "Methanogenesis"
    ),
    ns = ns,
    label = "show_reference_network_modal"
  )

  # Create observer to direct user to Help
  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits with metabolic networks")
  
  # Create output flags
  flag_if_multiple(output, "flag_multiple_organisms", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_organism_names)
  flag_if_multiple(output, "flag_multiple_substrates", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_input_substrates)
  flag_if_multiple(output, "flag_multiple_products", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_input_products)
  flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
                   value_fun = function() get_results()) 
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    df <- get_results()$predict_fluxes
    threshold <- input$threshold
    
    req(df)
    counts <- count_predictions(df, 
                organism_col = "Organism number", 
                trait_col = "End product", 
                value_col = "Flux", 
                threshold = threshold)
    
    format_summary_text(
      count1 = counts$traits_predictions, 
      count2 = counts$organisms_predictions, 
      label1 = "end products", 
      label2 = "organisms", 
      total2 = counts$organisms_total
    )
  })
  
  # Output downloadable csv of fluxes
  output$download_data <- create_download_handler(
    filename_prefix = "fluxes",
    data_source = function() get_results()$predict_fluxes
  )
  
  # Output overview plots
  shiny::observeEvent({list(get_results()$predict_fluxes, input$substrate_to_display, input$threshold)},
  {
    df = get_results()$predict_fluxes
    substrate_to_display = input$substrate_to_display
    threshold <- input$threshold
    
    req(df)
    df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
    
    #Summary plot
    output$summary_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="summary",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
      plot = plot_summary(df, 
                          coord_fixed = TRUE, 
                          hovertemplate = "<b>Endproduct: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% organisms positive")
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="treemap",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      hovertemplate <- "<b>Endproduct: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>"
      plot = plot_treemap(df,
                          hovertemplate = hovertemplate)
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="heatmap",
                            x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                            var_col = "Substrate", var_to_keep = substrate_to_display, 
                            z_threshold = threshold, drop_extra_y = FALSE, z_percentage = FALSE) 
      
      coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
      borders <- get_heatmap_border(df, ns, "heatmap_plot")
      hovertemplate <- "<b>Endproduct: %{x}</b><br><b>Organism: %{y}</b><br><b>Flux: %{z:.0f}</b><br><extra></extra>"
      plot = plot_heatmap(df, 
                          hovertemplate = hovertemplate,
                          legend_labels = c("0", "250", "500", "750", "1000"), 
                          legend_title = "Flux", 
                          zmax = 1000,
                          coord_fixed = coord_fixed,
                          horizontal_border = borders$horizontal_border,
                          vertical_border = borders$vertical_border
                          )
    })
  })
  
  # Output network graph
  output$network_plot <- plotly::renderPlotly(exp = {
    g <- get_network_graph()
    layout <- get_network_layout()
    network_dimensions <- input$set_network_dimensions
    
    # Get config
    config <- get_network_plot_config(network_dimensions)
    network_legend_key <- get_network_legend_key(network_dimensions)
    spread <- as.numeric(config$spread)
    vertex_default_size <- as.numeric(config$vertex_default_size)
    vertex_highlight_size <- as.numeric(config$vertex_highlight_size)
    
    # Highlighted metabolites
    vertices_to_highlight <- c(format_metabolite_name(input$substrate_to_display),
                               format_metabolite_name(input$product_to_display))
    
    # Format graph
    g <- format_network_graph(
      graph = g,
      show_flux = TRUE,
      show_modules = TRUE,
      vertices_to_highlight = vertices_to_highlight,
      vertex_default_size = vertex_default_size,
      vertex_highlight_size = vertex_highlight_size
    )
    
    # Build plot
    plot <- plot_network(
      graph = g,
      layout = layout,
      network_legend_key = network_legend_key,
      spread = spread,
      showlabels = FALSE
    )
    
    return(plot)
  })
  
  # Output downloadable csv of results
  output$download_network_model <- create_download_handler(
    filename_prefix = "model",
    data_source = function() {
      # Get inputs
      s <- get_results()$get_solved_models
      organism <- input$organism_to_display
      substrate <- input$substrate_to_display
      product <- input$product_to_display
      
      #Get network model
      s = s[[organism]][[substrate]][[product]]
    }
  )
  
  # Output modal for building networks
  observeEvent(input$`network_input-open_modal`, {
    # Open modal
    showModal(modalDialog(
      size = "xl", easyClose = FALSE, footer = NULL,
      modalUI(ns("modal"))
    ))
    
    # Call the server module for the modal
    shiny::callModule(modalServer, "modal")
  })
  
  # Output downloadable csv of reference network
  output$download_network <- create_download_handler(
    filename_prefix = reactive({ req(network_filename()) |> tools::file_path_sans_ext() }),
    data_source = reactive({ req(session$userData$network_data()) }),
    file_type = "csv"
  )
  
  # Output download link
  output$download_network_ui <- shiny::renderUI({
    df <- session$userData$network_data()
    if (is.null(df)) return(NULL)
    downloadLink(ns("download_network"), "Download network", class = "action-link")
  })
}
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
                                                     shiny::actionLink(ns("update_gene_function_choices"), 
                                                                       label = "Load examples",
                                                                       style = "margin-top: -6px; display: block;")
                                                   )
                                  ),
                                  bslib::nav_panel(title = "File upload",
                                                   fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
                                  )
                ),
                div("Type of metabolism (reference reactions)", class = "tight-heading"),
                bslib::navset_tab(id = ns("reaction_tabs"),
                                  bslib::nav_panel(title = "Database",
                                                   create_selectize_input(inputId = ns("reference_reactions_database"), multiple = FALSE),
                                   ),
                                  bslib::nav_panel(title = "File upload",
                                                   fileInput_modal(ns("reference_reactions_upload"), modalId = ns("reference_reactions_modal"))
                                   )
                  ),
                
                # Set parameters
                shiny::conditionalPanel(
                  condition = "input.reaction_tabs == 'Database'",
                  ns = ns, 
                  create_selectize_input(inputId = ns("substrates_database"), label = "Substrates"), 
                  create_selectize_input(inputId = ns("products_database"), label = "End products"),
                ),
                shiny::conditionalPanel(
                  condition = "input.reaction_tabs == 'File upload'",
                  ns = ns, 
                  create_selectize_input(inputId = ns("substrates_upload"), label = "Substrates"), 
                  create_selectize_input(inputId = ns("products_upload"), label = "End products"),
                ),
                
                # Advanced inputs
                shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
                shiny::conditionalPanel(
                  condition = "input.show_advanced & input.reaction_tabs == 'Database'",
                  ns = ns,
                  create_selectize_input(inputId = ns("unbalanced_intermediates_database"), label = "Unbalanced intermediates"),
                ),
                shiny::conditionalPanel(
                  condition = "input.show_advanced & input.reaction_tabs == 'File upload'",
                  ns = ns,
                  create_selectize_input(inputId = ns("unbalanced_intermediates_upload"), label = "Unbalanced intermediates"),
                ),
                shiny::conditionalPanel(
                  condition = "input.show_advanced",
                  ns = ns,
                  shiny::sliderInput(ns("threshold"), "Flux threshold", min = 0, max = 1000, value = 1),
                  create_switch_input(inputId = ns("all_subunits"), label = "Enzymes must have all subunits")
                ),
                
                # Make predictions
                shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary"),
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
                    title = "Prediction results",
                    
                  # Plot panels
                    create_plot_panel(ns, "summary", "Summary"),
                    create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                    create_plot_panel(ns, "heatmap", "Heatmap"),
                    create_plot_panel(ns, "network", "Metabolic network", use_spinner = TRUE),
                  
                  # Plot options
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_organisms",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("organism_to_display"), label = "Organism")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "output.flag_multiple_substrates",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("substrate_to_display"), label = "Substrate")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network' && output.flag_multiple_products",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("product_to_display"), label = "End product")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_layout"), label = "Layout")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_picker_input(inputId = ns("set_network_dimensions"), label = "Dimensions", choices = c("2", "3"), selected = "2")
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      div(
                        class = "flex-item",
                        create_switch_input(inputId = ns("hide_cofactors"), label = "Hide cofactors", label_position = "above")
                      )
                    )
                  ),
                  div(
                    class = "flex-container",
                    shiny::conditionalPanel(
                      condition = "input.results_tabs == 'Metabolic network'",
                      ns = ns, 
                      create_download_button(ns('download_network_model'), label = "Download network model")
                    )
                  )
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
    #Set namespace
    ns <- session$ns
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsNetwork")
    
    make_predictions_trigger <- make_action_button_trigger("make_predictions")
    
    updated_reference_reactions_trigger <- make_other_trigger(
      input$reference_reactions_database,
      input$reference_reactions_upload
    )
    
    url_change_trigger <- make_url_trigger(tab_name = "predictionsNetwork")

    update_metabolite_choices_trigger <- or_trigger(
      tab_selected_trigger, 
      updated_reference_reactions_trigger
    )
    
    get_graph_trigger <- make_other_trigger(
      url_change_trigger(),
      input$substrate_to_display, input$product_to_display,
      input$set_network_layout, input$set_network_dimensions,
      input$organism_to_display,
      input$hide_cofactors
    )

    # --- Get user input (events) ---
    # Get inputs
    get_inputs <- shiny::eventReactive({make_predictions_trigger()},
    {
      # Set source of inputs # debug
        functions_from_database <- isTRUE(input$function_tabs == "Database") & isFALSE(session$userData$img_init)
        functions_from_upload <- isTRUE(input$function_tabs == "File upload") & isFALSE(session$userData$img_init)
        reactions_from_database <- isTRUE(input$reaction_tabs == "Database") & isFALSE(session$userData$img_init)
        reactions_from_upload <- isTRUE(input$reaction_tabs == "File upload") & isFALSE(session$userData$img_init)
      
      # Get gene functions
        # Launch modal
        display_modal(ns = ns, message = "Loading gene functions")

        # get_gene_functions
          if (input$function_tabs == "Database") {
            database <- load_database()
            gene_functions <- load_gene_functions()
            selected_organisms <- input$gene_functions_database

            organism_by_genome <- get_organism_by_genome(database = database)
            gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)
            
            runValidationModal(need(gene_functions != "", 
                                    "Please choose at least one organism."))
          } else if (input$function_tabs == "File upload") {
            gene_functions <- validate_and_read_file(file_path = input$gene_functions_upload$datapath)
            gene_functions <- process_uploaded_gene_functions(gene_functions)
    
            runValidationModal(need(gene_functions != "", 
                                    "Please check the format of your predicted gene functions file and try again."))
          }

      # Get organism names
          organism_names <- colnames(gene_functions)
      
      # Get reference reactions
        # get_reference_reactions
          if (input$reaction_tabs == "Database") {
            selected <- input$reference_reactions_database
            reference_reactions <- get_reference_reactions_from_database(selected)
          } else if (input$reaction_tabs == "File upload") {
            reference_reactions <- validate_and_read_file(file_path = input$reference_reactions_upload$datapath)
            reference_reactions <- validate_reference_reactions(reference_reactions)
          }

          runValidationModal(need(reference_reactions != "", 
                                  "Please check the format of the reference reactions file and try again."))
      # Get substrates
        substrates <- switch(
          input$reaction_tabs,
          "Database" = input$substrates_database,
          "File upload" = input$substrates_upload,
          NULL
        )
        
        runValidationModal(need(substrates != "", "Please choose at least one substrate"))

      # Get products
        products <- switch(
          input$reaction_tabs,
          "Database" = input$products_database,
          "File upload" = input$products_upload,
          NULL
        )
        
        runValidationModal(need(products != "", "Please choose at least one product"))

      # Get unbalanced intermediates
        unbalanced_intermediates <- switch(
          input$reaction_tabs,
          "Database" = input$unbalanced_intermediates_database,
          "File upload" = input$unbalanced_intermediates_upload,
          NULL
        )
        
      # Get other inputs
        all_subunits <- input$all_subunits
        
      # Compile inputs
      list(
        gene_functions = gene_functions,
        organism_names = organism_names,
        reference_reactions = reference_reactions,
        substrates = substrates,
        products = products,
        unbalanced_intermediates = unbalanced_intermediates,
        all_subunits = all_subunits
      )
    }, 
    label = "get_inputs")
    
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
    # Perform computations
    compute_job <- shiny::eventReactive({make_predictions_trigger()},
    {
      results <- compute_network_predictions(
        reference_reactions = get_inputs()$reference_reactions,
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
      
      # Get model
      s = s[[organism]][[substrate]][[product]]
      
      # Change fluxes to 0 if product has flux less than threshold
      if (s$flux[which(s$name == "Ending_metabolite")] < threshold) {
        s$flux = 0
      }
      
      # Set metabolites to remove
      to_remove <- switch(
        as.character(hide_cofactors),
        "TRUE"  = setdiff(union(unbalanced_intermediates, enzyme_cofactors), c(product, substrate)),
        "FALSE" = unbalanced_intermediates
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
      selected <- get_organism_selections(input$reference_reactions_database)
      selected <- assign_if_invalid(selected, "Escherichia coli")
      
      update_select_input(inputId = "gene_functions_database", choices = choices, selected = selected)
      
      # Hide loading screen
      shinyjs::runjs("shinyjs.hide('network-loading-screen'); shinyjs.show('network-wrapper');")
    }, 
    label = "update_gene_function_choices", ignoreInit = TRUE)
    
    # Update choices for reference reactions
    observeEvent(tab_selected_trigger(), {
      choices <- get_choices_reference_reactions_in_database()
      
      update_select_input(inputId = "reference_reactions_database", choices = choices)
    }, label = "update_reference_reactions_choices")
    
    # Update choices for metabolites (substrates, products, and unbalanced intermediates)
    observeEvent({update_metabolite_choices_trigger()}, {
      # Get choices for metabolites
      selected_reaction <- if (input$reaction_tabs == "Database") input$reference_reactions_database else "Other"
      req(selected_reaction)
      if (input$reaction_tabs == "Database") {
        reference_reactions <- get_reference_reactions_from_database(selected_reaction)
      } else if (input$reaction_tabs == "File upload") {
        req(input$reference_reactions_upload$datapath)
        reference_reactions <- validate_and_read_file(file_path = input$reference_reactions_upload$datapath)
        reference_reactions <- validate_reference_reactions(reference_reactions)
        runValidationModal(need(reference_reactions != "", 
                                "Please check the format of the reference reactions file and try again."))
      }

      choices <- get_metabolite_names(reference_reactions$eq)
      
      # Get choices for substrates
      inputId <- if (input$reaction_tabs == "Database") "substrates_database" else "substrates_upload"
      metabolite_col <- "default_substrates"
      selected <- get_metabolite_selections(selected_reaction = selected_reaction, 
                                            metabolite_col = metabolite_col)
      update_select_input(inputId = inputId, choices = choices, selected = selected)
      
      # Get choices for products
      inputId <- if (input$reaction_tabs == "Database") "products_database" else "products_upload"
      metabolite_col <- "default_products"
      selected <- get_metabolite_selections(selected_reaction = selected_reaction, 
                                            metabolite_col = metabolite_col)
      update_select_input(inputId = inputId, choices = choices, selected = selected)
      
      # Get choices for unbalanced intermediates
      inputId <- if (input$reaction_tabs == "Database") "unbalanced_intermediates_database" else "unbalanced_intermediates_upload"
      metabolite_col <- "default_unbalanced_intermediates"
      selected <- get_metabolite_selections(selected_reaction = selected_reaction, 
                                            metabolite_col = metabolite_col)
      selected <- selected[selected %in% choices]

      update_select_input(inputId = inputId, choices = choices, selected = selected)
    }, label = "update_metabolite_choices")
  
    # Update choices for substrates, products, and organisms to display
    shiny::observeEvent(url_change_trigger(), {
      results <- get_results()
      
      update_picker_input(inputId = "substrate_to_display", choices = results$get_input_substrates)
      update_picker_input(inputId = "product_to_display", choices = results$get_input_products)
      update_picker_input(inputId = "organism_to_display", choices = results$get_organism_names)
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
      "gene_functions_uncharacterized",
      "gene_functions_rumen_cultured",
      "gene_functions_rumen_MAGs"
    ),
    labels = c(
      "E. coli",
      "Previously uncharacterized bacteria",
      "Cultured prokaryotes from rumen",
      "MAGs from rumen"
    ),
    ns = ns,
    label = "show_gene_functions_modal"
  )

  output_download_modal(
    input_id = "reference_reactions_modal",
    object_ids = c(
      "reference_reactions_glucose_fermentation",
      "reference_reactions_methanogenesis"
    ),
    labels = c(
      "Glucose fermentation",
      "Methanogenesis"
    ),
    ns = ns,
    label = "show_reference_reactions_modal"
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
  }
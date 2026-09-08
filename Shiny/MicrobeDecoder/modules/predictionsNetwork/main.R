# Define the Predictions from Networks Module in Shiny App
# This script defines the user interface (UI) and server for the predictions with metabolic networks module.
# Author: Timothy Hackmann
# Date: 22 May 2026

# === Define user interface (UI) ===
  predictionsNetworkUI <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
      # Title
      create_title_div("Predict traits with metabolic networks"),
      
      # Content
      bslib::layout_sidebar(
        # Sidebar
          sidebar = bslib::sidebar(
                id = ns("sidebar"), 
                open = get_sidebar_state(id),
                width = "30%",
                
                # Select data
                div("Organisms (gene functions)", class = "tight-heading"),
                bslib::navset_tab(id = ns("function_tabs"),
                                  bslib::nav_panel(title = "Database",
                                                   div(
                                                     create_selectize_input(inputId = ns("selected_organisms")),
                                                     div(
                                                       style = "display: flex; gap: 10px; align-items: baseline; margin-top: -8px;",
                                                       span(
                                                         style = "margin-top: -8px; padding: 0;",
                                                         shiny::actionLink(ns("load_gene_function_examples"), label = "Load examples")
                                                       ),
                                                       span(
                                                         style = "margin-top: -8px; padding: 0;",
                                                         fileInput_link(ns("selected_organisms_upload"), label = "Choose with file")
                                                       )
                                                     )
                                                   )
                                  ),
                                  bslib::nav_panel(title = "File upload",
                                                   fileInput_modal(ns("gene_functions"), modalId = ns("gene_functions_modal"))
                                  )
                ),
                div("Type of metabolism (reference network)", class = "tight-heading"),
                bslib::navset_tab(id = ns("reaction_tabs"),
                                  bslib::nav_panel(title = "Database",
                                                   create_selectize_input(inputId = ns("reference_network"), multiple = FALSE),
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
                 
                # Advanced
                bslib::accordion(
                  id = ns("advanced_accordion"),
                  open = FALSE,
                  multiple = TRUE,
                  class = "advanced-accordion",
                  
                  bslib::accordion_panel(
                    title = "Advanced settings",
                    value = "settings",
                    
                    create_selectize_input(inputId = ns("unbalanced_intermediates"), label = "Unbalanced intermediates"),
                    create_switch_input(inputId = ns("all_subunits"), label = "Enzymes must have all subunits")
                  ),
                  
                  bslib::accordion_panel(
                    title = "Advanced inputs",
                    value = "inputs",
                    
                    fileInput_modal(ns("tree_upload"), label = "Phylogenetic tree (optional)",
                                    accept = c(".nwk", ".newick", ".tre", ".tree", ".zip"), modalId = ns("tree_modal")),
                    fileInput_modal(ns("metadata_upload"), label = "Organism metadata (optional)",
                                    modalId = ns("metadata_modal"))
                  )
                ),
                
                # Make predictions
                shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
            ),
          
          # Main content area
          div(
             id = ns("results_page"),
             
             # Before results are available
             shiny::conditionalPanel(
               condition = "!output.flag_results",
               ns = ns,
               create_job_status_output(ns("job_status"))
             ),
             
             # After results are available
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
                      # The pickers choosing what is plotted stay in the toolbar.
                      # Everything that changes how it is drawn goes in the
                      # options accordion, whose button shares the toolbar and
                      # whose body opens across the row below it.  No condition
                      # is given because the flux threshold shows on every tab,
                      # so the accordion is never empty.
                      header = div(
                        class = "flex-container plot-options-container",
                        create_conditional_flex_item(ns,
                                                     "input.results_tabs == 'Metabolic network' && output.flag_multiple_organisms",
                                                     create_picker_input(ns("organism_to_display"), "Organism")
                        ),
                        create_conditional_flex_item(ns,
                                                     "input.results_tabs == 'Metabolic network' && output.flag_multiple_substrates",
                                                     create_picker_input(ns("substrate_to_display"), "Substrate")
                        ),
                        # The heatmap and the treemap share a picker of their own
                        # because they are the plots that can show every substrate
                        # at once, which the network tab cannot
                        create_conditional_flex_item(ns,
                                                     "(input.results_tabs == 'Heatmap' || input.results_tabs == 'Treemap') && output.flag_multiple_substrates",
                                                     create_picker_input(ns("plot_substrate_to_display"), "Substrate")
                        ),
                        create_conditional_flex_item(ns,
                                                     "input.results_tabs == 'Metabolic network' && output.flag_multiple_products",
                                                     create_picker_input(ns("product_to_display"), "End product")
                        ),
                        create_conditional_flex_item(ns,
                                                     "input.results_tabs == 'Tree' && output.flag_multiple_substrates",
                                                     create_picker_input(ns("tree_substrate_to_display"), "Substrate")
                        ),
                        create_conditional_flex_item(ns,
                                                     "input.results_tabs == 'Tree' && output.flag_multiple_products",
                                                     create_picker_input(ns("tree_product_to_display"), "End product")
                        ),
                        create_options_accordion(
                          div(
                            class = "flex-container plot-options-container",
                            # Shown on every tab: the threshold sets which fluxes count as
                            # a prediction, so it drives the plots, the summary text, and
                            # the substrate choices for the tree
                            div(class = "flex-item flex-item-slider",
                                create_slider_input(ns("flux_threshold"), "Flux threshold",
                                                    min = 0, max = 1000, value = 1)
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Metabolic network'",
                                                         create_picker_input(ns("network_layout"), "Layout")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Metabolic network'",
                                                         create_picker_input(ns("network_dimensions"), "Dimensions", choices = c("2", "3"), selected = "2")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Metabolic network'",
                                                         create_switch_input(ns("hide_cofactors"), "Hide cofactors", label_position = "above")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Metabolic network'",
                                                         create_switch_input(ns("hide_unbalanced_intermediates"), "Hide unbalanced", label_position = "above")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Tree'",
                                                         create_picker_input(ns("tree_layout"), "Layout")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Tree'",
                                                         create_switch_input(ns("tree_scale_bar"), "Scale bar", value = FALSE, label_position = "above")
                            ),
                            # One picker covers the axis, since the tick numbers are only
                            # meaningful once the axis is drawn
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs === 'Tree'",
                                                         create_picker_input(inputId = ns("tree_axis"), label = "x-axis",
                                                                             choices = c("Off", "Forward", "Backward"),
                                                                             selected = "Backward")
                            ),
                            # Separate switches so each plot can default differently: the
                            # tree starts binary, the heatmap starts continuous
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Heatmap'",
                                                         create_switch_input(ns("heatmap_binary"), "Values are binary", value = FALSE, label_position = "above")
                            ),
                            create_conditional_flex_item(ns,
                                                         "input.results_tabs == 'Tree'",
                                                         create_switch_input(ns("tree_binary"), "Values are binary", value = TRUE, label_position = "above")
                            )
                          ),
                          ns = ns
                        )
                      ),
                      
                      # Plot panels
                      create_plot_panel(ns, "tile", "Summary", fill = FALSE, height = "auto",
                                        flag_id = "flag_predictions",
                                        message_title = "No plot available",
                                        message_body = no_prediction_message),
                      create_plot_panel(ns, "heatmap", "Heatmap",
                                        flag_id = "flag_predictions",
                                        message_title = "No plot available",
                                        message_body = no_prediction_message),
                      create_plot_panel(ns, "treemap", "Treemap", centered = TRUE,
                                        flag_id = "flag_predictions",
                                        message_title = "No plot available",
                                        message_body = no_prediction_message),
                      create_plot_panel(ns, "network", "Metabolic network",
                                        flag_id = "flag_predictions",
                                        message_title = "No plot available",
                                        message_body = no_prediction_message),
                      create_plot_panel(ns, "tree", "Tree",
                                        flag_id = "flag_tree",
                                        message_title = "No plot available",
                                        message_body = "To make this plot, please upload a tree (advanced inputs) and re-run predictions."),
                      
                      # Network download button
                      footer = shiny::conditionalPanel(
                        condition = "input.results_tabs == 'Metabolic network'",
                        ns = ns,
                        create_download_button(ns("download_network_model"), "Download network model")
                      )
                  )
              )
            )
        )
    )
  }
  
  # === Define server ===
  predictionsNetworkServer <- function(id, selected_tab, on_ready) {
    shiny::moduleServer(id, function(input, output, session) {
    # --- Set namespace ----
    ns <- session$ns

    # --- Run Javascript functions ----
    shinyjs::runjs(sprintf(
      "shinyjs.resizeWidthFromHeight('%s', 1.045296);", ns("treemap-container")
    ))
    
    # --- Set variables ----
    ui_ready <- shiny::reactiveVal(FALSE) # For storing status of user interface (UI)
    session$userData$builder_selected <- reactiveVal("Glycolysis") # For storing the selected values in the builder
    session$userData$network_data <- reactiveVal(NULL) # For storing the user-built network
    network_filename <- reactiveVal(NULL) # For storing network's file name
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsNetwork")

    updated_reference_network_trigger <- make_other_trigger(
      input$reference_network,
      session$userData$network_data(),
      input$reference_network_upload
    )
    
    url_change_trigger <- make_url_trigger(tab_name = "predictionsNetwork")

    get_graph_trigger <- make_other_trigger(
      url_change_trigger(),
      input$substrate_to_display, 
      input$product_to_display,
      input$network_layout, 
      input$network_dimensions,
      input$organism_to_display,
      input$hide_cofactors,
      input$hide_unbalanced_intermediates
    )

    # --- Get user input (events) ---
    get_inputs <- function() {
      # Set flags
      functions_from_database <- isTRUE(input$function_tabs == "Database")
      functions_from_upload <- isTRUE(input$function_tabs == "File upload")
      network_from_database <- isTRUE(input$reaction_tabs == "Database")
      network_from_builder <- isTRUE(input$reaction_tabs == "Build")
      network_from_upload <- isTRUE(input$reaction_tabs == "File upload")

      # Compile inputs
      inputs <- get_network_inputs(
        functions_from_database = functions_from_database,
        functions_from_upload = functions_from_upload,
        network_from_database = network_from_database,
        network_from_builder = network_from_builder,
        network_from_upload = network_from_upload,
        selected_organisms = input$selected_organisms,
        gene_functions_upload_path = input$gene_functions$datapath,
        reference_network_upload_path = input$reference_network_upload$datapath,
        selected_reference_network = input$reference_network,
        substrates = input$substrates,
        products = input$products,
        unbalanced_intermediates = input$unbalanced_intermediates,
        all_subunits = input$all_subunits,
        tree_upload = input$tree_upload,
        metadata_upload = input$metadata_upload
      )

      # Shape into save function's signature
      list(
        reference_network        = inputs$reference_network,
        gene_functions           = inputs$gene_functions,
        organism_names           = inputs$organism_names,
        substrates               = inputs$substrates,
        products                 = inputs$products,
        unbalanced_intermediates = inputs$unbalanced_intermediates,
        all_subunits             = inputs$all_subunits,
        tree                     = inputs$tree,
        metadata                 = inputs$metadata
      )
    }

    # --- Perform computations ---
    setup_computation_jobs(
      ns              = ns,
      session         = session,
      submit_button = "make_predictions",
      get_inputs      = get_inputs,
      compute_fn_name = "run_job_network",
      tab_name        = "predictionsNetwork"
    )

    # --- Get results ---
    get_results <- eventReactive({ url_change_trigger() }, {
      job_id <- get_query_param()
      user_id <- get_query_param(param_name = "user")
      job_dir <- get_job_dir(tab = "predictionsNetwork", user_id = user_id)

      load_job_result(job_id, job_dir)
    }, label = "get_results")
    
    # --- Process results ---
    # Make network graph
    get_network_graph <- eventReactive({get_graph_trigger()},
    {
      # Get data
      results <- get_results()
      
      # Check required conditions
      req(!is.null(results))
      
      # Get inputs
      s <- results$get_solved_models
      organism <- input$organism_to_display
      substrate <- input$substrate_to_display
      product <- input$product_to_display
      unbalanced_intermediates <- results$get_unbalanced_intermediates
      threshold <- input$flux_threshold
      hide_cofactors <- input$hide_cofactors
      hide_unbalanced_intermediates <- input$hide_unbalanced_intermediates
      
      # Get model
      s <- rebuild_solved_model(s, organism, substrate, product)
      
      # Stop if no model (due to substrate and endproduct being identical)
      shiny::validate(shiny::need(!is.null(s), "No prediction for this substrate and end product"))
      
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
      g = make_network_graph(reaction_table = s, to_remove = to_remove)
      
      return(g)
    }, 
    label="get_network_graph")
  
    # Set layout for graph
    get_network_layout <- eventReactive(get_graph_trigger(),
    {
      g <- get_network_graph()

      layout <- set_network_layout(graph = g, type = input$network_layout, dimensions = input$network_dimensions)
      
      return(layout)
    },
    label="get_network_layout")

    # --- Update user interface (UI) elements ---
    # Update UI after loading module
    observeEvent(tab_selected_trigger(),
    {
      
      # Update choices for reference network
        update_reference_network_network(session = session)
  
      # Update choices for gene functions (organisms)
        update_selected_organisms_network(session = session)
      
      # Update choices for substrates
        update_substrates_network(session = session,
                                  reference_network = input$reference_network,
                                  reaction_tabs = input$reaction_tabs,
                                  upload_path = input$reference_network_upload$datapath)
        
      # Update choices for products
        update_products_network(session = session,
                                reference_network = input$reference_network,
                                reaction_tabs = input$reaction_tabs,
                                upload_path = input$reference_network_upload$datapath)
        
      # Update choices for unbalanced intermediates  
        update_unbalanced_intermediates_network(session = session,
                                                reference_network = input$reference_network,
                                                reaction_tabs = input$reaction_tabs,
                                                upload_path = input$reference_network_upload$datapath)
        
      # Update choices for tree layout
        update_tree_layout_network(session = session)
        
      # Signal user interface has been updated
        ui_ready(TRUE)
        
    },
    label="update_UI_after_loading")  

    # Update UI after computing results
    observeEvent(url_change_trigger(), {
      # Update (reset) spinners for plots and tables
        reset_spinners()
      
      # Check for required conditions
        req(!is.null(input$flux_threshold))
      
      # Update choices for substrate to display
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold)
        
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold,
                                            inputId = "plot_substrate_to_display",
                                            allow_any = TRUE)
        
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold,
                                            inputId = "tree_substrate_to_display",
                                            allow_any = TRUE)
        
      # Update choices for product to display
        update_product_to_display_network(session = session,
                                          data = get_results()$get_input_products)
        
        update_product_to_display_network(session = session,
                                          data = get_results()$get_input_products,
                                          inputId = "tree_product_to_display",
                                          allow_any = TRUE)
        
      # Update choices for organism to display
        update_organism_to_display_network(session = session,
                                           data = get_results()$get_organism_names)

      # Update sliders
        shinyjs::runjs(sprintf("shinyjs.refreshSlider('%s');", ns("flux_threshold")))
    },
    label="update_UI_after_results")
    
    # Update UI after user changes tree layout
    observeEvent(input$tree_layout, {
      param <- get_layout_param(assign_if_invalid(input$tree_layout, "Rectangular"))
      
      shinyWidgets::updatePickerInput(session = session, inputId = "tree_axis",
                                      selected = param$axis_start)
      shinyWidgets::updateSwitchInput(session = session, inputId = "tree_scale_bar",
                                      value = param$scale_bar_start)
    },
    label = "update_tree_plot_options")
    
    # Update UI after user changes input
    observeEvent(input$load_gene_function_examples, {
      # Update choices for gene functions (organisms)
        update_selected_organisms_network(session = session,
                                               reference_network = input$reference_network)
    },
    label="update_selected_organisms")
    
    observeEvent(input$selected_organisms_upload, {
      
      # Update choices for gene functions from upload  
        # Check conditions
        req(input$selected_organisms_upload)
        
        update_selected_organisms_network(session = session,
                                               upload_path = input$selected_organisms_upload$datapath)
    }, 
    label = "update_selected_organisms")
    
    observeEvent({updated_reference_network_trigger()}, {

      # Update choices for substrates
        update_substrates_network(session = session,
                                  reference_network = input$reference_network,
                                  reaction_tabs = input$reaction_tabs,
                                  upload_path = input$reference_network_upload$datapath)
        
      # Update choices for products
        update_products_network(session = session,
                                reference_network = input$reference_network,
                                reaction_tabs = input$reaction_tabs,
                                upload_path = input$reference_network_upload$datapath)
        
      # Update choices for unbalanced intermediates  
        update_unbalanced_intermediates_network(session = session,
                                                reference_network = input$reference_network,
                                                reaction_tabs = input$reaction_tabs,
                                                upload_path = input$reference_network_upload$datapath)

    }, 
    label = "update_metabolite_choices")
    
    observeEvent(input$flux_threshold, {
      
      # Update choices for substrate to display
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold)
        
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold,
                                            inputId = "plot_substrate_to_display",
                                            allow_any = TRUE)
        
        update_substrate_to_display_network(session = session,
                                            data = get_results()$predict_fluxes,
                                            substrates = get_results()$get_input_substrates,
                                            threshold = input$flux_threshold,
                                            inputId = "tree_substrate_to_display",
                                            allow_any = TRUE)
        
      # Update choices for product to display
        update_product_to_display_network(session = session,
                                          data = get_results()$get_input_products)
        
        update_product_to_display_network(session = session,
                                          data = get_results()$get_input_products,
                                          inputId = "tree_product_to_display",
                                          allow_any = TRUE)
        
      # Update choices for organism to display
        update_organism_to_display_network(session = session,
                                           data = get_results()$get_organism_names)
    }, 
    label = "update_selected_organisms")
    
    observeEvent({input$network_dimensions},
    {
      # Update choices for network layout
        update_network_layout_network(session = session,
                                          network_dimensions = input$network_dimensions)
    },
    label="update_network_layout")

    observeEvent(session$userData$network_data(), {
      # Update text for file input
        update_network_name_display_network(session = session,
                                            reference_network = session$userData$network_data(),
                                            network_filename = network_filename)
    },
    label="update_network_name_display")
    
    # Make other updates
      # Signal module is ready (hide loading screen)
      hide_loading_screen(
        trigger  = ui_ready,
        on_ready = on_ready
      )
      
    
  # --- Generate outputs ---
  # Output modals with example data
  shiny::observeEvent(input$gene_functions_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$gene_functions,
      help_panel = "Predict traits with metabolic networks"
    )
  }, ignoreInit = TRUE, label = "show_gene_functions_modal")
  
  shiny::observeEvent(input$reference_network_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$reference_network,
      help_panel = "Predict traits with metabolic networks"
    )
  }, ignoreInit = TRUE, label = "show_reference_network_modal")
  
  shiny::observeEvent(input$tree_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$tree,
      help_panel = "Predict traits with metabolic networks"
    )
  }, ignoreInit = TRUE, label = "show_tree_modal")
  
  shiny::observeEvent(input$metadata_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$metadata,
      help_panel = "Predict traits with metabolic networks"
    )
  }, ignoreInit = TRUE, label = "show_metadata_modal")

  # Create output flags
  flag_if_multiple(output, "flag_multiple_organisms", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_organism_names)
  flag_if_multiple(output, "flag_multiple_substrates", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_input_substrates)
  flag_if_multiple(output, "flag_multiple_products", trigger = url_change_trigger, 
                   value_fun = function() get_results()$get_input_products)
  flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
                   value_fun = function() get_results()) 
  flag_if_not_null(output, "flag_tree", trigger = url_change_trigger, 
                   value_fun = function() get_results()$tree) 
  flag_if_not_null(output, "flag_predictions", trigger = url_change_trigger, 
                   value_fun = function() {
                     df <- get_results()$predict_fluxes
                     # Return nothing when every flux is missing, which turns the flag off
                     if (is.null(df) || all(is.na(df$Flux))) NULL else df
                   }) 
  
  
  # Output message before results are available
  output$job_status <- render_job_status(
    tab_name      = "predictionsNetwork",
    empty_message = "Please make selections at left"
  )
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    # Get data
    df <- get_results()$predict_fluxes
    threshold <- input$flux_threshold
    
    # Check required conditions
    req(!is.null(df))
    
    # Count predictions
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
  get_plot_data <- eventReactive(list(url_change_trigger(), input$plot_substrate_to_display, 
                                      input$flux_threshold), 
  {
    
    # Get data
    df <- get_results()$predict_fluxes
    threshold <- input$flux_threshold
    organism_metadata <- get_results()$organism_metadata
    
    # Check required conditions
    shiny::req(!is.null(df))
    
    # Format organism names
    df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
    
    # Format organism metadata
    metadata <- format_metadata(df, organism_metadata)
    
    # Fall back to "Any" before the picker has been populated, which is what it
    # starts on once it is.
    substrate_to_display <- assign_if_invalid(input$plot_substrate_to_display, "Any")
    
    # Combine data
    # "Any" stands for every substrate. Leaving it out of the filter keeps a
    # cell with several fluxes, which the plots reduce to the largest of them.
    list(
      df = df,
      substrate_to_keep = if (identical(substrate_to_display, "Any")) NULL else substrate_to_display,
      threshold = threshold,
      metadata = metadata,
      cfg_tile = get_plot_config("predictionsNetwork", "tile"),
      cfg_tree = get_plot_config("predictionsNetwork", "tree")
    )
  }, 
  label = "get_plot_data")
  
  # Tile plot
  output$tile_plot <- plotly::renderPlotly({
    plot_data <- get_plot_data()
    df <- results_to_plot(df = plot_data$df, plot_type = "tile",
                          x_col = plot_data$cfg_tile$x_col, y_col = plot_data$cfg_tile$y_col, z_col = plot_data$cfg_tile$z_col,
                          var_col = plot_data$cfg_tile$var_col, var_to_keep = NULL,
                          z_threshold = plot_data$threshold,
                          drop_extra_y = plot_data$cfg_tile$drop_extra_y,
                          z_percentage = plot_data$cfg_tile$z_percentage)

    dims <- get_plotly_dimensions(ns = ns, plot_id = "tile_plot", default_width = 900)

    plot_tile(df,
              plot_width         = dims$width,
              display_threshold  = plot_data$cfg_tile$display_threshold,
              legend_title       = plot_data$cfg_tile$legend_title,
              label_header       = plot_data$cfg_tile$label_header,
              tile_header        = plot_data$cfg_tile$tile_header,
              hover_value_label  = plot_data$cfg_tile$hover_value_label,
              hover_value_scale  = plot_data$cfg_tile$hover_value_scale,
              hover_value_suffix = plot_data$cfg_tile$hover_value_suffix,
              hover_value_digits = plot_data$cfg_tile$hover_value_digits)
  })
  
  # Treemap plot
  output$treemap_plot <- plotly::renderPlotly({
    plot_data <- get_plot_data()
    
    # Drop cases where substrate and end product are identical
    plot_data$df <- drop_self_pairs(plot_data$df, substrate = plot_data$substrate_to_keep)
    
    df <- results_to_plot(df = plot_data$df, plot_type="treemap",
                          x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                          var_col = "Substrate", var_to_keep = plot_data$substrate_to_keep, 
                          z_threshold = plot_data$threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
    hovertemplate <- "<b>Endproduct: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>"
    plot_treemap(df,
                 hovertemplate = hovertemplate)
  })
  
  # Heatmap plot
  output$heatmap_plot <- plotly::renderPlotly({
    plot_data <- get_plot_data()
    
    # Drop cases where substrate and end product are identical
    plot_data$df <- drop_self_pairs(plot_data$df, substrate = plot_data$substrate_to_keep)
    
    df <- results_to_plot(df = plot_data$df, plot_type="heatmap",
                          x_col = "Organism name", y_col = "End product", z_col = "Flux", 
                          var_col = "Substrate", var_to_keep = plot_data$substrate_to_keep, 
                          z_threshold = plot_data$threshold, drop_extra_y = FALSE, z_percentage = FALSE) 
    
    coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
    borders <- get_heatmap_border(df, ns, "heatmap_plot")
    hovertemplate <- "<b>Organism: %{y}</b><br><b>Endproduct: %{x}</b><br><b>Flux: %{z:.0f}</b><extra></extra>"
    plot_heatmap(df, 
                 hovertemplate = hovertemplate,
                 legend_labels = c("0", "250", "500", "750", "1000"), 
                 legend_title = "Flux", 
                 zmax = 1000,
                 values_are_binary = isTRUE(input$heatmap_binary),
                 binary_value_label = if (isTRUE(input$heatmap_binary)) "Value" else NULL,
                 metadata = plot_data$metadata,
                 coord_fixed = coord_fixed,
                 horizontal_border = borders$horizontal_border,
                 vertical_border = borders$vertical_border
    )
  })
  
  # Output network graph
  output$network_plot <- plotly::renderPlotly(exp = {
    g <- get_network_graph()
    layout <- get_network_layout()
    network_dimensions <- input$network_dimensions
    
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
  
  # Tree plot
  # Format the uploaded tree and save to cache
  get_tree_layout_cached <- shiny::reactive({
    prepare_tree_layout(
      tree = get_results()$tree,
      layout_type = assign_if_invalid(input$tree_layout, "Rectangular")
    )
  })
  
  output$tree_plot <- plotly::renderPlotly({
    plot_data <- get_plot_data()
    layout <- get_tree_layout_cached()
    
    # Fall back to "Any" before the pickers have been populated, which is what
    # they start on once they are.
    substrate_to_display <- assign_if_invalid(input$tree_substrate_to_display, "Any")
    product_to_display <- assign_if_invalid(input$tree_product_to_display, "Any")
    
    # "Any" stands for every value of a variable. Leaving it out of the list
    # below skips the filter on it, which leaves a node with several fluxes and
    # draws the largest of them.
    var_to_keep <- list(
      var1 = if (identical(substrate_to_display, "Any")) NULL else substrate_to_display,
      var2 = if (identical(product_to_display, "Any")) NULL else product_to_display
    )
    
    # Reduce results to one value per node
    df <- results_to_tree_data(df = plot_data$df,
              spec = get_tree_data_spec("predictionsNetwork"),
              var_to_keep = var_to_keep,
              z_threshold = plot_data$threshold)
    
    # Get parameters for the layout and the range the value is colored over
    param <- get_layout_param(assign_if_invalid(input$tree_layout, "Rectangular"))
    z_range <- attr(df, "z_range")
    
    # Read the axis picker, which sets both whether the axis is drawn and how the ticks are numbered
    axis_mode <- assign_if_invalid(input$tree_axis, "Backward")
    
    plot_tree(layout                     = layout,
              df                         = df,
              label                      = plot_data$cfg_tree$label,
              match_hover_label          = plot_data$cfg_tree$match_hover_label,
              context_hover              = list(
                Substrate = substrate_to_display,
                Endproduct = product_to_display
              ),
              z_hover_label              = if (isTRUE(input$tree_binary %||% TRUE)) "Value" else "Flux",
              metadata                   = plot_data$metadata,
              type                       = param$shape,
              coord_fixed                = param$coord_fixed,
              x_to_y_ratio               = param$x_to_y_ratio %||% 1,
              branch_color_mode          = plot_data$cfg_tree$branch_color_mode,
              node_border_follows_branch = plot_data$cfg_tree$node_border_follows_branch,
              min_color                  = plot_data$cfg_tree$min_color,
              max_color                  = plot_data$cfg_tree$max_color,
              zmin                       = z_range$zmin,
              zmax                       = z_range$zmax,
              values_are_binary          = isTRUE(input$tree_binary %||% TRUE),
              show_axis                  = axis_mode != "Off",
              axis_title                 = plot_data$cfg_tree$axis_title,
              axis_from_tips             = axis_mode == "Backward",
              show_scale_bar             = isTRUE(input$tree_scale_bar))
  })
  
  # Output downloadable csv of results
  output$download_network_model <- create_download_handler(
    filename_prefix = "model",
    data_source = function() {
      # Get data
      s <- get_results()$get_solved_models
      
      # Check required conditions
      req(!is.null(s))
      
      # Get inputs
      organism <- input$organism_to_display
      substrate <- input$substrate_to_display
      product <- input$product_to_display
      
      # Get network model
      s <- rebuild_solved_model(s, organism, substrate, product)
    }
  )
  
  # Output modal for building networks
  observeEvent(input$`network_input-open_network_builder`, {
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
    })
  }
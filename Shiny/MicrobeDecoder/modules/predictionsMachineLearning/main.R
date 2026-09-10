# Define the Predictions from Machine Learning Module in Shiny App
# This script defines the user interface (UI) and server for the predictions using machine learning module.  
# Author: Timothy Hackmann
# Date: 22 May 2026

# === Define user interface (UI) ===
  predictionsMachineLearningUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        #Title
        create_title_div("Predict traits with machine learning"),
    
        bslib::layout_sidebar(
          #Sidebar
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
                                       fileInput_link(ns("selected_organisms_upload"), label = "Choose with file")
                                     )
                                   )
                                 )
                        ),
                        bslib::nav_panel(title = "File upload",
                                 fileInput_modal(ns("gene_functions"), modalId = ns("gene_functions_modal"))
                        )
                ),
                
                div("Traits or models", class = "tight-heading"),
                bslib::navset_tab(id = ns("model_tabs"),
                        bslib::nav_panel(title = "Standard traits",
                                 create_selectize_input(inputId = ns("model_names"), choices = NULL, selected = NULL),
                                 shiny::uiOutput(ns("model_warning"))
                        ),
                        bslib::nav_panel(title = "Other traits",
                                create_query_builder(ns = ns, input_id = "query_builder"),
                        ),
                        bslib::nav_panel(title = "Model upload",
                                fileInput_modal(ns("model_upload"), accept = c(".rds", ".zip"), modalId = ns("model_modal"))
                        )
                ),
                        
                # Advanced
                bslib::accordion(
                  id = ns("advanced_accordion"),
                  open = FALSE,
                  multiple = TRUE,
                  class = "advanced-accordion",
                  
                  bslib::accordion_panel(
                    title = "Advanced settings",
                    value = "settings",
                    
                    create_switch_input(inputId = ns("enable_saving"), label = "Enable saving of models", value = FALSE),
                    create_switch_input(inputId = ns("cache_models"), label = "Keep models in cache", value = FALSE),
                    
                    shiny::conditionalPanel(
                      condition = "input.model_tabs == `Other traits`",
                      ns = ns,
                      create_switch_input(inputId = ns("ignore_NA"), label = "Ignore missing values in database"),
                      shiny::sliderInput(ns("predictors_to_keep"), "Proportion of predictors to keep", min = 1e-3, max = 1, value = 0.1),
                      shiny::sliderInput(ns("responses_to_keep"), "Proportion of responses to keep", min = 1e-3, max = 1, value = 1),
                      shiny::sliderInput(ns("training_split"), "Proportion of responses for model training", min = 1e-3, max = 1, value = 0.7),
                      shiny::numericInput(ns("seed"), "Set seed for subsampling", value = 123, min = 1, step = 1),
                      shiny::numericInput(ns("ntree"), "Set number of trees", value = 50, min = 1, step = 1),
                      shiny::numericInput(ns("maxnodes"), "Set maximum nodes", value = 30, min = 1, step = 1),
                      shiny::sliderInput(ns("positive_class_weight"), "Weight for positive classes of responses", min = 1e-3, max = 1, value = 0.5),
                      create_switch_input(inputId = ns("balance_classes"), label = "Balance classes of responses", value = FALSE),
                      shiny::textInput(ns("trait_name"), "Name of trait (alphanumeric characters only)", value = "Custom trait", placeholder = "Enter an alphanumeric value")
                    )
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
              tags$span("Prediction Results", class = "nav-title")
            ),
            bslib::nav_spacer(),
            
            # Plot options
            # The model pickers stay in the toolbar; the threshold and the tree
            # options go in the accordion.  The accordion is hidden on the Model
            # tab, where none of its controls applies.
            header = div(
              class = "flex-container plot-options-container",
              create_conditional_flex_item(ns,
                                           "input.results_tabs === 'Tree'",
                                           create_picker_input(inputId = ns("tree_model_to_display"), label = "Model")
              ),
              create_conditional_flex_item(ns,
                                           "input.results_tabs === 'Model'",
                                           create_picker_input(inputId = ns("model_to_display"), label = "Model", multiple = FALSE)
              ),
              create_options_accordion(
                div(
                  class = "flex-container plot-options-container",
                  # Shown on every tab but Model, where the threshold has no effect
                  create_conditional_flex_item(ns,
                                               "input.results_tabs !== 'Model'",
                                               create_slider_input(ns("probability_threshold"), "Probability threshold",
                                                                   min = 0, max = 1, value = 0.5),
                                               item_class = "flex-item flex-item-slider"
                  ),
                  create_conditional_flex_item(ns,
                                               "input.results_tabs === 'Tree'",
                                               create_picker_input(inputId = ns("tree_layout"), label = "Layout")
                  ),
                  create_conditional_flex_item(ns,
                                               "input.results_tabs === 'Tree'",
                                               create_switch_input(inputId = ns("tree_scale_bar"), label = "Scale bar",
                                                                   value = FALSE, label_position = "above")
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
                                               "input.results_tabs === 'Heatmap'",
                                               create_switch_input(inputId = ns("heatmap_binary"), label = "Values are binary",
                                                                   value = FALSE, label_position = "above")
                  ),
                  create_conditional_flex_item(ns,
                                               "input.results_tabs === 'Tree'",
                                               create_switch_input(inputId = ns("tree_binary"), label = "Values are binary",
                                                                   value = TRUE, label_position = "above")
                  )
                ),
                ns = ns,
                condition = "input.results_tabs !== 'Model'"
              )
            ),
            
            # Result panels
            create_plot_panel(ns, "tile", "Summary", fill = FALSE, height = "auto"),
            create_plot_panel(ns, "heatmap", "Heatmap"),
            create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
            create_nav_panel(ns, "Model",
              div(
                shiny::textOutput(ns("training_summary")),
                create_conditional_download_button(
                  condition = "output.flag_models", inputId = "download_model",
                  label = "Download model", ns = ns)
              ),
              div(
                "Confusion matrix",
                create_plot_div(ns = ns, plot_type = "confusion_matrix", height = "30vh"),
                "Values in cells refer to number of organisms in the evaluation set.  Higher values in green cells are better."
              ),
              div(
                "Detailed metrics",
                create_plot_div(ns = ns, plot_type = "metrics", height = "220px"),
                "Higher values are better."
              ),
              div(
                create_download_button(ns('download_confusion_matrix'), label = "Download evaluation metrics")
              ),
              max_height = "none",
              fill = FALSE),
            create_plot_panel(ns, "tree", "Tree",
                              flag_id = "flag_tree",
                              message_title = "No plot available",
                              message_body = "To make this plot, please upload a tree (advanced inputs) and re-run predictions."),
            )
          )
          )
        )
        )
  }

# === Define server ===
  predictionsMachineLearningServer <- function(id, selected_tab, on_ready) {
    shiny::moduleServer(id, function(input, output, session) {
    # --- Set namespace ---
    ns <- session$ns

    # --- Run Javascript functions ----
    shinyjs::runjs(sprintf(
      "shinyjs.resizeWidthFromHeight('%s', 1.045296);", ns("treemap-container")
    ))
    shinyjs::runjs(sprintf(
      "registerQueryBuilderSelectizeStyling('%s');", ns("query_builder")
    ))

    # --- Set variables ----
    ui_ready <- shiny::reactiveVal(FALSE) # For storing status of user interface (UI)
    loading_screen_hidden <- shiny::reactiveVal(FALSE) # For storing status of loading screen
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsMachineLearning")
    
    tab_loaded_trigger <- make_tab_trigger(
      selected_tab, "predictionsMachineLearning", input, "selected_organisms"
    )

    url_change_trigger <- make_url_trigger(tab_name = "predictionsMachineLearning")

    # --- Get user input (events) ----
    get_inputs <- function() {
      # Set flags
      functions_from_database <- isTRUE(input$function_tabs == "Database")
      functions_from_upload   <- isTRUE(input$function_tabs == "File upload")
      models_from_standard <- isTRUE(input$model_tabs == "Standard traits")
      models_from_other    <- isTRUE(input$model_tabs == "Other traits")
      models_from_upload   <- isTRUE(input$model_tabs == "Model upload")
      
      # Compile inputs
      inputs <- get_ml_inputs(
        functions_from_database = functions_from_database,
        functions_from_upload = functions_from_upload,
        models_from_standard = models_from_standard,
        models_from_other = models_from_other,
        models_from_upload = models_from_upload,
        tree_upload = input$tree_upload,
        metadata_upload = input$metadata_upload,
        selected_organisms = input$selected_organisms,
        gene_functions_upload_path = input$gene_functions$datapath,
        model_names = input$model_names,
        trait_name = input$trait_name,
        model_upload = input$model_upload,
        model_upload_path = input$model_upload$datapath,
        model_path_config = model_path_config,
        query_string = input$query_builder,
        ignore_NA = input$ignore_NA,
        responses_to_keep = input$responses_to_keep,
        predictors_to_keep = input$predictors_to_keep,
        seed = input$seed,
        ntree = input$ntree,
        maxnodes = input$maxnodes,
        positive_class_weight = input$positive_class_weight,
        balance_classes = input$balance_classes,
        training_split = input$training_split,
        keep_models = (input$cache_models | input$enable_saving)
      )

      # Shape into save function's signature
      list(
        df                    = inputs$gene_functions,
        model_names           = inputs$model_names,
        model_paths           = inputs$model_paths,
        response              = inputs$response,
        predictors            = inputs$predictors,
        tree                  = inputs$tree,
        metadata              = inputs$metadata,
        seed                  = inputs$seed,
        ntree                 = inputs$ntree,
        maxnodes              = inputs$maxnodes,
        positive_class_weight = inputs$positive_class_weight,
        balance_classes       = inputs$balance_classes,
        training_split        = inputs$training_split,
        keep_models           = inputs$keep_models,
        enable_saving         = isTRUE(input$enable_saving)
      )
    }

    # --- Perform computations ---
    setup_computation_jobs(
      ns              = ns,
      session         = session,
      submit_button   = "make_predictions",
      get_inputs      = get_inputs,
      compute_fn_name = "run_job_ml",
      tab_name        = "predictionsMachineLearning"
    )

    # --- Get results ---
    get_results <- eventReactive({ url_change_trigger() }, {
      job_id <- get_query_param()
      user_id <- get_query_param(param_name = "user")
      job_dir <- get_job_dir(tab = "predictionsMachineLearning", user_id = user_id)
      load_job_result(job_id, job_dir)
    }, label = "get_results")
    
    # --- Process results ----
    # No logic for this module
    
    # --- Update user interface (UI) elements ---
    # Update UI after loading module
    observeEvent({tab_selected_trigger()},
    {
      # Update choices for gene functions (organisms)
        update_selected_organisms_ml(session = session)

      # Update query builder
        update_query_builder(inputId = "query_builder", choices = choices_traits_ML)
  
      # Update choices for models
        update_model_names_ml(session = session)
        
      # Update choices for tree layout
        update_tree_layout_ml(session = session)
        
      # Signal user interface has been updated
        ui_ready(TRUE)
    },
    label="update_UI_after_loading")
    
    # Update UI after computing results
    observeEvent(url_change_trigger(), {
      # Update (reset) spinners for plots and tables 
        reset_spinners()
      
      # Update choices for model to display
      # Runs before the threshold check because the choices ignore the threshold,
      # and the slider is not ready when the tab loads from History
        update_model_to_display_ml(session = session, data = get_results()$get_model_metadata)
      
      # Check for required conditions
        req(!is.null(input$probability_threshold))
        
      # Update choices for model shown on the tree
        update_tree_model_to_display_ml(session = session, data = get_results()$predict_traits,
                                        threshold = input$probability_threshold)

      # Update sliders
        shinyjs::runjs(sprintf("shinyjs.refreshSlider('%s');", ns("probability_threshold")))
    },
    label="update_UI_after_results")
    
    # Update UI after user changes probability threshold
    observeEvent(input$probability_threshold, {
      # Update choices for model shown on the tree, so the not-predicted flags
      # stay accurate
        update_tree_model_to_display_ml(session = session, data = get_results()$predict_traits,
                                        threshold = input$probability_threshold)
    },
    label="update_tree_model_to_display")
    
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
    observeEvent(input$selected_organisms_upload, {
      # Update choices for gene functions from database
        
        # Check required conditions
        req(input$selected_organisms_upload)
        
        update_selected_organisms_ml(session = session,
                                          upload_path = input$selected_organisms_upload$datapath)
    }, 
    label = "update_selected_organisms")
    
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
        help_panel = "Predict traits with machine learning"
      )
    }, ignoreInit = TRUE, label = "show_gene_functions_modal")
    
    shiny::observeEvent(input$model_modal, {
      create_download_modal(
        title      = "Example files",
        links      = example_file_links$model,
        help_panel = "Predict traits with machine learning"
      )
    }, ignoreInit = TRUE, label = "show_model_modal")
    
    shiny::observeEvent(input$tree_modal, {
      create_download_modal(
        title      = "Example files",
        links      = example_file_links$tree,
        help_panel = "Predict traits with machine learning"
      )
    }, ignoreInit = TRUE, label = "show_tree_modal")
    
    shiny::observeEvent(input$metadata_modal, {
      create_download_modal(
        title      = "Example files",
        links      = example_file_links$metadata,
        help_panel = "Predict traits with machine learning"
      )
    }, ignoreInit = TRUE, label = "show_metadata_modal")
    
    # Create output flags
    flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
                     value_fun = function() get_results()) 
    
    flag_if_not_null(output, "flag_models", trigger = url_change_trigger, 
                     value_fun = function() get_results()$get_models) 
    
    flag_if_not_null(output, "flag_tree", trigger = url_change_trigger, 
                     value_fun = function() get_results()$tree) 
    
    # Output message before results are available
    output$job_status <- render_job_status(
      tab_name      = "predictionsMachinelearning",
      empty_message = "Please make selections at left"
    )
    
    # Output summary text
    output$summary_text <- shiny::renderText({
      # Get data
      df <- get_results()$predict_traits
      threshold <- input$probability_threshold
      
      # Check required conditions
      req(!is.null(df))
      
      # Count predictions
      counts <- count_predictions(df, 
                  organism_col = "Organism number", 
                  trait_col = "Model", 
                  value_col = "Probability", 
                  threshold = threshold)
    
      format_summary_text(
        count1 = counts$traits_predictions, 
        count2 = counts$organisms_predictions, 
        label1 = "traits", 
        label2 = "organisms", 
        total2 = counts$organisms_total
      )
    })
    
    # Output downloadable csv of results
    output$download_data <- create_download_handler(
      filename_prefix = "results",
      data_source = function() {
        table <- get_results()$predict_traits
        table
      },
    )
    
    # Output overview plots
    # Get data
    get_plot_data <- eventReactive(list(url_change_trigger(), input$probability_threshold,
                                        input$tree_model_to_display), 
    {
      
      # Get inputs
      df <- get_results()$predict_traits
      threshold <- input$probability_threshold
      organism_metadata <- get_results()$organism_metadata
      
      # Check required conditions
      shiny::req(!is.null(df))
      
      # Format organism names
      df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
      
      # Format organism metadata
      metadata <- format_metadata(df, organism_metadata)
      
      # Combine data
      list(
        df = df,
        threshold = threshold,
        tree_model_to_display = input$tree_model_to_display,
        metadata = metadata,
        cfg_tile = get_plot_config("predictionsMachineLearning", "tile"),
        cfg_treemap = get_plot_config("predictionsMachineLearning", "treemap"),
        cfg_heatmap = get_plot_config("predictionsMachineLearning", "heatmap"),
        cfg_tree = get_plot_config("predictionsMachineLearning", "tree")
      )
    }, 
    label = "get_plot_data")
    
    # Tile plot
    output$tile_plot <- plotly::renderPlotly({
      plot_data <- get_plot_data()

      # Group models by trait category
      df <- get_trait_categories(plot_data$df)
      df <- results_to_plot(df = df, plot_type = "tile",
                x_col = plot_data$cfg_tile$x_col, y_col = plot_data$cfg_tile$y_col, z_col = plot_data$cfg_tile$z_col,
                var_col = plot_data$cfg_tile$var_col, var_to_keep = NULL,
                z_threshold = plot_data$threshold,
                drop_extra_y = plot_data$cfg_tile$drop_extra_y,
                z_percentage = plot_data$cfg_tile$z_percentage)

      dims <- get_plotly_dimensions(ns = ns, plot_id = "tile_plot", default_width = 900)

      plot_tile(df,
                plot_width        = dims$width,
                display_threshold = plot_data$cfg_tile$display_threshold,
                legend_title      = plot_data$cfg_tile$legend_title,
                label_header      = plot_data$cfg_tile$label_header,
                tile_header       = plot_data$cfg_tile$tile_header)
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      plot_data <- get_plot_data()
      df <- results_to_plot(df = plot_data$df, plot_type = "treemap",
                x_col = plot_data$cfg_treemap$x_col, y_col = plot_data$cfg_treemap$y_col, z_col = plot_data$cfg_treemap$z_col,
                var_col = plot_data$cfg_treemap$var_col, var_to_keep = NULL,
                z_threshold = plot_data$threshold,
                drop_extra_y = plot_data$cfg_treemap$drop_extra_y,
                z_percentage = plot_data$cfg_treemap$z_percentage)
      plot_treemap(df,
                hovertemplate = plot_data$cfg_treemap$hovertemplate)
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      plot_data <- get_plot_data()
      df <- results_to_plot(df = plot_data$df, plot_type = "heatmap",
                x_col = plot_data$cfg_heatmap$x_col, y_col = plot_data$cfg_heatmap$y_col, z_col = plot_data$cfg_heatmap$z_col,
                var_col = plot_data$cfg_heatmap$var_col, var_to_keep = NULL,
                z_threshold = plot_data$threshold,
                drop_extra_y = plot_data$cfg_heatmap$drop_extra_y,
                z_percentage = plot_data$cfg_heatmap$z_percentage)
      
      coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
      borders <- get_heatmap_border(df, ns, "heatmap_plot")
      
      plot_heatmap(df,
                hovertemplate     = plot_data$cfg_heatmap$hovertemplate,
                legend_labels     = as.character(plot_data$cfg_heatmap$legend_labels),
                legend_title      = plot_data$cfg_heatmap$legend_title,
                values_are_binary = isTRUE(input$heatmap_binary),
                binary_value_label = if (isTRUE(input$heatmap_binary)) "Value" else NULL,
                metadata          = plot_data$metadata,
                coord_fixed       = coord_fixed,
                horizontal_border = borders$horizontal_border,
                vertical_border   = borders$vertical_border)
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
      
      # Show one model at a time, so each node gets a single z value. Fall back
      # to the first model before the picker has been populated.
      model_to_display <- assign_if_invalid(plot_data$tree_model_to_display,
                                            unique(plot_data$df$Model)[1])
      
      # Reduce results to one value per node
      df <- results_to_tree_data(df = plot_data$df,
                spec = get_tree_data_spec("predictionsMachineLearning"),
                var_to_keep = list(var1 = model_to_display),
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
                context_hover              = list(Trait = model_to_display),
                z_hover_label              = if (isTRUE(input$tree_binary %||% TRUE)) "Value" else "% probability",
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
      
    # Model download and evaluation
    # Get data
    get_evaluation_data <- eventReactive(list(url_change_trigger(), input$model_to_display), 
    {
      # Get data
      metadata <- get_results()$get_model_metadata
      
      # Check required conditions
      req(!is.null(metadata))
      
      # Get model names
      model_names <- names(metadata)
      
      model_to_display <- assign_if_invalid(input$model_to_display, model_names[1])
      info <- metadata[[model_to_display]]
      
      eval <- info$evaluation
      if(!is.null(eval)){
        # Format the matrix for plotting
        eval$table = eval$table
        rownames(eval$table) <- c("Negative", "Positive")
        colnames(eval$table) <- c("Negative", "Positive")
        eval$table <- eval$table[c("Positive", "Negative"), c("Positive", "Negative")]
      }
      
      list(
        model_to_display = model_to_display,
        n_predictors = info$n_predictors,
        n_responses_train = info$n_responses,
        n_responses_eval = sum(eval$table),
        evaluation = eval
      )
    })
    
    # Output training summary
    output$training_summary <- shiny::renderText({
      evaluation_data <- get_evaluation_data()
      paste0("Model trained with ", evaluation_data$n_responses_train, " responses (organisms) and ", 
             evaluation_data$n_predictors, " predictors (gene functions). Model evaluated with an additional ",
             evaluation_data$n_responses_eval, " responses (organisms).")
    })
    
    # Plot confusion matrix
    output$confusion_matrix_plot <- plotly::renderPlotly({
      evaluation_data <- get_evaluation_data()
      plot_confusion_matrix(df = evaluation_data$evaluation)
    })

    # Plot metrics table
    output$metrics_plot <- plotly::renderPlotly({
      evaluation_data <- get_evaluation_data()
      plot_metrics_table(df = evaluation_data$evaluation)
    })

    # Output downloadable csv for confusion matrix 
    output$download_confusion_matrix <- create_download_handler(
      filename_prefix = reactive(input$model_to_display),
      data_source = function() {
        capture.output(print(get_evaluation_data()$evaluation))
      }
    )
    
    # Output downloadable rds for model
    output$download_model <- create_download_handler(
      filename_prefix = reactive(input$model_to_display),
      data_source = function() {
        # Get data
        models <- get_results()$get_models
        
        # Check required conditions
        req(!is.null(models))
        
        # Get selected model
        model_to_display <- input$model_to_display
        selected_model <- models[[model_to_display]]
      },
      file_type = "rds"
    )
    
    # Output modal for missing files
    output_missing_files_modal(input_id = "null_download_model", 
                               title = "No model available",
                               message = "Please enable saving of models (advanced settings) and re-run predictions.")
    
    # Output warning text for keeping too many models in memory
    output$model_warning <- shiny::renderUI({
      if (!isTRUE(input$enable_saving | input$cache_models)) {
        return(NULL)
      }
      
      bslib::card(
        class = "bg-warning border-warning",
        bslib::card_body("Saving multiple models or keeping them in cache may cause disconnection from server.")
      )
    })
      })
  }

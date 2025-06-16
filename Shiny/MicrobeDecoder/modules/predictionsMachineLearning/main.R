# Define the Predictions Using Machine Learning Module in Shiny App
# This script defines the user interface (UI) and server for the predictions using machine learning module.  
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Define user interface (UI) ===
	predictionsMachineLearningUI <- function(id) {
	  ns <- shiny::NS(id)
	  shiny::tagList(
		#Call JavaScript functions
		inject_js_resize(ns, "treemap-container"),
		inject_query_builder_js(ns, "query_builder"),
		
		# --- Loading screen ---
		create_loading_screen("ml-loading-screen"), 
		
		# --- Main UI (initially hidden ) ---
		shinyjs::hidden(
		  div(id = "ml-wrapper",
    		#Title
    		create_title_div("Predict traits with machine learning"),
    
    		bslib::layout_sidebar(
    		  #Sidebar
    		  sidebar = bslib::sidebar(
    		        id = ns("sidebar"), 
    					  width = "30%",
    					  
    					  # Select data
    					  div("Organisms (gene functions)", class = "tight-heading"),
    					  bslib::navset_tab(id = ns("function_tabs"),
    										bslib::nav_panel(title = "Database",
    														 create_selectize_input(inputId = ns("gene_functions_database"))
    										),
    										bslib::nav_panel(title = "File upload",
    														 fileInput_modal(ns("gene_functions_upload"), modalId = ns("gene_functions_modal"))
    										)
    					  ),
    					  
    					  div("Traits or models", class = "tight-heading"),
    					  bslib::navset_tab(id = ns("subtabs"),
    										bslib::nav_panel(title = "Standard traits",
    														 create_selectize_input(inputId = ns("model_names"), choices = NULL, selected = NULL),
    														 shiny::uiOutput(ns("model_warning"))
    										),
    										bslib::nav_panel(title = "Other traits",
    														create_query_builder(ns = ns, input_id = "query_builder"),
    										),
    										bslib::nav_panel(title = "Model upload",
    													  fileInput_modal(ns("model_upload"), accept = c(".rds"), modalId = ns("model_modal"))
    										),
    										
    										# Advanced inputs
    										shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
    										shiny::conditionalPanel(
    										  condition = "input.show_advanced",
    										  ns = ns,
    										  shiny::sliderInput(ns("threshold"), "Probability threshold", min = 0, max = 1, value = 0.5),
    										  create_switch_input(inputId = ns("enable_saving"), label = "Enable saving of models", value = FALSE),
    										),
    										shiny::conditionalPanel(
    										  condition = "input.show_advanced && input.subtabs == `Other traits`",
    										  ns = ns,
    										  create_switch_input(inputId = ns("ignore_missing"), label = "Ignore missing values in database"),
    										  shiny::sliderInput(ns("predictors_to_keep"), "Proportion of predictors to keep", min = 1e-3, max = 1, value = 0.1),
    										  shiny::sliderInput(ns("responses_to_keep"), "Proportion of responses to keep", min = 1e-3, max = 1, value = 1),
    										),
    										shiny::conditionalPanel(
    										  condition = "input.show_advanced && (input.subtabs == `Other traits`)",
    										  ns = ns,
    										  shiny::sliderInput(ns("training_split"), "Proportion of responses for model training", min = 1e-3, max = 1, value = 0.7),
    										  shiny::numericInput(ns("seed"), "Set seed for subsampling", value = 123, min = 1, step = 1),
    										  shiny::numericInput(ns("ntree"), "Set number of trees", value = 50, min = 1, step = 1),
    										  shiny::numericInput(ns("maxnodes"), "Set maximum nodes", value = 30, min = 1, step = 1),
    										  shiny::sliderInput(ns("positive_class_weight"), "Weight for positive classes of responses", min = 1e-3, max = 1, value = 0.5),
    										  shiny::textInput(ns("trait_name"), "Name of trait (alphanumeric characters only)", value = "Custom trait", placeholder = "Enter an alphanumeric value")
    										)
    						),
    					
    					# Make predictions
    					shiny::actionButton(ns("make_predictions"), "Make predictions", class = "btn btn-primary")
    		  ),
    		  #Main content area
    		  div(
    			id = ns("results_page"),
    
    			shiny::conditionalPanel(
    			  condition = "!output.flag_results",
    			  ns = ns,
    			  shiny::h4("Please upload files and make selections at left")
    			),
    			
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
    				title = "Prediction Results",
    				
    				# Panels
    				create_plot_panel(ns, "summary", "Summary"),
    				create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
    				create_plot_panel(ns, "heatmap", "Heatmap")
    			  ),
    			  
    			  # Model details
    			  bslib::card(
    				  bslib::card_header("Model training and evaluation"),
    				  full_screen = TRUE,
    				  create_picker_input(ns("model_to_display"), label = "Select a model", multiple = FALSE),
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
	predictionsMachineLearningServer <- function(input, output, session, x, selected_tab) {
	  # Set namespace
	  ns <- session$ns

	  # --- Define triggers for reactive expressions ---
	  tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsMachineLearning")
	  
	  make_predictions_trigger <- make_action_button_trigger("make_predictions")
	  
	  url_change_trigger <- make_url_trigger(tab_name = "predictionsMachineLearning")
	  
	  # --- Get user input (events) ----
	  # Get inputs
	  get_inputs <- shiny::eventReactive({make_predictions_trigger()}, {
	    # Get gene functions
	    # Update progress
	    # display_modal(ns = ns, message = "Loading gene functions") # debug
	    
	    # get_gene_functions
	    # Get file path
	    if (input$function_tabs == "Database") {
	      data <- load_database()
	      gene_functions <- load_gene_functions()
	      selected_organisms <- input$gene_functions_database
	      organism_by_genome <- get_organism_by_genome(database = data)
	      gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)
	      
	      runValidationModal(need(gene_functions != "", "Please choose at least one organism."))
	    } else if (input$function_tabs == "File upload") {
	      file_path <- input$gene_functions_upload$datapath
	      gene_functions <- validate_and_read_file(file_path = file_path)
	      gene_functions <- process_uploaded_gene_functions(gene_functions)
	      
	      runValidationModal(need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))
	    }
	    
	    # Get model names
	    if (input$subtabs == "Standard traits") {
	      model_names <- input$model_names
	    }else if(input$subtabs == "Other traits") {
	      model_names <- input$trait_name
	      runValidationModal(need(grepl("^[a-zA-Z0-9_ ]*$", model_names), "Please enter a valid trait name and try again."))
	      runValidationModal(need(model_names != "", "Please enter a valid trait name and try again."))
	    }else if(input$subtabs == "Model upload") {
	      model_names <- input$model_upload$name
	    }
	    
	    # Get file paths for models
	    if (input$subtabs == "Standard traits") {
	      model_paths <- lapply(model_names, function(model_name) {
	        model_paths[[model_name]]})
	      runValidationModal(need(model_paths != "", "Please choose at least one trait or model"))
	    }else if(input$subtabs == "Model upload") {
	      model_paths <- input$model_upload$datapath
	      runValidationModal(need(model_paths != "", "Please choose at least one trait or model"))
	    }else{
	      model_paths <- NULL
	    }
	    
	    # Get response variable
	    # Update progress
	    # display_modal(ns = ns, message = "Getting response variable") # debug
	    
	    # get_response
	    if (input$subtabs == "Other traits"){
	      query_string  <- input$query_builder
	      ignore_NA <- input$ignore_missing
	      
	      data <- load_database()
	      
	      query_string <- process_query_string(query_string)

	      runValidationModal(need(query_string != "", "Please build a valid query."))
	      
	      response <- format_response(data = data, 
	                                  query_string = query_string, 
	                                  ignore_NA = ignore_NA)
	      
	      n_response = length(unique(response$Response))
	      
	      runValidationModal(need(nrow(response)>0, "Please ensure the dataset has at least one response."))
	      runValidationModal(need(n_response == 2, "Please ensure that the response variable has exactly two classes."))
	      
	    }else{
	      response <- NULL
	    }
	    
	    # Get predictors
	    # get_predictors 
	    if (input$subtabs == "Other traits"){
	      # Update progress
	      # display_modal(ns = ns, message = "Getting predictors") # debug
	      
	      functions <- load_gene_functions()
	      responses_to_keep <- input$responses_to_keep
	      predictors_to_keep <- input$predictors_to_keep
	      seed <- input$seed
	      
	      predictors <- format_predictors(gene_functions = functions, 
	                                      responses_to_keep = responses_to_keep, 
	                                      predictors_to_keep = predictors_to_keep,
	                                      seed = seed)
	      
	      runValidationModal(need(ncol(predictors)>1, "Please ensure the dataset has at least one predictor"))
	      
	    }else{
	      predictors <- NULL
	    }
	    
	    # Get model training parameters
	    if(input$subtabs == "Other traits"){
	      seed <- input$seed
	      ntree <- input$ntree
	      maxnodes <- input$maxnodes
	      positive_class_weight <- input$positive_class_weight
	      training_split <- input$training_split
	    }else{
	      seed <- input$seed
	      ntree <- NULL
	      maxnodes <- NULL
	      positive_class_weight <- NULL
	      training_split <- NULL
	    }
	    
	    # Compile inputs
	    list(
	      gene_functions = gene_functions,
	      response = response,
	      predictors = predictors,
	      model_names = model_names,
	      model_paths = model_paths,
	      seed = seed,
	      ntree = ntree,
	      maxnodes = maxnodes,
	      positive_class_weight = positive_class_weight,
	      training_split = training_split
	    )
	  }, label = "get_inputs")
	  
	  # --- Process input ---
	  # Create job for computation
	  create_job <- shiny::eventReactive(make_predictions_trigger(), {
	    # Create job ID
	    job_id <- create_job_id()
	    
	    # Update URL with the  ID
	    url <- create_job_url(job_id = job_id, tab = "predictionsMachineLearning")
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
      results <- compute_ml_predictions(
        df = get_inputs()$gene_functions,
        model_names = get_inputs()$model_names,
        model_paths = get_inputs()$model_paths,
        response = get_inputs()$response,
        predictors = get_inputs()$predictors,
        seed = get_inputs()$seed,
        ntree = get_inputs()$ntree,
        maxnodes = get_inputs()$maxnodes,
        positive_class_weight = get_inputs()$positive_class_weight,
        training_split = get_inputs()$training_split,
        ns = ns
      )
      
      return(results)
    },
    label = "compute_job")
	  
	  # Get metadata for models
	  get_model_metadata <- shiny::eventReactive({make_predictions_trigger()}, {
	    models <- compute_job()$models

	    # Named model list
	    model_names <- names(models)
	    
	    metadata <- lapply(model_names, function(name) {
	      model <- models[[name]]
	      list(
	        n_predictors = length(model$forest$ncat),
	        n_responses = length(model$y),
	        evaluation  = model$evaluation_results
	      )
	    })
	    names(metadata) <- model_names

	    return(metadata)
	  }, label = "get_model_metadata")
	  
	  # --- Save and get results ---
	  # Save results
	  shiny::observeEvent({make_predictions_trigger()},
    {
      job_id <- create_job()
      job_dir <- get_job_dir(tab = "predictionsMachineLearning")
      enable_saving <- input$enable_saving
      
      results <- if(input$enable_saving) {
        list(
          predict_traits = compute_job()$probabilities,
          get_models = compute_job()$models,
          get_model_metadata = get_model_metadata()
        )
      }else{
        list(
          predict_traits = compute_job()$probabilities,
          get_model_metadata = get_model_metadata()
        )
      }
      
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
	    job_dir <- get_job_dir(tab = "predictionsMachineLearning", user_id = user_id)
	    load_job_result(job_id, job_dir)
	  })
	  
	  # --- Update user interface (UI) elements ---
	  # Update choices for gene functions (organisms)
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		  database <- load_database()
		  choices <- get_organism_choices(database)
		  update_select_input(inputId = "gene_functions_database", choices = choices)

      # Hide loading screen
		  shinyjs::runjs("shinyjs.hide('ml-loading-screen'); shinyjs.show('ml-wrapper');")
	  }, 
	  , label="update_gene_function_choices")

	  # Update query builder
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		update_query_builder(inputId = "query_builder", choices = choices_traits_ML)
	  },
	  label="update_query_builder")
	  
	  # Update choices for models
	  shiny::observeEvent({tab_selected_trigger()},
	  {
		choices = names(model_paths)
		selected = c(
					  "Fermentation (type of metabolism)",
					  "Methanogenesis (type of metabolism)"
					 )
		update_select_input(inputId = "model_names", choices = choices, selected = selected)
	  }, 
	  , label="update_model_choices")
	  
	  
	  # Update choices for model to display
	  shiny::observeEvent(url_change_trigger(), {
	    choices <- names(get_results()$get_model_metadata)
  		update_picker_input(inputId = "model_to_display", choices = choices)
	  },
	  label="update_model_to_display")
	  
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
	    input_id = "model_modal",
	    object_ids = c(
	      "model_fermentation",
	      "model_methanogenesis"
	    ),
	    labels = c(
	      "Fermentation",
	      "Methanogenesis"
	    ),
	    file_types = c("rds", "rds"),
	    ns = ns,
	    label = "show_model_modal"
	  )
	  
	  # Create observer to direct user to Help
	  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits with machine learning")
	  
	  # Create output flags
	  flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
	                   value_fun = function() get_results()) 
	  
	  flag_if_not_null(output, "flag_models", trigger = url_change_trigger, 
	                   value_fun = function() get_results()$get_models) 
	  
	  # Output summary text
	  output$summary_text <- shiny::renderText({
		df <- get_results()$predict_traits
		threshold <- input$threshold
		
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
    shiny::observeEvent({list(url_change_trigger(), input$threshold)},
    {
      # Get inputs
  		df <- get_results()$predict_traits
  		threshold <- input$threshold
  		
  		# Format organism names
  		req(df)
  		df <- format_organism_names(df, cols = "Organism name", abbreviate_names = FALSE)
  		
  		# Summary plot
  		output$summary_plot <- plotly::renderPlotly({
  		  df <- results_to_plot(df = df, plot_type="summary",
  								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
  								var_col = NULL, var_to_keep = NULL, 
  								z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
  		  plot = plot_summary(df, 
  							  coord_fixed = TRUE, 
  							  hovertemplate = "<b>Trait: %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>",
  							  legend_labels = c("0", "25", "50", "75", "100"), 
  							  legend_title = "% organisms positive")
  		})
  		
  		# Treemap plot
  		output$treemap_plot <- plotly::renderPlotly({    
  		  df <- results_to_plot(df = df, plot_type="treemap",
  								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
  								var_col = NULL, var_to_keep = NULL, 
  								z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
  		  hovertemplate <- "<b>Trait: %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>"
  		  plot = plot_treemap(df,
  							   hovertemplate = hovertemplate)
  		})
  		
  		# Heatmap plot
  		output$heatmap_plot <- plotly::renderPlotly({
  		  df <- results_to_plot(df = df, plot_type="heatmap",
  								x_col = "Organism name", y_col = "Model", z_col = "Probability", 
  								var_col = NULL, var_to_keep = NULL, 
  								z_threshold = threshold, drop_extra_y = FALSE, z_percentage = TRUE) 
  		  
  		  coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
  		  borders <- get_heatmap_border(df, ns, "heatmap_plot")
  		  hovertemplate <- "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>"
  		  
  		  plot <- plot_heatmap(df, 
  							   hovertemplate = hovertemplate,
  							   legend_labels = c("0", "25", "50", "75", "100"), 
  							   legend_title = "% probability",
  							   coord_fixed = coord_fixed,
  							   horizontal_border = borders$horizontal_border,
  							   vertical_border = borders$vertical_border
  							   )
  		})
  	  })
  	  
  	# Model download and evaluation
	  shiny::observeEvent({list(url_change_trigger(), input$model_to_display)},
	  {
	    metadata <- get_results()$get_model_metadata
	    model_names <- names(metadata)
	    
	    model_to_display <- assign_if_invalid(input$model_to_display, model_names[1])
	    info <- metadata[[model_to_display]]
	    
	    n_pred <- info$n_predictors
	    n_resp_train <- info$n_responses
	    eval <- info$evaluation
	    n_resp_eval <- sum(eval$table)
	    
  		# Output training summary
  		output$training_summary <- shiny::renderText(
  		  paste0("Model trained with ", n_resp_train, " responses (organisms) and ", 
  		         n_pred, " predictors (gene functions). Model evaluated with an additional ",
  		         n_resp_eval, " responses (organisms).")
  		)
  		
  		# Plot confusion matrix
  		output$confusion_matrix_plot <- plotly::renderPlotly({
  		  if(!is.null(eval)){
    		  # Format the matrix for plotting
    		  eval$table = eval$table
    		  rownames(eval$table) <- c("Negative", "Positive")
    		  colnames(eval$table) <- c("Negative", "Positive")
    		  eval$table <- eval$table[c("Positive", "Negative"), c("Positive", "Negative")]
  		  }
  		  
  		  plot <- plot_confusion_matrix(df = eval)
  		})
  
  		# Plot metrics table
  		output$metrics_plot <- plotly::renderPlotly({
  		  plot <- plot_metrics_table(df = eval)
  		})
  
  		# Output downloadable csv for confusion matrix 
  		output$download_confusion_matrix <- create_download_handler(
  		  filename_prefix = reactive(model_to_display),
  		  data_source = function() {
  		    capture.output(print(eval))  
  		  }
  		)
	  })
	  
	  # Output downloadable rds for model
	  output$download_model <- create_download_handler(
	    filename_prefix = reactive(input$model_to_display),
	    data_source = function() {
	      models <- get_results()$get_models
	      model_to_display <- input$model_to_display
	      selected_model <- models[[model_to_display]]
	    },
	    file_type = "rds"
	  )
	  
	  # Output modal for missing files
	  output_missing_files_modal(input_id = "null_download_model", 
	                             title = "No model available",
	                             message = "Please enable saving of models (advanced settings) and re-run predictions.")
	  
	  # Output warning text for loading too many models
	  observeEvent({input$model_names},
    {
	    threshold <- 5
	    n_selected <- length(input$model_names)

      if (n_selected >= threshold) {
	      output$model_warning <- renderUI({
	        bslib::card(
	          class = "bg-warning border-warning",
	          bslib::card_body("Loading", threshold, "or more models may cause disconnection from server.")
	        )
	      })
	    } else {
	      output$model_warning <- renderUI({ NULL })
	    }
	  })
	}
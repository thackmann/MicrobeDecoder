# Define the Predictions from Taxonomy Module in Shiny App
# This script defines the user interface (UI) and server for the predictions from taxonomy module.
# Author: Timothy Hackmann
# Date: 23 Mar 2025

# === Define user interface (UI) ===
  predictionsTaxonomyUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      #Call JavaScript functions
      inject_js_resize(ns, "treemap-container"),
      inject_query_builder_js(ns, "query_builder"),
      
      # --- Loading screen ---
      create_loading_screen("taxonomy-loading-screen"), 
      
      # --- Main UI (initially hidden ) ---
      #Title
      shinyjs::hidden(
        div(id = "taxonomy-wrapper",
            
        #Title
        create_title_div("Predict traits from taxonomy"),
    
        #Sidebar
        bslib::layout_sidebar(
          #Sidebar
          sidebar = bslib::sidebar(
            id = ns("sidebar"), 
            width = "30%",
            
            # Select data
            div("Choose organisms (taxa)"),
            bslib::navset_tab(id = ns("taxonomy_tabs"),
                              bslib::nav_panel(title = "Database",
                                               create_selectize_input(inputId = ns("taxonomy_database"))
                              ),
                              bslib::nav_panel(title = "File upload",
                                               fileInput_modal(ns("taxonomy_upload"), modalId = ns("taxonomy_file_modal")),
                              )
            ),
    
            # Select traits
            div("Choose traits"),
            bslib::navset_tab(id = ns("trait_tabs"),
                              bslib::nav_panel(title = "Standard traits",
                                               create_selectize_input(inputId = ns("set_traits")),
                              ),
                              bslib::nav_panel(title = "Other traits", 
                                                create_query_builder(ns = ns, input_id = "query_builder")
                              )
                      ),
           
            # Advanced inputs
            shiny::checkboxInput(ns("show_advanced"), "Show advanced settings", value = FALSE),
            
            shiny::conditionalPanel(
              condition = "input.show_advanced",
              ns = ns,
              shiny::sliderInput(ns("threshold"), "Probability threshold", min = 0, max = 1, value = 0.5),
              create_switch_input(inputId = ns("simple_names"), label = "Simplify names of taxa"),
              create_switch_input(inputId = ns("ignore_species"), label = "Ignore species names for taxa"),
              create_switch_input(inputId = ns("ignore_missing"), label = "Ignore missing values in database"),
              create_selectize_input(inputId = ns("system_taxonomy"), label = "Taxonomy", multiple = FALSE),
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
              shiny::h4("Please make selections at left")
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
                title = "Prediction results",
                
                # Panels
                create_plot_panel(ns, "summary", "Summary"),
                create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                create_plot_panel(ns, "heatmap", "Heatmap"),
                
                # Plot options
                div(
                  shiny::conditionalPanel(
                    condition = "output.flag_multiple_traits",
                    ns = ns,
                    create_picker_input(inputId = ns("trait_to_display"), label = "Trait")
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
predictionsTaxonomyServer <- function(input, output, session, x, selected_tab) {
  #Set namespace
  ns <- session$ns
  
  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsTaxonomy")
  make_predictions_trigger <- make_action_button_trigger("make_predictions")
  url_change_trigger <- make_url_trigger(param_name = "job", tab_name = "predictionsTaxonomy")
 
  # --- Create job for computation ---
  create_job <- shiny::eventReactive({make_predictions_trigger()}, 
  {
    # Create job ID
    job_id <- create_job_id()
    
    # Update URL with the  ID
    url <- create_job_url(job_id = job_id, tab = "predictionsTaxonomy")
    shiny::updateQueryString(sub(".*\\?", "?", url), mode = "push")
    
    # Update progress   
    display_modal(ns = ns, message = "Creating job for computation", value = 0, url = url)
    cat("Job created:", job_id)
    
    return(job_id)
    
  }, label = "create_job")
  
  # --- Get inputs ---
  # Get inputs
  get_inputs <- shiny::eventReactive({make_predictions_trigger()},
  {
     # Get query taxa
       if (input$taxonomy_tabs == "Database") {
         # Get inputs
         selected_organisms <- input$taxonomy_database
         query_taxa <- process_query_taxa(selected_organisms)
         runValidationModal(need((!is.null(query_taxa)) && (nrow(query_taxa) > 0), "Please choose an taxon"))
         
       } else if (input$taxonomy_tabs == "File upload") {
         # Validate, read, and process the taxonomy file
         query_taxa <- validate_and_read_file(file_path = input$taxonomy_upload$datapath)
         query_taxa <- process_uploaded_taxonomy(query_taxa)
         
         runValidationModal(need((!is.null(query_taxa)) && (nrow(query_taxa) > 0), "Please check the format of the taxonomy file and try again."))
       }
       
     # Get traits to predict
       if (input$trait_tabs == "Standard traits") {
         traits_to_predict <- input$set_traits
         runValidationModal(need(!is.null(traits_to_predict) && length(traits_to_predict) > 0, "Please choose a trait"))
       }else if(input$trait_tabs == "Other traits"){
         traits_to_predict <- "Custom trait"
       }
       
     # Get query string (from query builder)
       if(input$trait_tabs == "Other traits") {
         query_string  = input$query_builder
         query_string <- process_query_string(query_string)
         runValidationModal(need(query_string != "", "Please build a valid query."))
       }else if(input$trait_tabs == "Standard traits") {
         query_string <- NULL
       }
       
    # Get other inputs
      ignore_NA <- input$ignore_missing
      simple_names <- input$simple_names
      ignore_species <- input$ignore_species
      system_taxonomy <- input$system_taxonomy
    
     # Compile inputs
      list(
        query_taxa = query_taxa,
        traits_to_predict = traits_to_predict,
        query_string = query_string,
        ignore_NA = ignore_NA,
        simple_names = simple_names,
        ignore_species = ignore_species,
        system_taxonomy = system_taxonomy
      )
  }, 
  label = "get_inputs")
  
  #--- Perform computations ---
  # Perform computations
  compute_job <- shiny::eventReactive({make_predictions_trigger()},
  {
    results <- compute_taxonomy_predictions(
      data = load_database(),
      query_taxa = get_inputs()$query_taxa,
      query_string = get_inputs()$query_string,
      traits_to_predict = get_inputs()$traits_to_predict,
      ignore_NA = get_inputs()$ignore_NA,
      simple_names = get_inputs()$simple_names,
      ignore_species = get_inputs()$ignore_species,
      system_taxonomy = get_inputs()$system_taxonomy,
      ns = ns
    )
    
    return(results)
  },
  label="compute_job")
  
  # --- Save and get results ---
  # Save results
  shiny::observeEvent({make_predictions_trigger()},
  {
    job_id <- create_job()
    job_dir <- get_job_dir(tab = "predictionsTaxonomy")

    results <-
      list(
        predict_traits = compute_job()$probabilities
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
    # Load job
    job_id <- get_query_param()
    user_id <- get_query_param(param_name = "user")
    job_dir <- get_job_dir(tab = "predictionsTaxonomy", user_id = user_id)
    load_job_result(job_id, job_dir)
  })
  
  # --- Process results ----
  # No logic for this module
  
  # --- Update user interface (UI) elements ---
  # Update choices for traits
  shiny::observeEvent({tab_selected_trigger()},
  {
    choices <- choices_traits_taxonomy
    selected <-  c(
                    "Type of metabolism (FAPROTAX)", 
                    "Type of metabolism (Fermentation Explorer)", 
                    "Metabolites utilized (BacDive)", 
                    "Metabolites produced (BacDive)",
                    "Metabolites utilized (Fermentation Explorer)", 
                    "Metabolites produced (Fermentation Explorer)"
                   )
    update_select_input(inputId = "set_traits", choices = choices, selected = selected)
  },
  label="update_set_traits")
  
  # Update choices for system for taxonomy
  shiny::observeEvent({tab_selected_trigger()}, 
  {
      choices <- choices_system_taxonomy
      update_select_input(inputId = "system_taxonomy", choices = choices)
  },
  label="update_system_taxonomy")
  
  # Update choices for taxa
  shiny::observeEvent({input$system_taxonomy}, 
  {
        # Get data
        database <- load_database()
        
        # Get inputs
        system_taxonomy <- input$system_taxonomy
        system_taxonomy <- assign_if_invalid(system_taxonomy, "LPSN")
        
        # Get choices
        col_name <- paste0(system_taxonomy, " Taxonomy")
        choices <- expand_and_merge_taxonomy(data = database, col_name = col_name)
        choices <- get_taxon_choices(choices)
        
        selected <- "Escherichia (Genus)"
        
        update_select_input(inputId = "taxonomy_database", 
                            choices = choices, selected = selected)
        
        # Hide loading screen
        shinyjs::runjs("shinyjs.hide('taxonomy-loading-screen'); shinyjs.show('taxonomy-wrapper');")
  },
  ignoreInit = TRUE, label="update_choices_taxa")

  # Update choices for traits to display
  shiny::observeEvent(url_change_trigger(), {
    choices <- unique(get_results()$predict_traits$'Trait category')
    update_picker_input(inputId = "trait_to_display", choices = choices) 
  }, label = "update_traits_to_display")
  
  # Update query builder
  shiny::observeEvent({tab_selected_trigger()},
  {
    update_query_builder("query_builder", choices_traits_taxonomy)
  },
  ignoreInit = TRUE, label="update_query_builder")
  
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
  # Output modal with example data
  output_download_modal(
    input_id = "taxonomy_file_modal",
    object_ids = c(
      "taxa_uncharacterized",
      "taxa_rumen_cultured",
      "taxa_rumen_MAGs",
      "taxa_infant"
    ),
    labels = c(
      "Previously uncharacterized bacteria",
      "Cultured prokaryotes from rumen",
      "MAGs from rumen",
      "OTUs from infant gut"
    ),
    ns = ns,
    label = "show_data_modal"
  )

  # Create observer to navigate user to Help
  navigate_to_help(session = x, selected_tab = "help", selected_panel = "Predict traits from taxonomy")

  # Create output flags
  flag_if_multiple(output, "flag_multiple_traits", trigger = url_change_trigger, 
       value_fun = function() unique(get_results()$predict_traits$`Trait category`))
  
  flag_if_multiple(output, "flag_multiple_traits",trigger = url_change_trigger, 
       value_fun = function() unique(get_results()$predict_traits$`Trait category`))
  
  flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
       value_fun = function() get_results()) 
  
  # Output summary text
  output$summary_text <- shiny::renderText({
    df <- get_results()$predict_traits
    threshold <- input$threshold
    
    counts <- count_predictions(df, 
                organism_col = "Organism number", 
                trait_col = "Trait category", 
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
  
  # Output downloadable csv with matching results
  output$download_data <- create_download_handler(
    filename_prefix = "results",
    data_source = function() get_results()$predict_traits
  )

  # Output overview plots
  shiny::observeEvent({list(url_change_trigger(), input$threshold, input$trait_to_display)},
  {
    # Get inputs
    df <- get_results()$predict_traits
    threshold <- input$threshold
    trait_to_display = input$trait_to_display

    # Format organism names
    req(df)
    df <- format_organism_names(df, cols = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))
    
    # Summary plot
    output$summary_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="summary",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      hovertemplate = paste0("<b>",trait_to_display,": %{x}</b><br><b>% organisms positive: %{z:.2f}</b><br><extra></extra>")
      plot = plot_summary(df,
                          coord_fixed = TRUE,
                          hovertemplate = hovertemplate,
                          legend_labels = c("0", "25", "50", "75", "100"),
                          legend_title = "% organisms positive")
    })
    
    # Treemap plot
    output$treemap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="treemap",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      hovertemplate = paste0("<b>",trait_to_display,": %{label}</b><br><b>% total: %{value:.2f}</b><br><extra></extra>")
      plot = plot_treemap(df,
                          hovertemplate = hovertemplate)
    })
    
    # Heatmap plot
    output$heatmap_plot <- plotly::renderPlotly({
      df <- results_to_plot(df = df, plot_type="heatmap",
                            x_col = "Organism name", y_col = "Trait name", z_col = "Probability", 
                            var_col = "Trait category", var_to_keep = trait_to_display, 
                            z_threshold = threshold, drop_extra_y = TRUE, z_percentage = TRUE) 
      
      coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
      borders <- get_heatmap_border(df, ns, "heatmap_plot")
      hovertemplate <- "<b>Trait: %{x}</b><br><b>Organism: %{y}</b><br><b>% probability: %{z:.2f}</b><br><extra></extra>"
  
      plot <- plot_heatmap(df,
                          hovertemplate = hovertemplate,
                          data_are_binary = TRUE,
                          legend_labels = c("0", "25", "50", "75", "100"), 
                          legend_title = "% probability",
                          coord_fixed = coord_fixed,
                          horizontal_border = borders$horizontal_border,
                          vertical_border = borders$vertical_border
                          )
    })
  })
}
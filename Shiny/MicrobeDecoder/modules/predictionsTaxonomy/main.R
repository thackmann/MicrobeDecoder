# Define the Predictions from Taxonomy Module in Shiny App
# This script defines the user interface (UI) and server for the predictions from taxonomy module.
# Author: Timothy Hackmann
# Date: 22 May 2026

# === Define user interface (UI) ===
  predictionsTaxonomyUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      # Title
      create_title_div("Predict traits from taxonomy"),

        # Sidebar
        bslib::layout_sidebar(
          #Sidebar
          sidebar = bslib::sidebar(
            id = ns("sidebar"),
            open = get_sidebar_state(id),
            width = "30%",

            # Select data
            div("Organisms (taxa)", class = "tight-heading"),
            bslib::navset_tab(id = ns("taxonomy_tabs"),
                              bslib::nav_panel(title = "Database",
                                               create_selectize_input(inputId = ns("query_taxa"))
                              ),
                              bslib::nav_panel(title = "File upload",
                                               fileInput_modal(ns("query_taxa_upload"), modalId = ns("taxonomy_file_modal")),
                              )
            ),

            # Select traits
            div("Traits", class = "tight-heading"),
            bslib::navset_tab(id = ns("trait_tabs"),
                              bslib::nav_panel(title = "Standard traits",
                                               create_selectize_input(inputId = ns("traits_to_predict")),
                              ),
                              bslib::nav_panel(title = "Other traits",
                                                create_query_builder(ns = ns, input_id = "query_builder")
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

                create_switch_input(inputId = ns("hide_poor_traits"), label = "Hide poorly predicted traits", value = TRUE),
                create_switch_input(inputId = ns("match_all_ranks"), label = "All taxonomic ranks must match", value = FALSE),
                create_switch_input(inputId = ns("ignore_species"), label = "Ignore species names for taxa", value = FALSE),
                create_switch_input(inputId = ns("ignore_NA"), label = "Ignore missing values in database"),
                create_selectize_input(inputId = ns("system_taxonomy"), label = "Taxonomy", multiple = FALSE)
              ), 
              
              bslib::accordion_panel(
                title = "Advanced inputs",
                value = "inputs",

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
                # The trait and taxon pickers stay in the toolbar; the threshold
                # goes in the accordion.  No condition is given because the
                # threshold shows on every tab, so the accordion is never empty.
                header = div(
                  class = "flex-container plot-options-container",
                  create_conditional_flex_item(ns,
                                               "output.flag_multiple_traits && (input.results_tabs === 'Heatmap' || input.results_tabs === 'Treemap')",
                                               create_picker_input(inputId = ns("trait_to_display"), label = "Trait")
                  ),
                  create_conditional_flex_item(ns,
                                               "input.results_tabs === 'Database matches'",
                                               create_picker_input(inputId = ns("organism_to_display"), label = "Query taxon")
                  ),
                  create_options_accordion(
                    div(
                      class = "flex-container plot-options-container",
                      # Shown on every tab: the threshold sets which predictions count,
                      # so it drives every plot and the list of traits to display
                      div(class = "flex-item flex-item-slider",
                          create_slider_input(ns("probability_threshold"), "Probability threshold",
                                              min = 0, max = 1, value = 0.5)
                      ),
                      # Shown on the heatmap, the only plot here that shows a value
                      # per cell rather than presence or absence
                      create_conditional_flex_item(ns,
                                                   "input.results_tabs === 'Heatmap'",
                                                   create_switch_input(ns("heatmap_binary"), "Values are binary",
                                                                       value = FALSE, label_position = "above")
                      )
                    ),
                    ns = ns
                  )
                ),

                # Plot Panels
                create_plot_panel(ns, "tile", "Summary", fill = FALSE, height = "auto"),
                create_plot_panel(ns, "heatmap", "Heatmap"),
                create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                create_table_panel(ns, "matching_organisms", "Database matches")
              )
            )
        )
    )
  )
  }

# === Define server ===
predictionsTaxonomyServer <- function(id, selected_tab, on_ready) {
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

  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "predictionsTaxonomy")

  url_change_trigger <- make_url_trigger(param_name = "job", tab_name = "predictionsTaxonomy")
  
  # --- Get inputs ---
  # Plain function, not eventReactive. The job helper calls this in the
  # delayed submit phase after the modal has already been shown/flushed.
  # Drops UI-only fields so the returned list matches the compute function's
  # signature for do.call().
  get_inputs <- function() {
    # Set flags
    taxonomy_from_database <- isTRUE(input$taxonomy_tabs == "Database")
    taxonomy_from_upload   <- isTRUE(input$taxonomy_tabs == "File upload")
    traits_from_standard   <- isTRUE(input$trait_tabs == "Standard traits")
    traits_from_other      <- isTRUE(input$trait_tabs == "Other traits")

    # Call master input function
    inputs <- get_taxonomy_inputs(
      taxonomy_from_database = taxonomy_from_database,
      taxonomy_from_upload = taxonomy_from_upload,
      traits_from_standard = traits_from_standard,
      traits_from_other = traits_from_other,
      selected_organisms = input$query_taxa,
      taxonomy_upload_path = input$query_taxa_upload$datapath,
      traits_to_predict = input$traits_to_predict,
      query_string = input$query_builder,
      hide_poor_traits = input$hide_poor_traits,
      ignore_NA = input$ignore_NA,
      match_all_ranks = input$match_all_ranks,
      ignore_species = input$ignore_species,
      system_taxonomy = input$system_taxonomy,
      metadata_upload = input$metadata_upload
    )

    # Drop UI-only fields not consumed by the compute function
    inputs$hide_poor_traits <- NULL

    inputs
  }

  # --- Perform computations ---
  setup_computation_jobs(
    ns              = ns,
    session         = session,
    submit_button   = "make_predictions",
    get_inputs      = get_inputs,
    compute_fn_name = "run_job_taxonomy",
    tab_name        = "predictionsTaxonomy"
  )

  # --- Get results ---
  get_results <- eventReactive({ url_change_trigger() }, {
    job_id <- get_query_param()
    user_id <- get_query_param(param_name = "user")
    job_dir <- get_job_dir(tab = "predictionsTaxonomy", user_id = user_id)
    load_job_result(job_id, job_dir)
  }, label = "get_results")

  # --- Process results ----
  # No logic for this module

  # --- Update user interface (UI) elements ---
  # Update UI after loading module
  observeEvent(tab_selected_trigger(),
  {

    # Update choices for taxa
      update_query_taxa_taxonomy(session = session, system_taxonomy = input$system_taxonomy)

    # Update choices for traits
      update_traits_to_predict_taxonomy(session = session)

    # Update choices for system for taxonomy
      update_system_taxonomy_taxonomy(session = session)

    # Update query builder
      update_query_builder(inputId = "query_builder", choices = choices_traits_taxonomy)

    # Signal user interface has been updated
      ui_ready(TRUE)
  },
  label = "update_UI_after_loading")

  # Update UI after computing results
  observeEvent(url_change_trigger(),
  {
    # Update (reset) spinners for plots and tables 
      reset_spinners()
    
    # Check for required conditions
      req(!is.null(input$probability_threshold))

    # Update choices for traits to display
      update_trait_to_display_taxonomy(session = session, data = get_results()$predict_traits,
                                       threshold = input$probability_threshold)

    # Update choices for organisms to display
      update_organism_to_display_taxonomy(session = session, data = get_results()$query_taxa)

    # Update slider 
      shinyjs::runjs(sprintf("shinyjs.refreshSlider('%s');", ns("probability_threshold")))
  },
  label = "update_UI_after_results")

  # Update UI after user changes input
    observeEvent(input$hide_poor_traits,
    {

      # Update choices for traits
        update_traits_to_predict_taxonomy(session = session, poor_choices = input$hide_poor_traits)
  },
  label = "update_traits_to_predict")

    observeEvent(input$probability_threshold, {

      # Update choices for traits to display
        update_trait_to_display_taxonomy(session = session, data = get_results()$predict_traits,
                                         threshold = input$probability_threshold)
    },
    label = "update_traits_to_display")

  # Make other updates
    # Signal module is ready (hide loading screen)
    hide_loading_screen(
      trigger  = ui_ready,
      on_ready = on_ready
    )


  # --- Generate outputs ---
  # Output modal with example data
  shiny::observeEvent(input$taxonomy_file_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$taxa,
      help_panel = "Predict traits from taxonomy"
    )
  }, ignoreInit = TRUE, label = "show_data_modal")
  
  shiny::observeEvent(input$metadata_modal, {
    create_download_modal(
      title      = "Example files",
      links      = example_file_links$metadata,
      help_panel = "Predict traits from taxonomy"
    )
  }, ignoreInit = TRUE, label = "show_metadata_modal")

  # Create output flags
  flag_if_multiple(output, "flag_multiple_traits", trigger = url_change_trigger,
       value_fun = function() unique(get_results()$predict_traits$`Trait category`))

  flag_if_not_null(output, "flag_results", trigger = url_change_trigger,
       value_fun = function() get_results())

  # Output message before results are available
  output$job_status <- render_job_status(
    tab_name      = "predictionsTaxonomy",
    empty_message = "Please make selections at left"
  )

  # Output summary text
  output$summary_text <- shiny::renderText({
    # Get data
    df <- get_results()$predict_traits
    threshold <- input$probability_threshold
    
    # Check required conditions
    shiny::req(!is.null(df))
    
    # Count predictions
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
  get_plot_data <- eventReactive(list(url_change_trigger(), input$trait_to_display, input$probability_threshold),
  {

    # Get inputs
    df <- get_results()$predict_traits
    threshold <- input$probability_threshold
    trait_to_display <- input$trait_to_display
    organism_metadata <- get_results()$organism_metadata
    
    # Check required conditions
    shiny::req(!is.null(df))
    
    # Format organism names
    df <- format_organism_names(df, cols = c("Phylum", "Class", "Order", "Family", "Genus", "Species"))

    # Format organism metadata
    metadata <- format_metadata(df, organism_metadata,
                                cols = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                allow_id_override = FALSE)
    
    # Combine data
    list(
      df = df,
      threshold = threshold,
      trait_to_display = trait_to_display,
      metadata = metadata,
      cfg_tile = get_plot_config("predictionsTaxonomy", "tile"),
      cfg_treemap = get_plot_config("predictionsTaxonomy", "treemap"),
      cfg_heatmap = get_plot_config("predictionsTaxonomy", "heatmap")
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
                          var_col = plot_data$cfg_treemap$var_col, var_to_keep = plot_data$trait_to_display,
                          z_threshold = plot_data$threshold,
                          drop_extra_y = plot_data$cfg_treemap$drop_extra_y,
                          z_percentage = plot_data$cfg_treemap$z_percentage)
    plot_treemap(df,
                 hovertemplate = build_hovertemplate(plot_data$cfg_treemap$hovertemplate, plot_data$trait_to_display))
  })

  # Heatmap plot
  output$heatmap_plot <- plotly::renderPlotly({
    plot_data <- get_plot_data()
    df <- results_to_plot(df = plot_data$df, plot_type = "heatmap",
                          x_col = plot_data$cfg_heatmap$x_col, y_col = plot_data$cfg_heatmap$y_col, z_col = plot_data$cfg_heatmap$z_col,
                          var_col = plot_data$cfg_heatmap$var_col, var_to_keep = plot_data$trait_to_display,
                          z_threshold = plot_data$threshold,
                          drop_extra_y = plot_data$cfg_heatmap$drop_extra_y,
                          z_percentage = plot_data$cfg_heatmap$z_percentage)

    coord_fixed <- get_coord_heatmap(df, ns, "heatmap_plot")
    borders <- get_heatmap_border(df, ns, "heatmap_plot")

    plot_heatmap(df,
                 hovertemplate     = plot_data$cfg_heatmap$hovertemplate,
                 values_are_binary = isTRUE(input$heatmap_binary),
                 binary_value_label = if (isTRUE(input$heatmap_binary)) "Value" else NULL,
                 legend_labels     = as.character(plot_data$cfg_heatmap$legend_labels),
                 legend_title      = plot_data$cfg_heatmap$legend_title,
                 metadata          = plot_data$metadata, 
                 coord_fixed       = coord_fixed,
                 horizontal_border = borders$horizontal_border,
                 vertical_border   = borders$vertical_border)
  })

  # Output database matches
  output$matching_organisms_table <- DT::renderDataTable({

    # Get inputs
    data <- load_database()
    organism_to_display <- input$organism_to_display
    shiny::req(!is.null(organism_to_display) && nchar(organism_to_display) > 0)
    results <- get_results()
    shiny::req(!is.null(results))
    query_taxa <- get_results()$query_taxa

    # Look up the query_taxa row by index (picker value is the row number)
    shiny::req(!is.null(query_taxa))
    idx <- as.integer(organism_to_display)
    shiny::req(!is.na(idx) && idx >= 1L && idx <= nrow(query_taxa))
    query_taxa_row <- query_taxa[idx, , drop = FALSE]

    df <- get_matching_organisms_for_query(
      query_taxa_row    = query_taxa_row,
      data              = data,
      traits_to_predict = results$traits_to_predict,
      query_string      = results$query_string,
      ignore_species    = isTRUE(results$ignore_species),
      match_all_ranks   = isTRUE(results$match_all_ranks),
      system_taxonomy   = results$system_taxonomy %||% "LPSN"
    )

    DT::datatable(
      df,
      rownames = FALSE,
      escape   = FALSE,
      options  = list(scrollX = TRUE)
    )
  })
  })
}

# Define the Search Database Module in Shiny App
# This script defines the user interface (UI) and server for the search database module.
# Author: Timothy Hackmann
# Date: 22 May 2026

# === Define user interface (UI) ===
  # Search database tab
  databaseSearchUI <- function(id) {
    ns <- NS(id)
    shiny::tagList(
          # Title
          create_title_div("Search database"),
          
          bslib::layout_sidebar(
            #Sidebar
            sidebar = bslib::sidebar(
              id = ns("sidebar"),
              open = get_sidebar_state(id),
              width = "30%",
              
              create_query_builder(ns = ns, input_id = "query_builder", label = "Query"),
              
              shiny::actionButton(ns("run_search"), "Run search", class = "btn btn-primary")
            ),
            # Main content area
            shiny::div(
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
                  bslib::card_header(textOutput(ns("summary_text"))), 
                  create_download_button(ns('download_data'))
                ),
                
                # Tabs for plots
                bslib::navset_card_underline(
                  id = ns("results_tabs"),
                  
                  # Title
                  bslib::nav_item(
                    tags$span("Plots", class = "nav-title")
                  ),
                  bslib::nav_spacer(),
                  
                  # Plot options
                  # The variable picker stays in the toolbar; the tree layout
                  # goes in the accordion, which hides itself on the tabs where
                  # no option applies.  The note below is a caption rather than a
                  # control, so it sits outside both.
                  header = shiny::tagList(
                    div(
                      class = "flex-container plot-options-container",
                      create_conditional_flex_item(ns,
                                                   "input.results_tabs === 'Treemap'",
                                                   create_picker_input(inputId = ns("variable_to_display"), label = "Variable")
                      ),
                      create_options_accordion(
                        div(
                          class = "flex-container plot-options-container",
                          create_conditional_flex_item(ns,
                                                       "input.results_tabs === 'Tree'",
                                                       create_picker_input(inputId = ns("tree_layout"), label = "Layout")
                          )
                        ),
                        ns = ns,
                        condition = "input.results_tabs === 'Tree'"
                      )
                    ),
                    shiny::conditionalPanel(
                      condition = "input.results_tabs === 'Tree' || input.results_tabs === 't-SNE'",
                      ns = ns,
                      div(
                        class = "plot-options-note",
                        "Matching organisms are those that are fully colored. Only organisms with genome sequences are shown."
                      )
                    )
                  ),
                  
                  # Panels
                  create_plot_panel(ns, "treemap", "Treemap", centered = TRUE),
                  create_plot_panel(ns, "tree", "Tree"), 
                  create_plot_panel(ns, "tsne", "t-SNE")
                ),
                  
                # Detailed results
                bslib::card(
                  bslib::card_header("Detailed results"),  
                  full_screen = TRUE,
                  create_data_table(inputId = ns("table")),
                  bslib::layout_column_wrap(
                    width = 1/4, 
                    shiny::checkboxGroupInput(inputId = ns("info_organism"), label = "Organism"),
                    shiny::checkboxGroupInput(inputId = ns("info_databases"), label = "Databases"),
                    shiny::checkboxGroupInput(inputId = ns("info_metabolism"), label = "Metabolism"),
                    shiny::checkboxGroupInput(inputId = ns("info_traits"), label = "Traits")
                  )
                )
              )
            )
        )
    )
  }

# === Define server ===
  databaseSearchServer <- function(id, selected_tab, on_ready) {
    shiny::moduleServer(id, function(input, output, session) {
    # --- Set namespace ----
    ns <- session$ns
  
    # --- Run Javascript functions ----
    shinyjs::runjs(sprintf(
      "shinyjs.resizeWidthFromHeight('%s', 1.045296);", ns("treemap-container")
    ))
    shinyjs::runjs(sprintf(
      "registerQueryBuilderSelectizeStyling('%s');", ns("query_builder")
    ))
    shinyjs::runjs(sprintf(
      "registerQueryBuilderFiltersSetSignal('%s');", ns("query_builder")
    ))
      
    # --- Set variables ----
    ui_ready <- shiny::reactiveVal(FALSE) # For storing status of user interface (UI)
    loading_screen_hidden <- shiny::reactiveVal(FALSE) # For storing status of loading screen
    
    # --- Define triggers for reactive expressions ---
    tab_selected_trigger <- make_tab_trigger(selected_tab, "databaseSearch")

    url_change_trigger <- make_url_trigger(param_name = "job")

    build_table_trigger <- make_other_trigger(
      url_change_trigger(),
      input$info_organism,
      input$info_databases,
      input$info_metabolism,
      input$info_traits
    )
    
    get_tree_trigger <- make_other_trigger(url_change_trigger(), 
                                           input$tree_layout)
    
    # --- Get user input (events) ---
    # Plain function, not eventReactive. The job helper calls this in the
    # delayed submit phase after the modal has already been shown/flushed.
    get_inputs <- function() {
      get_search_inputs(query_string = input$query_builder)
    }

    # --- Perform computations ---
    setup_computation_jobs(
      ns              = ns,
      session         = session,
      submit_button   = "run_search",
      get_inputs      = get_inputs,
      compute_fn_name = "run_job_search",
      tab_name        = "databaseSearch",
      submit_message  = "Creating job for search",
      working_message = "Performing search",
      cancel_message  = "Search canceled"
    )

    # --- Get results ---
    get_results <- eventReactive({ url_change_trigger() }, {
      job_id <- get_query_param()
      user_id <- get_query_param(param_name = "user")
      job_dir <- get_job_dir(tab = "databaseSearch", user_id = user_id)
      load_job_result(job_id, job_dir)
    }, label = "get_results")

    # --- Process results ----
    # Build data table
    build_table <- eventReactive({build_table_trigger()}, {
      # Get data
      data <- get_results()$filter_data
      
      # Check required conditions
      req(!is.null(data))
      
      # Rename columns with links
      data <- data |>
        rename_and_overwrite(" link$", "") # Remove " link" and overwrite
      
      # Dynamically get names of selected columns from checkboxes
      selected_columns <- lapply(names(choices_checkboxes_search), function(category) {
        selected_vals <- input[[paste0("info_", category)]]
        selected_vals[!is.na(selected_vals)]
      }) |> unlist()
      
      # Keep only selected columns
      data <- data |> dplyr::select(dplyr::all_of(selected_columns))

      # Print status to log
      cat(file = stderr(), paste0("Ended search at ", Sys.time(), "\n"))
      
      return(data)
    },
    label = "build_table")
    
    # Plot phylogenetic tree
    plot_tree <- eventReactive({get_tree_trigger()}, {
      layout_type <- input$tree_layout
      
      # Get layouts
      layout <- switch(layout_type,
                       "Daylight" = load_data("layout_tree_daylight"),
                       "Equal angle" = load_data("layout_tree_equal_angle"),
                       "Rectangular" = load_data("layout_tree_rectangular"))
      
      branches_all <- switch(layout_type,
                             "Daylight" = load_data("plot_branches_all_daylight"),
                             "Equal angle" = load_data("plot_branches_all_equal_angle"),
                             "Rectangular" = load_data("plot_branches_all_rectangular"))
      
      tips_all <- switch(layout_type,
                         "Daylight" = load_data("plot_tips_all_daylight"),
                         "Equal angle" = load_data("plot_tips_all_equal_angle"),
                         "Rectangular" = load_data("plot_tips_all_rectangular"))
      
      # Get data
      data <- get_results()$filter_data
      
      # Check required conditions
      req(!is.null(data))
      
      # Get matching data and filter layout
      nodes_to_root <- load_data("nodes_to_root")
      layout_filtered <- filter_tree_layout(layout, data, nodes_to_root, id_column = "IMG Genome ID max quality")
      
      # Parameters for matching plots
      layout_param <- get_layout_param(layout_type)
      coord_fixed <- layout_param$coord_fixed
      x_to_y_ratio <- layout_param$x_to_y_ratio
      
      # Plot matching branches
      branches_matching <- ggtree_to_plotly(
        layout = layout_filtered,
        type = "daylight",  # assumed to match your actual layout logic
        coord_fixed = coord_fixed,
        x_to_y_ratio = x_to_y_ratio,
        color = green_color,
        linewidth = 1
      )
      
      # Format tips layout
      tips_matching_layout <- layout |>
        dplyr::filter(isTip == TRUE) |>
        add_taxonomy_to_layout(layout_ID = "label", taxonomy = data, taxonomy_ID = "IMG Genome ID max quality") |>
        add_fill_to_layout(group = "Phylum", lighten_amount = 0.2) |>
        add_color_to_layout(group = "Phylum", lighten_amount = 0)
      
      # Plot matching tips
      tips_matching <- plot_scatterplot(
        df = tips_matching_layout,
        color = tips_matching_layout$color,
        fill = tips_matching_layout$fill,
        stroke = 1,
        size = 5,
        shape = "circle",
        alpha = 1,
        label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
        coord_fixed = coord_fixed,
        x_to_y_ratio = x_to_y_ratio
      )
      
      # Combine plots
      combined_plot <- overlay_plots(branches_all, tips_all, branches_matching, tips_matching)
      return(combined_plot)
    }, label = "plot_tree")

    # Plot t-SNE scatterplot
    plot_tsne <- eventReactive({url_change_trigger()}, {
      # Load full plot
      plot_all <- load_data("plot_tsne_all")
      
      # Load layout for matching plot
      layout <- load_data("layout_tsne")
      
      # Get data
      data <- get_results()$filter_data
      
      # Check required conditions
      req(!is.null(data))
      
      # Format layout with taxonomy and colors
      layout <- add_taxonomy_to_layout(layout = layout, layout_ID = "IMG_Genome_ID_max_genes", # debug
                                       taxonomy = data, taxonomy_ID = "IMG Genome ID max quality")
      layout <- add_fill_to_layout(layout = layout, group = "Phylum", lighten_amount = 0.2)
      layout <- add_color_to_layout(layout = layout, group = "Phylum", lighten_amount = 0)
      
      # Create matching plot
      plot_matching <- plot_scatterplot(
        df = layout,
        color = layout$color,
        fill = layout$fill,
        stroke = 1,
        size = 5,
        shape = "circle",
        alpha = 1,
        label = c("Phylum", "Class", "Order", "Family", "Genus", "Species"),
        ticklen.x = 4, ticklen.y = 4,
        showticklabels.x = TRUE, showticklabels.y = TRUE,
        title.x = "Dimension 1", title.y = "Dimension 2",
        coord_fixed = TRUE, x_to_y_ratio = 1
      )
      
      # Overlay plots
      plot_combined <- overlay_plots(plot_all, plot_matching)
      
      return(plot_combined)
    }, label = "plot_tsne")
    
    # --- Update user interface (UI) elements ---
    # Update UI after loading module
    observeEvent({tab_selected_trigger()}, {
      
      # Update query builder
        update_query_builder(inputId = "query_builder", choices = choices_traits_search)
  
      # Update variable to display
        update_variable_to_display_search(session = session)
      
      # Update tree layout
        update_tree_layout_search(session = session)
  
      # Update checkboxes for organisms
        update_info_organism_search(session = session)
  
      # Update checkboxes for databases
        update_info_databases_search(session = session)
        
      # Update checkboxes for metabolism
        update_info_metabolism_search(session = session)
  
      # Update checkboxes for traits
        update_info_traits_search(session = session)
        
      # Signal user interface has been updated
        ui_ready(TRUE)
        
    },
    label="update_UI_after_loading")
  
    # Update UI after computing results
    observeEvent(url_change_trigger(), {
      # Update (reset) spinners for plots and tables 
      reset_spinners()
    },
    label="update_UI_after_results")
    
    # Update UI after user changes input
    # No logic for this module
    
    # Make other updates
    # Signal module is ready (hide loading screen)
    hide_loading_screen(
      trigger = shiny::reactive({
        isTRUE(ui_ready()) &&
          !is.null(input$query_builder_ready)
      }),
      on_ready = on_ready
    )

    
    # --- Generate outputs ---
    # Output number of matching organisms
    output$summary_text <- shiny::renderText({
      # Get data
      data <- get_results()$filter_data
      
      # Check required conditions
      req(!is.null(data))
      
      # Create summary
      paste0("Query matched ", nrow(data), " organisms")
    })
       
    # Create output flags
    flag_if_not_null(output, "flag_results", trigger = url_change_trigger, 
         value_fun = function() get_results())
    
    
    # Output message before results are available
    output$job_status <- render_job_status(
      tab_name      = "databaseSearch",
      empty_message = "Please make selections at left"
    )
    
    # Output overview plots
    # shiny::observeEvent({list(url_change_trigger(), input$variable_to_display)},
    # {
      # Treemap plot
      output$treemap_plot <- plotly::renderPlotly({
        # Get data
        df <- get_results()$filter_data
        var_name <- input$variable_to_display
        
        # Check required conditions
        req(!is.null(df))
        req(!is.null(var_name))
        
        # Load plot config
        cfg_treemap <- get_plot_config("databaseSearch", "treemap")
        
        df <- search_results_to_plot(df = df, plot_type = "treemap", var_name = var_name)
        
        plot_treemap(df,
                     hovertemplate = build_hovertemplate(cfg_treemap$hovertemplate, var_name))
      })
      
      # Tree plot
      output$tree_plot = plotly::renderPlotly({
        plot = plot_tree()
      })
      
      # t-SNE plot
      output$tsne_plot <- plotly::renderPlotly({
         plot = plot_tsne()
      })

      # Output table with matching organisms and columns
      output$table <- DT::renderDataTable({
        table = build_table()
      }, escape = FALSE, options = list(scrollX = TRUE))
      
      # Output downloadable csv with matching results
      output$download_data <- create_download_handler(
        filename_prefix = "results",
        data_source = function() {
          # Get data
          data <- get_results()$filter_data
          
          # Check required conditions
          req(!is.null(data))
          
          # Remove link columns
          data |>
            dplyr::select(-dplyr::ends_with("link"))
        }
      )
    # })
    })
  }
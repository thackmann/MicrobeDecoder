# Define the History Module in Shiny App
# This script defines the user interface (UI) and server for the 
# prediction history module.
# Author: Timothy Hackmann
# Date: 10 April 25

# === Define user interface (UI) ===
historyUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
      # Title
      create_title_div("Prediction history"),
      
      # Main content area
      bslib::card(
        bslib::card_header("Past Jobs"),
        
        # Table and buttons
          # Scrollable table
          div(
            create_data_table(inputId = ns("job_table"))
          ),
          
        # Action buttons
        div(
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(
              ns("refresh_history"), "Refresh jobs", class = "btn btn-primary"
            ),
            actionButton(
              ns("select_all_toggle"), "Select all jobs", class = "btn btn-secondary"
            ),
            actionButton(
              ns("delete_selected"), "Delete selected jobs", class = "btn btn-danger"
            )
          )
          # div( # uncomment to activate ability to view all users
          #   shiny::conditionalPanel(
          #     condition = "output.flag_localhost",
          #     ns = ns,
          #     actionButton(ns("view_all_users"), "View all users", class = "btn btn-primary")
          #   )
          # )
        )
      )
  )
}


# === Define server ===
historyServer <- function(id, selected_tab, on_ready, show_all_users = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
  # --- Set namespace ---
  ns <- session$ns

  # --- Run Javascript functions ---
  shinyjs::runjs(sprintf("registerJobTableHandlers('%s');", ns("job_table")))
  
  # --- Define reactive values ---
  ui_ready <- reactiveVal(FALSE) # For storing status of user interface (UI)
  select_all_state <- reactiveVal(TRUE) # Value for toggling checkboxes
  table_records <- reactiveVal(NULL) # For storing the most recent job records
  table_rendered <- reactiveVal(NULL) # For storing the most recent version of the table
  status_cache <- new.env(parent = emptyenv()) # Cache of parsed job records keyed by file path
  # show_all <- reactiveVal(FALSE) # uncomment to activate ability to view all users
  
  # --- Define triggers for reactive expressions ---
  tab_selected_trigger <- make_tab_trigger(selected_tab, "history")
  tab_loaded_trigger <- make_tab_trigger(selected_tab, "history")
  manual_trigger <- make_manual_trigger()
  regenerate_table_trigger <- manual_trigger$trigger
  trigger_reexecute <- manual_trigger$reexecute
  
  # --- Handle user actions ---
    # Refresh the History table manually
    observeEvent(input$refresh_history, {
      trigger_reexecute()
    })

    # Ask browser which jobs (table rows) are selected
    observeEvent(input$delete_selected, {
      session$sendCustomMessage(ns("job_table_get_selected"), list())
    })
    
    # Delete selected jobs in table
    observeEvent(input$job_table_selected_jobs, {
      # Get IDs of jobs to delete
      selected_ids <- input$job_table_selected_jobs
      
      # Determine where to look for jobs
      if (show_all_users) {
        search_dir <- "jobs"
      } else {
        search_dir <- file.path("jobs", session$userData$user_id())
      }
      
      # Delete all files for selected jobs
      for (id in selected_ids) {
        matches <- list.files(
          search_dir, recursive = TRUE, full.names = TRUE,
          pattern = paste0("^", id, "(\\.status\\.rds|\\.progress\\.rds|\\.cancel\\.rds|_inputs\\.rds|\\.rds)$")
        )
        file.remove(matches)
      }
      
      # Refresh table
      trigger_reexecute()
    })
    
    # Cancel requested jobs
    observeEvent(input$cancel_job, {
      # Get IDs of jobs to cancel
      job_id <- input$cancel_job
      
      # Get matching job
      records <- table_records()
      if (is.null(records) || nrow(records) == 0) {
        return(invisible(NULL))
      }
      
      row <- records[records$job_id == job_id, , drop = FALSE]
      if (nrow(row) == 0) {
        return(invisible(NULL))
      }
      
      # Determine if job can be cancelled
      if (!row$status[1] %in% c("submitted", "running")) {
        trigger_reexecute()
        return(invisible(NULL))
      }
      
      # Write request for cancellation
      job_dir <- file.path("jobs", row$user_id[1], row$tab[1])
      request_job_cancel(
        job_dir      = job_dir,
        job_id       = job_id,
        requested_by = session$userData$user_id()
      )
      
      # Refresh table
      trigger_reexecute()
    })

  # --- Refresh cache ---
    # Refresh records when triggered by a user action. The initial tab load is
    # handled by the timer observer below so opening History causes only one scan.
    observeEvent(regenerate_table_trigger(), {
      recs <- read_history_records(
        session         = session,
        show_all_users  = show_all_users,
        cache           = status_cache
      )
      table_records(recs)
    },
    ignoreInit = TRUE, label = "refresh_records_on_trigger")
    
    # Refresh records more often while jobs are active
    observe({
      if (!identical(selected_tab(), "history")) {
        return(NULL)
      }

      recs <- read_history_records(
        session         = session,
        show_all_users  = show_all_users,
        cache           = status_cache
      )
      table_records(recs)

      has_active_jobs <- !is.null(recs) &&
        nrow(recs) > 0 &&
        any(recs$status %in% c("submitted", "running"))

      refresh_interval <- if (has_active_jobs) 1000L else 5000L
      shiny::invalidateLater(refresh_interval, session = session)
    },
    label = "refresh_records_on_timer")
    
    # # Show all users # uncomment to activate ability to view all users
    # observeEvent(input$view_all_users, {
    #   show_all(!show_all())
    #   trigger_reexecute()
    # })  
    
  # --- Update user interface (UI) elements ---
    # Update UI after loading module
    observeEvent(tab_selected_trigger(),
    {
      ui_ready(TRUE)
    },
    label="update_UI_after_loading")
    
    
    # Update UI after computing results
    # No logic for this module
    
    # Update UI after user changes input
    # No logic for this module
    
    # Make other updates
      # Update checkboxes
      observeEvent(input$select_all_toggle, {
        session$sendCustomMessage(ns("job_table_toggle_checkboxes"), select_all_state())
        select_all_state(!select_all_state())
        updateActionButton(session, "select_all_toggle",
                           label = if (select_all_state()) "Select All" else "Deselect All")
      })
      
      # Update table
      observe({
        # Build the updated table
        df <- build_history_table(
          records         = table_records(),
          session         = session,
          show_all_users  = show_all_users
        )
        
        # Display the update if table is different from that currently rendered
        if (!identical(df, shiny::isolate(table_rendered()))) {
          table_rendered(df)
          DT::replaceData(DT::dataTableProxy("job_table"), df,
                          resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
        }
      },
      label = "update_table")
      
      # Signal module is ready (hide loading screen)
      hide_loading_screen(
        trigger  = ui_ready,
        on_ready = on_ready
      )
    
  # --- Generate outputs ---
  # Create output flags
  flag_if_not_null(output, "flag_localhost", trigger = reactive(TRUE), 
                   value_fun = function() is_running_locally())
  
  # Output table
  output$job_table <- DT::renderDataTable({
    df0 <- shiny::isolate(
      build_history_table(
        records         = table_records(),
        session         = session,
        show_all_users  = show_all_users
      )
    )
    DT::datatable(
      df0,
      escape = FALSE, selection = "none", rownames = FALSE,
      options = list(
        pageLength = 10,
        order = list(list(5, 'desc')),
        language = list(emptyTable = "No jobs found.")
      )
    )
  })
  })
}
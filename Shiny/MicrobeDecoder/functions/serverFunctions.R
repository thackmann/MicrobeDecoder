#' Server Functions for App
#' 
#' This script defines functions for the server function of
#' of the Shiny app. These functions modularize common elements to improve 
#' readability, maintainability, and consistency across modules.
#' 
#' @author Timothy Hackmann
#' @date 9 Mar 2025

# === Reactive flags ===
  #' Create a reactive output flag that returns TRUE if value is not NULL
  #'
  #' This helper is typically used to define `output$flag_*` values for controlling
  #' visibility of UI elements via `shiny::conditionalPanel()`. It assigns a reactive
  #' expression to `output[[output_id]]` that returns TRUE if `value_fun()` is not NULL.
  #'
  #' @param output The Shiny output object. Defaults to the current reactive domain's output.
  #' @param output_id The name of the output (as string) to assign to.
  #' @param trigger A reactive expression used to trigger reevaluation.
  #' @param value_fun A function that returns the value to test for NULL.
  #' @param label Optional label for debugging (used in `eventReactive`).
  #' @return None (side effect: assigns to output)
  #'
  #' @examples
  #' flag_if_not_null(output, "flag_results", trigger = make_trigger, value_fun = get_results)
  #'
  #' # In UI:
  #' # conditionalPanel("output.flag_results", ...)
  flag_if_not_null <- function(output, output_id, trigger, value_fun, label = NULL) {
    output[[output_id]] <- shiny::eventReactive(trigger(), {
      !is.null(value_fun())
    }, label = label %||% output_id)
    
    shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
  }
  
  #' Create a reactive output flag that returns TRUE if value has multiple elements or columns
  #'
  #' This helper is typically used to define `output$flag_*` values for controlling
  #' visibility of UI elements via `shiny::conditionalPanel()`. It assigns a reactive
  #' expression to `output[[output_id]]` that returns TRUE if `value_fun()` has more
  #' than one element (or more than one column, if a data.frame or matrix).
  #'
  #' @param output The Shiny output object. Defaults to the current reactive domain's output.
  #' @param output_id The name of the output (as string) to assign to.
  #' @param trigger A reactive expression used to trigger reevaluation.
  #' @param value_fun A function that returns a vector, list, or data.frame.
  #' @param label Optional label for debugging (used in `eventReactive`).
  #' @return None (side effect: assigns to output)
  #'
  #' @examples
  #' flag_if_multiple(output, "flag_multiple_organisms", trigger = make_trigger, value_fun = get_gene_functions)
  #'
  #' # In UI:
  #' # conditionalPanel("output.flag_multiple_organisms", ...)
  flag_if_multiple <- function(output, output_id, trigger, value_fun, label = NULL) {
    output[[output_id]] <- shiny::eventReactive(trigger(), {
      val <- value_fun()
      
      if (length(val) > 1) {
        return(TRUE)
      } else if (length(val) == 1 && (is.data.frame(val) || is.matrix(val))) {
        return(ncol(val) > 1)
      } else {
        return(FALSE)
      }
    }, label = label %||% output_id)
    
    shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
  }

# === Job status outputs ===
  #' Create the message shown before job results are available
  #'
  #' This function creates the message shown when a saved computation job does
  #' not yet have results to display.
  #'
  #' @param status Character job status.
  #' @param percent Numeric progress from 0 to 100, or \code{NA} when not known.
  #' @param error_message Character error message for a failed job, or \code{NA}.
  #' @return A Shiny \code{div} containing the job message.
  #' @export
  create_job_status_message <- function(status,
                                        percent = NA_real_,
                                        error_message = NA_character_) {
    status <- status %||% "missing"
    
    message <- switch(
      status,
      submitted   = c("Your results are not ready yet",
                      "Your job has been submitted and is waiting to start."),
      running     = c("Your results are not ready yet",
                      if (is.na(percent)) {
                        "Your job is still running."
                      } else {
                        sprintf("Your job is still running (%d%% done).",
                                max(0, min(100, round(percent))))
                      }),
      error       = c("This job did not finish",
                      "The job stopped with an error before producing results. You can submit it again."),
      cancelled   = c("This job was cancelled",
                      "Because the job did not finish, there are no results to show."),
      interrupted = c("This job was interrupted",
                      "The job stopped when clicking the back button on the browser or because the app restarted. Please submit it again."),
      completed   = c("Your results are ready",
                      "The job has finished."),
      missing     = c("Job results cannot be found",
                      "The job may have been deleted or expired."),
      c("Job results are not available",
        "These results are not available yet.")
    )
    
    refreshable <- !(status %in% c("error", "cancelled", "interrupted"))
    refresh_label <- if (identical(status, "completed")) {
      "Refresh to view your results."
    } else {
      "Refresh to check again."
    }
    
    div(
      class = "job-status-message",
      shiny::h4(message[[1]]),
      shiny::p(message[[2]]),
      if (refreshable) {
        shiny::p(shiny::tags$a(href = "javascript:window.location.reload();",
                               refresh_label))
      },
      if (identical(status, "error") &&
          !is.na(error_message) && nzchar(error_message)) {
        shiny::p(
          class = "text-muted",
          style = "margin-top:8px; font-size:0.85em; word-break:break-word; max-width:60ch;",
          error_message
        )
      }
    )
  }
  
  #' Render the message shown before job results are available
  #'
  #' This helper shows the module's normal starting message when the URL does not
  #' name a job. When a job is named but its result is not available, it reads the
  #' status file and shows the matching job message.
  #'
  #' @param tab_name Name of the module tab.
  #' @param empty_message Message shown before a job has been selected.
  #' @param session The Shiny session object.
  #' @return A Shiny UI renderer.
  #' @export
  render_job_status <- function(tab_name,
                                empty_message,
                                session = shiny::getDefaultReactiveDomain()) {
    shiny::renderUI({
      job_id <- get_query_param(session = session, param_name = "job")
      
      if (is.null(job_id)) {
        return(shiny::h4(empty_message))
      }
      
      user_id <- get_query_param(session = session, param_name = "user")
      job_dir <- get_job_dir(
        tab     = tab_name,
        user_id = user_id,
        session = session
      )
      
      record <- read_job_status(get_status_filepath(job_dir, job_id))
      
      if (is.null(record)) {
        return(create_job_status_message(status = "missing"))
      }
      
      create_job_status_message(
        status        = label_job_status(record),
        percent       = suppressWarnings(
          as.numeric(record$percent %||% NA_real_)
        ),
        error_message = record$error_message %||% NA_character_
      )
    })
  }

# === Reactive triggers ===
  #' Create a reactive trigger based on one or more expressions
  #'
  #' This function is usually used to define a trigger for reactive outputs or observers.
  #'
  #' @param ... Expressions to track. Can be individual inputs or expressions.
  #' @return A reactive expression (used to trigger updates)
  #' @examples
  #' my_trigger <- make_trigger(input$go_button, input$some_setting)
  make_trigger <- function(...) {
    exprs <- rlang::enquos(...)
    
    reactive({
      lapply(exprs, function(expr) rlang::eval_tidy(expr))
    })
  }
  
  #' Trigger when an action button is clicked
  #'
  #' @param button_id The ID of the action button.
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param baseline The baseline click count to compare against. The trigger fires when the button's click count exceeds this value. Default is `0L`.
  #' @return A reactive expression that triggers after clicking the action button
  make_action_button_trigger <- function(button_id, input = getDefaultReactiveDomain()$input,
                                         baseline = 0L) {
    make_trigger(req((input[[button_id]] %||% 0L) > baseline))
  }
  
  #' Trigger when a specific tab is selected
  #'
  #' This function creates a reactive trigger that activates only when the currently
  #' selected tab matches a specified tab name. It is useful for delaying computations
  #' or outputs until a particular tab is in view.  An option is available to make
  #' the trigger also depend on a specified input (e.g., input$query_builder_valid).
  #'
  #' @param tab_fn A reactive expression that returns the name of the currently selected tab.
  #' @param tab_name A character string specifying the name of the tab to trigger on.
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param input_id A character string specifying the input to depend on (e.g., `"query_builder_valid"`, `"selected_organisms"`). When provided, the trigger also fires when this input changes.
  #' @return A reactive expression that returns TRUE when the tab matches `tab_name`,
  #'   optionally gated on a new event from `input[[input_id]]`. Returns NULL when
  #'   the selected tab does not match `tab_name`, which prevents downstream
  #'   `observeEvent` calls from firing.
  #'
  #' @examples
  #' # Trigger only when 'history' is selected
  #' tab_trigger <- make_tab_trigger(selected_tab, "history")
  #'
  #' # Trigger when 'databaseSearch' is selected and query builder has finished rendering
  #' tab_loaded_trigger <- make_tab_trigger(
  #'   selected_tab, "databaseSearch", input, "query_builder_valid"
  #' )
  #'
  #' # Trigger when 'predictionsMachineLearning' is selected and organism selectize has populated
  #' tab_loaded_trigger <- make_tab_trigger(
  #'   selected_tab, "predictionsMachineLearning", input, "selected_organisms"
  #' )
  #'
  #' @export
  make_tab_trigger <- function(tab_fn = selected_tab, tab_name, input = NULL, input_id = NULL) {
    if (!is.null(input) && !is.null(input_id)) {
      reactive({
        if (tab_fn() != tab_name) return(NULL)
        TRUE
      }) |> bindEvent(input[[input_id]])
    } else {
      reactive({
        if (tab_fn() != tab_name) return(NULL)
        TRUE
      })
    }
  }
  
  #' Create a reactive trigger based on a query parameter and active tab in the URL
  #'
  #' This creates a reactive expression that returns TRUE when a given query parameter (e.g., ?job=xyz)
  #' is present and the tab in the URL matches the expected tab name.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param param_name The query parameter to look for (default = "job")
  #' @param tab_name Optional tab name to match against the `?tab=...` value in the URL.
  #' @return A reactive expression that returns TRUE when both the query parameter is set and the tab matches
  #' @export
  make_url_trigger <- function(session = getDefaultReactiveDomain(), param_name = "job", tab_name = NULL) {
    reactive({
      query <- parseQueryString(session$clientData$url_search)

      value <- query[[param_name]]
      tab   <- query$tab

      # req(value)
      # req(tab)
      
      if (is.null(value) || value == "") return(NULL)
      if (!is.null(tab_name) && tab != tab_name) return(NULL)

      return(TRUE)
    })
  }
  
  #' Create a manually-fired reactive trigger
  #'
  #' This function returns a `trigger`/`reexecute` pair that lets you manually fire
  #' a reactive expression. The `trigger` is a reactive expression suitable for
  #' passing to `eventReactive()` or similar; calling `reexecute()` causes
  #' anything depending on `trigger` to re-evaluate. Implemented via a
  #' `reactiveVal` counter that is incremented on each manual fire.
  #'
  #' @return A list with two elements:
  #'   \item{trigger}{A reactive expression that invalidates when `reexecute()` is called.}
  #'   \item{reexecute}{A function that, when called, fires the trigger.}
  #' @export
  make_manual_trigger <- function() {
    counter <- reactiveVal(0)
    list(
      trigger = reactive({ counter(); TRUE }),
      reexecute = function() counter(counter() + 1)
    )
  }
  
  #' Create a general-purpose reactive trigger
  #'
  #' A wrapper around make_trigger() for naming consistency.
  #' Use this when the trigger doesn't fit other named categories (e.g., tab or button triggers).
  #'
  #' @inheritParams make_trigger
  #' @return A reactive expression (used to trigger updates)
  #' @export
  make_other_trigger <- function(...) {
    make_trigger(...)
  }
  
  #' Create a reactive trigger that fires when any of two triggers activate
  #'
  #' This function returns a reactive trigger that invalidates whenever
  #' either `trigger1` or `trigger2` fires. It uses a reactiveVal-based counter
  #' that increments on each event, forcing reactivity.
  #'
  #' @param trigger1 A reactive expression (e.g. an eventReactive or reactive)
  #' @param trigger2 A second reactive expression.
  #' @param label Optional label for debugging/logging.
  #' @return A reactive expression that returns an incrementing counter
  #'
  #' @examples
  #' combined_trigger <- make_combined_trigger(tab_selected_trigger, make_predictions_trigger)
  #' observeEvent(combined_trigger(), {
  #'   cat("Triggered by either source\n")
  #' })
  #'
  #' @export
  or_trigger <- function(trigger1, trigger2, label = NULL) {
    counter <- reactiveVal(NULL)  # Start with NULL
    
    observeEvent(trigger1(), {
      counter(if (is.null(counter())) 1 else counter() + 1)
    })
    
    observeEvent(trigger2(), {
      counter(if (is.null(counter())) 1 else counter() + 1)
    })
    
    reactive({
      counter()
    })
  }

# === Update User Interface (UI) Elements ===
  #' Update Query Builder Filters
  #' 
  #' This function loads query filters, filters them based on the selected variable list, 
  #' and updates the query builder input. It includes a brief delay 
  #' to ensure UI elements are fully updated before the builder is updated (otherwise
  #' the update may not be successful).
  #' 
  #' @param inputId A character string specifying the ID of the query builder input.
  #' @param choices A character vector of variable names to filter the query builder filters.
  #' @param setRules A list of rules apply to the query builder.
  #' @param delay_time An optional delay in milliseconds before applying the update (default: 250 ms).
  #' 
  #' @return Updates the query builder input dynamically.
  #' 
  #' @examples
  #' update_query_builder("query_builder", choices_traits_taxonomy)
  update_query_builder <- function(inputId, choices, setRules = NULL, delay_time = 250) {
    filters <- load_data("query_filters")
    filters <- purrr::keep(filters, ~ .x$id %in% choices)
    
    shinyjs::delay(delay_time, {
      jqbr::updateQueryBuilder(
        inputId    = inputId,
        setFilters = filters,
        setRules   = setRules
      )
    })
  }
  
  #' Update Selectize Input
  #'
  #' This function updates a Selectize input in a Shiny application. It dynamically sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param inputId A character string specifying the ID of the Selectize input to update.
  #' @param choices A character vector of choices to populate the Selectize input.
  #' @param selected A character vector specifying the default selected choice(s).
  #' @param server A logical indicating whether to use server-side processing (default: TRUE).
  #'
  #' @return Updates the specified Selectize input dynamically.
  #'
  #' @examples
  #' update_select_input(session, "selected_organisms", choices = c("Option 1", "Option 2"))
  update_select_input <- function(session = getDefaultReactiveDomain(), 
                                  inputId, choices = NULL, selected = NULL, server = TRUE) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)

    shiny::updateSelectizeInput(session, inputId = inputId, choices = choices, selected = selected, server = server)
  }
  
  #' Update Picker Input
  #'
  #' This function updates a Picker input (shinyWidgets) in a Shiny application. It sets
  #' the available choices and selects a default value.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param inputId A character string specifying the ID of the Picker input to update.
  #' @param choices A character vector of choices to populate the Picker input.
  #' @param selected A character vector specifying the default selected choice(s).
  #' @param choicesOpt An optional list of options for the choices (e.g., icons, subtext).
  #'
  #' @return Updates the specified Picker input dynamically.
  #'
  #' @examples
  #' update_picker_input(session, "variable_to_display", choices = c("Phylum", "Genus"))
  update_picker_input <- function(session = getDefaultReactiveDomain(), 
                                  inputId, choices = NULL, selected = NULL,
                                  choicesOpt = NULL) {
    if (is.null(choices)) choices <- character(0)
    if (is.null(selected)) selected <- head(choices, 1)
    
    shinyWidgets::updatePickerInput(session, inputId = inputId, choices = choices, 
                                    selected = selected, choicesOpt = choicesOpt)
  }
  
  #' Update Text Input
  #'
  #' This function updates a text input field in a Shiny application. It sets the value
  #' of the input dynamically during a session.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param inputId A character string specifying the ID of the text input to update.
  #' @param value A character string specifying the new value for the input.
  #' @param placeholder Optional. A character string for placeholder text if needed.
  #'
  #' @return Updates the specified text input dynamically.
  #'
  #' @examples
  #' update_text_input(session, "network_name_display", value = "Custom network")
  update_text_input <- function(session = getDefaultReactiveDomain(),
                                inputId,
                                value = "",
                                placeholder = NULL) {
    shiny::updateTextInput(session = session, inputId = inputId, value = value, placeholder = placeholder)
  }
  
  #' Update Checkbox Group Input
  #'
  #' This function updates a checkbox group input in a Shiny application. It dynamically sets
  #' the available choices and selects default values.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param inputId A character string specifying the ID of the checkbox group input to update.
  #' @param choices A named list or character vector of choices to populate the checkbox group.
  #' @param selected A character vector specifying the default selected choices. Defaults to `NULL`.
  #' @return Updates the specified checkbox group input dynamically.
  #'
  #' @examples
  #' update_checkbox_group(session, "info_organism", choices = c("Genus", "Species"))
  update_checkbox_group <- function(session = getDefaultReactiveDomain(), 
                                    inputId, choices = NULL, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    
    shiny::updateCheckboxGroupInput(session, inputId = inputId, choices = choices, selected = selected)
  }
 
# === Spinners ===
  #' Let the spinners in a module fire again
  #'
  #' This function clears the mark that keeps a spinner hidden after its output
  #' has rendered once.  Call it when a new job is loaded so the spinners show
  #' again while the new results are drawn.
  #'
  #' @param session The Shiny session object. Defaults to the current session.
  #' @return None (side effect: sends a message to the browser)
  #'
  #' @examples
  #' reset_spinners()
  reset_spinners <- function(session = getDefaultReactiveDomain()) {
    session$sendCustomMessage("resetSpinners", list(prefix = session$ns("")))
  }

# === Loading screen ===
  #' Hide the loading screen
  #'
  #' This function hides the loading screen for the app.  It is triggered when 
  #' the module is ready. It waits until the trigger returns `TRUE`, then calls 
  #' `on_ready()` once.
  #'
  #' @param trigger A reactive expression that returns `TRUE` when the module is
  #'   ready.
  #' @param on_ready A function called when the module is ready.
  #' @param label Optional label for the observer (used for debugging). Defaults
  #'   to `"hide_loading_screen"`.
  #'
  #' @return A Shiny observer (called for its side effect).
  #'
  #' @examples
  #' hide_loading_screen(
  #'   trigger  = ui_ready,
  #'   on_ready = on_ready
  #' )
  hide_loading_screen <- function(trigger,
                                  on_ready,
                                  label = "hide_loading_screen") {
    shiny::observeEvent(
      {
        shiny::req(trigger())
        TRUE
      },
      {
        on_ready()
      },
      once = TRUE,
      label = label
    )
  }
  
# === Modals ===
  #' Create a Shiny Modal with Download Links
  #'
  #' This function shows a modal with a list of download links.  An optional help
  #' link points the user to detailed guidelines on the Help page.
  #'
  #' @param title The title of the modal.
  #' @param links A list of links, as in the `example_file_links` catalog.
  #' @param help_panel The Help panel to open when the link is clicked (matches a navset_pill value).
  #'   When `NULL`, no help link is shown.
  #' @return None. The function shows the modal as a side effect.
  #' @export
  create_download_modal <- function(title = "Example files", links, help_panel = NULL) {
    # Build optional link to guidelines on the Help page
    guidelines <- if (!is.null(help_panel)) {
      htmltools::div("Click ",
                     htmltools::tags$a(
                       href = "#",
                       onclick = sprintf("shinyjs.goToHelpPanel('%s'); return false;", help_panel),
                       "here"
                     ),
                     " to see detailed guidelines."
      )
    }

    shiny::showModal(shiny::modalDialog(
      shiny::h3(title),
      links,
      guidelines,
      easyClose = TRUE, footer = NULL
    ))
  }

  #' Create a Shiny Modal with an Error Message
  #'
  #' This function creates a modal dialog with an error message.
  #'
  #' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
  #' @param title The title of the modal. Default is `"Download Unavailable"`.
  #' @param message The error message text to display.
  #' @return None. The function shows the modal as a side effect.
  #' @export
  create_error_modal <- function(ns = identity,
                             title = "Download Unavailable",
                             message = "Data is not available to download.") {
    shiny::showModal(
      shiny::modalDialog(
        shiny::h3(title),
        htmltools::div(
          HTML(message)
        ),
        easyClose = TRUE,
        footer = NULL
      )
    )
  }
  
  #' Output Modal for Missing Files
  #'
  #' This function creates a Shiny observer that triggers a modal with an error message
  #' when a specific input is activated (e.g., clicking a faux download button).
  #'
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param input_id The ID of the input that triggers the modal (e.g., `"null_download"`).
  #' @param title The title to display in the modal dialog. Default is `"Download Unavailable"`.
  #' @param message The body text to show in the modal dialog.
  #' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
  #' @param label An optional label for the observer, useful for debugging.
  #' @return A Shiny observer that shows an error modal when the input is triggered.
  #' @export
  output_missing_files_modal <- function(input = getDefaultReactiveDomain()$input, 
                                         input_id,
                                         title = "Data Unavailable",
                                         message = "Data is not available to download.",
                                         ns = identity,
                                         label = NULL) {
    observeEvent(input[[input_id]], {
      # Set inputs and outputs
      create_error_modal(ns = ns, title = title, message = message)
    }, label = label)
  }
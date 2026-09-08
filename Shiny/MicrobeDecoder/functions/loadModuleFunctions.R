# Functions for Loading App Modules
# This script contains functions for loading Shiny modules when their tabs are
# opened.
# These functions prepare empty module containers, open modules on demand, close
# temporary modules, and notify the browser when a module is ready.
# Author: Timothy Hackmann
# Date: 16 May 2026

# === Timing ===
  # Stores simple timing records for opening and closing modules.
  .module_timings <- new.env(parent = emptyenv())

  #' Record a timing for a module operation
  #'
  #' Saves one timing record into `.module_timings`.
  #'
  #' @param tab_name A character string. The name of the tab.
  #' @param op Character. The operation that was timed (`"open"` or `"close"`).
  #' @param ms Numeric. The elapsed time in milliseconds.
  #'
  #' @return None (side effect: writes to `.module_timings`).
  .record_timing <- function(tab_name, op, ms) {
    entry <- list(tab = tab_name, op = op, ms = ms,
                  at  = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"))
    key <- paste0(tab_name, "::", op)
    existing <- if (exists(key, envir = .module_timings, inherits = FALSE))
      get(key, envir = .module_timings) else list()
    existing[[length(existing) + 1L]] <- entry
    assign(key, existing, envir = .module_timings)
  }

  #' Time a module operation
  #'
  #' Runs `expr` and records the wall-clock time it took.  Used to wrap
  #' the body of `open_module()` and `close_module()` so each call's
  #' duration is recorded.
  #'
  #' @param tab_name A character string. The name of the tab.
  #' @param op Character. The operation being timed (`"open"` or `"close"`).
  #' @param expr An expression to evaluate.
  #'
  #' @return The value of `expr` (called for its side effects).
  .time_op <- function(tab_name, op, expr) {
    t0 <- Sys.time()
    on.exit({
      ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      .record_timing(tab_name, op, ms)
    })
    force(expr)
  }

# === Loading and unloading modules ===
  # Opens modules when their tabs are selected and closes temporary modules
  # when their tabs are left.
  .destroyable_cache <- new.env(parent = emptyenv())
  
  #' Notify that a module is ready
  #'
  #' Hides the module loading screen and reveals the module content.
  #'
  #' @param tab_name A character string. The name of the tab/module being revealed. Used to construct the loading screen ID (`<tab_name>-loading`) and module container.
  #' @return None (side effect: runs JavaScript to toggle visibility).
  #'
  #' @examples
  #' # notify_module_ready("predictionsTaxonomy")
  notify_module_ready <- function(tab_name) {
    loading_id <- paste0(tab_name, "-loading")
    content_id <- paste0(tab_name, "-content")
    shinyjs::runjs(sprintf(
      "shinyjs.hide('%s'); shinyjs.show('%s');
       if (window.notifyModuleReady) window.notifyModuleReady('%s');",
      loading_id, content_id, tab_name
    ))
  }

  #' Initialize a module container
  #'
  #' Creates an empty container and loading screen for a module before the
  #' module itself is loaded.
  #'
  #' @param tab_name A character string. The name of the tab. Used to construct the container ID (`<tab_name>-init`) and the loading screen ID.
  #' @return A `shiny::div` containing the loading screen, ready to be placed
  #'   inside a `nav_panel()`.
  #'
  #' @examples
  #' # Inside a nav_panel:
  #' # bslib::nav_panel(
  #' #   value = "predictionsTaxonomy",
  #' #   title = "From taxonomy",
  #' #   make_empty_module("predictionsTaxonomy")
  #' # )
  initialize_module <- function(tab_name) {
    shiny::div(
      id = paste0(tab_name, "-init"),
      create_loading_screen(paste0(tab_name, "-loading"))
    )
  }
  
  #' Open a module
  #'
  #' Inserts the module UI, starts the module server, and reveals the module when
  #' it is ready. The module server is wrapped so its observers can be cleaned up
  #' later if the module is temporary.
  #'
  #' @param tab_name A character string. The name of the tab to open.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param loaded A `reactiveValues` object tracking which tabs have been loaded. Set to `TRUE` for `tab_name` after the module is opened.
  #' @param selected_tab A reactive expression that returns the currently
  #'   selected tab.
  #' @param module_registry A named list describing each module's UI function, server function, and extra arguments. Built in `variables/variablesTemp.R`.
  #' @return None (side effects: inserts UI, calls server function,
  #'   updates `loaded`, records a timing).
  #'
  #' @examples
  #' # Inside setup_module_loading()
  #' # open_module(input$tabs, session, loaded, selected_tab, module_registry)
  open_module <- function(tab_name, session, loaded, selected_tab, module_registry) {

    if (isTRUE(loaded[[tab_name]])) {
      # Hide the app loading screen when entering an already loaded tab.
      notify_module_ready(tab_name)
      return(invisible())
    }

    spec <- module_registry[[tab_name]]
    if (is.null(spec)) {
      warning("open_module: no registry entry for tab '", tab_name, "'")
      return(invisible())
    }

    .time_op(tab_name, "open", {

      # Insert the module UI.
      shiny::insertUI(
        selector  = paste0("#", tab_name, "-init"),
        where     = "beforeEnd",
        ui        = shinyjs::hidden(
          shiny::div(
            id = paste0(tab_name, "-content"),
            get(spec$ui_fn)(tab_name)
          )
        ),
        immediate = TRUE
      )

      # Build or retrieve the destroyable server function.
      if (!exists(tab_name, envir = .destroyable_cache, inherits = FALSE)) {
        server_fn <- get(spec$server_fn)
        assign(tab_name,
               shiny.destroy::makeModuleServerDestroyable(server_fn),
               envir = .destroyable_cache)
      }
      destroyable_fn <- get(tab_name, envir = .destroyable_cache)

      # Assemble the server argument list.
      call_args <- list(id = tab_name)
      if ("selected_tab" %in% spec$needs) call_args$selected_tab <- selected_tab
      if ("on_ready"     %in% spec$needs) call_args$on_ready     <- function() notify_module_ready(tab_name)

      # Start the module server.
      do.call(destroyable_fn, call_args)

      if (!("on_ready" %in% spec$needs)) {
        notify_module_ready(tab_name)
      }

      loaded[[tab_name]] <- TRUE
    })
  }

  #' Close a module
  #'
  #' Cleans up a temporary module when its tab is left. This destroys the
  #' module's observers, removes its content from the page, and restores the
  #' loading screen for the next visit.
  #'
  #' @param tab_name A character string. The name of the tab to close.
  #' @param loaded A `reactiveValues` object tracking which tabs have been loaded. Set to `FALSE` for `tab_name` after the module is closed.
  #' @return None (side effects: destroys observers, removes the content
  #'   div, updates `loaded`, records a timing).
  #'
  #' @examples
  #' # Inside setup_module_loading()
  #' # close_module(previous_tab, loaded)
  close_module <- function(tab_name, loaded) {

    if (!isTRUE(loaded[[tab_name]])) {
      return(invisible())
    }

    .time_op(tab_name, "close", {

      session <- shiny::getDefaultReactiveDomain()

      # Let the module clean up work that lives outside its UI.
      if (!is.null(session) && !is.null(session$userData$module_cleanup)) {
        cleanup_fn <- session$userData$module_cleanup[[tab_name]]
        if (is.function(cleanup_fn)) {
          tryCatch(
            cleanup_fn(),
            error = function(err) NULL
          )
        }
        session$userData$module_cleanup[[tab_name]] <- NULL
      }

      # Destroy observers registered by the module.
      if (!is.null(session) && !is.null(session$userData$.shiny.destroy)) {
        entries <- session$userData$.shiny.destroy[[tab_name]]
        if (!is.null(entries)) {
          for (e in entries) {
            if (inherits(e, "Observer")) {
              tryCatch(e$destroy(), error = function(err) NULL)
            }
          }
          # Clear the per-tab list before the next open.
          session$userData$.shiny.destroy[[tab_name]] <- NULL
        }
      }
      
      # Close any modal opened by the tab being removed.
      shiny::removeModal()
      
      if (!is.null(session) &&
          !is.null(session$userData$modal_open) &&
          is.function(session$userData$modal_open)) {
        session$userData$modal_open(FALSE)
      }
      
      # Remove the module content from the page.
      shiny::removeUI(
        selector  = paste0("#", tab_name, "-content"),
        immediate = TRUE
      )

      # Restore the loading screen for the next open.
      shinyjs::runjs(sprintf("shinyjs.show('%s');", paste0(tab_name, "-loading")))

      # Remove the module content from the page.
      shiny::removeUI(
        selector  = paste0("#", tab_name, "-content"),
        immediate = TRUE
      )

      loaded[[tab_name]] <- FALSE
    })
  }

  #' Set up module loading
  #'
  #' Opens modules when their tabs are selected. Persistent modules stay loaded
  #' after their first visit, while temporary modules are closed when their tabs
  #' are left.
  #'
  #' @param transient_tabs Character vector. Tabs that are opened and
  #'   closed as the user enters and leaves them.
  #' @param persistent_tabs Character vector. Tabs that are opened once
  #'   and kept loaded for the rest of the session.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param tab_input_id Character. The ID of the tabset input
  #'   (default `"tabs"`).
  #' @param module_registry A named list describing each module's UI function, server function, and extra arguments. Built in `variables/variablesTemp.R`.
  #' @return None (called for its side effects).
  #'
  #' @examples
  #' # Inside server function
  #' # setup_module_loading(
  #' #   persistent_tabs = c("home"),
  #' #   transient_tabs  = c("predictionsTaxonomy", "predictionsNetwork", ...)
  #' # )
  setup_module_loading <- function(
      transient_tabs  = character(0),
      persistent_tabs = character(0),
      session         = getDefaultReactiveDomain(),
      input           = getDefaultReactiveDomain()$input,
      tab_input_id    = "tabs",
      module_registry = get0("module_registry", envir = globalenv())
  ) {
    loaded       <- shiny::reactiveValues()
    selected_tab <- shiny::reactive(input[[tab_input_id]])
    prev_tab     <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input[[tab_input_id]], {
      shiny::req(input[[tab_input_id]])

      current  <- input[[tab_input_id]]
      previous <- prev_tab()

      # Close the tab being left if it is temporary.
      if (!is.null(previous) && previous != current && previous %in% transient_tabs) {
        close_module(previous, loaded)
      }

      # Open the tab being entered.
      if (current %in% transient_tabs) {
        open_module(current, session, loaded, selected_tab, module_registry)
      } else if (current %in% persistent_tabs) {
        open_module(current, session, loaded, selected_tab, module_registry)
      }

      prev_tab(current)
    })
  }

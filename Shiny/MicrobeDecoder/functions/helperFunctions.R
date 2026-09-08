# Helper Functions for App
# This script contains various helper functions for use in the Shiny app,
# including custom operators, file input handling, validation, and data cleaning.
# Author: Timothy Hackmann
# Date: 9 Mar 2025

# === General ===
  #' Null Defaults Operator
  #'
  #' This custom operator returns the first argument if it is not NULL, otherwise returns the second argument.
  #'
  #' @param x The value to check for NULL.
  #' @param y The default value to return if `x` is NULL.
  #' @return Returns `x` if it is not NULL; otherwise, returns `y`.
  #' @export
  `%||%` <- function(x, y) {
    if (!is.null(x)) x else y
  }
  
  #' Negation of In Operator
  #'
  #' This custom operator returns the negation of the `%in%` operator, 
  #' checking if elements are not present in a vector or list.
  #'
  #' @param x The values to check.
  #' @param table The vector or list to check against.
  #' @return A logical vector indicating if elements of `x` are not in `table`.
  #' @export
  #' @importFrom base Negate
  `%nin%` = Negate(`%in%`)
  
  #' Assign a Default Value if Input is Invalid
  #'
  #' This function checks whether an input value (`x`) is `NULL`, `NA`, or an empty string (`""`). 
  #' If so, it assigns and returns the specified `default` value. Otherwise, it returns `x` unchanged.
  #'
  #' @param x The input value to check.
  #' @param default The default value to return if `x` is invalid (`NULL`, `NA`, or `""`).
  #'
  #' @return The original `x` if valid, or `default` if `x` is `NULL`, `NA`, or an empty string.
  #' 
  #' @examples
  #' assign_if_invalid(NULL, "LPSN")  # Returns "LPSN"
  #' assign_if_invalid(NA, "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("", "LPSN")    # Returns "LPSN"
  #' assign_if_invalid("Valid", "LPSN") # Returns "Valid"
  #'
  #' @export
  assign_if_invalid <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) {
      return(default)
    }
    return(x)
  }

  #' Check if Shiny App is Running Locally
  #'
  #' Determines whether a Shiny application is running locally
  #' (e.g., via `runApp()` or RStudio) by checking the session's hostname.
  #' This can be useful to conditionally show messages, warnings, or behaviors
  #' that should only apply in a deployed environment (e.g., Shiny Server).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return A logical value: `TRUE` if the app is running on `localhost` or `127.0.0.1`, otherwise `FALSE`.
  #'
  #' @examples
  #' \dontrun{
  #'   if (!is_running_locally(session)) {
  #'     showNotification("You are running this on a server.")
  #'   }
  #' }
  #'
  #' @export
  is_running_locally <- function(session = getDefaultReactiveDomain()) { 
    # Tries to detect if running on localhost (127.0.0.1 or localhost)
    hostname <- session$clientData$url_hostname
    grepl("^(localhost|127\\.0\\.0\\.1)$", hostname)
  }
  
  #' Round to Nearest Target Value
  #'
  #' Rounds a numeric value to the nearest value from a given vector of target values.
  #'
  #' @param x A numeric value to round.
  #' @param targets A numeric vector of target values to round to.
  #'
  #' @return The element from `targets` that is closest to `x`.
  #'
  #' @examples
  #' round_to_nearest(0.13, c(0, 0.1, 0.5, 1, 2))
  #' round_to_nearest(1.7, c(0, 0.1, 0.5, 1, 2))
  round_to_nearest <- function(x, targets) {
    targets[which.min(abs(x - targets))]
  }
  
# === Navigation of tabs ===
  #' Extract a query string parameter from the URL
  #'
  #' This function safely retrieves a value from the URL's query string, optionally prints it,
  #' and returns `NULL` if not found or empty.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param param_name The name of the parameter to extract (default is "job").
  #' @param verbose Logical; if TRUE, prints the query and result.
  #'
  #' @return The value of the query parameter or NULL if missing.
  #' @export
  #'
  #' @examples
  #' job_id <- get_query_param(session, "job")
  get_query_param <- function(session = getDefaultReactiveDomain(), 
                              param_name = "job", verbose = FALSE) {
    query <- shiny::parseQueryString(session$clientData$url_search)
    
    if (verbose) print(query)
    
    if (!is.null(query) && param_name %in% names(query) && nzchar(query[[param_name]])) {
      if (verbose) message("Retrieved ", param_name, ": ", query[[param_name]])
      return(query[[param_name]])
    }
    
    if (verbose) message("No value found for '", param_name, "' in query string.")
    return(NULL)
  }
  
  #' Get the open state for a module's sidebar
  #'
  #' This function decides whether a module's sidebar starts open or closed.  It
  #' returns "closed" when the app was opened with a saved job for that tab, so
  #' results are in view right away.  Otherwise it returns the usual state, which
  #' is open on wide screens and closed on narrow ones.
  #'
  #' @param tab_name The name of the module's tab (e.g., "predictionsNetwork").
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #'
  #' @return A character string for the `open` argument of `bslib::sidebar()`.
  #' @export
  #'
  #' @examples
  #' get_sidebar_state("predictionsNetwork")
  get_sidebar_state <- function(tab_name, session = getDefaultReactiveDomain()) {
    job <- get_query_param(session = session, param_name = "job")
    tab <- get_query_param(session = session, param_name = "tab")
    
    if (!is.null(job) && identical(tab, tab_name)) "closed" else "desktop"
  }
  
  #' Navigate to Tab on App Initialization
  #'
  #' Navigates to the correct tab when the Shiny app first loads.  It parses the
  #' the tab based on the `?tab=` query parameter in the url.  
  #' 
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param tab_input_id The ID of the tabset input (e.g., `"tabs"`).
  #'
  #' @return `NULL`. Called for side effects.
  #' @export
  init_navigation <- function(session = getDefaultReactiveDomain(),
                                     input = getDefaultReactiveDomain()$input,
                                     tab_input_id = "tabs") {
    observe({
      if (is.null(session$userData$has_initialized) || !session$userData$has_initialized) {
        session$userData$has_initialized <- TRUE
        
        # Get tab and other query parameters
        tab <- get_query_param(param_name = "tab")
        
        # Navigate to correct tab
        if (!is.null(tab)) {
            shinyjs::runjs(sprintf("shinyjs.goToTab('%s');", tab))
        }
        
        # Update navigation counters
        session$userData$initial_tab <- tab
        session$userData$navigation_count <- 0L
      }
    })
  }
  
# === Get and format IP addresses ===  
  #' Get the User's IP Address
  #'
  #' Attempts to retrieve the user's public IP address using a client-side JavaScript call to `https://api.ipify.org`.
  #' If that fails (e.g., due to no internet access), the function falls back to server-side values from the `session$request` object.
  #'
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return A reactive expression that returns the user's IP address as a character string, or `"unknown"` if no IP can be determined.
  #' @export
  get_user_ip <- function(input = getDefaultReactiveDomain()$input,
                                   session = getDefaultReactiveDomain()) {
    eventReactive(input$user_ip, {
      ip <- input$user_ip
      
      # Fallback if input$user_ip is empty
      if (is.null(ip) || ip == "") {
        ip <- session$request$HTTP_X_FORWARDED_FOR %||%
          session$request$HTTP_X_REAL_IP %||%
          session$request$REMOTE_ADDR %||%
          "unknown"
      }
      
      trimws(ip)
    }, ignoreNULL = FALSE)
  }
  
  #' Generate a User ID by Hashing the IP Address
  #'
  #' Takes a reactive expression that returns an IP address and returns a reactive expression containing a SHA-256 hash of that IP.
  #' If the IP is `NULL` or `"unknown"`, the user ID will also be `"unknown"`.
  #'
  #' @param ip_reactive A reactive expression returning a character string representing the user's IP address.
  #'
  #' @return A reactive expression that returns a hashed user ID (SHA-256), or `"unknown"` if the IP is not usable.
  #' @export
  get_user_id <- function(ip_reactive) {
    reactive({
      ip <- ip_reactive()
      if (is.null(ip) || ip == "unknown") return("unknown")
      digest::digest(ip, algo = "crc32")
    })
  }
  
  #' Format sanitized IP for display
  #'
  #' @param sanitized_ip A string like "127001" or "19216811".
  #' @return Best-effort formatted IP like "127.0.0.1"
  #' @export
  format_ip_for_display <- function(sanitized_ip) {
    if (!grepl("^[0-9]+$", sanitized_ip)) return(sanitized_ip)  # fallback for unexpected input
    
    # Try to reformat as IPv4 (best-effort)
    if (nchar(sanitized_ip) == 6 || nchar(sanitized_ip) == 7 || nchar(sanitized_ip) == 8) {
      parts <- substring(sanitized_ip, c(1, 2, 4, 6), c(1, 3, 5, nchar(sanitized_ip)))
      return(paste(parts, collapse = "."))
    }
    
    # Just return original if reformatting fails
    sanitized_ip
  }
  
# === Clear old computation jobs ===    
  #' Delete Old or Large Files in a Directory
  #'
  #' Deletes files with a given extension in a directory (and subdirectories)
  #' if they are older than `max_age_days` or larger than `max_size_MB`.
  #'
  #' @param dir Directory to search for files.
  #' @param file_extension File extension to match (without dot). Default is `"rds"`.
  #' @param max_age_days Maximum file age in days before deletion. Default is `30`.
  #' @param max_size_MB Maximum file size in MB before deletion. Default is `50`.
  #' @param exclude_subdirs A character vector of subdirectory names to exclude from deletion.
  #' @param verbose Logical; if `TRUE`, prints messages. Default is `TRUE`.
  #' @return Character vector of deleted file paths (invisible).
  #' @export
  cleanup_old_large_files <- function(dir,
                                      file_extension = "rds",
                                      max_age_days = 30,
                                      max_size_MB = 50,
                                      exclude_subdirs = NULL,
                                      verbose = TRUE) {
    if (!dir.exists(dir)) return(invisible(character()))
    
    # Find matching files
    pattern <- paste0("(?i)\\.", file_extension, "$")
    files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
    
    # Exclude subdirectories if specified
    if (!is.null(exclude_subdirs) && length(files) > 0) {
      esc  <- paste0("/", exclude_subdirs, "/")
      excl <- paste(esc, collapse = "|")
      files <- files[!grepl(excl, files)]
    }
    
    if (length(files) == 0) {
      if (verbose) message("No matching files found.")
      return(invisible(character()))
    }
    
    # Get file info
    info <- fs::file_info(files)
    info$file_age_days <- as.numeric(difftime(Sys.time(), info$modification_time, units = "days"))
    info$file_size_MB <- info$size / 1024^2
    
    # Filter by age or size
    to_delete <- info$path[info$file_age_days > max_age_days | info$file_size_MB > max_size_MB]
    to_delete <- to_delete[!is.na(to_delete) & to_delete != ""]
    
    if (length(to_delete) == 0) {
      if (verbose) message("No files to delete.")
      return(invisible(character()))
    }
    
    # Delete files
    deleted <- file.remove(to_delete)
    if (verbose) {
      message("Deleted ", sum(deleted), " file(s):")
      message(paste(to_delete[deleted], collapse = "\n"))
    }
    
    invisible(to_delete[deleted])
  }
  
  #' Delete Expired Computation Jobs
  #'
  #' Deletes computation jobs that are too old or too large. Unlike
  #' `cleanup_old_large_files()`, this works on whole jobs rather than single
  #' files, so a job's result, inputs, and status file are removed together. This
  #' keeps the History tab from reporting jobs whose files have been partly
  #' removed, and it removes jobs that never produced a result, such as those that
  #' failed or were canceled.
  #'
  #' A job is identified by the files that share its job ID inside a tool
  #' directory. Its age is taken from the most recent change to any of those
  #' files, and its size is the total size of those files. Jobs in excluded
  #' subdirectories, such as the demonstration user, are never deleted.
  #'
  #' @param dir Directory to search for jobs.
  #' @param max_age_days Maximum job age in days before deletion. Default is `30`.
  #' @param max_size_MB Maximum job size in MB before deletion. Default is `50`.
  #' @param exclude_subdirs A character vector of subdirectory names to exclude from deletion.
  #' @param verbose Logical; if `TRUE`, prints messages. Default is `TRUE`.
  #' @return Character vector of deleted file paths (invisible).
  #' @export
  delete_expired_jobs <- function(dir = "jobs",
                                  max_age_days = 30,
                                  max_size_MB = 50,
                                  exclude_subdirs = NULL,
                                  verbose = TRUE) {
    if (!dir.exists(dir)) return(invisible(character()))
    
    # Find all job files (results, inputs, and status records)
    files <- list.files(dir, pattern = "(?i)\\.rds$", full.names = TRUE, recursive = TRUE)
    
    # Exclude subdirectories if specified
    if (!is.null(exclude_subdirs) && length(files) > 0) {
      esc  <- paste0("/", exclude_subdirs, "/")
      excl <- paste(esc, collapse = "|")
      files <- files[!grepl(excl, files)]
    }
    
    if (length(files) == 0) {
      if (verbose) message("No jobs found.")
      return(invisible(character()))
    }
    
    # Group files by job. A job's files share a directory and job ID, with
    # suffixes for the result, inputs, and status.
    job_id_of <- function(path) {
      name <- basename(path)
      name <- sub("\\.status\\.rds$", "", name)
      name <- sub("\\.progress\\.rds$", "", name)
      name <- sub("\\.cancel\\.rds$", "", name)
      name <- sub("_inputs\\.rds$", "", name)
      sub("\\.rds$", "", name)
    }
    job_keys <- file.path(dirname(files), vapply(files, job_id_of, character(1)))
    
    # Get file info
    info <- fs::file_info(files)
    
    # Find the age and size of each job from its files, then mark whole jobs for
    # deletion when they are too old or too large.
    to_delete <- character()
    for (idx in split(seq_along(files), job_keys)) {
      job_age_days <- as.numeric(difftime(Sys.time(), max(info$modification_time[idx]), units = "days"))
      job_size_MB <- sum(info$size[idx]) / 1024^2
      if (job_age_days > max_age_days || job_size_MB > max_size_MB) {
        to_delete <- c(to_delete, files[idx])
      }
    }
    
    if (length(to_delete) == 0) {
      if (verbose) message("No jobs to delete.")
      return(invisible(character()))
    }
    
    # Delete files
    deleted <- file.remove(to_delete)
    if (verbose) {
      message("Deleted ", sum(deleted), " file(s) from expired jobs.")
    }
    
    invisible(to_delete[deleted])
  }
  
  #' Set Up Automatic Job Cleanup
  #'
  #' Periodically deletes old or large computation jobs from a given directory.
  #'
  #' @inheritParams delete_expired_jobs
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param interval_hours Cleanup interval in hours. Default is 6.
  #'
  #' @return A reactive observer that performs cleanup periodically.
  #' @export
  setup_auto_cleanup <- function(session = getDefaultReactiveDomain(),
                                 dir = "jobs",
                                 max_age_days = 30,
                                 max_size_MB = 50,
                                 interval_hours = 6,
                                 exclude_subdirs = "examples",
                                 verbose = TRUE) {
    auto_cleanup_timer <- reactiveTimer(interval_hours * 60 * 60 * 1000, session = session)
    
    observe({
      auto_cleanup_timer()
      start_time <- Sys.time()
      if (verbose) message("Running scheduled cleanup in: ", dir)
      
      t1 <- Sys.time()
      deleted <- delete_expired_jobs(
        dir = dir,
        max_age_days = max_age_days,
        max_size_MB = max_size_MB,
        exclude_subdirs = exclude_subdirs,
        verbose = verbose
      )
      t2 <- Sys.time()
      
      if (verbose) {
        total_time <- round(difftime(t2, start_time, units = "secs"), 3)
        message("Cleanup process completed in ", total_time, " seconds.")
      }
    })
  }

# === Perform animation ===
  #' Trigger Typing Animation When Home Tab is Selected
  #'
  #' This function sets up an `observeEvent` inside the server function
  #' to trigger a typing animation whenever the user navigates to the "home" tab.
  #' It sends a custom message `"triggerTypingEffect"` to the client when the home
  #' tab is selected, which should be handled by JavaScript in `www/custom.js`.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param input The Shiny input object. Defaults to the current reactive domain's input.
  #' @return None. The function sets up a reactive observer.
  #'
  #' @examples
  #' # Inside server function
  #' trigger_typing(session, input)
  #'
  #' @export
  trigger_typing <- function(session = getDefaultReactiveDomain(), 
                             input = getDefaultReactiveDomain()$input) {
    observeEvent(input$tabs, {
      if (input$tabs == "home") {
        session$sendCustomMessage("triggerTypingEffect", "home")
      }
    })
  }
  
# === Download handlers ===
  #' Create a Shiny Download Handler for Multiple File Types
  #'
  #' This function generates a download handler that loads data and then
  #' saves it in the specified format for download.  It can generate `.csv`,
  #' `.rds`, `.txt`, or `.zip` files. The source data can be provided as:
  #' \itemize{
  #'   \item An \strong{object in memory} (e.g., a data.frame, list, character vector).
  #'   \item A \strong{function} that returns an object when evaluated.
  #'   \item A \strong{file path} to an existing file (e.g., pre-generated `.csv`,
  #'         `.rds`, `.txt`, or `.zip`).
  #' }
  #'
  #' If \code{zip = TRUE}, the content is first written or copied to a temporary
  #' file and then zipped before download. If the source is already a `.zip`
  #' file, it will simply be copied without re-zipping.
  #'
  #' @param filename_prefix A character string or reactive used as the prefix for the downloaded file's name.
  #' @param data_source An object, a function returning an object, or a file path to an existing file.
  #' @param file_type A character string indicating the file type for download.
  #'        Can be `"csv"`, `"rds"`, or `"txt"`. Default is `"csv"`.
  #' @param zip Logical. If `TRUE`, the file will be zipped before download.
  #'        If the source is already a `.zip` file, it is copied directly.
  #' @return A Shiny download handler, ready to be assigned to an output object
  #'         in a Shiny server function.
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' # From an in-memory object (CSV)
  #' output$downloadData <- create_download_handler(
  #'   "gene_functions",
  #'   function() gene_functions_data
  #' )
  #'
  #' # From an in-memory object, saved as RDS
  #' output$downloadModel <- create_download_handler(
  #'   "model_methanogenesis",
  #'   function() "data/random_forest_models/methanogenesis.rds",
  #'   file_type = "rds"
  #' )
  #'
  #' # From an in-memory object, zipped as CSV
  #' output$downloadCsvZip <- create_download_handler(
  #'   "gene_functions",
  #'   function() gene_functions_data,
  #'   file_type = "csv",
  #'   zip = TRUE
  #' )
  #'
  #' # From an existing file on disk (copy as-is)
  #' output$downloadRdsFile <- create_download_handler(
  #'   "existing_model",
  #'   "path/to/model.rds",
  #'   file_type = "rds"
  #' )
  #'
  #' # From an existing pre-zipped file (copied directly)
  #' output$downloadZip <- create_download_handler(
  #'   "example_bundle",
  #'   "path/to/example_bundle.zip",
  #'   zip = TRUE
  #' )
  #' }
  create_download_handler <- function(filename_prefix, data_source, file_type = "csv", zip = TRUE) {
    shiny::downloadHandler(
      filename = function() {
        prefix <- if (is.reactive(filename_prefix)) filename_prefix() else filename_prefix
        ext <- if (zip) "zip" else file_type
        paste0(prefix, ".", ext)
      },
      content = function(file) {
        # Resolve data
        data <- if (is.function(data_source)) data_source() else data_source
        
        # Case 1: zip requested
        if (zip) {
          if (is.character(data) && length(data) == 1L && file.exists(data) && grepl("\\.zip$", data, ignore.case = TRUE)) {
            # Already a .zip file b copy as-is
            ok <- file.copy(data, file, overwrite = TRUE)
            if (!ok) stop("Failed to copy zip file from ", data)
          } else {
            # Otherwise create a zip from object or other file type
            tmp <- tempfile(fileext = paste0(".", file_type))
            
            if (is.character(data) && length(data) == 1L && file.exists(data)) {
              file.copy(data, tmp, overwrite = TRUE)
            } else {
              if (file_type == "csv") {
                utils::write.csv(data, tmp, row.names = FALSE)
              } else if (file_type == "rds") {
                saveRDS(data, tmp)
              } else if (file_type == "txt") {
                writeLines(as.character(data), tmp)
              } else {
                stop("Unsupported file type for zipping: ", file_type)
              }
            }
            
            utils::zip(zipfile = file, files = tmp, flags = "-j")  # -j strips paths
          }
          
        } else {
          # Case 2: no zip
          if (is.character(data) && length(data) == 1L && file.exists(data)) {
            ok <- file.copy(data, file, overwrite = TRUE)
            if (!ok) stop("Failed to copy file from ", data)
          } else {
            if (file_type == "csv") {
              utils::write.csv(data, file, row.names = FALSE)
            } else if (file_type == "rds") {
              saveRDS(data, file)
            } else if (file_type == "txt") {
              writeLines(as.character(data), file)
            } else {
              stop("Unsupported file type: ", file_type)
            }
          }
        }
      },
      contentType = if (zip) {
        "application/zip"
      } else {
        switch(file_type,
               csv = "text/csv",
               txt = "text/plain",
               zip = "application/zip",
               "application/octet-stream") # rds / default
      }
    )
  }

# === Modals ===
	#' Stop Process with Condition
	#'
	#' This helper function stops the execution of code with a specified condition and error message.
	#'
	#' @param class A character vector of classes to assign to the error.
	#' @param message A character string specifying the error message.
	#' @return Stops the execution with the specified condition.
	#' @export
	stop_with_condition <- function(class, message) {
	  cond <- structure(
		list(message = message),
		class = c(class, 'error', 'condition')
	  )
	  stop(cond)
	}

	#' Stop Reactive Process with Condition
	#'
	#' This helper function stops a Shiny reactive process with a specified condition.
	#'
	#' @param message A character string specifying the error message.
	#' @param class A character vector of classes to assign to the error.
	#' @return Stops the reactive process with the specified condition.
	#' @export
	reactive_stop <- function(message = "", class = NULL) {
	  stop_with_condition(c("shiny.silent.error", class), message)
	}

	#' Validate Input and Launch Modal on Error
	#'
	#' This function validates Shiny input and launches a modal dialog with an error message if validation fails.
	#' It is modified from shiny::validate() to launch modal rather than returning a simple text output. 
	#' 
	#' @param ... Validation conditions to check.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @param errorClass A character vector of error classes to assign to the error. Default is an empty character vector.
	#' @param delay_time The delay in milliseconds before launching modal. 
	#' @return Invisible if validation passes; otherwise, stops the reactive process with a modal error message.
	#' @export
	#' @importFrom shiny removeModal showModal modalDialog h4 p
	#' @importFrom rlang list2
	#' @importFrom stats na.omit
	run_validation_modal <- function(..., session = getDefaultReactiveDomain(), errorClass = character(0), delay_time = 250) {
	  # Test validation conditions
	  results <- sapply(rlang::list2(...), function(x) {
		if (is.null(x)) 
		  return(NA_character_)
		else if (identical(x, FALSE)) 
		  return("")
		else if (is.character(x)) 
		  return(paste(as.character(x), collapse = "\n"))
		else stop("Unexpected validation result: ", as.character(x))
	  })
	  results <- stats::na.omit(results)
	  if (length(results) == 0) 
		return(invisible())
	  results <- results[nzchar(results)]
	  
	  # Show error message
	  shinyjs::delay(delay_time, { 
		# Remove existing modals
		shiny::removeModal()
		
		# Set the modal open state to FALSE
		session$userData$modal_open(FALSE)
		
		# Launch modal
		shiny::showModal(shiny::modalDialog(
		  shiny::h4("Error in input"),
		  shiny::p(paste(results)),
		  easyClose = TRUE, footer = NULL
		))
	  })
	  
	  # Stop reactive process
	  reactive_stop(paste("", collapse = "\n"), c(errorClass,
												 "validation"))
	}

	#' Display Modal with Progress Bar and Optional Link
	#'
	#' This function displays or updates a modal with a progress bar.
	#' If a modal is already open, it updates the message and progress bar.
	#'
	#' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @param id The id of the progress bar (default = `ns("pb")`).
	#' @param message Message to display above the progress bar. Default is `"Initializing"`.
	#' @param value Value for the progress bar. Default is `0`.
	#' @param url Optional URL to display below the message.
	#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
	#' @return None. The function displays a modal as a side effect.
	#' @importFrom shiny showModal modalDialog tags removeModal
	#' @importFrom shinyjs runjs
	#' @importFrom shinyWidgets progressBar updateProgressBar
	#' @export
	display_modal <- function(session = getDefaultReactiveDomain(),
	                          id = NULL,
	                          message = "Initializing",
	                          value = 0,
	                          url = NULL,
	                          ns = identity,
	                          cancel_input_id = NULL,
	                          cancel_label = "Cancel") {
	  # Assign id if null
	  if (is.null(id)) {
	    if (is.function(ns)) {
	      id <- ns("pb")
	    } else {
	      id <- "pb"
	    }
	  }
	  
	  # Clamp progress bar value to 0-100
	  value <- max(0, min(100, value))
	  
	  # Use a second id for the inner Bootstrap bar.  The outer id is kept as the
	  # function argument so existing calls do not need to change.
	  bar_id <- paste0(id, "_bar")
	  
	  if (isFALSE(session$userData$modal_open())) {
	    cancel_button <- NULL
	    if (!is.null(cancel_input_id)) {
	      cancel_button <- shiny::tags$div(
	        class = "progress-modal-actions",
	        style = "display:flex; justify-content:flex-end; margin-top:12px;",
	        shiny::actionButton(
	          inputId = cancel_input_id,
	          label = cancel_label,
	          class = "btn btn-danger"
	        )
	      )
	    }
	    
	    progress_modal_css <- shiny::tags$style(shiny::HTML(
	      "
      .progress-modal-body .modal-link-text {
        margin-top: 0 !important;
        margin-bottom: 4px !important;
      }

      .progress-modal-progress {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }

      .progress-modal-progress .progress {
        height: 16px !important;
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }

      .progress-modal-progress .progress-bar {
        line-height: 16px !important;
        font-size: 12px !important;
        padding: 0 !important;
      }
      "
	    ))
	    
	    # Create compact modal content.  Put the cancel button in the body rather
	    # than in modalDialog(footer = ...), because Bootstrap's modal footer adds
	    # extra vertical space that makes this progress dialog look oversized.
	    modal_content <- shiny::tags$div(
	      class = "progress-modal-body",
	      style = "padding-bottom:2px;",
	      progress_modal_css,
	      shiny::tags$h4(
	        id = "modal-text",
	        style = "margin-top:0; margin-bottom:8px;",
	        message
	      ),
	      if (!is.null(url)) shiny::tags$div(
	        id = "modal-link",
	        class = "modal-link-text",
	        style = "margin-top:0; margin-bottom:4px;",
	        "Results will be stored at ",
	        shiny::tags$a(href = url, "this link", target = "_blank"),
	        "."
	      ),
	      shiny::tags$div(
	        class = "progress-modal-progress",
	        shiny::tags$div(
	          id = id,
	          class = "progress",
	          shiny::tags$div(
	            id = bar_id,
	            class = "progress-bar",
	            role = "progressbar",
	            style = paste0("width:", value, "%;"),
	            `aria-valuenow` = value,
	            `aria-valuemin` = 0,
	            `aria-valuemax` = 100,
	            paste0(value, "%")
	          )
	        )
	      ),
	      cancel_button
	    )
	    
	    shiny::showModal(shiny::modalDialog(
	      modal_content,
	      easyClose = FALSE,
	      footer = NULL
	    ))
	    
	    session$userData$modal_open(TRUE)
	  } else {
	    # Update the message text
	    shinyjs::runjs(sprintf(
	      "document.getElementById('modal-text').innerText = %s;",
	      jsonlite::toJSON(message, auto_unbox = TRUE)
	    ))
	    
	    # Update or insert the job URL
	    if (!is.null(url)) {
	      link_update_js <- sprintf(
	        "
        document.getElementById('modal-link')?.remove();

        const p = document.createElement('div');
        p.id = 'modal-link';
        p.className = 'modal-link-text';
        p.style.marginTop = '0';
        p.style.marginBottom = '4px';

        const text = document.createTextNode('Results will be stored at ');
        const a = document.createElement('a');
        a.href = %s;
        a.target = '_blank';
        a.textContent = 'this link';

        p.appendChild(text);
        p.appendChild(a);
        p.appendChild(document.createTextNode('.'));

        const modalBody = document.querySelector('.progress-modal-body');
        const progress = modalBody.querySelector('.progress-modal-progress');
        modalBody.insertBefore(p, progress);
        ",
	        jsonlite::toJSON(url, auto_unbox = TRUE)
	      )
	      shinyjs::runjs(link_update_js)
	    }
	    
	    # Update progress bar
	    progress_update_js <- sprintf(
	      "
      (function() {
        const bar = document.getElementById(%s);
        if (!bar) return;

        bar.style.width = '%s%%';
        bar.setAttribute('aria-valuenow', '%s');
        bar.innerText = '%s%%';
      })();
      ",
	      jsonlite::toJSON(bar_id, auto_unbox = TRUE),
	      value,
	      value,
	      value
	    )
	    
	    shinyjs::runjs(progress_update_js)
	  }
	}
	
	#' Hide Modal with Progress Bar
	#'
	#' This function hides a modal with a progress bar. It includes a brief delay 
	#' to ensure UI elements are fully updated before the modal is removed.
	#'
	#' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @param id The id of the progress bar to update (default = `ns("pb")`).
	#' @param delay_time The delay in milliseconds before hiding the modal. Default is `1000`.
	#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
	#' @return None. The function hides the modal as a side effect.
	#' @importFrom shinyjs delay
	#' @importFrom shinyWidgets updateProgressBar
	#' @importFrom shiny removeModal
	#' @export
	hide_modal_with_progress <- function(session = getDefaultReactiveDomain(),
	                                     id = NULL,
	                                     delay_time = 1000,
	                                     ns = identity) {
	  if (is.null(id)) {
	    if (is.function(ns)) {
	      id <- ns("pb")
	    } else {
	      id <- "pb"
	    }
	  }
	  
	  shinyjs::delay(delay_time, {
	    shinyWidgets::updateProgressBar(session = session, id = id, value = 100)
	    shiny::removeModal()
	    session$userData$modal_open(FALSE)
	  })
	}

# === File reading ===
	#' Concatenate a Vector into Noun Phrase
	#'
	#' This function takes a character vector and formats it into a a non, 
	#' using commas and a conjunction (e.g., "or" or "and") before the last element.
	#'
	#' @param vec A character vector of words or phrases to concatenate.
	#' @param conjunction A string specifying the conjunction to use before the last element (default is "or").
	#'
	#' @return A string representing the concatenated list.
	#' @examples
	#' format_list(c("apple", "banana", "cherry")) # "apple, banana, or cherry"
	#' format_list(c("apple", "banana"), "and")   # "apple and banana"
	#' format_list("apple")                        # "apple"
	#' format_list(character(0))                   # ""
	#'
	#' @export
	vector_to_phrase <- function(vec, conjunction = "or") {
	  n <- length(vec)
	  
	  if (n == 0) {
	    return("")
	  } else if (n == 1) {
	    return(vec)
	  } else if (n == 2) {
	    return(paste0(vec, collapse = paste0(" ", conjunction, " ")))
	  } else {
	    return(paste0(paste0(vec[-n], collapse = ", "), conjunction, vec[n]))
	  }
	}
	
	#' Validate and Read a File
	#'
	#' This function validates that the file is of an accepted type, ensures it 
	#' exists, and reads it accordingly.  Optionally, it can detect if the file
	#' starts with a comment line (#).  If detected, it will keep only the 
	#' last comment line, which is the one that usually contains headers.
	#'
	#' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @param file_path A character string. The path to the file (typically `input$foo$datapath`).
	#' @param original_name A character string. The original filename uploaded by the user (typically `input$foo$name`). Used to determine the file extension. Default is `NA`.
	#' @param accepted_extensions A character vector. The accepted file extensions (e.g., "csv")`.
	#' @param detect_comments Logical. Detect comment lines in file? Default is `TRUE`.
	#' @return A data frame (for CSV/TXT/KO/Excel) or an R object (for RDS), or NULL if an error occurs.
	#' @export
	#' @importFrom tools file_ext
	#' @importFrom shiny req
	#' @importFrom utils read.csv
	#' @importFrom readr read_csv read_delim
	#' @importFrom base readRDS
	#' @importFrom readxl read_excel
	validate_and_read_file <- function(
    session = getDefaultReactiveDomain(),
    file_path,          # <- input$foo$datapath
    original_name = NA, # <- input$foo$name
    accepted_extensions = c(
      "csv", "tsv", "txt", "rds", "ko",
      "nwk", "newick", "tree", "tre",
      "xls", "xlsx", "zip"
    ),
    detect_comments = TRUE
	) {
	  if (is.null(file_path) || file_path == "") {
	    run_validation_modal(session = session, "No file selected. Please upload a file.")
	    return(NULL)
	  }
	  
	  # Prefer extension from original filename; fall back to path.
	  ext_from_name <- tolower(tools::file_ext(if (is.na(original_name)) "" else original_name))
	  ext_from_path <- tolower(tools::file_ext(file_path))
	  file_ext <- if (nzchar(ext_from_name)) ext_from_name else ext_from_path
	  
	  # Fallback: light content sniff for ZIP (xlsx containers)
	  if (!nzchar(file_ext)) {
	    con <- file(file_path, "rb"); on.exit(try(close(con), silent = TRUE), add = TRUE)
	    sig <- try(readBin(con, what = "raw", n = 4), silent = TRUE)
	    if (!inherits(sig, "try-error") && length(sig) == 4 && identical(as.raw(c(0x50,0x4B,0x03,0x04)), sig)) {
	      # Could be .xlsx (or a generic .zip). We'll try xlsx first.
	      file_ext <- "xlsx"
	    }
	  }
	  
	  if (!file_ext %in% accepted_extensions) {
	    run_validation_modal(session = session, paste(
	      "Invalid file type. Please upload a", vector_to_phrase(accepted_extensions)
	    ))
	    return(NULL)
	  }
	  
	  # --- ZIP handling (explicit .zip uploads of data files) ---
	  if (identical(file_ext, "zip")) {
	    inner_exts <- setdiff(tolower(accepted_extensions), "zip")
	    pattern <- paste0("\\.(", paste(inner_exts, collapse = "|"), ")$")
	    z <- tryCatch(
	      extract_from_zip(file_path, pattern = pattern),
	      error = function(e) {
	        run_validation_modal(
	          session,
	          paste("Invalid zip file. Please ensure it contains a", vector_to_phrase(accepted_extensions))
	        )
	        NULL
	      }
	    )
	    if (is.null(z)) return(NULL)
	    on.exit(unlink(z$temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
	    file_path <- z$path
	    file_ext  <- tolower(z$extension)
	  }
	  
	  # --- Optional comment handling for plain text files ---
	  if (detect_comments && file_ext %in% c("csv","tsv","txt")) {
	    lines <- readLines(file_path, warn = FALSE)
	    if (length(lines) > 0 && startsWith(trimws(lines[1]), "#")) {
	      non_comment_idx <- which(!startsWith(trimws(lines), "#"))
	      cutoff <- if (length(non_comment_idx) > 0) min(non_comment_idx) - 1L else length(lines)
	      last_idx <- cutoff
	      header <- sub("^\\s*#+\\s*", "", lines[last_idx])
	      body   <- if (last_idx < length(lines)) lines[(last_idx+1):length(lines)] else character(0)
	      tmp <- tempfile(fileext = paste0(".", file_ext))
	      writeLines(c(header, body), tmp)
	      file_path <- tmp
	    }
	  }
	  
	  # --- Reader dispatch ---
	  load_function <- switch(
	    file_ext,
	    "rds"  = readRDS,
	    "csv"  = function(file, ...) readr::read_csv(file, show_col_types = FALSE, ...),
	    "tsv"  = function(file, ...) readr::read_delim(file, delim = "\t", show_col_types = FALSE, ...),
	    "txt"  = function(file, ...) readr::read_delim(file, delim = "\t", show_col_types = FALSE, ...),
	    "ko"   = function(file, ...) utils::read.table(file, sep = "\t", header = FALSE, fill = TRUE, ...),
	    "nwk"    = ape::read.tree,
	    "newick" = ape::read.tree,
	    "tree"   = ape::read.tree,
	    "tre"    = ape::read.tree,
	    "xls"  = function(file, ...) readxl::read_excel(path = file, ...),
	    "xlsx" = function(file, ...) readxl::read_excel(path = file, ...),
	    stop(sprintf("Unsupported file extension: %s", file_ext))
	  )
	  
	  data <- tryCatch(
	    do.call(load_function, list(file_path)),
	    error = function(e) {
	      # More actionable feedback (still shows your modal)
	      msg <- sprintf("Failed reading '%s' as %s: %s",
	                     ifelse(is.na(original_name), basename(file_path), original_name),
	                     toupper(file_ext), conditionMessage(e))
	      cat("[validate_and_read_file] ", msg, "\n")  # server log
	      run_validation_modal(session = session, "Error reading the file. Please check the file format.")
	      NULL
	    }
	  )
	  
	  data
	}
	

	#' Get Tree from Uploaded File
	#'
	#' Reads a phylogenetic tree uploaded by the user.
	#'
	#' @param tree_upload A Shiny file upload object containing `name` and `datapath`.
	#' @param session The Shiny session object.
	#' @return A phylo object.
	get_tree_from_upload <- function(
	  tree_upload,
	  session = shiny::getDefaultReactiveDomain()
	) {
	  if (is.null(tree_upload)) {
	    return(NULL)
	  }
	  
	  tree <- validate_and_read_file(
	    file_path = tree_upload$datapath,
	    original_name = tree_upload$name,
	    accepted_extensions = c(
	      "nwk",
	      "newick",
	      "tree",
	      "tre",
	      "zip"
	    ),
	    detect_comments = FALSE,
	    session = session
	  )
	  
	  run_validation_modal(
	    session = session,
	    shiny::need(
	      inherits(tree, c("phylo", "multiPhylo")),
	      "Please check the format of your tree and try again."
	    )
	  )
	  
	  if (inherits(tree, "multiPhylo")) {
	    tree <- tree[[1]]
	  }
	  
	  return(tree)
	}
	
# === Handling organism metadata ===
	#' Find a Column by Name
	#'
	#' Finds a column without regard to case. Optionally uses the first column
	#' when the requested name is absent.
	#'
	#' @param df A data frame.
	#' @param name Column name to find.
	#' @param fallback_first Use the first column when no match is found.
	#' @return The matching column name, or `NA_character_`.
	#' @export
	find_column <- function(df, name, fallback_first = FALSE) {
	  if (!is.data.frame(df) || ncol(df) == 0) {
	    return(NA_character_)
	  }
	  
	  if (!is.null(name) && length(name) == 1 && !is.na(name) && nzchar(name)) {
	    hit <- which(tolower(names(df)) == tolower(name))
	    
	    if (length(hit) > 0) {
	      return(names(df)[hit[1]])
	    }
	  }
	  
	  if (isTRUE(fallback_first)) {
	    return(names(df)[1])
	  }
	  
	  NA_character_
	}
	
	#' Read Uploaded Organism Metadata
	#'
	#' Reads an uploaded metadata table while preserving its column names for
	#' use as hover labels.
	#'
	#' @param metadata_upload A Shiny file upload object, or `NULL`.
	#' @param session The Shiny session object.
	#' @return A metadata data frame, or `NULL` when no file was uploaded.
	#' @export
	get_metadata_from_upload <- function(
    metadata_upload,
    session = shiny::getDefaultReactiveDomain()
	) {
	  if (is.null(metadata_upload)) {
	    return(NULL)
	  }
	  
	  metadata <- validate_and_read_file(
	    file_path = metadata_upload$datapath,
	    original_name = metadata_upload$name,
	    accepted_extensions = c("csv", "tsv", "txt", "xls", "xlsx", "zip"),
	    detect_comments = TRUE,
	    session = session
	  )
	  
	  if (is.null(metadata)) {
	    return(NULL)
	  }
	  
	  metadata <- as.data.frame(
	    metadata,
	    stringsAsFactors = FALSE,
	    check.names = FALSE
	  )
	  
	  run_validation_modal(
	    session = session,
	    shiny::need(
	      ncol(metadata) >= 1 && nrow(metadata) >= 1,
	      "Please upload metadata with at least one column and one row."
	    )
	  )
	  
	  metadata
	}
	
	#' Bind Uploaded Metadata to Organisms
	#'
	#' Matches uploaded metadata to organisms by row order or, when allowed, by
	#' an identifier column such as `Genome`.
	#'
	#' @param results Results containing the organism name and order columns.
	#' @param metadata Uploaded metadata, or `NULL`.
	#' @param key_col Organism-name column used by plots.
	#' @param number_col Column giving the original organism order.
	#' @param allow_id_override Allow an identifier column to override row order.
	#' @param id_col Identifier column to look for.
	#' @return One row per organism with `key_col` followed by metadata columns.
	#' @export
	bind_metadata_to_organisms <- function(
    results,
    metadata,
    key_col = "Organism name",
    number_col = "Organism number",
    allow_id_override = TRUE,
    id_col = "Genome"
	) {
	  if (is.null(metadata)) {
	    return(NULL)
	  }
	  
	  organism_cols <- intersect(c(number_col, key_col), names(results))
	  organisms <- unique(results[, organism_cols, drop = FALSE])
	  
	  if (number_col %in% names(organisms)) {
	    organisms <- organisms[order(organisms[[number_col]]), , drop = FALSE]
	  }
	  
	  organism_names <- as.character(organisms[[key_col]])
	  identifier_col <- if (allow_id_override) {
	    find_column(metadata, id_col)
	  } else {
	    NA_character_
	  }
	  
	  if (!is.na(identifier_col)) {
	    metadata_rows <- match(
	      organism_names,
	      as.character(metadata[[identifier_col]])
	    )
	    metadata_cols <- setdiff(names(metadata), identifier_col)
	  } else {
	    metadata_rows <- seq_along(organism_names)
	    metadata_rows[metadata_rows > nrow(metadata)] <- NA_integer_
	    metadata_cols <- names(metadata)
	  }
	  
	  if (length(metadata_cols) == 0) {
	    return(NULL)
	  }
	  
	  out <- metadata[metadata_rows, metadata_cols, drop = FALSE]
	  out <- cbind(
	    stats::setNames(
	      data.frame(organism_names, stringsAsFactors = FALSE),
	      key_col
	    ),
	    out
	  )
	  rownames(out) <- NULL
	  
	  out
	}
	
	#' Align Metadata Rows
	#'
	#' Reorders metadata to match a vector of organism keys. Unmatched keys
	#' receive missing values.
	#'
	#' @param organism_keys Organism keys in the required output order.
	#' @param metadata Metadata keyed by `id_col`.
	#' @param id_col Name of the metadata key column.
	#' @param reserved Columns to exclude from the result.
	#' @return Aligned metadata columns, or `NULL` when none remain.
	#' @export
	align_metadata_rows <- function(
    organism_keys,
    metadata,
    id_col = "Organism name",
    reserved = character(0)
	) {
	  if (is.null(metadata)) {
	    return(NULL)
	  }
	  
	  resolved_id_col <- find_column(metadata, id_col, fallback_first = TRUE)
	  metadata_cols <- setdiff(names(metadata), c(resolved_id_col, reserved))
	  
	  if (length(metadata_cols) == 0) {
	    return(NULL)
	  }
	  
	  metadata_rows <- match(
	    as.character(organism_keys),
	    as.character(metadata[[resolved_id_col]])
	  )
	  
	  out <- metadata[metadata_rows, metadata_cols, drop = FALSE]
	  rownames(out) <- NULL
	  
	  out
	}
	
	#' Format Rows as Hover Text
	#'
	#' Converts each row into `Column: value` lines. Missing and empty values
	#' are omitted.
	#'
	#' @param df A data frame.
	#' @param sep Separator between column names and values.
	#' @param newline Separator between fields.
	#' @param leading Text added before non-empty results.
	#' @return One hover string per row.
	#' @export
	format_row_hover <- function(df, sep = ": ", newline = "<br>", leading = "") {
	  if (is.null(df)) {
	    return(character(0))
	  }
	  
	  headers <- names(df)
	  
	  vapply(seq_len(nrow(df)), function(i) {
	    values <- as.character(unlist(df[i, , drop = FALSE], use.names = FALSE))
	    keep <- !is.na(values) & trimws(values) != "" & values != "NA"
	    
	    if (!any(keep)) {
	      return("")
	    }
	    
	    fields <- paste0(headers[keep], sep, values[keep])
	    paste0(leading, paste(fields, collapse = newline))
	  }, character(1))
	}
	
	#' Format Organism Metadata
	#'
	#' Combines metadata already present in results with uploaded metadata.
	#' Result columns take precedence when names overlap.
	#'
	#' @param df Results containing the organism key.
	#' @param metadata Uploaded metadata, or `NULL`.
	#' @param cols Result columns to include.
	#' @param key_col Organism-name column used by plots.
	#' @param number_col Column giving the original organism order.
	#' @param allow_id_override Allow an uploaded identifier to override row order.
	#' @param id_col Uploaded identifier column to look for.
	#' @return One row per organism with `key_col` followed by metadata fields.
	#' @export
	format_metadata <- function(
    df,
    metadata,
    cols = NULL,
    key_col = "Organism name",
    number_col = "Organism number",
    allow_id_override = TRUE,
    id_col = "Genome"
	) {
	  if (!is.data.frame(df) || !(key_col %in% names(df))) {
	    return(NULL)
	  }
	  
	  result_cols <- intersect(cols, names(df))
	  from_results <- NULL
	  
	  if (length(result_cols) > 0) {
	    cols_to_take <- intersect(c(number_col, key_col, result_cols), names(df))
	    from_results <- unique(df[, cols_to_take, drop = FALSE])
	    
	    if (number_col %in% names(from_results)) {
	      from_results <- from_results[order(from_results[[number_col]]), , drop = FALSE]
	      from_results[[number_col]] <- NULL
	    }
	    
	    rownames(from_results) <- NULL
	  }
	  
	  from_upload <- bind_metadata_to_organisms(
	    results = df,
	    metadata = metadata,
	    key_col = key_col,
	    number_col = number_col,
	    allow_id_override = allow_id_override,
	    id_col = id_col
	  )
	  
	  if (is.null(from_results)) {
	    out <- from_upload
	  } else if (is.null(from_upload)) {
	    out <- from_results
	  } else {
	    upload_cols <- setdiff(names(from_upload), names(from_results))
	    
	    if (length(upload_cols) == 0) {
	      out <- from_results
	    } else {
	      out <- dplyr::left_join(
	        from_results,
	        from_upload[, c(key_col, upload_cols), drop = FALSE],
	        by = key_col
	      )
	    }
	  }
	  
	  if (is.null(out) || ncol(out) <= 1) {
	    return(NULL)
	  }
	  
	  out
	}
	
# === Processing gene functions ===
	#' Detect Columns Matching a Pattern
	#'
	#' This function detects which columns in a dataframe contain values matching a specified pattern.
	#'
	#' @param data A data frame to search for matching patterns.
	#' @param pattern A character string specifying the regular expression pattern to match.
	#' @return A character vector of column names that match the pattern, or NA if none are found.
	#' @export
	#' @importFrom base grepl
	detect_pattern_column <- function(data, pattern) {
	  matching_columns <- vector("character")
	  
	  for (col_name in colnames(data)) {
	    if (any(grepl(pattern, data[[col_name]], perl = TRUE))) {
	      matching_columns <- c(matching_columns, col_name)
	    }
	  }
	  
	  if (length(matching_columns) == 0) {
	    return(NA)
	  } else {
	    return(matching_columns)
	  }
	}
	
	#' Check if a column name matches genome or organism identifiers
	#'
	#' This helper function checks if a column name is related to genome or organism identifiers.
	#' It matches case-insensitively against a set of known headers.
	#'
	#' @param column_name Character. A column name.
	#' 
	#' @return Logical. TRUE if the column name matches, FALSE otherwise.
	detect_genome_column <- function(column_name) {
	  valid_genome_headers <- c("Genome", "Genome ID", "Genome Name", 
	                            "Organism", "Organism ID", "Organism Name")
	  return(tolower(column_name) %in% tolower(valid_genome_headers))
	}
	
	#' Check if a column contains KO identifiers
	#'
	#' This helper function checks if a column contains KO (KEGG Orthology) identifiers.
	#' It does this by applying a regex pattern to the column's values.
	#'
	#' @param column Vector. A column from a data frame.
	#' @return Logical. TRUE if the column contains KO identifiers, FALSE otherwise.
	detect_ko_column <- function(column) {
	  ko_pattern <- "^K\\d{5}$"
	  
	  # Remove NA values before checking
	  return(any(grepl(ko_pattern, column[!is.na(column)])))
	}
	
	#' Check if Data Follows IMG Gene Cart Format
	#'
	#' This function checks if the data contains columns in IMG gene carts.
	#' It does this by checking if it has columns with headers 
	#' "gene_oid", "Genome ID", "Genome Name", and "KO". These headers are rare
	#' in other files.
	#' 
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data follows IMG format, FALSE otherwise.
	#' @export
	is_img_gene_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  required_columns <- c("gene_oid", "Genome ID", "Genome Name", "KO")
	  
	  return(all(required_columns %in% colnames(data)))
	}
	
	#' Check if Data Follows KAAS Format
	#'
	#' This function checks if the data contains columns in KAAS (.ko) files.
	#' It does this by checking if there are two columns.  
	#' The second column contains KO IDs, and the first column does not contain the 
	#' header "Genome" or "Organism". This rules out most files where 
	#' genome or organism identifiers are present as the first column.
	#'
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data follows KAAS format, FALSE otherwise.
	#' @export
	is_kaas_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Check if there are exactly two columns
	  if (ncol(data) != 2) return(FALSE)
	  
	  # Ensure that the second column contains KO IDs
	  if (!detect_ko_column(data[[2]])) return(FALSE)
	  
	  # Ensure the first column does not have a genome/organism identifier as a header
	  has_invalid_first_column <- detect_genome_column(colnames(data)[1])
	  
	  return(!has_invalid_first_column)
	}
	
	#' Check if Data Follows PICRUSt Format
	#'
	#' This function checks if the data contains columns from the KO_predicted.tsv
	#' file of PICRUSt2.  It does this by checking if the header for the first
	#' column is "sequence" and the remaining headers are KO IDs.
	#' 
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data follows PICRUSt format, FALSE otherwise.
	#' @export
	is_picrust_format <- function(data) {
	  # Check if the first column header contains "sequence"
	  if (!is.data.frame(data)) return(FALSE)
	  if (!"sequence" %in% colnames(data)) return(FALSE)
	  
	  # Check if column headers are KO IDs
	  ko_pattern <- "^K\\d{5}$"
	  ko_cols <- colnames(data)[grepl(ko_pattern, colnames(data))]
	  if (length(ko_cols) == 0) return(FALSE)
	  
	  # Check if all KO columns are numeric
	  all_numeric <- all(sapply(data[, ko_cols, drop = FALSE], is.numeric))
	  
	  return(all_numeric)
	}
	
	#' Check if Data Follows HUMAnN Format
	#'
	#' This function checks if the data contains columns from the genefamilies_ko.tsv
	#' file of HUMAnN  It does this by checking if the header for the first
	#' column contains "HUMAnN".  It also checks if the values of the first column
	#' have KO IDs.
	#'
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data follows KAAS format, FALSE otherwise.
	#' @export
	is_humann_format <- function(data) {
	  # Check if the first column header contains "HUMAnN"
	  if (!is.data.frame(data)) return(FALSE)
	  if (!grepl(pattern = "HUMAnN", x = colnames(data)[1])) return(FALSE)
	  
	  # Check if the first column contains KO IDs
	  if (!detect_ko_column(data[[1]])) return(FALSE)
	  
	  return(TRUE)
	}
	
	#' Check if Data Follows eggNOG Format
	#'
	#' This function checks if the data follows eggNOG-mapper format.
	#' It does this by checking if the header for the first
	#' column contains "KEGG_ko ".  It also checks if this column has KO IDs.
	#'
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data appears to follow eggNOG format, FALSE otherwise.
	#' @export
	is_eggnog_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  if (nrow(data) < 1 || ncol(data) < 2) return(FALSE)
	  
	  # Check for KEGG_ko column
	  if (!"KEGG_ko" %in% colnames(data)) return(FALSE)
	  
	  # Extract and clean KO values
	  ko_values <- data[["KEGG_ko"]]
	  ko_values <- ko_values[!is.na(ko_values) & ko_values != ""]
	  
	  ko_ids <- unlist(strsplit(ko_values, ","))
	  ko_ids <- stringr::str_trim(ko_ids)
	  
	  return(any(grepl("^ko:K\\d{5}$", ko_ids)))
	}
	
	#' Check if Data Has One Column with KO IDs
	#'
	#' This function checks if the data contains at least one column of KO IDs.
	#'
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data has exactly one KO ID column, FALSE otherwise.
	#' @export
	is_single_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Detect all KO columns by checking their values
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Check if there is exactly one KO column
	  return(sum(ko_columns) == 1)
	}
	
	#' Check if Data Has Multiple Columns with KO IDs
	#'
	#' This function checks if the data has multiple columns of KO IDs.
	#'
	#' @param data A data frame. The data to check.
	#' @return Logical. TRUE if the data has multiple KO ID columns, FALSE otherwise.
	#' @export
	is_multi_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(FALSE)
	  
	  # Detect all KO columns by checking their values
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Check if there are multiple KO columns
	  return(sum(ko_columns) > 1)
	}
	
	#' Process Data Following IMG Format
	#'
	#' This function extracts the "Genome Name" and "KO" columns from an IMG gene cart dataset,
	#' then pivots the data so that each genome name becomes a column header with KO IDs as values.
	#' It also extracts clean KO IDs from strings like "KO:K00001".
	#'
	#' @param data A data frame. The data to process.
	#' @return A data frame where genome names are column headers, and KO IDs are values.
	#' @export
	process_img_gene_format <- function(data) {
	  if (is_img_format(data)) return(NULL)
	  
	  # Select relevant columns and remove missing values
	  processed_data <- data |>
	    dplyr::select(`Genome Name`, KO) |>
	    dplyr::filter(!is.na(KO) & KO != "")
	  
	  # Extract only KO IDs
	  processed_data <- processed_data |>
	    dplyr::mutate(KO = stringr::str_extract(KO, "(?<=KO:)K\\d{5}")) |>
	    dplyr::filter(!is.na(KO))
	  
	  # Convert to tibble for tidyr functions
	  processed_data <- tibble::as_tibble(processed_data)
	  
	  # Pivot wider: Genome Names become column headers, KO IDs as values
	  processed_data <- processed_data |>
	    dplyr::group_by(`Genome Name`) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>  # Create row index to prevent duplication collapse
	    tidyr::pivot_wider(names_from = `Genome Name`, values_from = KO) |>
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	#' Process Data Following KAAS Format
	#'
	#' This function extracts KO identifiers from a KAAS-formatted dataset,
	#' assigns "Organism" as the genome column header, and removes rows without KO IDs.
	#'
	#' @param data A data frame. The data to process.
	#' @return A data frame containing the standard "Genome" and "KO" columns.
	#' @export
	process_kaas_format <- function(data) {
	  if (!is_kaas_format(data)) return(NULL)
	  
	  # Extract only the KO ID column (second column)
	  processed_data <- data[, 2, drop = FALSE]
	  
	  # Remove rows where KO is missing or empty
	  processed_data <- processed_data[processed_data[[1]] != "", , drop = FALSE]
	  processed_data <- processed_data[!is.na(processed_data[[1]]), , drop = FALSE]
	  
	  # Assign standard column names
	  colnames(processed_data) <- c("KO")
	  
	  # Add "Organism" as the first column
	  processed_data <- cbind(Genome = "Organism", processed_data)
	  
	  # Pivot wider: "Organism" as the column header, KO IDs as values
	  processed_data <- processed_data |>
	    dplyr::group_by(Genome) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) |>
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	#' Process Data Following PICRUSt Format
	#'
	#' This function extracts KO identifiers from a PICRUSt-formatted dataset, 
	#' then pivots the data wider so that each genome becomes a column header 
	#' with KO IDs as values.
	#'
	#' @param data A data frame. The PICRUSt-style input data.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	process_picrust_format <- function(data) {
	  if (!is_picrust_format(data)) return(NULL)
	  
	  # Remove columns that have gene count of 0
	  processed_data <- data
	  processed_data <- processed_data[, c(TRUE, colSums(processed_data[,-1]) != 0)] 
	  
	  # Set non-zero column values equal to KO ID
	  processed_data <- processed_data |>
	    dplyr::mutate(across(-sequence, ~ ifelse(. != 0, dplyr::cur_column(), NA_character_)))
	  
	  # Pivot longer
	  processed_data <- processed_data |>
	    tidyr::pivot_longer(
	      cols = -sequence,
	      names_to = "KO",
	      values_to = "value"
	    ) |>
	    dplyr::filter(!is.na(value)) |>
	    dplyr::select(sequence, KO)
	  
	  # Pivot wider to match output style
	  processed_data <- processed_data |>
	    dplyr::group_by(sequence) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>
	    tidyr::pivot_wider(names_from = sequence, values_from = KO) |>
	    dplyr::select(-row_id)
	  
	  return(processed_data)
	}
	
	#' Process Data Following HUMAnN Format
	#'
	#' This function extracts KO and genome IDs from a HUMAnN-formatted dataset, 
	#' then pivots the data wider so that each genome becomes a column header 
	#' with KO IDs as values.
	#'
	#' @param data A data frame. The HUMAnN input data.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	process_humann_format <- function(data) {
	  if (!is_humann_format(data)) return(NULL)
	  
	  # Remove rows with no genome or KO ID
	  processed_data <- data
	  processed_data <- processed_data |>
	    dplyr::filter(grepl("\\|", .data[[1]]) & grepl("^K\\d{5}\\|", .data[[1]]))
	  
	  # Separate KO ID and genome columns
	  processed_data <- processed_data |> 
	    tidyr::separate(
	      col = 1,
	      into = c("KO", "Genome"),
	      sep = "\\|",
	      remove = TRUE
	    ) |>
	    dplyr::select("KO", "Genome")
	  
	  # Remove unclassified
	  processed_data <- processed_data |> 
	    dplyr::filter(Genome!="unclassified")
	  
	  # Pivot to wide format: KO IDs as values, genomes as columns
	  processed_data <- processed_data |>
	    dplyr::group_by(Genome) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>  # prevent duplicate KO collapse
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) |>
	    dplyr::select(-row_id)
	  
	  return(processed_data)
	}
	
	#' Process Data Following eggNOG Format
	#'
	#' This function extracts KO identifiers from an eggNOG-mapper dataset,
	#' assigns "Organism" as the genome column header, and removes rows without KO IDs.
	#' It also handles cases where multiple KO IDs are comma-delimited in a single field.
	#'
	#' @param data A data frame. The data to process.
	#' @return A data frame with KO identifiers expanded and organized under a single "Organism" column.
	#' @export
	process_eggnog_format <- function(data) {
	  if (!is_eggnog_format(data)) return(NULL)
	  
	  # Extract the KEGG_ko column
	  ko_data <- data[["KEGG_ko"]]
	  
	  # Remove NA or empty entries
	  ko_data <- ko_data[!is.na(ko_data) & ko_data != ""]
	  
	  # Expand comma-delimited KO IDs (e.g., "ko:K00001,ko:K00002")
	  ko_split <- unlist(strsplit(ko_data, ","))
	  
	  # Trim whitespace and remove invalid entries
	  ko_clean <- stringr::str_trim(ko_split)
	  ko_clean <- ko_clean[grepl("^ko:K\\d{5}$", ko_clean)]
	  
	  # Remove "ko:" prefix
	  ko_clean <- sub("^ko:", "", ko_clean)
	  
	  if (length(ko_clean) == 0) return(NULL)
	  
	  # Build dataframe with "Organism" column
	  processed_data <- data.frame(
	    Genome = "Organism",
	    KO = ko_clean,
	    stringsAsFactors = FALSE
	  )
	  
	  # Pivot to wide format: Genome as column header, KO as values
	  processed_data <- processed_data |>
	    dplyr::group_by(Genome) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) |>
	    dplyr::select(-row_id)
	  
	  return(processed_data)
	}
	
	#' Process Data Following Single-KO Format
	#'
	#' This function extracts a single KO column from a dataset and assigns a default
	#' genome name ("Organism") if no genome column is found. The data is then pivoted
	#' wider so that each genome becomes a column header with KO IDs as values.
	#'
	#' @param data A data frame. The data to process.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	process_single_ko_format <- function(data) {
	  # Detect KO-related columns
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Detect genome-related columns
	  genome_columns <- sapply(colnames(data), detect_genome_column)
	  
	  if (any(genome_columns)) {
	    # There is a genome column, keep it
	    processed_data <- data[, genome_columns | ko_columns, drop = FALSE]
	  } else {
	    # No genome column detected, assume a single organism
	    processed_data <- data[, ko_columns, drop = FALSE]
	    processed_data <- cbind(Genome = "Organism", processed_data)
	  }
	  
	  colnames(processed_data) <- c("Genome", "KO")  # Standardize column names
	  
	  # Convert to tibble for tidyr functions
	  processed_data <- tibble::as_tibble(processed_data)
	  
	  # Pivot wider: Genome Names become column headers, KO IDs as values
	  processed_data <- processed_data |>
	    dplyr::group_by(Genome) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>
	    tidyr::pivot_wider(names_from = Genome, values_from = KO) |>
	    dplyr::select(-row_id)  # Remove the row index column
	  
	  return(processed_data)
	}
	
	#' Process Data Following Multi-KO Format
	#'
	#' This function extracts columns that contain KO identifiers from a dataset
	#' that follows the multi-KO format.
	#'
	#' @param data A data frame. The data to process.
	#' @return A data frame containing only columns with KO identifiers.
	#' @export
	process_multi_ko_format <- function(data) {
	  if (!is.data.frame(data)) return(NULL)
	  
	  # Identify columns that contain KO IDs
	  ko_columns <- sapply(data, detect_ko_column)
	  
	  # Subset data to keep only KO ID columns
	  return(data[, ko_columns, drop = FALSE])
	}
	#' Process Uploaded Gene Functions File
	#'
	#' This function processes an uploaded gene functions file.  
	#' The type of processing depends on the file type detected.
	#' 
	#' @param gene_functions A data frame. The gene functions data.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	process_uploaded_gene_functions <- function(gene_functions) {
	  if (!is.data.frame(gene_functions)) return(NULL)
	  
	  if (is_img_gene_format(gene_functions)) {
	    gene_functions <- process_img_gene_format(gene_functions)
	  } else if (is_humann_format(gene_functions)) {
	    gene_functions <- process_humann_format(gene_functions)
	  } else if (is_picrust_format(gene_functions)) {
	    gene_functions <- process_picrust_format(gene_functions)
	  } else if (is_kaas_format(gene_functions)) {
	    gene_functions <- process_kaas_format(gene_functions)
	  } else if (is_eggnog_format(gene_functions)) {
	    gene_functions <- process_eggnog_format(gene_functions)
	  } else if (is_single_ko_format(gene_functions)) {
	    gene_functions <- process_single_ko_format(gene_functions)
	  } else if (is_multi_ko_format(gene_functions)) {
	    gene_functions <- process_multi_ko_format(gene_functions)
	  } else {
	    return(NULL)
	  }
	  
	  return(gene_functions)
	}
	
	#' Process Gene Functions from the Database
	#'
	#' This function processes gene functions for selected organisms, filtering and reshaping the data accordingly.
	#'
	#' @param gene_functions A data frame. The gene functions data.
	#' @param organism_by_genome A data frame. The data frame mapping organisms to genomes.
	#' @param selected_organisms A character vector. The organisms selected by the user.
	#' @return A data frame with gene functions for the selected organisms, with renamed columns to use organism names.
	#' @export
	#' @importFrom dplyr filter group_by mutate select
	#' @importFrom tidyr pivot_wider
	process_database_gene_functions <- function(gene_functions, organism_by_genome, selected_organisms) {
	  filtered_data <- organism_by_genome |>
	    dplyr::filter(Organism %in% selected_organisms)
	  
	  filtered_data$Genome <- as.character(filtered_data$Genome)
	  
	  gene_functions <- gene_functions |>
	    dplyr::filter(Genome %in% filtered_data$Genome) |>
	    dplyr::group_by(Genome) |>
	    dplyr::mutate(row_id = dplyr::row_number()) |>
	    tidyr::pivot_wider(names_from = Genome, values_from = Database_ID) |>
	    dplyr::select(-row_id) |>
	    dplyr::select(all_of(filtered_data$Genome))
	  
	  colnames(gene_functions) <- filtered_data$Organism
	  
	  gene_functions <- as.data.frame(gene_functions)
	  
	  return(gene_functions)
	}
	
	#' Get Selections for Organisms from Uploaded File
	#'
	#' This function reads a file of uploaded organism names, validates it, trims whitespace,
	#' and filters to keep only valid choices present in the database.
	#'
	#' @param upload_file Character. Path to the uploaded file.
	#' @param choices A character vector of valid organism names to match against.
	#' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @return A character vector of valid organism names from the upload file.
	#' @export
	get_uploaded_organism_selections <- function(upload_file, choices, session = getDefaultReactiveDomain()) {
	  if (is.null(upload_file) || !file.exists(upload_file)) {
	    return(character(0))
	  }
	  
	  uploaded <- validate_and_read_file(file_path = upload_file, session = session)
	  
	  if (is.null(uploaded)) {
	    return(character(0))
	  }
	  
	  # Handle cases: vector, single-column data frame, or coercible types
	  if (is.character(uploaded)) {
	    organism_names <- uploaded
	  } else if (is.data.frame(uploaded) && ncol(uploaded) >= 1) {
	    organism_names <- uploaded[[1]]
	  } else {
	    run_validation_modal(session = session, "Please check the format of your file and try again")
	    return(NULL)
	  }
	  
	  organism_names <- trimws(organism_names)
	  valid_names <- organism_names[organism_names %in% choices]
	  
	  return(valid_names)
	}
	
	
	#' Get Gene Functions from the Database
	#'
	#' This function retrieves gene function annotations from the database for the selected organisms.
	#' It loads the database, gene function table, and organism-to-genome mappings, and processes the
	#' gene functions to reshape them into a wide format where each column corresponds to an organism.
	#'
	#' @param selected_organisms A character vector. A list of organism names selected by the user.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	#'
	#' @seealso [process_database_gene_functions()], [load_data()], [get_organism_by_genome()]
	get_gene_functions_from_database <- function(selected_organisms)
	{
	  database <- load_database()
	  gene_functions <- load_data("gene_functions")
	  organism_by_genome <- get_organism_by_genome(database = database)
	  gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)
	  
	  return(gene_functions)
	}
	
	#' Get Gene Functions from Uploaded File
	#'
	#' This helper function reads and processes a gene functions file uploaded by the user.
	#' It first validates and reads the file based on its extension, and then processes it according
	#' to the detected format (IMG, KAAS, single KO column, or multi KO column).
	#'
	#' @param upload_path A character string. Path to the uploaded file.
	#' @param session The Shiny session object. Defaults to the current reactive domain.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	#' @export
	get_gene_functions_from_upload <- function(upload_path, session = shiny::getDefaultReactiveDomain()) {
	  gene_functions <- validate_and_read_file(file_path = upload_path, session = session)
	  gene_functions <- process_uploaded_gene_functions(gene_functions)
	  return(gene_functions)
	}
	
	#' Get Gene Functions
	#'
	#' Gets gene functions from either a database or an uploaded file.
	#'
	#' @param functions_from_database Logical. Use database gene functions?
	#' @param functions_from_upload Logical. Use uploaded gene functions?
	#' @param selected_organisms A character vector of selected organism names.
	#' @param upload_path File path to uploaded gene functions.
	#' @return A data frame of gene functions with organisms as column headers and KO IDs as values.
	get_gene_functions <- function(functions_from_database,
	                               functions_from_upload,
	                               selected_organisms = NULL,
	                               upload_path = NULL) {
	  if (functions_from_database) {
	    gene_functions <- get_gene_functions_from_database(selected_organisms)
	    run_validation_modal(need(gene_functions != "", "Please choose at least one organism."))
	  } else if (functions_from_upload) {
	    gene_functions <- get_gene_functions_from_upload(upload_path = upload_path)
	    run_validation_modal(need(gene_functions != "", "Please check the format of your predicted gene functions file and try again."))
	  } else {
	    stop("No valid source for gene functions specified.")
	  }
	  return(gene_functions)
	}
  
# === Processing query strings ===  
  #' Process a Query String for More Precise Matching
  #'
  #' This function updates a query string generated by a query builder to ensure 
  #' that all `grepl()` expressions match only standalone values or values separated by semicolons.
  #' It modifies **all** occurrences of `grepl()` in the input query by adding regex lookarounds 
  #' (`(?<=^|;)` and `(?=;|$)`) and enables Perl-compatible regular expressions (`perl = TRUE`).
  #'
  #' ## Effect of the Modification
  #' - The modified pattern will now **only match** if the search term:
  #'   - Appears at the **start of the string**.
  #'   - Appears at the **end of the string**.
  #'   - Is **preceded and/or followed by a semicolon (`;`)**.
  #'   (e.g., `"butyrate"` will match `"acetate;butyrate;isobutyrate"`).
  #' - **Partial matches within words or substrings will not occur**.
  #' - **Does not match** if the search term is part of a longer word 
  #'   (e.g., `"butyrate"` will not match `"isobutyrate"`).
  #' - **Handles multiple `grepl()` statements** in a complex query while preserving logical operators (`&`, `|`).
  #'
  #' @param query_string A character string representing a complex query with one or more `grepl()` expressions. Each `grepl()` should be in the form `grepl("pattern", `column_name`)`.
  #' @return A modified query string where **all `grepl()` calls** include lookarounds to match 
  #'         only standalone occurrences of the search term or those separated by semicolons.
  #'
  #' @examples
  #' query_string <- "grepl(\"butyrate\", `End products`) & grepl(\"acetate\", `End products`)"
  #' new_query_string <- process_query_string(query_string)
  #' print(new_query_string)
  #' # Output: "grepl(\"(?<=^|;)butyrate(?=;|$)\", `End products`, perl = TRUE) & grepl(\"(?<=^|;)acetate(?=;|$)\", `End products`, perl = TRUE)"
  #'
  #' @export
  process_query_string <- function(query_string) {
    query_string <- gsub(
      pattern = "grepl\\(\"(.*?)\",\\s*(`[^`]+`|[[:alnum:]_\\.]+)\\)",  # match quoted column names with backticks or valid variable names
      replacement = "grepl(\"(?<=^|;)\\1(?=;|$)\", \\2, perl = TRUE)",
      x = query_string
    )
    
    return(query_string)
  }
  
  #' Get Query String
  #'
  #' Processes and validates the query string from the query builder input.
  #' Validation checks that the query is non-init after processing and that the
  #' query builder reports no rule errors (e.g. incomplete or invalid rules).
  #'
  #' @param query_string Raw query string from `input$query_builder`.
  #' @return A cleaned and validated query string.
  get_query_string <- function(query_string) {
    run_validation_modal(need(!is.null(query_string), "Please build a valid query."))

    query_string <- process_query_string(query_string)

    run_validation_modal(need(query_string != "", "Please build a valid query."))

    return(query_string)
  }

  #' Extract Variables from Query Builder Output
  #'
  #' This function extracts all variables enclosed in backticks from a query builder output string.
  #'
  #' @param query_string A character string containing a query with variables enclosed in backticks.
  #' @return A character vector of the extracted variable names.
  #' @export
  extract_query_var <- function(query_string) {
    # Extract variables between backticks
    variables <- stringr::str_extract_all(query_string, "`[^`]+`")[[1]]
    
    # Remove the backticks
    variables <- stringr::str_replace_all(variables, "`", "")
    
    return(variables)
  }
  
  #' Filter Data Based on Query String
  #'
  #' This function dynamically filters data based on a query string from jqbr::queryBuilderInput()
  #' The query string is evaluated and used to filter the data.
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A character string representing the filtering condition (e.g., `` `Gram_stain` == "positive" ``).
  #' @return A filtered data frame based on the query string condition.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang parse_expr
  filter_data_by_query <- function(data, query_string) {
    data_filtered <- data |> dplyr::filter(!!rlang::parse_expr(query_string))
    return(data_filtered)
  }
  
  #' Filter Data Excluding NA Values
  #'
  #' This function filters data, removing rows that have NA or "NA" values in the variables 
  #' extracted from the query string.  The query string is from jqr::queryBuilderInput().
  #'
  #' @param data A data frame containing the data to be filtered.
  #' @param query_string A character string representing the query filter (e.g., `` `Gram_stain` == "positive" ``).
  #' @return A filtered data frame that excludes rows where any extracted variable has NA or "NA" values.
  #' @export
  #' @importFrom dplyr filter
  #' @importFrom rlang sym
  filter_data_excluding_na <- function(data, query_string) {
    vars <- extract_query_var(query_string)
    data_all <- data
    for (var in vars) {
      data_all <- data_all |> dplyr::filter(!is.na(!!rlang::sym(var)) | !!rlang::sym(var) != "NA")
    }
    return(data_all)
  }
  
# === Handle taxonomy ===  
  #' Expand Taxonomy String into Vector
  #'
  #' Converts a Greengenes-compatible taxonomy string into a vector of taxonomic levels.
  #'
  #' @param tax_string A character string formatted as a Greengenes taxonomy, 
  #'   with levels separated by semicolons and prefixed with rank indicators (e.g., `d__`, `p__`).
  #' @param simplify_species Logical. If TRUE, only the species epithet (e.g., "coli" from "Escherichia coli") is returned
  #'   in the species position. Default is TRUE.
  #' @return A character vector containing the taxonomic levels without prefixes. If `simplify_species` is TRUE, the species 
  #'   level contains only the epithet.
  #' @examples
  #' tax_string <- "d__Bacteria;p__Abditibacteriota;c__Abditibacteriia;o__Abditibacteriales;f__Abditibacteriaceae;g__Abditibacterium;s__Abditibacterium utsteinense"
  #' expand_taxonomy(tax_string, simplify_species = TRUE)
  #' @export
  expand_taxonomy <- function(tax_string, simplify_species = TRUE) {
    # Split the taxonomy string by semicolons
    tax_vector <- unlist(strsplit(tax_string, ";"))
    
    # Remove the rank prefixes (d__, p__, etc.) and extract only the names
    tax_vector <- sub("^[a-z]__*", "", tax_vector)
    
    # If simplify_species is TRUE, extract only the species epithet
    if (simplify_species && length(tax_vector) >= 7) {
      tax_vector[7] <- sub(".* ", "", tax_vector[7]) # Remove genus part, leaving only the species epithet
    }
    
    return(tax_vector)
  }
  
  #' Expand and Merge Taxonomy into Dataframe
  #'
  #' Expands taxonomic strings in a specified column of a given dataframe and
  #' merges the extracted taxonomic levels into that dataframe. If the required
  #' taxonomy columns (\"Domain\", \"Phylum\", \"Class\", \"Order\", \"Family\") already
  #' exist, the function skips processing. It also skips over any rows that are
  #' NA.
  #'
  #' @param data A data frame containing a column with taxonomic strings formatted as Greengenes-style taxonomy.
  #' @param col_name A string specifying the name of the column containing
  #'   the taxonomic strings.
  #' @param drop_species Logical; if TRUE (default), the \"Genus\" and \"Species\" columns are dropped from the result after expansion, preserving the original behaviour. Set to FALSE to retain the \"Species\" column (and \"Genus\") in the returned data frame, e.g. when species-level choices are needed for a dropdown.
  #' @return A data frame with additional columns for expanded taxonomy, specifically \"Domain\", \"Phylum\", \"Class\", \"Order\", and \"Family\". When \code{drop_species = TRUE} (default), \"Genus\" and \"Species\" are also removed. When \code{drop_species = FALSE}, all seven rank columns are retained.
  #' @export
  expand_and_merge_taxonomy <- function(data, col_name, drop_species = TRUE) {
    required_cols <- c("Domain", "Phylum", "Class", "Order", "Family")
    
    if (!all(required_cols %in% colnames(data))) {
      tax <- data[[col_name]] |>
        purrr::discard(is.na) |>
        purrr::map(expand_taxonomy) |>
        purrr::map_dfr(~purrr::set_names(as.list(.),
                                         c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")))
      
      if (drop_species) {
        tax <- dplyr::select(tax, -Genus, -Species)
      }
      
      cols_to_drop <- intersect(colnames(data), colnames(tax))
      data_trimmed <- data[!is.na(data[[col_name]]), 
                           !colnames(data) %in% cols_to_drop, 
                           drop = FALSE]
      
      data <- cbind(tax, data_trimmed)
    }
    
    return(data)
  }
 
# === Format organism names in plots === # 
  #' Concatenate selected columns into a single string
  #'
  #' This function merges multiple columns in a dataframe into a single column, 
  #' using a specified separator. It optimizes performance by applying transformations 
  #' only to unique rows and mapping them back to the full dataframe.
  #'
  #' @param df A data frame containing the columns to be merged.
  #' @param name_col A string specifying the name of the new column where the merged text will be stored.
  #' @param cols A character vector of column names to be concatenated.
  #' @param sep A string separator used to join elements. Default is a space (" ").
  #' @param na.rm Logical; if TRUE, removes NA values before merging. Default is TRUE.
  #' @param remove Logical; if TRUE, removes original columns after merging. Default is FALSE.
  #'
  #' @return A data frame with a new column containing the merged text.
  #' @export
  concatenate_columns <- function(df, name_col = "Organism name", cols, sep = " ", na.rm = TRUE, remove = FALSE) {
    if (!all(cols %in% names(df))) {
      stop("One or more specified columns do not exist in the dataframe.")
    }
    
    # Create a distinct map of unique rows and their collapsed values
    map <- df |>
      dplyr::distinct(dplyr::across(all_of(cols))) |>
      dplyr::mutate(
        !!name_col := tidyr::unite(dplyr::pick(dplyr::everything()), col = !!name_col, dplyr::all_of(cols), sep = sep, na.rm = na.rm, remove = FALSE)[[name_col]]
      ) |>
      dplyr::select(dplyr::all_of(cols), !!rlang::sym(name_col))
    
    # Merge the collapsed values back to the original dataframe
    df <- df |>
      dplyr::left_join(map, by = cols)
    
    # Remove original columns if requested
    if (remove) {
      df <- df |>
        dplyr::select(-dplyr::all_of(cols))
    }
    
    return(df)
  }
  
  #' Generate abbreviated organism names
  #'
  #' This function creates an abbreviated name for each unique organism entry in a dataframe.
  #' - If only one word is present, "sp." is appended.
  #' - If multiple words are present and the last word is lowercase, the last two words are returned.
  #' - Otherwise, the last word is returned with "sp." appended.
  #'
  #' This function optimizes performance by applying abbreviations only to unique values
  #' and then mapping them back to the original dataframe.
  #'
  #' @param df A data frame containing organism names.
  #' @param name_col A string specifying the column with organism names to be abbreviated.
  #'
  #' @return A data frame with `name_col` replaced by its abbreviated form.
  #' @export
  create_organism_abbreviations <- function(df, name_col) {
    if (!name_col %in% names(df)) {
      stop("The specified name_col does not exist in the dataframe.")
    }
    
    # Create a distinct map of unique names and their abbreviations
    map <- df |>
      dplyr::distinct(!!rlang::sym(name_col)) |>
      dplyr::mutate(
        Abbreviated_Name = purrr::map_chr(!!rlang::sym(name_col), function(text) {
          words <- unlist(strsplit(text, "\\s+"))
          n <- length(words)
          
          if (n == 0 || is.na(text)) {
            return(NA_character_)
          } else if (n == 1) {
            return(paste(words, "sp."))
          } else if (grepl("^[a-z]+$", words[n])) {
            return(paste(words[n - 1], words[n]))
          } else {
            return(paste(words[n], "sp."))
          }
        })
      )
    
    # Merge the abbreviation map back to the original dataframe
    df <- df |>
      dplyr::left_join(map, by = name_col) |>
      dplyr::select(-!!rlang::sym(name_col)) |>
      dplyr::rename(!!rlang::sym(name_col) := Abbreviated_Name)
    
    return(df)
  }
  
  #' Ensure unique organism names in a dataframe
  #'
  #' This function ensures that names in a specified column are unique by appending numeric suffixes when duplicates exist.
  #' If an index column is provided, names will be made unique only within the same index.
  #'
  #' @param df A data frame containing organism names.
  #' @param name_col A string specifying the column name containing organism names.
  #' @param index_col Optional; a string specifying the column to group uniqueness checks by. If NULL, names are made unique across the entire data frame.
  #' @return A data frame with unique names in the specified column.
  #' @export
  ensure_unique_names <- function(df, name_col, index_col = NULL) {
    if (!name_col %in% names(df)) {
      stop("The specified name_col does not exist in the dataframe.")
    }
    
    if (!is.null(index_col) && !index_col %in% names(df)) {
      stop("The specified index_col does not exist in the dataframe.")
    }
    
    # Create mapping of unique names within index_col
    map <- df |>
      dplyr::group_by(!!rlang::sym(index_col),
                      !!rlang::sym(name_col)) |>
      dplyr::distinct(!!rlang::sym(name_col), .keep_all = TRUE) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!rlang::sym(name_col)) |>
      dplyr::mutate(
        count = dplyr::n(),  # Count occurrences within group
        id = ifelse(count == 1, "", as.character(dplyr::row_number()))  # Assign "" if unique, else sequential number
      ) |>
      dplyr::ungroup() |>
      dplyr::select(!!rlang::sym(index_col), !!rlang::sym(name_col), id)
    
    # Apply numbers from the map back to the original dataframe
    df <- df |>
      dplyr::left_join(map, by = c(index_col, name_col)) |>
      dplyr::mutate(!!name_col := paste0(.data[[name_col]], ifelse(id == "", "", paste0(" ", id)))) |>
      dplyr::select(-id)  # Remove helper column
    
    return(df)
  }
  
  #' Format organism names from a selection of columns
  #'
  #' This function takes a dataframe and a selection of columns (e.g., `Phylum` to `Species`),
  #' collapses them into a single organism name (if `abbreviate_names = TRUE`), 
  #' creates an abbreviation, and ensures that names are unique **within the same organism number**.
  #'
  #' @param df A data frame containing taxonomy or organism name columns.
  #' @param cols A character vector of column names to be collapsed into an organism name.
  #' @param name_col A string specifying the new column name for the formatted organism names. Default is "Organism name".
  #' @param index_col A string specifying a column to ensure uniqueness within groups. Default is "Organism number".
  #' @param abbreviate_names Logical; if TRUE (default), collapses taxon columns and generates organism abbreviations. If FALSE, ensures uniqueness only.
  #'
  #' @return A data frame with a new column containing formatted and unique organism names.
  #' @export
  format_organism_names <- function(df, cols, name_col = "Organism name", index_col = "Organism number", abbreviate_names = TRUE) {
    if (!index_col %in% names(df)) {
      stop("The specified index_col does not exist in the dataframe.")
    }
    
    if (abbreviate_names) {
      if (!all(cols %in% names(df))) {
        stop("One or more specified columns do not exist in the dataframe.")
      }
      
      # Collapse taxon columns into a single string
      df <- concatenate_columns(df, name_col = name_col, cols = cols)
      
      # Create organism abbreviations
      df <- create_organism_abbreviations(df, name_col)
    }
    
    # Ensure names are unique
    df <- ensure_unique_names(df, name_col, index_col)
    
    return(df)
  }
  
# === Prepare data for tree plots ===
  #' Get the Tree Data Specification for a Module
  #'
  #' This function returns a description of what a module's results look like, which is everything
  #' the rest of this file needs to reduce them to one value per node.  Adding a module to the tree
  #' plot means adding an entry here rather than changing the functions that read it.
  #'
  #' The specification has four parts.  The match part says which columns can identify an organism,
  #' and so which column a tip label is matched against.  The variable part lists the columns the
  #' results are split by, in the order they are chosen; a module with two of them needs both fixed
  #' before a node has a single value.  The value part says which column holds the value and how to
  #' read it.  The format part says whether the results are one row per observation ("long") or one
  #' row per organism with the traits spread across columns ("wide").
  #'
  #' @param module_name The name of the module, as used in the modules folder.
  #' @return A list describing the results, or NULL when the module is not known.
  #' @export
  get_tree_data_spec <- function(module_name) {
    specs <- list(
      predictionsMachineLearning = list(
        module = "predictionsMachineLearning",
        format = "long",
        match_cols = c("Organism name"),
        var_dims = list(
          list(id = "var1", label = "Model", column = "Model")
        ),
        z_col = "Probability",
        z_mode = "probability",
        z_aggregate = "max",
        z_label = "Probability (%)"
      ),

      predictionsNetwork = list(
        module = "predictionsNetwork",
        format = "long",
        match_cols = c("Organism name"),
        # Two variables, so both a substrate and an end product must be chosen before a node has a
        # single flux.  Fixing only the substrate leaves one value per end product.
        var_dims = list(
          list(id = "var1", label = "Substrate", column = "Substrate"),
          list(id = "var2", label = "End product", column = "End product")
        ),
        z_col = "Flux",
        z_mode = "continuous",
        z_aggregate = "sum",
        z_label = "Flux"
      ),

      predictionsTaxonomy = list(
        module = "predictionsTaxonomy",
        format = "long",
        # There is no organism name in these results, so a tip is matched against a taxonomic rank
        # instead.  Which rank is a choice, because it decides how coarse the matching is.
        match_cols = c("Species", "Genus", "Family", "Order", "Class", "Phylum"),
        var_dims = list(
          list(id = "var1", label = "Trait category", column = "Trait category"),
          list(id = "var2", label = "Trait name", column = "Trait name")
        ),
        z_col = "Probability",
        z_mode = "probability",
        z_aggregate = "max",
        z_label = "Probability (%)"
      ),

      databaseSearch = list(
        module = "databaseSearch",
        format = "wide",
        match_cols = c("IMG Genome ID max quality", "IMG Genome ID", "GTDB ID",
                       "NCBI Taxonomy ID", "Species", "Genus", "Family", "Order",
                       "Class", "Phylum"),
        # The first variable is which trait column to read, and the second is which of its values to
        # look for.  Both are worked out from the results rather than fixed here, because the trait
        # columns differ with what the search returned.
        var_dims = list(
          list(id = "var1", label = "Trait", column = NULL),
          list(id = "var2", label = "Value", column = NULL)
        ),
        z_col = NULL,
        z_mode = "presence",
        z_aggregate = "max",
        z_label = "Present (%)",
        # Values are held as semicolon-separated lists, so a cell may name several traits at once.
        value_separator = ";",
        # Columns that never describe a trait, and so are never offered as one.
        non_trait_cols = c("Domain", "Phylum", "Class", "Order", "Family", "Genus",
                           "Species", "Subspecies", "Strain", "LPSN ID", "LPSN Taxonomy",
                           "GTDB ID", "GTDB Taxonomy", "GOLD Organism ID", "GOLD Project ID",
                           "NCBI Taxonomy ID", "NCBI Taxonomy", "IMG Genome ID",
                           "IMG Genome ID max quality", "BacDive ID", "Bergey Taxonomy")
      )
    )

    return(specs[[module_name]])
  }

  #' Get the Trait Columns of a Wide Set of Results
  #'
  #' This function lists the columns of a wide set of results that describe a trait, which are the
  #' ones worth offering as something to color a tree by.  Columns that identify or classify an
  #' organism are left out, as are columns that are entirely empty.
  #'
  #' @param df A data frame of results in wide format.
  #' @param spec A specification, as returned by get_tree_data_spec().
  #' @return A character vector of column names.
  #' @export
  get_tree_trait_columns <- function(df, spec) {
    # Check if the results are empty and return nothing if so
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(character(0))
    }

    candidates <- setdiff(names(df), spec$non_trait_cols)

    # Drop columns with nothing in them, which a search often returns several of
    keep <- vapply(candidates, function(column) {
      any(!is.na(df[[column]]) & trimws(as.character(df[[column]])) != "")
    }, logical(1))

    return(candidates[keep])
  }

  #' Split Values Held as a List in One Cell
  #'
  #' This function splits cells that hold several values separated by a semicolon, which is how the
  #' database stores traits an organism has more than one of.  A cell holding one value is returned
  #' as one value.
  #'
  #' @param values A character vector of cells.
  #' @param separator The character the values are separated by.
  #' @return A list of character vectors, one per cell.
  #' @export
  split_tree_values <- function(values, separator = ";") {
    values <- as.character(values)
    values[is.na(values)] <- ""

    split_values <- strsplit(values, separator, fixed = TRUE)

    return(lapply(split_values, function(value) trimws(value[trimws(value) != ""])))
  }

  #' Check Whether a Column Holds Numbers
  #'
  #' This function decides whether a column of a wide set of results should be read as a number,
  #' which decides whether its values are colored on a continuous scale or looked for by name.  A
  #' column counts as numeric when everything in it that is not empty reads as a number.
  #'
  #' @param values A vector of cells.
  #' @return TRUE when the column holds numbers.
  #' @export
  is_numeric_tree_column <- function(values) {
    if (is.numeric(values)) {
      return(TRUE)
    }

    filled <- as.character(values)
    filled <- filled[!is.na(filled) & trimws(filled) != ""]

    if (length(filled) == 0) {
      return(FALSE)
    }

    converted <- suppressWarnings(as.numeric(filled))

    return(!any(is.na(converted)))
  }

  #' Get the Column a Tip Label is Matched Against
  #'
  #' This function picks which column identifies an organism, which is the column a tip label is
  #' matched against.  It takes the first column named in the specification that the results
  #' actually have something in, so a module falls back to a coarser identifier when a finer one is
  #' missing.  Passing a column name overrides the choice.
  #'
  #' @param df A data frame of results.
  #' @param spec A specification, as returned by get_tree_data_spec().
  #' @param match_by A column to use instead of choosing one, or NULL to choose.
  #' @return The name of the column, or NA when none of them hold anything.
  #' @export
  get_tree_match_column <- function(df, spec, match_by = NULL) {
    if (!is.null(match_by) && !is.na(match_by) && match_by %in% names(df)) {
      return(match_by)
    }

    for (column in spec$match_cols) {
      if (column %in% names(df)) {
        values <- as.character(df[[column]])

        if (any(!is.na(values) & trimws(values) != "")) {
          return(column)
        }
      }
    }

    return(NA_character_)
  }

  #' Combine Several Values into One
  #'
  #' This function combines the values of rows that end up on the same tip, which happens whenever
  #' the matching is coarser than the results.  Matching taxonomy predictions at the genus level,
  #' for example, puts every species of a genus on one tip.
  #'
  #' @param z A numeric vector of values.
  #' @param method How to combine them: "max", "min", "mean", "sum", or "first".
  #' @return A single value.
  #' @export
  aggregate_tree_z <- function(z, method = "max") {
    z <- z[!is.na(z)]

    if (length(z) == 0) {
      return(NA_real_)
    }

    value <- switch(method,
                    "max" = max(z),
                    "min" = min(z),
                    "mean" = mean(z),
                    "sum" = sum(z),
                    "first" = z[1],
                    max(z))

    return(value)
  }

  #' Get the Range a Value is Colored Over
  #'
  #' This function returns the low and high ends of the color scale.  A probability is always
  #' colored over the whole of nought to a hundred, so that trees of different traits can be
  #' compared.  A flux has no fixed range, so its high end is taken from the values themselves and
  #' rounded up, which is what stops every node saturating at the top of the scale.
  #'
  #' @param z A numeric vector of values, after any conversion to a percentage.
  #' @param z_mode How the value is read: "probability", "presence", "continuous", or "numeric".
  #' @return A list with zmin and zmax.
  #' @export
  get_tree_z_range <- function(z, z_mode = "probability") {
    if (z_mode %in% c("probability", "presence")) {
      return(list(zmin = 0, zmax = 100))
    }

    finite_z <- z[is.finite(z)]

    # Check if there is nothing to take a range from and return a default if so
    if (length(finite_z) == 0) {
      return(list(zmin = 0, zmax = 1))
    }

    z_max <- max(finite_z)
    z_min <- min(0, min(finite_z))

    # Check if every value is the same and widen the range if so, since a scale of no width
    # colors every node the same
    if (z_max <= z_min) {
      return(list(zmin = z_min, zmax = z_min + 1))
    }

    return(list(zmin = z_min, zmax = signif(z_max, 2)))
  }

  #' Turn Module Results into Tree Data
  #'
  #' This function reduces a set of results to the one value per node that plot_tree() colors by.
  #' It is the single entry point the modules call, and it is the tree's counterpart to
  #' results_to_plot().
  #'
  #' Every module ends at the same place: a data frame with an x column holding the identifier a tip
  #' is matched against and a z column holding the value.  How it gets there differs.  Results split
  #' by two variables are filtered on both, since fixing only one leaves a node with several values.
  #' Results in wide format have no value column at all, so the value is whether the chosen trait
  #' holds the chosen value, unless the trait is a number, in which case the number is the value.
  #'
  #' The range the value is colored over is returned alongside it rather than assumed, because a
  #' probability runs from nought to a hundred while a flux runs to whatever it happens to reach.
  #'
  #' @param df A data frame of results.
  #' @param spec A specification, as returned by get_tree_data_spec().
  #' @param var_to_keep A named list of the value chosen for each variable, named by variable id.
  #'   A variable left out or set to NULL is not filtered on, which leaves several values per node.
  #' @param match_by The column a tip label is matched against, or NULL to choose one.
  #' @param z_aggregate How to combine rows landing on the same tip, or NULL to use the module's own
  #'   default.
  #' @param z_threshold A value below which the value is set to nought, matching what the other
  #'   plots do with the threshold slider.
  #' @return A data frame with x, z, and n columns, where n is how many rows were combined into each
  #'   value.  The color range, the column matched on, and the label of the value are attached as
  #'   the attributes z_range, match_column, and z_label.
  #' @export
  results_to_tree_data <- function(df, spec, var_to_keep = list(), match_by = NULL,
                                   z_aggregate = NULL, z_threshold = 0) {
    empty_result <- function(match_column = NA_character_) {
      out <- data.frame(x = character(0), z = numeric(0), n = integer(0),
                        stringsAsFactors = FALSE)
      attr(out, "z_range") <- list(zmin = 0, zmax = 100)
      attr(out, "match_column") <- match_column
      attr(out, "z_label") <- spec$z_label
      attr(out, "z_mode") <- spec$z_mode
      return(out)
    }

    # Check if the results are empty and return an empty result if so
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(empty_result())
    }

    # Get the column a tip is matched against
    match_column <- get_tree_match_column(df, spec, match_by = match_by)

    # Check if nothing identifies an organism and return an empty result if so
    if (is.na(match_column)) {
      return(empty_result())
    }

    if (is.null(z_aggregate)) {
      z_aggregate <- spec$z_aggregate
    }

    x <- as.character(df[[match_column]])
    z_mode <- spec$z_mode

    if (identical(spec$format, "wide")) {
      trait_column <- var_to_keep[["var1"]]

      # Check if no trait has been chosen and return an empty result if so
      if (is.null(trait_column) || is.na(trait_column) || !(trait_column %in% names(df))) {
        return(empty_result(match_column))
      }

      if (is_numeric_tree_column(df[[trait_column]])) {
        # A numeric trait is colored by its own values, over its own range
        z <- suppressWarnings(as.numeric(as.character(df[[trait_column]])))
        z_mode <- "numeric"
      } else {
        value_to_find <- var_to_keep[["var2"]]

        # Check if no value has been chosen and return an empty result if so
        if (is.null(value_to_find) || is.na(value_to_find)) {
          return(empty_result(match_column))
        }

        # A cell may hold several values, so look for the chosen one among them
        cell_values <- split_tree_values(df[[trait_column]], spec$value_separator)
        z <- vapply(cell_values, function(values) {
          if (length(values) == 0) NA_real_ else as.numeric(value_to_find %in% values) * 100
        }, numeric(1))

        z_mode <- "presence"
      }
    } else {
      # Filter on every variable that has been chosen.  Leaving one unchosen leaves a node with
      # several values, which the aggregation below then has to combine.
      keep <- rep(TRUE, nrow(df))

      for (dim in spec$var_dims) {
        chosen_value <- var_to_keep[[dim$id]]

        if (!is.null(chosen_value) && !is.na(chosen_value) && dim$column %in% names(df)) {
          keep <- keep & (as.character(df[[dim$column]]) == chosen_value)
        }
      }

      x <- x[keep]
      z <- suppressWarnings(as.numeric(df[[spec$z_col]][keep]))

      # A probability is stored from nought to one and colored from nought to a hundred
      if (identical(z_mode, "probability")) {
        z <- z * 100
      }
    }

    # Drop rows with nothing to match on, which a rank that is not filled in leaves behind
    keep <- !is.na(x) & trimws(x) != "" & x != "NA"
    x <- x[keep]
    z <- z[keep]

    # Check if nothing is left and return an empty result if so
    if (length(x) == 0) {
      return(empty_result(match_column))
    }

    # Set values below the threshold to nought, as the other plots do
    if (!is.null(z_threshold) && z_threshold > 0) {
      threshold <- if (z_mode %in% c("probability", "presence")) z_threshold * 100 else z_threshold
      z[!is.na(z) & z < threshold] <- 0
    }

    # Combine rows landing on the same tip
    grouped <- split(z, x)
    out <- data.frame(
      x = names(grouped),
      z = vapply(grouped, aggregate_tree_z, numeric(1), method = z_aggregate),
      n = vapply(grouped, length, integer(1)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    attr(out, "z_range") <- get_tree_z_range(out$z, z_mode = z_mode)
    attr(out, "match_column") <- match_column
    attr(out, "z_label") <- if (identical(z_mode, "numeric")) var_to_keep[["var1"]] else spec$z_label
    # The mode a value ends up read as is not always the one the specification names, since a trait
    # column that turns out to hold numbers is colored by its own values rather than looked for by
    # name.  Return the mode that was actually used so the caller can report it.
    attr(out, "z_mode") <- z_mode

    return(out)
  }
  
# === Other ===
  #' Collapse Dataframe Columns into a Single Column
  #'
  #' This function collapses a set of columns in a dataframe into a single column with 
  #' semi-colon separated values. The rows in the columns can contain user-defined 
  #' positive and negative values (e.g., "1" for presence and "0" for absence).
  #' The names of the columns will be the new values in the new column.
  #' These values will be semi-colon separated (e.g., column1;column2), 
  #' and values absent in the original column will be excluded.
  #'
  #' @param df A data frame containing the columns to be collapsed.
  #' @param cols A character vector of column names to collapse.
  #' @param new_col_name The name of the new column to store collapsed values.
  #' @param delete A string pattern to remove from column names during the collapse. Default is an empty string.
  #' @param positive_value The value that represents a "positive" presence (default is "1").
  #' @param negative_value The value that represents a "negative" absence (default is "0").
  #' @return A data frame with the specified columns collapsed into a single column.
  #' @export
  #' @importFrom dplyr rowwise mutate ungroup select
  #' @importFrom stringr str_replace
  collapse_columns <- function(df, cols, new_col_name, delete = "", positive_value = "1", negative_value = "0") {
    df[[new_col_name]] <- sapply(1:nrow(df), function(i) {
      col_values <- gsub(delete, "", cols)
      # Identify positive matches based on the positive value
      values <- col_values[df[i, cols] == positive_value]
      # Exclude NA and empty values
      values <- values[!is.na(values) & values != "NA"]
      if (length(values) > 0) {
        return(paste(values, collapse = ";"))
      } else {
        return(NA)
      }
    })
    df <- df[, !(colnames(df) %in% cols)]
    
    return(df)
  }
  
  #' Get Organism and Genome Information
  #'
  #' This function extracts organism names and their corresponding genome IDs
  #' from a cleaned database. The organism names are formatted as 
  #' "Genus Species Subspecies", with missing subspecies values properly handled.
  #'
  #' @param database A data frame containing organism data, including columns for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that contains genome IDs. Defaults to `"IMG Genome ID max quality"`.
  #' @param genus_col A character string specifying the column name that contains genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains subspecies information. Defaults to `"Subspecies"`.
  #' @param remove_na_genome A logical value indicating whether to remove organisms with NA genome IDs. Defaults to TRUE.
  #'
  #' @return A data frame with two columns: `Organism` and `Genome`.
  #' @examples
  #' # Example usage
  #' get_organism_by_genome(database_clean, 
  #'                        genome_id_col = "Genome_ID", 
  #'                        genus_col = "Genus_Name",
  #'                        species_col = "Species_Name",
  #'                        subspecies_col = "Subspecies_Name",
  #'                        remove_na_genome = TRUE)
  #'
  #' @importFrom dplyr filter mutate rename select
  #' @export
  get_organism_by_genome <- function(database, 
                                     genome_id_col = "IMG Genome ID max quality", 
                                     genus_col = "Genus", 
                                     species_col = "Species", 
                                     subspecies_col = "Subspecies",
                                     remove_na_genome = TRUE) {
    
    result <- database |>
      dplyr::mutate(Organism = paste(.data[[genus_col]], .data[[species_col]], .data[[subspecies_col]], sep = " ")) |>
      dplyr::mutate(Organism = gsub(" NA$", "", Organism)) |>
      dplyr::rename(Genome = dplyr::all_of(genome_id_col)) |>
      dplyr::select(Organism, Genome) 
    
    if (remove_na_genome) {
      result <- result |> dplyr::filter(!is.na(Genome) & Genome != "NA")
    }
    
    return(result)
  }
  
  #' Get Choices for Organisms
  #'
  #' This function gets names of organisms that have gene functions in
  #' the database. It can optionally filter organisms based on their 
  #' metabolism type and genome ID column.
  #'
  #' @param database A data frame containing organism data, including columns for genome IDs, genus, species, and subspecies.
  #' @param genome_id_col A character string specifying the column name that 
  #'   contains genome IDs. Defaults to `"IMG Genome ID max quality"`.
  #' @param genus_col A character string specifying the column name that contains 
  #'   genus information. Defaults to `"Genus"`.
  #' @param species_col A character string specifying the column name that contains 
  #'   species information. Defaults to `"Species"`.
  #' @param subspecies_col A character string specifying the column name that contains 
  #'   subspecies information. Defaults to `"Subspecies"`.
  #' @param filter_by_genome_id A logical indicating whether to filter out rows with 
  #'   missing or "NA" values in the `genome_id_col`. Defaults to `TRUE`.
  #'
  #' @return A character vector of organism names formatted as "Genus Species Subspecies".
  #'   Entries with missing values in `Subspecies` are cleaned to remove trailing "NA".
  #'   
  #' @examples
  #' # Example usage
  #' get_organism_choices(database_clean, 
  #'                      genome_id_col = "Genome_ID", 
  #'                      genus_col = "Genus_Name",
  #'                      species_col = "Species_Name",
  #'                      subspecies_col = "Subspecies_Name",
  #'                      filter_by_genome_id = FALSE)
  #'
  #' @importFrom dplyr filter mutate pull
  #' @export
  get_organism_choices <- function(database, 
                                   genome_id_col = "IMG Genome ID max quality", 
                                   genus_col = "Genus", 
                                   species_col = "Species", 
                                   subspecies_col = "Subspecies", 
                                   filter_by_genome_id = TRUE) {
    
    if (filter_by_genome_id) {
      database <- database |>
        dplyr::filter(.data[[genome_id_col]] != "NA" & !is.na(.data[[genome_id_col]]))
    }
    
    database |>
      dplyr::mutate(choices = paste(.data[[genus_col]], 
                                    .data[[species_col]], 
                                    .data[[subspecies_col]], sep = " ")) |>
      dplyr::mutate(choices = gsub(" NA$", "", choices)) |>
      dplyr::pull(choices)
  }
  
  #' Count Traits and Organisms with Predictions
  #'
  #' This function calculates the total number of unique traits and organisms in a dataset, 
  #' as well as the number of traits and organisms with predictions available at or above a specified threshold.
  #'
  #' @param df A data frame containing trait prediction data.
  #' @param organism_col A string specifying the column name that contains organism identifiers. Default is `"Organism number"`.
  #' @param trait_col A string specifying the column name that contains trait categories. Default is `"Trait category"`.
  #' @param value_col A string specifying the column name that contains the predicted probability values. Default is `"Probability"`.
  #' @param threshold A numeric value specifying the minimum probability required for a prediction to be considered valid.
  #' @return A list containing four values:
  #'   \item{traits_total}{Total number of unique traits in the dataset.}
  #'   \item{traits_predictions}{Number of unique traits with predictions above the threshold.}
  #'   \item{organisms_total}{Total number of unique organisms in the dataset.}
  #'   \item{organisms_predictions}{Number of unique organisms with predictions above the threshold.}
  #'
  #' @importFrom dplyr filter pull n_distinct
  #' @importFrom rlang sym
  #' @export
  count_predictions <- function(df, organism_col = "Organism number", trait_col = "Trait category", value_col = "Probability", threshold = 0.5) {
    # Get dataframe of all variables
    df_total <- df
    
    # Get dataframe of predicted variables
    df_predicted <- df |> dplyr::filter(!!rlang::sym(value_col) >= threshold)
    
    # Count organisms
    organisms_total <- df_total |> 
      dplyr::pull(organism_col) |> dplyr::n_distinct()
    organisms_predictions <- df_predicted |> 
      dplyr::pull(organism_col) |> dplyr::n_distinct()
    
    # Count traits
    traits_total <- df_total |> 
      dplyr::pull(trait_col) |> dplyr::n_distinct()
    traits_predictions <- df_predicted |> 
      dplyr::pull(trait_col) |> dplyr::n_distinct()
    
    return(
      list(
        traits_total = traits_total, 
        traits_predictions = traits_predictions, 
        organisms_total = organisms_total, 
        organisms_predictions = organisms_predictions)
    )
  }
  
  #' Format Summary Text for Predictions
  #'
  #' Generates a summary statement about the number of traits or end products predicted 
  #' for a given number of organisms.
  #'
  #' @param count1 Integer. The number of predicted traits or end products.
  #' @param count2 Integer. The number of organisms with predictions.
  #' @param label1 Character. The label for the predicted items (e.g., "traits", "end products").
  #' @param label2 Character. The label for the entities being predicted for (e.g., "organisms").
  #' @param total2 Integer (optional). The total number of organisms in the dataset.
  #' 
  #' @return A character string summarizing the predictions.
  #' @examples
  #' format_summary_text(5, 2, "traits", "organisms", 3)
  #' # Returns: "5 traits predicted for 2 of 3 organisms"
  #'
  #' format_summary_text(0, 3, "traits", "organisms", 3)
  #' # Returns: "No traits predicted for 3 organisms"
  #'
  #' @export
  format_summary_text <- function(count1, count2, label1, label2, total2 = NULL) {
    label1_singular <- sub("s$", "", label1)  # Convert to singular form if needed
    label2_singular <- sub("s$", "", label2)  # Convert to singular form if needed
    
    string <- NULL
    
    # First part (e.g., "5 traits predicted for ")
    if(count1 > 1){
      string <- paste0(count1, " ", label1, " predicted for ")
    }else if(count1 == 1){
      string <- paste0(count1, " ", label1_singular, " predicted for ")
    }else if(count1 == 0){
      string <- paste0("No ", label1, " predicted for ")
    }
    
    # Second part (e.g., "2 organisms")
    if(count2 > 1){
      string <- paste0(string, count2, " ", label2)
    } else if(count2 == 1){
      string <- paste0(string, count2, " ", label2_singular)
    } else if(count2 == 0 & total2 > 1){
      string <- paste0(string, total2, " ", label2)
    } else if(count2 == 0 & total2 == 1){
      string <- paste0(string, total2, " ", label2_singular)
    } 
    
    # Third part (e.g., "(No predictions for 1 more organism"))
    if(count1 > 0 & total2 > 1 & (total2 - count2)>1){
      string <- paste0(string, " (", total2 - count2, " more ", label2, " had no predictions)")
    }
    
    if(count1 > 0 & total2 > 1 & (total2 - count2)==1){
      string <- paste0(string, " (", total2 - count2, " more ", label2_singular, " had no predictions)")
    }
    
    return(string)
  }
  
  #' Get Unpredicted Choices
  #'
  #' Identifies values in a category column that have no predicted values at or
  #' above a specified threshold. Can be used for traits, substrates, or any
  #' other categorical variable in a prediction dataframe.
  #'
  #' @param df A data frame containing prediction data.
  #' @param choices_col A string specifying the column name that contains the
  #'   categories to evaluate (e.g., `"Trait category"`, `"Substrate"`).
  #' @param value_col A string specifying the column name that contains predicted
  #'   values (e.g., `"Probability"`, `"Flux"`).
  #' @param threshold A numeric value specifying the minimum value required for a choice to be considered predicted.
  #' @return A character vector of category values for which no predictions meet
  #'   or exceed the threshold.
  #'
  #' @examples
  #' get_unpredicted_choices(df, choices_col = "Trait category",
  #'                         value_col = "Probability", threshold = 0.5)
  #' get_unpredicted_choices(df, choices_col = "Substrate",
  #'                         value_col = "Flux", threshold = 1)
  #'
  #' @importFrom dplyr filter pull
  #' @importFrom rlang sym
  #' @export
  get_unpredicted_choices <- function(df, choices_col, value_col, threshold) {
    predicted <- df |>
      dplyr::filter(!!rlang::sym(value_col) >= threshold) |>
      dplyr::pull(choices_col) |>
      unique()
    
    all_choices <- df |> dplyr::pull(choices_col) |> unique()
    
    setdiff(all_choices, predicted)
  }
  
  #' Format Choices for Picker Input
  #'
  #' Builds a named choices vector and choicesOpt list for use with
  #' `shinyWidgets::pickerInput` or `update_picker_input`. Choices that match
  #' `unpredicted` are labeled in grey italic with `(<label>)` appended.
  #'
  #' @param all_choices A character vector of all choice values.
  #' @param unpredicted A character vector of choice values that are not predicted.
  #' @param label A character string appended in parentheses to unpredicted choices. Default is `"not predicted"`.
  #' @return A list with two elements:
  #'   \item{choices}{Named character vector where names are display labels and values are original choice values.}
  #'   \item{choicesOpt}{A list with a `content` element containing HTML strings for styling.}
  #'
  #' @examples
  #' fmt <- format_picker_choices(all_choices, unpredicted)
  #' update_picker_input(inputId = "trait_to_display",
  #'                     choices = fmt$choices,
  #'                     choicesOpt = fmt$choicesOpt)
  format_picker_choices <- function(all_choices, unpredicted, label = "not predicted") {
    is_unpredicted <- all_choices %in% unpredicted
    
    choices <- setNames(
      all_choices,
      ifelse(is_unpredicted,
             paste0(all_choices, " (", label, ")"),
             all_choices)
    )
    
    choicesOpt <- list(
      content = ifelse(
        is_unpredicted,
        paste0('<span class="trait-not-predicted">', all_choices, ' [', label, ']</span>'),
        all_choices
      )
    )
    
    list(choices = choices, choicesOpt = choicesOpt)
  }
  
  #' Extract file from a ZIP
  #'
  #' Looks inside a .zip and returns the path to the first file that matches a
  #' supported data extension. Creates a temporary directory for extraction and
  #' returns it so the caller can clean up afterward.
  #'
  #' @param zip_path Path to a .zip file.
  #' @param pattern  Regex of supported extensions inside the zip.
  #' @return A list with elements: path (character), extension (character), temp_dir (character).
  #' @keywords internal
  extract_from_zip <- function(
    zip_path,
    pattern = "\\.(csv|tsv|txt|xls|xlsx|rds|ko)$"
  ) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    utils::unzip(zip_path, exdir = temp_dir)
    extracted_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
    valid_files <- extracted_files[grepl(pattern, extracted_files, ignore.case = TRUE)]
    
    if (length(valid_files) == 0) {
      unlink(temp_dir, recursive = TRUE, force = TRUE)
      stop("No CSV, TSV, TXT, XLS, XLSX, KO, or RDS files found in the ZIP archive.")
    }
    
    path <- valid_files[1]
    list(path = path, extension = tools::file_ext(path), temp_dir = temp_dir)
  }
  
  #' Save Data in Original Format and Zip It
  #'
  #' This function saves a dataframe in its original format (e.g., CSV, RDS), 
  #' compresses it into a ZIP archive, and optionally removes the original file.
  #'
  #' @param data A data frame to be saved.
  #' @param fp A character string specifying the full file path, including the original extension (e.g., "data/file.csv").
  #' @param remove_original A logical value indicating whether to delete the original file after zipping. Default is `TRUE`.
  #' @param overwrite A logical value indicating whether to overwrite an existing ZIP file. Default is `TRUE`.
  #' 
  #' @return The function does not return anything. It writes the file to disk.
  #' 
  #' @export
  save_as_zip <- function(data, fp, remove_original = TRUE, overwrite = TRUE) {
    # Ensure the zip package is available
    if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required. Install it with install.packages('zip')")
    
    # Normalize the file path
    fp <- normalizePath(fp, winslash = "/", mustWork = FALSE)
    
    # Extract file name, extension, and directory
    ext <- tools::file_ext(fp)
    if (ext == "") stop("File path must include an extension (e.g., .csv, .rds).")
    
    file_name <- basename(fp)  # e.g., "database_clean.csv"
    dir_name <- dirname(fp)    # e.g., "data"
    
    # Create a folder with the same name as the object
    object_name <- tools::file_path_sans_ext(file_name)  # e.g., "database_clean"
    zip_folder <- file.path(dir_name, object_name)
    
    # Ensure the folder is clean
    if (dir.exists(zip_folder)) unlink(zip_folder, recursive = TRUE)
    dir.create(zip_folder)
    
    # Define paths
    save_fp <- file.path(zip_folder, file_name)  # Save inside the new folder
    zip_fp <- file.path(dir_name, paste0(object_name, ".zip"))  # ZIP file in same directory
    
    # Save file based on extension
    switch(ext,
           "csv" = write.csv(data, save_fp, row.names = FALSE),
           "rds" = saveRDS(data, save_fp),
           stop("Unsupported file format: ", ext)
    )
    
    # Overwrite existing ZIP file if specified
    if (overwrite && file.exists(zip_fp)) file.remove(zip_fp)
    
    # Create ZIP file using zip::zipr()
    zip::zipr(zip_fp, files = save_fp, recurse = FALSE, compression_level = 9)
    
    # Ensure the ZIP file was created before deleting the original
    if (file.exists(zip_fp) && remove_original) unlink(zip_folder, recursive = TRUE)
  }
  
  #' Build a hovertemplate from a Config String
  #'
  #' Hovertemplates in plot-config CSVs are stored as either literal strings or
  #' sprintf-style templates with one `%s` container for a user-selected label
  #' (e.g. the trait or substrate the user is viewing). This helper picks the
  #' right behavior based on whether the template contains an unescaped `%s`.
  #'
  #' Note on escaping: when a CSV value contains `%s`, the literal `%`
  #' characters used by plotly format specifiers (e.g. `%{x}`) must be escaped
  #' as `%%` in the CSV, because the value will be passed through `sprintf()`.
  #' Templates without `%s` are returned verbatim and need no escaping.
  #'
  #' @param template A character string. Either a literal hovertemplate (used
  #'   as-is) or a sprintf template containing `%s` (substituted).
  #' @param label The label to substitute into a `%s` container. Ignored if the template contains no `%s`.
  #' @return A character string ready to pass as `hovertemplate` to plotly.
  build_hovertemplate <- function(template, label = NULL) {
    if (is.null(template)) return(NULL)
    if (grepl("%s", template, fixed = TRUE) && !is.null(label)) {
      sprintf(template, label)
    } else {
      template
    }
  }
  
  #' Get Configuration for Plots
  #'
  #' This function loads the configuration for plots in the app.  It first loads
  #' `data/config/plot_var.csv` and returns the parsed configuration for a single
  #' `module_name` + `plot_type` combination as a named list. Values are
  #' auto-coerced: empty strings become `NULL`, `"TRUE"`/`"FALSE"` become
  #' logicals, all-numeric strings become numeric, semicolon-delimited strings
  #' become character vectors (or numeric vectors if all elements are numeric),
  #' and everything else is kept as a character string.
  #'
  #' @param module_name Name of the module, e.g. `"predictionsTaxonomy"`.
  #'   Filters `config` to only rows with this `module`.
  #' @param plot_type A character string identifying which plot's config to
  #'   return, e.g. `"summary"`, `"treemap"`, `"heatmap"`. Filters `config`
  #'   to only rows with this `plot_type`.
  #' @return A named list of parsed values. Names come from the `var` column.
  #' @examples
  #' # cfg_summary <- get_plot_config("predictionsTaxonomy", "summary")
  #' @export
  get_plot_config <- function(module_name, plot_type) {
    config <- load_data("config_plots")
    
    # Filter rows for this module and plot
    rows <- config[config$module == module_name &
                     config$plot_type == plot_type, , drop = FALSE]
    if (nrow(rows) == 0) return(list())
    
    # Coerce a single value string to its R type
    coerce_value <- function(x) {
      # Empty / missing -> NULL
      if (is.na(x) || !nzchar(x)) return(NULL)
      
      # Vector (semicolon-delimited)
      if (grepl(";", x, fixed = TRUE)) {
        parts <- strsplit(x, ";", fixed = TRUE)[[1]]
        nums <- suppressWarnings(as.numeric(parts))
        if (!any(is.na(nums))) return(nums)
        return(parts)
      }
      
      # Logical
      if (x %in% c("TRUE", "FALSE")) return(as.logical(x))
      
      # Numeric (single)
      num <- suppressWarnings(as.numeric(x))
      if (!is.na(num)) return(num)
      
      # Fall through: keep as character (e.g. column names, hovertemplates)
      x
    }
    
    out <- lapply(rows$value, coerce_value)
    names(out) <- rows$var
    out
  }
# Functions for the History Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 5 June 2026

#' Render a Progress Bar for a History Table
#'
#' Creates the HTML used to show the progress of one job in the History table.
#' The color of the bar reflects the job status.
#'
#' @param percent Numeric. The percent of the job that has been completed.
#' @param status Character. The current job status.
#'
#' @return A character string containing HTML for a progress bar.
render_history_progress_bar <- function(percent, status) {
  pct <- if (is.na(percent)) 0 else max(0, min(100, round(percent)))
  
  if (status == "completed") pct <- 100
  if (status == "submitted") pct <- 0
  
  fill <- dplyr::recode(
    status,
    submitted   = "#adb5bd",
    running     = "#0d6efd",
    completed   = "#198754",
    error       = "#dc3545",
    cancelled   = "#adb5bd",
    interrupted = "#adb5bd",
    .default    = "#adb5bd"
  )
  
  sprintf(
    paste0(
      "<div style='background:#e9ecef;border-radius:6px;height:16px;",
      "width:120px;position:relative;overflow:hidden'>",
      "<div style='background:%s;width:%d%%;height:100%%'></div>",
      "<span style='position:absolute;left:0;top:0;width:100%%;",
      "text-align:center;font-size:11px;line-height:16px;color:#212529'>%d%%</span>",
      "</div>"
    ),
    fill, pct, pct
  )
}

#' Read Job Records for the History Table
#'
#' Reads the status records used to build the History table. By default, only
#' jobs belonging to the current user are included. An administrator can
#' instead request records for all users.
#'
#' @param session The Shiny session object. Defaults to the current reactive
#'   domain.
#' @param show_all_users Logical. Whether to include jobs from all users.
#'   Default is \code{FALSE}.
#' @param jobs_dir Character. Directory containing saved jobs.
#'   Default is \code{"jobs"}.
#'
#' @return A data frame containing one row for each job.
read_history_records <- function(
    session = shiny::getDefaultReactiveDomain(),
    show_all_users = FALSE,
    jobs_dir = "jobs",
    cache = NULL
) {
  if (show_all_users) {
    user_dirs <- list.dirs(
      jobs_dir,
      recursive = FALSE,
      full.names = TRUE
    )
  } else {
    user_dirs <- file.path(
      jobs_dir,
      session$userData$user_id()
    )
  }
  
  report_job_status(user_dirs, cache = cache)
}

#' Build the History Table
#'
#' Formats job records for display in the History tab. This adds readable
#' labels, progress bars, cancel buttons, and links to completed jobs.
#'
#' @param records A data frame containing job status records.
#' @param session The Shiny session object.
#' @param show_all_users Logical. Whether the table is being shown to an
#'   administrator. Default is \code{FALSE}.
#'
#' @return A data frame formatted for display in the History table.
build_history_table <- function(records,
                                session = shiny::getDefaultReactiveDomain(),
                                show_all_users = FALSE) {
  ns <- session$ns
  
  cols <- c(
    "Select", "Tool", "Status", "Progress", "Action", "Time",
    "Size (KB)", "User", "Job"
  )
  
  # Return an empty table with the expected columns when no jobs exist.
  if (is.null(records) || nrow(records) == 0) {
    return(
      as.data.frame(
        matrix(
          character(0),
          ncol = length(cols),
          dimnames = list(NULL, cols)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Use readable names for the tools.
  tool_labels <- dplyr::recode(
    records$tab,
    databaseSearch             = "Database search",
    predictionsTaxonomy        = "Taxonomy",
    predictionsMachineLearning = "Machine learning",
    predictionsNetwork         = "Metabolic networks",
    .default                   = "Unknown"
  )
  
  # Use readable names for the job states.
  status_labels <- dplyr::recode(
    records$status,
    submitted   = "Submitted",
    running     = "Running",
    completed   = "Completed",
    error       = "Error",
    cancelled   = "Cancelled",
    interrupted = "Interrupted",
    .default    = "Unknown"
  )
  
  # Show the error message when the user points to a failed job.
  has_error <- !is.na(records$error_message) &
    nzchar(records$error_message)
  
  status_display <- ifelse(
    has_error,
    paste0(
      "<span title='",
      htmltools::htmlEscape(records$error_message, attribute = TRUE),
      "'>",
      status_labels,
      "</span>"
    ),
    status_labels
  )
  
  # Add a progress bar for each job.
  progress_display <- mapply(
    render_history_progress_bar,
    records$percent,
    records$status,
    USE.NAMES = FALSE
  )
  
  # Add a Cancel button for jobs that are still active.
  is_active <- records$status %in% c("submitted", "running")
  cancel_input <- ns("cancel_job")
  
  action_display <- ifelse(
    is_active,
    sprintf(
      paste0(
        "<button class='btn btn-warning btn-sm' ",
        "onclick=\"Shiny.setInputValue('%s', '%s', {priority:'event'})\">",
        "Cancel</button>"
      ),
      cancel_input,
      records$job_id
    ),
    ""
  )
  
  # Build links for completed jobs
  tab_query <- paste0(
    "?tab=", records$tab,
    "&user=", records$user_id,
    "&job=", records$job_id
  )
  
  urls <- paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    if (!is.null(session$clientData$url_port)) {
      paste0(":", session$clientData$url_port)
    },
    session$clientData$url_pathname,
    tab_query
  )
  
  # is_completed <- records$status == "completed"
  
  # job_display <- ifelse(
  #   is_completed,
  #   paste0(
  #     "<a href='",
  #     urls,
  #     "' target='_blank'>",
  #     records$job_id,
  #     "</a>"
  #   ),
  #   records$job_id
  # )
  
  job_display <-
    paste0(
      "<a href='",
      urls,
      "' target='_blank'>",
      records$job_id,
      "</a>"
    )
  
  # Format values for display
  time_display <- format(records$submitted_at, "%Y-%m-%d %H:%M:%S")
  
  size_KB <- ifelse(
    is.na(records$result_size),
    NA_real_,
    round(records$result_size / 1024, 0)
  )
  
  users_display <- vapply(
    records$user_id,
    format_ip_for_display,
    character(1)
  )
  
  # Compile table
  df <- data.frame(
    Select = sprintf(
      "<input type='checkbox' class='row_checkbox' name='row_selected' value='%s'>",
      records$job_id
    ),
    Tool        = tool_labels,
    Status      = status_display,
    Progress    = progress_display,
    Action      = action_display,
    Time        = time_display,
    `Size (KB)` = size_KB,
    User        = users_display,
    Job         = job_display,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Show the full user ID in the administrator view
  if (show_all_users) {
    df$User <- records$user_id
  }
  
  df
}
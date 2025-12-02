# Define Functions for Help Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 13 Nov 2025

#' Open a demo job in a new browser tab
#'
#' This helper constructs a job URL of the form
#' ?tab=<tab>&user=<user>&job=<job> using the current session URL
#' and opens it in a new browser tab.
#'
#' @param session The Shiny session object.
#' @param demo_tab The internal tab/tool ID (e.g., "predictionsTaxonomy").
#' @param demo_user The user folder for the demo job (e.g., "demo_user").
#' @param demo_job The job ID (file name without .rds).
#' @return Invisibly returns the URL that was opened.
#' @export
open_demo_job <- function(session, demo_tab, demo_user = "demo_user", demo_job) {
  cd <- session$clientData
  
  # Build query string (?tab=...&user=...&job=...)
  tab_query <- paste0(
    "?tab=", demo_tab,
    "&user=", demo_user,
    "&job=",  demo_job
  )
  
  # Open URL
  shinyjs::runjs(sprintf("window.open('%s', '_blank');", tab_query))
  
  invisible(url)
}
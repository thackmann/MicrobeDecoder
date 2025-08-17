# Helper functions for API for App
# This script defines helper functions for the API for the app
# Timothy Hackmann
# 13 May 25

#' Handle Callback Posting to a Provided URL
#'
#' This function sends a POST request to a callback URL with the given result,
#' wrapped in a named JSON object. It is useful for asynchronous API workflows.
#' If a callback URL is specified, the function does not return the result directly
#' but instead sends it to the URL and returns a submission status.
#'
#' @param result The object to send (e.g., a list of probabilities).
#' @param callback_url Character. The URL to which the result should be posted.
#' @param label Character. The name to wrap around the result in the JSON body (default: "probabilities").
#'
#' @return A list indicating status and callback URL if posted, or NULL if no callback is used.
#' @export
handle_callback <- function(result, callback_url, label = "probabilities") {
  if (!is.null(callback_url)) {
    tryCatch({
      httr::POST(
        url = callback_url,
        body = jsonlite::toJSON(setNames(list(result), label), auto_unbox = TRUE),
        encode = "json"
      )
    }, error = function(e) {
      message("Callback failed: ", e$message)
    })

    return(list(status = "submitted", callback_url = callback_url))
  }

  return(NULL)  # continue regular return
}

#' Replace NULL with NA in Lists
#'
#' This recursive helper function replaces any `NULL` values in a list with `NA`.
#' Useful for cleaning API input or output data structures where `NULL` is not allowed.
#'
#' @param x A list or value to process.
#'
#' @return The input with all NULL values replaced by NA.
#' @export
replace_null_with_na <- function(x) {
  if (is.list(x)) lapply(x, replace_null_with_na)
  else if (is.null(x)) NA
  else x
}
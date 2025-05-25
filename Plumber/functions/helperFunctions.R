# Helper functions for API for App
# This script defines helper functions for the API for the app
# Timothy Hackmann
# 13 May 25

#' Handle callback posting to a provided URL
#'
#' This function sends a POST request to the callback_url with the given result.
#' If the callback_url is present, it suppresses the direct return of the result
#' and instead returns a status message.
#'
#' @param result The object to send (e.g., probabilities)
#' @param callback_url The callback URL to post the result to
#' @param label A label to wrap the result in (e.g., "probabilities")
#'
#' @return Either a status message (if callback used) or NULL (to continue execution)
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

replace_null_with_na <- function(x) {
  if (is.list(x)) lapply(x, replace_null_with_na)
  else if (is.null(x)) NA
  else x
}

get_gene_functions_from_database <- function(selected_organisms)
{
  database <- load_database()
  gene_functions <- load_gene_functions()
  organism_by_genome <- get_organism_by_genome(database = database)
  gene_functions <- process_database_gene_functions(gene_functions, organism_by_genome, selected_organisms)

  return(gene_functions)
}

# Get choices for metabolites (substrates, products, or unbalanced intermediates)
get_metabolite_choices <- function(selected_reaction)
{
  reference_reactions <- get_reference_reactions_from_database(selected_reaction)
  metabolites <- get_metabolite_names(reference_reactions$equation)

  return(metabolites)
}

get_model_paths_from_database <- function(model_names)
{
  model_paths <- lapply(model_names, function(model_name) {
    model_paths[[model_name]]})

  return(model_paths)
}

get_choices_model_names <- function()
{
  choices <- names(model_paths)

  return(choices)
}
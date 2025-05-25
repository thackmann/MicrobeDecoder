# Endpoint Functions for API for App
# This script uses functions to define the endpoints for the API for the app
# Timothy Hackmann
# 16 May 25

# === Status check ===
  #* Check API status and current server time
  #* @get /status
  check_status <- function() {
    list(status = "OK", time = Sys.time())
  }
  
# === Example requests ===
  #* Show example curl requests
  #* @get /examples
  #* Show example curl requests

  get_examples <- function(req, res) {
    get("plumber_directory", envir = .GlobalEnv)

    fp <- file.path(plumber_directory, "data/examples.md")
    message("Serving example file from: ", fp)
    
    if (!file.exists(fp)) {
      res$status <- 404
      return(list(error = paste("File not found:", fp)))
    }
    
    plumber::include_file(fp, res)
  }
  
  
# === Compute results ====  
  #* Predict microbial traits based on taxonomy
  #* @param query_taxa:object Table of input taxonomy
  #* @param query_string:string An optional query string to filter the database.
  #* @param traits_to_predict:array Names of traits to predict
  #* @param ignore_NA:boolean Whether to ignore NA values
  #* @param simple_names:boolean Whether to use simplified names for taxa
  #* @param ignore_species:boolean Whether to ignore species rank in taxa
  #* @param system_taxonomy:string Taxonomic system to use (e.g., "LPSN")
  #* @post /compute/taxonomy
  compute_taxonomy <- function(req, res) {
    # Process inputs
    input <- jsonlite::fromJSON(req$postBody)
    query_taxa <- tibble::as_tibble(input$query_taxa)
    query_taxa <- process_uploaded_taxonomy(query_taxa)
    
    # Compute result
    result <- compute_taxonomy_predictions(
      data = load_database(),
      query_taxa = query_taxa,
      query_string = input$query_string %||% NULL,
      traits_to_predict = input$traits_to_predict,
      ignore_NA = input$ignore_NA %||% TRUE,
      simple_names = input$simple_names %||% TRUE,
      ignore_species = input$ignore_species %||% TRUE,
      system_taxonomy = input$system_taxonomy %||% "LPSN"
    )
    
    # Print result
    print(head(result))
    
    # Send result to callback url
    callback_result <- handle_callback(result, input$callback_url, "probabilities")

    return(result)
  }
  
  #* Predict traits from metabolic networks
  #* @param selected_organisms:array Selected organism names (used to look up gene functions)
  #* @param gene_functions:object A data frame of gene functions (if organisms not provided)
  #* @param reference_reactions:string The name of the set of reference reactions to use
  #* @param substrates:array Names of substrates
  #* @param products:array Names of products
  #* @param unbalanced_intermediates:array Names of unbalanced intermediates
  #* @param all_subunits:boolean Whether all enzyme subunits are required for activity.
  #* @post /compute/networks
  compute_networks <- function(req, res) {
    # Process inputs
    input <- jsonlite::fromJSON(req$postBody)
    
    has_organisms <- !is.null(input$selected_organisms) && length(input$selected_organisms) > 0
    has_gene_functions <- !is.null(input$gene_functions) && length(input$gene_functions) > 0
    
    if (!has_organisms && !has_gene_functions) {
      res$status <- 400
      return(list(error = "You must specify either 'selected_organisms' or 'gene_functions'."))
    }
    
    if (has_organisms) {
      gene_functions <- get_gene_functions_from_database(input$selected_organisms)
    } else {
      gene_functions <- tibble::as_tibble(input$gene_functions)
      gene_functions <- process_uploaded_gene_functions(gene_functions)
    }
    
    reference_reactions <- get_reference_reactions_from_database(input$reference_reactions)
    
    # Compute result
    result <- compute_network_predictions(
      reference_reactions = reference_reactions,
      gene_functions = gene_functions,
      substrates = input$substrates,
      products = input$products,
      unbalanced_intermediates = input$unbalanced_intermediates,
      all_subunits = input$all_subunits %||% TRUE
    )
    
    # Print result
    print(head(result))
    
    # Send result to callback url
    callback_result <- handle_callback(result, input$callback_url, "probabilities")
    
    return(result)
  }
  
  #* Predict traits using machine learning
  #* @param selected_organisms:array Selected organism names (used to look up gene functions)
  #* @param gene_functions:object A data frame of gene functions (if organisms not provided)
  #* @param model_names:array Names of pre-trained models to use
  #* @post /compute/ml
  compute_ml <- function(req, res) {
    # Process inputs
    input <- jsonlite::fromJSON(req$postBody)
    
    has_organisms <- !is.null(input$selected_organisms) && length(input$selected_organisms) > 0
    has_gene_functions <- !is.null(input$gene_functions) && length(input$gene_functions) > 0
    
    if (!has_organisms && !has_gene_functions) {
      res$status <- 400
      return(list(error = "You must specify either 'selected_organisms' or 'gene_functions'."))
    }
    
    if (has_organisms) {
      gene_functions <- get_gene_functions_from_database(input$selected_organisms)
    } else {
      gene_functions <- tibble::as_tibble(input$gene_functions)
      gene_functions <- process_uploaded_gene_functions(gene_functions)
    }
    
    model_paths <- get_model_paths_from_database(input$model_names)
    
    # Compute result
    result <- compute_ml_predictions(
      df = gene_functions,
      model_names = input$model_names,
      model_paths = model_paths,
      response = NULL,
      predictors = NULL,
      seed = NULL,
      ntree = NULL,
      maxnodes = NULL,
      positive_class_weight = NULL,
      training_split = NULL
    )
    
    # Keep only results to be sent back
    result <- result["probabilities"]
    
    # Send result to callback url
    callback_result <- handle_callback(result, input$callback_url, "probabilities")
    
    # Print result
    print(head(result))
    
    return(result)
  }

# === List parameters ====  
  #* List parameters for predicting traits from taxonomy
  #* @get /parameters/taxonomy
  parameters_taxonomy <- function() {
    # Get object
    parameters <- list(
      query_taxa = list(
        choices = NA,
        default = NA
      ),
      traits_to_predict = list(
        choices = choices_traits_taxonomy,
        default = "Type of metabolism"
      ),
      system_taxonomy = list(
        choices = choices_system_taxonomy,
        default = "LPSN"
      ),
      ignore_NA = list(
        choices = c(TRUE, FALSE),
        default = TRUE
      ),
      simple_names = list(
        choices = c(TRUE, FALSE),
        default = TRUE
      ),
      ignore_species = list(
        choices = c(TRUE, FALSE),
        default = TRUE
      ),
      callback_url = list(
        choices = NA,
        default = NA
      )
    )
    
    # Return parameters
    return(parameters)
  }
  
  #* List parameters for predicting traits with networks
  #* @param selected_reaction:object The selected reference reaction
  #* @get /parameters/networks
  parameters_networks <- function(selected_reaction = NULL) {
    # Get parameters
    if (is.null(selected_reaction)) {
      selected_reaction <- "Fermentation (glucose)"
    }
    
    parameters <- list(
      gene_functions  = list(
        choices = NA,
        default = NA
      ),
      selected_organisms   = list(
        choices = NA,
        default = NA
      ),
      reference_reactions = list(
        choices = get_choices_reference_reactions_in_database(),
        default = head(get_choices_reference_reactions_in_database())
      ),
      substrates = list(
        choices = get_metabolite_choices(selected_reaction),
        default = get_metabolite_selections(selected_reaction, 
                                            metabolite_col = "default_substrates")
      ),
      products = list(
        choices = get_metabolite_choices(selected_reaction),
        default = get_metabolite_selections(selected_reaction, 
                                            metabolite_col = "default_products")
      ),
      unbalanced_intermediates = list(
        choices = get_metabolite_choices(selected_reaction),
        default = get_metabolite_selections(selected_reaction, 
                                            metabolite_col = "default_unbalanced_intermediates")
      ),
      all_subunits = list(
        choices = c(TRUE, FALSE),
        default = TRUE
      ),
      callback_url = list(
        choices = NA,
        default = NA
      )
    )
    
    # Return parameters
    return(parameters)
  }
  
  #* List parameters for predicting traits with machine learning
  #* @get /parameters/ml
  parameters_ml <- function() {
    # Get parameters
    model_choices <- get_choices_model_names()
    default_models <- c(
      "Fermentation (type of metabolism)", 
      "Methanogenesis (type of metabolism)"
    )
    
    parameters <- list(
      gene_functions  = list(
        choices = NA,
        default = NA
      ),
      selected_organisms   = list(
        choices = NA,
        default = NA
      ),
      model_names = list(
        choices = model_choices,
        default = default_models[default_models %in% model_choices]
      ),
      callback_url = list(
        choices = NA,
        default = NA
      )
    )
    
    # Return parameters
    return(parameters)
  }
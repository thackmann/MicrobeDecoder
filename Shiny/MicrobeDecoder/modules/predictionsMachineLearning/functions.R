# Define Functions for Predictions Using Machine Learning Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 9 Mar 2025

#' Round Values to Binary
#'
#' This function rounds numeric values to 0 or 1 based on a threshold of 0.5.
#'
#' @param x A numeric vector of values to be rounded.
#' @return A numeric vector with values rounded to 0 or 1.
#' @export
make_binary <- function(x) {
  ifelse(x >= 0.5, 1, 0)
}

#' Extract Genomes from Dataframe
#'
#' This function extracts unique genomes from a given dataframe, removing NA and "NA" values.
#'
#' @param data A data frame containing genome information.
#' @param genome_column The column containing genome IDs.
#' @return A vector of unique genome IDs.
#' @export
extract_genomes <- function(data, genome_column = "IMG Genome ID max quality") {
  # Select the genome column, filter out NA and "NA", and get unique values
  genomes <- data %>%
    dplyr::select(!!rlang::sym(genome_column)) %>%
    dplyr::filter(!is.na(!!rlang::sym(genome_column)) & !!rlang::sym(genome_column) != "NA") %>%
    dplyr::pull(!!rlang::sym(genome_column)) %>%
    # unique() %>%
    as.character() 
  
  return(genomes)
}

#' Format Response Variable for Random Forest Model
#'
#' This function formats the response variable for a random forest model.  The input
#' is the app's database and a query string.  The query string specifies which organisms
#' in the database are positive for the trait.  The function returns a dataframe with
#' responses (0 = negative for trait, 1 = positive for trait) and genome ID for each organism.
#' By default, organisms with NA for any variables in the query are excluded.
#' It optionally subsamples a proportion of rows to reduce the number of responses.
#'
#' @param data The app's database (a data frame)
#' @param query_string A string representing the query filter (e.g., "`Gram_stain` == \"positive\"").
#' @param ignore_NA Logical. If TRUE, organisms with NA are not counted. Default is TRUE.
#' @return A response data frame with genome IDs and a binary response (1 for positive trait, 0 otherwise).
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang parse_expr sym
format_response <- function(data, query_string, ignore_NA = TRUE) {
  # Get data for organisms with positive traits
  data_positive <- filter_data_by_query(data, query_string)
  
  # Get data for all organisms (excluding those with NA values if specified)
  if(ignore_NA)
  {
    data_all <- filter_data_excluding_na(data, query_string)
  }else
  {
    data_all = data
  }
  
  # Get genomes for organisms
  positive_genomes <- extract_genomes(data_positive)
  all_genomes <- extract_genomes(data_all)
  
  # Ensure both vectors are character
  positive_genomes <- as.character(positive_genomes)
  all_genomes <- as.character(all_genomes)
  
  # Create a response dataframe with binary values: 1 for positive genomes, 0 for others
  response <- data.frame(
    Genome = all_genomes,
    Response = ifelse(all_genomes %in% positive_genomes, 1, 0)
  )
  
  return(response)
}

#' Format Predictor Variables for Random Forest Model
#'
#' This function gets predictors for a random forest model from gene functions 
#' of a given set of genomes. It puts genomes in rows and gene functions in columns, 
#' converting the latter to binary values (0 = absent in genome, 1 = present in genome).  
#' It optionally subsamples a proportion of the columns to reduce the number of predictors
#' and rows to reduce the number of responses.
#'
#' @param gene_functions A data frame containing gene functions with KO IDs and Genome IDs.
#' @param seed An optional seed value for reproducibility. Default is NULL.
#' @param responses_to_keep An optional proportion of rows to keep when subsampling. Must be between 0 and 1. Default is 1
#' @param predictors_to_keep An optional proportion of columns to keep when subsampling. Must be between 0 and 1. Default is 1
#' @param only_keep_genome_ko A logical indicating whether to retain only the Genome column and KO columns. Default is TRUE
#' @return A data frame of formatted predictors.
#' @export
#' @importFrom dplyr select mutate distinct sample_n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym
format_predictors = function(gene_functions, seed = NULL, responses_to_keep = 1, predictors_to_keep = 1, only_keep_genome_ko = TRUE) {
  predictors = gene_functions
  
  ko_column <- detect_pattern_column(data = predictors, pattern = "^K[0-5]{5}$")
  
  predictors <- predictors %>% 
    dplyr::select(Genome, !!rlang::sym(ko_column)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = 1) %>% 
    tidyr::pivot_wider(names_from = !!rlang::sym(ko_column), values_from = value, values_fill = list(value = 0))

  # Ensure Genome is character
  predictors$Genome <- as.character(predictors$Genome) 
  predictors <- predictors %>%
    dplyr::select(Genome, everything())
  
  # Remove all non-genome and non-KO columns if the option is enabled
  if (only_keep_genome_ko) {
    ko_columns <- grep("^K[0-9]{5}$", colnames(predictors), value = TRUE)
    predictors <- predictors %>% dplyr::select(Genome, all_of(ko_columns))
  }
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Subsample rows
  if (!is.null(responses_to_keep)) {
    if (responses_to_keep > 1 || responses_to_keep < 0) {
      stop("Row proportion must be between 0 and 1.")
    }
    total_rows <- nrow(predictors)
    n_rows <- round(total_rows * responses_to_keep)
    predictors <- predictors %>% dplyr::sample_n(n_rows)
  }
  
  # Subsample columns (excluding Genome column)
  if (!is.null(predictors_to_keep)) {
    if (predictors_to_keep > 1 || predictors_to_keep < 0) {
      stop("Column proportion must be between 0 and 1.")
    }
    
    non_genome_columns <- setdiff(colnames(predictors), "Genome")
    total_cols <- length(non_genome_columns)
    n_cols <- round(total_cols * predictors_to_keep)
    selected_cols <- sample(non_genome_columns, n_cols)
    
    predictors <- predictors %>%
      dplyr::select(Genome, all_of(selected_cols))
  }
  
  return(predictors)
}

#' Format Data for Random Forest Model
#'
#' This function formats the data by joining predictors and response if the "Genome" column is present,
#' or combines them using cbind if "Genome" is not present. It ensures that the response column is named "Response".
#'
#' @param predictors A data frame of formatted predictors.
#' @param response A data frame of formatted response variables.
#' @return A data frame where the first column is the response and the remaining columns are predictors.
#' @export
#' @importFrom dplyr inner_join select rename
format_rf_data <- function(predictors, response) {
  # Check for "Genome" column in both predictors and response
  if ("Genome" %in% colnames(predictors) && "Genome" %in% colnames(response)) {
    # Join predictors and response by Genome
    data <- predictors %>%
      dplyr::inner_join(response, by = "Genome") %>%
      dplyr::select(-Genome)
    
    # Ensure the response column is named "Response"
    data <- data %>% dplyr::rename(Response = last_col())
    
  } else {
    # Check if number of rows in predictors and response match
    if (nrow(predictors) != nrow(response)) {
      stop("The number of rows in 'predictors' and 'response' must be the same.")
    }
    
    # Combine predictors and response using cbind
    data <- cbind(predictors, Response = response)
  }
  
  # Do further formatting
  data$Response <- as.factor(data$Response)
  data <- data %>% dplyr::select(Response, everything())
  
  return(data)
}

#' Split Train and Test Data
#'
#' This function splits the dataset into training and test sets.
#' The response variable should be the first column.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param training_split The proportion of data to use for training. Default is 0.7.
#' @return A list containing training and test datasets.
#' @export
split_data <- function(data, seed = 123, training_split = 0.7) {
  set.seed(seed)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(training_split, 1-training_split))
  train <- data[ind == 1, ]
  test <- data[ind == 2, ]
  
  split = list(train = train, test = test)
  
  return(split)
}

#' Train Random Forest Model
#'
#' This function trains a random forest model using the formatted data.
#' The model assumes that the first column is the response variable and the remaining columns are predictors.
#' The model can be configured with a specified number of trees and maximum nodes.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param ntree The number of trees to grow in the random forest. Default is 500.
#' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
#' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
#' @return A random forest model object.
#' @export
#' @importFrom randomForest randomForest
train_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight=0.5) {
  # Get response column
  response_column <- data[[1]]
  if (!is.factor(response_column)) {
    response_column <- as.factor(response_column)
  }
  
  # The remaining columns are predictors
  predictors <- data[, -1]
  
  # Combine response and predictors into a new data frame for modeling
  modeling_data <- data.frame(Response = response_column, predictors)
  
  # Fit the random forest model
  rf <- randomForest::randomForest(Response ~ ., data = data, ntree = ntree, maxnodes = maxnodes, classwt = c("0" = 1-positive_class_weight, "1" = positive_class_weight)) 
  
  return(rf)
}

#' Evaluate Random Forest Model
#'
#' This function evaluates the random forest model using the test data.
#' It returns a confusion matrix
#'
#' @param rf A random forest model object.
#' @param data A data frame containing the test data. The first column must be the response.
#' @return A confusion matrix 
#' @export
#' @importFrom caret confusionMatrix
evaluate_rf <- function(rf, data) {
  # Get model
  model = rf
  
  # Get response column
  response_column <- data[[1]]
  if (!is.factor(response_column)) {
    response_column <- as.factor(response_column)
  }
  
  # The remaining columns are predictors
  predictors <- data[, -1]
  
  # Get predictions
  predictions = randomForest:::predict.randomForest(object = model, newdata = predictors, type = "prob")[, 2] # Assuming the second column is the probability of the positive class
  
  # Convert predicted probabilities to classes based on a threshold of 0.5
  predicted_classes <- as.factor(ifelse(predictions >= 0.5, levels(response_column)[2], levels(response_column)[1]))
  
  # Create a confusion matrix
  confusion_matrix <- caret::confusionMatrix(predicted_classes, response_column, positive = "1")
  
  return(confusion_matrix)
}

#' Build Random Forest Model
#'
#' This function trains and evaluates a random forest model.
#'
#' @param data A data frame where the first column is the response and the remaining columns are predictors.
#' @param seed An optional seed value for reproducibility. Default is 123.
#' @param training_split The proportion of data to use for training. Default is 0.7.
#' @param ntree The number of trees to grow in the random forest. Default is 500.
#' @param maxnodes The maximum number of terminal nodes trees in the forest can have. Default is NULL.
#' @param positive_class_weight The weight given to the positive class of responses. Default is 0.5 (equal weight for negative and positive classes).
#' @return A random forest model object with evaluation results added.
#' @export
#' @importFrom randomForest randomForest
#' @importFrom caret confusionMatrix
build_rf <- function(data, seed = 123, training_split = 0.7, ntree = 500, maxnodes = NULL, positive_class_weight = 0.5) {
  # Split data into training and test sets
  data_split <- split_data(data = data, seed = seed, training_split = training_split)
  train <- data_split$train
  test <- data_split$test
  
  # Check if training or test data is empty
  if (nrow(train) == 0|nrow(test) == 0) {
    return(NULL)
  }
  
  # Train the model
  rf <- train_rf(data = train, seed = seed, ntree = ntree, maxnodes = maxnodes, positive_class_weight = positive_class_weight)
  
  # Evaluate the model
  confusion_matrix <- evaluate_rf(rf = rf, data = test)
  
  # Add evaluation results to the random forest model
  rf$evaluation_results <- confusion_matrix
  
  return(rf)
}

#' Save Random Forest Model
#'
#' This function saves a random forest model to an RDS file with optional compression and environment cleaning to reduce file size.
#'
#' @param rf A random forest model object to save.
#' @param data_fp The file path where the model should be saved.
#' @param remove_proximity Logical. If TRUE, removes proximity data from the model to reduce file size. Default is TRUE.
#' @param clean_environment Logical. If TRUE, removes non-essential objects from the environment to reduce file size. Default is TRUE.
#' @param compress The compression method to use when saving the RDS file. Default is "xz".
#' @return Saves the random forest model to the specified file path.
#' @export
save_rf = function(rf, data_fp, remove_proximity=TRUE, clean_environment = TRUE, compress="xz")
{
  #Remove data for proximity (reduces file size)
  if(remove_proximity==TRUE)
  {
    rf$proximity <- NULL 
  }
  
  # Remove non-essential objects from the environment (reduces file size)
  if(clean_environment==TRUE)
  {
    rm(list = setdiff(ls(envir = attr(rf$terms, ".Environment")), c("train", "test", "rf", "var")), envir = attr(rf$terms, ".Environment"))
  }
  
  saveRDS(object=rf, file = data_fp, compress = compress)
}

#' Run Random Forest Model Predictions
#'
#' This function makes predictions using a list of pre-trained random forest models. 
#' It ensures that the necessary predictors are present in the dataframe and in the correct order.
#'
#' @param df A dataframe containing the predictor variables.
#' @param models A named list of random forest models to use for predictions. Non-lists will be wrapped into lists.
#' @return A dataframe in long format with columns: "Organism number", "Organism name", "Model", and "Probability".
#' @export
#' @importFrom base lapply setdiff
run_random_forest <- function(df, model, model_name) {
  if (is.null(model$importance)) {
    stop("Model importance is NULL. Check model structure.")
  }
  
  predictors <- rownames(model$importance)
  
  missing_predictors <- setdiff(predictors, colnames(df))
  if (length(missing_predictors) > 0) {
    df[missing_predictors] <- 0
  }
  
  df_ordered <- df[, predictors, drop = FALSE]
  
  predictions <- randomForest:::predict.randomForest(object = model, newdata = df_ordered, type = "prob")[, 2]
  
  prediction_df <- data.frame(
    "Organism number" = seq_len(nrow(df)),
    "Organism name" = rownames(df),
    "Model" = model_name,
    "Probability" = predictions,
    check.names = FALSE
  )
  
  colnames(prediction_df) <- c("Organism number", "Organism name", "Model", "Probability")
  
  return(prediction_df)
}

# Load Selected Models
#'
#' This function loads a list of models based on the selected model names and paths provided.
#' The function checks and loads each model file from the specified paths.
#'
#' @param session The Shiny session object.
#' @param model_names A character vector of selected model names to be loaded.
#' @param model_paths A vector of file paths corresponding to the models.
#' @param file_upload A Boolean describing if files are uploaded. 
#' @return A named list of loaded model objects.
#' @export
#' @importFrom base lapply
load_models <- function(session = getDefaultReactiveDomain(), 
                        model_names, model_paths, file_upload = FALSE) {
  if(!file_upload) {
    model_list <- lapply(seq_along(model_names), function(i) {
      obj <- check_and_load(file_path = model_paths[[i]])
      return(obj)
    })
  } else if(file_upload) {
    model_list <- lapply(seq_along(model_names), function(i) {
      obj <- validate_and_read_file(session = session, file_path = model_paths[[i]])
      return(obj)
    })
  }
  
  names(model_list) <- model_names
  return(model_list)
}

#' Load or train machine learning models
#'
#' This function either loads pre-trained models from paths or trains new models
#' from the provided data and parameters.
#'
#' @param model_names Character vector of model names.
#' @param model_paths Character vector of file paths to saved models (can be NULL).
#' @param predictors Character vector of predictor column names.
#' @param response Name of the response column.
#' @param seed Random seed for reproducibility.
#' @param ntree Number of trees for random forest.
#' @param maxnodes Max terminal nodes for random forest.
#' @param positive_class_weight Numeric weight for positive class.
#' @param training_split Proportion of data to use for training.
#'
#' @return A named list of trained or loaded models.
#' @export
get_models <- function(model_names, 
                       model_paths = NULL, 
                       predictors = NULL, 
                       response = NULL, 
                       seed = NULL, 
                       ntree = NULL, 
                       maxnodes = NULL, 
                       positive_class_weight = NULL, 
                       training_split = NULL) {
  # Initialize variables
  models <- NULL
  
  # Load or train models
  if (!is.null(model_paths)) {
    if (!is.function(load_models)) stop("load_models is not a function")
    models <- load_models(model_names = model_names, model_paths = model_paths, file_upload = FALSE)
  } else {
    rf_data <- format_rf_data(predictors = predictors, response = response)
    model <- build_rf(
      data = rf_data,
      seed = seed,
      training_split = training_split,
      ntree = ntree,
      maxnodes = maxnodes,
      positive_class_weight = positive_class_weight
    )
    
    models <- list(model)
    names(models) <- model_names
  }
  
  return(models)
}

#' Predict traits using a trained machine learning model
#'
#' This function reshapes the input data and generates trait predictions using
#' a single model.
#'
#' @param df A data frame of gene functions with organisms as columns.
#' @param model A trained machine learning model.
#' @param model_name A character string naming the model.
#' @param ns The Shiny namespace function.
#'
#' @return A data frame of predicted probabilities.
#' @export
predict_traits_ml <- function(df, model, model_name, ns) {
  # Format data
  df <- df %>% tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "Organism", values_to = "Database_ID") %>%
    dplyr::mutate(value = 1)
  df <- df %>% dplyr::select(Organism, Database_ID) %>% dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>% tidyr::pivot_wider(names_from = Database_ID, values_from = value, values_fill = list(value = 0))
  df <- as.data.frame(df)
  row.names(df) <- df$Organism
  
  # Perform main logic
  probabilities <- run_random_forest(df = df, model = model, model_name = model_name)
  colnames(probabilities) <- gsub("\\.rds", "", colnames(probabilities))
  
  return(probabilities)
}

#' Get metadata from random forest models
#'
#' Given a named list of \code{randomForest} classification models, return per-model
#' metadata needed by the UI (number of predictors, number of responses used to
#' train, and any attached evaluation results).
#'
#' @param models A \strong{named} list of fitted \code{randomForest} models.
#'   Each element should have the usual slots (e.g., \code{$forest}, \code{$y}).
#' @return A named list. For each model name, a list with:
#'   \itemize{
#'     \item \code{n_predictors}: integer, number of predictors the model expects.
#'     \item \code{n_responses}:  integer, number of training responses used.
#'     \item \code{evaluation}:   the object stored at \code{$evaluation_results} if present; otherwise \code{NULL}.
#'   }
#' @examples
#' \dontrun{
#' md <- get_metadata_from_models(models)
#' str(md[["Fermentation (type of metabolism)"]])
#' }
#' @export
get_metadata_from_models <- function(models) {
  if (is.null(models) || !length(models)) return(list())
  
  stopifnot(!is.null(names(models)), all(nzchar(names(models))))
  
  meta <- lapply(models, function(m) {
    # Get number of predictors
    n_pred <- tryCatch({
      if (!is.null(m$forest) && !is.null(m$forest$ncat)) {
        length(m$forest$ncat)
      } else if (!is.null(m$importance)) {
        nrow(m$importance)
      } else {
        NA_integer_
      }
    }, error = function(...) NA_integer_)
    
    # Get number of responses
    n_resp <- tryCatch({
      if (!is.null(m$y)) length(m$y) else NA_integer_
    }, error = function(...) NA_integer_)
    
    # Get evaluation results
    eval_obj <- if (!is.null(m$evaluation_results)) m$evaluation_results else NULL
    
    list(
      n_predictors = n_pred,
      n_responses  = n_resp,
      evaluation   = eval_obj
    )
  })
  
  names(meta) <- names(models)
  
  return(meta)
}

#' Compute predictions using machine learning models
#'
#' Loads or trains models, predicts trait probabilities, and gets metadata for 
#' models.
#'
#' @param df A dataframe of gene functions
#' @param model_names A character vector of names of pre-trained models
#' @param model_paths A character vector of file paths to pre-trained models
#' @param response The response variable name
#' @param predictors A character vector of predictor variable names
#' @param seed Random seed used for reproducibility
#' @param ntree Number of trees to grow
#' @param maxnodes Maximum number of terminal nodes in a tree
#' @param positive_class_weight Numeric weight for positive class
#' @param training_split Proportion of data to use for training
#' @param keep_models Logical.  If TRUE, return models with results.  If FALSE, models are not returned and instead removed from memory.
#' @param ns The namespace function for the Shiny module
#'
#' @return A named list with trained models and predicted probabilities.
#' @export
compute_ml_predictions <- function(df, model_names, model_paths, response, predictors,
                                   seed, ntree, maxnodes, positive_class_weight,
                                   training_split, keep_models = FALSE, ns = NULL) {
  all_probabilities <- list()
  all_metadata      <- list()
  all_models        <- list()
  
  for (i in seq_along(model_names)) {
    name  <- model_names[[i]]
    path  <- if (!is.null(model_paths)) model_paths[i] else NULL
    
    # Update progress
    if (!is.null(ns)) display_modal(ns = ns, message = paste0("Prediction in progress"), value = round(100 * (i - 1) / length(model_names)))
    cat(file = stderr(), paste0("Started getting model ", i, " of ", length(model_names), " at ", Sys.time(), "\n"))
    
    # Load or train model
    env_before     <- ls(envir = .GlobalEnv)
    model          <- get_models(name, path, predictors, response, seed, ntree, maxnodes, positive_class_weight, training_split)[[1]]
    env_after      <- ls(envir = .GlobalEnv)
    loaded_objects <- setdiff(env_after, env_before)
    
    # Predict
    cat(file = stderr(), paste0("Started prediction for model ", i, " of ", length(model_names), " at ", Sys.time(), "\n"))
    probs <- predict_traits_ml(df, model = model, model_name = name, ns = ns)
    all_probabilities[[i]] <- probs
    
    # Collect metadata
    all_metadata[[name]] <- get_metadata_from_models(stats::setNames(list(model), name))[[1]]
    
    # Optionally retain model
    if (keep_models) {
      all_models[[name]] <- model
    }
    
    # Unload model
    rm(model)
    if (length(loaded_objects)) {
      rm(list = loaded_objects, envir = .GlobalEnv)
    }
    gc()
    
    cat(file = stderr(), paste0("Ended prediction for model ", i, " of ", length(model_names), " at ", Sys.time(), "\n"))
  }
  
  # Combine results
  probabilities <- do.call(rbind, all_probabilities)
  
  if (keep_models) {
    results <- list(models = all_models, probabilities = probabilities, metadata = all_metadata)
  } else {
    results <- list(probabilities = probabilities, metadata = all_metadata)
  }
  
  return(results)
}

# === Get inputs ===
  #' Get Model Names
  #'
  #' Extracts model names based on the selected tab.
  #'
  #' @param models_from_standard Logical. Using standard model list?
  #' @param models_from_other Logical. Defining custom model name?
  #' @param models_from_upload Logical. Uploading a model file?
  #' @param model_names Vector of selected model names.
  #' @param trait_name Custom trait name (if applicable).
  #' @param model_upload Upload input object containing name field.
  #'
  #' @return Character vector of model names.
  get_model_names <- function(models_from_standard,
                              models_from_other,
                              models_from_upload,
                              model_names = NULL,
                              trait_name = NULL,
                              model_upload = NULL) {
    if (models_from_standard) {
      return(model_names)
    } else if (models_from_other) {
      runValidationModal(need(grepl("^[a-zA-Z0-9_ ]*$", trait_name), "Please enter a valid trait name and try again."))
      runValidationModal(need(trait_name != "", "Please enter a valid trait name and try again."))
      return(trait_name)
    } else if (models_from_upload) {
      return(model_upload$name)
    } else {
      return(NULL)
    }
  }
  
  #' Get Model Paths from Database
  #'
  #' This helper function retrieves file paths for a list of model names from a configuration object.
  #'
  #' @param model_names Character vector. A list of model names to retrieve paths for.
  #' @param model_path_config Named list. A mapping of model names to their file paths.
  #'
  #' @return A list of file paths corresponding to the given model names.
  #' @export
  get_model_paths_from_database <- function(model_names, model_path_config) {
    model_paths <- lapply(model_names, function(model_name) {
      model_path_config[[model_name]]
    })
    
    return(model_paths)
  }
  
  #' Get Model Paths
  #'
  #' Determines file paths for models based on source.
  #'
  #' @param models_from_standard Logical. Use preloaded models?
  #' @param models_from_upload Logical. Use uploaded model file?
  #' @param model_names Character vector of model names.
  #' @param model_path_config Named list mapping model names to file paths.
  #' @param model_upload_path File path of uploaded model file.
  #'
  #' @return A list or character vector of model paths.
  #' @export
  get_model_paths <- function(models_from_standard,
                              models_from_upload,
                              model_names,
                              model_path_config,
                              model_upload_path) {
    if (models_from_standard) {
      paths <- get_model_paths_from_database(model_names, model_path_config)
      runValidationModal(need(paths != "", "Please choose at least one trait or model"))
      return(paths)
    } else if (models_from_upload) {
      runValidationModal(need(model_upload_path != "", "Please choose at least one trait or model"))
      return(model_upload_path)
    } else {
      return(NULL)
    }
  }
  
  #' Get Response Variable
  #'
  #' Formats response variable based on query string for custom traits.
  #'
  #' @param models_from_other Logical. Is this for a custom trait?
  #' @param query_string Raw query string from query builder.
  #' @param ignore_NA Logical. Whether to ignore NA responses.
  #'
  #' @return A dataframe containing the formatted response variable.
  get_response <- function(models_from_other,
                           query_string,
                           ignore_NA) {
    if (!models_from_other) return(NULL)
    
    query_string <- get_query_string(query_string)
    
    data <- load_database()
    response <- format_response(data = data, query_string = query_string, ignore_NA = ignore_NA)
    
    runValidationModal(need(nrow(response) > 0, "Please ensure the dataset has at least one response."))
    runValidationModal(need(length(unique(response$Response)) == 2, "Please ensure that the response variable has exactly two classes."))
    
    return(response)
  }
  
  #' Get Predictors
  #'
  #' Generates predictors from gene functions for custom traits.
  #'
  #' @param models_from_other Logical. Is this for a custom trait?
  #' @param responses_to_keep Character vector of response levels to include.
  #' @param predictors_to_keep Character vector of predictor names to include.
  #' @param seed Random seed for reproducibility.
  #'
  #' @return A dataframe of predictor variables.
  get_predictors <- function(models_from_other,
                             responses_to_keep,
                             predictors_to_keep,
                             seed) {
    if (!models_from_other) return(NULL)
    
    gene_functions <- load_gene_functions()
    predictors <- format_predictors(
      gene_functions = gene_functions,
      responses_to_keep = responses_to_keep,
      predictors_to_keep = predictors_to_keep,
      seed = seed
    )
    
    runValidationModal(need(ncol(predictors) > 1, "Please ensure the dataset has at least one predictor"))
    return(predictors)
  }
  
  #' Get Inputs for Machine Learning Module
  #'
  #' This is the main function for getting all inputs for the module
  #'
  #' @param functions_from_database Logical. Use gene functions from database?
  #' @param functions_from_upload Logical. Use gene functions from uploaded file?
  #' @param models_from_standard Logical. Use standard trait models?
  #' @param models_from_other Logical. Use custom trait definition?
  #' @param models_from_upload Logical. Use uploaded model file?
  #' @param selected_organisms Character vector of selected organisms.
  #' @param gene_functions_upload_path File path to uploaded gene functions.
  #' @param model_names_input Character vector of model names (for standard models).
  #' @param trait_name Character name of custom trait.
  #' @param model_upload Upload object with `name` field.
  #' @param model_upload_path Path to uploaded model file.
  #' @param model_path_config Named list of available model paths.
  #' @param query_string Character query string for response formatting.
  #' @param ignore_NA Logical. Ignore NA values in response variable?
  #' @param responses_to_keep Character vector of response levels to include.
  #' @param predictors_to_keep Character vector of predictor names to include.
  #' @param seed Numeric random seed.
  #' @param ntree Number of trees in the random forest (if applicable).
  #' @param maxnodes Maximum number of terminal nodes (if applicable).
  #' @param positive_class_weight Numeric weight for positive class (if applicable).
  #' @param training_split Training data proportion (if applicable).
  #' @param keep_models Logical.  If TRUE, return models with results.  If FALSE, models are not returned and instead removed from memory.
  #'
  #' @return A named list of all ML input components.
  get_ml_inputs <- function(
      functions_from_database,
      functions_from_upload,
      models_from_standard,
      models_from_other,
      models_from_upload,
      selected_organisms = NULL,
      gene_functions_upload_path = NULL,
      model_names_input = NULL,
      trait_name = NULL,
      model_upload = NULL,
      model_upload_path = NULL,
      model_path_config = NULL,
      query_string = NULL,
      ignore_NA = NULL,
      responses_to_keep = NULL,
      predictors_to_keep = NULL,
      seed,
      ntree = NULL,
      maxnodes = NULL,
      positive_class_weight = NULL,
      training_split = NULL,
      keep_models = FALSE
  ) {
    gene_functions <- get_gene_functions(
      functions_from_database = functions_from_database,
      functions_from_upload = functions_from_upload,
      selected_organisms = selected_organisms,
      upload_path = gene_functions_upload_path
    )
    
    model_names <- get_model_names(
      models_from_standard = models_from_standard,
      models_from_other = models_from_other,
      models_from_upload = models_from_upload,
      model_names = model_names_input,
      trait_name = trait_name,
      model_upload = model_upload
    )
    
    model_paths <- get_model_paths(
      models_from_standard = models_from_standard,
      models_from_upload = models_from_upload,
      model_names = model_names,
      model_path_config = model_path_config,
      model_upload_path = model_upload_path
    )
    
    response <- get_response(
      models_from_other = models_from_other,
      query_string = query_string,
      ignore_NA = ignore_NA
    )
    
    predictors <- get_predictors(
      models_from_other = models_from_other,
      responses_to_keep = responses_to_keep,
      predictors_to_keep = predictors_to_keep,
      seed = seed
    )
    
    list(
      gene_functions = gene_functions,
      response = response,
      predictors = predictors,
      model_names = model_names,
      model_paths = model_paths,
      seed = seed,
      ntree = if (models_from_other) ntree else NULL,
      maxnodes = if (models_from_other) maxnodes else NULL,
      positive_class_weight = if (models_from_other) positive_class_weight else NULL,
      training_split = if (models_from_other) training_split else NULL,
      keep_models = keep_models
    )
  }
  
# === Other ===
  #' Get Choices for Model Names
  #'
  #' This function returns the names of all available models from a provided `model_path_config` object.
  #' It is used to populate choices in model selection UIs or APIs.
  #'
  #' @param model_path_config Named list. A mapping of model names to file paths.
  #'
  #' @return A character vector of model names.
  #' @export
  get_choices_model_names <- function(model_path_config) {
    choices <- names(model_path_config)
    return(choices)
  }
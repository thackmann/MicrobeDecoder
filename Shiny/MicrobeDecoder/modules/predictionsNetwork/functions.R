# Define Functions for Predictions with Metabolic Networks Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 22 May 2026

# === Getting inputs ===
  #' Validate Reference Network Data
  #'
  #' This function checks whether the uploaded reference network data contains all required columns.
  #' If any required columns are missing, it returns an empty string.
  #'
  #' @param reference_network A data frame. The uploaded reference network data.
  #' @return The validated data frame or an empty string if required columns are missing.
  #' @export
  #' @importFrom dplyr select
  validate_reference_network <- function(reference_network) {
    required_columns <- c("name", "eq", "way", "ec", "ko", "md")
    
    if (!all(required_columns %in% colnames(reference_network))) {
      return("")
    }
    
    return(reference_network)
  }

  #' Get Reference Network from the Database
  #'
  #' This function retrieves a set of reference networks corresponding to a selected 
  #' reference reaction name. It uses a configuration file to identify the appropriate 
  #' file and then loads its contents.
  #'
  #' @param selected_network A character string indicating the selected reference network.
  #' @param all_option A character string representing the "select all" value. Default is "All reactions".
  #'
  #' @return A data frame containing the filtered or complete reference network.
  #' @examples
  #' ref_data <- get_reference_network_from_database("Fermentation (glucose)")
  #' ref_all <- get_reference_network_from_database("All")
  #'
  #' @export
  get_reference_network_from_database <- function(selected_network,
                                                  all_option = "All reactions") {
    main_network <- load_data("main_reference_network")
    
    if (selected_network != all_option) {
      configured_network <- dplyr::filter(main_network, nt == selected_network)
    } else {
      configured_network <- main_network
    }
    
    return(configured_network)
  }
  
  #' Get Reference Network
  #'
  #' This function returns the reference network from a database, built-in session data, or uploaded file.
  #'
  #' @param network_from_database Logical. Use database source?
  #' @param network_from_builder Logical. Use in-session network builder?
  #' @param network_from_upload Logical. Use uploaded file?
  #' @param selected_network A character string indicating the selected reference network.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param upload_path File path to the uploaded network file (if using upload source).
  #' @return A validated reference network data frame.
  get_reference_network <- function(network_from_database,
                                    network_from_builder,
                                    network_from_upload,
                                    selected_network = NULL,
                                    session = shiny::getDefaultReactiveDomain(),
                                    upload_path = NULL) {
    if (network_from_database) {
      reference_network <- get_reference_network_from_database(selected_network)
    } else if (network_from_builder) {
      reference_network <- session$userData$network_data()
    } else if (network_from_upload) {
      reference_network <- validate_and_read_file(file_path = upload_path)
      reference_network <- validate_reference_network(reference_network)
    } else {
      stop("No valid source specified for reference network.")
    }
    
    run_validation_modal(shiny::need(nrow(reference_network) > 0,
                                   "Please check the format of the reference network file and try again."))
    
    return(reference_network)
  }

  #' Get Inputs for Metabolic Networks Module
  #'
  #' This is the main function for getting all inputs for the module
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param functions_from_database Logical. Whether gene functions come from the database.
  #' @param functions_from_upload Logical. Whether gene functions are uploaded by user.
  #' @param network_from_database Logical. Whether reference network comes from the database.
  #' @param network_from_builder Logical. Whether reference network is built in-session.
  #' @param network_from_upload Logical. Whether reference network is uploaded by user.
  #' @param selected_organisms A character vector of selected organism IDs.
  #' @param gene_functions_upload_path File path to uploaded gene functions (if any).
  #' @param reference_network_upload_path File path to uploaded network (if any).
  #' @param selected_reference_network Character or list of reference network IDs from database.
  #' @param substrates Character vector of selected substrates.
  #' @param products Character vector of selected products.
  #' @param unbalanced_intermediates Character vector of metabolites to exclude.
  #' @param all_subunits Logical. Whether to consider all subunits for module completion.
  #' @param tree_upload A Shiny file upload object holding a phylogenetic tree, or NULL.
  #' @param metadata_upload A Shiny file upload object holding organism metadata, or NULL.
  #'
  #' @return A list of processed inputs.
  get_network_inputs <- function(
      session = shiny::getDefaultReactiveDomain(),
      functions_from_database,
      functions_from_upload,
      network_from_database,
      network_from_builder,
      network_from_upload,
      selected_organisms,
      gene_functions_upload_path,
      reference_network_upload_path,
      selected_reference_network,
      substrates,
      products,
      unbalanced_intermediates,
      all_subunits,
      tree_upload = NULL,
      metadata_upload = NULL
  ) {
    # Load gene functions
    gene_functions <- get_gene_functions(
      functions_from_database = functions_from_database,
      functions_from_upload = functions_from_upload,
      selected_organisms = selected_organisms,
      upload_path = gene_functions_upload_path
    )
    
    # Get organism names
    organism_names <- colnames(gene_functions)
    
    # Get reference network
    reference_network <- get_reference_network(
      network_from_database = network_from_database,
      network_from_builder = network_from_builder,
      network_from_upload = network_from_upload,
      selected_network = selected_reference_network,
      session = session,
      upload_path = reference_network_upload_path
    )
    
    # Get tree
    tree <- get_tree_from_upload(
      tree_upload = tree_upload
    )
    
    # Get metadata
    metadata <- get_metadata_from_upload(
      metadata_upload = metadata_upload
    )
    
    # Validate substrates and products
    run_validation_modal(shiny::need(substrates != "", "Please choose at least one substrate"))
    run_validation_modal(shiny::need(products != "", "Please choose at least one product"))
    
    # Compile and return input list
    list(
      gene_functions = gene_functions,
      organism_names = organism_names,
      reference_network = reference_network,
      substrates = substrates,
      products = products,
      unbalanced_intermediates = unbalanced_intermediates,
      all_subunits = all_subunits,
      tree = tree,
      metadata = metadata
    )
  }

# === Computing results ===
  #' Splitting reaction equation into substrate and product
  #'
  #' This function is based on \code{fbar::split_on_arrow}.
  #'
  #' @param equations Character vector of reaction equations.
  #' @param regex_arrow Regular expression for the arrow splitting sides of the reaction equation.
  #'
  #' @return a \code{data_frame}, with columns: \describe{
  #'   \item{reversible}{boolean, is reaction reversible}
  #'   \item{before}{the left hand side of the reaction string}
  #'   \item{after}{the right hand side of the reaction string}
  #' }
  #'
  #' @import dplyr
  #' @import stringr
  #' @importFrom rlang .data
  #' @keywords internal
  #' @noRd
  split_on_arrow <- function(equations, regex_arrow = '<?[-=]+>'){
    stopifnot("each equation must contain exactly one reaction arrow" =
                all(stringr::str_count(equations, regex_arrow) == 1))
    
    split <- stringr::str_split_fixed(equations, regex_arrow, 2)
    
    colnames(split) <- c('before', 'after')
    
    split |>
      tibble::as_tibble() |>
      dplyr::mutate(reversible = equations |>
                      stringr::str_extract(regex_arrow) |>
                      stringr::str_detect('<'),
                    before = stringr::str_trim(.data$before),
                    after = stringr::str_trim(.data$after)
      )
  }
  
  
  #' Expand half reaction equations into a long form
  #'
  #' This function is based on \code{fbar::parse_met_list}.
  #'
  #' @param mets Character vector of halves of reaction equations.
  #'
  #' @return a \code{date_frame} with columns: \describe{
  #'   \item{stoich}{the stoichiometric coefficient}
  #'   \item{met}{the metabolite}
  #' }
  #'
  #' @import dplyr
  #' @import stringr
  #' @keywords internal
  #' @noRd
  parse_met_list <- function(mets){
    pattern_stoich <- '^[[:space:]]*[[:digit:].()e-]+[[:space:]]+'
    stoich <- mets |>
      stringr::str_extract(pattern_stoich) |>
      stringr::str_replace_all('[[:space:]()]+','') |>
      as.numeric()
    stoich[is.na(stoich)] <- 1
    met <- mets |>
      stringr::str_replace(pattern_stoich,'') |>
      stringr::str_trim()
    tibble::tibble(stoich, met)
  }
  
  
  #' Validate an expanded (long-format) metabolic model
  #'
  #' Checks that the list produced by \code{reactiontbl_to_expanded} has the
  #' expected structure before it is converted to a solver model.
  #'
  #' This function is based on \code{fbar:::validate_expanded}.
  #'
  #' @param expanded A list describing the metabolic network model in long format. It should contain
  #'   data frames \code{rxns}, \code{mets}, and \code{stoich}.
  #'
  #' @return \code{TRUE} invisibly if valid; otherwise stops with an error.
  #'
  #'
  #' @keywords internal
  #' @noRd
  validate_expanded <- function(expanded){
    
    stopifnot(
      "rxns must be a data frame" =
        'data.frame' %in% class(expanded$rxns),
      "rxns must have a 'name' column" =
        'name' %in% names(expanded$rxns),
      "rxns must have an 'uppbnd' column" =
        'uppbnd' %in% names(expanded$rxns),
      "rxns must have a 'lowbnd' column" =
        'lowbnd' %in% names(expanded$rxns),
      "rxns must have an 'obj_coef' column" =
        'obj_coef' %in% names(expanded$rxns),
      "reaction names in stoich and rxns must match" =
        setequal(expanded$stoich$name, expanded$rxns$name),
      "metabolites in stoich and mets must match" =
        setequal(expanded$stoich$met, expanded$mets$met)
    )
    
    return(TRUE)
  }
  
  
  #' Parse a reaction table to an intermediate, long format
  #'
  #' The long format can also be suitable for manipulating equations.
  #'
  #' The \code{reaction_table} must have columns:
  #' \itemize{
  #'  \item \code{name},
  #'  \item \code{eq},
  #'  \item \code{uppbnd},
  #'  \item \code{lowbnd}, and
  #'  \item \code{obj_coef}.
  #' }
  #'
  #' This function is based on \code{fbar::reactiontbl_to_expanded}.
  #'   
  #' @param reaction_table A data frame describing the metabolic model.
  #' @param regex_arrow Regular expression for the arrow splitting sides of the reaction equation.
  #'
  #' @return A list of data frames: \itemize{
  #'   \item \code{rxns}, which has one row per reaction,
  #'   \item \code{mets}, which has one row for each metabolite, and
  #'   \item \code{stoich}, which has one row for each time a metabolite appears in a reaction.
  #' }
  #'
  #' @export
  #' @import dplyr
  #' @import stringr
  #' @importFrom rlang .data
  reactiontbl_to_expanded <- function(reaction_table, regex_arrow = '<?[-=]+>'){
    stopifnot(
      "reaction_table must be a data frame" =
        'data.frame' %in% class(reaction_table),
      "reaction_table must have a 'name' column" =
        'name' %in% names(reaction_table),
      "reaction_table must have an 'eq' column" =
        'eq' %in% names(reaction_table),
      "reaction_table must have an 'uppbnd' column" =
        'uppbnd' %in% names(reaction_table),
      "reaction_table must have a 'lowbnd' column" =
        'lowbnd' %in% names(reaction_table),
      "reaction_table must have an 'obj_coef' column" =
        'obj_coef' %in% names(reaction_table),
      "reaction names must be unique" =
        sum(duplicated(reaction_table$name)) == 0,
      # can't handle compartments at start of string
      "equations cannot start with a compartment tag like [c]:" =
        !any(stringr::str_detect(reaction_table$eq, '^\\[\\w+?]:'))
    )
    
    const_inf <- 1000
    
    equation_sides <- split_on_arrow(reaction_table[['eq']], regex_arrow) |>
      dplyr::mutate(name = reaction_table[['name']])
    
    
    metabolite_strings <- dplyr::bind_rows(
      equation_sides |>
        dplyr::transmute(.data$name, string = .data$before, direction = -1),
      equation_sides |>
        dplyr::transmute(.data$name, string = .data$after, direction = 1)
    )
    
    metabolite_rows <- metabolite_strings |>
      dplyr::mutate(symbol = stringr::str_split(.data$string, stringr::fixed(' + '))) |>
      (function(x){
        if(nrow(x)>0){
          tidyr::unnest(x, cols=.data$symbol)
        } else {
          return(x)
        }
      })() |>
      dplyr::filter(.data$symbol!='')
    
    stoich_table <- dplyr::bind_cols(metabolite_rows,
                                           parse_met_list(metabolite_rows$symbol)) |>
      dplyr::transmute(name = .data$name,
                       stoich = .data$stoich*.data$direction,
                       met = .data$met) |>
      dplyr::filter(.data$met!='')
    
    return(list(stoich = stoich_table |>
                  dplyr::group_by(.data$name, .data$met) |>
                  dplyr::summarise(stoich = sum(.data$stoich)) |>
                  dplyr::ungroup(),
                rxns = reaction_table |>
                  dplyr::ungroup(),
                mets = stoich_table |>
                  dplyr::group_by(.data$met) |>
                  dplyr::summarise() |>
                  dplyr::ungroup()))
  }
  
  #' Parse a long format metabolic model to an ROI model
  #'
  #' This parses the long format produced by \code{reactiontbl_to_expanded} to an ROI model.
  #'
  #' This function is based on \code{fbar::expanded_to_ROI}.
  #' 
  #' @details
  #' To solve models using ROI, you will need a solver plugin for ROI. Probably the easiest one to install is ROI.plugin.glpk.
  #' To install this in Linux, run \code{sudo apt-get install libglpk-dev} in a terminal, and then run \code{install.packages('ROI.plugin.glpk')} in R.
  #'
  #' @param expanded A list describing the metabolic network model in long format. It should contain
  #'   data frames \code{rxns}, \code{mets}, and \code{stoich}.
  #'
  #' @return An ROI optimization problem.
  #'
  #'
  #' @export
  #' @import Matrix
  #' @import ROI
  expanded_to_ROI <- function(expanded){
    
    validate_expanded(expanded)
    
    rxns <- expanded$rxns
    stoich <- expanded$stoich
    mets <- expanded$mets
    
    stoichiometric_matrix <- Matrix::sparseMatrix(j = match(stoich$name, rxns$name),
                                                  i = match(stoich$met, mets$met),
                                                  x = stoich$stoich,
                                                  dims = c(nrow(mets),
                                                           nrow(rxns)
                                                  ),
                                                  dimnames = list(metabolites=mets$met,
                                                                  reactions=rxns$name)
    )
    
    roi_model <- ROI::OP(objective = ROI::L_objective(L = rxns$obj_coef),
                       constraints =  ROI::L_constraint(L = as.matrix(stoichiometric_matrix),
                                                        dir = rep('==', times=nrow(stoichiometric_matrix)),
                                                        rhs = rep(0, times=nrow(stoichiometric_matrix))
                       ),
                       bounds=ROI::V_bound(li = seq_along(rxns$lowbnd), lb = rxns$lowbnd,
                                           ui = seq_along(rxns$uppbnd), ub = rxns$uppbnd),
                       maximum = TRUE
    )
    
    return(roi_model)
  }
  
  #' Find Fluxes for a Metabolic Model
  #'
  #' This function solves a metabolic network model and returns it with a flux
  #' value calculated for each reaction.
  #'
  #' This function is based on \code{fbar::find_fluxes_df}.
  #'
  #' @param expanded A list describing the metabolic network model in long format. It should contain
  #'   data frames \code{rxns}, \code{mets}, and \code{stoich}.
  #' @param solver The name of the solver to use (for example \code{"ecos"} or
  #'   \code{"glpk"}). If left as \code{NULL} (the default), a suitable solver is
  #'   found automatically.
  #' @return The input model with an added \code{flux} value for each reaction.
  #' @export
  find_fluxes <- function(expanded, solver = NULL) {
    # Build the solvable form of the model
    .t_roi <- Sys.time()
    roi_model <- expanded_to_ROI(expanded)
    
    # Find a solver if the caller did not name one
    if (is.null(solver)) {
      available <- ROI::ROI_applicable_solvers(roi_model)
      if (length(available) < 1) {
        stop("No suitable solver was found. The documentation includes instructions on how to install one.")
      }
      solver <- available[[1]]
    }

    # Solve the model. Naming the solver lets it skip searching for one.
    res <- ROI::ROI_solve(roi_model, solver = solver)
    
    if (res$status$code != 0) {
      reaction_table <- expanded$rxns |>
        dplyr::mutate(flux = 0)
    } else {
      reaction_table <- expanded$rxns |>
        dplyr::mutate(flux = res[["solution"]])
    }
    
    return(reaction_table)
  }

  #' Add One Enzymatic Reaction
  #' 
  #' This function adds one enzymatic reaction to a metabolic network model
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param name A character vector of names for each reaction.
  #' @param lowbnd A numeric vector of lower bounds for each reaction.
  #' @param uppbnd A numeric vector of upper bounds for each reaction.
  #' @param obj_coef A numeric vector of objective coefficients for each reaction.
  #' @param eq A character vector of equation for the reaction.
  #' @param rn A character vector of reaction ID for each reaction.
  #' @param ec A character vector of the EC number for the reaction.
  #' @param ko A character vector of KO IDs for the reaction.
  #' @param md A character vector of modules corresponding to the reaction.
  #' @return A data frame of the metabolic network model with the reaction added
  #' @export
  add_one_reaction = function(reaction_table = NULL, name = NA, lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq, rn = NA, ec = NA, ko = NA, md = NA) {
    #Add a reaction
    append = data.frame(name, lowbnd, uppbnd, obj_coef, eq, rn, ec, ko, md)
    
    #Format equations
    append$eq = format_metabolite_name(name = append$eq, remove_coefficient = FALSE)
    
    if (is.null(reaction_table) | is.function(reaction_table) | length(reaction_table) == 0) {
      reaction_table = append
    } else {
      reaction_table = rbind(reaction_table, append)
    }
    
    return(reaction_table)
  }
  
  #' Delete One Enzymatic Reaction
  #' 
  #' This function deletes one enzymatic reaction to a metabolic network model
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param ec A character vector of the EC number for the reaction.
  #' @return A data frame of the metabolic network model with the reaction deleted
  #' @export
  delete_one_reaction = function(reaction_table, ec) {
    #Delete a reaction
    reaction_table = reaction_table[-which(reaction_table$ec == ec),]
    
    return(reaction_table)
  }
  
  #' Add Reactions of Glycolysis
  #' 
  #' This function adds the 10 reactions that make up glycolysis to a metabolic network model
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @return A data frame of the metabolic network model with glycolysis reactions added
  #' @export
  add_glycolysis = function(reaction_table = NULL) {
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_1", lowbnd = 0, uppbnd = 1000, obj_coef = 0, eq = "ATP + D-Glucose <=> ADP + D-Glucose 6-phosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_2", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "D-Glucose 6-phosphate <=> D-Fructose 6-phosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_3", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "ATP + D-Fructose 6-phosphate <=> ADP + D-Fructose 1,6-bisphosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_4", lowbnd = 0, uppbnd = 1000, obj_coef = 0, eq = "D-Fructose 1,6-bisphosphate <=> Glycerone phosphate + D-Glyceraldehyde 3-phosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_5", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "D-Glyceraldehyde 3-phosphate <=> Glycerone phosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_6", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "D-Glyceraldehyde 3-phosphate + Orthophosphate + NAD+ <=> 3-Phospho-D-glyceroyl phosphate + NADH + H+", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_7", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "ATP + 3-Phospho-D-glycerate <=> ADP + 3-Phospho-D-glyceroyl phosphate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_8", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "2-Phospho-D-glycerate <=> 3-Phospho-D-glycerate", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_9", lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq = "2-Phospho-D-glycerate <=> Phosphoenolpyruvate + H2O", rn  = NA, ec = NA, ko = NA, md = NA)
    reaction_table = add_one_reaction(reaction_table, name = "Extra_reaction_10", lowbnd = -1000, uppbnd = 0, obj_coef = 0, eq = "ATP + Pyruvate <=> ADP + Phosphoenolpyruvate", rn  = NA, ec = NA, ko = NA, md = NA)
    
    reaction_table$eq = format_metabolite_name(name = reaction_table$eq, remove_coefficient = FALSE)
    
    return(reaction_table)
  }
  
  #' Format Names of Metabolites and Equations
  #' 
  #' This function formats the names of metabolites and equations in a metabolic network model
  #' It is designed to work with reactions from KEGG database
  #' It removes extra spaces, coefficients, and charges
  #' 
  #' @param name A character vector of metabolite or reaction names.
  #' @param remove_coefficient A logical indicating whether to remove coefficients from the names.
  #' @param add_underscore A logical indicating whether to add underscores to the names.
  #' @param remove_charge A logical indicating whether to remove charges from the names.
  #' @return A character vector of formatted names
  #' @export
  format_metabolite_name = function(name, remove_coefficient = TRUE, add_underscore = TRUE, remove_charge = TRUE) {
    name = gsub(pattern = "alpha-D", replacement = "D", x = name)
    name = gsub(pattern = "beta-D", replacement = "D", x = name)
    name = gsub(pattern = "^ ", replacement = "", x = name)
    name = gsub(pattern = " $", replacement = "", x = name)
    name = gsub(pattern = "^n ", replacement = "", x = name) # remove "n" as coefficient
    name = gsub(pattern = "\\+ n ", replacement = "+ ", x = name) # remove any remaining "n" as coefficient
    
    if (add_underscore == TRUE) {
      name = gsub(pattern = "-", replacement = "_", x = name)
      name = gsub(pattern = "([aA-zZ])( )([aA-zZ0-9])", replacement = "\\1_\\3", x = name)
      name = gsub(pattern = "(\\))( )", replacement = "\\1_", x = name)
      name = gsub(pattern = "(,)", replacement = "_", x = name)
    }
    
    if (remove_charge == TRUE) {
      name = gsub(pattern = "([aA-zZ0-9])(\\+)", replacement = "\\1", x = name)
    }
    
    if (remove_coefficient == TRUE) {
      name = gsub(pattern = "^\\d+ ", replacement = "", x = name)
    }
    
    return(name)
  }
  
  #' Remove Redundant Reactions
  #' 
  #' This function removes redundant reactions from a metabolic network model.
  #' It keeps only one reaction for each unique eq and ko.
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @return A data frame of the metabolic network model with redundant reactions removed
  #' @export
  #' @importFrom dplyr distinct
  remove_redundant_reactions = function(reaction_table) {
    reaction_table <- reaction_table |> dplyr::distinct(eq, ko, .keep_all = TRUE)
    
    return(reaction_table)
  }
  
  #' Remove Null Reactions
  #'
  #' This function removes reactions where the substrates and products are identical,
  #' such as "A <=> A", or "A + B <=> A + B".
  #'
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @return A data frame with null reactions removed
  #' @export
  #' @importFrom stringr str_split
  remove_null_reactions <- function(reaction_table) {
    eq_split <- stringr::str_split(reaction_table$eq, pattern = "<=>", simplify = TRUE)
    lhs <- trimws(eq_split[, 1])
    rhs <- trimws(eq_split[, 2])
    
    # Split into metabolite sets
    lhs_split <- lapply(lhs, function(x) sort(trimws(unlist(strsplit(x, " \\+ ")))))
    rhs_split <- lapply(rhs, function(x) sort(trimws(unlist(strsplit(x, " \\+ ")))))
    
    # Identify reactions where the sets are identical
    is_null <- mapply(function(a, b) identical(a, b), lhs_split, rhs_split)
    
    # Remove null reactions
    reaction_table <- reaction_table[!is_null, ]
    return(reaction_table)
  }
 
  #' Specify Which Metabolites Are Unbalanced
  #'
  #' This function specifies which metabolites are unbalanced
  #' Unbalanced metabolites which can accumulate (or be consumed) in unlimited quantities
  #' NADH and ATP are examples of metabolites usually assumed to be unbalanced
  #' In the metabolic network, these can accumulate without needing to be regenerated to NAD+ or ADP
  #' This simplifies the network, as reactions for consuming NADH and ATP don't have to be included
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param names A character vector of metabolite names.
  #' @param lowbnd A numeric vector of lower bounds for each reaction.
  #' @param uppbnd A numeric vector of upper bounds for each reaction.
  #' @return A data frame of the metabolic network model with unbalanced metabolites added
  #' @export
  #' @importFrom stringr str_split
  add_unbalanced_metabolites = function(reaction_table, names, lowbnd = -10^6, uppbnd = 10^6) {
    #Get names of all metabolites
    metabolites = reaction_table
    metabolites = stringr::str_split(string = metabolites$eq, pattern = "<=>", simplify = TRUE)
    metabolites = as.character(metabolites)
    metabolites = stringr::str_split(string = metabolites, pattern = " \\+", simplify = TRUE)
    metabolites = as.character(metabolites)
    metabolites = format_metabolite_name(metabolites)
    metabolites = unique(metabolites)
    
    #Get names of unbalanced metabolites
    index = names %in% metabolites
    names = names[index]
    
    #Format equations for unbalanced metabolites
    name = paste0("Unbalanced_metabolite_", seq_len(length(names)))
    lowbnd = lowbnd[index]
    uppbnd = uppbnd[index]
    obj_coef = 0
    eq = paste0(names, " <=>")
    rn = NA
    ec = NA
    ko = NA
    md = NA
    append = data.frame(name, lowbnd, uppbnd, obj_coef, eq, rn, ec, ko, md)
    
    reaction_table = rbind(reaction_table, append)
    
    return(reaction_table)
  }
  
  #' Specify Starting and Ending Metabolites
  #' 
  #' This function specifies the starting and ending metabolites in a metabolic network model
  #' For glucose fermentation, the starting metabolite would be glucose
  #' and the ending metabolite would be lactate or another product.
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param starting_metabolite A character vector of the starting metabolite in the network.
  #' @param ending_metabolite A character vector of the ending metabolite in the network.
  #' @param lowbnd A numeric vector of lower bounds for each reaction.
  #' @param uppbnd A numeric vector of upper bounds for each reaction.
  #' @return A data frame of the metabolic network model with the starting and ending metabolites added
  #' @export
  #' @importFrom dplyr filter distinct
  add_target_metabolites = function(reaction_table, starting_metabolite, ending_metabolite, lowbnd = -1000, uppbnd = 10^6) {
    name = c("Starting_metabolite", "Ending_metabolite")
    lowbnd = c(lowbnd, 0)
    uppbnd = c(0, uppbnd)
    obj_coef = c(0, 1)
    eq = c(paste0(starting_metabolite, " <=>"), paste0(ending_metabolite, " <=>"))
    rn = NA
    ec = NA
    ko = NA
    md = NA
    
    match = match(x = eq, table = reaction_table$eq)
    match = match[!is.na(match)]
    if (length(match) > 0) {
      reaction_table = reaction_table[-match,]
    }
    
    append = data.frame(name, lowbnd, uppbnd, obj_coef, eq, rn, ec, ko, md)
    reaction_table = rbind(reaction_table, append)
    
    return(reaction_table)
  }
  
  #' Specify Starting and Ending Metabolites in an Expanded Model
  #' 
  #' This function specifies the starting and ending metabolites in an expanded metabolic network model
  #' 
  #' @param expanded A list describing the metabolic network model in long format. It should contain
  #'   data frames \code{rxns}, \code{mets}, and \code{stoich}.
  #' @param starting_metabolite A character vector of the starting metabolite in the network.
  #' @param ending_metabolite A character vector of the ending metabolite in the network.
  #' @param lowbnd A numeric vector of lower bounds for each reaction.
  #' @param uppbnd A numeric vector of upper bounds for each reaction.
  #' @return A long format metabolic network model with the starting and ending metabolites added
  #' @export
  set_target_metabolites = function(expanded, starting_metabolite, ending_metabolite, lowbnd = -1000, uppbnd = 10^6) {
    name = c("Starting_metabolite", "Ending_metabolite")
    lowbnd = c(lowbnd, 0)
    uppbnd = c(0, uppbnd)
    obj_coef = c(0, 1)
    eq = c(paste0(starting_metabolite, " <=>"), paste0(ending_metabolite, " <=>"))
    rn = NA
    ec = NA
    ko = NA
    md = NA
    
    to_remove = expanded$rxns$name[expanded$rxns$eq %in% eq | expanded$rxns$name %in% name]
    expanded$rxns = expanded$rxns[!expanded$rxns$name %in% to_remove,]
    expanded$stoich = expanded$stoich[!expanded$stoich$name %in% to_remove,]
    
    append = data.frame(name, lowbnd, uppbnd, obj_coef, eq, rn, ec, ko, md)
    expanded$rxns = rbind(expanded$rxns, append)
    
    append = data.frame(name, met = c(starting_metabolite, ending_metabolite), stoich = -1)
    expanded$stoich = rbind(expanded$stoich, append)
    expanded$mets = expanded$stoich |>
      dplyr::select(.data$met) |>
      dplyr::distinct()
    
    return(expanded)
  }
  
  #' Find Enzymes in Genome
  #' 
  #' This function finds which enzyme reactions in a metabolic network are present in an organism's genome.
  #' The user provides a metabolic network model along with database IDs for the organism's genes.
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param gene_functions A character vector of database IDs for the organism's genes.
  #' @param all_subunits A logical indicating if enzymes are considered present only if all subunits match (default TRUE).
  #' @return A character vector of reaction IDs for enzymes present in the genome.
  #' @export
  #' @importFrom stringr str_split
  find_enzymes = function(reaction_table, gene_functions, all_subunits = TRUE) {
    x = stringr::str_split(reaction_table$ko, pattern = ", ")
    for (i in seq_along(x)) {
      for (j in seq_along(x[[i]])) {
        x[[i]][j] = x[[i]][j] %in% gene_functions
      }
    }  
    
    if(all_subunits == TRUE) {
      for (i in seq_along(x)) {
        x[[i]] = all(x[[i]] == TRUE)
      }
    } else {
      for (i in seq_along(x)) {
        x[[i]] = any(x[[i]] == TRUE)
      }
    }
    
    x = unlist(x)
    
    enzymes = reaction_table$ko[x]
    
    return(enzymes)
  }
  
  #' Create Initial Dataframe for Metabolic Network Model
  #' 
  #' This function creates an initial dataframe for the metabolic network model
  #' The user specifies the name, lower bound, upper bound, objective coefficient, eq, 
  #' official name, gene association, and md for each reaction
  #' 
  #' @param name A character vector of names for enzyme for the reaction.
  #' @param lowbnd A numeric vector of lower bounds for each reaction.
  #' @param uppbnd A numeric vector of upper bounds for each reaction.
  #' @param obj_coef A numeric vector of objective coefficients for each reaction.
  #' @param eq A character vector of equation for the reaction.
  #' @param rn A character vector of reaction ID for each reaction.
  #' @param ec A character vector of the EC number for the reaction.
  #' @param ko A character vector of KO IDs for the reaction.
  #' @param md A character vector of modules corresponding to the reaction.
  #' @return A data frame of the formatted data
  #' @export
  #' @importFrom dplyr distinct
  create_network_dataframe = function(name = NA, lowbnd = -1000, uppbnd = 1000, obj_coef = 0, eq, rn = NA, ec = NA, ko = NA, md = NA) {
    # Create dataframe
    reaction_table = data.frame(name, lowbnd, uppbnd, obj_coef, rn, eq, ec, ko, md)
    
    # Format equations  
    reaction_table$eq = format_metabolite_name(name = reaction_table$eq, remove_coefficient = FALSE)
    
    # Remove extra rows
    reaction_table = dplyr::distinct(reaction_table)
    reaction_table = reaction_table[which(reaction_table$eq != ""),]
    
    return(reaction_table)
  }
  
  #' Build Metabolic Network Model
  #' This function builds a metabolic network model from a set of enzymatic reactions
  #' The user specifies the eq, way, name, official name, gene association, and md for each reaction
  #' If no eq is provided, the function will create a model with reactions of glycolysis as an example 
  #' 
  #' @param eq A character vector of equation for the reaction.
  #' @param way A character vector of the way of each reaction (Forward, Reverse, or Bidirectional)
  #' @param name A character vector of names for enzyme for the reaction.
  #' @param rn A character vector of reaction ID for each reaction.
  #' @param ec A character vector of the EC number for the reaction.
  #' @param ko A character vector of KO IDs for the reaction.
  #' @param md A character vector of modules corresponding to the reaction.
  #' @param starting_metabolite A character vector of the starting metabolite in the network.
  #' @param ending_metabolite A character vector of the ending metabolite in the network.
  #' @param unbalanced_intermediates A character vector of metabolites that are unbalanced and can accumulate.
  #' @param unbalanced_products A character vector of metabolites that are unbalanced and can accumulate.
  #' @param remove_redundant_reactions A logical indicating whether to remove redundant reactions.
  #' @param remove_null_reactions A logical indicating whether to remove null reactions.
  #' @param add_glycolysis A logical indicating whether to add the 10 reactions of glycolysis.
  #' @return A data frame of the metabolic network model
  #' @examples
  #' build_network_model()
  #' @export
  #' @importFrom dplyr if_else
  build_network_model = function(eq = NULL, 
                                 way = "Bidirectional", 
                                 name = NA, 
                                 rn = NA,
                                 ec = NA, 
                                 ko = NA, 
                                 md = NA, 
                                 starting_metabolite = "D-Glucose", 
                                 ending_metabolite = "Pyruvate", 
                                 unbalanced_intermediates = c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), 
                                 unbalanced_products = NULL, remove_redundant_reactions = TRUE, 
                                 remove_null_reactions = TRUE, add_glycolysis = FALSE) {
    if (is.null(eq)) {
      eq = add_glycolysis()$eq
    }
    
    # Create Initial Dataframe For Network Model
    uppbnd = vector(length = length(eq))
    lowbnd = vector(length = length(eq))
    uppbnd = dplyr::if_else(way == "Reverse", 0, 1000)
    lowbnd = dplyr::if_else(way == "Forward", 0, -1000)
    reaction_table = create_network_dataframe(name, lowbnd = lowbnd, uppbnd = uppbnd, obj_coef = 0, eq = eq, rn = rn, ec = ec, ko = ko, md = md)
    reaction_table$name = paste0("Enzyme_", seq_len(nrow(reaction_table)))

    # Add Enzymes of Glycolysis
    if (add_glycolysis == TRUE) {
      reaction_table = add_glycolysis(reaction_table = reaction_table)
    }
    
    # Remove Redundant Reactions
    if (remove_redundant_reactions == TRUE) {
      reaction_table = remove_redundant_reactions(reaction_table)
    }
    
    # Remove Null Reactions
    if (remove_null_reactions == TRUE) {
      reaction_table = remove_null_reactions(reaction_table)
    }
    
    # Add Target Metabolites
    if (!is.null(starting_metabolite) & !is.null(ending_metabolite)) {
      starting_metabolite = format_metabolite_name(starting_metabolite)
      ending_metabolite = format_metabolite_name(ending_metabolite)
      reaction_table = add_target_metabolites(reaction_table = reaction_table, starting_metabolite = starting_metabolite, ending_metabolite = ending_metabolite)
    }
    
    # Add Unbalanced Metabolites
    if (!is.null(unbalanced_intermediates) & !is.null(unbalanced_products)) {
      unbalanced_intermediates = format_metabolite_name(unbalanced_intermediates)
      unbalanced_products = format_metabolite_name(unbalanced_products)
      unbalanced_intermediates = unbalanced_intermediates[!unbalanced_intermediates %in% ending_metabolite]
      unbalanced_products = unbalanced_products[!unbalanced_products %in% ending_metabolite]
      names = c(unbalanced_intermediates, unbalanced_products)
      lowbnd = c(rep(-10^6, times = length(unbalanced_intermediates)), rep(0, times = length(unbalanced_products)))
      uppbnd = c(rep(10^6, times = length(unbalanced_intermediates)), rep(10^6, times = length(unbalanced_products)))
      reaction_table = add_unbalanced_metabolites(reaction_table = reaction_table, names = names, lowbnd = lowbnd, uppbnd = uppbnd)
    }
    
    return(reaction_table)
  }
  
  #' Simplify Network Model
  #' 
  #' This function removes extra reactions from a network model
  #' The user specifies which reactions to keep (according to ko)
  #' The rest are removed (or have flux is constrained to 0)
  #' The reactions that are removed are usually ones that are absent from a specific organism
  #' 
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param ko A character vector of KO IDs for the reaction.
  #' @param remove_NA A logical indicating whether to remove reactions that have NA for ko.
  #' @param constrain_flux A logical indicating whether to constrain flux to 0 for removed reactions.
  #' @param remove_reactions A logical indicating whether to remove reactions.
  #' @return A data frame of the simplified metabolic network model
  #' @export
  #' @importFrom dplyr filter
  simplify_network_model <- function(reaction_table, ko = NULL, remove_NA = FALSE, constrain_flux = TRUE, remove_reactions = FALSE) {
    # Identify which reactions to remove
    to_remove <- reaction_table |> 
      dplyr::filter(!grepl(pattern = "Unbalanced_metabolite", name)) |> 
      dplyr::pull(ko)
    
    if(remove_NA)
    {
      to_remove <- which(to_remove %nin% ko)
    }else{
      to_remove <- which(!is.na(to_remove) & (to_remove %nin% ko))
    }
    
    # Constrain flux
    if (constrain_flux == TRUE) {
      reaction_table$lowbnd[to_remove] = 0
      reaction_table$uppbnd[to_remove] = 0
    }
    
    # Remove reactions
    if (remove_reactions == TRUE) {
      reaction_table = reaction_table[-to_remove,]
    }
    
    return(reaction_table)
  }
  
  #' Solve Metabolic Network Models for An Organism
  #'
  #' This function builds and solves metabolic network model specific for an organism.
  #' The user provide the network model (usually a reference model containing reactions occurring across all organisms),
  #' the database IDs of the organism's genes, a set of substrates, and a set of products.
  #' The function returns a set of solved network models encompassing all combinations substrates and products.
  #'
  #' @param expanded A list describing the metabolic network model in long format. It should contain
  #'   data frames \code{rxns}, \code{mets}, and \code{stoich}.
  #' @param gene_functions A character vector of database IDs corresponding to the organism's genes.
  #' @param substrates A vector of substrate metabolites.
  #' @param products A vector of product metabolites.
  #' @param all_subunits A logical indicating if enzymes are considered present only if all subunits match. Default is `TRUE`.
  #' @param solver The name of the solver to use, passed through to
  #'   \code{find_fluxes}. If \code{NULL} (the default), each solve finds its own
  #'   solver.
  #' @return A nested list of solved network models for each combination of substrate and product.
  #'   Combinations where the substrate and the end product are the same compound are left empty,
  #'   as they have no prediction.
  #' @export
  solve_network_model <- function(expanded, gene_functions, substrates, products, all_subunits = TRUE, solver = NULL) {
    .t_setup <- Sys.time()
    # Find Enzymes
    ko <- find_enzymes(reaction_table = expanded$rxns, gene_functions = gene_functions, all_subunits = all_subunits)
    
    # Simplify Model
    expanded$rxns <- simplify_network_model(reaction_table = expanded$rxns, ko = ko)

    # Initialize named list for substrates
    solved_models <- setNames(vector("list", length(substrates)), substrates)
    
    for (j in seq_along(substrates)) {
      substrate <- substrates[j]
      starting_metabolite <- rep(substrate, times = length(products))
      
      # Initialize named list for products
      solved_models[[substrate]] <- setNames(vector("list", length(products)), products)
      
      for (k in seq_along(products)) {
        product <- products[k]
        
        # Format names of target metabolites
        starting_name <- format_metabolite_name(starting_metabolite[k])
        ending_name <- format_metabolite_name(product)
        
        # Do not find fluxes when starting and ending target metabolites are identical
        if (identical(starting_name, ending_name)) {
          next
        }
        
        # Add target metabolites
        .t_at <- Sys.time()
        expanded_model <- set_target_metabolites(
          expanded = expanded,
          starting_metabolite = starting_name,
          ending_metabolite = ending_name
        )

        # Find fluxes
        solved_models[[substrate]][[product]] <- find_fluxes(expanded_model, solver = solver)
      }
    }
    
    return(solved_models)
  }
  
  #' Find a solver that can handle a metabolic model
  #'
  #' Solving a metabolic model requires a numerical solver. Several may be
  #' installed, and not all of them can handle every kind of model. This function
  #' looks at a model and returns the name of a solver that is able to solve it.
  #'
  #' @param roi_model An ROI optimization problem created by \code{expanded_to_ROI}.
  #'
  #' @return The name of a solver that can handle the model, as a single piece of
  #'   text. If no installed solver can handle it, the function stops with a
  #'   message explaining that a solver needs to be installed.
  #'
  #' @export
  find_solver <- function(roi_model) {
    available <- ROI::ROI_applicable_solvers(roi_model)
    
    if (length(available) < 1) {
      # Diagnostic: capture what this process (possibly a mirai worker) sees.
      # Worker console output is invisible, so write to a file we can inspect.
      diag <- c(
        paste("time:", format(Sys.time())),
        paste("installed solvers:", paste(ROI::ROI_installed_solvers(), collapse = ", ")),
        paste("registered solvers:", paste(ROI::ROI_registered_solvers(), collapse = ", ")),
        paste("ROI.plugin.ecos on search path:", "ROI.plugin.ecos" %in% loadedNamespaces()),
        paste("ROI.plugin.glpk on search path:", "ROI.plugin.glpk" %in% loadedNamespaces()),
        paste(".libPaths:", paste(.libPaths(), collapse = ", "))
      )
      writeLines(diag, file.path(tempdir(), "solver_diagnostic.txt"))
      # also try the app dir, in case tempdir differs per process
      try(writeLines(diag, "solver_diagnostic.txt"), silent = TRUE)
      stop("No suitable solver was found. The documentation includes instructions on how to install one.")
    }
    
    available[[1]]
  }
  
  #' Get Solved Metabolic Network Models Across Organisms
  #'
  #' This function solves metabolic network models across organisms.
  #'
  #' @param gene_functions A data frame with gene functions for each organism (columns = organisms).
  #' @param organism_names A character vector of organism names (must match columns of gene_functions).
  #' @param reaction_table A data frame describing the metabolic network model, with one row per reaction.
  #'   It should include columns \code{name}, \code{eq}, \code{lowbnd}, \code{uppbnd}, \code{obj_coef},
  #'   \code{rn}, \code{ec}, \code{ko}, and \code{md}.
  #' @param substrates A character vector of substrate compounds.
  #' @param products A character vector of product compounds.
  #' @param all_subunits Logical; whether all subunits must be present to enable a reaction.
  #' @param progress_file Path to a progress \code{.rds} file (typically created
  #'   by \code{create_job_filepaths()}), or \code{NULL} to disable progress
  #'   reporting.  Default is \code{NULL}.
  #' @param find_solver_once Logical. If \code{TRUE} (the default), find a
  #'   suitable solver one time here and reuse it for every solve below. If
  #'   \code{FALSE}, every solve finds its own solver (the old, slower behavior).
  #'   This flag exists mainly to let you measure the speed difference; in normal
  #'   use leave it \code{TRUE}.
  #' @return A named list of solved models, one per organism.
  #' @export
  get_solved_models <- function(gene_functions, organism_names,
                                reaction_table,
                                substrates, products, all_subunits = TRUE,
                                progress_file = NULL,
                                find_solver_once = TRUE) {
    # Get expanded network model
    expanded <- reactiontbl_to_expanded(reaction_table)
    
    # Find a solver 
    solver <- NULL
    if (find_solver_once) {
      roi_model <- expanded |>
        
        expanded_to_ROI()
      solver <- find_solver(roi_model)
    }
    
    # Get number of organisms
    n_organisms <- length(organism_names)

    # Initialize values
    solved_models <- setNames(vector("list", n_organisms), organism_names)
    
    for (i in seq_along(organism_names)) {
      organism_name <- organism_names[i]
      filtered_gene_functions <- gene_functions[, i]
      filtered_gene_functions <- filtered_gene_functions[!is.na(filtered_gene_functions) & filtered_gene_functions != ""]
      
      cat(file = stderr(), paste0("Solving model for organism: ", organism_name, "\n"))
      
      solved_models[[organism_name]] <- solve_network_model(
        expanded = expanded,
        gene_functions = filtered_gene_functions,
        substrates = substrates,
        products = products,
        all_subunits = all_subunits,
        solver = solver
      )
      
      # Update progress
      write_progress(progress_file,
                     progress = i / max(1L, n_organisms),
                     message = "Prediction in progress"
      )
    }

    solved_models
  }
  
  #' Get Flux Values from Network Models
  #'
  #' This function gets the flux values for metabolites in a set of metabolic network models.
  #'
  #' @param solved_models A nested list of solved network models from `get_solved_models()`. Structure: [organism][substrate][product] = data frame with flux results.
  #' @param organism_names A character vector of organism names (same order as `solved_models`).
  #' @param substrates A character vector of substrates (same order as 2nd level of `solved_models`).
  #' @param products A character vector of products (same order as 3rd level of `solved_models`).
  #'
  #' @return A `tibble` with one row per (organism, substrate, product) and a column for the flux value.
  #' 
  #' @export
  #' @importFrom purrr pmap_dfr
  #' @importFrom tibble tibble
  get_fluxes <- function(solved_models, organism_names, substrates, products) {
    # Create all combinations of indices
    combos <- expand.grid(
      i = seq_along(organism_names),
      j = seq_along(substrates),
      k = seq_along(products),
      stringsAsFactors = FALSE
    )
    
    # Map over combinations and extract flux
    purrr::pmap_dfr(combos, function(i, j, k) {
      reaction_table <- solved_models[[i]][[j]][[k]]
      
      # Report flux is NA if no solved model
      if (is.null(reaction_table)) {
        flux_value <- NA_real_
      } else {
        match_index <- which(reaction_table$name == "Ending_metabolite")
        flux_value <- if (length(match_index) > 0) reaction_table$flux[match_index] else NA_real_
      }
      
      tibble::tibble(
        `Organism number` = i,
        `Organism name` = organism_names[i],
        `Substrate` = substrates[j],
        `End product` = products[k],
        `Flux` = flux_value
      )
    })
  }
  
  #' Compute Predictions from Metabolic Networks
  #'
  #' This is the main function for predicting traits in the module.
  #'
  #' @param reference_network A data frame of the reference network.
  #' @param gene_functions A data frame of gene functions.
  #' @param substrates Character vector of substrates.
  #' @param products Character vector of products.
  #' @param unbalanced_intermediates Character vector of unbalanced intermediates.
  #' @param all_subunits Logical, whether all enzyme subunits are required for activity.
  #' @param progress_file Path to a progress \code{.rds} file (typically created
  #'   by \code{create_job_filepaths()}), or \code{NULL} to disable progress
  #'   reporting.  Default is \code{NULL}.
  #' @return A named list with solved models and fluxes.
  #' @export
  compute_network_predictions <- function(reference_network, gene_functions, substrates, products, unbalanced_intermediates, all_subunits,
                                          progress_file = NULL) {
    # Update progress
    write_progress(progress_file, 0, "Getting reference network model")
    
    # Get reference network model
    reaction_table <- build_network_model(
      eq = reference_network$eq,
      way = reference_network$way,
      name = reference_network$name,
      rn = reference_network$rn,
      ec = reference_network$ec,
      ko = reference_network$ko,
      md = reference_network$md,
      starting_metabolite = NULL,
      ending_metabolite = NULL,
      unbalanced_intermediates = unbalanced_intermediates,
      unbalanced_products = products[products %nin% unbalanced_intermediates],
      remove_redundant_reactions = FALSE
    )

    # Update progress
    write_progress(progress_file, 0, "Prediction in progress")
    cat(file = stderr(), paste0("Started prediction at ", Sys.time(), "\n"))
    
    # Get fluxes
    organism_names <- colnames(gene_functions)
    solved_models <- get_solved_models(gene_functions, 
                                       organism_names, 
                                       reaction_table, 
                                       substrates, products, 
                                       all_subunits, 
                                       progress_file = progress_file)
    fluxes <- get_fluxes(solved_models, organism_names, substrates, products)

    # Update progress
    cat(file = stderr(), paste0("Ended prediction at ", Sys.time(), "\n"))
    
    return(list(solved_models = solved_models, fluxes = fluxes, reaction_table = reaction_table))
  }

  #' Pack solved models into a compact form for saving
  #'
  #' The solved models nearly all hold the same content.  Every model carries a
  #' copy of the reference reaction table and differs only in its flux values,
  #' its bounds, and the target metabolite rows that were added to it.  Saving
  #' them whole repeats the reaction table once per model, which makes the job
  #' file very large.  This function stores the shared parts one time and keeps
  #' only the differing parts for each model.
  #'
  #' Use \code{rebuild_solved_model()} to get a single model back.
  #'
  #' @param solved_models A nested list of solved models from \code{get_solved_models()}.
  #' @param reaction_table The reference reaction table the models were built from.
  #' @return A list holding the reference reactions, the bounds for each organism,
  #'   the rows used by each substrate and product, and the fluxes for each model.
  #' @export
  pack_solved_models <- function(solved_models, reaction_table) {
    organism_names <- names(solved_models)

    # Collect the bounds for each organism
    bounds <- setNames(vector("list", length(organism_names)), organism_names)
    for (organism in organism_names) {
      lowbnd <- reaction_table$lowbnd
      uppbnd <- reaction_table$uppbnd
      for (substrate_models in solved_models[[organism]]) {
        for (model in substrate_models) {
          if (is.null(model)) next
          index <- match(model$name, reaction_table$name)
          found <- !is.na(index)
          lowbnd[index[found]] <- model$lowbnd[found]
          uppbnd[index[found]] <- model$uppbnd[found]
        }
      }
      bounds[[organism]] <- list(lowbnd = lowbnd, uppbnd = uppbnd)
    }

    # Collect the rows each substrate and product uses
    first_models <- solved_models[[1]]
    targets <- setNames(vector("list", length(first_models)), names(first_models))
    for (substrate in names(first_models)) {
      product_models <- first_models[[substrate]]
      targets[[substrate]] <- setNames(vector("list", length(product_models)), names(product_models))
      for (product in names(product_models)) {
        model <- product_models[[product]]
        if (is.null(model)) next
        index <- match(model$name, reaction_table$name)
        found <- !is.na(index)
        targets[[substrate]][[product]] <- list(
          keep  = index[found],
          extra = model[!found, setdiff(names(model), "flux"), drop = FALSE]
        )
      }
    }

    # Collect the fluxes for each model
    fluxes <- setNames(vector("list", length(organism_names)), organism_names)
    for (organism in organism_names) {
      substrate_models <- solved_models[[organism]]
      fluxes[[organism]] <- setNames(vector("list", length(substrate_models)), names(substrate_models))
      for (substrate in names(substrate_models)) {
        product_models <- substrate_models[[substrate]]
        fluxes[[organism]][[substrate]] <- setNames(vector("list", length(product_models)), names(product_models))
        for (product in names(product_models)) {
          model <- product_models[[product]]
          if (is.null(model)) next
          fluxes[[organism]][[substrate]][[product]] <- model$flux
        }
      }
    }

    list(reactions = reaction_table, bounds = bounds, targets = targets, fluxes = fluxes)
  }

  #' Rebuild one solved model
  #'
  #' This function puts a single solved model back together from the packed form
  #' written by \code{pack_solved_models()}.  It returns the same data frame the
  #' model was saved from, with one row per reaction and a flux value for each.
  #'
  #' @param solved_models The packed models from a job file.
  #' @param organism The name of the organism.
  #' @param substrate The name of the substrate.
  #' @param product The name of the end product.
  #' @return A data frame of the model, or \code{NULL} when there is no model for
  #'   this combination.
  #' @export
  rebuild_solved_model <- function(solved_models, organism, substrate, product) {
    target <- solved_models$targets[[substrate]][[product]]
    flux <- solved_models$fluxes[[organism]][[substrate]][[product]]

    if (is.null(target) || is.null(flux)) {
      return(NULL)
    }

    # Take the shared reactions and apply the bounds for this organism
    model <- solved_models$reactions[target$keep, , drop = FALSE]
    model$lowbnd <- solved_models$bounds[[organism]]$lowbnd[target$keep]
    model$uppbnd <- solved_models$bounds[[organism]]$uppbnd[target$keep]

    # Add the target metabolite rows and the fluxes
    model <- rbind(model, target$extra)
    model$flux <- flux
    rownames(model) <- NULL

    model
  }

  #' Run a computation job for the metabolic networks module
  #'
  #' This functions runs a computation job for module.  It involves running the 
  #' main function for predicting traits then saving the result to a file.
  #'
  #' @param reference_network A reference network data frame.
  #' @param gene_functions A data frame of gene functions for the query organisms.
  #' @param organism_names A character vector of organism names.
  #' @param substrates A character vector of selected substrates.
  #' @param products A character vector of selected products.
  #' @param unbalanced_intermediates A character vector of metabolites to exclude.
  #' @param all_subunits Logical.
  #' @param tree A phylo object to plot the results on, or NULL.
  #' @param metadata A data frame of organism metadata to show in the tree hover text, or NULL.
  #' @param job_id Character job id.
  #' @param job_dir Directory where the result file is to be written.
  #' @param progress_file Path to the progress \code{.rds} file, or \code{NULL}.
  #' @return Invisibly \code{TRUE}.
  #' @export
  run_job_network <- function(reference_network, gene_functions, organism_names,
                                       substrates, products, unbalanced_intermediates,
                                       all_subunits, tree = NULL, metadata = NULL,
                                       job_id, job_dir, progress_file = NULL) {
    result <- compute_network_predictions(
      reference_network        = reference_network,
      gene_functions           = gene_functions,
      substrates               = substrates,
      products                 = products,
      unbalanced_intermediates = unbalanced_intermediates,
      all_subunits             = all_subunits,
      progress_file            = progress_file
    )

    payload <- list(
      get_organism_names           = organism_names,
      get_input_substrates         = substrates,
      get_input_products           = products,
      get_unbalanced_intermediates = unbalanced_intermediates,
      get_solved_models            = pack_solved_models(result$solved_models, result$reaction_table),
      predict_fluxes               = result$fluxes,
      tree                         = tree,
      organism_metadata            = metadata
    )

    save_job_result(job_id = job_id, result = payload, job_dir = job_dir)

    invisible(TRUE)
  }
  
# === Generating user interface (UI) ===  
  #' Create a Custom File Input UI with Modal Trigger
  #'
  #' This function creates a UI element that mimics a Shiny file input box but instead launches a modal dialog 
  #' when clicked. It is useful in cases where the "file input" is generated or selected interactively 
  #' (e.g., via a network builder or configuration tool) rather than uploaded.
  #'
  #' The element includes a styled button labeled \code{"Build..."} that triggers a modal when clicked, and a 
  #' read-only text field showing the name or status of the "file". An optional label can be displayed above.
  #'
  #' @param id The module ID used to namespace the input elements.
  #' @param label Optional text label shown above the pseudo file input. Default is `NULL`.
  #' @return A Shiny UI tag list simulating a file input with a modal trigger.
  #' @export
  #' @importFrom shiny NS div tags actionLink
  create_network_input <- function(id, label = NULL) {
    ns <- NS(id)
    
    div(
      class = "form-group shiny-input-container",
      if (!is.null(label)) create_input_label(ns("network_builder"), label),
      
      # File input with Build button
      div(
        class = "input-group",
        tags$label(
          class = "input-group-btn input-group-prepend",
          actionLink(
            inputId = ns("open_network_builder"),
            label = tags$span(class = "btn btn-default btn-file", "Build...")
          )
        ),
        tags$input(
          id = ns("network_name_display"),
          type = "text",
          class = "form-control",
          placeholder = "No network built",
          readonly = "readonly"
        )
      )
    )
  }
  
  
  # create_conditional_flex_item() moved to functions/userInterfaceFunctions.R,
  # where all four modules can reach it, and gained an item_class argument.
  
# === Updating user interface (UI) elements ===
  #' Get Metabolite Names
  #' 
  #' This function gets names of metabolites from a reaction eq
  #' It is designed to work with reactions from KEGG database
  #' 
  #' @param eq A character vector of equation for the reaction.
  #' @param to_remove A character vector of metabolites to remove.
  #' @return A character vector of metabolite names
  #' @export
  #' @importFrom stringr str_split
  get_metabolite_names = function(eq, to_remove = NULL) {
    name_list <- stringr::str_split(eq, pattern = "<=>")
    name <- unlist(lapply(name_list, function(part) {
      stringr::str_split(part, pattern = "[ ]\\+")
    }))
    
    name <- trimws(name)  # Remove any leading/trailing whitespace
    name <- name[name != ""]  # Remove empty strings
    
    name <- format_metabolite_name(name, add_underscore = FALSE, remove_charge = FALSE)
    
    if (!is.null(to_remove)) {
      name = name[!name %in% format_metabolite_name(to_remove, add_underscore = FALSE, remove_charge = FALSE)]
    }      
    
    name = unique(name)
    name = sort(name)
    
    return(name)
  }
  
  #' Get Choices for Metabolites for a Selected Reaction
  #'
  #' This function retrieves all metabolite names (e.g., substrates, products, or intermediates)
  #' involved in a selected reaction by parsing its reaction equations from the reference network.
  #'
  #' @param network_from_database Logical. Use database source?
  #' @param network_from_builder Logical. Use in-session network builder?
  #' @param network_from_upload Logical. Use uploaded file?
  #' @param selected_network A character string indicating the selected reference network.
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param upload_path File path to the uploaded network file (if using upload source).
  #' @return A character vector of metabolite names involved in the reaction.
  #' @export
  get_metabolite_choices <- function(network_from_database = FALSE,
                                     network_from_builder = FALSE,
                                     network_from_upload = FALSE,
                                     selected_network = NULL,
                                     session = shiny::getDefaultReactiveDomain(),
                                     upload_path = NULL) {
    reference_network <- get_reference_network(
      network_from_database = network_from_database,
      network_from_builder = network_from_builder,
      network_from_upload = network_from_upload,
      selected_network = selected_network,
      upload_path = upload_path,
    )
    
    metabolites <- get_metabolite_names(reference_network$eq)
    
    return(metabolites)
  }
  
  #' Get Selections for Metabolites from the Configuration File
  #'
  #' This function gets the default choices for metabolites from the configuration file.
  #' Types of metabolites include substrates, products, and unbalanced intermediates.  
  #' The function loads the configuration file, and retrieves information for a given 
  #' reference reaction (row) and metabolite type (column). 
  #'
  #' @param selected_network A character string indicating the selected reference network.
  #' @param metabolite_col A string giving the column name in the configuration that holds the metabolites.
  #'
  #' @return A character vector of default metabolites (may be empty if not found).
  #' @export
  get_metabolite_selections <- function(selected_network, metabolite_col){
    network_config <- load_data("config_reference_networks")
    
    # Safely get the cell content
    value <- network_config |>
      dplyr::filter(.data$reference_network == selected_network) |>
      dplyr::pull(dplyr::all_of(metabolite_col))
    
    if (length(value) == 0 || is.na(value)) {
      return(character(0))
    }
    
    # Split the semicolon-separated string
    default <- strsplit(value, ";")[[1]]
    
    return(default)
  }
  
  #' Get Selections for Organisms from Configuration File
  #'
  #' This function retrieves a default selection of organisms corresponding to a selected 
  #' network name. It uses a configuration file to identify the appropriate 
  #' file and then loads its contents.
  #'
  #' @param selected_network A character string indicating the selected reference network.
  #' @return A character vector of default metabolites (may be empty if not found).
  #' @export
  get_default_organism_selections <- function(selected_network) {
    network_config <- load_data("config_reference_networks")
    
    value <- network_config |>
      dplyr::filter(.data$reference_network == selected_network) |>
      dplyr::pull(selected_organisms)
    
    if (length(value) == 0 || is.na(value)) {
      return(character(0))
    }
    
    selections <- strsplit(value, ";")[[1]]
    
    return(selections)
  }
  
  #' Get Choices for Reference Network in the Database
  #'
  #' This function retrieves the available reference reaction names from the 
  #' configuration file for reference network, excluding the "Other" category.
  #'
  #' @return A character vector of reference reaction names.
  #' @examples
  #' choices <- get_choices_reference_network_in_database()
  #'
  #' @export
  get_choices_reference_network_in_database <- function() {
    network_config <- load_data("config_reference_networks")
    choices <- setdiff(unique(network_config$reference_network), "Other")
    return(choices)
  }
  
  #' Create Labeled Choices from Multiple Columns
  #'
  #' Generates a named character vector for use in select inputs,
  #' where each value is labeled with its variable (e.g., "Glycolysis (module)").
  #' Preserves order within each column and omits NAs.
  #'
  #' @param data A data frame containing the source columns.
  #' @param vars Named character vector of the form `c("column_name" = "label")`.
  #' @param all_option A character string to use as the "select all" option. If `NULL`, it's omitted.
  #'
  #' @return A named character vector with labels for display and original values as choices.
  #' @examples
  #' vars_to_label <- c("md" = "module", "rn" = "reaction", "ko" = "KO", "eq" = "equation")
  #' create_labeled_choices(main_network, vars_to_label, all_option = "All reactions")
  #'
  #' @export
  create_labeled_choices <- function(data, vars, all_option = "All reactions") {
    choices <- purrr::map2(
      names(vars), vars,
      function(var, label) {
        vals <- data[[var]]
        vals <- vals[!is.na(vals)]
        vals <- vals[!duplicated(vals)]
        stats::setNames(vals, paste0(vals, " (", label, ")"))
      }
    ) |>
      unlist(use.names = TRUE)
    
    if (!is.null(all_option)) {
      choices <- c(setNames(all_option, all_option), choices)
    }
    
    return(choices)
  }
  
  #' Update Choices for Reference Networks from the Database (Network Module)
  #'
  #' Populates the \code{reference_network} selectize input with
  #' the reference networks available in the database, via
  #' \code{get_choices_reference_network_in_database()}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{reference_network} selectize input.
  #' @export
  update_reference_network_network <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices <- get_choices_reference_network_in_database()
    
    # Update UI
    update_select_input(session = session, inputId = "reference_network",
                        choices = choices)
  }
  
  #' Update Choices for Gene Functions from the Database (Network Module)
  #'
  #' Populates the \code{selected_organisms} selectize input with
  #' organism names from the loaded database. The selection logic depends
  #' on which optional argument is supplied:
  #' \itemize{
  #'   \item \code{upload_path} (uploaded gene-functions file): selections
  #'     are derived from the upload via
  #'     \code{get_uploaded_organism_selections}, falling back to
  #'     \emph{Escherichia coli} if invalid.
  #'   \item \code{reference_network} (network change): selections
  #'     are derived from the reference network via
  #'     \code{get_default_organism_selections}, falling back to
  #'     \emph{Escherichia coli} if invalid.
  #'   \item neither (the at-load case): selection defaults to
  #'     \emph{Escherichia coli}.
  #' }
  #' \code{upload_path} takes precedence over \code{reference_network}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param reference_network Optional reference network name (typically `input$reference_network`). Falls back to `"Fermentation of glucose"` if missing.
  #' @param upload_path Optional path to an uploaded gene-functions file (typically `input$selected_organisms_upload$datapath`).
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{selected_organisms} selectize input.
  #' @export
  update_selected_organisms_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                     reference_network,
                                                     upload_path = NULL) {
    # Detect whether caller supplied reference_network. The
    # at-load case omits it entirely (selected = "Escherichia coli"); the
    # change-event case supplies it (selected derived from the network).
    # Using missing() rather than a NULL-check preserves the original
    # behavior when input$reference_network is itself NULL.
    has_reference_network <- !missing(reference_network)
    if (missing(reference_network)) reference_network <- NULL
    
    # Load data
    database <- load_database()
    
    # Get inputs
    reference_network <- assign_if_invalid(
      reference_network,
      "Fermentation of glucose"
    )
    
    # Get choices
    choices <- get_organism_choices(database = database)
    
    if (!is.null(upload_path)) {
      selected <- get_uploaded_organism_selections(upload_path, choices)
      selected <- assign_if_invalid(selected, c("Escherichia coli"))
    } else if (has_reference_network) {
      selected <- get_default_organism_selections(reference_network)
      selected <- assign_if_invalid(selected, c("Escherichia coli"))
    } else {
      selected <- "Escherichia coli"
    }
    
    # Update UI
    update_select_input(session = session, inputId = "selected_organisms",
                        choices = choices, selected = selected)
  }
  
  #' Update Choices for Substrates (Network Module)
  #'
  #' Populates the \code{substrates} selectize input with metabolites
  #' available in the currently selected reference network (whether from
  #' the database, the in-session builder, or an uploaded file). Defaults
  #' the selection to the network's \code{default_substrates}, falling
  #' back to the first available choice.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param reference_network The reference network name (typically `input$reference_network`). Falls back to `"Fermentation of glucose"` if missing.
  #' @param reaction_tabs Which reaction-source tab is active (typically
  #'   \code{input$reaction_tabs}; one of \code{"Database"}, \code{"Build"},
  #'   or \code{"File upload"}). Falls back to \code{"Database"} if
  #'   invalid.
  #' @param upload_path Path to an uploaded reference network file (typically `input$reference_network_upload$datapath`).
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{substrates} selectize input.
  #' @export
  update_substrates_network <- function(session = shiny::getDefaultReactiveDomain(),
                                        reference_network = NULL,
                                        reaction_tabs = NULL,
                                        upload_path = NULL) {
    # Get inputs
    reference_network <- assign_if_invalid(
      reference_network,
      "Fermentation of glucose"
    )
    reaction_tabs <- assign_if_invalid(reaction_tabs, "Database")
    selected_network <- if (reaction_tabs == "Database") reference_network else "Other"
    selected_network <- assign_if_invalid(selected_network, "Other")
    
    # Get choices
    choices <- get_metabolite_choices(
      network_from_database = isTRUE(reaction_tabs == "Database"),
      network_from_builder = isTRUE(reaction_tabs == "Build"),
      network_from_upload = isTRUE(reaction_tabs == "File upload"),
      selected_network = selected_network,
      upload_path = upload_path
    ) 
    
    selected <- get_metabolite_selections(selected_network = selected_network, 
                                          metabolite_col = "default_substrates")
    selected <- assign_if_invalid(selected, choices[1])
    
    # Update UI
    update_select_input(session = session, inputId = "substrates",
                        choices = choices, selected = selected)
  }
  
  #' Update Choices for Products (Network Module)
  #'
  #' Populates the \code{products} selectize input with metabolites
  #' available in the currently selected reference network. Defaults the
  #' selection to the network's \code{default_products}, falling back to
  #' the first available choice.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param reference_network The reference network name (typically `input$reference_network`). Falls back to `"Fermentation of glucose"` if missing.
  #' @param reaction_tabs Which reaction-source tab is active (typically
  #'   \code{input$reaction_tabs}). Falls back to \code{"Database"} if
  #'   invalid.
  #' @param upload_path Path to an uploaded reference network file (typically `input$reference_network_upload$datapath`).
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{products} selectize input.
  #' @export
  update_products_network <- function(session = shiny::getDefaultReactiveDomain(),
                                      reference_network = NULL,
                                      reaction_tabs = NULL,
                                      upload_path = NULL) {
    # Get inputs
    reference_network <- assign_if_invalid(
      reference_network,
      "Fermentation of glucose"
    )
    reaction_tabs <- assign_if_invalid(reaction_tabs, "Database")
    selected_network <- if (reaction_tabs == "Database") reference_network else "Other"
    selected_network <- assign_if_invalid(selected_network, "Other")
    
    # Get choices
    choices <- get_metabolite_choices(
      network_from_database = isTRUE(reaction_tabs == "Database"),
      network_from_builder = isTRUE(reaction_tabs == "Build"),
      network_from_upload = isTRUE(reaction_tabs == "File upload"),
      selected_network = selected_network,
      upload_path = upload_path
    ) 
    
    selected <- get_metabolite_selections(selected_network = selected_network, 
                                          metabolite_col = "default_products")
    selected <- assign_if_invalid(selected, choices[1])
    
    # Update UI
    update_select_input(session = session, inputId = "products",
                        choices = choices, selected = selected)
  }
  
  #' Update Choices for Unbalanced Intermediates (Network Module)
  #'
  #' Populates the \code{unbalanced_intermediates} selectize input with
  #' metabolites available in the currently selected reference network.
  #' Defaults the selection to the network's
  #' \code{default_unbalanced_intermediates}, restricted to those present
  #' in the choice list (no first-choice fallback).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param reference_network The reference network name (typically `input$reference_network`). Falls back to `"Fermentation of glucose"` if missing.
  #' @param reaction_tabs Which reaction-source tab is active (typically
  #'   \code{input$reaction_tabs}). Falls back to \code{"Database"} if
  #'   invalid.
  #' @param upload_path Path to an uploaded reference network file (typically `input$reference_network_upload$datapath`).
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{unbalanced_intermediates} selectize input.
  #' @export
  update_unbalanced_intermediates_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                      reference_network = NULL,
                                                      reaction_tabs = NULL,
                                                      upload_path = NULL) {
    # Get inputs
    reference_network <- assign_if_invalid(
      reference_network,
      "Fermentation of glucose"
    )
    reaction_tabs <- assign_if_invalid(reaction_tabs, "Database")
    selected_network <- if (reaction_tabs == "Database") reference_network else "Other"
    selected_network <- assign_if_invalid(selected_network, "Other")
    
    # Get choices
    choices <- get_metabolite_choices(
      network_from_database = isTRUE(reaction_tabs == "Database"),
      network_from_builder = isTRUE(reaction_tabs == "Build"),
      network_from_upload = isTRUE(reaction_tabs == "File upload"),
      selected_network = selected_network,
      upload_path = upload_path
    ) 
    
    selected <- get_metabolite_selections(selected_network = selected_network, 
                                          metabolite_col = "default_unbalanced_intermediates")
    selected <- selected[selected %in% choices]
    
    # Update UI
    update_select_input(session = session, inputId = "unbalanced_intermediates",
                        choices = choices, selected = selected)  
  }
  
  #' Update Choices for Substrates to Display (Network Module)
  #'
  #' Populates the \code{substrate_to_display} picker input with the
  #' input substrates from the prediction results. Substrates whose
  #' maximum flux falls below \code{threshold} are flagged as
  #' "substrate not predicted" via \code{format_picker_choices}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param data The predicted flux data used to flag unpredicted substrates.
  #' @param substrates The substrate choices to display.
  #' @param threshold Numeric flux threshold below which a substrate is considered unpredicted.
  #' @param inputId The picker input to update.
  #' @param allow_any Whether to offer an "Any" choice, which stands for every substrate at once.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the picker input.
  #' @export
  update_substrate_to_display_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                  data,
                                                  substrates,
                                                  threshold,
                                                  inputId = "substrate_to_display",
                                                  allow_any = FALSE) {
    # Check required conditions
    req(!is.null(data), !is.null(substrates))
    
    # Get choices
    unpredicted <- get_unpredicted_choices(data, choices_col = "Substrate", 
                                           value_col = "Flux", threshold = threshold)
    fmt <- format_picker_choices(substrates, unpredicted, label = "substrate not predicted")
    
    # Offer every substrate at once.  The first choice is the one selected, so
    # putting "Any" first is what makes it the one shown to begin with.
    if (allow_any) {
      fmt$choices <- c(Any = "Any", fmt$choices)
      fmt$choicesOpt$content <- c("Any", fmt$choicesOpt$content)
    }
    
    # Update UI
    update_picker_input(session = session, inputId = inputId,
                        choices = fmt$choices, choicesOpt = fmt$choicesOpt)
  }
  
  #' Update Choices for Products to Display (Network Module)
  #'
  #' Populates the \code{product_to_display} picker input with the input
  #' products from the prediction results.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param data The product choices to display.
  #' @param inputId The picker input to update.
  #' @param allow_any Whether to offer an "Any" choice, which stands for every end product at once.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the picker input.
  #' @export
  update_product_to_display_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                data,
                                                inputId = "product_to_display",
                                                allow_any = FALSE) {
    # Check required conditions
    req(!is.null(data))
    
    # Offer every end product at once.  The first choice is the one selected, so
    # putting "Any" first is what makes it the one shown to begin with.
    if (allow_any) {
      data <- c("Any", data)
    }
    
    # Update UI
    update_picker_input(session = session, inputId = inputId, choices = data)
  }
  
  #' Update Choices for Organisms to Display (Network Module)
  #'
  #' Populates the \code{organism_to_display} picker input with the
  #' organism names from the prediction results.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param data The organism choices to display.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{organism_to_display} picker input.
  #' @export
  update_organism_to_display_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                 data) {
    # Check required conditions
    req(!is.null(data))
    
    # Update UI
    update_picker_input(session = session, inputId = "organism_to_display",
                        choices = data)
  }
  
  #' Update Choices for Tree Layout (Network Module)
  #'
  #' Populates the \code{tree_layout} picker input with the layouts the tree can
  #' be drawn in.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{tree_layout} picker input.
  #' @export
  update_tree_layout_network <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices <- c("Rectangular", "Circular", "Ape")
    
    # Update UI
    update_picker_input(session = session, inputId = "tree_layout",
                        choices = choices)
  }
  
  #' Update Choices for Network Layout (Network Module)
  #'
  #' Populates the \code{network_layout} picker input with the layouts
  #' supported for the current network dimensionality (2-D vs 3-D).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param network_dimensions Network dimensionality as a string
  #'   (typically \code{input$network_dimensions}; either \code{"2"}
  #'   or \code{"3"}).
  #'
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{network_layout} picker input.
  #' @export
  update_network_layout_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                network_dimensions = NULL) {
    # Get choices
    choices <- switch(network_dimensions,
                      "3" = c("FR", "KK", "DRL", "MDS"),
                      "2" = c("FR", "KK", "DH", "GEM", "DRL", "MDS", "Graphopt"),
                      NULL)
    
    # Update UI
    update_picker_input(session = session, inputId = "network_layout",
                        choices = choices)
  }
  
  #' Update Choices for Network Name to Display (Network Module)
  #'
  #' Updates the read-only \code{<network_input_id>-network_name_display}
  #' text input to reflect the currently active reference network. When
  #' the network has zero rows the display reads "Custom network";
  #' otherwise it shows a timestamped CSV file name and stores that name
  #' in the supplied \code{network_filename} reactive value.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param reference_network A data frame containing the active reference network (typically `session$userData$network_data()`).
  #' @param network_filename A \code{reactiveVal} for storing the
  #'   generated file name. The helper writes to it when a non-empty
  #'   network is supplied.
  #' @param network_input_id The id used for the nested
  #'   \code{create_network_input} module. Defaults to
  #'   \code{"network_input"} to match the typical caller. Override only
  #'   if the caller passed a different id to \code{create_network_input}.
  #'
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the text input and (when applicable) the \code{network_filename}
  #'   reactive value.
  #' @export
  update_network_name_display_network <- function(session = shiny::getDefaultReactiveDomain(),
                                                  reference_network,
                                                  network_filename,
                                                  network_input_id = "network_input") {
    # Get value
    file_name <- paste0("network_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    network_filename(file_name)
    display <- if (nrow(reference_network) == 0) "Custom network" else file_name
    
    # Update UI
    update_text_input(session = session,
                      inputId = paste0(network_input_id, "-network_name_display"),
                      value = display)
  }
  
# === Generating outputs ===
  #' Remove Rows Where the End Product Is the Substrate
  #'
  #' A network is never solved when the substrate and the end product are the same
  #' compound, so the flux of that pair is always missing.  This function removes
  #' those rows from a table of fluxes, which keeps an empty column out of the
  #' heatmap.  When a substrate is given, only the pairs of that substrate are
  #' removed.
  #'
  #' @param df A data frame of fluxes, with one row per organism, substrate and end product.
  #' @param substrate The substrate on display, or NULL to remove the pairs of every substrate.
  #' @param substrate_col Name of the column holding the substrate.
  #' @param product_col Name of the column holding the end product.
  #' @return The data frame without the rows where the end product is the substrate.
  #' @export
  drop_self_pairs <- function(df, substrate = NULL,
                              substrate_col = "Substrate", product_col = "End product") {
    # Leave the data alone when the columns are not there
    if (is.null(df) || !all(c(substrate_col, product_col) %in% colnames(df))) {
      return(df)
    }
    
    # Compare names the same way the solver does
    substrate_name <- format_metabolite_name(as.character(df[[substrate_col]]))
    product_name <- format_metabolite_name(as.character(df[[product_col]]))
    
    # Mark the rows where the two names are the same
    is_self_pair <- substrate_name == product_name
    is_self_pair[is.na(is_self_pair)] <- FALSE
    
    # Limit the removal to one substrate when one is given
    if (!is.null(substrate) && length(substrate) == 1 && !is.na(substrate) && nzchar(substrate)) {
      is_self_pair <- is_self_pair & substrate_name == format_metabolite_name(as.character(substrate))
    }
    
    return(df[!is_self_pair, , drop = FALSE])
  }
  
  #' Get Configuration for Network Plots
  #'
  #' Filters the plot config for a given value of `network_dimensions` (e.g. 2 or 3)
  #' and returns a named list of configuration values.
  #'
  #' @param network_dimensions Integer indicating 2D or 3D layout.
  #' @return A named list with settings like `vertex_default_size`, `line_color`, etc.
  #' @export
  get_network_plot_config <- function(network_dimensions) {
    config <- load_data("config_network_layout")
    
    # Filter for selected dimensions
    config_filtered <- config[config$network_dimensions == network_dimensions, ]
    
    # Convert to named list (parsing `;` delimited values)
    config_list <- lapply(config_filtered$value, function(x) {
      if (grepl(";", x)) strsplit(x, ";")[[1]] else as.numeric(x)
    })
    
    names(config_list) <- config_filtered$var
    return(config_list)
  }
  
  #' Generate Legend Key for Network Plot
  #'
  #' Constructs a legend key for the network plot based on the provided network dimensions
  #' (e.g., 2 for 2D or 3 for 3D). This retrieves configuration values from the plot config file.
  #'
  #' @param network_dimensions Integer specifying the network dimensionality (2 or 3).
  #'
  #' @return A data frame with legend information:
  #'   \describe{
  #'     \item{name}{Character vector of flux level labels.}
  #'     \item{line_color}{Character vector of hex color codes.}
  #'     \item{line_width}{Numeric vector of line widths.}
  #'   }
  #' @export
  #' @seealso \code{\link{get_network_plot_config}}, \code{\link{load_data}}
  get_network_legend_key <- function(network_dimensions) {
    config <- get_network_plot_config(network_dimensions)
    
    data.frame(
      name = config$name,
      line_color = config$line_color,
      line_width = as.numeric(config$line_width),
      stringsAsFactors = FALSE
    )
  }
  
  #' Construct Graph of Metabolic Network
  #' 
  #' This function constructs a graph (igraph object) from a metabolic network model
  #' The metabolic network model is usually solved before being passed to this function
  #' 
  #' @param reaction_table A data frame describing a solved metabolic network model, with one row per reaction
  #'   and a \code{flux} column.
  #' @param add_flux A logical indicating whether to add fluxes to the graph.
  #' @param to_remove A character vector of metabolites not to be displayed in the graph.
  #' @param keep_missing_names A logical indicating whether to keep reactions where \code{ec} is \code{NA}.
  #' @param add_underscore A logical indicating whether to add underscores to the names.
  #' @return An igraph object of the metabolic network
  #' @export
  #' @importFrom igraph graph_from_data_frame
  #' @importFrom tidyr separate separate_rows
  #' @importFrom dplyr distinct select
  make_network_graph <- function(reaction_table, add_flux=TRUE, 
                                 to_remove = c("NAD+", "NADH", "ATP", "ADP", "Orthophosphate", "H2O", "H+", "CO2"), 
                                 keep_missing_names = TRUE,
                                 add_underscore = FALSE) 
  {
    # Get data
    df <- reaction_table
    
    # Get names of reactants and products
    df <- df |>
      dplyr::mutate(eq_original = eq) |> 
      tidyr::separate(col = eq, into = c("reactant", "product"), sep = "<=>") |>
      dplyr::filter(reactant != "", product != "") |>
      tidyr::separate_rows(reactant, sep = " \\+ ") |>
      tidyr::separate_rows(product,  sep = " \\+ ") |>
      dplyr::relocate(eq_original, .after = name) |>
      dplyr::rename(eq = eq_original)
    
    # Format names of reactants and products
    df$reactant <- format_metabolite_name(name = df$reactant, add_underscore = add_underscore)
    df$product <- format_metabolite_name(name = df$product, add_underscore = add_underscore)
    df <- dplyr::distinct(df)
    
    # Remove any names (usually unbalanced metabolites) not to be displayed in graph
    to_remove = format_metabolite_name(to_remove)
    df = df[!(df$reactant %in% to_remove), ]
    df = df[!(df$product %in% to_remove), ]
    
    # Remove enzymes with no ec
    if(!keep_missing_names)
    {
      df = df[!is.na(df$ec),] 
    }
    
    if(add_flux){
      # Add fluxes
      match <- match(x = paste0(df$name), table = paste0(reaction_table$name))
      df$flux <- reaction_table$flux[match]
      df <- df |> dplyr::select(dplyr::any_of(c("reactant", "product", "ec", "eq", "ko", "rn", "md", "flux", "lowbnd", "uppbnd")))
    }else{
      df <- df |> dplyr::select(dplyr::any_of(c("reactant", "product", "ec", "eq", "ko", "rn", "md")))
    }
    
    #Get graph
    g = igraph::graph_from_data_frame(d = df, directed = FALSE)
    
    return(g)
  }
  
  #' Set Layout for Graph of Metabolic Network
  #' 
  #' This function sets the layout for the graph
  #' 
  #' @param graph An igraph object of the metabolic network.
  #' @param type A character vector of the layout type.
  #' @param dimensions A numeric vector of the number of dimensions.
  #' @return A numeric vector of the layout
  #' @export
  #' @importFrom igraph layout_with_fr layout_with_kk layout_with_lgl layout_with_dh layout_with_gem layout_with_drl layout_with_mds layout_with_graphopt layout_with_sugiyama layout_randomly layout_in_circle layout_on_sphere layout_as_star layout_as_tree
  set_network_layout <- function(graph, type = "FR", dimensions = 2) {
    # Get graph
    g <- graph
    
    # Define a list of layout functions
    layout_functions <- list(
      FR = igraph::layout_with_fr,
      KK = igraph::layout_with_kk,
      LGL = igraph::layout_with_lgl,
      DH = igraph::layout_with_dh,
      GEM = igraph::layout_with_gem,
      DRL = igraph::layout_with_drl,
      MDS = igraph::layout_with_mds,
      Graphopt = igraph::layout_with_graphopt,
      Sugiyama = igraph::layout_with_sugiyama,
      Random = igraph::layout_randomly,
      Circle = igraph::layout_in_circle,
      Sphere = igraph::layout_on_sphere,
      Star = igraph::layout_as_star,
      Tree = igraph::layout_as_tree
    )
    
    # Check if the specified type is available
    if (!type %in% names(layout_functions)) {
      stop("Invalid layout type. Please choose from: ", paste(names(layout_functions), collapse = ", "))
    }
    
    # Get the appropriate layout function
    layout_function <- layout_functions[[type]]
    
    # Set layout with dimensions if applicable
    if (type %in% c("FR", "KK", "DRL", "MDS")) {
      layout <- layout_function(g, dim = dimensions)
    } else {
      layout <- layout_function(g)
    }
    
    return(layout)
  }
  
  #' Format Network Graph
  #' 
  #' Format appearance of the graph of the metabolic network for the Shiny app
  #'
  #' @param graph An igraph object of the metabolic network.
  #' @param show_flux A logical indicating whether to show fluxes.
  #' @param show_modules A logical indicating whether to show modules.
  #' @param vertex_default_color A character vector of the default color for vertices.
  #' @param vertex_highlight_color A character vector of the color for highlighted vertices.
  #' @param vertex_missing_reaction_color A character vector of the color for missing reaction vertices.
  #' @param vertex_default_frame.color A character vector of the default frame color for vertices.
  #' @param vertex_missing_reaction_frame.color A character vector of the frame color for missing reaction vertices.
  #' @param vertex_highlight_frame.color A character vector of the frame color for highlighted vertices.
  #' @param vertex_default_frame.width A numeric vector of the default frame width for vertices.
  #' @param vertex_missing_reaction_frame.width A numeric vector of the frame width for missing reaction vertices.
  #' @param vertex_highlight_frame.width A numeric vector of the frame width for highlighted vertices.
  #' @param vertex_default_opacity A numeric vector of the default opacity for vertices.
  #' @param vertex_missing_reaction_opacity A numeric vector of the opacity for missing reaction vertices.
  #' @param vertex_default_size A numeric vector of the default size for vertices.
  #' @param vertex_highlight_size A numeric vector of the size for highlighted vertices.
  #' @param vertex_highlight_frame.size A numeric vector of the frame size for highlighted vertices.
  #' @param vertex_color_lighten A numeric vector of the amount to lighten the vertex color.
  #' @param vertex_label A character vector of the vertex label.
  #' @param edge_default_color A character vector of the default color for edges.
  #' @param edge_missing_reaction_color A character vector of the color for missing reaction edges.
  #' @param edge_zero_flux_color A character vector of the color for edges with zero flux.
  #' @param edge_positive_flux_color A character vector of the color for edges with positive flux.
  #' @param edge_default_opacity A numeric vector of the default opacity for edges.
  #' @param edge_missing_reaction_opacity A numeric vector of the opacity for missing reaction edges.
  #' @param edge_zero_flux_opacity A numeric vector of the opacity for edges with zero flux.
  #' @param edge_positive_flux_opacity A numeric vector of the opacity for edges with positive flux.
  #' @param edge_default_width A numeric vector of the default width for edges.
  #' @param edge_missing_reaction_width A numeric vector of the width for missing reaction edges.
  #' @param edge_low_flux_width A numeric vector of the width for edges with low flux.
  #' @param edge_medium_flux_width A numeric vector of the width for edges with medium flux.
  #' @param edge_high_flux_width A numeric vector of the width for edges with high flux.
  #' @param vertices_to_highlight A character vector of vertices to highlight.
  #' @return An igraph object of the metabolic network with formatted appearance
  #' @export
  #' @importFrom colorspace lighten
  #' @importFrom dplyr bind_rows filter full_join group_by pull rename select summarize ungroup
  #' @importFrom igraph V as_data_frame
  format_network_graph <- function(graph, show_flux = FALSE, show_modules = FALSE,
                                   vertex_default_color = "#7f7f7f",
                                   vertex_highlight_color = "#ff0000",
                                   vertex_missing_reaction_color = "#ffffff",
                                   vertex_default_frame.color = "#7f7f7f",
                                   vertex_missing_reaction_frame.color = "#ffffff",
                                   vertex_highlight_frame.color = "#ff0000",
                                   vertex_default_frame.width = 2,
                                   vertex_missing_reaction_frame.width = 1,
                                   vertex_highlight_frame.width = 3,
                                   vertex_default_opacity = 1,
                                   vertex_missing_reaction_opacity = 0.1,
                                   vertex_default_size = 10,
                                   vertex_highlight_size = 15,
                                   vertex_highlight_frame.size = 5,
                                   vertex_color_lighten = 0.2,
                                   vertex_label = NA,
                                   edge_default_color = "#7f7f7f",
                                   edge_missing_reaction_color = "#7f7f7f",
                                   edge_zero_flux_color = "#7f7f7f",
                                   edge_positive_flux_color = "#00B050",
                                   edge_default_opacity = 1,
                                   edge_missing_reaction_opacity = 0.1,
                                   edge_zero_flux_opacity = 1,
                                   edge_positive_flux_opacity = 1,
                                   edge_default_width = 0.5,
                                   edge_missing_reaction_width = 0.5,
                                   edge_low_flux_width = 0.5,
                                   edge_medium_flux_width = 1,
                                   edge_high_flux_width = 2,
                                   vertices_to_highlight = NULL) {
    
    # Convert graph to data frame
    df <- igraph::as_data_frame(graph, what = "edges")
    
    # Initialize vertices data frame
    vertices <- igraph::V(graph)$name
    vertices <- data.frame(name = vertices, color = vertex_default_color, size = vertex_default_size)
    
    # Initialize fluxes data frame
    if (show_flux) {
      fluxes <- df |> dplyr::select(lowbnd, uppbnd, flux)
    }
    
    # Set default vertex attributes
    vertices$color <- vertex_default_color
    vertices$size <- vertex_default_size
    vertices$frame.color <- vertex_default_frame.color
    vertices$frame.width <- vertex_default_frame.width
    vertices$opacity <- vertex_default_opacity
    vertices$label <- vertex_label
    
    # Modify attributes based on flux
    if (show_flux) {
      active_vertices <- df |>
        dplyr::select(from, uppbnd, lowbnd) |>
        dplyr::rename(name = from) |>
        dplyr::bind_rows(
          df |>
            dplyr::select(to, uppbnd, lowbnd) |>
            dplyr::rename(name = to)
        ) |>
        dplyr::filter(!(uppbnd == 0 & lowbnd == 0)) |>
        dplyr::pull(name) |>
        unique()
      
      vertices$color <- ifelse(vertices$name %in% active_vertices, vertices$color, vertex_missing_reaction_color)
      vertices$frame.color <- ifelse(vertices$name %in% active_vertices, vertices$frame.color, vertex_missing_reaction_frame.color)
      vertices$opacity <- ifelse(vertices$name %in% active_vertices, vertices$opacity, vertex_missing_reaction_opacity)
    }else{
      active_vertices = NULL
    }
    
    # Modify attributes based on mds
    if (show_modules) {
      most_common_md <- df |>
        dplyr::select(from, md) |>
        dplyr::rename(name = from) |>
        dplyr::bind_rows(
          df |>
            dplyr::select(to, md) |>
            dplyr::rename(name = to)
        ) |>
        dplyr::group_by(name, md) |>
        dplyr::summarise(count = dplyr::n(), .groups = 'drop') |>
        dplyr::group_by(name) |>
        dplyr::slice_max(order_by = count, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(name, md)
      
      vertices <- vertices |>
        dplyr::full_join(most_common_md, by = "name")
      
      n <- length(unique(vertices$md))
      color_palette <- colorspace::qualitative_hcl(n, h = c(15, 375 * (n - 1) / n), c = 100, l = 65, fixup = TRUE, alpha = 1)
      
      vertices$md <- as.factor(vertices$md)
      levels(vertices$md) <- color_palette
      vertices$md <- as.character(vertices$md)
      vertices$frame.color <- vertices$md
      
      vertices$color <- vertices$md
      vertices$color = colorspace::lighten(vertices$color, amount = vertex_color_lighten)
      
      vertices <- vertices |> dplyr::select(-md)
    }
    
    # Highlight specific vertices
    if (!is.null(vertices_to_highlight)) {
      vertices$size <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_size, vertices$size)
      vertices$frame.color <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_frame.color, vertices$frame.color)
      vertices$frame.width <- ifelse(vertices$name %in% vertices_to_highlight, vertex_highlight_frame.width, vertices$frame.width)
      vertices$label <- ifelse(vertices$name %in% vertices_to_highlight, vertices$name, vertices$label)
    }
    
    # Initialize edges data frame
    edges <- df |> dplyr::select(to, from)
    edges$color <- edge_default_color
    edges$width <- edge_default_width
    edges$opacity <- edge_default_opacity
    
    # Modify edge attributes based on flux
    if (show_flux) {
      edges$color <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_color, 
                            ifelse(abs(fluxes$flux) > 1, edge_positive_flux_color, edge_zero_flux_color))
      
      edges$opacity <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_opacity, 
                              ifelse(abs(fluxes$flux) > 1, edge_positive_flux_opacity, edge_zero_flux_opacity))
      
      edges$width <- abs(fluxes$flux) / 10 + 1
      edges$width <- ifelse(fluxes$uppbnd == 0 & fluxes$lowbnd == 0, edge_missing_reaction_width, edges$width)
      
      edges$width <- ifelse(edges$width <= 2, edge_low_flux_width, 
                            ifelse(edges$width < 20, edge_medium_flux_width, edge_high_flux_width))  
    }
    
    # Add attributes back to the graph
    igraph::V(graph)$color <- vertices$color
    igraph::V(graph)$size <- vertices$size
    igraph::V(graph)$frame.color <- vertices$frame.color
    igraph::V(graph)$frame.width <- vertices$frame.width
    igraph::V(graph)$opacity <- vertices$opacity
    igraph::V(graph)$label <- vertices$label
    igraph::E(graph)$color <- edges$color
    igraph::E(graph)$width <- edges$width
    igraph::E(graph)$opacity <- edges$opacity
    
    return(graph)
  }
  
# === Generating content for modal ===
  #' Filter Metabolic Network by Value Match
  #'
  #' This function filters a reference network based on whether any of 
  #' the specified values appear in selected columns (`md`, `ko`, `rn`, or `eq`). 
  #' It returns the subset of rows where any of the values are present in at least 
  #' one of those columns. This allows users to select reactions from a large
  #' network.  
  #'
  #' @param data A data frame or tibble containing at least the columns `nt`, `md`, `ko`, `rn`, and `eq`. Typically the main network database.
  #' @param values A character vector of values to match against the `md`, `ko`, 
  #'   `rn`, or `eq` columns. If `NULL`, the original `data` is returned unmodified.
  #' @param all_option A character string used to indicate "no filtering" (default = "All").
  #'
  #' @return A data frame containing only rows where at least one of the given 
  #'   `values` appears in any of the target columns.
  #'
  #' @examples
  #' \dontrun{
  #'   filter_network(data, values = c("K00844", "R00232"))
  #'   filter_network(data, values = "All") # returns all rows
  #' }
  #'
  #' @export
  filter_network <- function(data, values = NULL, all_option = "All reactions") {
    if (is.null(values) || (length(values) == 1 && values == all_option)) {
      return(data)
    }
    
    data <- data |> 
      dplyr::filter(
        nt %in% values |
          md %in% values |
          ko %in% values |
          rn %in% values |
          eq %in% values
      ) |>
      dplyr::distinct(md, ko, rn, eq, .keep_all = TRUE)
    
    return(data)
  }

  #' Determine Metabolites to Remove from the Network
  #'
  #' This function returns a character vector of metabolites to exclude from the graph,
  #' based on user input flags and exceptions.
  #'
  #' @param hide_unbalanced_intermediates Logical. Whether to remove unbalanced intermediates.
  #' @param hide_cofactors Logical. Whether to remove cofactors.
  #' @param unbalanced_intermediates Character vector of unbalanced intermediates.
  #' @param enzyme_cofactors Character vector of cofactors.
  #' @param exceptions Character vector of metabolites to always keep (e.g., product, substrate).
  #'
  #' @return Character vector of metabolite names to remove.
  get_metabolites_to_remove <- function(hide_unbalanced_intermediates = FALSE,
                                        hide_cofactors = FALSE,
                                        unbalanced_intermediates = character(),
                                        enzyme_cofactors = character(),
                                        exceptions = character()) {
    to_remove <- character()
    
    if (isTRUE(hide_unbalanced_intermediates)) {
      to_remove <- union(to_remove, unbalanced_intermediates)
    }
    if (isTRUE(hide_cofactors)) {
      to_remove <- union(to_remove, enzyme_cofactors)
    }
    
    setdiff(to_remove, exceptions)
  }  
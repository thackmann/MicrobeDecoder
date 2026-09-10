# Define Functions for Search Database Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 22 May 2026

# === General ===
  #' Rename and Overwrite Column Names
  #'
  #' This function renames columns in a dataframe by replacing a specified pattern with a replacement string.
  #' If the renaming results in duplicate column names, only the last occurrence of each name is retained.
  #'
  #' @param data A data frame whose column names need to be renamed.
  #' @param pattern A character string containing a regular expression to match in column names.
  #' @param replacement A character string that will replace the matched pattern in column names.
  #'
  #' @return The input data frame with renamed column names. If duplicates arise after renaming, earlier occurrences are removed, keeping only the last occurrence of each duplicated column name.
  #' @export
  #'
  #' @examples
  rename_and_overwrite <- function(data, pattern, replacement) {
    colnames(data) <- colnames(data) |>
      stringr::str_replace(pattern, replacement)
    
    data <- data[, !duplicated(names(data), fromLast = TRUE)]
    
    data
  }

# === Getting inputs ===
  #' Get Inputs for Search Module
  #'
  #' This is the main function for getting all inputs for the module
  #'
  #' @param query_string Raw query string from query builder.
  #' @param col_name The column name in the data containing taxonomy.
  #'   Default is `"LPSN Taxonomy"`.
  #' @return A named list containing `query_string` and `col_name`.
  get_search_inputs <- function(query_string, col_name = "LPSN Taxonomy") {
    list(
      query_string = get_query_string(query_string),
      col_name     = col_name
    )
  }

# === Computing results ===
  #' Compute Search Results
  #'
  #' The main function for getting search results for the module.
  #'
  #' @param query_string A character string representing the query used to filter the data.
  #' @param data A data frame representing the database.
  #' @param col_name The column name in the data containing taxonomy.
  #' @param progress_file Path to a progress \code{.rds} file (typically created
  #'   by \code{create_job_filepaths()}), or \code{NULL} to disable progress
  #'   reporting.  Default is \code{NULL}.
  #' @return A filtered data frame of search results.
  #' @export
  compute_search_results <- function(query_string, data, col_name, progress_file = NULL) {
    # Update progress
    write_progress(progress_file, 0, "Performing search")
    cat(file = stderr(), paste0("Started search at ", Sys.time(), "\n"))
    
    # Add taxonomy of organisms to data
    data <- expand_and_merge_taxonomy(data = data, col_name = col_name)
    
    # Filter data according to query
    data <- filter_data_by_query(data = data, query_string = query_string)
    
    # Update progress
    cat(file = stderr(), paste0("Ended search at ", Sys.time(), "\n"))
    
    return(data)
  }

  #' Run a search job and save the result
  #'
  #' This functions runs a computation job for module.  It involves running the 
  #' main function for getting the search results, then saving the result to a 
  #' file.
  #'
  #' @param query_string A character string giving the database filter.
  #' @param col_name The column name in the data containing taxonomy.
  #' @param job_id Character job id.
  #' @param job_dir Directory where the result file is to be written.
  #' @param progress_file Path to the progress \code{.rds} file, or \code{NULL}.
  #' @return Invisibly \code{TRUE}.
  #' @export
  run_job_search <- function(query_string, col_name,
                                  job_id, job_dir, progress_file = NULL) {
    data <- load_database()
    
    filter_data <- compute_search_results(
      query_string  = query_string,
      data          = data,
      col_name      = col_name,
      progress_file = progress_file
    )

    payload <- list(
      filter_data = filter_data
    )

    save_job_result(job_id = job_id, result = payload, job_dir = job_dir)

    invisible(TRUE)
  }
  
# === Updating user interface (UI) elements ===
  #' Update Choices for Variable to Display (Database Search Module)
  #'
  #' Populates the \code{variable_to_display} picker input with the trait
  #' / taxonomy variables available in the search module. Defaults the
  #' selection to \code{"Phylum"}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{variable_to_display} picker input.
  #' @export
  update_variable_to_display_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices <- choices_traits_search
    selected <- "Phylum"
    
    # Update UI
    update_picker_input(session = session, inputId = "variable_to_display",
                        choices = choices, selected = selected)
  }
  
  #' Update Chpices for Tree Layout (Database Search Module)
  #'
  #' Populates the \code{tree_layout} picker input with the supported
  #' tree layouts (\code{"Equal angle"}, \code{"Daylight"},
  #' \code{"Rectangular"}).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{tree_layout} picker input.
  #' @export
  update_tree_layout_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices <- c("Equal angle", "Daylight", "Rectangular")
    
    # Update UI
    update_picker_input(session = session, inputId = "tree_layout",
                        choices = choices)
  }
  
  #' Update Choices for Organism Checkboxes (Database Search Module)
  #'
  #' Populates the \code{info_organism} checkbox group with the
  #' organism-info fields configured in
  #' \code{choices_checkboxes_search$organism}, defaulting the selection
  #' to \code{"Genus"} and \code{"Species"} (when available).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{info_organism} checkbox group input.
  #' @export
  update_info_organism_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices = choices_checkboxes_search$organism$choices
    selected = intersect(c("Genus", "Species"),
                         choices_checkboxes_search$organism$choices)
    
    # Update UI
    update_checkbox_group(session = session, inputId = "info_organism",
                          choices = choices, selected = selected)
  }
  
  #' Update Choices for Database Checkboxes (Database Search Module)
  #'
  #' Populates the \code{info_databases} checkbox group with
  #' the database-info fields configured in
  #' \code{choices_checkboxes_search$databases}, defaulting the selection
  #' to LPSN, Bergey, GTDB, NCBI, GOLD, IMG, and BacDive identifiers
  #' (those that are present in the choice list).
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{info_databases} checkbox group input.
  #' @export
  update_info_databases_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices = choices_checkboxes_search$databases$choices
    selected = intersect(c(
                          "LPSN Page", "Bergey Article",
                          "GTDB ID", "NCBI Taxonomy ID", "GOLD Organism ID",
                          "IMG Genome ID", "BacDive ID"
                          ),
                          choices_checkboxes_search$databases$choices)
    
    # Update UI
    update_checkbox_group(session = session, inputId = "info_databases",
                          choices = choices, selected = selected)
  }
  
  #' Update Choices for Metabolism Checkboxes (Database Search Module)
  #'
  #' Populates the \code{info_metabolism} checkbox group with
  #' the metabolism-info fields configured in
  #' \code{choices_checkboxes_search$metabolism}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{info_metabolism} checkbox group input.
  #' @export
  update_info_metabolism_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices = choices_checkboxes_search$metabolism$choices
    
    # Update UI
    update_checkbox_group(session = session, inputId = "info_metabolism",
                          choices = choices)
  }
  
  #' Update Choices for Trait Checkboxes (Database Search Module)
  #'
  #' Populates the \code{info_traits} checkbox group with the
  #' trait-info fields configured in
  #' \code{choices_checkboxes_search$traits}.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Invisibly \code{NULL}. Called for its side effect of updating
  #'   the \code{info_traits} checkbox group input.
  #' @export
  update_info_traits_search <- function(session = shiny::getDefaultReactiveDomain()) {
    # Get choices
    choices = choices_checkboxes_search$traits$choices
    
    # Update UI
    update_checkbox_group(session = session, inputId = "info_traits",
                          choices = choices)
  }

# === Generating outputs ===
  #' Format Search Results for Plots
  #'
  #' This function formats taxonomy results into a format suitable 
  #' for different types of plots, though only treemap plots are
  #' supported at present.   
  #'
  #' @param df A data frame containing the taxonomy results.
  #' @param plot_type A character string specifying the type of plot ("treemap").
  #' @param var_name Optional. A character string specifying the variable name to filter by.
  #' @return A formatted data frame ready for plotting.
  #' @export
  #' @importFrom dplyr  mutate select group_by n summarize
  search_results_to_plot <- function(df, plot_type, var_name = NULL) {
    if (plot_type == "treemap") {
      df = df |> 
        dplyr::select(all_of(var_name)) |> 
        dplyr::rename(y = all_of(var_name)) |>
        tidyr::drop_na() |>  
        dplyr::filter(!is.na(y)) 
      
      df <- df |>
        dplyr::group_by(y) |>
        dplyr::summarise(z = dplyr::n(), .groups = 'drop') |>
        dplyr::mutate(z = z / sum(z))
      
      # Convert to percentage
      df$z = df$z * 100
    }
    
    return(df)
  }
  
  #' Filter Tree Layout Based on Selected Organisms
  #'
  #' This function filters a tree layout to retain only the branches that lead to 
  #' organisms matching a given set of IDs from a specified column. It ensures that
  #' only relevant parent-child relationships are kept, while preserving root nodes.
  #'
  #' @param layout A data frame representing the phylogenetic tree layout, containing columns `node`, `label`, `parent`, etc.
  #' @param data A data frame containing the selected organisms.
  #' @param nodes_to_root A data frame mapping tip nodes to their parent nodes, containing columns `tip_node`, `parent_node`, and `child_node`.
  #' @param id_column A string specifying the column in `data` that contains the organism IDs.
  #'        Defaults to `"IMG Genome ID max quality"`.
  #'
  #' @return A filtered version of `layout` containing only the branches leading 
  #'         to the selected organisms, along with root nodes.
  #' 
  #' @export
  filter_tree_layout <- function(layout, data, nodes_to_root, id_column = "IMG Genome ID max quality") {
    if (!id_column %in% names(data)) {
      stop(glue::glue("Column '{id_column}' not found in data"))
    }
    
    # Select matching tip nodes based on the given ID column
    selected_tips <- layout$node[which(layout$label %in% data[[id_column]])]
    
    # Find matching parent-child node pairs
    match <- nodes_to_root |>
      dplyr::filter(tip_node %in% selected_tips) |>
      dplyr::distinct(parent_node, child_node)
    
    # Retain only relevant branches and root nodes
    layout_filtered <- layout |>
      dplyr::semi_join(match, by = c("parent" = "parent_node", "node" = "child_node")) |>
      dplyr::bind_rows(layout |> dplyr::filter(parent == node))
    
    return(layout_filtered)
  }
  
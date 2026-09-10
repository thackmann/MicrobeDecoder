# Define Functions for Obtaining Data from IMG
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

#' Read Annotation Tab File
#'
#' Reads an IMG annotation tab file from a folder. The file is expected at
#' `folder_path/folder_name/folder_name.<database>.tab.txt`.
#'
#' @param folder_path Path to the genome folder.
#' @param database Annotation type, one of "ko", "cog", or "pfam".
#'
#' @return A dataframe with the file contents, or `NULL` if the file is missing.
#'
#' @importFrom readr read_delim
#' @export
read_annotation_tab_file <- function(folder_path, database = c("ko", "cog", "pfam")) {
  database <- match.arg(database)
  
  folder_name <- basename(folder_path)
  file_name <- paste0(folder_name, ".", database, ".tab.txt")
  file_path <- file.path(folder_path, folder_name, file_name)
  
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  data <- readr::read_delim(file_path, delim = "\t", col_names = TRUE, quote = "\"",
                            escape_double = TRUE, na = c("", "NA"),
                            trim_ws = TRUE, show_col_types = FALSE)
  
  return(data)
}



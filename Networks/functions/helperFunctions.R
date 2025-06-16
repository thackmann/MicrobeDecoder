# Helper Functions for Reference Networks
# This script contains various helper functions for building reference networks
# for the app.
# Author: Timothy Hackmann
# Date: 25 Apr 2025

# === General ===
  #' Pipe Operator
  #'
  #' This operator is imported from the magrittr package and is used to chain operations together.
  #'
  import::from(magrittr, "%>%")

# === Retrieve information from KEGG ===
  #' Retrieve the Content of a Web Page
  #'
  #' This function retrieves the content of a web page using the `polite` package.
  #' You can choose whether to return the parsed HTML or plain text by setting the MIME type.
  #'
  #' @param url A character string representing the URL of the web page to scrape.
  #' @param user_agent A character string specifying the user agent to use for the request. 
  #'        Default is `"me"`.
  #' @param accept A character string indicating the MIME type to return:
  #'        `"html"` (default) returns an HTML document, `"txt"` returns plain text.
  #'
  #' @return The body of the web page, either as an HTML document or a plain text string.
  #' @export
  #'
  #' @examples
  #' # Get HTML
  #' html <- get_web_page_body("https://example.com")
  #'
  #' # Get plain text
  #' text <- get_web_page_body("https://rest.kegg.jp/list/pathway", accept = "txt")
  get_web_page_body <- function(url, user_agent = "me", accept = c("html", "txt")) {
    accept <- match.arg(accept)
    
    session <- polite::bow(url, user_agent = user_agent)
    content <- polite::scrape(bow = session, accept = accept)
    
    return(content)
  }
  
  #' Fetch KEGG Link Table
  #'
  #' Downloads and parses a 2-column link table from the KEGG REST API.
  #' This function is useful for retrieving mappings between KEGG entries,
  #' such as reactions to pathways or KO IDs to reactions.
  #'
  #' @param url A character string representing the full URL to a KEGG REST endpoint.
  #'
  #' @return A data frame with two columns: \code{From} and \code{To}, representing the mapping.
  #' @export
  fetch_kegg_link <- function(url) {
    text <- get_web_page_body(url)
    
    read.table(text = text, sep = "\t", col.names = c("From", "To"), stringsAsFactors = FALSE)
  }
  
  
  #' Get KEGG Modules Associated with a Pathway Map
  #'
  #' This function retrieves the KEGG module identifiers and descriptions associated 
  #' with a given KEGG pathway map (e.g., \code{"map00910"}) by parsing the flat file 
  #' returned from the KEGG REST API. It extracts all lines within the md block 
  #' and returns a data frame with module IDs and their full descriptions.
  #'
  #' @param map A character string representing the KEGG map ID (e.g., \code{"map00910"}).
  #'
  #' @return A data frame with two columns:
  #'   \describe{
  #'     \item{md}{The KEGG module identifier (e.g., \code{"M00175"}).}
  #'     \item{md_Name}{The full name or description of the md, 
  #'                        including optional pathway context (e.g., \code{"Nitrogen fixation, nitrogen => ammonia [PATH:map00910]"}).}
  #'   }
  #' Returns \code{NULL} if the md block is not found or if the query fails.
  #'
  #' @examples
  #' map_to_md("map00910")
  #'
  #' @export
  map_to_md <- function(map) {
    url <- paste0("https://rest.kegg.jp/get/", map)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (is.na(txt)) return(NULL)
    
    lines <- strsplit(txt, "\n")[[1]]
    start <- which(grepl("^MODULE", lines))[1]
    if (is.na(start)) return(NULL)
    
    end <- which((seq_along(lines) > start) & grepl("^\\S", lines))[1]
    if (is.na(end)) end <- length(lines) + 1
    
    md_lines <- lines[start:(end - 1)]
    md_lines_clean <- gsub("^MODULE\\s+|^\\s+", "", md_lines)
    
    md <- sub("\\s+.*", "", md_lines_clean)
    return(unique(md))
  }
  
  #' Retrieve Metadata for a KEGG Module
  #'
  #' Fetches metadata such as name, definition, class, and component KO IDs for a given KEGG module ID.
  #'
  #' @param md A character string like "M00175".
  #'
  #' @return A named list with metadata fields, or NA if the module is not found.
  #' @export
  get_module_metadata <- function(md) {
    url <- paste0("https://rest.kegg.jp/get/", md)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (is.na(txt)) return(NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    
    parse_field <- function(field) {
      match <- grep(paste0("^", field, "\\s+"), lines, value = TRUE)
      if (length(match)) trimws(sub(paste0("^", field, "\\s+"), "", match)) else NA
    }
    
    list(
      md   = md,
      name        = parse_field("NAME"),
      definition  = parse_field("DEFINITION"),
      class       = parse_field("CLASS"),
      pathway     = parse_field("PATHWAY"),
      ko         = grep("^  K\\d{5}", lines, value = TRUE) %>% 
        stringr::str_extract_all("K\\d{5}") %>% 
        unlist()
    )
  }
  
  #' Retrieve KEGG Compound Name
  #'
  #' Given a KEGG compound ID (e.g., "C00022"), this function retrieves the corresponding compound name.
  #'
  #' @param cid A character string representing a KEGG compound ID.
  #'
  #' @return A character string with the compound name, or NA if not found.
  #' @export
  get_compound_name <- function(cid) {
    url <- paste0("https://rest.kegg.jp/get/cpd:", cid)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (is.na(txt)) return(NA)
    name_line <- grep("^NAME\\s+", strsplit(txt, "\n")[[1]], value = TRUE)
    if (length(name_line) == 0) return(NA)
    name <- stringr::str_trim(sub("^NAME\\s+", "", name_line))
    sub(";$", "", name)
  }
  
  #' Replace KEGG Compound IDs with Names in reaction equation
  #'
  #' Replaces all KEGG compound IDs (e.g., "C00022") in a reaction equation with their corresponding names.
  #'
  #' @param eq A character string representing a KEGG reaction equation with compound IDs.
  #'
  #' @return A character string with compound names in place of IDs, or NA if input is NA.
  #' @export
  replace_compounds <- function(eq) {
    if (is.na(eq)) return(NA)
    for (i in seq_len(nrow(compound_df))) {
      eq <- stringr::str_replace_all(eq, compound_df$Compound_ID[i], compound_df$Compound_Name[i])
    }
    eq
  }
  
  #' Get Reaction IDs for a KEGG Map
  #'
  #' This function retrieves reaction IDs associated with a given KEGG ID.
  #' It can optionally retrieve this information from modules rather than
  #' from the map directly.  
  #'
  #' @param map KEGG map ID (e.g., "map00010", "map00190")
  #' @param use_modules Logical; if TRUE, uses md links instead of pathway links
  #' @return Data frame with columns `ID` (map or module ID) and `rn`
  map_to_rn <- function(map, use_modules = FALSE) {
    if (use_modules) {
      md <- map_to_md(map)
      purrr::map_dfr(md, function(md2) {
        res <- fetch_kegg_link(paste0("https://rest.kegg.jp/link/rn/md:", md2))
        if (is.null(res) || length(res) == 0 || nrow(res) == 0) {
          return(tibble::tibble(rn = character()))
        }
        res %>%
          dplyr::mutate(rn = sub("rn:", "", To)) %>%
          dplyr::select(rn) %>%
          dplyr::distinct()
      }) %>%
        dplyr::pull(rn) %>%
        unique()
    } else {
      fetch_kegg_link(paste0("https://rest.kegg.jp/link/rn/path:", map)) %>%
        dplyr::mutate(
          rn = sub("rn:", "", To),
          map = map
        ) %>%
        dplyr::pull(rn) %>%
        unique()
    }
  }
  
  #' Get Reaction IDs for a KEGG Map
  #'
  #' This function retrieves reaction IDs associated with a given KEGG ID.
  #' It can optionally retrieve this information from modules rather than
  #' from the map directly.  
  #'
  #' @param map KEGG map ID (e.g., "map00010", "map00190")
  #' @param use_modules Logical; if TRUE, uses md links instead of pathway links
  #' @return Data frame with columns `ID` (map or module ID) and `rn`
  map_to_rn <- function(map, use_modules = FALSE) {
    if (use_modules) {
      md <- map_to_md(map)
      purrr::map_dfr(md, function(md2) {
        res <- fetch_kegg_link(paste0("https://rest.kegg.jp/link/rn/md:", md2))
        if (is.null(res) || length(res) == 0 || nrow(res) == 0) {
          return(tibble::tibble(rn = character()))
        }
        res %>%
          dplyr::mutate(rn = sub("rn:", "", To)) %>%
          dplyr::select(rn) %>%
          dplyr::distinct()
      }) %>%
        dplyr::pull(rn) %>%
        unique()
    } else {
      fetch_kegg_link(paste0("https://rest.kegg.jp/link/rn/path:", map)) %>%
        dplyr::mutate(
          rn = sub("rn:", "", To),
          map = map
        ) %>%
        dplyr::pull(rn) %>%
        unique()
    }
  }
  
  #' Get KO ID for Reaction ID in KEGG
  #'
  #' This function takes a reaction ID and finds the corresponding KO IDs
  #' It can either download all reaction ID-KO IDs in bulk or query a single 
  #' rn directly
  #'
  #' @param rn A character for a KEGG reaction ID
  #' @param download_all A logical indicating if the reactions should be downloaded
  #' @return A character with KO IDs
  rn_to_ko <- function(rn, download_all = TRUE) {
    if (download_all == TRUE) {
      ko <- fetch_kegg_link("https://rest.kegg.jp/link/ko/rn") %>%
        dplyr::mutate(
          ko = sub("ko:", "", To),
          rn2 = sub("rn:", "", From)
        ) %>%
        dplyr::filter(rn2 == rn) %>%
        dplyr::pull(ko) %>%
        unique()
    } else {
      ko <- fetch_kegg_link(paste0("https://rest.kegg.jp/link/ko/rn:", rn)) %>%
        dplyr::mutate(
          ko = sub("ko:", "", To),
          rn = sub("rn:", "", From)
        ) %>%
        dplyr::pull(ko) %>%
        unique()
    }
    
    if(length(ko) == 0) ko <- NA_character_
    
    return(ko)
  }
  
  #' Get Reaction Equation for Reaction ID from KEGG
  #'
  #' This function takes a KEGG reaction ID (e.g., "R00623") and returns the corresponding reaction equation.
  #' It optionally replaces compound IDs with compound names using KEGG metadata.
  #'
  #' @param rn A character string for a KEGG reaction ID.
  #' @param return_names Logical. If TRUE, replaces compound IDs with their names. Default is TRUE
  #' @return A character string with the reaction equation, optionally with compound names.
  #' @export
  rn_to_eq <- function(rn, return_names = TRUE) {
    url <- paste0("https://rest.kegg.jp/get/rn:", rn)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    eq_line <- grep("^EQUATION\\s+", lines, value = TRUE)
    eq <- if (length(eq_line)) stringr::str_trim(sub("^EQUATION\\s+", "", eq_line)) else NA
    
    if (return_names && !is.na(eq)) {
      # Extract KEGG compound IDs (C00001, C00002, etc.)
      compound_ids <- stringr::str_extract_all(eq, "C\\d{5}")[[1]] %>% unique()
      compound_names <- vapply(compound_ids, get_compound_name, character(1), USE.NAMES = FALSE)
      compound_df <- data.frame(Compound_ID = compound_ids, Compound_Name = compound_names, stringsAsFactors = FALSE)
      
      # Replace IDs with names
      for (i in seq_len(nrow(compound_df))) {
        eq <- stringr::str_replace_all(eq, compound_df$Compound_ID[i], compound_df$Compound_Name[i])
      }
    }
    
    return(eq)
  }
  
  #' Get EC Number for Reaction ID from KEGG
  #'
  #' This function takes a reaction ID and returns the corresponding EC number
  #'
  #' @param rn A character for a KEGG reaction ID
  #' @return A character string with the reaction equation
  #' @export
  rn_to_ec <- function(rn) {
    url <- paste0("https://rest.kegg.jp/get/rn:", rn)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA_character_)
    if (is.na(txt)) return(NA_character_)
    
    lines <- strsplit(txt, "\n")[[1]]
    enzyme_line <- grep("^ENZYME\\s+", lines, value = TRUE)
    
    ec <- if (length(enzyme_line)) {
      enzyme_str <- sub("^ENZYME\\s+", "", enzyme_line)
      ec_raw <- unlist(strsplit(enzyme_str, "\\s+"))
      ec_clean <- ec_raw[grepl("^\\d+\\.\\d+\\.\\d+\\.\\d+$", ec_raw)]
      if (length(ec_clean)) paste(ec_clean, collapse = ", ") else NA_character_
    } else {
      NA_character_
    }
    
    return(ec)
  }
  
  #' Get Name for for Reaction ID from KEGG
  #'
  #' This function takes a KEGG reaction ID (e.g., "R00623") and returns the corresponding name.
  #'
  #' @param rn A character string for a KEGG reaction ID.
  #' @export
  rn_to_name <- function(rn) {
    url <- paste0("https://rest.kegg.jp/get/rn:", rn)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    name_line <- grep("^NAME\\s+", lines, value = TRUE)
    name <- if (length(name_line)) stringr::str_trim(sub("^NAME\\s+", "", name_line[1])) else NA
    
    return(name)
  }
  
  #' Get Name for KO ID in KEGG
  #'
  #' This function takes a KO ID and finds the corresponding name
  #'
  #' @param ko A character for a KO ID
  #' @return A character with names
  ko_to_name <- function(ko)
  {
    url <- paste0("https://rest.kegg.jp/get/ko:", ko)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    name_line <- grep("^NAME\\s+", lines, value = TRUE)
    
    name <- if (length(name_line)) stringr::str_trim(sub("^NAME\\s+", "", name_line)) else NA
    
    return(name)
  }
  
  #' Get Symbol for KO ID in KEGG
  #'
  #' This function takes a KO ID and finds the corresponding symbol
  #'
  #' @param ko A character for a KO ID
  #' @return A character with symbols
  ko_to_symbol <- function(ko)
  {
    url <- paste0("https://rest.kegg.jp/get/ko:", ko)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    symbol_line <- grep("^SYMBOL\\s+", lines, value = TRUE)
    
    symbol <- if (length(symbol_line)) {
      stringr::str_trim(strsplit(sub("^SYMBOL\\s+", "", symbol_line), ",")[[1]][1])
    } else {
      NA
    }
    
    return(symbol)
  }
  
  #' Get KO IDs for Module ID from KEGG
  #'
  #' This function takes a KEGG module ID and returns the corresponding KO IDs.
  #'
  #' @param md A character for a KEGG module ID (e.g., "M00001")
  #' @return A character vector of KO IDs
  #' @export
  md_to_ko <- function(md) {
    url <- paste0("https://rest.kegg.jp/link/ko/md:", md)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    df <- read.table(text = txt, sep = "\t", col.names = c("From", "To"), stringsAsFactors = FALSE)
    ko <- sub("ko:", "", df$To)
    
    ko <- unique(ko)
    if (length(ko) == 0) ko <- NA_character_
    
    return(ko)
  }
  
  #' Get Reaction IDs for Module ID from KEGG
  #'
  #' This function takes a KEGG module ID and returns the corresponding rn (RN) IDs.
  #'
  #' @param md A character for a KEGG module ID (e.g., "M00001")
  #' @return A character vector of KEGG reaction IDs
  #' @export
  md_to_rn <- function(md) {
    url <- paste0("https://rest.kegg.jp/link/rn/md:", md)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    df <- read.table(text = txt, sep = "\t", col.names = c("From", "To"), stringsAsFactors = FALSE)
    rn <- sub("rn:", "", df$To)
    
    rn <- unique(rn)
    if (length(rn) == 0) rn <- NA_character_
    
    return(rn)
  }
  
  #' Get KO IDs for EC number from KEGG
  #'
  #' This function takes an EC number and returns the corresponding KO IDs.
  #'
  #' @param ec A character for an EC number (e.g., "1.1.1.1")
  #' @return A character vector of KO IDs
  #' @export
  ec_to_ko <- function(ec) {
    url <- paste0("https://rest.kegg.jp/link/ko/ec:", ec)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    df <- read.table(text = txt, sep = "\t", col.names = c("From", "To"), stringsAsFactors = FALSE)
    ko <- sub("ko:", "", df$To)
    
    ko <- unique(ko)
    if (length(ko) == 0) ko <- NA_character_
    
    return(ko)
  }
  
  #' Get Reaction IDs for EC Number from KEGG
  #'
  #' This function takes an EC number and returns the corresponding KEGG reaction IDs.
  #'
  #' @param ec A character for an EC number (e.g., "1.1.1.1")
  #' @return A character vector of KEGG reaction IDs
  #' @export
  ec_to_rn <- function(ec) {
    url <- paste0("https://rest.kegg.jp/link/rn/ec:", ec)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    df <- read.table(text = txt, sep = "\t", col.names = c("From", "To"), stringsAsFactors = FALSE)
    rn <- sub("rn:", "", df$To)
    
    rn <- unique(rn)
    if (length(rn) == 0) rn <- NA_character_
    
    return(rn)
  }
  
  #' Get Name for EC Number from from KEGG
  #'
  #' his function takes an EC number and returns the corresponding name.
  #'
  #' @param ec A character for an EC number (e.g., "1.1.1.1")
  #' @export
  ec_to_name <- function(ec) {
    url <- paste0("https://rest.kegg.jp/get/ec:", ec)
    txt <- tryCatch(get_web_page_body(url, accept = "txt"), error = function(e) NA)
    if (length(txt) == 0 || is.na(txt)) return(NA)
    
    lines <- strsplit(txt, "\n")[[1]]
    name_line <- grep("^NAME\\s+", lines, value = TRUE)
    name <- if (length(name_line)) stringr::str_trim(sub("^NAME\\s+", "", name_line[1])) else NA
    
    return(name)
  }
  
  #' Get KEGG Metadata for a Reaction
  #'
  #' Given a KEGG reaction ID, returns metadata including KO IDs, KO names, KO symbols, the reaction equation, and EC numbers.
  #'
  #' @param rn A character string representing a KEGG reaction ID (e.g., "R00623")
  #' @param download_all A logical indicating whether to use the full KEGG ko-rn mapping or query one-by-one
  #'
  #' @return A named list with elements: `ko`, `name`, `symbol`, `eq`, and `ec`
  #' @export
  get_kegg_metadata_from_rn <- function(rn, download_all = TRUE) {
    ko <- rn_to_ko(rn, download_all = download_all)
    name <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_name, character(1)) else rn_to_name(rn)
    symbol <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_symbol, character(1)) else NA_character_
    eq <- rn_to_eq(rn)
    ec <- rn_to_ec(rn)
    
    list(
      ko = ko,
      name = name,
      symbol = symbol,
      rn = rn,
      eq = eq,
      ec = ec
    )
  }
  
  #' Get KEGG Metadata for a Module
  #'
  #' Given a KEGG module ID, returns metadata including KO IDs, KO names, KO symbols, reaction IDs, eq, and EC numbers.
  #'
  #' @param md A character string representing a KEGG module ID (e.g., "M00001")
  #'
  #' @return A named list with elements: `ko`, `name`, `symbol`, `rn`, `eq`, and `ec`
  #' @export
  get_kegg_metadata_from_md <- function(md) {
    ko <- md_to_ko(md)
    rn_ids <- md_to_rn(md)
    
    if (length(rn_ids) > 1 && !all(is.na(rn_ids))) {
      warning("Multiple reaction IDs detected. Mapping of KO IDs to reaction IDs not possible.")
    }
    
    name <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_name, character(1)) else NA_character_
    symbol <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_symbol, character(1)) else NA_character_
    
    eq <- if (length(rn_ids) > 0 && !all(is.na(rn_ids))) vapply(rn_ids, rn_to_eq, character(1)) else NA_character_
    ec <- if (length(rn_ids) > 0 && !all(is.na(rn_ids))) vapply(rn_ids, rn_to_ec, character(1)) else NA_character_
    
    list(
      ko = ko,
      name = name,
      symbol = symbol,
      rn = rn_ids,
      eq = eq,
      ec = ec
    )
  }
  
  #' Get KEGG Metadata for an EC number
  #'
  #' Given an EC number, returns metadata including KO IDs, KO names, KO symbols, reaction IDs, and eq.
  #'
  #' @param ec A character string representing an EC number (e.g., "1.1.1.1")
  #'
  #' @return A named list with elements: `ko`, `name`, `symbol`, `rn`, `eq`, and `ec`
  #' @export
  get_kegg_metadata_from_ec <- function(ec) {
    ko <- ec_to_ko(ec)
    rn_ids <- ec_to_rn(ec)
    
    name <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_name, character(1)) else ec_to_name(ec)
    symbol <- if (length(ko) > 0 && !all(is.na(ko))) vapply(ko, ko_to_symbol, character(1)) else NA_character_
    
    eq <- if (length(rn_ids) > 0 && !all(is.na(rn_ids))) vapply(rn_ids, rn_to_eq, character(1)) else NA_character_
    if (length(eq) == 0) eq <- NA_character_
    
    list(
      ko = ko,
      name = name,
      symbol = symbol,
      rn = rn_ids,
      eq = eq,
      ec = ec
    )
  }
  
  #' Get KEGG Metadata from KO IDs
  #'
  #' Given one or more KEGG KO IDs (e.g., "K00134"), returns metadata including KO names,
  #' KO symbols, reaction IDs, equations, and EC numbers.
  #'
  #' @param ko A character vector of KEGG KO IDs.
  #'
  #' @return A named list with elements: `ko`, `name`, `symbol`, `rn`, `eq`, and `ec`
  #' @export
  get_kegg_metadata_from_ko <- function(ko) {
    name   <- vapply(ko, ko_to_name, character(1))
    symbol <- vapply(ko, ko_to_symbol, character(1))
    
    # Get rn for each ko
    rn_df <- fetch_kegg_link("https://rest.kegg.jp/link/rn/ko") %>%
      dplyr::mutate(
        ko_ids = sub("ko:", "", From),
        rn = sub("rn:", "", To)
      ) %>%
      dplyr::filter(ko_ids %in% ko)
    
    rn_list <- split(rn_df$rn, rn_df$ko)
    
    # Get equation and EC number for each KO ID's reactions
    rn_all <- unique(rn_df$rn)
    eq_list <- vapply(rn_all, rn_to_eq, character(1), USE.NAMES = TRUE)
    ec_list <- vapply(rn_all, rn_to_ec, character(1), USE.NAMES = TRUE)
    
    # Map back to ko
    rn_by_ko <- lapply(ko, function(k) rn_list[[k]] %||% NA_character_)
    eq_by_ko <- lapply(rn_by_ko, function(rn) {
      if (all(is.na(rn))) return(NA_character_)
      unique(unlist(eq_list[rn], use.names = FALSE))
    })
    ec_by_ko <- lapply(rn_by_ko, function(rn) {
      if (all(is.na(rn))) return(NA_character_)
      unique(unlist(ec_list[rn], use.names = FALSE))
    })
    
    list(
      ko = ko,
      name = name,
      symbol = symbol,
      rn = rn_by_ko,
      eq = eq_by_ko,
      ec = ec_by_ko
    )
  }
  
  #' Get KEGG Metadata from Reaction, Module, or EC number
  #'
  #' Given a KEGG reaction ID (rn), module ID (md), or EC number (ec), returns a flattened data frame
  #' containing ko, KO name, KO symbol, reaction ID, equation, and EC number.
  #' Only one of `rn`, `md`, or `ec` should be supplied.
  #'
  #' @param rn A character string representing a KEGG reaction ID (e.g., "R00623")
  #' @param md A character string representing a KEGG module ID (e.g., "M00001")
  #' @param ec A character string representing an EC number (e.g., "1.1.1.1")
  #' @param ko A character vector of KO IDs (e.g., c("K00001", "K00002"))
  #' @param download_all A logical indicating whether to download the full ko-rn mapping (used for `rn`)
  #' @param manual_ko Optional character vector of KO IDs to use if lookup fails or override is needed
  #' @param manual_name Optional character vector of KO names
  #' @param manual_symbol Optional character vector of KO symbols
  #' @param manual_rn Optional character vector of reaction IDs
  #' @param manual_eq Optional character vector of reaction equations
  #' @param manual_ec Optional character vector of EC numbers
  #'
  #' @return A `data.frame` with columns: ko, name, symbol, rn, eq, ec
  #' @export
  get_kegg_metadata <- function(
      rn = NULL, md = NULL, ec = NULL, ko = NULL, download_all = TRUE,
      manual_ko = NULL, manual_name = NULL, manual_symbol = NULL,
      manual_rn = NULL, manual_eq = NULL, manual_ec = NULL
  ) {
    input_types <- c(!is.null(rn), !is.null(md), !is.null(ec), !is.null(ko))
    if (sum(input_types) != 1) {
      stop("Please provide exactly one of: rn, md, ec, or ko.")
    }
    
    metadata <- if (!is.null(rn)) {
      get_kegg_metadata_from_rn(rn, download_all = download_all)
    } else if (!is.null(md)) {
      get_kegg_metadata_from_md(md)
    } else if (!is.null(ec)) {
      get_kegg_metadata_from_ec(ec)
    } else {
      get_kegg_metadata_from_ko(ko)
    }
    
    # Manual overrides
    ko     <- if (!is.null(manual_ko)) manual_ko else metadata$ko
    name   <- if (!is.null(manual_name)) manual_name else metadata$name
    symbol <- if (!is.null(manual_symbol)) manual_symbol else metadata$symbol
    rn     <- if (!is.null(manual_rn)) manual_rn else metadata$rn
    eq     <- if (!is.null(manual_eq)) manual_eq else metadata$eq
    ec     <- if (!is.null(manual_ec)) manual_ec else metadata$ec
    
    ko     <- if (is.null(ko) || all(is.na(ko))) NA_character_ else ko
    name   <- if (is.null(name) || all(is.na(name))) NA_character_ else name
    symbol <- if (is.null(symbol) || all(is.na(symbol))) NA_character_ else symbol
    rn     <- if (is.null(rn) || all(is.na(rn))) NA_character_ else rn
    eq     <- if (is.null(eq) || all(is.na(eq))) NA_character_ else eq
    ec     <- if (is.null(ec) || all(is.na(ec))) NA_character_ else ec
    
    # Flatten rn, eq, ec if nested lists (ko input)
    if (is.list(rn)) rn <- unlist(rn, use.names = FALSE)
    if (is.list(eq)) eq <- unlist(eq, use.names = FALSE)
    if (is.list(ec)) ec <- unlist(ec, use.names = FALSE)
    
    df <- expand.grid(
      ko = ko,
      rn = rn,
      stringsAsFactors = FALSE
    )
    
    df$name   <- rep_len(name, nrow(df))
    
    df$symbol <- rep_len(symbol, nrow(df))
    
    df$eq <- if (!is.null(names(eq))) {
      unname(eq[match(df$rn, names(eq))])
    } else {
      rep_len(eq, nrow(df))
    }
    
    df$ec <- if (!is.null(names(ec))) {
      unname(ec[match(df$rn, names(ec))])
    } else {
      rep_len(ec, nrow(df))
    }
    
    return(df)
  }
  
# === Parse KEGG module Definitions ===
  #' Add Placeholders for Subunit Groups
  #'
  #' Replaces additive subunit groups in KEGG module definitions with the placeholder string "PLACEHOLDER".
  #'
  #' @param definition A character string representing a KEGG module definition.
  #'
  #' @return A modified definition string with additive groups replaced by placeholders.
  #' @export
  add_placeholders <- function(definition) {
    s1 = definition
    s2 <- gsub("\\+\\([^()]+\\)\\+", "+PLACEHOLDER+", s1)
    s2 <- gsub("\\+\\([^()]+\\)", "+PLACEHOLDER", s2)
    s2 <- gsub("\\([^()]+\\)\\+", "PLACEHOLDER+", s2)
    
    return(s2)
  }
  
  #' Find what text was hidden by placeholders
  #'
  #' @param placeholder_string String with placeholders (e.g., "PLACEHOLDER").
  #' @param original_string The original string.
  #' @param placeholder Placeholder text. Default is "PLACEHOLDER".
  #'
  #' @return Character vector of inserted fragments from the original string.
  #' @export
  find_hidden_text <- function(s1, s2, placeholder = "PLACEHOLDER") {
    parts <- strsplit(s1, split = placeholder, fixed = TRUE)[[1]]
    
    # Handle case where placeholder was at the end but no "" was appended
    if (length(parts) == 1 && grepl("[+\\-]$", parts[1])) {
      parts <- c(parts[1], "")
    }
    
    n <- length(parts) - 1
    replacements <- character(n)
    
    # Start position in s2
    pos <- 1
    
    for (i in seq_len(n)) {
      # Find part[i] starting from current position
      if (nzchar(parts[i])) {
        m1 <- regexpr(parts[i], substr(s2, pos, nchar(s2)), fixed = TRUE)
        if (m1[1] == -1) stop("Could not find part[i] in s2")
        start <- pos + m1[1] + attr(m1, "match.length") - 1
      } else {
        start <- pos
      }
      
      # Find part[i + 1] starting from 'start'
      if (nzchar(parts[i + 1])) {
        m2 <- regexpr(parts[i + 1], substr(s2, start, nchar(s2)), fixed = TRUE)
        if (m2[1] == -1) stop("Could not find part[i + 1] in s2")
        end <- start + m2[1] - 2
      } else {
        end <- nchar(s2)
      }
      
      replacements[i] <- substr(s2, start, end)
      pos <- end + 1
    }
    
    replacements <- gsub("^\\s*\\(*|\\)*\\s*$", "", replacements)
    replacements <- trimws(replacements)
    
    return(replacements)
  }
  
  #' Remove Optional KO IDs and Groups from a KEGG module Definition
  #'
  #' Removes both individual KO IDs (e.g., '-K00242') and grouped ko sets (e.g., '-(K00242,K18859)').
  #'
  #' @param definition A character string representing a KEGG module definition.
  #'
  #' @return A cleaned definition string.
  #' @export
  remove_optional_kos <- function(definition) {
    definition <- gsub("-K\\d{5}", "", definition)
    definition <- gsub("-\\([^()]*?\\)", "", definition)
    return(definition)
  }
  
  #' Clean KEGG module Definition
  #'
  #' Prepares a KEGG module definition for parsing by protecting additive groups,
  #' replacing top-level spaces, and removing optional ko entries.
  #'
  #' @param definition A character string representing a KEGG module definition.
  #'
  #' @return A cleaned md definition string.
  #' @export
  clean_md_definition <- function(definition) {
    definition <- remove_optional_kos(definition)
    definition <- gsub("\\+\\(", "_ADDOPEN_", definition)
    definition <- gsub("\\)\\+", "_ADDCLOSE_", definition)
    definition <- gsub("[()]", "", definition)
    definition <- gsub("\\s+", ",", definition)
    definition <- gsub("_ADDOPEN_", "+(", definition)
    definition <- gsub("_ADDCLOSE_", ")+", definition)
    
    return(definition)
  }
  
  #' Parse Subunit String into KO Options
  #'
  #' Parses a subunit string into a named list of KO options. This is used for both
  #' placeholder resolution and general subunit parsing within md definitions.
  #'
  #' @param def_string A string representing ko options (e.g., "K00163,K00161+K00162").
  #'
  #' @return A named list of options (each a vector of KO IDs).
  #' @export
  parse_subunit_string <- function(def_string) {
    def_string <- gsub("^\\((.*)\\)$", "\\1", def_string)
    alt_parts <- strsplit(def_string, ",", fixed = TRUE)[[1]]
    options <- lapply(alt_parts, function(x) strsplit(x, "+", fixed = TRUE)[[1]])
    names(options) <- paste0("option_", seq_along(options))
    return(options)
  }
  
  #' Split at Top-Level Commas
  #'
  #' Splits a character string by commas that are **not enclosed in parentheses**.
  #' This is useful for parsing KEGG module definitions where additive or alternative
  #' ko groups are nested within parentheses and should not be split.
  #'
  #' @param s A character string to split, typically a KEGG subunit or md definition string.
  #'
  #' @return A character vector of substrings split by top-level commas only.
  #' @examples
  #' split_top_level_commas("K00123+(K00456,K00457)+K00789")
  #' # Returns: c("K00123+(K00456,K00457)+K00789") since no top-level comma exists
  #'
  #' split_top_level_commas("K00123,K00456,(K00789,K00890),K00999")
  #' # Returns: c("K00123", "K00456", "(K00789,K00890)", "K00999")
  #'
  #' @keywords internal
  split_top_level_commas <- function(s) {
    chars <- strsplit(s, "")[[1]]
    parts <- c()
    depth <- 0
    last <- 1
    for (i in seq_along(chars)) {
      if (chars[i] == "(") depth <- depth + 1
      if (chars[i] == ")") depth <- depth - 1
      if (chars[i] == "," && depth == 0) {
        parts <- c(parts, substr(s, last, i - 1))
        last <- i + 1
      }
    }
    parts <- c(parts, substr(s, last, nchar(s)))
    trimws(parts)
  }
  
  #' Create Nested Module list from Cleaned Definition
  #'
  #' Converts a cleaned KEGG module definition into a nested list representing enzymes, subunits, and ko options.
  #'
  #' @param cleaned A cleaned definition string.
  #'
  #' @return A nested list structured by enzyme b subunit b option b KO IDs.
  #' @export
  make_module_list_from_cleaned <- function(cleaned) {
    enzyme_parts <- split_top_level_commas(cleaned)
    md_list <- list()
    
    for (enzymex in seq_along(enzyme_parts)) {
      enzyme <- enzyme_parts[enzymex]
      subunits <- strsplit(enzyme, "+", fixed = TRUE)[[1]]
      
      subunit_list <- list()
      for (subunit_idx in seq_along(subunits)) {
        option_list <- parse_subunit_string(subunits[subunit_idx])
        subunit_list[[paste0("subunit_", subunit_idx)]] <- option_list
      }
      
      md_list[[paste0("enzyme_", enzymex)]] <- subunit_list
    }
    
    return(md_list)
  }
  
  #' Resolve Placeholder Subunits in a Module List
  #'
  #' Replaces "PLACEHOLDER" entries in a nested module list with parsed KO sets from the original definition.
  #'
  #' @param md_list A nested list generated by `make_module_list_from_cleaned()`.
  #' @param replacements A character vector of ko group definitions that replaced each placeholder.
  #'
  #' @return The same module list with resolved ko sets.
  #' @export
  resolve_placeholders_in_md_list <- function(md_list, replacements) {
    placeholder_index <- 1
    for (enzyme_name in names(md_list)) {
      subunits <- md_list[[enzyme_name]]
      for (subunit_name in names(subunits)) {
        options <- subunits[[subunit_name]]
        for (option_name in names(options)) {
          value <- options[[option_name]]
          if (length(value) == 1 && value == "PLACEHOLDER") {
            repl <- replacements[placeholder_index]
            placeholder_index <- placeholder_index + 1
            resolved_options <- parse_subunit_string(repl)
            md_list[[enzyme_name]][[subunit_name]] <- resolved_options
          }
        }
      }
    }
    return(md_list)
  }
  
  #' Flatten Nested MOdule List into a Data Frame
  #'
  #' Converts a nested enzyme module list into a long-format data frame,
  #' using simplified numeric/letter identifiers for enzyme, subunit, and option.
  #'
  #' @param md_list A nested list representing enzymes, subunits, options, and KO IDs.
  #'
  #' @return A data frame with columns: Enzyme (numeric), Subunit (letter), Option (letter), ko (character).
  #' @export
  flatten_md_list <- function(md_list) {
    flat <- data.frame(
      Enzyme = integer(),
      Subunit = character(),
      Option = character(),
      ko = character(),
      stringsAsFactors = FALSE
    )
    
    for (enzyme_name in names(md_list)) {
      enzymex <- as.integer(sub("enzyme_", "", enzyme_name))
      enzyme <- md_list[[enzyme_name]]
      
      for (subunit_name in names(enzyme)) {
        subunit_idx <- as.integer(sub("subunit_", "", subunit_name))
        subunit <- enzyme[[subunit_name]]
        subunit_letter <- LETTERS[subunit_idx]
        
        for (option_name in names(subunit)) {
          option_idx <- as.integer(sub("option_", "", option_name))
          option_letter <- LETTERS[option_idx]
          
          kos <- subunit[[option_name]]
          
          flat <- rbind(
            flat,
            data.frame(
              Enzyme = enzymex,
              Subunit = subunit_letter,
              Option = option_letter,
              ko = kos,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
    
    return(flat)
  }
  
  #' Parse KEGG module Definition into KO Summary Table
  #'
  #' Parses a full KEGG module definition string and returns a summary data frame
  #' with ko combinations per subunit option.
  #'
  #' @param definition A character string representing a KEGG module definition.
  #'
  #' @return A data frame summarizing enzyme, subunit, and ko combinations.
  #' @export
  parse_module_definition <- function(definition) {
    flagged <- add_placeholders(definition)
    cleaned <- clean_md_definition(flagged)
    replacements <- find_hidden_text(flagged, definition)
    
    md_list <- make_module_list_from_cleaned(cleaned)
    resolved <- resolve_placeholders_in_md_list(md_list, replacements)
    df <- flatten_md_list(resolved)
    
    df_summary <- df %>%
      dplyr::group_by(Enzyme, Subunit, Option) %>%
      dplyr::summarise(ko = paste(ko, collapse = ", "), .groups = "drop")
    
    return(df_summary)
  }
  
# === Enzyme ko DataFrame Functions ===
  #' Expand Enzyme ko DataFrame from Nested Structure
  #'
  #' Converts a nested list of enzyme ko combinations into a long-format dataframe.
  #'
  #' @param module_enzymes A named list of modules b enzymes b ko sets.
  #' @return A tibble with columns: md, enzyme, ko_set, ko
  #' @export
  expand_enzyme_ko_df <- function(module_enzymes) {
    purrr::imap_dfr(module_enzymes, function(md_entry, md) {
      purrr::imap_dfr(md_entry, function(ko_list, enzyme) {
        enzyme <- as.character(enzyme)
        purrr::map_dfr(ko_list, function(ko) {
          tibble::tibble(
            md = md,
            enzyme = enzyme,
            ko_set = paste(ko, collapse = ", "),
            ko = ko
          )
        })
      })
    })
  }
  
  #' Summarize Enzyme Annotations by Joining ko Metadata
  #'
  #' Joins enzyme ko combinations with ko metadata (e.g., ec, KO symbol, KO name) and summarizes
  #' annotations for each unique enzyme combination.
  #'
  #' @param df A dataframe produced by \code{expand_enzyme_ko_df()}.
  #' @param kegg_metadata A dataframe containing ko metadata with columns like \code{ko}, \code{rn}, \code{Equation}, \code{ec}, \code{symbol}, \code{name}.
  #' @param remove_na Logical; if TRUE (default), removes rows with \code{NA} in the \code{rn} column.
  #'
  #' @return A summarized dataframe grouped by \code{md}, \code{enzyme}, and \code{ko_set}.
  #'
  #' @keywords internal
  summarize_enzyme_annotations <- function(df, kegg_metadata, remove_na = TRUE) {
    result <- df %>%
      tidyr::unnest_longer(ko) %>%
      dplyr::left_join(kegg_metadata, by = "ko", relationship = "many-to-many") %>%
      dplyr::distinct(ko, md, enzyme, ko_set, rn, eq, ec, symbol, name) %>%
      dplyr::group_by(md, enzyme, ko_set, rn, eq) %>%
      dplyr::summarise(
        ko =  paste(unique(na.omit(ko)), collapse = ", "),
        ec     = paste(unique(na.omit(ec)), collapse = ", "),
        symbol = paste(unique(na.omit(symbol)), collapse = ", "),
        name   = paste(unique(na.omit(name)), collapse = ", "),
        .groups = "drop"
      ) %>%
      dplyr::filter(ko_set == ko) %>%
      dplyr::select(-ko)
    
    if (remove_na) {
      result <- dplyr::filter(result, !is.na(rn))
    }
    
    result %>% dplyr::arrange(md, enzyme, rn)
  }
  
  
  #' Collapse KO Combinations Across Shared Reaction Mappings
  #'
  #' Collapses enzyme combinations that map to the same KO set and reaction equation,
  #' aggregating all associated modules into a single semicolon-delimited string.
  #'
  #' @param enzyme_summary A dataframe produced by \code{summarize_enzyme_annotations()}.
  #'
  #' @return A collapsed dataframe with unique ko combinations and associated reactions.
  #'
  #' @keywords internal
  collapse_by_ko_set <- function(enzyme_summary) {
    enzyme_summary %>%
      dplyr::select(-enzyme) %>%
      dplyr::group_by(ko_set, rn, eq, ec, symbol, name) %>%
      dplyr::summarise(
        md = paste(sort(unique(na.omit(md))), collapse = ", "),
        .groups   = "drop"
      ) %>%
      dplyr::arrange(md, rn)
  }
  
# === Extract subunits from KEGG names ===
  #' Extract Optional KO IDs from a KEGG module Definition
  #'
  #' Finds KO IDs marked as optional (i.e., prefixed with '-') in the KEGG module definition.
  #' This includes both individual entries like '-K00531' and grouped entries like '-(K00242,K18859)'.
  #'
  #' @param definition A character string representing a KEGG module definition.
  #'
  #' @return A character vector of optional (inhibitory) KO IDs (e.g., "K00531", "K00242", "K18859").
  #' @export
  get_optional_kos <- function(definition) {
    # Match individual ko entries like -K00531
    single_kos <- stringr::str_extract_all(definition, "-K\\d{5}")[[1]] %>%
      gsub("-", "", x = .)
    
    # Match grouped ko entries like -(K00242,K18859)
    group_kos <- stringr::str_extract_all(definition, "-\\([^()]*?\\)")[[1]] %>%
      gsub("[-()]", "", .) %>%                # Remove '-', '(', ')'
      strsplit(split = ",") %>%              # Split into individual KO IDs
      unlist()
    
    sort(unique(c(single_kos, group_kos)))
  }
  
  #' Get All Complete ko Combinations for Each Enzyme
  #'
  #' For each Enzyme group, this function identifies all combinations of KO IDs
  #' that together span one option for each required Subunit.
  #'
  #' @param df A data.frame with columns: Enzyme, Subunit, Option, ko.
  #'        The ko can contain comma-separated values.
  #'
  #' @return A named list, each element containing a data.frame of ko sets forming a complete enzyme.
  #' @export
  get_complete_enzyme_combinations <- function(df) {
    result <- list()
    
    # Split the input data by enzyme
    enzyme_list <- split(df, df$Enzyme)
    
    for (enzyme in names(enzyme_list)) {
      enzyme_df <- enzyme_list[[enzyme]]
      
      # Split comma-separated KO IDs and create one row per KO ID
      enzyme_df <- enzyme_df %>%
        dplyr::mutate(ko = stringr::str_split(ko, ",\\s*")) %>%
        tidyr::unnest(ko)
      
      # Group ko options per Subunit and Option
      grouped <- enzyme_df %>%
        dplyr::group_by(Subunit, Option) %>%
        dplyr::summarise(ko_set = list(ko), .groups = "drop")
      
      # Split into a list: one element per Subunit containing options (each a vector of KO IDs)
      subunit_options <- grouped %>%
        split(.$Subunit) %>%
        purrr::map(~ .x$ko_set)
      
      # Generate all combinations: one option per Subunit
      combos <- purrr::cross(subunit_options)
      
      # Convert each combo (list of ko vectors) into one flat ko vector
      ko_sets <- purrr::map(combos, ~ unique(unlist(.x)))
      
      result[[paste0("Enzyme_", enzyme)]] <- ko_sets
    }
    
    return(result)
  }
  
  #' Extract Subunit Identifier from KEGG Name
  #'
  #' This function extracts the subunit name from KEGG gene or enzyme names.
  #' It handles cases like "subunit alpha" and "large subunit", removes EC numbers,
  #' and returns the word that either follows or precedes "subunit". Users can also
  #' specify additional words to include in the match before "subunit" (e.g., "membrane").
  #'
  #' @param vec A character vector of KEGG gene or enzyme names.
  #' @param extra_words Optional character vector of words to include before the subunit identifier
  #'                    (e.g., c("membrane", "cytochrome")). Default is NULL.
  #' @return A character vector of extracted subunit identifiers, or NA if none found.
  #' @export
  extract_subunits <- function(
      vec, 
      extra_words = c("membrane", "cytochrome")
  ) {
    vec_clean <- stringr::str_remove(vec, "\\[ec:[^\\]]+\\]")
    subunit_prefix <- if (!is.null(extra_words) && length(extra_words) > 0) {
      paste0("(?:(?:", paste(extra_words, collapse = "|"), ")\\s+)?")
    } else {
      ""
    }
    subunit <- dplyr::case_when(
      stringr::str_detect(vec_clean, "(?i)subunit\\s+[\\w/-]+") ~
        stringr::str_remove(
          stringr::str_extract(vec_clean, "(?i)subunit\\s+[\\w/-]+"),
          "(?i)subunit\\s+"
        ),
      stringr::str_detect(vec_clean, paste0("(?i)", subunit_prefix, "[\\w/-]+\\s+subunit")) ~
        stringr::str_remove(
          stringr::str_extract(
            vec_clean,
            paste0("(?i)", subunit_prefix, "[\\w/-]+(?=\\s+subunit)")
          ),
          "^\\s*"
        ),
      TRUE ~ NA_character_
    )
    return(subunit)
  }
  
  #' Extract Base Name from KEGG Entry
  #'
  #' Removes EC number and subunit phrase (either "subunit ID" or "ID subunit"), 
  #' and optionally removes specified trailing characters or patterns.
  #'
  #' @param name A character vector of KEGG names.
  #' @param subunit_id A character vector of subunit identifiers (same length as `name`).
  #' @param remove_trailing_char Character vector of patterns to remove from the end.
  #'
  #' @return A character vector of base names.
  #' @export
  get_base_name <- function(name, 
                            subunit_id, 
                            remove_trailing_char = c(",", "I/II/III", "I/III", "II", "III")) {
    name_no_ec <- stringr::str_remove(name, "\\[ec:[^\\]]+\\]")
    base_name <- purrr::map2_chr(name_no_ec, subunit_id, ~ {
      if (!is.na(.y)) {
        str <- stringr::str_remove(
          .x,
          paste0("(?i)(subunit\\s+\\b", .y, "\\b|\\b", .y, "\\b\\s+subunit).*")
        )
      } else {
        str <- stringr::str_remove(.x, "(?i)subunit.*")
      }
      stringr::str_squish(str)
    })
    if (!is.null(remove_trailing_char) && length(remove_trailing_char) > 0) {
      for (pattern in remove_trailing_char) {
        base_name <- stringr::str_remove(base_name, paste0(stringr::fixed(pattern), "\\s*$"))
      }
    }
    
    return(base_name)
  }
  
  #' Get Base Symbol from KO symbol
  #'
  #' Removes the last letter from a KO symbol (e.g., "oadA" becomes "oad").
  #'
  #' @param symbol A character vector of KO symbols.
  #' @return A character vector with the last letter removed.
  #' @export
  get_base_symbol <- function(symbol) {
    stringr::str_replace(symbol, ".{1}$", "")
  }
  
  #' Identify and Clean Non-md KO IDs
  #'
  #' Filters  KEGG metadata to exclude KO IDs already present in modules or marked as optional.
  #'
  #' @param kegg_metadata A dataframe with column ko.
  #' @param module_enzymes A dataframe of module enzymes with ko_set.
  #' @param module_metadata A named list of module metadata, including definitions.
  #' @return A dataframe filtered to exclude module and optional kos.
  filter_nonmodule_kos <- function(kegg_metadata, module_enzymes, module_metadata) {
    md_kos <- module_enzymes %>%
      dplyr::select(ko_set) %>%
      tidyr::separate_rows(ko_set, sep = ", ") %>%
      dplyr::distinct(ko_set) %>%
      dplyr::pull(ko_set)
    
    optional_kos <- purrr::map(module_metadata, function(x) {
      if (!is.null(x$definition)) get_optional_kos(x$definition) else character(0)
    }) %>% unlist() %>% unique()
    
    excluded_kos <- c(md_kos, optional_kos)
    kegg_metadata %>% dplyr::filter(!ko %in% excluded_kos)
  }
  
  #' Get Elements that Cover of All Characters in a Vector
  #'
  #' This function takes a vector and finds combinations of elements that include 
  #' the set of all characters.  For example, in the vector c("A", "B", "A/B", "C"), 
  #' it would find that indices c("1", "2", "4") or c("3", "4") collectively cover 
  #' all characters ("A", "B", and "C"). It can return either all valid combinations 
  #' or only minimal and/or shortest ones. 
  #'
  #' @param vec A character vector where each element contains one or more
  #'   chars separated by `sep`, representing the chars available at that position.
  #' @param shortest_only Logical; if TRUE, returns only the shortest combinations
  #'   that cover all chars. Default is FALSE (returns all minimal combinations).
  #' @param minimal_only Logical. If \code{TRUE}, only minimal combinations 
  #'  (i.e., those not supersets of smaller valid combinations) are returned. 
  #'  Defaults to \code{TRUE}.
  #' @param sep Separator used to split multiple chars in an entry. Default is "/".
  #'
  #' @return A list of minimal index combinations (each as a vector of positions)
  #'   that together cover all unique chars in the input.
  #'
  #' @examples
  #' get_complete_set(c("A", "B", "A/B", "C"), sep = "/", shortest_only = FALSE)
  #'
  #' @export
  get_complete_set <- function(vec, shortest_only = FALSE, minimal_only = TRUE, sep = "/") {
    df <- tibble::tibble(index = seq_along(vec), char = vec)
    df_expanded <- df %>%
      dplyr::mutate(char = stringr::str_split(char, stringr::fixed(sep))) %>%
      tidyr::unnest(char) %>%
      dplyr::mutate(char = stringr::str_trim(char))
    all_char <- unique(df_expanded$char)
    all_indices <- unique(df_expanded$index)
    valid_combinations <- list()
    for (i in seq_along(all_indices)) {
      combos <- combn(all_indices, i, simplify = FALSE)
      for (combo in combos) {
        chars_in_combo <- df_expanded %>%
          dplyr::filter(index %in% combo) %>%
          dplyr::pull(char) %>%
          unique()
        if (setequal(chars_in_combo, all_char)) {
          valid_combinations <- append(valid_combinations, list(sort(combo)))
        }
      }
    }
    if (minimal_only && length(valid_combinations) > 1) {
      is_minimal <- rep(TRUE, length(valid_combinations))
      for (i in seq_along(valid_combinations)) {
        for (j in seq_along(valid_combinations)) {
          if (i != j &&
              all(valid_combinations[[j]] %in% valid_combinations[[i]]) &&
              length(valid_combinations[[j]]) < length(valid_combinations[[i]])) {
            is_minimal[i] <- FALSE
            break
          }
        }
      }
      valid_combinations <- valid_combinations[is_minimal]
    }
    if (shortest_only && length(valid_combinations) > 0) {
      min_length <- min(purrr::map_int(valid_combinations, length))
      valid_combinations <- valid_combinations[purrr::map_lgl(valid_combinations, ~ length(.x) == min_length)]
    }
    return(valid_combinations)
  }
  
  #' Format ko Enzymes Not in Modules
  #'
  #' Builds enzyme tables from kos not found in any module definition by grouping based on subunit annotations.
  #'
  #' @param df A dataframe of KEGG metadata filtered to exclude module-associated KO IDs.
  #' @return A list with enzyme groupings, ready to pass to get_complete_enzyme_combinations().
  format_nonmodule_enzymes <- function(df) {
    df <- df %>%
      dplyr::mutate(
        row_index = dplyr::row_number(),
        subunit_id = extract_subunits(name),
        has_subunit = !is.na(subunit_id),
        name_base = get_base_name(name, subunit_id),
        symbol_base = dplyr::if_else(
          has_subunit,
          get_base_symbol(symbol),
          NA_character_
        )
      )
    
    df_subunits <- df %>% dplyr::filter(has_subunit)
    df_non_subunits <- df %>% dplyr::filter(!has_subunit)
    
    enzyme <- 1
    
    # Only build subunit table if there are rows
    subunit_table <- if (nrow(df_subunits) > 0) {
      df_subunits_grouped <- df_subunits %>%
        dplyr::group_by(eq, name_base, symbol_base) %>%
        dplyr::group_split()
      
      purrr::map_dfr(df_subunits_grouped, function(group) {
        vec <- group$subunit_id
        sets <- get_complete_set(vec, shortest_only = TRUE)
        if (length(sets) == 0) return(NULL)
        purrr::map_dfr(sets, function(idx) {
          subset <- group[idx, ]
          result <- tibble::tibble(
            Enzyme = enzyme,
            Subunit = LETTERS[seq_along(idx)],
            Option = "A",
            ko = subset$ko
          )
          enzyme <<- enzyme + 1
          result
        })
      })
    } else {
      tibble::tibble(Enzyme = integer(), Subunit = character(), Option = character(), ko = character())
    }
    
    # Only build non-subunit table if there are rows
    non_subunit_table <- if (nrow(df_non_subunits) > 0) {
      df_non_subunits %>%
        dplyr::arrange(row_index) %>%
        dplyr::mutate(Enzyme = seq(enzyme, enzyme + dplyr::n() - 1)) %>%
        dplyr::transmute(
          Enzyme,
          Subunit = "A",
          Option = "A",
          ko = ko
        )
    } else {
      tibble::tibble(Enzyme = integer(), Subunit = character(), Option = character(), ko = character())
    }
    
    other_enzyme_table <- dplyr::bind_rows(subunit_table, non_subunit_table) %>%
      dplyr::mutate(Enzyme = as.integer(Enzyme)) %>%
      dplyr::arrange(Enzyme)
    
    list("NA" = other_enzyme_table %>%
           dplyr::select(Enzyme, Subunit, Option, ko) %>%
           dplyr::mutate(
             Enzyme = as.integer(Enzyme),
             Subunit = as.character(Subunit),
             Option = as.character(Option),
             ko = as.character(ko)
           ) %>%
           dplyr::arrange(Enzyme, Subunit))
  }
  
# === Manually Correct Errors in Network ===  
  #' Combine Rows in Network by KO ID
  #'
  #' Combines rows in a network by KO ID. Each group is matched
  #' independently from the original data to prevent row loss across groups.
  #'
  #' @param df A dataframe with ko and related annotation columns.
  #' @param ko_groups A list of character vectors of KO IDs to be combined. Required.
  #' @param keep_all_ko Logical; if TRUE, retains all KO IDs from matched rows in the merged ko. 
  #'                     If FALSE (default), retains only the KO IDs from the group.
  #' @param rn_match Optional character vector of `rn` values to restrict merging to. Default is NULL (no restriction).
  #'
  #' @return A dataframe with specified KO groups merged into single rows.
  #' @export
  combine_ko <- function(df, ko_groups, keep_all_ko = TRUE, rn_match = NULL) {
    if (missing(ko_groups) || !is.list(ko_groups) || length(ko_groups) == 0) {
      stop("`ko_groups` must be a non-empty list of KO ID vectors.")
    }
    
    df <- df %>% dplyr::mutate(.row = dplyr::row_number())
    
    merged_rows <- list()
    matched_indices <- c()
    
    for (group in ko_groups) {
      group <- sort(group)
      
      df_matched <- df %>%
        dplyr::mutate(ko_vec = strsplit(ko, ",\\s*")) %>%
        dplyr::filter(
          purrr::map_lgl(ko_vec, ~ any(.x %in% group)) &
            (is.null(rn_match) | rn %in% rn_match)
        )
      
      if (nrow(df_matched) == 0) next
      
      matched_indices <- c(matched_indices, df_matched$.row)
      
      merged <- df_matched %>%
        dplyr::group_by(eq) %>%
        dplyr::summarise(
          ko = if (keep_all_ko) {
            paste(sort(unique(unlist(strsplit(ko, ",\\s*")))), collapse = ", ")
          } else {
            paste(group, collapse = ", ")
          },
          rn        = paste(unique(rn), collapse = ", "),
          way       = paste(unique(way), collapse = ", "),
          map       = paste(unique(map), collapse = ", "),
          ec        = paste(unique(na.omit(ec)), collapse = ", "),
          symbol    = paste(unique(unlist(strsplit(symbol, ",\\s*"))), collapse = ", "),
          name      = paste(unique(unlist(strsplit(name, ",\\s*"))), collapse = ", "),
          md        = paste(unique(unlist(strsplit(md, ",\\s*"))), collapse = ", "),
          .groups   = "drop"
        )
      
      merged_rows[[length(merged_rows) + 1]] <- merged
    }
    
    merged_df <- dplyr::bind_rows(merged_rows)
    
    final_df <- df %>%
      dplyr::filter(!.row %in% matched_indices) %>%
      dplyr::select(-.row) %>%
      dplyr::bind_rows(merged_df) %>%
      dplyr::arrange(rn, ko)
    
    return(final_df)
  }
  
  #' Add a KO (and metadata) to a Specific Reaction Row
  #'
  #' Adds KO, symbol, and name from a donor row to a matched target row. 
  #' The recipient row is identified using ko_match and/or rn_match.
  #'
  #' @param df A dataframe with columns: ko, symbol, name, rn, etc.
  #' @param ko_match Optional string to match in the target `ko` column.
  #' @param rn_match Optional string to match in the target `rn` column.
  #' @param ko_to_add The KO ID to add, taken from a source row that already contains it.
  #'
  #' @return A dataframe with the KO, name, and symbol added to the matched row.
  #' @export
  add_ko <- function(df, ko_match = NULL, rn_match = NULL, ko_to_add) {
    if (missing(ko_to_add) || is.null(ko_to_add)) {
      stop("`ko_to_add` must be specified.")
    }
    
    # Find source row
    source_row <- df %>%
      dplyr::filter(ko == ko_to_add)
    
    if (nrow(source_row) == 0) stop("KO to add not found in any row.")
    if (nrow(source_row) > 1) stop("KO to add matched multiple rows. Ensure it is unique.")
    
    source_ko     <- ko_to_add
    source_name   <- unlist(strsplit(source_row$name, ",\\s*"))
    source_symbol <- unlist(strsplit(source_row$symbol, ",\\s*"))
    
    # Add row IDs for identification
    df$row_id <- seq_len(nrow(df))
    
    # Apply filtering based on non-null ko_match and rn_match
    target_rows <- df
    if (!is.null(ko_match)) {
      target_rows <- target_rows %>% dplyr::filter(grepl(ko_match, ko, fixed = TRUE))
    }
    if (!is.null(rn_match)) {
      target_rows <- target_rows %>% dplyr::filter(grepl(rn_match, rn, fixed = TRUE))
    }
    
    if (nrow(target_rows) != 1) {
      stop("Target row must match exactly one entry (based on ko_match and/or rn_match).")
    }
    
    target_idx <- target_rows$row_id
    target_row <- df[target_idx, ]
    
    # Split values
    target_ko     <- unlist(strsplit(target_row$ko, ",\\s*"))
    target_name   <- unlist(strsplit(target_row$name, ",\\s*"))
    target_symbol <- unlist(strsplit(target_row$symbol, ",\\s*"))
    
    # Skip if already present
    if (source_ko %in% target_ko) {
      df$row_id <- NULL
      return(df)
    }
    
    # Add new values
    new_ko     <- paste(c(target_ko, source_ko), collapse = ", ")
    new_name   <- paste(c(target_name, source_name[1]), collapse = ", ")
    new_symbol <- paste(c(target_symbol, source_symbol[1]), collapse = ", ")
    
    # Update
    df$ko[target_idx]     <- new_ko
    df$name[target_idx]   <- new_name
    df$symbol[target_idx] <- new_symbol
    
    # Clean up NA
    df$ko <- gsub("^NA,\\s*", "", df$ko)
    df$name <- gsub("^NA,\\s*", "", df$name)
    df$symbol <- gsub("^NA,\\s*", "", df$symbol)
    
    df$row_id <- NULL
    return(df)
  }
  
  #' Split Rows in Network by KO ID
  #'
  #' Splits rows by comma-delimited KOs in `ko`, and aligns any number of metadata columns.
  #' Only rows that match the required KO filter (and optionally the RN filter) will be split.
  #'
  #' @param df A dataframe with columns like `ko`, `name`, and `symbol`.
  #' @param split_cols Character vector of column names (besides `ko`) to split. Default: c("name", "symbol").
  #' @param ko_match Character string to match in `ko`. Required.
  #' @param rn_match Optional character string to match in `rn`.
  #'
  #' @return A dataframe where matched rows are split one per KO.
  #' @export
  split_ko <- function(df,
                       split_cols = c("name", "symbol"),
                       ko_match,
                       rn_match = NULL) {
    
    # Require ko_match
    if (missing(ko_match) || is.null(ko_match)) {
      stop("`ko_match` must be specified.")
    }
    
    # Match KO (required)
    match_ko <- grepl(ko_match, df$ko, fixed = TRUE)
    
    # Match RN (optional)
    if (!is.null(rn_match) && "rn" %in% names(df)) {
      match_rn <- grepl(rn_match, df$rn, fixed = TRUE)
    } else {
      match_rn <- TRUE  # No filtering by rn if not specified
    }
    
    # Final match condition: must match KO and, if specified, RN
    match_rows <- match_ko & match_rn
    
    df_to_split <- df[match_rows, , drop = FALSE]
    df_to_keep  <- df[!match_rows, , drop = FALSE]
    
    if (nrow(df_to_split) == 0) return(df)
    
    df_split <- df_to_split %>%
      dplyr::mutate(row_id = dplyr::row_number()) %>%
      purrr::pmap_dfr(function(row_id, ...) {
        row <- list(...)
        
        ko_vec <- strsplit(row$ko, ",\\s*")[[1]]
        split_data <- lapply(split_cols, function(col) strsplit(row[[col]], ",\\s*")[[1]])
        names(split_data) <- split_cols
        
        n <- length(ko_vec)
        if (!all(vapply(split_data, length, integer(1)) == n)) {
          stop("Mismatch in number of KOs and split column values in row: ", row_id)
        }
        
        tibble::tibble(
          ko = ko_vec,
          !!!split_data,
          !!!row[setdiff(names(row), c("ko", split_cols, "row_id"))]
        )
      })
    
    dplyr::bind_rows(df_to_keep, df_split)
  }
  
  
  #' Remove a KO from a Specific ko Row
  #'
  #' Removes a specific KO from the `ko` in a matched row. If the KO is found, it is removed.
  #' The row is updated in place, and no new row is added.
  #'
  #' @param df A dataframe with a `ko` column (comma-separated KO IDs).
  #' @param ko Character. Exact `ko` string to match (e.g., "K00236, K25801, K00234, K00235").
  #' @param ko_remove Character. The KO to remove from the set (e.g., "K00236").
  #'
  #' @return A dataframe with the KO removed from the `ko`.
  #' @export
  remove_ko <- function(df, ko, ko_remove) {
    row_index <- which(trimws(df$ko) == trimws(ko))
    
    if (length(row_index) != 1) {
      stop("Could not uniquely identify row with that ko.")
    }
    
    ko_list <- unlist(strsplit(df$ko[row_index], ",\\s*"))
    
    if (!(ko_remove %in% ko_list)) {
      warning("KO to remove not found in the specified ko.")
      return(df)
    }
    
    # Remove KO and reconstruct ko
    updated_ko_list <- setdiff(ko_list, ko_remove)
    
    # If removing would result in empty ko, remove the whole row
    if (length(updated_ko_list) == 0) {
      df <- df[-row_index, ]
    } else {
      df$ko[row_index] <- paste(updated_ko_list, collapse = ", ")
    }
    
    return(df)
  }
  
  #' Delete Rows in Network by KO ID
  #'
  #' Removes rows from the network where ko matches
  #' any KO ID in the provided vector exactly.
  #'
  #' @param df A dataframe with a `ko` column.
  #' @param kos A character vector of KO IDs (e.g., c("K02164", "K02448")).
  #'
  #' @return A filtered dataframe excluding rows with exact ko matches.
  #' @export
  delete_ko <- function(df, kos) {
    df %>%
      dplyr::filter(!ko %in% kos)
  }
  
  #' Add a Reaction to the Network
  #'
  #' This helper function appends a new row in the network with information for 
  #' a new reaction.
  #'
  #' @param df A tibble (e.g., `enzyme_combined`) to which the new row will be added.
  #' @param ko Character. KEGG Orthology combinations (e.g., "K00134,K00135").
  #' @param rn Character. reaction ID (e.g., "R00001").
  #' @param eq Character. The biochemical equation.
  #' @param ec Character. EC number (e.g., "1.1.1.1").
  #' @param symbol Character. KO symbol (e.g., "adh").
  #' @param name Character. KO name or description.
  #' @param md Character. Associated KEGG module ID(s), comma-separated if multiple.
  #'
  #' @return A tibble with the new row appended.
  #' @export
  add_rn <- function(df, ko, rn, eq, ec, symbol, name, md) {
    new_row <- tibble::tibble(
      ko = ko,
      rn = rn,
      eq = eq,
      ec = ec,
      symbol = symbol,
      name = name,
      md = md
    )
    dplyr::bind_rows(df, new_row)
  }
  
  #' Replace a Reaction Equation in a Dataframe
  #'
  #' This function finds a row (or rows) in a dataframe where the `eq` column matches 
  #' a specified value, and replaces it with one or more new versions. Optionally, it can
  #' also update the `ec` and `rn` columns for the new equations, and restrict replacement
  #' to rows where a `ko_match` string is found in the `ko` column.
  #'
  #' If multiple new equations are provided, the matching row is duplicated for each.
  #'
  #' @param df A dataframe containing a column called `eq`.
  #' @param old_eq A character string representing the equation to replace.
  #' @param new_eq A character vector of one or more replacement equations.
  #' @param new_ec Optional character vector of new EC numbers, same length as `new_eq`.
  #' @param new_rn Optional character vector of new reaction IDs, same length as `new_eq`.
  #' @param ko_match Optional character string to restrict replacement to rows where `ko_match`
  #'        is present in the `ko` column (comma-delimited KO IDs).
  #'
  #' @return A modified dataframe with the specified equation replaced by the new versions.
  #' @export
  replace_equation <- function(df, old_eq, new_eq, new_ec = NULL, new_rn = NULL, ko_match = NULL) {
    if (!"eq" %in% names(df)) stop("The dataframe must contain a column named 'eq'.")
    
    # Match rows based on eq and optional ko_match
    match_eq <- df$eq == old_eq
    match_ko <- if (!is.null(ko_match) && "ko" %in% names(df)) {
      grepl(ko_match, df$ko, fixed = TRUE)
    } else {
      TRUE
    }
    
    rows_to_replace <- which(match_eq & match_ko)
    
    if (length(rows_to_replace) == 0) return(df)
    
    if (!is.null(new_ec) && length(new_ec) != length(new_eq)) {
      stop("Length of new_ec must match length of new_eq.")
    }
    
    if (!is.null(new_rn) && length(new_rn) != length(new_eq)) {
      stop("Length of new_rn must match length of new_eq.")
    }
    
    replacement_blocks <- purrr::map(rows_to_replace, function(i) {
      row <- df[i, , drop = FALSE]
      expanded <- row[rep(1, length(new_eq)), ]
      expanded$eq <- new_eq
      if (!is.null(new_ec)) expanded$ec <- new_ec
      if (!is.null(new_rn)) expanded$rn <- new_rn
      expanded
    })
    
    df_remaining <- df[-rows_to_replace, ]
    dplyr::bind_rows(df_remaining, dplyr::bind_rows(replacement_blocks))
  }

  #' Fill in Reaction Info (eq, rn, ec, ko) by Match
  #'
  #' This function fills in missing values of `eq`, `rn`, `ec`, and/or `ko` in-place for matched rows.
  #' Matches are based on `ko`, `name`, `eq`, `ec`, or `rn` columns.
  #'
  #' @param df A dataframe with columns like `eq`, `rn`, `ec`, `ko`, and `name`.
  #' @param ko_match Optional string to match in `ko` (comma-delimited KO IDs).
  #' @param name_match Optional string to match in `name`.
  #' @param eq_match Optional string to match in `eq`.
  #' @param ec_match Optional string to match in `ec`.
  #' @param rn_match Optional string to match in `rn`.
  #' @param new_eq Optional character value to replace missing `eq`.
  #' @param new_rn Optional character value to replace missing `rn`.
  #' @param new_ec Optional character value to replace missing `ec`.
  #' @param new_ko Optional character value to replace missing `ko`.
  #'
  #' @return A dataframe with updated rows.
  #' @export
  #'
  #' @examples
  #' fill_info(df, ko_match = "K14086", new_eq = "H+ <=> H2", new_ec = "7.2.1.A", new_rn = "R00019A", new_ko = "K99999")
  fill_info <- function(df,
                        ko_match = NULL,
                        name_match = NULL,
                        eq_match = NULL,
                        ec_match = NULL,
                        rn_match = NULL,
                        new_eq = NULL,
                        new_rn = NULL,
                        new_ec = NULL,
                        new_ko = NULL) {
    if (!any(c(!is.null(new_eq), !is.null(new_rn), !is.null(new_ec), !is.null(new_ko)))) {
      stop("You must provide at least one of `new_eq`, `new_rn`, `new_ec`, or `new_ko`.")
    }
    
    # Identify matching rows
    match_ko   <- if (!is.null(ko_match)) grepl(ko_match, df$ko, fixed = TRUE) else FALSE
    match_name <- if (!is.null(name_match)) grepl(name_match, df$name, fixed = TRUE) else FALSE
    match_eq   <- if (!is.null(eq_match) && "eq" %in% names(df)) grepl(eq_match, df$eq, fixed = TRUE) else FALSE
    match_ec   <- if (!is.null(ec_match) && "ec" %in% names(df)) grepl(ec_match, df$ec, fixed = TRUE) else FALSE
    match_rn   <- if (!is.null(rn_match) && "rn" %in% names(df)) grepl(rn_match, df$rn, fixed = TRUE) else FALSE
    
    match_any <- match_ko | match_name | match_eq | match_ec | match_rn
    
    # Fill or overwrite
    df <- dplyr::mutate(df,
                        eq = ifelse(match_any & !is.null(new_eq), new_eq, eq),
                        rn = ifelse(match_any & !is.null(new_rn), new_rn, rn),
                        ec = ifelse(match_any & !is.null(new_ec), new_ec, ec),
                        ko = ifelse(match_any & !is.null(new_ko), new_ko, ko)
    )
    
    return(df)
  }
  
  #' Remove a Reaction from the Network
  #'
  #' This helper function removes a row from the network
  #' based on an exact match to one or more specified columns. Only non-NULL
  #' arguments are used in the match.
  #'
  #' @param df A tibble (nework) to filter.
  #' @param ko Optional character. Exact value to match in `ko`.
  #' @param rn Optional character. Exact value to match in `rn`.
  #' @param eq Optional character. Exact value to match in `eq`.
  #' @param ec Optional character. Exact value to match in `ec`.
  #' @param symbol Optional character. Exact value to match in `symbol`.
  #' @param name Optional character. Exact value to match in `name`.
  #' @param md Optional character. Exact value to match in `md`.
  #'
  #' @return A tibble with matching row(s) removed.
  #' @export
  remove_rn <- function(df,
                        ko = NULL,
                        rn = NULL,
                        eq = NULL,
                        ec = NULL,
                        symbol = NULL,
                        name = NULL,
                        md = NULL) {
    conditions <- list(
      if (!is.null(ko)) !is.na(df$ko) & df$ko == ko else TRUE,
      if (!is.null(rn))     !is.na(df$rn)     & df$rn     == rn     else TRUE,
      if (!is.null(eq))     !is.na(df$eq)     & df$eq     == eq     else TRUE,
      if (!is.null(ec))     !is.na(df$ec)     & df$ec     == ec     else TRUE,
      if (!is.null(symbol)) !is.na(df$symbol) & df$symbol == symbol else TRUE,
      if (!is.null(name))   !is.na(df$name)   & df$name   == name   else TRUE,
      if (!is.null(md))     !is.na(df$md)     & df$md     == md     else TRUE
    )
    
    matches <- Reduce(`&`, conditions)
    df[!matches, , drop = FALSE]
  }

# === Configure and Save Networks ===  
  #' Configure Reactions for Metabolic Network
  #'
  #' This function extracts and orders reactions from the master network based on
  #' the specified modules and a reaction configuration table.
  #'
  #' @param master_network A dataframe of the master network, containing name, equation, way, ec, ko, md, rn, and symbol
  #' @param mds A character vector specifying the modules to keep in the configured network and the order they should appear in.         
  #' @param reaction_config A dataframe that specifies which reactions belong to modules and their direction, containing `rn`, `md`, and `way`
  #'
  #' @return A tibble containing only reactions from the specified modules, merged with full metadata
  #'         from `master_network`, and ordered by `mds` and appearance in `master_network`.
  #'
  #' @export
  configure_network <- function(master_network, mds, reaction_config) {
    configured_list <- vector("list", length(mds))
    
    # Save original column order
    master_cols <- names(master_network)
    
    # Rename original 'md' to 'mdk' in column order
    col_order <- ifelse(master_cols == "md", "mdk", master_cols)
    
    for (i in seq_along(mds)) {
      md_i <- mds[i]
      
      subset <- reaction_config %>%
        dplyr::filter(md == md_i) %>%
        dplyr::select(rn, md, way) %>%
        dplyr::left_join(master_network, by = "rn", suffix = c("", ".master")) %>%
        dplyr::rename(mdk = md.master) %>%
        dplyr::select(-way.master) %>%
        dplyr::semi_join(master_network, by = "rn") %>%
        dplyr::distinct()
      
      # Move md (from config) to the front; preserve original col order otherwise
      subset <- subset %>%
        dplyr::relocate(md, .before = dplyr::everything()) %>%
        dplyr::select(c("md", col_order))
      
      configured_list[[i]] <- subset
    }
    
    configured_network <- dplyr::bind_rows(configured_list)
    return(configured_network)
  }
  
  #' Save a Network to CSV
  #'
  #' @param network A data frame of the filtered network.
  #' @param name A string used as the file name (e.g., "glucose_fermentation" -> "glucose_fermentation.csv").
  #' @param directory The directory to save the file in.
  #'
  #' @return Invisibly returns the path to the saved file.
  save_network <- function(network, name, directory) {
    out_path <- file.path(directory, paste0("", name, ".csv"))
    readr::write_csv(network, out_path)
    message("Saved: ", out_path)
    invisible(out_path)
  }
  
  #' Save a Data Frame to CSV Using Its Object Name
  #'
  #' This helper function saves a data frame as a CSV file using the name of the object
  #' as the file name. The file is saved to a subdirectory (default: "data") inside a specified directory.
  #'
  #' @param obj A data frame or tibble to be saved.
  #' @param dir A character string specifying the base directory where the file should be saved.
  #'
  #' @return Invisibly returns the file path to which the CSV was saved.
  #'
  #' @examples
  #' \dontrun{
  #' save_csv_named(glucose_fermentation, dir = network_directory)
  #' save_csv_named(methanogenesis, dir = network_directory)
  #' }
  #'
  #' @export
  object_to_csv <- function(obj, dir = ".") {
    obj_name <- deparse(substitute(obj))
    file_path <- file.path(dir, paste0(obj_name, ".csv"))
    readr::write_csv(obj, file_path)
    invisible(file_path)
  }
  
  #' Clean and Collapse Character Values
  #'
  #' This helper function removes a specified string value (default = "NA") from a character vector
  #' if other values are present. It does not remove real NA values (i.e., NA_character_). It then
  #' collapses the remaining values into a comma-delimited string.
  #'
  #' @param x A character vector to clean and collapse.
  #' @param to_remove A string value to remove if other values are present. Default is "NA".
  #'
  #' @return A single character string, with values cleaned and collapsed.
  #'
  #' @examples
  #' clean_and_collapse(c("NA", "M00001"))
  #' # Returns: "M00001"
  #'
  #' clean_and_collapse(c("NA"))
  #' # Returns: "NA"
  #'
  #' clean_and_collapse(c(NA, "NA", "None"))
  #' # Returns: "NA, None"
  #'
  #' clean_and_collapse(c("None", "M00001"))
  #' # Returns: "None, M00001"
  #'
  #' @export
  clean_and_collapse <- function(x, to_remove = "NA") {
    vals <- unique(x)
    # Do not remove actual NA (NA_character_)
    na_locs <- is.na(vals)
    keep_vals <- vals[!na_locs]
    
    if (to_remove %in% keep_vals && length(keep_vals) > 1) {
      keep_vals <- keep_vals[keep_vals != to_remove]
    }
    
    # Re-add actual NA values (if any)
    keep_vals <- c(keep_vals, vals[na_locs])
    
    stringr::str_c(keep_vals, collapse = ", ")
  }
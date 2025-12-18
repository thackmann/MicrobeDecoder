# Define Functions for Obtaining Data from LPSN
# These are functions specific to this data source
# Author: Timothy Hackmann
# Date: 4 April 2025

  #' Extract Links from Web Page Body
  #'
  #' This function extracts all hyperlinks (URLs) from the body of a web page.
  #'
  #' @param body An HTML document object, typically returned by functions like `polite::scrape()` or `rvest::read_html()`.
  #' @param tag A character string specifying the HTML tag to search for. Default is `"a"`.
  #' @param attribute A character string specifying the attribute to extract. Default is `"href"`.
  #'
  #' @return A character vector of extracted links.
  #' @export
  #'
  #' @examples
  #' # Example usage:
  #' body <- polite::scrape(polite::bow("https://example.com"))
  #' links <- extract_links(body)
  #'
  extract_links <- function(body, tag = "a", attribute = "href") {
    # Extract specified attributes from the specified HTML tags
    links <- body %>%
      rvest::html_nodes(tag) %>%
      rvest::html_attr(attribute)
    
    return(links)
  }
  
  #' Download a FASTA file from a given link
  #'
  #' This function downloads a FASTA file from the provided URL and saves it to the specified file path.
  #' If the download fails, an error message is returned.
  #'
  #' @param fasta_link A character string containing the URL of the FASTA file to download.
  #' @param fp A character string specifying the file path where the downloaded FASTA file will be saved.
  #'
  #' @return A list with the following elements:
  #' \describe{
  #'   \item{success}{A logical value indicating whether the download was successful.}
  #'   \item{filepath}{The file path where the FASTA file was saved (NULL if unsuccessful).}
  #'   \item{message}{A message indicating the status of the operation (e.g., success or error).}
  #' }
  #'
  #' @examples
  #' # Example usage
  #' fasta_link <- "https://example.com/sample.fasta"
  #' fp <- "path/to/save/sample.fasta"
  #' result <- download_fasta_file(fasta_link, fp)
  #' print(result$message)
  #'
  #' @export
  download_fasta_file <- function(fasta_link, fp) {
    tryCatch({
      # Download and save the file
      fasta_file <- httr::GET(fasta_link)
      writeBin(httr::content(fasta_file, "raw"), fp)
      
      list(success = TRUE, filepath = fp, message = paste("Downloaded:", fp))
    }, error = function(e) {
      list(success = FALSE, filepath = NULL, message = paste("Error:", e$message))
    })
  }
  
  #' Combine DNAStringSet Objects
  #'
  #' This function combines a list of `DNAStringSet` objects into a single `DNAStringSet`.
  #'
  #' @param fasta_list A list of `DNAStringSet` objects to be combined.
  #'
  #' @return A combined `DNAStringSet` object containing all sequences and their corresponding IDs.
  #' @import Biostrings ShortRead
  #' @export
  #'
  #' @examples
  #' \dontrun{
  #' combined_fasta <- combine_fasta(fasta_list)
  #' }
  combine_fasta <- function(fasta_list) {
    # Combine sequences
    combined_reads <- do.call(c, lapply(fasta_list, ShortRead::sread))
    
    # Combine IDs
    combined_ids <- do.call(c, lapply(fasta_list, ShortRead::id))
    
    # Create a new ShortRead object
    ShortRead::ShortRead(sread = combined_reads, id = combined_ids)
  }
  
  #' Extract Taxonomic Ranks from Links
  #'
  #' This function extracts Genus, Family, and other taxonomic ranks from a
  #' vector of links. The links are from the LPSN and typically follow the
  #' format `"/rank/name"`, but full URLs such as
  #' `"https://lpsn.dsmz.de/rank/name"` are also supported.
  #'
  #' For infraspecific ranks (Species, Subspecies, Variety, Form), LPSN
  #' encodes the full name as a hyphen-separated slug, e.g.:
  #'   - `/species/thermosynechococcus-vestitus`
  #'   - `/variety/synechococcus-elongatus-vestitus`
  #'
  #' In these cases, this function returns only the final epithet
  #' (e.g. `"vestitus"`).
  #'
  #' @param phylogeny A character vector of links (relative paths or full URLs).
  #' @param ranks A character vector of ranks to extract (e.g.
  #'   `c("Genus", "Family", "Order", "Class", "Phylum", "Domain",
  #'      "Species", "Subspecies", "Variety", "Form")`).
  #'
  #' @return A named character vector with the extracted taxonomic names for
  #'   each rank. If a rank is not present in `phylogeny`, its value will be `NA`.
  #'
  #' @examples
  #' # Relative paths
  #' phylogeny_example <- c(
  #'   "/genus/abditibacterium",
  #'   "/family/abditibacteriaceae",
  #'   "/order/abditibacteriales",
  #'   "/class/abditibacteriia",
  #'   "/phylum/abditibacteriota",
  #'   "/domain/bacteria"
  #' )
  #' ranks <- c("Genus", "Family", "Order", "Class", "Phylum", "Domain")
  #' extract_phylogeny(phylogeny_example, ranks)
  #'
  #' # Full URLs with species/variety, etc.
  #'phylogeny_example <- c(
  #'  "https://lpsn.dsmz.de/variety/synechococcus-elongatus-vestitus",
  #'  "https://lpsn.dsmz.de/species/synechococcus-elongatus",
  #'  "https://lpsn.dsmz.de/genus/synechococcus",
  #'  "https://lpsn.dsmz.de/family/synechococcaceae",
  #'  "https://lpsn.dsmz.de/order/synechococcales",
  #'  "https://lpsn.dsmz.de/class/chroococcophyceae",
  #'  "https://lpsn.dsmz.de/phylum/cyanobacteriota",
  #'  "https://lpsn.dsmz.de/kingdom/bacillati",
  #'  "https://lpsn.dsmz.de/domain/bacteria"
  #')
  #' ranks = c("Variety", "Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom", "Domain")
  #' extract_phylogeny(phylogeny_example, ranks)
  #'
  #' @export
  extract_phylogeny <- function(phylogeny, ranks) {
    
    # Initialize output vector
    named_phylogeny <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
    
    # Normalize ranks to lowercase for matching
    ranks_lower <- tolower(ranks)
    
    # Infraspecific ranks → lowercase only last epithet
    infra_ranks <- c("species", "subspecies", "variety", "form")
    
    for (item in phylogeny) {
      
      # Strip any scheme, domain, query, trailing slashes → leave "/rank/name"
      item_path <- sub("^https?://[^/]+", "", item)
      item_path <- sub("\\?.*$", "", item_path)
      item_path <- sub("/+$", "", item_path)
      
      # Extract rank and slug
      rank <- stringr::str_extract(item_path, "(?<=^/)[^/]+")
      name <- stringr::str_extract(item_path, "[^/]+$")
      
      # Remove trailing numeric suffixes like "-1", "-2"
      name <- sub("-[0-9]+$", "", name)
      
      if (is.na(rank) || is.na(name)) next
      
      rank_lower <- tolower(rank)
      
      # Only process requested ranks
      if (!rank_lower %in% ranks_lower) next
      
      # Determine output value
      if (rank_lower %in% infra_ranks) {
        # Only the last epithet, forced lowercase
        parts <- strsplit(name, "-", fixed = TRUE)[[1]]
        value <- tolower(parts[length(parts)])
      } else {
        # Higher ranks → Title Case (first letter uppercase)
        # Convert slug to a readable form: replace hyphens with spaces, title case, remove spaces
        clean <- gsub("-", " ", name)
        clean <- stringr::str_to_title(clean)
        value <- gsub(" ", "", clean)  # Re-collapse
      }
      
      # Store
      idx <- which(ranks_lower == rank_lower)
      named_phylogeny[idx] <- value
    }
    
    named_phylogeny
  }
  
  #' Null-or-Empty Coalescing Operator
  #'
  #' This operator returns the first argument \code{x} unless it is
  #' \code{NULL}, has length zero, or is \code{NA}, in which case it returns
  #' \code{y}.  
  #' It is useful for providing default values in pipelines and helper
  #' functions where missing, empty, or undefined objects should fall back
  #' to a defined default.
  #'
  #' @param x The primary value to check.
  #' @param y The fallback value returned when \code{x} is \code{NULL},
  #'   has length zero, or is \code{NA}.
  #'
  #' @return \code{x} if it is non-null, non-empty, and not \code{NA};
  #'   otherwise \code{y}.
  #'
  #' @examples
  #' NULL %||% "default"
  #' #> "default"
  #'
  #' NA %||% 5
  #' #> 5
  #'
  #' "" %||% "fallback"
  #' #> ""
  #'
  #' @export
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || is.na(x)) y else x
  }
  
  #' Extract a field from the body of the LPSN page
  #' 
  #' This helper function extracts the text following a label such as
  #' \code{"Name"} or \code{"Category"} from an LPSN taxon page.
  #' It looks both at the text node(s) following the bold label
  #' (e.g., \code{<b>Record number:</b> 50}) and at the text of the
  #' parent paragraph. If multiple matches are found, it returns the
  #' longest non-empty value after stripping the label prefix.
  #'
  #' @param body An HTML document (result of \code{rvest::read_html()}).
  #' @param label_fragment A text fragment that identifies the label,
  #'   e.g., \code{"Name"}, \code{"Category"}, \code{"Proposed as"},
  #'   \code{"Record number"}.
  #'
  #' @return A cleaned character string containing the extracted field,
  #'   or \code{NA_character_} if the field is not found.
  #'
  #' @examples
  #' \dontrun{
  #' html <- rvest::read_html("https://lpsn.dsmz.de/genus/zymomonas")
  #' extract_lpsn_field(html, "Name")
  #' extract_lpsn_field(html, "Record number")
  #' }
  #'
  #' @export
  extract_lpsn_field <- function(body, label_fragment) {
    
    # Text from parent <p> element
    parent_text <- body %>%
      rvest::html_nodes(
        xpath = sprintf(
          "//b[contains(normalize-space(.), '%s')]/parent::p",
          label_fragment
        )
      ) %>%
      rvest::html_text()
    
    # Text nodes following the <b>Label</b>
    sibling_text <- body %>%
      rvest::html_nodes(
        xpath = sprintf(
          "//b[contains(normalize-space(.), '%s')]/following-sibling::text()",
          label_fragment
        )
      ) %>%
      rvest::html_text()
    
    # Combine, preferring parent text but keeping siblings as fallback
    text <- c(parent_text, sibling_text)
    
    if (length(text) == 0) {
      return(NA_character_)
    }
    
    # Strip label prefix (e.g., "Name:" or "Record number:") and clean
    cleaned <- text %>%
      stringr::str_replace("^\\s*[^:]+:\\s*", "") %>%
      stringr::str_squish()
    
    # Remove empty strings
    cleaned <- cleaned[cleaned != ""]
    if (length(cleaned) == 0) {
      return(NA_character_)
    }
    
    # Return the longest cleaned value (captures full "Type strain" line)
    cleaned[which.max(nchar(cleaned))]
  }
  
  
  #' Retrieve taxonomy metadata from an LPSN organism page
  #'
  #' This function extracts taxonomic and nomenclatural metadata from an
  #' LPSN species or higher-rank taxon page. Fields extracted include:
  #' \itemize{
  #'   \item Name
  #'   \item Category (rank)
  #'   \item Proposed as
  #'   \item Etymology
  #'   \item Pronunciation, gender
  #'   \item Valid publication
  #'   \item Nomenclatural status
  #'   \item Taxonomic status
  #'   \item Record number
  #' }
  #'
  #' The function returns both the raw taxon name as provided by LPSN and a
  #' simplified form:
  #'
  #' \itemize{
  #'   \item For species: \code{"Genus species"}
  #'   \item For higher ranks: \code{"Genus"} (first word)
  #' }
  #'
  #' @param body An HTML document from LPSN, produced by
  #'   \code{rvest::read_html()}.
  #'
  #' @return A tibble with the extracted fields.
  #'
  #' @examples
  #' \dontrun{
  #' html <- rvest::read_html("https://lpsn.dsmz.de/species/zymomonas-mobilis")
  #' get_taxon_info(html)
  #' }
  #'
  #' @export
  get_taxon_info <- function(body) {
    
    # Extract raw values
    name                 <- extract_lpsn_field(body, "Name")
    category             <- extract_lpsn_field(body, "Category")
    proposed_as          <- extract_lpsn_field(body, "Proposed as")
    etymology            <- extract_lpsn_field(body, "Etymology")
    pronunciation_gender <- extract_lpsn_field(body, "Pronunciation, gender")
    type_strain          <- extract_lpsn_field(body, "Type strain")
    holotype             <- extract_lpsn_field(body, "Holotype")  
    rRNA_gene            <- extract_lpsn_field(body, "16S rRNA gene")
    valid_publication    <- extract_lpsn_field(body, "Valid publication")
    effective_publication <- extract_lpsn_field(body, "Effective publication")
    nomenclatural_status <- extract_lpsn_field(body, "Nomenclatural status")
    taxonomic_status     <- extract_lpsn_field(body, "Taxonomic status")
    risk_group           <- extract_lpsn_field(body, "Risk group")
    record_number        <- extract_lpsn_field(body, "Record number")
    
    # Return structured output
    tibble::tibble(
      Name                    = name,
      Category                = category,
      `Proposed as`           = proposed_as,
      Etymology               = etymology,
      `Pronunciation, gender` = pronunciation_gender,
      `Type strain`           = type_strain,
      Holotype                = holotype,  
      `16S rRNA gene`         = rRNA_gene,
      `Valid publication`     = valid_publication,
      `Effective publication` = effective_publication,
      `Nomenclatural status`  = nomenclatural_status,
      `Taxonomic status`      = taxonomic_status,
      `Risk group`            = risk_group,
      `Record number`         = record_number
    )
  }
  
  #' Get links to child taxa
  #'
  #' This helper function parses an LPSN taxon page and extracts links to
  #' child taxa from the "Child taxa" table. 
  #'
  #' @param body An HTML document object for an LPSN page, typically
  #'   produced by \code{rvest::read_html()} or \code{polite::scrape()}.
  #' @param base_url A character scalar giving the base URL of the LPSN
  #'   site. Defaults to \code{"https://lpsn.dsmz.de"}.
  #' @param taxonomic_status_pattern Optional character scalar giving a text
  #'   fragment that must appear in the "Taxonomic status" column for a row
  #'   to be kept (default \code{"correct name"}). Set to \code{NULL} to
  #'   keep all child taxa regardless of status (including synonyms,
  #'   misspellings, etc.).
  #'
  #' @return A character vector of absolute URLs pointing to child taxa.
  #'   If no matching links are found, an empty character vector is
  #'   returned.
  #'
  #' @examples
  #' \dontrun{
  #' body <- rvest::read_html("https://lpsn.dsmz.de/genus/synechococcus")
  #' # default: only "correct name" children
  #' get_child_taxa_links(body)
  #'
  #' # all children, regardless of taxonomic status
  #' get_child_taxa_links(body, taxonomic_status_pattern = NULL)
  #' }
  #' @export
  get_child_taxa_links <- function(body,
                                   base_url = "https://lpsn.dsmz.de",
                                   taxonomic_status_pattern = NULL) {
    
    # XPath to the rows of the "Child taxa" table only
    # (first table following the <span><b>Child taxa:</b></span> label)
    if (is.null(taxonomic_status_pattern)) {
      # no status filter: take all rows
      xpath <- paste0(
        "//span[b[contains(., 'Child taxa:')]]",
        "/following::table[contains(@class,'detail-table')][1]",
        "//tbody/tr/td[1]//a"
      )
    } else {
      # filter rows based on text in any <td> (Taxonomic status column)
      xpath <- sprintf(paste0(
        "//span[b[contains(., 'Child taxa:')]]",
        "/following::table[contains(@class,'detail-table')][1]",
        "//tbody/tr[.//td[contains(., '%s')]]/td[1]//a"
      ), taxonomic_status_pattern)
    }
    
    hrefs <- body %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_attr("href") %>%
      unique()
    
    if (length(hrefs) == 0) return(character(0))
    
    paste0(base_url, hrefs)
  }
  
  #' Make a taxon record following LPSN format
  #'
  #' This function takes a url for a taxon in LPSN and outputs information similar
  #' to rows of https://lpsn.dsmz.de/downloads/taxonomy.csv. 
  #' 
  #' @param body An HTML document for a taxon page.
  #' @param url_for_parents Optional character vector of URLs (or paths) for all
  #'   parent taxa for this taxon. If NULL (default), lineage fields are set to NA.
  #' @param ranks Character vector of rank names to extract (default:
  #'   c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")).
  #' @param taxon_url Optional character scalar: the taxon URL. If NULL, address fields become NA.
  #'
  #' @return A one-row tibble with lineage columns and metadata.
  make_taxon_record <- function(body,
                                url_for_parents = NULL,
                                ranks = c("Domain", "Phylum", "Class", "Order",
                                          "Family", "Genus", "Species",
                                          "Subspecies", "Variety", "Form"),
                                taxon_url = NULL) {
    
    # -------------------------------------------------------------------------
    # Lineage (Phylum / Class / Order / Family / Genus / Species / ...)
    # -------------------------------------------------------------------------
    if (is.null(url_for_parents)) {
      # No parental lineage provided → fill lineage columns with NA
      tax_vec <- stats::setNames(rep(NA_character_, length(ranks)), ranks)
    } else {
      # Extract lineage normally
      tax_vec <- extract_phylogeny(url_for_parents, ranks = ranks)
    }
    
    lineage_tbl <- tibble::as_tibble(as.list(tax_vec))
    
    # -------------------------------------------------------------------------
    # Page-level metadata
    # -------------------------------------------------------------------------
    info <- get_taxon_info(body)
    
    # --- Genus / species / infraspecific from lineage ------------------------
    # Prefer lineage; fall back to NA if not present
    genus_name <- tax_vec["Genus"]   %||% NA_character_
    sp_epithet <- tax_vec["Species"] %||% NA_character_
    
    infra_rank    <- NA_character_
    infra_epithet <- NA_character_
    
    # Check which of Subspecies / Variety / Form is actually populated
    infra_rank_candidates <- c("Subspecies", "Variety", "Form")
    non_na_infra <- infra_rank_candidates[
      !is.na(tax_vec[infra_rank_candidates]) &
        tax_vec[infra_rank_candidates] != ""
    ]
    
    if (length(non_na_infra) >= 1) {
      # If somehow more than one is filled, just take the first
      infra_rank    <- non_na_infra[1]
      infra_epithet <- tax_vec[[infra_rank]]
    }
    
    # Generic infraspecific epithet used in LPSN "Subspecies" column
    subsp_epithet <- if (!is.na(infra_epithet) && infra_epithet != "") {
      infra_epithet
    } else {
      NA_character_
    }
    
    # Reference (Valid > Effective)
    reference <- info$`Valid publication` %||% info$`Effective publication`
    
    # Status
    status_pieces <- c(
      info$Category,
      info$`Proposed as`,
      info$`Nomenclatural status`,
      info$`Taxonomic status`
    )
    status_pieces <- status_pieces[!is.na(status_pieces) & status_pieces != ""]
    status <- if (length(status_pieces)) paste(status_pieces, collapse = "; ")
    else NA_character_
    
    # Authors (crude)
    pub_for_authors <- info$`Effective publication` %||% info$`Valid publication`
    authors <- if (!is.na(pub_for_authors))
      pub_for_authors %>%
      stringr::str_extract("^[^\\.]+") %>%
      stringr::str_squish()
    else NA_character_
    
    risk_grp           <- info$`Risk group`      %||% NA_character_
    nomenclatural_type <- info$`Type strain`     %||% info$`Holotype`
    record_no          <- info$`Record number`   %||% NA_character_
    
    address    <- taxon_url
    record_lnk <- taxon_url
    
    dplyr::bind_cols(
      lineage_tbl,
      tibble::tibble(
        genus_name         = genus_name,
        sp_epithet         = sp_epithet,
        infra_rank         = infra_rank,      # "Subspecies", "Variety", or "Form"
        subsp_epithet      = subsp_epithet,   # generic infraspecific epithet
        reference          = reference,
        status             = status,
        authors            = authors,
        address            = address,
        risk_grp           = risk_grp,
        nomenclatural_type = nomenclatural_type,
        record_no          = record_no,
        record_lnk         = record_lnk
      )
    )
  }
  
  #' Get Information for a Taxon and All Descendant Taxa
  #'
  #' This function takes the URL of a parent taxon on LPSN and returns a tibble
  #' with information for that taxon and all of its descendant taxa. If the parent
  #' taxon is a phylum, the function will return information on all taxa
  #' (classes, orders, families, genera, species, subspecies, varieties, forms,
  #' etc.) below it.
  #'  
  #' The crawl is recursive: for each page, it finds child taxa links
  #' (using \code{get_child_taxa_links()}), constructs a record for the current
  #' taxon with \code{make_taxon_record()}, then descends into its children.
  #'
  #' @param taxon_url Character scalar: URL of the current LPSN taxon.
  #' @param url_for_parents Character vector of URLs (or paths) for all parent taxa
  #'   for this taxon. Defaults to \code{taxon_url} at the root call.
  #' @param max_taxa Integer: maximum number of taxon records to collect.
  #'
  #' @return Tibble of taxon records (possibly empty).
  get_information_on_child_taxa <- function(taxon_url,
                                            url_for_parents = taxon_url,
                                            max_taxa = Inf) {
    # Fetch page
    body <- tryCatch(
      {
        get_web_page_body(url = taxon_url)
      },
      error = function(e) {
        warning("Skipping ", taxon_url,
                " (failed to fetch body): ", conditionMessage(e))
        NULL
      }
    )
    
    if (is.null(body)) {
      warning("Skipping ", taxon_url, " (no HTML body returned).")
      return(tibble::tibble())
    }
    
    message("Visited: ", taxon_url)
    
    # Get URL for child taxa
    url_for_children <- get_child_taxa_links(body)
    is_leaf <- length(url_for_children) == 0
    
    # Start with a record for the *current* taxon
    records <- make_taxon_record(
      body            = body,
      url_for_parents = url_for_parents,
      taxon_url       = taxon_url
    )
    
    # Stop early if we've hit the maximum
    if (nrow(records) >= max_taxa || is_leaf) {
      return(dplyr::slice(records, seq_len(min(nrow(records), max_taxa))))
    }
    
    # Recurse into children
    for (child_url in url_for_children) {
      if (nrow(records) >= max_taxa) break
      
      # Extend the lineage path with this child
      new_url_for_parents <- c(url_for_parents, child_url)
      remaining           <- max_taxa - nrow(records)
      
      child_records <- get_information_on_child_taxa(
        taxon_url       = child_url,
        url_for_parents = new_url_for_parents,
        max_taxa        = remaining
      )
      
      if (nrow(child_records)) {
        records <- dplyr::bind_rows(records, child_records)
      }
    }
    
    records
  }
  
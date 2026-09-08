# Define Functions for Examples Module
# These are functions specific to this module
# Author: Timothy Hackmann
# Date: 2 Aug 2026

# === Find example jobs ===
#' Make an empty table of example jobs
#'
#' This helper returns a table with no rows but the same columns as
#' \code{find_example_jobs()}. It is used when no examples are found.
#'
#' @return A data frame with zero rows.
#' @export
make_empty_example_jobs <- function() {
  data.frame(
    dataset       = character(0),
    tool          = character(0),
    job           = character(0),
    path          = character(0),
    dataset_label = character(0),
    tool_label    = character(0),
    job_label     = character(0),
    stringsAsFactors = FALSE
  )
}

#' Find example jobs saved on disk
#'
#' This function lists the job files saved under the examples folder. Files are
#' expected to be organized by dataset and then by tool, as in
#' \code{jobs/examples/<dataset>/<tool>/<job>.rds}. Files saved alongside
#' results (status, progress, input, and cancel files) are skipped, and so are
#' files that are not at the expected depth.
#'
#' @param jobs_dir Folder holding example jobs. Default is
#'   \code{example_jobs_dir}.
#' @return A data frame with one row per job, sorted by dataset and tool. It
#'   has the dataset, tool, and job names, the file path, and the names shown
#'   to users.
#' @export
find_example_jobs <- function(jobs_dir = example_jobs_dir) {
  # Drop any trailing slash so paths can be split reliably
  jobs_dir <- sub("/+$", "", jobs_dir)

  # Return an empty table when the folder is missing
  if (!dir.exists(jobs_dir)) {
    return(make_empty_example_jobs())
  }

  # Find all saved files
  files <- list.files(
    jobs_dir,
    pattern    = "\\.rds$",
    recursive  = TRUE,
    full.names = TRUE
  )

  # Drop files saved alongside results
  files <- files[!grepl("\\.(status|progress|cancel)\\.rds$|_inputs\\.rds$", basename(files))]

  if (length(files) == 0) {
    return(make_empty_example_jobs())
  }

  # Split paths into dataset, tool, and file name
  relative <- substring(files, nchar(jobs_dir) + 2L)
  parts <- strsplit(relative, "/", fixed = TRUE)

  # Keep files at the expected depth (dataset, then tool, then file)
  keep <- lengths(parts) == 3L
  files <- files[keep]
  parts <- parts[keep]

  if (length(files) == 0) {
    return(make_empty_example_jobs())
  }

  dataset <- vapply(parts, `[`, character(1), 1L)
  tool    <- vapply(parts, `[`, character(1), 2L)
  job     <- sub("\\.rds$", "", vapply(parts, `[`, character(1), 3L))

  # Build the table of jobs
  jobs <- data.frame(
    dataset       = dataset,
    tool          = tool,
    job           = job,
    path          = files,
    dataset_label = label_example_item(dataset, example_dataset_labels),
    tool_label    = label_example_item(tool, example_tool_labels),
    job_label     = label_example_job(dataset, job),
    stringsAsFactors = FALSE
  )

  # Sort by dataset, then tool, then name of job
  jobs <- jobs[
    order(
      rank_by_name(jobs$dataset, names(example_dataset_labels)),
      jobs$dataset,
      rank_by_name(jobs$tool, names(example_tool_labels)),
      jobs$tool,
      jobs$job_label
    ), ,
    drop = FALSE
  ]

  rownames(jobs) <- NULL

  jobs
}

# === Name example jobs ===
#' Tidy a folder or file name for display
#'
#' This helper replaces underscores and dashes with spaces and capitalizes the
#' first letter. It gives a readable name for folders and files that have no
#' name set in the module variables.
#'
#' @param x Character vector of folder or file names.
#' @return A character vector of tidied names.
#' @export
prettify_example_name <- function(x) {
  x <- gsub("[_-]+", " ", x)
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

#' Look up the name shown for a dataset or tool
#'
#' This helper matches a folder name against the names set in the module
#' variables. Folders with no match get a tidied version of the folder name.
#'
#' @param x Character vector of folder names.
#' @param labels Named character vector of names shown to users.
#' @return A character vector of names shown to users.
#' @export
label_example_item <- function(x, labels) {
  out <- unname(labels[x])

  # Fall back to a tidied folder name
  is_missing <- is.na(out)
  out[is_missing] <- prettify_example_name(x[is_missing])

  out
}

#' Look up the name shown for a job
#'
#' This helper matches a job against the names set in the module variables. It
#' first tries a key with the dataset and job ("ancestral/all"), then the job
#' alone ("all"), and last a tidied version of the file name.
#'
#' @param dataset Character vector of dataset names.
#' @param job Character vector of job names.
#' @return A character vector of names shown to users.
#' @export
label_example_job <- function(dataset, job) {
  # Try a key with the dataset and job
  out <- unname(example_job_labels[paste0(dataset, "/", job)])

  # Try a key with the job alone
  is_missing <- is.na(out)
  out[is_missing] <- unname(example_job_labels[job[is_missing]])

  # Fall back to a tidied file name
  is_missing <- is.na(out)
  out[is_missing] <- prettify_example_name(job[is_missing])

  out
}

#' Rank items to match a preferred order
#'
#' This helper gives each item its position in a preferred order. Items not in
#' that order are ranked last, so they sort after the ones that are.
#'
#' @param x Character vector of items to rank.
#' @param preferred Character vector giving the preferred order.
#' @return An integer vector of ranks.
#' @export
rank_by_name <- function(x, preferred) {
  rank <- match(x, preferred)
  rank[is.na(rank)] <- length(preferred) + 1L
  rank
}

# === Build links to example jobs ===
#' Build the address of an example job
#'
#' This helper builds a job URL of the form ?tab=<tool>&user=<user>&job=<job>.
#' The user is the examples folder plus the dataset (e.g., "examples/rumen"),
#' which is how the app finds the job file on disk.
#'
#' @param dataset The dataset folder (e.g., "rumen").
#' @param tool The tool folder, which is also the tab ID (e.g.,
#'   "predictionsTaxonomy").
#' @param job The job name (file name without .rds).
#' @param jobs_folder The examples folder within the jobs folder. Default is
#'   \code{example_jobs_folder}.
#' @return A string with the query part of the URL.
#' @export
build_example_job_url <- function(dataset, tool, job,
                                  jobs_folder = example_jobs_folder) {
  user <- paste0(jobs_folder, "/", dataset)

  paste0(
    "?tab=",  utils::URLencode(tool, reserved = TRUE),
    "&user=", utils::URLencode(user, reserved = TRUE),
    "&job=",  utils::URLencode(job,  reserved = TRUE)
  )
}

#' Create a link to an example job
#'
#' This helper creates a link that opens one example job in a new browser tab.
#'
#' @param dataset The dataset folder (e.g., "rumen").
#' @param tool The tool folder, which is also the tab ID (e.g.,
#'   "predictionsTaxonomy").
#' @param job The job name (file name without .rds).
#' @param label The text shown for the link.
#' @return A link (an HTML anchor tag).
#' @export
create_example_job_link <- function(dataset, tool, job, label) {
  shiny::tags$a(
    href   = build_example_job_url(dataset = dataset, tool = tool, job = job),
    target = "_blank",
    rel    = "noopener",
    label
  )
}

# === Build user interface (UI) for examples ===
#' Describe a dataset
#'
#' This helper returns the text shown below the name of a dataset. It returns
#' nothing when the dataset has no description.
#'
#' @param dataset The dataset folder (e.g., "rumen").
#' @return A paragraph of text, or NULL.
#' @export
describe_example_dataset <- function(dataset) {
  description <- example_dataset_descriptions[[dataset]]

  if (is.null(description)) {
    return(NULL)
  }

  shiny::p(class = "examples-description", description)
}

#' Create the list of jobs for one dataset
#'
#' This helper creates the description of a dataset followed by its jobs. Jobs
#' are grouped by tool, with one link for each job.
#'
#' @param dataset_jobs A data frame of jobs for a single dataset, as returned
#'   by \code{find_example_jobs()}.
#' @return A list of UI elements.
#' @export
create_example_dataset_list <- function(dataset_jobs) {
  tools <- unique(dataset_jobs$tool)

  # Create one group of links per tool
  tool_lists <- lapply(tools, function(tool) {
    tool_jobs <- dataset_jobs[dataset_jobs$tool == tool, , drop = FALSE]

    shiny::div(
      class = "examples-tool",
      shiny::tags$h6(tool_jobs$tool_label[1]),
      shiny::tags$ul(
        lapply(seq_len(nrow(tool_jobs)), function(i) {
          shiny::tags$li(
            create_example_job_link(
              dataset = tool_jobs$dataset[i],
              tool    = tool_jobs$tool[i],
              job     = tool_jobs$job[i],
              label   = tool_jobs$job_label[i]
            )
          )
        })
      )
    )
  })

  shiny::tagList(
    describe_example_dataset(dataset_jobs$dataset[1]),
    tool_lists
  )
}

#' Create the list of all example jobs
#'
#' This function creates the part of the user interface that lists example
#' jobs. Jobs are grouped by dataset, and within each dataset by tool. Each
#' dataset gets its own panel, and all panels are open when the tab loads.
#'
#' @param jobs A data frame of jobs, as returned by \code{find_example_jobs()}.
#' @return A UI element listing the examples.
#' @export
create_examples_list <- function(jobs) {
  # Show a message when no examples are saved on disk
  if (nrow(jobs) == 0) {
    return(shiny::p("No examples are available."))
  }

  # Create one panel per dataset
  panels <- lapply(unique(jobs$dataset), function(dataset) {
    dataset_jobs <- jobs[jobs$dataset == dataset, , drop = FALSE]

    bslib::accordion_panel(
      title = dataset_jobs$dataset_label[1],
      value = dataset,
      create_example_dataset_list(dataset_jobs)
    )
  })

  do.call(
    bslib::accordion,
    c(panels, list(open = TRUE, multiple = TRUE))
  )
}

# === Open example jobs ===
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
  # Build query string (?tab=...&user=...&job=...)
  tab_query <- paste0(
    "?tab=", demo_tab,
    "&user=", demo_user,
    "&job=",  demo_job
  )

  # Open URL
  shinyjs::runjs(sprintf("window.open('%s', '_blank');", tab_query))

  invisible(tab_query)
}

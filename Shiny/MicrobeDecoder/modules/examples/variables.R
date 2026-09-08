# Define Variables for Examples Module
# These are variables specific to this module
# Author: Timothy Hackmann
# Date: 2 Aug 2026

# === Set location of example jobs ===
# Example jobs are saved as jobs/examples/<dataset>/<tool>/<job>.rds
#' @export
example_jobs_root <- "jobs"

#' @export
example_jobs_folder <- "examples"

#' @export
example_jobs_dir <- file.path(example_jobs_root, example_jobs_folder)

# === Set names shown to users ===
# Names for datasets (folders one level below jobs/examples).  The order here
# is the order datasets appear in the app.  Datasets not listed are shown last.
#' @export
example_dataset_labels <- c(
  rumen     = "Bacterial isolates from the rumen",
  ancestral = "Ancestral microbes"
)

# Names for tools (folders one level below each dataset).  The order here is
# the order tools appear within a dataset.  Tools not listed are shown last.
#' @export
example_tool_labels <- c(
  predictionsTaxonomy        = "Taxonomy",
  predictionsNetwork         = "Metabolic networks",
  predictionsMachineLearning = "Machine learning",
  databaseSearch             = "Database search"
)

# Names for individual jobs (file names without .rds).  Keys can be a job name
# ("all") or a dataset and job name ("ancestral/all").  Jobs not listed get a
# name built from the file name.
#' @export
example_job_labels <- c(
  all                 = "All traits",
  aerobic_respiration = "Aerobic respiration"
)
# Functions for Running Computation Jobs
# This script contains functions for running longer app computations outside
# the main Shiny process.  These functions execute jobs, monitor them, and load
# results when a job finishes.
#
# The overall workflow is 
#
# 1. Execution
#
# setup_computation_jobs()
# |
# start_job()
# |
# prepare_job()
# |
# setup_workers()
# |
# submit_job_to_worker()
# |
# <mirai>
# |
# run_job_in_worker()
# |
# run_job_network() / run_job_ml() / run_job_search()
# |
# compute_*()
# |
# save_job_result()
# 
# 2. Monitoring
#
# observe_job_management()
# | <every 250 ms>
# manage_job()
# +-- read_progress()
# +-- refresh_job_modal()
# +-- rewrite_job_status()
# +-- <check mirai>
# 
# 3. Finalization
#
# <mirai resolves>
# |
# get_job_outcome()
# |
# <completed/error>
# |
# end_job()
# +-- finalize_job_status()
# +-- <update URL>
# +-- <close modal>
# +-- reset_job()
# |
# <module sees URL change>
# |
# load_job_result()
#
# Author: Timothy Hackmann
# Date: 22 May 2026


# === Create job IDs and filepaths ===
  #' Create a unique ID for a computation job
  #'
  #' This function creates an ID used to distinguish one computation job from
  #' another. The ID is included in file names and URLs so that the app can save,
  #' retrieve, monitor, and cancel the correct job.
  #'
  #' By default, the function creates a full UUID. It can optionally create a
  #' shorter hash when a compact ID is preferred.
  #'
  #' @param short Logical. Whether to create a short hash instead of a full UUID.
  #' @return A character string containing the new job ID.
  #' @export
  create_job_id <- function(short = FALSE) {
      if (short) {
        digest::digest(Sys.time(), algo = "xxhash32", serialize = FALSE)
      } else {
        uuid::UUIDgenerate()
      }
    }

  #' Create a URL for a saved computation job
  #'
  #' This function creates a URL that points to the results for a specific
  #' computation job. The URL identifies the app tab, the user, and the job.
  #'
  #' The URL can be displayed while a job is running or added to the browser
  #' history after a job finishes. This allows the user to return to saved results
  #' later or share the URL when appropriate.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param job_id The job ID (no prefix needed)
  #' @param user_id User ID (hashed or otherwise). If NULL, it is auto-detected.
  #' @param tab The current tab name (e.g., "predictionsTaxonomy")
  #'
  #' @return A string URL like ?tab=predictionsTaxonomy&job=abc123&user=xyz
  #' @export
  create_job_url <- function(session = shiny::getDefaultReactiveDomain(),
                             job_id,
                             user_id = NULL,
                             tab
  ) {
    if (is.null(user_id)){
      user_id <- session$userData$user_id()
    }

    query <- paste0("?tab=", tab, "&user=", user_id, "&job=", job_id)

    paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (!is.null(session$clientData$url_port)) paste0(":", session$clientData$url_port),
      session$clientData$url_pathname,
      query
    )
  }

  #' Create the path for a temporary progress file
  #'
  #' This function creates the path for the temporary RDS file used to report the
  #' progress of a computation job. While the worker performs the computation, it
  #' writes updates to this file. The main Shiny process reads the file to update
  #' the progress modal shown to the user.
  #'
  #' The function creates only the path. It does not create the file itself.
  #'
  #' @param job_id A character job identifier, typically from
  #'   \code{create_job_id()}.
  #' @return A list with element \code{progress_file}.
  #' @export
  create_progress_filepath <- function(job_id) {
    list(
      progress_file = file.path(tempdir(),
                                paste0(job_id, "_progress.rds"))
    )
  }

  #' Construct a job directory path
  #'
  #' @param tab Tab name (e.g., "predictionsTaxonomy")
  #' @param user_id User ID (hashed or otherwise). If NULL, it is auto-detected.
  #' @param base_dir Base job directory (default = "jobs")
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @return Full job directory path
  #' @export
  get_job_dir <- function(tab,
                          user_id = NULL,
                          base_dir = "jobs",
                          session = shiny::getDefaultReactiveDomain()){
    if (is.null(user_id)){
      user_id <- session$userData$user_id()
    }

    file.path(base_dir, user_id, tab)
  }

# === Save and load jobs ===
  #' Save an object to an RDS file without leaving a partial file behind
  #'
  #' This function writes an object to a temporary file and then renames it into
  #' place. A reader in another session therefore never opens a half-written
  #' file. When renaming is not allowed, the file is copied instead.
  #'
  #' @param object The object to save.
  #' @param path Path to the destination \code{.rds} file.
  #' @return Invisibly returns the path.
  #' @export
  save_object_safely <- function(object, path) {
    tmp <- paste0(path, ".tmp-", Sys.getpid())
    saveRDS(object, tmp)

    if (!file.rename(tmp, path)) {
      file.copy(tmp, path, overwrite = TRUE)
      unlink(tmp)
    }

    invisible(path)
  }

  #' Save the result of a computation job
  #'
  #' This function saves the final result of a computation job as an RDS file.
  #' The file name is based on the job ID so that the result can be loaded later.
  #'
  #' The worker calls this function after it finishes a computation.
  #'
  #' @param job_id Job ID string.
  #' @param result The result to save.
  #' @param job_dir Path to job directory.
  #' @return None. The function saves the result to disk as a side effect.
  #' @export
  save_job_result <- function(job_id, result, job_dir) {
    dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, file = file.path(job_dir, paste0(job_id, ".rds")))
  }

  #' Load the saved result of a computation job
  #'
  #' This function loads the saved RDS file for a computation job. If the result
  #' file does not exist, the function returns `NULL`.
  #'
  #' @param job_id Job ID string.
  #' @param job_dir Path to job directory.
  #' @return The loaded result object, or NULL if not found
  #' @export
  load_job_result <- function(job_id, job_dir) {
    path <- file.path(job_dir, paste0(job_id, ".rds"))
    if (file.exists(path)) readRDS(path) else NULL
  }

# === Track job status ===
  #' Return the status-file path for a job.
  #'
  #' This function constructs the path to the status file for a computation job.
  #' The status file is a small RDS file saved next to the job's result. It
  #' records whether the job was submitted, is running, completed, failed, or was
  #' canceled.
  #'
  #' @param job_dir Path to the job directory.
  #' @param job_id Job ID string.
  #' @return A character path to the status \code{.rds} file.
  #' @export
  get_status_filepath <- function(job_dir, job_id) {
    file.path(job_dir, paste0(job_id, ".status.rds"))
  }

  #' Save the status of a computation job
  #'
  #' This function saves the current status of a computation job as a small RDS
  #' file next to the job's result. The session that owns the job updates this
  #' file as the job moves between states.
  #'
  #' The user and tab are taken from the job directory, which has the form
  #' \code{jobs/<user>/<tab>}. The update time is set to the current time so the
  #' History tab can detect jobs whose owning session has stopped reporting.
  #'
  #' @param job_dir Path to the job directory.
  #' @param job_id Job ID string.
  #' @param status Character status: one of "submitted", "running", "completed",
  #'   "error", "cancelled", or "interrupted".
  #' @param submitted_at Time the job was submitted. Defaults to the current time.
  #' @param finished_at Time the job reached a terminal state, or NULL.
  #' @param error_message Character error message for failed jobs, or NULL.
  #' @param percent Numeric progress from 0 to 100, or NA when not known.
  #' @return Invisibly returns the path to the status file.
  #' @export
  write_job_status <- function(job_dir,
                               job_id,
                               status,
                               submitted_at = Sys.time(),
                               finished_at = NULL,
                               error_message = NULL,
                               percent = NA_real_) {
    dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
    
    record <- list(
      job_id        = job_id,
      user_id       = basename(dirname(job_dir)),
      tab           = basename(job_dir),
      status        = status,
      submitted_at  = submitted_at,
      updated_at    = Sys.time(),
      finished_at   = finished_at,
      error_message = error_message,
      percent       = percent
    )

    save_object_safely(record, get_status_filepath(job_dir, job_id))
  }

  #' Read the status of a computation job
  #'
  #' This function reads a job's status file. The owning session may be writing
  #' the file at the same moment, so the function returns `NULL` if the file is
  #' missing or cannot be read. The caller can try again later.
  #'
  #' @param path Path to the status \code{.rds} file.
  #' @return The status record list, or `NULL`.
  #' @export
  read_job_status <- function(path) {
    if (is.null(path) || !file.exists(path)) {
      return(NULL)
    }

    tryCatch(readRDS(path), error = function(e) NULL)
  }

  #' Label the status of a computation job
  #'
  #' This function returns the status a job should be labeled as. A job that has
  #' reached a terminal state keeps that state. A job still marked as submitted or
  #' running is labeled as "interrupted" when its owning session has stopped
  #' updating the status file for longer than `stale_seconds`. This happens when
  #' the app process that owned the job stops before the job finishes.
  #'
  #' @param record A status record from `read_job_status()`.
  #' @param stale_seconds Number of seconds without an update before a
  #'   non-terminal job is treated as interrupted.
  #' @return A character status string.
  #' @export
  label_job_status <- function(record, stale_seconds = 300) {
    status <- record$status %||% "submitted"

    if (status %in% job_final_statuses) {
      return(status)
    }

    updated_at <- record$updated_at
    if (is.null(updated_at)) {
      return(status)
    }

    age <- as.numeric(difftime(Sys.time(), updated_at, units = "secs"))
    if (is.finite(age) && age > stale_seconds) "interrupted" else status
  }

  #' Read job status records, using the cache when files are unchanged
  #'
  #' This function reads the status record for each status file. An entry is
  #' keyed by the full path of the status file and is reused only while that
  #' file's modification time is unchanged.
  #'
  #' Only the record itself is cached. Anything derived from the result file is
  #' left to the caller, because the result file can change while the status
  #' file does not.
  #'
  #' @param status_files Character vector of status file paths.
  #' @param cache Environment used to store records between calls, or NULL.
  #' @return A list of status records, with NULL for files that could not be
  #'   read.
  read_status_records <- function(status_files, cache = NULL) {
    mtimes <- file.info(status_files)$mtime

    lapply(seq_along(status_files), function(i) {
      path <- status_files[i]
      mtime <- mtimes[i]

      # Read from cache if the status file is unchanged, and from disk otherwise
      hit <- if (!is.null(cache)) cache[[path]] else NULL

      if (!is.null(hit) && !is.na(mtime) && identical(hit$mtime, mtime)) {
        return(hit$record)
      }

      record <- read_job_status(path)

      # Store record in cache
      if (!is.null(cache) && !is.null(record) && !is.na(mtime)) {
        cache[[path]] <- list(mtime = mtime, record = record)
      }

      record
    })
  }

  #' Write the final status of a job unless one is already recorded
  #'
  #' This function records the status of a job that has finished. A status
  #' already written by the worker or by an earlier call is kept, so a later
  #' cleanup cannot replace a completed or failed job with "interrupted". Set
  #' `overwrite_final` when the caller should win regardless.
  #'
  #' The function reports the status it wrote, so a caller can tell when its own
  #' status was dropped.
  #'
  #' @param job_dir Path to the job directory, or NULL.
  #' @param job_id Job ID string, or NULL.
  #' @param status One of the statuses from `job_final_statuses`.
  #' @param submitted_at Time the job was submitted.
  #' @param error_message Character error message for a failed job, or NULL.
  #' @param percent Numeric final progress from 0 to 100.
  #' @param overwrite_final Logical. Whether to replace a status that is already
  #'   final.
  #' @return Invisibly the status that was written, or NULL when nothing was
  #'   written.
  #' @export
  finalize_job_status <- function(job_dir,
                                  job_id,
                                  status,
                                  submitted_at,
                                  error_message = NULL,
                                  percent = NA_real_,
                                  overwrite_final = FALSE) {
    if (is.null(job_dir) || is.null(job_id)) {
      return(invisible(NULL))
    }

    existing <- read_job_status(get_status_filepath(job_dir, job_id))

    already_final <- !is.null(existing) &&
      existing$status %in% job_final_statuses

    if (already_final && !isTRUE(overwrite_final)) {
      return(invisible(NULL))
    }

    write_job_status(
      job_dir       = job_dir,
      job_id        = job_id,
      status        = status,
      submitted_at  = submitted_at,
      finished_at   = Sys.time(),
      error_message = error_message,
      percent       = percent
    )

    invisible(status)
  }
  
  #' Pull a field from a list of records
  #'
  #' This function extracts the same field from every record and returns it as
  #' a character, numeric, or POSIXct vector. Missing fields become NA.
  #'
  #' @param records A list of records.
  #' @param name Name of the field to extract.
  #' @param type Type of vector to return: "chr", "num", or "time".
  #' @return A character, numeric, or POSIXct vector.
  pull_from_record <- function(records, name, type = c("chr", "num", "time")) {
    type <- match.arg(type)
    
    if (type == "chr") {
      return(vapply(
        records,
        function(record) as.character(record[[name]] %||% NA_character_),
        character(1)
      ))
    }
    
    if (type == "num") {
      return(vapply(
        records,
        function(record) as.numeric(record[[name]] %||% NA_real_),
        numeric(1)
      ))
    }
    
    # Convert values to dates/times in POSIXct format
    .POSIXct(vapply(
      records,
      function(record) {
        value <- record[[name]]
        if (is.null(value)) NA_real_ else as.numeric(as.POSIXct(value))
      },
      numeric(1)
    ))
  }
  
  #' Report status of jobs
  #'
  #' This is the main function for reporting status for jobs.
  #' It finds status files for computation jobs in a specified
  #' directory reads them, then returns a dataframe with the status of each job.
  #' This dataframe is used to populate the History table in the app.
  #' Records are read once and then stored in cache until they are modified.
  #'
  #' The status and the result size are worked out again on every call. The
  #' status depends on the current time, and the result file can change without
  #' the status file changing, so neither can be cached against the status
  #' file's modification time.
  #'
  #' @param user_dirs Character vector of user job directories.
  #' @param stale_seconds Number of seconds passed to `label_job_status()` when
  #'   checking whether a job has been interrupted.
  #' @param cache Optional environment used to store status records between
  #'   calls.
  #' @return A data frame with one row per job, or `NULL` if no jobs are found.
  #' @export
  report_job_status <- function(user_dirs, stale_seconds = 300, cache = NULL) {
    # Find status files in user directories
    user_dirs <- user_dirs[dir.exists(user_dirs)]
    if (length(user_dirs) == 0) {
      return(NULL)
    }

    status_files <- list.files(
      user_dirs,
      pattern = "\\.status\\.rds$",
      recursive = TRUE,
      full.names = TRUE
    )

    # Drop cached records for files that are gone
    if (!is.null(cache)) {
      gone <- setdiff(ls(cache, all.names = TRUE), status_files)
      
      if (length(gone)) {
        rm(list = gone, envir = cache)
      }
    }
    
    if (length(status_files) == 0) {
      return(NULL)
    }

    # Read the records, then drop any file that could not be read
    records <- read_status_records(status_files, cache)

    keep <- !vapply(records, is.null, logical(1))
    records <- records[keep]
    status_files <- status_files[keep]

    if (length(records) == 0) {
      return(NULL)
    }
    
    # Pull values from records
    job_ids <- pull_from_record(records, "job_id", "chr")
    user_ids <- pull_from_record(records, "user_id", "chr")
    tabs <- pull_from_record(records, "tab", "chr")

    # Get job status
    statuses <- vapply(
      records,
      label_job_status,
      character(1),
      stale_seconds = stale_seconds
    )

    # Get job size, reading the result files in one pass
    result_sizes <- rep(NA_real_, length(status_files))
    completed <- statuses == "completed"

    if (any(completed)) {
      result_sizes[completed] <- file.info(
        sub("\\.status\\.rds$", ".rds", status_files[completed])
      )$size
    }

    # Return a dataframe with status records
    data.frame(
      job_id        = job_ids,
      user_id       = user_ids,
      tab           = tabs,
      status        = statuses,
      submitted_at  = pull_from_record(records, "submitted_at", "time"),
      updated_at    = pull_from_record(records, "updated_at", "time"),
      finished_at   = pull_from_record(records, "finished_at", "time"),
      error_message = pull_from_record(records, "error_message", "chr"),
      percent       = pull_from_record(records, "percent", "num"),
      result_size   = result_sizes
    )
  }

# === Track modal state ===
  #' Check whether the progress modal is open
  #'
  #' This function reports whether a progress modal is on screen. The flag is
  #' stored on the session and may not be set up yet, so the function returns
  #' `FALSE` rather than failing when the flag is missing.
  #'
  #' @param session The Shiny session object.
  #' @return `TRUE` if a modal is open, otherwise `FALSE`.
  #' @export
  is_modal_open <- function(session) {
    flag <- session$userData$modal_open

    if (is.null(session) || !is.function(flag)) {
      return(FALSE)
    }

    isTRUE(shiny::isolate(flag()))
  }

  #' Record whether the progress modal is open
  #'
  #' This function updates the app-wide record of whether a progress modal is on
  #' screen. It does nothing when the flag has not been set up.
  #'
  #' @param session The Shiny session object.
  #' @param open Logical. Whether a modal is now open.
  #' @return Invisibly NULL.
  #' @export
  set_modal_open <- function(session, open) {
    flag <- session$userData$modal_open

    if (!is.null(session) && is.function(flag)) {
      shiny::isolate(flag(isTRUE(open)))
    }

    invisible(NULL)
  }

# === Monitor job progress ===
  #' Save a progress update for a computation job
  #'
  #' This function saves the latest progress update for a computation job as a
  #' small RDS file. A worker calls it at checkpoints during a long computation,
  #' and the main Shiny process reads the file to update the progress modal.
  #'
  #' The progress value is limited to the range from 0 to 1 and is also saved as
  #' a percentage from 0 to 100.
  #'
  #' The update is written to a temporary file and then renamed into place, so
  #' the main process never opens a half-written file.
  #'
  #' @param progress_file Path to the progress \code{.rds} file, or \code{NULL}
  #'   to disable progress reporting.
  #' @param progress Numeric fraction in \code{[0, 1]}.  Clamped to range.
  #' @param message Optional character status message.
  #' @param detail Optional list of additional structured detail.
  #' @param retries Number of times to retry writing the file when it is briefly
  #'   blocked by the reader. Default is 5.
  #' @param wait Seconds to pause between retries. Default is 0.01.
  #' @return Invisibly returns the progress record, or \code{NULL} when progress
  #'   reporting is disabled.
  #' @export
  write_progress <- function(progress_file, progress, message = NULL, detail = NULL,
                             retries = 5L, wait = 0.01) {
    if (is.null(progress_file)) {
      return(invisible(NULL))
    }

    progress <- max(0, min(1, progress))

    progress_data <- list(
      progress = progress,
      percent  = round(progress * 100),
      message  = message %||% "",
      detail   = detail,
      time     = as.character(Sys.time())
    )

    # Write to a unique temporary file first. If even this fails, skip the
    # update; a missed progress report must never stop the computation.
    tmp <- paste0(progress_file, ".tmp-", Sys.getpid())
    ok <- tryCatch({ saveRDS(progress_data, tmp); TRUE }, error = function(e) FALSE)
    if (!ok) {
      return(invisible(NULL))
    }

    # Move the finished file into place. The reader may hold the destination
    # open for a moment, so retry briefly before giving up on this update.
    for (i in seq_len(retries)) {
      if (isTRUE(suppressWarnings(file.rename(tmp, progress_file)))) {
        return(invisible(progress_data))
      }
      if (isTRUE(suppressWarnings(file.copy(tmp, progress_file, overwrite = TRUE)))) {
        unlink(tmp)
        return(invisible(progress_data))
      }
      Sys.sleep(wait)
    }

    unlink(tmp)
    invisible(progress_data)
  }

  #' Read the latest progress update for a computation job
  #'
  #' This function reads the most recent progress update written by
  #' \code{write_progress()}. The main Shiny process uses the result to update
  #' the progress modal shown to the user.
  #'
  #' The worker may be writing the file at the same moment the main process tries
  #' to read it. When that happens the read fails, so the function retries a few
  #' times before giving up.
  #'
  #' @param progress_file Path to the progress \code{.rds} file, or \code{NULL}.
  #' @param retries Number of times to retry reading the file when it is briefly
  #'   blocked by the writer. Default is 3.
  #' @param wait Seconds to pause between retries. Default is 0.02.
  #' @return The progress record list, or \code{NULL}.
  #' @export
  read_progress <- function(progress_file, retries = 3L, wait = 0.02) {
    if (is.null(progress_file) || !file.exists(progress_file)) {
      return(NULL)
    }

    for (i in seq_len(retries)) {
      val <- suppressWarnings(tryCatch(readRDS(progress_file), error = function(e) NULL))
      if (!is.null(val)) {
        return(val)
      }
      Sys.sleep(wait)
    }

    NULL
  }

# === Store active job state ===
  #' Create a record for tracking an active computation job
  #'
  #' This function creates the reactive values used to track one module's active
  #' computation job. The record stores the job ID, file paths, worker handle,
  #' status, timing information, and observer handles.
  #'
  #' Each module receives its own job-state record so that its computation can be
  #' monitored, canceled, and cleaned up independently.
  #'
  #' The `status` slot takes one of three values. "idle" means the module has no
  #' job. "preparing" means a job has been created and the module is collecting
  #' its inputs; the update observer skips a module in this state. "running"
  #' means the job has been sent to a worker.
  #'
  #' @return A reactiveValues object used by one module's job observers.
  #' @export
  create_job_state <- function() {
    state <-  shiny::reactiveValues(
                handle        = NULL,
                job_id        = NULL,
                job_dir       = NULL,
                input_file    = NULL,
                progress_file = NULL,
                last_progress = NULL,
                status        = "idle",
                submitted_at  = NULL,
                status_submitted_at = NULL,
                status_heartbeat_at = NULL,
                status_percent = NA_real_,
                obs_submit    = NULL,
                obs_cancel    = NULL,
                obs_update    = NULL
              )
    
    return(state)
  }

  #' Clear the stored state of a computation job
  #'
  #' This function resets a module's job-state record after a computation
  #' finishes, fails, or is canceled. It removes references to the worker handle,
  #' job ID, file paths, progress updates, and timing information.
  #'
  #' This function changes only the stored state. It does not cancel a running
  #' worker, remove files, close a modal, or destroy observers. Those actions are
  #' handled by `end_job()` or `reset_job()`.
  #'
  #' @param state A reactiveValues object from create_job_state().
  #' @return Invisibly NULL.
  #' @export
  clear_job_state <- function(state) {
    state$handle        <- NULL
    state$job_id        <- NULL
    state$job_dir       <- NULL
    state$input_file    <- NULL
    state$progress_file <- NULL
    state$last_progress <- NULL
    state$status        <- "idle"
    state$submitted_at  <- NULL
    state$status_submitted_at <- NULL
    state$status_heartbeat_at <- NULL
    state$status_percent <- NA_real_
    invisible(NULL)
  }

# === Start and stop workers ===
  #' Start workers for computation jobs
  #'
  #' This function starts workers (daemons) used to run computation
  #' jobs. Each worker is initialized once when the app starts so that it can
  #' run multiple jobs without being restarted each time.
  #'
  #' Each worker moves into the app directory, sources the session setup
  #' functions, and calls initialize_session() to load the app files and
  #' plugins it needs. Dispatcher mode is used so that jobs can wait in a
  #' queue and active jobs can be cancelled.
  #'
  #' @param n Integer number of local workers. Use \code{1L} to run one
  #'   computation at a time, or increase this value if the server has enough
  #'   CPUs and memory.
  #' @param app_dir Root directory of the Shiny app.
  #' @param session_setup_file Path to the file that defines
  #'   \code{initialize_session()}. Relative paths are resolved from
  #'   \code{app_dir}.
  #'
  #' @return Invisibly returns \code{TRUE}.
  #' @export
  start_workers <- function(
    n = 1L,
    app_dir = getwd(),
    session_setup_file = "functions/sessionFunctions.R"
  ) {
    if (!requireNamespace("mirai", quietly = TRUE)) {
      stop(
        "Package 'mirai' is required. Install it with install.packages('mirai').",
        call. = FALSE
      )
    }

    # Get location of session setup functions
    app_dir <- normalizePath(
      app_dir,
      winslash = "/",
      mustWork = TRUE
    )

    if (!grepl("^([A-Za-z]:)?[/\\\\]", session_setup_file)) {
      session_setup_file <- file.path(
        app_dir,
        session_setup_file
      )
    }

    session_setup_file <- normalizePath(
      session_setup_file,
      winslash = "/",
      mustWork = TRUE
    )

    # Start workers
    mirai::daemons(
      n = as.integer(n),
      dispatcher = TRUE
    )
    
    # Initialize session in workers
    mirai::everywhere(
      {
        setwd(app_dir)
        source(session_setup_file, local = FALSE)
        
        initialize_session(
          app_dir = app_dir,
          verbose = FALSE
        )
        
        assign(".worker_ready", TRUE, envir = .GlobalEnv)
        assign(".worker_app_dir", app_dir, envir = .GlobalEnv)
        
        NULL
      },
      app_dir = app_dir,
      session_setup_file = session_setup_file
    )

    invisible(TRUE)
  }

  #' Stop workers for computation jobs
  #'
  #' This function stops workers (daemons) by reducing its pool to 0.
  #' It is called when the app exits.  This ensure that workers do not linger
  #' between app restarts.
  #'
  #' @return Invisibly returns \code{NULL}.
  #' @export
  stop_workers <- function() {
    if (requireNamespace("mirai", quietly = TRUE)) {
      mirai::daemons(0L)
    }

    .worker_state$started <- FALSE

    invisible(NULL)
  }

  #' Set up workers for running computation jobs
  #'
  #' This function starts the worker pool the first time it is needed and does
  #' nothing on later calls. It lets the app defer the cost of warming a worker
  #' until a user submits a computation, so sessions that never run a job never
  #' pay for it.
  #'
  #' @param n Integer number of local workers.
  #' @param app_dir Root directory of the Shiny app.
  #' @param session_setup_file Path to the file that defines
  #'   \code{initialize_session()}.
  #'
  #' @return Invisibly \code{TRUE} if the pool was started on this call,
  #'   \code{FALSE} if it was already running.
  #' @export
  setup_workers <- function(
    n = 1L,
    app_dir = getwd(),
    session_setup_file = "functions/sessionFunctions.R"
  ) {
    if (isTRUE(.worker_state$started)) {
      return(invisible(FALSE))
    }

    # Start workers
    start_workers(
      n                  = n,
      app_dir            = app_dir,
      session_setup_file = session_setup_file
    )
    .worker_state$started <- TRUE
    .worker_state$app_dir <- normalizePath(app_dir, winslash = "/", mustWork = FALSE)

    # Stop workers (on app exit)
    shiny::onStop(function() {
      stop_workers()
    })

    invisible(TRUE)
  }

# === Submit and run computation jobs ===
  #' Prepare a computation job for submission to a worker
  #'
  #' This function prepares the information needed to run one computation job in
  #' a worker. It creates the job directory, saves the potentially
  #' large computation inputs as an RDS file, and returns a smaller list of job
  #' instructions.
  #'
  #' The main Shiny process sends only this smaller instruction list to the
  #' worker. The worker then loads the full inputs from the saved RDS file. This
  #' avoids sending large R objects directly between processes.
  #'
  #' @param inputs Named list of compute-function arguments.
  #' @param compute_fn_name Character name of the compute function.
  #' @param tab_name Tab name for result storage.
  #' @param app_dir Root directory of the app.
  #' @param job_id Job ID.
  #' @param job_dir Directory where result and input files are stored.
  #' @param progress_file Temporary progress file path.
  #' @param input_file Optional input RDS path. If NULL, one is created in job_dir.
  #' @return A named list with job metadata and file paths.
  #' @export
  prepare_job <- function(inputs,
                          compute_fn_name,
                          tab_name,
                          app_dir = getwd(),
                          job_id = create_job_id(),
                          job_dir = get_job_dir(tab = tab_name),
                          progress_file = create_progress_filepath(job_id)$progress_file,
                          input_file = NULL) {
    dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.null(input_file)) {
      input_file <- file.path(job_dir, paste0(job_id, "_inputs.rds"))
    }

    saveRDS(inputs, file = input_file)

    list(
      app_dir         = normalizePath(app_dir, winslash = "/", mustWork = TRUE),
      job_id          = job_id,
      job_dir         = job_dir,
      input_file      = normalizePath(input_file, winslash = "/", mustWork = TRUE),
      progress_file   = progress_file,
      compute_fn_name = compute_fn_name,
      tab_name        = tab_name
    )
  }

  #' Run a computation job inside a worker
  #'
  #' This function runs one computation job inside a worker. It checks
  #' the job specification, loads the saved inputs from disk, finds the requested
  #' computation function, and runs that function.
  #'
  #' The computation function receives the saved inputs and the paths needed to
  #' report progress and save its result.
  #'
  #' @param job_spec Named list created by \code{prepare_job()}.
  #' @return Invisibly returns \code{TRUE}.
  #' @export
  run_job_in_worker <- function(job_spec) {
    required <- c(
      "app_dir",
      "job_id",
      "job_dir",
      "input_file",
      "progress_file",
      "compute_fn_name",
      "tab_name"
    )

    missing <- setdiff(required, names(job_spec))
    if (length(missing) > 0L) {
      stop(
        "run_job_in_worker() is missing required job_spec element(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }

    if (!file.exists(job_spec$input_file)) {
      stop(
        "run_job_in_worker() could not find input_file: ",
        job_spec$input_file,
        call. = FALSE
      )
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(job_spec$app_dir)

    inputs <- readRDS(job_spec$input_file)

    fn <- get(job_spec$compute_fn_name, envir = .GlobalEnv, mode = "function")

    do.call(
      fn,
      c(inputs,
        list(job_id        = job_spec$job_id,
             job_dir       = job_spec$job_dir,
             progress_file = job_spec$progress_file))
    )

    invisible(TRUE)
  }

  #' Submit a computation job to a worker
  #'
  #' This function sends a computation job from the main Shiny process to a
  #' worker. It first saves the information needed to track the job,
  #' such as the job ID and file paths. It then asks a worker to run the job and
  #' stores the returned handle so that the main Shiny process can monitor or
  #' cancel the job later.
  #'
  #' @param state ReactiveValues job state.
  #' @param job_spec Plain job specification from prepare_job().
  #' @return Invisibly returns the mirai handle.
  #' @export
  submit_job_to_worker <- function(state, job_spec) {
    state$job_id        <- job_spec$job_id
    state$job_dir       <- job_spec$job_dir
    state$input_file    <- job_spec$input_file
    state$progress_file <- job_spec$progress_file
    state$last_progress <- NULL
    state$status        <- "running"
    state$submitted_at  <- Sys.time()

    handle <- mirai::mirai(
      {
        run_job_in_worker(job_spec)
      },
      job_spec = job_spec
    )

    state$handle <- handle

    # Save the job's status and start the heartbeat clock
    write_job_status(
      job_dir      = job_spec$job_dir,
      job_id       = job_spec$job_id,
      status       = "running",
      submitted_at = shiny::isolate(state$status_submitted_at %||% state$submitted_at),
      percent      = 0
    )
    state$status_heartbeat_at <- Sys.time()
    state$status_percent <- 0

    cat(file = stderr(), paste0("Created computation job ", job_spec$job_id,
                               " at ", Sys.time(), "\n"))

    invisible(handle)
  }

# === Manage a running computation job ===
  #' Check whether a job value is an error
  #'
  #' This function checks whether a value returned through \code{job$data}
  #' represents an error rather than a successful result.
  #'
  #' @param value Object returned through \code{job$data}.
  #' @return Logical scalar.
  #' @export
  is_job_error <- function(value) {
    isTRUE(tryCatch(mirai::is_mirai_error(value), error = function(e) FALSE)) ||
      inherits(value, "miraiError") ||
      inherits(value, "errorValue") ||
      inherits(value, "error")
  }

  #' Get the error message from a finished job
  #'
  #' This function formats the error message from a finished job, or returns
  #' \code{NULL} if the job is still running or completed without error.
  #'
  #' @param job A finished \code{mirai} object.
  #' @return Character string, or \code{NULL}.
  #' @export
  job_error_message <- function(job) {
    if (is.null(job) || mirai::unresolved(job)) {
      return(NULL)
    }

    value <- job$data

    if (!is_job_error(value)) {
      return(NULL)
    }

    msg <- tryCatch(conditionMessage(value), error = function(e) NULL)

    if (!is.null(msg) && nzchar(msg)) {
      return(msg)
    }

    paste(utils::capture.output(print(value)), collapse = "\n")
  }

  #' Refresh the progress modal for a running job
  #'
  #' This function redraws the progress modal from the latest progress record.
  #' It redraws only when there is a new record and the modal is on screen. A
  #' record is identified by its percentage, message, and time, so an unchanged
  #' record does not cause a redraw.
  #'
  #' @param state A `reactiveValues` object created by `create_job_state()`.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param progress A progress record from `read_progress()`, or NULL.
  #' @param working_message Character. Message shown when the worker has not
  #'   written one of its own.
  #' @return Invisibly `TRUE` if the modal was redrawn.
  refresh_job_modal <- function(state, ns, session, progress, working_message) {
    if (is.null(progress) || !is_modal_open(session)) {
      return(invisible(FALSE))
    }

    progress_key <- paste(
      progress$percent,
      progress$message,
      progress$time,
      sep = "|"
    )

    if (identical(progress_key, state$last_progress)) {
      return(invisible(FALSE))
    }

    state$last_progress <- progress_key

    display_modal(
      ns      = ns,
      message = progress$message %||% working_message,
      value   = progress$percent
    )

    invisible(TRUE)
  }

  #' Rewrite the status file of a job at a fixed interval
  #'
  #' This function rewrites the status file of a running job on a timer. The
  #' History tab treats a job whose status file has stopped changing as
  #' interrupted, so a running job has to keep reporting.
  #'
  #' @param state A `reactiveValues` object created by `create_job_state()`.
  #' @param now The current time.
  #' @param heartbeat_seconds Minimum number of seconds between updates.
  #' @return Invisibly `TRUE` if the status file was written.
  rewrite_job_status <- function(state, now, heartbeat_seconds) {
    last_heartbeat <- state$status_heartbeat_at

    due <- is.null(last_heartbeat) ||
      as.numeric(difftime(now, last_heartbeat, units = "secs")) >= heartbeat_seconds

    if (!isTRUE(due)) {
      return(invisible(FALSE))
    }

    write_job_status(
      job_dir      = state$job_dir,
      job_id       = state$job_id,
      status       = "running",
      submitted_at = state$status_submitted_at %||% state$submitted_at,
      percent      = state$status_percent
    )

    state$status_heartbeat_at <- now
    invisible(TRUE)
  }

  #' Get the final outcome of a job
  #'
  #' This function decides whether a finished job completed or failed. A job
  #' that reported an error failed. A job with no error but no saved result also
  #' failed, because the worker stopped before writing its result.
  #'
  #' @param state A `reactiveValues` object created by `create_job_state()`.
  #' @return A list with elements `status` and `error_message`.
  get_job_outcome <- function(state) {
    # Check for a worker error
    error_message <- job_error_message(state$handle)

    if (!is.null(error_message)) {
      return(list(status = "error", error_message = error_message))
    }

    # Check for the saved result
    result_path <- file.path(
      state$job_dir,
      paste0(state$job_id, ".rds")
    )

    if (!file.exists(result_path)) {
      return(list(
        status = "error",
        error_message = paste0(
          "Prediction finished, but the result file was not found: ",
          result_path
        )
      ))
    }

    list(status = "completed", error_message = NULL)
  }

  #' Manage a running computation job
  #'
  #' This is the main function for managing a computation job
  #' It checks for an active worker, reads progress, refreshes the modal,
  #' writes heartbeat updates, and responds to cancellation requests.
  #'
  #' When the worker stops, the function determines whether the job completed or
  #' failed and passes that result to `end_job()`.
  #'
  #' The job is read once at the start of each call, so every branch below sees
  #' the same view of the job.
  #'
  #' @param state A `reactiveValues` object created by `create_job_state()`.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param tab_name Character. The name of the app tab that started the job.
  #' @param session The Shiny session object.
  #' @param working_message Character. Default modal message shown before the
  #'   worker writes its first progress message.
  #' @param heartbeat_seconds Numeric. Minimum number of seconds between updates
  #'   to the running status file.
  #' @return Invisibly `TRUE` if the job remains active, otherwise `FALSE`.
  #' @export
  manage_job <- function(state,
                          ns,
                          tab_name,
                          session = shiny::getDefaultReactiveDomain(),
                          working_message = "Prediction in progress",
                          heartbeat_seconds = 5) {
    # Stop if there is no active worker
    if (is.null(state$handle)) {
      return(invisible(FALSE))
    }

    # Get progress
    now <- Sys.time()
    progress <- read_progress(state$progress_file)
    running <- tryCatch(
      !is.null(state$handle) && isTRUE(mirai::unresolved(state$handle)),
      error = function(e) FALSE
    )
    cancel_file <- get_cancel_filepath(state$job_dir, state$job_id)
    cancelled <- file.exists(cancel_file)

    # Record progress
    if (!is.null(progress$percent)) {
      state$status_percent <- progress$percent
    }

    # Manage jobs that are running
    if (isTRUE(running)) {
      
      # Stop jobs that are being cancelled
      if (isTRUE(cancelled)) {
        end_job(
          state        = state,
          status       = "cancelled",
          ns           = ns,
          session      = session,
          remove_modal = TRUE,
          modal_delay  = 500
        )

        return(invisible(FALSE))
      }

      # Update the modal and the status file of running jobs
      refresh_job_modal(state, ns, session, progress, working_message)
      rewrite_job_status(state, now, heartbeat_seconds)

      return(invisible(TRUE))
    }

    # End jobs that are no longer running
    outcome <- get_job_outcome(state)

    end_job(
      state         = state,
      status        = outcome$status,
      ns            = ns,
      tab_name      = if (identical(outcome$status, "completed")) tab_name else NULL,
      session       = session,
      error_message = outcome$error_message,
      remove_modal  = TRUE,
      update_url    = identical(outcome$status, "completed")
    )

    invisible(FALSE)
  }

# === Stop and clean up jobs ===
  # Return the cancel-request path for a job.
  get_cancel_filepath <- function(job_dir, job_id) {
    file.path(job_dir, paste0(job_id, ".cancel.rds"))
  }

  #' Request cancellation of a computation job
  #'
  #' This function writes a small marker file into a job's directory to request
  #' that the session owning the job stop it. The owning session sees the marker
  #' the next time it checks the job and performs the cancellation.
  #'
  #' If the owning session is no longer running, the request is never acted on.
  #' In that case the job becomes interrupted on its own once its status file
  #' stops being updated, and the marker is removed with the rest of the job.
  #'
  #' @param job_dir Path to the job directory.
  #' @param job_id Job ID string.
  #' @param requested_by Optional identifier of the user making the request.
  #' @return Invisibly returns the path to the cancel-request file.
  #' @export
  request_job_cancel <- function(job_dir, job_id, requested_by = NA) {
    dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

    record <- list(
      requested_by = requested_by,
      requested_at = Sys.time()
    )

    # Write to a temporary file and rename so readers never see a partial file.
    save_object_safely(record, get_cancel_filepath(job_dir, job_id))
  }

  #' Reset the local state for a computation job
  #'
  #' Removes temporary files and clears the module's local job state. It does
  #' not stop an active worker or write a final status.
  #'
  #' @param state ReactiveValues job state.
  #' @return Invisibly NULL.
  #' @export
  reset_job <- function(state) {
    job_id <- shiny::isolate(state$job_id)
    job_dir <- shiny::isolate(state$job_dir)
    input_file <- shiny::isolate(state$input_file)
    progress_file <- shiny::isolate(state$progress_file)

    if (!is.null(progress_file) && file.exists(progress_file)) {
      unlink(progress_file, recursive = TRUE, force = TRUE)
    }

    if (!is.null(input_file) && file.exists(input_file)) {
      unlink(input_file, recursive = TRUE, force = TRUE)
    }

    if (!is.null(job_dir) && !is.null(job_id)) {
      cancel_file <- get_cancel_filepath(job_dir, job_id)
      if (file.exists(cancel_file)) {
        unlink(cancel_file, force = TRUE)
      }
    }

    clear_job_state(state)
    invisible(NULL)
  }

  #' Close the progress modal for a job
  #'
  #' This function closes the progress modal. The modal is removed with its
  #' progress animation when a namespace function is available, and removed
  #' directly otherwise. The app-wide record of whether a modal is open is
  #' cleared either way.
  #'
  #' @param session The Shiny session object.
  #' @param ns Function used to apply the module namespace to input IDs, or NULL
  #'   when the modal has already been removed.
  #' @param modal_delay Number of milliseconds to wait before closing the modal.
  #' @return Invisibly NULL.
  close_job_modal <- function(session, ns = NULL, modal_delay = 0) {
    if (is.function(ns)) {
      hide_modal_with_progress(
        session    = session,
        ns         = ns,
        delay_time = modal_delay
      )
    } else {
      shiny::removeModal()
    }

    set_modal_open(session, FALSE)
    invisible(NULL)
  }

  #' End a computation job
  #'
  #' This function records a final job status and clears the resources used by
  #' one job. It handles completed, failed, cancelled, and interrupted jobs from
  #' both the submission and worker stages.
  #'
  #' A running worker is stopped before its files and local state are cleared.
  #' A final status already saved on disk is preserved by
  #' `finalize_job_status()`, so later module cleanup cannot replace a completed
  #' or failed result with "interrupted".
  #'
  #' @param state ReactiveValues job state.
  #' @param status Final status: "completed", "error", "cancelled", or
  #'   "interrupted".
  #' @param ns Function used to apply the module namespace to input IDs, or NULL
  #'   when the modal has already been removed.
  #' @param tab_name Character. App tab used to create the completed-job URL.
  #' @param session The Shiny session object.
  #' @param error_message Error message for a failed job, or NULL.
  #' @param percent Final percentage, or NULL to use 100 for a completed job and
  #'   the last recorded percentage otherwise.
  #' @param remove_modal Logical. Whether to close the progress modal.
  #' @param modal_delay Number of milliseconds to wait before closing the modal.
  #' @param update_url Logical. Whether to place the completed job in the URL.
  #' @param overwrite_final Logical. Whether to replace a status that is already
  #'   final.
  #' @return Invisibly the status that was written, or NULL when the status on
  #'   disk was kept.
  #' @export
  end_job <- function(state,
                      status,
                      ns = NULL,
                      tab_name = NULL,
                      session = shiny::getDefaultReactiveDomain(),
                      error_message = NULL,
                      percent = NULL,
                      remove_modal = TRUE,
                      modal_delay = 0,
                      update_url = FALSE,
                      overwrite_final = FALSE) {
    if (!status %in% job_final_statuses) {
      stop(
        paste0(
          "end_job() requires status to be one of: ",
          paste(job_final_statuses, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    # Save the current state before clearing it.
    job_id <- shiny::isolate(state$job_id)
    job_dir <- shiny::isolate(state$job_dir)
    handle <- shiny::isolate(state$handle)
    submitted_at <- shiny::isolate(state$status_submitted_at) %||%
      shiny::isolate(state$submitted_at)

    cat(file = stderr(), paste0("Ended computation job ", job_id,
                               " at ", Sys.time(), " (", status, ")\n"))

    if (is.null(percent)) {
      percent <- if (identical(status, "completed")) {
        100
      } else {
        shiny::isolate(state$status_percent)
      }
    }

    # Stop the worker when the job is still running.
    running <- !is.null(handle) &&
      isTRUE(tryCatch(mirai::unresolved(handle), error = function(e) FALSE))

    if (running) {
      tryCatch(
        mirai::stop_mirai(handle),
        error = function(e) FALSE
      )
    }

    # Record the final status
    written <- finalize_job_status(
      job_dir         = job_dir,
      job_id          = job_id,
      status          = status,
      submitted_at    = submitted_at,
      error_message   = error_message,
      percent         = percent,
      overwrite_final = overwrite_final
    )

    # Point the browser to the saved result only after successful completion.
    if (isTRUE(update_url) &&
        identical(status, "completed") &&
        !is.null(tab_name) &&
        !is.null(job_id)) {
      url <- create_job_url(
        session = session,
        job_id  = job_id,
        tab     = tab_name
      )

      shiny::updateQueryString(
        sub(".*\\?", "?", url),
        mode    = "push",
        session = session
      )
    }

    # Close the modal before clearing the local state
    if (isTRUE(remove_modal)) {
      close_job_modal(
        session     = session,
        ns          = ns,
        modal_delay = modal_delay
      )
    }

    reset_job(state)
    invisible(written)
  }

  #' Destroy the observers used for computation jobs
  #'
  #' This function destroys the observers used to submit, cancel, and update jobs
  #' for one module. It is called when the module is unloaded so the observers do
  #' not keep running after the module has closed.
  #'
  #' @param state ReactiveValues job state.
  #' @return Invisibly NULL.
  #' @export
  destroy_job_observers <- function(state) {
    for (slot in c("obs_update", "obs_submit", "obs_cancel")) {
      observer <- shiny::isolate(state[[slot]])

      if (!is.null(observer) && inherits(observer, "Observer")) {
        tryCatch(
          observer$destroy(),
          error = function(e) NULL
        )
      }

      state[[slot]] <- NULL
    }

    invisible(NULL)
  }

  #' Catch jobs interrupted by user
  #'
  #' This function listens for the notification sent when browser navigation
  #' closes a progress modal. It then calls the cleanup function registered by
  #' each module so any active job can be stopped and cleared.
  #'
  #' Removing the modal from the screen does not stop the R job by itself. The
  #' registered cleanup functions handle that work. After the callbacks run,
  #' this function records that no modal is open.
  #'
  #' @param session The Shiny session object.
  #' @param input The Shiny input object.
  #' @return NULL. Called for side effects.
  #' @export
  catch_interrupted_jobs <- function(session = shiny::getDefaultReactiveDomain(),
                                     input = shiny::getDefaultReactiveDomain()$input) {
    shiny::observeEvent(input$nav_modal_force_closed, {

      event <- input$nav_modal_force_closed
      shiny::req(event)

      callbacks <- session$userData$navigation_job_cleanup %||% list()

      for (tab_name in names(callbacks)) {
        cb <- callbacks[[tab_name]]

        if (is.function(cb)) {
          tryCatch(
            cb(event),
            error = function(e) {
            }
          )
        }
      }

      # Reset modal_open reactiveVal app-wide (set up in app.R server fn).
      set_modal_open(session, FALSE)
    }, ignoreInit = TRUE)
  }

  #' Register cleanup for a module's computation jobs
  #'
  #' This function registers the cleanup procedures used when a module closes or
  #' when browser navigation interrupts an active computation job.
  #'
  #' When the module closes, the cleanup procedure removes the browser-navigation
  #' callback, stops any active job, closes the progress modal, and destroys the
  #' module's job observers.
  #'
  #' When browser navigation closes the progress modal, the cleanup procedure
  #' stops any active job but leaves the module observers in place. JavaScript
  #' has already removed the modal from the screen.
  #'
  #' @param state ReactiveValues job state.
  #' @param session The Shiny session object.
  #' @param tab_name Character. The name of the app tab that owns the job.
  #' @return Invisibly NULL.
  #' @export
  register_job_cleanup <- function(state, session, tab_name) {
    if (is.null(session$userData$module_cleanup)) {
      session$userData$module_cleanup <- list()
    }

    if (is.null(session$userData$navigation_job_cleanup)) {
      session$userData$navigation_job_cleanup <- list()
    }

    # Clean up the job and its observers when the module is unloaded.
    session$userData$module_cleanup[[tab_name]] <- function() {
      session$userData$navigation_job_cleanup[[tab_name]] <- NULL

      end_job(
        state        = state,
        status       = "interrupted",
        session      = session,
        remove_modal = TRUE
      )

      destroy_job_observers(state)
    }

    # Finish the job when browser navigation has already removed the modal.
    session$userData$navigation_job_cleanup[[tab_name]] <- function(event) {
      end_job(
        state        = state,
        status       = "interrupted",
        session      = session,
        remove_modal = FALSE
      )
    }

    invisible(NULL)
  }

# === Perform computations ===
  #' Start a computation job
  #'
  #' Creates the job record and progress modal immediately, then waits for the
  #' browser to receive the modal before collecting inputs and submitting the
  #' job to a worker.
  #'
  #' @param state ReactiveValues job state.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param get_inputs Function that returns the module-specific inputs.
  #' @param compute_fn_name Name of the worker computation function.
  #' @param tab_name Name of the app tab that owns the job.
  #' @param submit_message Message shown while inputs are prepared.
  #' @return Invisibly NULL.
  #' @export
  start_job <- function(state,
                        ns,
                        session,
                        get_inputs,
                        compute_fn_name,
                        tab_name,
                        submit_message) {
    handle <- shiny::isolate(state$handle)
    status <- shiny::isolate(state$status)

    if (!is.null(handle) || identical(status, "preparing")) {
      return(invisible(NULL))
    }

    reset_job(state)

    job_id <- create_job_id()
    files <- create_progress_filepath(job_id)
    job_dir <- get_job_dir(tab = tab_name)
    submitted_at <- Sys.time()

    state$job_id <- job_id
    state$job_dir <- job_dir
    state$progress_file <- files$progress_file
    state$status <- "preparing"
    state$submitted_at <- submitted_at
    state$last_progress <- NULL
    state$status_submitted_at <- submitted_at
    state$status_heartbeat_at <- NULL
    state$status_percent <- 0

    write_job_status(
      job_dir      = job_dir,
      job_id       = job_id,
      status       = "submitted",
      submitted_at = submitted_at,
      percent      = 0
    )

    display_modal(
      ns              = ns,
      message         = submit_message,
      value           = 0,
      url             = create_job_url(job_id = job_id, tab = tab_name),
      cancel_input_id = ns("cancel_job")
    )

    # Collect inputs only after the modal has reached the browser.
    session$onFlushed(
      function() {
        if (!identical(shiny::isolate(state$job_id), job_id)) {
          return(invisible(NULL))
        }

        inputs <- tryCatch(
          shiny::isolate(get_inputs()),
          error = identity
        )

        final_status <- NULL
        error_message <- NULL

        if (inherits(inputs, "error")) {
          final_status <- "error"
          error_message <- conditionMessage(inputs)
        } else if (is.null(inputs)) {
          final_status <- "cancelled"
        } else {
          cancel_file <- get_cancel_filepath(job_dir, job_id)
          if (file.exists(cancel_file)) {
            unlink(cancel_file, force = TRUE)
            final_status <- "cancelled"
          }
        }

        if (!is.null(final_status)) {
          end_job(
            state         = state,
            status        = final_status,
            ns            = ns,
            session       = session,
            error_message = error_message,
            percent       = 0
          )

          return(invisible(NULL))
        }

        job_spec <- prepare_job(
          inputs          = inputs,
          compute_fn_name = compute_fn_name,
          tab_name        = tab_name,
          app_dir         = getwd(),
          job_id          = job_id,
          job_dir         = job_dir,
          progress_file   = files$progress_file
        )

        setup_workers(app_dir = getwd())
        submit_job_to_worker(state = state, job_spec = job_spec)

        invisible(NULL)
      },
      once = TRUE
    )

    invisible(NULL)
  }

  #' Create the observer for the Cancel button
  #'
  #' This function creates the observer that responds when the user clicks
  #' Cancel. It writes a cancel-request marker so the regular update can
  #' finish the job.
  #'
  #' @param state ReactiveValues job state.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param tab_name Character. The name of the app tab that owns the job.
  #' @param cancel_message Character. Message shown when cancellation is requested.
  #' @return The observer handle.
  #' @export
  observe_job_cancellation <- function(state,
                                         ns,
                                         session,
                                         tab_name,
                                         cancel_message) {
    input <- session$input

    shiny::observeEvent(
      input$cancel_job,
      {
        job_id <- shiny::isolate(state$job_id)
        job_dir <- shiny::isolate(state$job_dir)

        # Ignore Cancel when the module has no active job.
        if (is.null(job_id) && is.null(shiny::isolate(state$handle))) {
          return(invisible(NULL))
        }

        # Leave a request for the polling observer to handle.
        display_modal(
          ns      = ns,
          message = cancel_message,
          value   = 100
        )

        request_job_cancel(
          job_dir      = job_dir,
          job_id       = job_id,
          requested_by = session$userData$user_id()
        )
      },
      ignoreInit = TRUE,
      label = paste0(tab_name, "_cancel_job")
    )
  }

  #' Create the observer for submitted jobs
  #'
  #' This function creates the observer that begins job submission when the user
  #' clicks the module's Submit button.
  #'
  #' @param state ReactiveValues job state.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param submit_button Character. Input ID of the Submit button.
  #' @param get_inputs Function. Collects and returns the module-specific inputs.
  #' @param compute_fn_name Character. Name of the worker computation function.
  #' @param tab_name Character. The name of the app tab that owns the job.
  #' @param submit_message Character. Message shown while inputs are prepared.
  #' @return The observer handle.
  #' @export
  observe_job_submission <- function(state,
                                         ns,
                                         session,
                                         submit_button,
                                         get_inputs,
                                         compute_fn_name,
                                         tab_name,
                                         submit_message) {
    input <- session$input

    shiny::observeEvent(
      input[[submit_button]],
      {
        start_job(
          state           = state,
          ns              = ns,
          session         = session,
          get_inputs      = get_inputs,
          compute_fn_name = compute_fn_name,
          tab_name        = tab_name,
          submit_message  = submit_message
        )
      },
      ignoreInit = TRUE,
      label = paste0(tab_name, "_submit_job")
    )
  }

  #' Create the observer that updates a running job
  #'
  #' This function creates the observer that regularly checks a worker for
  #' progress updates, cancellation requests, errors, and completion.
  #'
  #' @param state ReactiveValues job state.
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param tab_name Character. The name of the app tab that owns the job.
  #' @param working_message Character. Default message shown while the worker runs.
  #' @return The observer handle.
  #' @export
  observe_job_management <- function(state,
                                         ns,
                                         session,
                                         tab_name,
                                         working_message) {
    shiny::observe(
      {
        shiny::invalidateLater(250, session)

        # Wait until input preparation has finished.
        if (identical(state$status, "preparing")) {
          return(invisible(NULL))
        }

        # Do nothing when the module has no active worker.
        if (is.null(state$handle)) {
          return(invisible(NULL))
        }

        manage_job(
          state           = state,
          ns              = ns,
          tab_name        = tab_name,
          session         = session,
          working_message = working_message
        )

        invisible(NULL)
      },
      label = paste0(tab_name, "_manage_job")
    )
  }

  #' Set up computation jobs for a module
  #'
  #' This is the main function for executing computation jobs. It
  #' creates the job state, registers cleanup procedures, and creates observers
  #' for submission, cancellation, and updates.
  #'
  #' @param ns Function used to apply the module namespace to input IDs.
  #' @param session The Shiny session object.
  #' @param submit_button Character. The input ID of the button used to start the
  #'   computation.
  #' @param get_inputs Function. Collects and returns the module-specific inputs
  #'   as a named list.
  #' @param compute_fn_name Character. The name of the computation function that
  #'   should run inside the worker.
  #' @param tab_name Character. The name of the app tab that owns the job.
  #' @param submit_message Character. Message shown as soon as the user submits
  #'   the job.
  #' @param working_message Character. Default modal message shown while the
  #'   worker runs.
  #' @param cancel_message Character. Message shown when the user cancels the
  #'   job.
  #' @return The `reactiveValues` object used to track the module's active job.
  #' @export
  setup_computation_jobs <- function(
    ns,
    session,
    submit_button = NULL,
    get_inputs,
    compute_fn_name,
    tab_name,
    submit_message  = "Creating job for computation",
    working_message = "Prediction in progress",
    cancel_message  = "Prediction canceled"
  ) {
    if (is.null(submit_button)) {
      stop("setup_computation_jobs() requires submit_button.")
    }

    # Create the shared state used by this module's job helpers.
    state <- create_job_state()

    # Register cleanup for jobs interrupted by user
    register_job_cleanup(
      state    = state,
      session  = session,
      tab_name = tab_name
    )

    # Create the observers used while the module is active.
    state$obs_cancel <- observe_job_cancellation(
      state          = state,
      ns             = ns,
      session        = session,
      tab_name       = tab_name,
      cancel_message = cancel_message
    )

    state$obs_submit <- observe_job_submission(
      state           = state,
      ns              = ns,
      session         = session,
      submit_button   = submit_button,
      get_inputs      = get_inputs,
      compute_fn_name = compute_fn_name,
      tab_name        = tab_name,
      submit_message  = submit_message
    )

    state$obs_update <- observe_job_management(
      state           = state,
      ns              = ns,
      session         = session,
      tab_name        = tab_name,
      working_message = working_message
    )

    # Return the state so the module can read its active job.
    return(state)
  }

#' # User Interface Functions for App
#' 
#' This script defines functions for creating the user interface (UI) components 
#' of the Shiny app. These functions modularize common UI elements to improve 
#' readability, maintainability, and consistency across modules.
#' 
#' @author Timothy Hackmann
#' @date 22 February 2025

#' Create Loading Screen with Fading SVG
#'
#' Generates a centered loading screen that fades an SVG logo and text in a loop,
#' with customizable delay before the first fade and navbar offset.
#'
#' @param id A string specifying the HTML ID for the loading screen div.
#' @param navbar_height_px Estimated height of the navbar in pixels (default = 165).
#' @param fade_delay_sec Time to wait before first fade-in, in seconds (default = 0.2).
#'
#' @return A `div` tag containing the loading screen and inline animation CSS.
#' @export
create_loading_screen <- function(id,
                                  navbar_height_px = 165,
                                  fade_delay_sec = 0.2) {
  shiny::tagList(
    # Inject animation CSS with customizable delay
    tags$style(HTML(sprintf("
      .fade-svg {
        opacity: 0;
        animation-name: fadeInOut;
        animation-duration: 2s;
        animation-delay: %.1fs;
        animation-iteration-count: infinite;
        animation-timing-function: ease-in-out;
        animation-fill-mode: forwards;
      }

      @keyframes fadeInOut {
        0%%   { opacity: 0.2; }
        50%%  { opacity: 1; }
        100%% { opacity: 0.2; }
      }
    ", fade_delay_sec))),
    
    # The loading screen
    shiny::div(
      id = id,
      style = paste(
        "display: flex;",
        "justify-content: center;",
        "align-items: center;",
        "flex-direction: column;",
        sprintf("height: calc(100vh - %dpx);", navbar_height_px),
        "width: 100%;",
        "text-align: center;",
        "margin-top: 0;"
      ),
      tags$img(
        src = 'MicrobeDecoderLogo.svg',
        width = 150,
        class = "fade-svg"
      ),
      shiny::p(
        "Loading",
        style = "color: grey; font-size: 30px;",
        class = "fade-svg"
      ),
      tags$span(class = "visually-hidden", "Loading...")
    )
  )
}

#' Create a Query Builder Input
#' 
#' This function generates a standardized `jqbr::queryBuilderInput` element with customizable 
#' input ID, dynamically loaded filters, and rules.
#' 
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param input_id A character string specifying the ID of the query builder input.
#' @param rules A list of default rules for the query builder. If `NULL`, a default rule is used.
#' @param label (Optional) A label or title to be displayed above the query builder.
#' @return A `div` containing the query builder input.
#' 
#' @examples
#' create_query_builder(ns, "query_builder", query_rules_search, "Build query")
#' create_query_builder(ns, "query_builder", query_rules_taxonomy)
#' create_query_builder(ns, "query_builder", query_rules_ML, "Machine Learning Query")
create_query_builder <- function(ns, 
                                 input_id, 
                                 rules = NULL,
                                 label = NULL) {
  filters <- load_data("placeholder_filters")

  if (is.null(rules)) {
    rules <- list(
      condition = "AND",
      rules = list(
        list(id = "Temperature for growth in degrees (BacDive)",
             operator = "greater",
             value = "39"
            )
        )
    )
  }
  
  div(
    if (!is.null(label)) div(label),
    jqbr::queryBuilderInput(
      inputId = ns(input_id),
      filters = filters,
      return_value = "r_rules",
      display_errors = TRUE,
      rules = rules,
      add_na_filter = FALSE
    )
  )
}


#' Create Loading Spinner with Default Color
#'
#' This function wraps `shinycssloaders::withSpinner()` and applies a default spinner color.
#'
#' @param ui_element The UI element to wrap with a spinner.
#' @param color The color of the spinner. Default is `"#3C8DBC"`.
#' @param proxy.height Height of the spinner proxy element shown while the UI element loads. Default is `400`.
#' @param use_fill_carrier Logical indicating whether to apply `bslib::as_fill_carrier()`. Default is `TRUE`.
#' @param fires_once Logical indicating whether the spinner should show on the first render only. Default is `FALSE`.
#' @param ... Additional arguments passed to `shinycssloaders::withSpinner()`.
#' @return The UI element wrapped with a loading spinner.
#'
#' @examples
#' add_spinner(plotly::plotlyOutput("plot"))
#' add_spinner(DT::dataTableOutput("table"))
add_spinner <- function(ui_element, color = "#3C8DBC", proxy.height = 400, use_fill_carrier = TRUE,
                        fires_once = FALSE, ...) {
  spinner <- shinycssloaders::withSpinner(ui_element, color = color, proxy.height = proxy.height,
                                          hide.ui = !fires_once, ...)
  
  # Mark the spinner so it shows on the first render only
  if (fires_once) {
    spinner <- htmltools::tagAppendAttributes(spinner, `data-spin-once` = "true")
  }
  
  if (use_fill_carrier) {
    spinner <- bslib::as_fill_carrier(spinner)
  }
  
  spinner
}

#' Create a Plot Container
#' 
#' This function generates a standardized plot container with a loading spinner
#' 
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param plot_type A character string specifying the type of plot (e.g., "treemap", "heatmap").
#' @param width The width of the plot. Default is `"100%"`.
#' @param height The height of the plot (default: "40vh").
#' 
#' @return A `div` container for the plot.
#' 
#' @examples
#' create_plot_div(ns, "treemap")
#' create_plot_div(ns, "heatmap", width = "80%", height = "50vh")
create_plot_div <- function(ns, plot_type, width = "100%", height = "40vh") {
  div(
    id = ns(paste0(plot_type, "-container")),
    class = paste0(plot_type, "-container-style"),
    plotly::plotlyOutput(ns(paste0(plot_type, "_plot")), width = width, height = height)
  )
}

#' Create a Plot Output with Optional Spinner
#'
#' This function wraps `plotly::plotlyOutput()` and optionally applies a spinner using `add_spinner()`.
#'
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param inputId The input ID for the plot.
#' @param width The width of the plot. Default is `"100%"`.
#' @param height The height of the plot (default: "100%").
#' @param use_spinner Logical, whether to wrap the output in a loading spinner (default: TRUE).
#' @return A `plotlyOutput`, optionally wrapped with a loading spinner.
#'
#' @examples
#' create_plot_output(ns, ns("plot"))
#' create_plot_output(ns, ns("plot"), use_spinner = FALSE)
#' create_plot_output(ns, ns("plot"), width = "80%", height = "400px")
create_plot_output <- function(ns, inputId, width = "100%", height = "100%", use_spinner = TRUE) {
  plot_output <- plotly::plotlyOutput(inputId, width = width, height = height)
  
  if (use_spinner) {
    plot_output <- add_spinner(plot_output, use_fill_carrier = TRUE, fires_once = TRUE)
  }
  
  plot_output
}

#' Create a Navigation Panel with Optional Empty-State Message
#'
#' This function wraps arbitrary content in a `bslib::card()` inside a
#' `bslib::nav_panel()`, so any content can be shown as a tab in a navset.
#' When `flag_id` is given, the content is shown only while that boolean output
#' is TRUE, and a plain-text message is shown in its place otherwise.  The
#' content is not rendered at all while hidden, so no empty plot area or table
#' is drawn behind the message.
#'
#' @param ns A namespace function for module compatibility.
#' @param title The tab title.
#' @param ... Content to place inside the card.
#' @param max_height The maximum height of the card (default: "50vh"). Use "none"
#'   for content that should grow to its natural height.
#' @param centered Logical, whether to center the content inside the card (default: FALSE).
#' @param full_screen Logical, whether to allow the card to be expanded full-screen (default: TRUE).
#' @param fill Logical, whether the content should stretch to fill the card (default: TRUE).
#'   Aspect-ratio plots (heatmap, treemap) fill the card; content-height panels
#'   set `fill = FALSE` so the content keeps its natural height and the card scrolls.
#' @param flag_id Name of a boolean output (for example "flag_tree") that controls whether
#'   the content or the message is shown (default: NULL, always show the content).
#' @param message_title Bold first line of the message shown when `flag_id` is FALSE.
#' @param message_body Instruction line shown below `message_title`.
#'
#' @return A `nav_panel` containing a `bslib::card`.
#'
#' @examples
#' create_nav_panel(ns, "Notes", div("Some content"))
#' create_nav_panel(ns, "Model evaluation", div("..."), flag_id = "flag_models",
#'                  message_title = "No model available",
#'                  message_body = "Please enable saving of models and re-run predictions.")
create_nav_panel <- function(ns, title, ..., max_height = "50vh", centered = FALSE,
                             full_screen = TRUE, fill = TRUE,
                             flag_id = NULL, message_title = NULL, message_body = NULL) {
  # Define the base style
  base_style <- paste0("max-height: ", max_height, 
                       "; overflow-y: auto; border: none; box-shadow: none;")
  
  # Add flex centering if requested
  if (centered) {
    base_style <- paste0(base_style, " display: flex; align-items: center; justify-content: center;")
  }
  
  # Collect the content
  content <- shiny::tagList(...)
  
  # Fill panels stretch to the card; content-height panels keep their own height
  # and let the card scroll.  as_fill_carrier needs a single tag, so a list of
  # several elements is wrapped in a div first.
  if (fill) {
    if (!inherits(content, "shiny.tag")) {
      content <- if (length(content) == 1) content[[1]] else div(content)
    }
    content <- content |> bslib::as_fill_carrier()
  }
  
  # Show the content only when the flag output is TRUE.  The panel doing the
  # hiding sits between the card and the content, so it has to stretch as well.
  # Otherwise it keeps its own height and the content has nothing to fill.
  if (!is.null(flag_id)) {
    content <- shiny::conditionalPanel(
      condition = paste0("output.", flag_id),
      ns = ns,
      content
    )
    
    if (fill) {
      content <- content |> bslib::as_fill_carrier()
    }
  }
  
  # Build the message shown in place of the content
  message_panel <- NULL
  if (!is.null(flag_id)) {
    message_panel <- shiny::conditionalPanel(
      condition = paste0("!output.", flag_id),
      ns = ns,
      div(
        class = "plot-message",
        if (!is.null(message_title)) div(message_title, class = "plot-message-title"),
        if (!is.null(message_body)) div(message_body)
      )
    )
  }
  
  bslib::nav_panel(
    title = title,
    bslib::card(
      full_screen = full_screen,
      style = base_style,
      content,
      message_panel
    )
  )
}

#' Create a Plot Panel
#'
#' This function builds the plot output and its resizable container, then hands
#' them to `create_nav_panel()`.  It is a thin wrapper kept for the common case
#' of a tab holding a single plot.
#'
#' @param ns A namespace function for module compatibility.
#' @param plot_type A string naming the plot (used for the output ID and container class).
#' @param title The tab title.
#' @param width The width of the plot (default: "100%").
#' @param height The height of the plot (default: "100%").
#' @param max_height The maximum height of the card (default: "50vh").
#' @param centered Logical, whether to center the plot inside the card (default: FALSE).
#' @param use_spinner Logical, whether to wrap the plot in a loading spinner (default: TRUE).
#' @param full_screen Logical, whether to allow the card to be expanded full-screen (default: TRUE).
#' @param fill Logical, whether the plot should fill the card (default: TRUE).
#' @param flag_id Name of a boolean output controlling whether the plot or the message
#'   is shown (default: NULL, always show the plot).
#' @param message_title Bold first line of the message shown when `flag_id` is FALSE.
#' @param message_body Instruction line shown below `message_title`.
#'
#' @return A `nav_panel` containing a `bslib::card` with a `plotlyOutput`.
#'
#' @examples
#' create_plot_panel(ns, "summary", "Summary")
#' create_plot_panel(ns, "treemap", "Treemap", max_height = "60vh", centered = TRUE)
create_plot_panel <- function(ns, plot_type, title, width = "100%", height = "100%", 
                              max_height = "50vh", centered = FALSE, 
                              use_spinner = TRUE, full_screen = TRUE, fill = TRUE,
                              flag_id = NULL, message_title = NULL, message_body = NULL) {
  # Define the container ID
  container_id <- ns(paste0(plot_type, "-container"))
  
  # Create the plot output
  plot_output <- create_plot_output(ns, ns(paste0(plot_type, "_plot")),
                                    width = width, height = height,
                                    use_spinner = use_spinner)
  
  # Wrap plot in a resizable div container
  plot_container <- div(
    id = container_id,
    class = paste0(plot_type, "-container-style"),
    plot_output
  )
  
  create_nav_panel(ns, title = title, plot_container,
                   max_height = max_height, centered = centered,
                   full_screen = full_screen, fill = fill,
                   flag_id = flag_id, message_title = message_title,
                   message_body = message_body)
}

#' Create a DataTable with Optional Spinner
#'
#' This function wraps `DT::dataTableOutput()` and optionally applies a spinner using `add_spinner()`.
#'
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param inputId The input ID for the data table.
#' @param width The width of the table. Default is `"100%"`.
#' @param height The height of the table (default: "auto").
#' @param use_spinner Logical, whether to wrap the output in a loading spinner (default: TRUE).
#' @return A `dataTableOutput`, optionally wrapped with a loading spinner.
#'
#' @examples
#' create_data_table(ns, ns("table"))
#' create_data_table(ns, ns("table"), use_spinner = FALSE)
#' create_data_table(ns, ns("table"), width = "80%", height = "400px")
create_data_table <- function(ns, inputId, width = "100%", height = "auto", use_spinner = TRUE) {
  table_output <- DT::dataTableOutput(inputId, width = width, height = height)
  
  if (use_spinner) {
    table_output <- add_spinner(table_output, use_fill_carrier = FALSE, fires_once = TRUE)
  }
  
  table_output
}

#' Create a Table Panel
#' 
#' This function generates a standardized nav panel with a data table inside a styled card.
#' Its structure mirrors create_plot_panel().
#' 
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param table_type A character string specifying the type of table (e.g., "matching_organisms").
#'   Used as the prefix for the container ID and the table output ID (`{table_type}_table`).
#' @param title The title of the navigation panel.
#' @param width The width of the table. Default is `"100%"`.
#' @param height The height of the table (default: "auto").
#' @param max_height The maximum height of the card (default: "50vh").
#' @param use_spinner Logical, whether to wrap the table in a loading spinner (default: TRUE).
#' @param full_screen Logical, whether to allow the card to be expanded full-screen (default: TRUE).
#' 
#' @return A `nav_panel` containing a `bslib::card` with a `dataTableOutput`.
#' 
#' @examples
#' create_table_panel(ns, "matching_organisms", "Database matches")
#' create_table_panel(ns, "matching_organisms", "Database matches", use_spinner = FALSE)
create_table_panel <- function(ns, table_type, title, width = "100%", height = "auto",
                               max_height = "50vh",
                               use_spinner = TRUE, full_screen = TRUE) {
  # Define the container ID
  container_id <- ns(paste0(table_type, "-container"))
  
  # Define the base style
  base_style <- paste0("max-height: ", max_height,
                       "; overflow-y: auto; border: none; box-shadow: none;")
  
  # Create the table output
  table_output <- create_data_table(ns, ns(paste0(table_type, "_table")),
                                    width = width, height = height,
                                    use_spinner = use_spinner)
  
  # Wrap table in a container div
  table_container <- div(
    id = container_id,
    class = paste0(table_type, "-container-style"),
    table_output
  )
  
  bslib::nav_panel(
    title = title,
    bslib::card(
      full_screen = full_screen,
      style = base_style,
      table_container
    )
  )
}


#' Create a job-status output with an optional spinner
#'
#' This function creates the placeholder for the message shown before job
#' results are available.
#'
#' @param inputId ID for the job-status output.
#' @param use_spinner Whether to show a spinner while the message is loading.
#' @return A Shiny UI output, optionally wrapped with a spinner.
#' @export
create_job_status_output <- function(inputId, use_spinner = TRUE) {
  status_output <- shiny::uiOutput(inputId)
  
  if (use_spinner) {
    status_output <- add_spinner(
      status_output,
      proxy.height     = 60,
      use_fill_carrier = FALSE
    )
  }
  
  status_output
}

#' Create a Module Title
#' 
#' This function generates a standardized title for a module in the Shiny app.
#' 
#' @param title A character string specifying the title text.
#' @return A `div` container with an `h3` title.
#' 
#' @examples
#' create_section_title("Predict traits from taxonomy")
#' create_section_title("Search database")
#' create_section_title("Predict traits with machine learning")
create_title_div <- function(title) {
  div(
    shiny::h3(title)
  )
}

#' Create a Label for Shiny Input
#'
#' This helper function creates a label for a Shiny input element.
#'
#' @param inputId The input ID for the label.
#' @param label A character string specifying the label text.
#' @return A Shiny UI element for the label.
#' @export
#' @importFrom shiny tags
create_input_label <- function(inputId, label = NULL) {
  shiny::tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = inputId
  )
}

#' Define a File Input Box with a Link to a Modal
#'
#' This function defines a custom file input box in a Shiny app, which includes a link to a modal for additional actions or information.
#'
#' @param inputId The input ID for the file input.
#' @param label A label for the file input. Default is `NULL` (no label).
#' @param multiple Logical. If TRUE, allows multiple file selection. Default is TRUE.
#' @param accept A character vector of accepted file types (e.g., csv).
#' @param width A character string specifying the width of the input box. Default is `NULL`.
#' @param buttonLabel The label for the file browse button. Default is "Browse...".
#' @param placeholder Placeholder text for when no file is selected. Default is "No file selected".
#' @param modalId The input ID for the modal link.
#' @param clearable Whether to show a link that clears the selected file. Default is TRUE
#' @param clearLabel The text for the clear link. Default is "Remove file".
#' @param modalLabel The label for the modal link. Default is "Download example".
#'
#' @return A Shiny UI element for the custom file input box.
#' @export
#' @importFrom shiny tags div actionLink
fileInput_modal <- function(inputId, label = NULL, multiple = TRUE, 
                            accept = c("text/csv", 
                                       "text/comma-separated-values,text/plain", 
                                       "text/tab-separated-values", 
                                       "application/vnd.ms-excel", 
                                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                       ".csv", ".tsv", ".txt", ".rds", ".ko", 
                                       ".nwk", ".newick", ".tree", ".tre",
                                       ".xls", ".xlsx", ".zip"), 
                            width = NULL, buttonLabel = "Browse...", 
                            placeholder = "No file selected", 
                            modalId, modalLabel = "Download example",
                            clearable = TRUE, clearLabel = "Remove file") {
  
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)
  
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- shiny::tags$input(id = inputId, name = inputId, type = "file", 
                                style = "display: none;", `data-restore` = restoredValue)
  
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  
  shiny::div(
    class = "form-group shiny-input-container", 
    style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
    
    if (!is.null(label)) create_input_label(inputId, label), 
    
    shiny::div(
      class = "input-group", 
      shiny::tags$label(class = "input-group-btn input-group-prepend", 
                        shiny::span(class = "btn btn-default btn-file", 
                                    buttonLabel, inputTag)), 
      shiny::tags$input(type = "text", 
                        class = "form-control", placeholder = placeholder, 
                        readonly = "readonly")),
    
    # The clear link is hidden until a file is chosen.  Showing it and wiring
    # up the click are handled by delegated handlers in www/custom.js, keyed
    # off the `.file-input-clear-trigger` class and the `data-target` attribute.
    shiny::div(
      class = "flex-container",
      shiny::actionLink(inputId = modalId, label = modalLabel),
      if (clearable) {
        shiny::tags$a(clearLabel,
                      href = "#",
                      class = "file-input-clear-trigger",
                      `data-target` = inputId,
                      style = "display: none;")
      }
    ),
    
    shiny::tags$div(
      id = paste(inputId, "_progress", sep = ""), 
      class = "progress active shiny-file-input-progress", 
      shiny::tags$div(class = "progress-bar"))
  )
}

#' Create a file input triggered by a link
#'
#' This function creates a hidden file input that is triggered by a link-styled action.
#' This is similar to fileInput(), but it gives the visual appearance of a link.
#'
#' @param inputId The input ID.
#' @param label The text for the clickable link. Default is `"Upload file"`.
#' @param accept Character vector of accepted file types (default: common text and spreadsheet formats).
#' @param multiple Whether multiple file upload is allowed (default: FALSE).
#' @param width Optional width of the wrapper div.
#' @return A Shiny UI tag list with link-triggered file input.
#' @export
fileInput_link <- function(inputId,
                           label = "Upload file",
                           accept = c(
                             "text/csv", 
                             "text/comma-separated-values,text/plain", 
                             "text/tab-separated-values", 
                             "application/vnd.ms-excel", 
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                             ".csv", ".tsv", ".txt", ".rds", ".ko", ".xls", ".xlsx", ".zip"),
                           multiple = FALSE,
                           width = NULL) {
  
  # Click forwarding from the action link to the hidden file input is wired
  # up by a generic delegated handler in www/custom.js, keyed off the
  # `.file-input-link-trigger` class and the `data-target` attribute.
  shiny::tags$div(
    style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
    
    # Action link that triggers file input
    shiny::actionLink(paste0(inputId, "_trigger"), label = label,
                      class = "file-input-link-trigger",
                      `data-target` = inputId,
                      style = "display: block; margin-top: 10px;"),
    
    # Hidden file input with actual inputId
    shiny::tags$input(
      id = inputId,
      name = inputId,
      type = "file",
      style = "display: none;",
      accept = paste(accept, collapse = ","),
      multiple = if (multiple) "multiple" else NULL
    )
  )
}

#' Create a Standardized Selectize Input
#'
#' This function creates a standardized `selectizeInput` with sensible defaults
#' based on the most common use cases in the app.
#'
#' @param inputId The input ID for the select input.
#' @param label The display label for the input. Default is `NULL` (no label).
#' @param choices A list of values to select from. Default is `NULL`.
#' @param selected The initially selected value. Default is `NULL`.
#' @param multiple Logical. If TRUE, allows multiple selections. Default is TRUE.
#' @param width The width of the input. Default is `NULL`.
#' @param options A list of additional options for selectize.js. Default enables search & multi-selection UI.
#'
#' @return A `selectizeInput` element.
#'
#' @examples
#' create_selectize_input("query_taxa")
#' create_selectize_input("traits_to_predict", multiple = FALSE)
#' create_selectize_input("models", choices = names(model_paths), selected = names(model_paths))
create_selectize_input <- function(inputId, 
                                   label = NULL, 
                                   choices = NULL, 
                                   selected = NULL, 
                                   multiple = TRUE, 
                                   width = NULL,
                                   options = list(`actions-box` = TRUE, 
                                                  `live-search` = TRUE, 
                                                  dropdownParent = 'body',
                                                  plugins = list("remove_button")
                                    )) {
  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    width = width,
    options = options
  )
}

#' Create a Download Button with Spinner
#'
#' This function wraps `shiny::downloadButton()` and applies a default spinner using `add_spinner()`.
#'
#' @param inputId The input ID for the download button.
#' @param label The label for the button. Default is `"Download"`.
#' @param ... Additional arguments passed to `shiny::downloadButton()`.
#' @return A `downloadButton` wrapped with a spinner.
#'
#' @examples
#' create_download_button("download_data", "Download Results")
#' create_download_button("download_model", "Download Model")
create_download_button <- function(inputId, label = "Download results", ...) {
  shiny::downloadButton(inputId, label, ...) |> 
    add_spinner(use_fill_carrier = FALSE, proxy.height = 120)
}

#' Create Conditional Download Button with Spinner
#'
#' This function generates a conditional UI that switches between a real download button and a null action button.
#' This is useful when a download is conditionally available.
#'
#' @param condition JavaScript condition as a string (e.g., `"output.flag_models"`).
#' @param inputId The input ID for the download button.
#' @param label Label for the button. Default is `"Download results"`.
#' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
#' @param null_inputId The input ID for the null action button. Default is `"null_"` prefixed to `inputId`.
#' @param ... Additional arguments passed to `shiny::downloadButton()`.
#' @return A tagList with conditional download and fallback buttons.
#' @export
#'
#' @examples
#' create_conditional_download_button("output.flag_models", "download_model", label = "Download model", ns = NS("module"))
create_conditional_download_button <- function(condition,
                                               inputId,
                                               label = "Download results",
                                               ns = identity,
                                               null_inputId = paste0("null_", inputId),
                                               ...) {
  tagList(
    shiny::conditionalPanel(
      condition = condition,
      ns = ns,
      create_download_button(ns(inputId), label = label, ...)
    ),
    shiny::conditionalPanel(
      condition = paste0("!(", condition, ")"),
      ns = ns,
      shiny::actionButton(
        inputId = ns(null_inputId),
        label = label,
        icon = icon("download"),
        class = "btn btn-default shiny-download-link"
      )
    )
  )
}


#' Create a Standardized Picker Input
#'
#' This function creates a `pickerInput` with sensible defaults based on its usage in the app.
#'
#' @param inputId The input ID for the picker input.
#' @param label The display label for the input. Default is `NULL` (no label).
#' @param choices A list of values to select from. Default is `NULL`.
#' @param selected The initially selected value. Default is `NULL`.
#' @param multiple Logical. If TRUE, allows multiple selections. Default is FALSE.
#' @param width The width of the input. Default is `"100%"`.
#' @param options A list of additional options for the picker. Default enables search.
#'
#' @return A `pickerInput` element.
#'
#' @examples
#' create_picker_input("variable_to_display")
#' create_picker_input("tree_layout", label = "Layout")
#' create_picker_input("network_dimensions", label = "Dimensions", choices = c("2", "3"), selected = "3")
create_picker_input <- function(inputId, label = NULL, choices = NULL,
                                selected = NULL, multiple = FALSE, width = "100%",
                                options = list(`actions-box` = TRUE,
                                               `live-search` = TRUE,
                                               container = "body")) {
  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    width = width,
    options = options
  )
}

#' Create a Switch Input with Configurable Label Position
#'
#' This function wraps `shinyWidgets::switchInput()` inside a `div` and displays the label
#' either above or to the right of the switch.
#'
#' @param inputId The input ID for the switch.
#' @param label The label text displayed near the switch.
#' @param value Logical. The default value of the switch. Default is `TRUE`.
#' @param size The size of the switch (`"small"`, `"default"`, or `"large"`). Default is `"small"`.
#' @param inline Logical. Whether to display the switch inline. Default is `TRUE`.
#' @param label_position Where to place the label: `"above"` (default) or `"right"`.
#'
#' @return A `div` container with a labeled switch input.
#'
#' @examples
#' create_switch_input("simplify_names", "Simplify names", label_position = "above")
#' create_switch_input("hide_labels", "Hide labels", label_position = "right")
create_switch_input <- function(inputId, label, value = TRUE, size = "small", inline = TRUE, label_position = "above") {
  label_tag <- shiny::tags$label(`for` = inputId, label)
  
  if (label_position == "right") {
    shiny::div(
      class = "horizontal-container",
      label_tag,
      shinyWidgets::switchInput(inputId = inputId, value = value, size = size, inline = inline)
    )
  } else {
    shiny::div(
      class = "vertical-container",
      label_tag,
      shinyWidgets::switchInput(inputId = inputId, value = value, size = size, inline = inline)
    )
  }
}


#' Create a Slider Input Sized for the Plot Options Toolbar
#'
#' This function wraps `shiny::sliderInput()` in a div so the slider lines up with
#' the pickers and switches in the plot options toolbar.  Tick marks are off by
#' default, which drops the row of grid labels and keeps the slider at about the
#' same height as its neighbours.
#'
#' @param inputId The input ID for the slider.
#' @param label The label text shown above the slider.
#' @param min The lowest value the slider allows.
#' @param max The highest value the slider allows.
#' @param value The starting value of the slider.
#' @param step The size of each move of the handle. Default is `NULL` (chosen by Shiny).
#' @param width The width of the slider. Default is `"100%"`.
#' @param ticks Logical. Whether to draw tick marks. Default is `FALSE`.
#'
#' @return A `div` container with a slider input.
#'
#' @examples
#' create_slider_input("probability_threshold", "Probability threshold", min = 0, max = 1, value = 0.5)
#' create_slider_input("flux_threshold", "Flux threshold", min = 0, max = 1000, value = 1)
create_slider_input <- function(inputId, label = NULL, min = 0, max = 1, value = 0.5,
                                step = NULL, width = "100%", ticks = FALSE) {
  shiny::div(
    class = "compact-slider",
    shiny::sliderInput(
      inputId = inputId,
      label = label,
      min = min,
      max = max,
      value = value,
      step = step,
      width = width,
      ticks = ticks
    )
  )
}


#' Create a Conditional Flex Item
#'
#' This function wraps a UI element in a conditional panel and a flex item. It
#' is used for plot options, which are shown or hidden depending on the tab
#' open, without changing how the toolbar is laid out.
#'
#' @param ns A namespace function for module compatibility.
#' @param condition A character string with a JavaScript condition.
#' @param ui_element A UI element to place inside the flex item.
#' @param item_class A character string with the class for the flex item.
#'   Sliders need "flex-item flex-item-slider", which is wider than the default.
#' @return A conditional panel holding the UI element.
#' @export
#' @importFrom shiny conditionalPanel div
create_conditional_flex_item <- function(ns, condition, ui_element, item_class = "flex-item") {
  shiny::conditionalPanel(
    condition = condition,
    ns = ns,
    div(class = item_class, ui_element)
  )
}

#' Create the Accordion Holding Plot Options
#'
#' This function creates the accordion that holds the options for a plot. The
#' variables that pick what is plotted stay in the toolbar; everything that
#' changes how it is drawn goes here.
#'
#' With `inline = TRUE` the accordion is set to display: contents by the style
#' sheet, so its button and its collapsing body become items of the toolbar it
#' sits in. The button takes a quarter of the toolbar and the body takes all of
#' it, so the body wraps onto the next line and the two make an L. The
#' conditional panel wrapping the accordion is dropped from the box tree in the
#' same way, or it, rather than the toolbar, becomes the parent of those items.
#'
#' The accordion is given no ID. An ID makes bslib report its open panels as an
#' input, and the accordion is closed by editing classes (see custom.js), which
#' would leave that input stale.
#'
#' The controls are passed first so that unnamed arguments cannot be bound to
#' the arguments after them, all of which must be given by name.
#'
#' @param ... UI elements to place in the accordion body.
#' @param ns A namespace function for module compatibility.
#' @param title A character string with the text on the accordion button.
#' @param condition A character string with a JavaScript condition deciding
#'   whether to show the accordion at all, or NULL to always show it. Give a
#'   condition when every control inside is itself conditional, so that tabs
#'   with no options do not get an accordion that opens onto nothing.
#' @param inline Whether the button shares the toolbar above (TRUE) or sits on a
#'   row of its own (FALSE).
#' @param icon An icon for the accordion button.
#' @return An accordion, wrapped in a conditional panel when a condition is given.
#' @export
#' @importFrom shiny conditionalPanel icon NS
create_options_accordion <- function(..., ns = shiny::NS(NULL), title = "Options",
                                     condition = NULL, inline = TRUE,
                                     icon = shiny::icon("sliders")) {
  accordion <- bslib::accordion(
    open = FALSE,
    multiple = FALSE,
    class = paste("options-accordion",
                  if (inline) "options-accordion-inline" else "options-accordion-row"),
    bslib::accordion_panel(title = title, icon = icon, ...)
  )

  if (is.null(condition)) return(accordion)

  panel <- shiny::conditionalPanel(condition = condition, ns = ns, accordion)

  if (inline) {
    panel <- htmltools::tagAppendAttributes(panel, class = "options-accordion-wrapper")
  }

  panel
}

#' Create a Home Button UI Element
#'
#' This function creates a UI element for a home button in a Shiny app. 
#' The button includes an image, title, subtitle, and a customizable action button.
#'
#' @param image_name A character string specifying the name of the image file (without extension) to be displayed on the button.
#' @param title A character string specifying the title text to be displayed on the button.
#' @param subtitle A character string specifying the subtitle text to be displayed on the button.
#' @param button_name A character string specifying the ID for the Shiny action button.
#' @param icon_background_color A character string specifying the background color of the icon. Default is "red".
#' @param position A character string specifying the position class for the action button. Default is "left".
#' @return A Shiny UI element representing a home button with the specified properties.
#' @export
#' @importFrom shiny div tags actionButton
home_button <- function(image_name, title, subtitle, button_name, icon_background_color = "red", position = "left") {
  div(
    class = "home-button-box",
    div(
      class = "home-button-grid",
      div(
        class = "home-button-icon",
        tags$img(
          src = paste0(image_name, ".svg"),
          style = paste0(
            'background-color:', icon_background_color, ';', 
            'border-radius: 5px; display: block; max-width: 100%; max-height: 75px; height: auto; margin: 0 auto;'
          )
        )
      ),
      div(
        div(
          class = "home-button-title",
          title
        ),
        div(
          class = "home-button-subtitle",
          subtitle
        )
      )
    ),
    shiny::actionButton(button_name, title, class = paste0("home-action-button-", position))
  )
}
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

#' Inject JavaScript to Retrieve Public IP Address (Non-Namespaced)
#'
#' This function injects a JavaScript snippet that retrieves the user's public IP address 
#' upon session initialization using the `https://api.ipify.org` service. The IP address 
#' is then set as the Shiny input `"user_ip"`.
#'
#' @return A `tags$script` HTML element containing the JavaScript code.
#'
#' @details This version hardcodes the input ID as `"user_ip"` and is intended for use 
#' outside of Shiny modules. For a module-compatible version that respects namespacing, 
#' use `inject_ip_script()`.
#'
#' @examples
#' ui <- fluidPage(
#'   inject_ip_js(),
#'   verbatimTextOutput("ip")
#' )
#'
#' server <- function(input, output, session) {
#'   output$ip <- renderText({ input$user_ip })
#' }
#'
inject_ip_js <- function() {
  htmltools::tags$script(
    '
    $(document).on("shiny:sessioninitialized", function () {
      $.get("https://api.ipify.org", function (response) {
        Shiny.setInputValue("user_ip", response);
      });
    });
    '
  )
}

#' Inject JavaScript to Resize Containers Dynamically
#' 
#' This function injects a JavaScript snippet that adjusts the width of a container 
#' based on its height using `shinyjs.resizeWidthFromHeight`. 
#' 
#' @param ns The namespace function for the module.
#' @param container_id The ID of the container to be resized.
#' 
#' @return A `tags$script` object containing the JavaScript.
#' 
#' @examples
#' inject_js_resize(ns, "treemap-container")
inject_js_resize <- function(ns, container_id) {
  tags$script(HTML(sprintf("
    $(document).ready(function(){
      shinyjs.resizeWidthFromHeight('%s', 1.045296);
    });
  ", ns(container_id))))
}

#' Inject JavaScript for Query Builder Styling
#' 
#' This function injects JavaScript to modify the appearance of `selectize` inputs 
#' within a `queryBuilder` component by adjusting the CSS classes.
#' 
#' @param ns The namespace function for the module.
#' @param query_id The ID of the `queryBuilder` input.
#' 
#' @return A `tags$head` object containing the JavaScript.
#' 
#' @examples
inject_query_builder_js <- function(ns, query_id) {
  tags$head(
    tags$script(
      sprintf(
        "
        $( document ).ready(function() {
          $('#%s').on('afterCreateRuleInput.queryBuilder', function(e, rule) {
            if (rule.filter.plugin == 'selectize') {
              rule.$el.find('.rule-value-container').css('min-width', '10vw')
                .find('.selectize-control').removeClass('form-select');
              rule.$el.find('.rule-value-container').find('.selectize-dropdown').removeClass('form-select');
            }
          });
        });",
        ns(query_id)
      )
    )
  )
}

#' Inject JavaScript to Retrieve Public IP Address
#'
#' This function injects a JavaScript snippet that runs when the Shiny session is initialized.
#' It uses `$.get("https://api.ipify.org")` to retrieve the user's public IP address and assigns
#' it to the Shiny input named `"user_ip"`. This allows the server to reactively access the user's IP.
#'
#' @return A `tags$script` HTML element that can be included in a Shiny UI to run the JavaScript code.
#'
#' @details This version uses a hardcoded input ID (`user_ip`) and is appropriate for use outside of modules.
#' For module-safe use with namespacing, see `inject_ip_script()`.
#'
#' @examples
#' ui <- fluidPage(
#'   inject_ip_js(),
#'   verbatimTextOutput("ip")
#' )
#'
#' server <- function(input, output, session) {
#'   output$ip <- renderText({ input$user_ip })
#' }
#'
inject_ip_script <- function(ns) {
  tags$head( 
    tags$script(HTML(sprintf(
      '
      $(document).on("shiny:sessioninitialized", function() {
        $.get("https://api.ipify.org", function(response) {
          Shiny.setInputValue("%s", response);
        });
      });
    ', ns("user_ip")))
    )
  )
}

#' Create a Query Builder Input
#' 
#' This function generates a standardized `jqbr::queryBuilderInput` element with customizable 
#' input ID, dynamically loaded filters, and rules.
#' 
#' @param ns The namespace function for the module.
#' @param input_id A character string specifying the ID of the query builder input.
#' @param rules The default rules for the query builder.
#' @param label (Optional) A label or title to be displayed above the query builder.
#' 
#' @return A `div` containing the query builder input.
#' 
#' @examples
#' create_query_builder(ns, "query_builder", query_rules_search, load_query_filters, "Build query")
#' create_query_builder(ns, "query_builder", query_rules_taxonomy, load_query_filters)
#' create_query_builder(ns, "query_builder", query_rules_ML, load_query_filters, "Machine Learning Query")
create_query_builder <- function(ns, input_id, label = NULL) {
  filters <- load_placeholder_filters()

  rules <- list(
    condition = "AND",
    rules = list(list(id = "Type of metabolism (FAPROTAX)", operator = "contains"))
  )
  
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
#' @param use_fill_carrier Logical indicating whether to apply `bslib::as_fill_carrier()`. Default is `TRUE`.
#' @param ... Additional arguments passed to `shinycssloaders::withSpinner()`.
#'
#' @return The UI element wrapped with a loading spinner.
#'
#' @examples
#' with_default_spinner(plotly::plotlyOutput("plot"))
#' with_default_spinner(DT::dataTableOutput("table"))
add_spinner <- function(ui_element, color = "#3C8DBC", proxy.height = 400, use_fill_carrier = TRUE, ...) {
  spinner <- shinycssloaders::withSpinner(ui_element, color = color, proxy.height = proxy.height, ...)
  
  if (use_fill_carrier) {
    spinner <- bslib::as_fill_carrier(spinner)
  }
  
  spinner
}

#' Create a Plot Container
#' 
#' This function generates a standardized plot container with a loading spinner
#' 
#' @param ns The namespace function for the module.
#' @param plot_type A character string specifying the type of plot (e.g., "treemap", "heatmap").
#' @param width The width of the plot (default: "100%").
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

#' Create a Plot Panel
#' 
#' This function generates a standardized nav panel with a plot inside a styled card.
#' 
#' @param ns The namespace function for the module.
#' @param plot_type A character string specifying the type of plot (e.g., "summary", "treemap").
#' @param title The title of the navigation panel.
#' @param width The width of the plot (default: "100%").
#' @param height The height of the plot (default: "100%").
#' @param max_height The maximum height of the card (default: "50vh").
#' @param centered Logical, whether to center the plot inside the card (default: FALSE).
#' @param use_spinner Logical, whether to wrap the plot in a loading spinner (default: TRUE).
#' 
#' @return A `nav_panel` containing a `bslib::card` with a `plotlyOutput`.
#' 
#' @examples
#' create_plot_panel(ns, "summary", "Summary")
#' create_plot_panel(ns, "treemap", "Treemap", max_height = "60vh", centered = TRUE, use_spinner = FALSE)
create_plot_panel <- function(ns, plot_type, title, width = "100%", height = "100%", 
                              max_height = "50vh", centered = FALSE, use_spinner = FALSE) {
  # Define the container ID
  container_id <- ns(paste0(plot_type, "-container"))
  
  # Define the base style
  base_style <- paste0("max-height: ", max_height, 
                       "; overflow-y: auto; border: none; box-shadow: none;")
  
  # Add flex centering if requested
  if (centered) {
    base_style <- paste0(base_style, " display: flex; align-items: center; justify-content: center;")
  }
  
  # Create the plot output
  plot_output <- plotly::plotlyOutput(ns(paste0(plot_type, "_plot")), width = width, height = height)
  
  # Optionally wrap with spinner
  if (use_spinner) {
    plot_output <- add_spinner(plot_output)
  }
  
  # Wrap plot in a resizable div container
  plot_container <- div(
    id = container_id,
    class = paste0(plot_type, "-container-style"),
    plot_output
  ) %>% bslib::as_fill_carrier()
  
  bslib::nav_panel(
    title = title,
    bslib::card(
      full_screen = TRUE,
      style = base_style,
      plot_container
    )
  )
}


#' Create a Module Title
#' 
#' This function generates a standardized title for a module in the Shiny app.
#' 
#' @param title A character string specifying the title text.
#' 
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
#' @param label A label for the file input. Default is NULL (no label).
#' @param multiple Logical. If TRUE, allows multiple file selection. Default is TRUE.
#' @param accept A character vector of accepted file types. Default includes csv, txt, rds, KO, xlx, and xlsx formats.
#' @param width A character string specifying the width of the input box. Default is NULL.
#' @param buttonLabel The label for the file browse button. Default is "Browse...".
#' @param placeholder Placeholder text for when no file is selected. Default is "No file selected".
#' @param modalId The input ID for the modal link.
#' @param modalLabel The label for the modal link. Default is "Download example".
#'
#' @return A Shiny UI element for the custom file input box.
#' @export
#' @importFrom shiny tags div actionLink
fileInput_modal <- function(inputId, label = NULL, multiple = TRUE, 
                            accept = c("text/csv", "text/comma-separated-values,text/plain", 
                                       "application/vnd.ms-excel", 
                                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                       ".csv", ".txt", ".rds", ".ko", ".xls", ".xlsx"), 
                            width = NULL, buttonLabel = "Browse...", 
                            placeholder = "No file selected", 
                            modalId, modalLabel = "Download example") {
  
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
    
    shiny::actionLink(inputId = modalId, label = modalLabel),
    
    shiny::tags$div(
      id = paste(inputId, "_progress", sep = ""), 
      class = "progress active shiny-file-input-progress", 
      shiny::tags$div(class = "progress-bar"))
  )
}

#' Create a Standardized Selectize Input
#'
#' This function creates a standardized `selectizeInput` with sensible defaults
#' based on the most common use cases in the app.
#'
#' @param inputId The input ID for the select input.
#' @param label The display label for the input. Default is NULL (no label).
#' @param choices A list of values to select from. Default is NULL.
#' @param selected The initially selected value. Default is NULL.
#' @param multiple Logical. If TRUE, allows multiple selections. Default is TRUE.
#' @param width The width of the input. Default is NULL.
#' @param options A list of additional options for selectize.js. Default enables search & multi-selection UI.
#'
#' @return A `selectizeInput` element.
#'
#' @examples
#' create_selectize_input("taxonomy_database")
#' create_selectize_input("set_traits", multiple = FALSE)
#' create_selectize_input("models", choices = names(model_paths), selected = names(model_paths))
create_selectize_input <- function(inputId, label = NULL, choices = NULL, 
                                   selected = NULL, multiple = TRUE, width = NULL,
                                   options = list(`actions-box` = TRUE, `live-search` = TRUE, 
                                                  dropdownParent = 'body')) {
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
#' @param label The label for the button (default: "Download").
#' @param ... Additional arguments passed to `shiny::downloadButton()`.
#'
#' @return A `downloadButton` wrapped with a spinner.
#'
#' @examples
#' create_download_button("download_data", "Download Results")
#' create_download_button("download_model", "Download Model")
create_download_button <- function(inputId, label = "Download results", ...) {
  shiny::downloadButton(inputId, label, ...) %>% 
    add_spinner(use_fill_carrier = FALSE)
}

#' Create Conditional Download Button with Spinner
#'
#' This function generates a conditional UI that switches between a real download button and a null action button.
#' This is useful when a download is conditionally available.
#'
#' @param condition JavaScript condition as a string (e.g., "output.flag_models").
#' @param inputId The input ID for the download button.
#' @param label Label for the button (default: "Download results").
#' @param ns Optional namespace function (default: `identity`).
#' @param null_inputId The input ID for the null action button (default: `"null_"` prefixed to `inputId`).
#' @param ... Additional arguments passed to `shiny::downloadButton()`.
#'
#' @return A tagList with conditional download and fallback buttons.
#' @export
#'
#' @examples
#' create_conditional_download_button("output.flag_models", "download_model", label = "Download model", ns = NS("module"))
#' Create Conditional Download Button with Spinner
#'
#' This function generates a conditional UI that switches between a real download button
#' and a null action button. This is useful when a download is conditionally available.
#'
#' @param condition JavaScript condition as a string (e.g., "output.flag_models").
#' @param inputId The input ID for the download button.
#' @param label Label for the button (default: "Download results").
#' @param ns Optional namespace function (default: `identity`).
#' @param null_inputId The input ID for the null action button (default: `"null_"` prefixed to `inputId`).
#' @param ... Additional arguments passed to `shiny::downloadButton()`.
#'
#' @return A tagList with conditional download and fallback buttons.
#' @export
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
#' @param label The display label for the input. Default is NULL (no label).
#' @param choices A list of values to select from. Default is NULL.
#' @param selected The initially selected value. Default is NULL.
#' @param multiple Logical. If TRUE, allows multiple selections. Default is FALSE.
#' @param width The width of the input. Default is "100%".
#' @param options A list of additional options for the picker. Default enables search.
#'
#' @return A `pickerInput` element.
#'
#' @examples
#' create_picker_input("variable_to_display")
#' create_picker_input("set_tree_layout", label = "Layout")
#' create_picker_input("set_network_dimensions", label = "Dimensions", choices = c("2", "3"), selected = "3")
create_picker_input <- function(inputId, label = NULL, choices = NULL, 
                                selected = NULL, multiple = FALSE, width = "100%",
                                options = list(`actions-box` = TRUE, `live-search` = TRUE)) {
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

#' Create a DataTable with Spinner
#'
#' This function wraps `DT::dataTableOutput()` and applies a spinner using `add_spinner()`.
#'
#' @param inputId The input ID for the data table.
#' @return A `dataTableOutput` wrapped with a loading spinner.
#'
#' @examples
#' create_data_table("table")
create_data_table <- function(ns, inputId) {
  DT::dataTableOutput(inputId) %>% 
    add_spinner(use_fill_carrier = FALSE)
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
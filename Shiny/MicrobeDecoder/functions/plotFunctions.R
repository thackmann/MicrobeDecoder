# Functions for Creating Plots in App
# This script contains functions for generating and customizing different types of plots,
# including summary plots, heatmaps, treemaps, metabolic networks, phylogenetic tree, and scatterplots.
# The functions utilize the Plotly library to make plots interactive. 
# Author: Timothy Hackmann
# Date: 14 October 2024

# === Colors ===
  # Functions for turning values and groups into colors.  Named colors
  # (red_color, green_color, na_grey, and others) are defined in variablesTemp9.R.
  #   Ordered values     scale_z_to_color(), bin_z_to_color(), to_plotly_colorscale()
  #   Categorical groups ggplot_hue_colors(), add_fill_to_layout(), add_color_to_layout()
  #   Transparency       hex_to_rgba()

  #' Generate Color Palette
  #' 
  #' This function generates a color palette with a specified number of colors, using a gradient between the provided minimum and maximum colors.  Ask for 101 colors to get a lookup table for values on a 0 to 100 scale.
  #' 
  #' @param n_colors The number of colors to generate in the palette.
  #' @param min_color The color to use for the minimum value in the gradient.
  #' @param max_color The color to use for the maximum value in the gradient.
  #' @return A vector of color values in hexadecimal format.
  #' @export
  #' @importFrom grDevices colorRampPalette
  generate_color_palette <- function(n_colors, min_color = "black", max_color = "green") {
    grDevices::colorRampPalette(c(min_color, max_color))(n_colors)
  }

  #' Generate Evenly Spaced Categorical Colors
  #'
  #' This function returns colors spread evenly around the hue circle, matching the palette ggplot2 uses for discrete variables.  Use it for categorical groups, where colors need to be told apart rather than ranked.
  #'
  #' @param n The number of colors to return.
  #' @param lighten_amount How much to lighten the colors, from 0 (none) to 1 (white).  Negative values darken instead.
  #' @return A character vector of colors in hexadecimal format.
  #' @export
  #' @importFrom colorspace qualitative_hcl lighten
  ggplot_hue_colors <- function(n, lighten_amount = 0) {
    colors <- colorspace::qualitative_hcl(n, h = c(15, 375 * (n - 1) / n), c = 100, l = 65, fixup = TRUE, alpha = 1)

    if (lighten_amount != 0) {
      colors <- colorspace::lighten(colors, amount = lighten_amount)
    }

    colors
  }

  #' Map z Values to a Color Gradient
  #'
  #' This function maps numeric z values to colors along a two-color gradient.  Values are clamped to the range zmin to zmax, scaled to 0 to 1, and interpolated between min_color and max_color.  Values that are NA, such as tree nodes with no matching data, get na_color.
  #'
  #' @param z A numeric vector of values to map.
  #' @param min_color The color mapped to zmin.
  #' @param max_color The color mapped to zmax.
  #' @param zmin The value mapped to min_color.
  #' @param zmax The value mapped to max_color.
  #' @param na_color The color used for NA values.
  #' @return A character vector of colors in hexadecimal format, one per element of z.
  #' @export
  #' @importFrom grDevices colorRamp rgb
  scale_z_to_color <- function(z, min_color = "black", max_color = "green",
                               zmin = 0, zmax = 100, na_color = na_grey) {
    frac <- (z - zmin) / (zmax - zmin)
    frac <- pmin(pmax(frac, 0), 1)
    ramp <- grDevices::colorRamp(c(min_color, max_color))
    out <- rep(na_color, length(z))
    ok <- !is.na(frac)
    if (any(ok)) {
      rgb_mat <- ramp(frac[ok])
      out[ok] <- grDevices::rgb(rgb_mat[, 1], rgb_mat[, 2], rgb_mat[, 3],
                                maxColorValue = 255)
    }
    out
  }

  #' Map z Values to a Binned Color Gradient
  #' 
  #' This function maps numeric z values to colors the same way scale_z_to_color() does, but puts them into bins first.  Binning makes it possible to color branches of a tree by z without drawing one plotly trace per branch, since the number of traces becomes the number of bins.
  #' 
  #' @param z A numeric vector of values to map.
  #' @param n_bins The number of bins to divide the color scale into.  Fewer bins mean fewer traces and a faster plot.
  #' @param min_color The color mapped to zmin.
  #' @param max_color The color mapped to zmax.
  #' @param zmin The value mapped to min_color.
  #' @param zmax The value mapped to max_color.
  #' @param na_color The color used for NA values.
  #' @return A character vector of colors in hexadecimal format, one per element of z.
  #' @export
  bin_z_to_color <- function(z, n_bins = 32, min_color = "black", max_color = "green",
                             zmin = 0, zmax = 100, na_color = na_grey) {
    # Get the bin each value falls in
    n_bins <- max(as.integer(n_bins), 2L)
    breaks <- seq(zmin, zmax, length.out = n_bins + 1L)
    clamped <- pmin(pmax(z, zmin), zmax)
    bin_index <- findInterval(clamped, breaks, rightmost.closed = TRUE)
    bin_index[!is.na(bin_index) & bin_index < 1L] <- 1L
    
    # Color each value as though it sat at the middle of its bin
    bin_midpoint <- (breaks[bin_index] + breaks[bin_index + 1L]) / 2
    
    scale_z_to_color(bin_midpoint, min_color = min_color, max_color = max_color,
                     zmin = zmin, zmax = zmax, na_color = na_color)
  }

  #' Build a Plotly Colorscale from a Two-Color Gradient
  #'
  #' This function returns a colorscale in the form plotly expects, a list of stop and color pairs, following the same gradient as scale_z_to_color().  Two stops leave plotly to interpolate between the endpoints, while more stops hand plotly the shape of the gradient directly.
  #'
  #' @param min_color The color at the first stop.
  #' @param max_color The color at the last stop.
  #' @param n_stops The number of evenly spaced stops.
  #' @return A list of stop and color pairs for use as a plotly colorscale.
  #' @export
  to_plotly_colorscale <- function(min_color = "black", max_color = "green", n_stops = 2) {
    fracs <- seq(0, 1, length.out = n_stops)
    colors <- scale_z_to_color(fracs * 100, min_color = min_color, max_color = max_color,
                               zmin = 0, zmax = 100)
    Map(function(frac, color) list(frac, color), fracs, colors)
  }

  #' Convert Colors to RGBA
  #' 
  #' This function converts colors to the rgba() strings plotly uses for transparency.  Use it where an element should be see-through.  Where an element should stay opaque but look faded, lighten it toward white with colorspace::lighten() instead.  Colors and alpha values are recycled to a common length, so vectors can be passed directly.
  #' 
  #' @param hex_color A vector of colors, in any form col2rgb() accepts.
  #' @param alpha A numeric vector of values between 0 and 1 giving the transparency.
  #' @return A character vector of colors in rgba() format.
  #' @export
  #' @importFrom grDevices col2rgb
  hex_to_rgba <- function(hex_color, alpha = 1) {
    rgb <- grDevices::col2rgb(hex_color)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
  }

  #' Add Group Colors to Graph Layout
  #' 
  #' This function assigns one color per level of a grouping variable and writes it to a graph layout, in either the fill column or the color (border) column.  Group values are stripped of unusual characters and whitespace before levels are taken.
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param group The name of the column in the layout data frame used for grouping.
  #' @param target The column to write colors to, either "fill" or "color".
  #' @param lighten_amount A numeric value indicating the amount by which to lighten the colors.
  #' @return A data frame containing the layout coordinates with the target color column added.
  #' @export
  add_group_colors_to_layout <- function(layout, group, target = c("fill", "color"), lighten_amount = 0) {
    target <- match.arg(target)
    
    # Check if layout is empty
    if (nrow(layout) == 0) {
      return(layout)
    }
    
    # Convert the group column to character, clean up non-standard characters, and convert back to factor
    layout[[group]] <- as.character(layout[[group]])
    layout[[group]] <- gsub("[^[:alnum:] [:punct:]]", "", layout[[group]]) # Removes non-alphanumeric characters except standard punctuation
    layout[[group]] <- trimws(layout[[group]]) # Removes leading and trailing whitespace
    layout[[group]] <- as.factor(layout[[group]]) # Convert back to factor
    
    # Get unique levels for the specified group
    group_levels <- sort(unique(layout[[group]]))
    
    # Generate default ggplot2 colors for the group levels, lightening if asked
    group_colors <- ggplot_hue_colors(length(group_levels), lighten_amount = lighten_amount)
    names(group_colors) <- group_levels
    
    # Apply colors to the layout based on the cleaned group levels
    layout[[target]] <- group_colors[layout[[group]]]
    
    return(layout)
  }

  #' Add Fill Color to Graph Layout
  #' 
  #' This function applies fill colors to a graph layout based on a specified grouping variable, with optional lightening of the colors.  See add_group_colors_to_layout().
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param group The name of the column in the layout data frame used for grouping.
  #' @param lighten_amount A numeric value indicating the amount by which to lighten the fill colors.
  #' @return A data frame containing the layout coordinates with added fill colors.
  #' @export
  add_fill_to_layout <- function(layout, group, lighten_amount = 0) {
    add_group_colors_to_layout(layout, group, target = "fill", lighten_amount = lighten_amount)
  }

  #' Add Border Color to Graph Layout
  #' 
  #' This function applies border colors to a graph layout based on a specified grouping variable, with optional lightening of the colors.  See add_group_colors_to_layout().
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param group The name of the column in the layout data frame used for grouping.
  #' @param lighten_amount A numeric value indicating the amount by which to lighten the border colors.
  #' @return A data frame containing the layout coordinates with added border colors.
  #' @export
  add_color_to_layout <- function(layout, group, lighten_amount = 0) {
    add_group_colors_to_layout(layout, group, target = "color", lighten_amount = lighten_amount)
  }

# === Binary values ===
  # Helpers for showing values as present or absent instead of as a gradient.
  # A cell or node counts as present when it sits above zmin (sub-threshold
  # values are set to zero upstream), so present takes the max color and a plus,
  # and absent takes the min color and a minus.  The heatmap and the tree share
  # these, so the two figures behave the same way.

  #' Map Values to the Ends of a Color Scale
  #' 
  #' This function collapses values to the two ends of a color scale, so a plot can show presence and absence in place of a gradient.  A value counts as present when it sits above zmin and takes zmax (the max color); otherwise it takes zmin (the min color).  It works on a vector or a matrix and keeps the shape it was given.
  #' 
  #' @param z A numeric vector or matrix of values.
  #' @param zmin The low end of the color scale.
  #' @param zmax The high end of the color scale.
  #' @return z, with each non-missing value replaced by zmin or zmax.
  #' @export
  binarize_z <- function(z, zmin, zmax) {
    ifelse(is.na(z), NA_real_, ifelse(z > zmin, zmax, zmin))
  }

  #' Label Values as Present or Absent
  #' 
  #' This function returns a plus for present values and a minus for absent ones, to match binarize_z().  A value counts as present when it sits above zmin.  Missing values become na.  It works on a vector or a matrix and keeps the shape it was given.
  #' 
  #' @param z A numeric vector or matrix of values.
  #' @param zmin The low end of the color scale; values above it are present.
  #' @param na What to return for missing values (default "").
  #' @return A character vector or matrix of "+", "-", or na.
  #' @export
  binary_symbols <- function(z, zmin, na = "") {
    ifelse(is.na(z), na, ifelse(z > zmin, "+", "-"))
  }

  #' Show a Binary Value in a Hover Template
  #' 
  #' This function rewrites the value line of a plotly hover template so it shows a plus or a minus (drawn from the trace's text) in place of the number.  When value_label is given, it also relabels that line, so the hover matches a legend titled the same way.  The value line is the one carrying the z placeholder.
  #' 
  #' @param hovertemplate A plotly hover template string.
  #' @param value_label The label to show before the value, or NULL to keep the label the template already carries.
  #' @return The rewritten hover template string.
  #' @export
  set_binary_hover_value <- function(hovertemplate, value_label = NULL) {
    if (!is.null(value_label) && nzchar(value_label)) {
      # Swap the value line's label, number, and any bold tags for the label and a plus or minus
      gsub("(<b>)?[^<>]*%\\{z(:[^}]*)?\\}(</b>)?",
           paste0("<b>", value_label, ": %{text}</b>"),
           hovertemplate)
    } else {
      # Keep the value's existing label and show the plus or minus in place of the number
      gsub("%\\{z(:[^}]*)?\\}", "%{text}", hovertemplate)
    }
  }

# === General ===
  #' Extract Axis Settings
  #' 
  #' This function extracts settings for a specific axis (x, y, or z) from a list of arguments, with default settings provided.
  #' 
  #' @param axis_prefix The prefix indicating the axis ("x", "y", or "z").
  #' @param args A list of arguments containing axis settings.
  #' @param default_settings A list of default settings for the axis.
  #' @return A list of settings for the specified axis.
  #' @export
  extract_axis_settings <- function(axis_prefix, args, default_settings) {
    axis_settings <- default_settings
    
    # Override with user-defined settings
    for (setting in names(default_settings)) {
      # Extract settings for a specific axis (x, y, z)
      arg_name <- paste0(setting, ".", axis_prefix)
      
      if (!is.null(args[[arg_name]])){
        axis_settings[[setting]] <- args[[arg_name]]
      } else {
        # If settings do not match a specific axis, extract settings with no axis specified
        arg_name <- setting
        if (!is.null(args[[arg_name]])){
          axis_settings[[setting]] <- args[[arg_name]]
        }
      }
    }
    return(axis_settings)
  }
  
  #' Generate Axis Settings
  #' 
  #' This function generates axis settings for Plotly layouts, allowing customization of tick labels, tick lengths, and axis titles.
  #' 
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param include_z A logical value indicating whether to include z-axis settings (for 3D plots).
  #' @param ... Additional arguments for customizing axis settings.
  #' @return A list of axis settings for the Plotly layout.
  #' @export
  #' @importFrom plotly layout
  generate_axis_settings <- function(coord_fixed = FALSE, x_to_y_ratio = 1, include_z = FALSE, ...) {
    args <- list(...)
    
    # Define default settings
    default_settings <- list(
      title = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = TRUE,
      ticks = NULL,
      ticklen = 4,
      showline = FALSE,
      autorange = NULL,
      range = NULL,
      side = NULL,
      tickangle = NULL,
      type = NULL,          
      categoryorder = NULL,   
      categoryarray = NULL,        
      tickmode = NULL,
      tickvals = NULL,
      ticktext = NULL,
      tickfont = NULL
    )
    
    # Extract settings for each axis
    xaxis_settings <- extract_axis_settings("x", args, default_settings)
    yaxis_settings <- extract_axis_settings("y", args, default_settings)
    zaxis_settings <- if (include_z) extract_axis_settings("z", args, default_settings) else NULL
    
    # Remove NULL entries to allow default behavior
    xaxis_settings <- xaxis_settings[!sapply(xaxis_settings, is.null)]
    yaxis_settings <- yaxis_settings[!sapply(yaxis_settings, is.null)]
    if (include_z) {
      zaxis_settings <- zaxis_settings[!sapply(zaxis_settings, is.null)]
    }
    
    # Fix x-y coordinates
    if (coord_fixed) {
      xaxis_settings$scaleanchor <- "y"
      xaxis_settings$scaleratio <- x_to_y_ratio
      xaxis_settings$constrain <- "domain"
      yaxis_settings$scaleanchor <- "x"
      yaxis_settings$scaleratio <- 1 / x_to_y_ratio
      yaxis_settings$constrain <- "domain"
    }
    
    # Return settings
    if (include_z) {
      return(list(xaxis = xaxis_settings, yaxis = yaxis_settings, zaxis = zaxis_settings))
    } else {
      return(list(xaxis = xaxis_settings, yaxis = yaxis_settings))
    }
  }
  
  #' Add Shapes to a Plot
  #'
  #' This function adds shapes to a plotly plot, keeping the shapes the plot
  #' already carries. plotly holds the shapes from each layout() call aside and
  #' merges them when the plot is built, and because a list of shapes has no
  #' names the merge keeps the first list and drops the rest, so a second call
  #' would lose its shapes without a word. The shapes held aside are therefore
  #' collected and cleared first, and the whole set is applied in one call.
  #'
  #' @param plot A plotly plot object to add the shapes to.
  #' @param shapes A list of shapes to add.
  #' @return A plotly plot object with the shapes added.
  #' @export
  #' @importFrom plotly layout
  add_shapes <- function(plot, shapes) {
    # Collect the shapes held aside and clear them, going by position as the names repeat
    existing_shapes <- list()
    for (i in seq_along(plot$x$layoutAttrs)) {
      staged <- plot$x$layoutAttrs[[i]]$shapes
      if (!is.null(staged)) {
        existing_shapes <- c(existing_shapes, staged)
        plot$x$layoutAttrs[[i]]$shapes <- NULL
      }
    }
    existing_shapes <- c(existing_shapes, plot$x$layout$shapes)

    # Apply the whole set in one call
    plot <- plot |> plotly::layout(shapes = c(existing_shapes, shapes))

    return(plot)
  }
  
  #' Add Categorical Legend
  #'
  #' This function adds a categorical color legend to a plotly plot: a title above
  #' a vertical stack of color boxes, one per label, shaded from `min_color` to
  #' `max_color`. It returns the plot with the legend added, the same as the other
  #' plot helpers.
  #'
  #' The legend can be drawn in two coordinate systems. In "paper" coordinates
  #' (the default) the boxes are sized as a fraction of the plot frame and the
  #' stack is centered vertically, which suits the square heatmap-style plots. In
  #' "data" coordinates the boxes are sized in pixels and the stack is pinned near
  #' the top, which suits plots drawn in pixel coordinates (such as the trait
  #' profile), where a centered, percentage-sized legend would be distorted on a
  #' wide, short, or scrolling canvas. In "data" coordinates the caller reserves
  #' the legend's strip in the plotting area and can size the plot with
  #' `get_legend_height()`; in "paper" coordinates the function reserves
  #' the right margin itself.
  #'
  #' The boxes are added with add_shapes(), which keeps any shapes the plot
  #' already carries. Without this the boxes silently vanish on a plot that
  #' draws its own shapes (e.g. the tiles).
  #'
  #' @param plot A plotly plot object to which the legend will be added.
  #' @param legend_labels A vector of labels to display in the legend.
  #' @param min_color The color for the low end of the gradient, used to build a
  #'   single column of boxes when `box_colors` is not given.
  #' @param max_color The color for the high end of the gradient, used to build a
  #'   single column of boxes when `box_colors` is not given.
  #' @param legend_title The title shown above the boxes.
  #' @param coords The coordinate system: "paper" for fraction-of-frame boxes
  #'   centered vertically, or "data" for pixel-sized boxes pinned to the top.
  #' @param box_colors An optional list of color vectors, one per column, each
  #'   giving a column's box colors top to bottom. Use this to show several
  #'   palettes side by side (e.g. one per category) instead of a single gradient.
  #'   Supported in "data" coordinates.
  #' @param legend_box_size The size of each color box. A fraction of the frame in
  #'   "paper" coordinates, or pixels in "data" coordinates. The default suits each.
  #' @param legend_spacing_y The vertical space between boxes, in the same units
  #'   as `legend_box_size`.
  #' @param legend_start_x The left edge of the legend. Defaults to just right of
  #'   the frame in "paper" coordinates, or the left edge of the reserved right
  #'   margin in "data" coordinates.
  #' @param legend_start_y The vertical position of the legend title in "data"
  #'   coordinates. Ignored in "paper" coordinates, where the stack is centered.
  #' @param column_gap The horizontal space between columns, in pixels ("data").
  #' @param plot_width The width of the plot in pixels, used to place the legend
  #'   in "data" coordinates.
  #' @param right_margin The width of the right margin to leave for the legend.
  #' @param font_size The font size for the title and labels in "data" coordinates.
  #' @return A plotly plot object with the legend added.
  #' @export
  #' @importFrom plotly layout
  add_categorical_legend <- function(plot, legend_labels, min_color = NULL, max_color = NULL,
                                     legend_title = NULL,
                                     coords = c("paper", "data"),
                                     box_colors = NULL,
                                     legend_box_size = NULL,
                                     legend_spacing_y = NULL,
                                     legend_start_x = NULL,
                                     legend_start_y = NULL,
                                     column_gap = 4,
                                     plot_width = 900,
                                     right_margin = 200,
                                     font_size = 13) {
    coords <- match.arg(coords)
    n_colors <- length(legend_labels)

    # Initialize lists for shapes and annotations
    shapes <- list()
    annotations <- list()

    if (coords == "paper") {
      # Set defaults for paper coordinates
      if (is.null(legend_box_size))  legend_box_size  <- 2
      if (is.null(legend_spacing_y)) legend_spacing_y <- 1
      if (is.null(legend_start_x))   legend_start_x   <- 1.05

      # Generate the color palette from the min and max colors
      palette <- generate_color_palette(n_colors = n_colors, min_color = min_color, max_color = max_color)

      # Calculate legend position and spacing
      total_legend_height <- n_colors * (legend_box_size + legend_spacing_y) / 100
      center_y <- 0.5  # Vertical center of the plot
      legend_start_y <- center_y + total_legend_height / 2

      # Add title annotation for the vertical legend, centered at the plot
      annotations[[1]] <- list(
        x = legend_start_x,
        y = legend_start_y + (legend_spacing_y / 500),  # Slightly above the first box
        text = legend_title,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 14, color = "black"),
        xshift = 0,
        yshift = 0
      )

      # Calculate y positions and add boxes and labels for the centered vertical legend
      for (i in 1:n_colors) {
        y_position <- legend_start_y - ((i - 1) * (legend_spacing_y / 100)) - ((legend_box_size * (i - 1)) / 100)

        # Get scale ratio
        xaxis_scaleratio <- plot$x$layout$xaxis$scaleratio
        yaxis_scaleratio <- plot$x$layout$yaxis$scaleratio
        x_to_y_ratio <- xaxis_scaleratio/yaxis_scaleratio

        if (length(x_to_y_ratio) == 0) {
          x1_position <- legend_start_x + (legend_box_size / 100)
        } else {
          x1_position <- legend_start_x + (legend_box_size / (100 * x_to_y_ratio))
        }

        # Add shape for the legend box
        shapes[[i]] <- list(
          type = "rect",
          xref = "paper",
          yref = "paper",
          x0 = legend_start_x,
          x1 = x1_position,
          y0 = y_position - (legend_box_size / 100),
          y1 = y_position,
          fillcolor = palette[i],
          line = list(color = palette[i])
        )

        # Add annotation for the label next to the box
        annotations[[i + 1]] <- list(
          x = x1_position + 0.005,
          y = y_position - (legend_box_size / 200),
          text = legend_labels[i],
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "middle",
          font = list(size = 12),
          xshift = 5  # Fixed pixel padding
        )
      }
    } else {
      # Set defaults for data coordinates
      if (is.null(legend_box_size))  legend_box_size  <- 16
      if (is.null(legend_spacing_y)) legend_spacing_y <- 8
      if (is.null(legend_start_x))   legend_start_x   <- plot_width - right_margin
      if (is.null(legend_start_y))   legend_start_y   <- 12

      # Default to one gradient column
      if (is.null(box_colors)) {
        box_colors <- list(generate_color_palette(n_colors = n_colors, min_color = min_color, max_color = max_color))
      }
      n_columns <- length(box_colors)

      # Wrap the title so it stays within the reserved margin
      legend_title <- wrap_text(legend_title, right_margin - 12, font_size)
      n_title_lines <- length(strsplit(legend_title, "<br>", fixed = TRUE)[[1]])
      title_height <- n_title_lines * font_size * 1.2

      # Add title annotation above the boxes
      annotations[[length(annotations) + 1]] <- list(
        x = legend_start_x,
        y = legend_start_y,
        text = legend_title,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        showarrow = FALSE,
        font = list(size = font_size, color = "#333333")
      )

      # Lay out one row per label
      first_box_top <- legend_start_y + title_height + legend_spacing_y
      labels_x <- legend_start_x + n_columns * (legend_box_size + column_gap) - column_gap + 8

      for (i in seq_len(n_colors)) {
        box_top <- first_box_top + (i - 1) * (legend_box_size + legend_spacing_y)
        box_bottom <- box_top + legend_box_size

        # Add one box per column for this row
        for (j in seq_len(n_columns)) {
          box_left <- legend_start_x + (j - 1) * (legend_box_size + column_gap)

          shapes[[length(shapes) + 1]] <- list(
            type = "rect",
            x0 = box_left,
            x1 = box_left + legend_box_size,
            y0 = box_top,
            y1 = box_bottom,
            fillcolor = box_colors[[j]][i],
            line = list(color = box_colors[[j]][i])
          )
        }

        # Add the shared row label to the right of the last column
        annotations[[length(annotations) + 1]] <- list(
          x = labels_x,
          y = (box_top + box_bottom) / 2,
          text = legend_labels[i],
          xanchor = "left",
          yanchor = "middle",
          showarrow = FALSE,
          font = list(size = font_size, color = "#333333")
        )
      }
    }

    # Add the boxes
    plot <- add_shapes(plot, shapes)

    # Apply the labels
    if (coords == "paper") {
      plot <- plot |>
        plotly::layout(
          annotations = annotations,
          margin = list(r = right_margin)  # Make room for the legend on the right
        )
    } else {
      plot <- plot |>
        plotly::layout(
          annotations = annotations
        )
    }

    return(plot)
  }
  
  #' Create Placeholder Plot with Custom Message
  #' 
  #' This function generates a placeholder plot with a custom message displayed in the center of the plot area.
  #' The message can be used to indicate that no results were found or to provide other information to the user.
  #' 
  #' @param message The message to display in the center of the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x0 The x-coordinate of the lower left corner of the message box.
  #' @param y0 The y-coordinate of the lower left corner of the message box.
  #' @param x1 The x-coordinate of the upper right corner of the message box.
  #' @param y1 The y-coordinate of the upper right corner of the message box.
  #' @param font_size The font size of the message text.
  #' @param font_color The color of the message text.
  #' @param bgcolor The background color of the message box.
  #' @param rect_color The color of the border of the message box.
  #' @param rect_fillcolor The fill color of the message box.
  #' @return A plotly plot object displaying the custom message.
  #' @examples
  #' @export
  #' @importFrom plotly plot_ly layout
  plot_message <- function(message = "No results found",coord_fixed = TRUE,
                           x0 = -0.5, y0 = -0.5, x1 = 0.5, y1 = 0.5,
                           font_size = 20, font_color = "black",
                           bgcolor = "rgba(255, 255, 255, 0.0)", rect_color = "rgba(0, 0, 0, 0.5)",rect_fillcolor = "rgba(0, 0, 0, 0.25)") {
    # Get axis settings
    axis_settings <- generate_axis_settings(
      showticklabels.x = FALSE,
      showticklabels.y = FALSE,
      ticklen.x = 0,
      ticklen.y = 0,
      coord_fixed = coord_fixed
    )
    
    # Make plot
    plot <- plotly::plot_ly(
      type = "scatter",
      mode = "none"  # No markers, lines, or text
    ) |>
      plotly::layout(
        shapes = list(
          list(
            type = "rect",
            x0 = x0, y0 = y0, x1 = x1, y1 = y1,
            fillcolor = rect_fillcolor,
            line = list(color = rect_color),
            layer = "above"  # Set the rectangle to be above the other elements
          )
        ),
        annotations = list(
          x = (x0 + x1) / 2,  # Center the annotation in the x direction
          y = (y0 + y1) / 2,  # Center the annotation in the y direction
          text = message,
          showarrow = FALSE,
          xref = "x",  
          yref = "y",  
          xanchor = "center",
          yanchor = "middle",
          font = list(size = font_size, color = font_color),
          bgcolor = bgcolor,
          borderpad = 10
        ),
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    return(plot)
  }
  
  #' Measure Text Width
  #'
  #' This function estimates the width needed for a tile label.
  #'
  #' @param text A character string.
  #' @param font_size The font size in points.
  #' @return The approximate width in pixels.
  #' @export
  measure_text_width <- function(text, font_size = 13) {
    width_inches <- grid::convertWidth(
      grid::grobWidth(
        grid::textGrob(
          label = text,
          gp = grid::gpar(fontsize = font_size)
        )
      ),
      unitTo = "in",
      valueOnly = TRUE
    )
    
    return(width_inches * 96)
  }
  
  #' Shorten Labels
  #'
  #' This function shortens labels that exceed a maximum rendered width and
  #' appends an ellipsis. Labels within the limit are returned unchanged. Factor
  #' inputs are returned as factors with shortened values.
  #'
  #' @param labels The labels to shorten.
  #' @param max_char The maximum label width in standard-character units. One
  #'   unit is the rendered width of the number `0`. Ignored when `max_pixels`
  #'   is supplied.
  #' @param max_pixels Optional maximum rendered width in pixels.
  #' @param font_size The font size used to measure rendered text width.
  #' @param ellipsis The character appended when text is shortened.
  #' @return The labels, shortened where they exceed the width limit.
  #' @export
  shorten_labels <- function(
    labels,
    max_char = 20,
    max_pixels = NULL,
    font_size = 12,
    ellipsis = "\u2026"
  ) {
    # Record input type
    was_factor <- is.factor(labels)
    
    # Convert labels to text
    labels <- as.character(labels)
    
    # Calculate maximum width when not supplied directly
    if (is.null(max_pixels)) {
      max_pixels <- measure_text_width(
        strrep("0", max_char),
        font_size
      )
    }
    
    # Shorten individual labels
    short_labels <- vapply(
      labels,
      function(label) {
        # Preserve missing labels
        if (is.na(label)) {
          return(NA_character_)
        }
        
        # Keep labels that fit
        if (measure_text_width(label, font_size) <= max_pixels) {
          return(label)
        }
        
        # Reserve ellipsis width
        text_width <- max_pixels - measure_text_width(ellipsis, font_size)
        
        # Handle insufficient width
        if (text_width <= 0) {
          return(ellipsis)
        }
        
        # Initialize binary search
        lower <- 0L
        upper <- nchar(label)
        best <- 0L
        
        # Find longest fitting text
        while (lower <= upper) {
          middle <- (lower + upper) %/% 2L
          candidate <- substr(label, 1, middle)
          
          if (measure_text_width(candidate, font_size) <= text_width) {
            best <- middle
            lower <- middle + 1L
          } else {
            upper <- middle - 1L
          }
        }
        
        # Handle empty result
        if (best == 0L) {
          return(ellipsis)
        }
        
        # Append ellipsis
        return(paste0(substr(label, 1, best), ellipsis))
      },
      character(1)
    )
    
    # Restore factor input
    if (was_factor) {
      short_labels <- factor(
        short_labels,
        levels = unique(short_labels)
      )
    }
    
    return(short_labels)
  }

  #' Wrap Label to a Width
  #'
  #' This function breaks a label into lines that each fit within a maximum
  #' pixel width, so a long label stays inside its column instead of running
  #' into neighboring content. Words are kept whole; a single word wider than
  #' the limit is left on its own line.
  #'
  #' @param text The label to wrap.
  #' @param max_width The maximum line width in pixels.
  #' @param font_size The font size the label is drawn at.
  #' @return The label with line breaks inserted as `<br>`.
  #' @export
  wrap_text <- function(text, max_width, font_size = 13) {
    words <- strsplit(text, "\\s+")[[1]]
    
    lines <- character(0)
    current <- ""
    
    for (word in words) {
      candidate <- if (nchar(current) == 0) word else paste(current, word)
      
      # Keep adding words while they fit; always place at least one word per line
      if (measure_text_width(candidate, font_size) <= max_width || nchar(current) == 0) {
        current <- candidate
      } else {
        lines <- c(lines, current)
        current <- word
      }
    }
    
    if (nchar(current) > 0) {
      lines <- c(lines, current)
    }
    
    paste(lines, collapse = "<br>")
  }
  

# --- Plot tiles ---
  #' Set Traits as Visible or Hidden
  #'
  #' This function takes a dataframe of traits and their values and marks
  #' traits as hidden if their value is below a threshold.  The number of
  #' hidden traits is counted and recorded in a separate row.
  #'
  #' @param df A data frame containing `var`, `y`, and `z`.
  #' @param display_threshold The minimum value of z required to show y.
  #' @return A formatted data frame ready for `plot_tile()`.
  #' @export
  #' @importFrom dplyr arrange bind_rows count desc filter mutate
  set_trait_visibility <- function(df,
                                   display_threshold = 0
  ) {
    # Set var if not supplied
    if (!"var" %in% names(df)) {
      df <- df |>
        dplyr::mutate(var = "traits")
    }
    
    # Get groups of var
    var_groups <- unique(df$var)
    
    # Identify var to show
    visible <- df |>
      dplyr::mutate(
        var = as.character(var),
        y = as.character(y),
        hidden = FALSE
      ) |>
      dplyr::filter(z >= display_threshold)
    
    # Count traits that were hidden
    hidden <- df |>
      dplyr::mutate(var = as.character(var)) |>
      dplyr::filter(z < display_threshold) |>
      dplyr::count(var, name = "n_hidden")
    
    if (nrow(hidden) > 0) {
      hidden <- hidden |>
        dplyr::mutate(
          y = ifelse(
            n_hidden == 1,
            "+1 other trait",
            paste0("+ ", n_hidden, " other traits")
          ),
          z = NA_real_,
          hidden = TRUE
        ) |>
        dplyr::select(var, y, z, hidden)
      
      visible <- dplyr::bind_rows(visible, hidden)
    }
    
    # Sort traits within each category
    visible <- visible |>
      dplyr::mutate(var = factor(var, levels = var_groups)) |>
      dplyr::arrange(var, hidden, dplyr::desc(z), tolower(y))
    
    return(visible)
  }
  
  #' Create Rounded Rectangle Path
  #'
  #' This function creates a rounded rectangle for use in a Plotly shape.
  #'
  #' @param x0 The left edge.
  #' @param y0 The top edge.
  #' @param x1 The right edge.
  #' @param y1 The bottom edge.
  #' @param radius The corner radius.
  #' @return A character string describing the rectangle.
  #' @export
  create_rounded_rectangle <- function(x0, y0, x1, y1, radius = 8) {
    radius <- min(radius, (x1 - x0) / 2, (y1 - y0) / 2)
    
    path <- paste(
      "M", x0 + radius, y0,
      "L", x1 - radius, y0,
      "Q", x1, y0, x1, y0 + radius,
      "L", x1, y1 - radius,
      "Q", x1, y1, x1 - radius, y1,
      "L", x0 + radius, y1,
      "Q", x0, y1, x0, y1 - radius,
      "L", x0, y0 + radius,
      "Q", x0, y0, x0 + radius, y0,
      "Z"
    )
    
    return(path)
  }
  
  #' Pack Trait Tiles into Rows
  #'
  #' This function moves a tile to the next row when it no longer fits.
  #'
  #' @param df A data frame containing `tile_width`.
  #' @param available_width The width available for tiles.
  #' @param box_gap The space between tiles.
  #' @return The input data frame with tile positions added.
  #' @export
  pack_trait_tiles <- function(df, available_width, box_gap = 6) {
    current_x <- 0
    current_row <- 1
    
    df$tile_x <- NA_real_
    df$tile_row <- NA_integer_
    
    for (i in seq_len(nrow(df))) {
      tile_width <- df$tile_width[i]
      
      if (tile_width > available_width) {
        # Warn and keep the oversized tile
        warning(
          "The tile label '",
          df$y[i],
          "' is wider than the available width and may overflow."
        )
      }
      
      if (current_x > 0 & current_x + box_gap + tile_width > available_width) {
        current_x <- 0
        current_row <- current_row + 1
      }
      
      if (current_x > 0) {
        current_x <- current_x + box_gap
      }
      
      df$tile_x[i] <- current_x
      df$tile_row[i] <- current_row
      
      current_x <- current_x + tile_width
    }
    
    return(df)
  }
  
  #' Scale Font Size
  #'
  #' This function adjusts a font size to match the size of a plot. The font is
  #' held steady across a neutral band of sizes around the reference, so the
  #' reference proportions are preserved and only the tile packing reflows. The
  #' font is scaled only when the plot is much narrower or much wider than the
  #' reference, and is always clamped to the supplied bounds. When a height is
  #' also supplied, the more constrained of width and height drives the scaling.
  #'
  #' @param font_size The font size used at the reference size.
  #' @param plot_width The current width of the plot.
  #' @param reference_width The width at which `font_size` is used without adjustment.
  #' @param plot_height Optional. The current height of the plot.
  #' @param reference_height Optional. The height at which `font_size` is used
  #'   without adjustment. Required if `plot_height` is supplied.
  #' @param min_font_size The smallest font size allowed.
  #' @param max_font_size The largest font size allowed.
  #' @param hold_lower The lower edge of the neutral band, as a fraction of the
  #'   reference size. At or above this fraction the font is held at `font_size`.
  #' @param hold_upper The upper edge of the neutral band, as a fraction of the
  #'   reference size. At or below this fraction the font is held at `font_size`.
  #' @return The adjusted font size.
  #' @export
  scale_font_size <- function(font_size,
                              plot_width,
                              reference_width,
                              plot_height = NULL,
                              reference_height = NULL,
                              min_font_size = 10,
                              max_font_size = 18,
                              hold_lower = 0.7,
                              hold_upper = 1.5) {
    # Scale by width, using the more constrained dimension when a height is given
    ratio <- plot_width / reference_width
    
    if (!is.null(plot_height) && !is.null(reference_height)) {
      ratio <- min(ratio, plot_height / reference_height)
    }
    
    # Hold the font steady within the neutral band
    if (ratio >= hold_lower && ratio <= hold_upper) {
      return(font_size)
    }
    
    # Outside the band, scale from the nearer band edge so the font changes
    # continuously as the plot keeps shrinking or growing.
    if (ratio < hold_lower) {
      scaled_font_size <- font_size * (ratio / hold_lower)
    } else {
      scaled_font_size <- font_size * (ratio / hold_upper)
    }
    
    # Clamp to bounds
    scaled_font_size <- max(
      min_font_size,
      min(max_font_size, scaled_font_size)
    )
    
    return(scaled_font_size)
  }
  
  #' Prepare Tile Header
  #'
  #' This function prepares the heading text shown above a tile plot.
  #' One heading sits above the category-label column and another sits above the
  #' tile area, so each region of the plot can be named. Each heading is drawn on
  #' a shaded bar matching the width of the region it labels. Either heading may
  #' be omitted, and when both are omitted no space is reserved.
  #'
  #' @param label_header The heading shown above the category-label column.
  #' @param tile_header The heading shown above the tile area.
  #' @param plot_width The width of the plot in pixels.
  #' @param label_width The width reserved for category labels.
  #' @param font_size The font size for the headings.
  #' @param bar_color The fill color of the heading bars.
  #' @param text_color The color of the heading text.
  #' @return A list containing shapes, annotations, and the height reserved for
  #'   the header.
  #' @export
  prepare_tile_header <- function(label_header = NULL,
                                  tile_header = NULL,
                                  plot_width = 900,
                                  label_width = 190,
                                  font_size = 13,
                                  bar_color = "#c9c9c9",
                                  text_color = "#1f2937") {
    # Reserve no space when there is nothing to show
    if (is.null(label_header) && is.null(tile_header)) {
      return(list(
        shapes = list(),
        annotations = list(),
        height = 0
      ))
    }
    
    # Set header dimensions
    bar_height <- max(36, font_size * 2.6)
    bar_gap <- 6
    bottom_pad <- 10
    
    shapes <- list()
    annotations <- list()
    
    # Heading above the category-label column
    if (!is.null(label_header)) {
      shapes[[length(shapes) + 1]] <- list(
        type = "rect",
        x0 = 0,
        x1 = label_width - bar_gap,
        y0 = 0,
        y1 = bar_height,
        line = list(width = 0),
        fillcolor = bar_color,
        layer = "below"
      )
      
      annotations[[length(annotations) + 1]] <- list(
        x = (label_width - bar_gap) / 2,
        y = bar_height / 2,
        text = label_header,
        xanchor = "center",
        yanchor = "middle",
        showarrow = FALSE,
        font = list(size = font_size, color = text_color)
      )
    }
    
    # Heading above the tile area
    if (!is.null(tile_header)) {
      shapes[[length(shapes) + 1]] <- list(
        type = "rect",
        x0 = label_width,
        x1 = plot_width,
        y0 = 0,
        y1 = bar_height,
        line = list(width = 0),
        fillcolor = bar_color,
        layer = "below"
      )
      
      annotations[[length(annotations) + 1]] <- list(
        x = (label_width + plot_width) / 2,
        y = bar_height / 2,
        text = tile_header,
        xanchor = "center",
        yanchor = "middle",
        showarrow = FALSE,
        font = list(size = font_size, color = text_color)
      )
    }
    
    return(list(
      shapes = shapes,
      annotations = annotations,
      height = bar_height + bottom_pad
    ))
  }
  
  #' Get Height of a Data-Coordinate Categorical Legend
  #'
  #' This function reports the pixel height a "data" coordinate categorical legend
  #' occupies, so a caller drawing in pixel coordinates can keep the plot tall
  #' enough to show the whole legend. It mirrors the geometry
  #' `add_categorical_legend()` uses in "data" coordinates.
  #'
  #' @param legend_labels The labels shown beside the color boxes.
  #' @param legend_title The title shown above the boxes.
  #' @param font_size The font size for the title and labels.
  #' @param right_margin The width of the right margin reserved for the legend.
  #' @param legend_box_size The size of each color box, in pixels.
  #' @param legend_spacing_y The vertical space between boxes, in pixels.
  #' @return The legend height in pixels.
  #' @export
  get_legend_height <- function(legend_labels, legend_title = NULL, font_size = 13,
                                        right_margin = 200, legend_box_size = 16,
                                        legend_spacing_y = 8) {
    n_colors <- length(legend_labels)
    
    # The title wraps to fit the margin, so its height depends on the line count
    legend_title <- wrap_text(legend_title, right_margin - 12, font_size)
    n_title_lines <- length(strsplit(legend_title, "<br>", fixed = TRUE)[[1]])
    title_height <- n_title_lines * font_size * 1.2
    
    title_height + legend_spacing_y +
      n_colors * legend_box_size +
      max(n_colors - 1, 0) * legend_spacing_y
  }
  
  #' Format Organisms in Hover Text
  #'
  #' This function turns the semicolon-delimited organism and value columns from
  #' `results_to_plot(plot_type = "tile")` into the lines shown in the
  #' tile hover. Organisms are listed most-confident first, and the list is capped
  #' so a widely shared trait does not produce an unwieldy tooltip; any organisms
  #' beyond the cap are summarized as a count on a final line.
  #'
  #' @param x_text A semicolon-delimited character string of organism names.
  #' @param z_text A semicolon-delimited character string of values on a 0-1 scale.
  #' @param max_shown The largest number of organisms listed before the remainder
  #'   are summarized as a count.
  #' @param value_label The label shown before each value in the hover text.
  #' @param value_scale The number each value is multiplied by before display.
  #'   Use 100 for probabilities stored on a 0-1 scale, and 1 for fluxes.
  #' @param value_suffix Text shown after each formatted value.
  #' @param value_digits Number of digits used by `round()`. Use NULL to keep the
  #'   scaled value as-is.
  #' @return A single character string with one organism per line, or NULL when
  #'   there are no organisms to show.
  #' @export
  format_organism_text <- function(x_text,
                                z_text,
                                max_shown = 10,
                                value_label = "Probability",
                                value_scale = 100,
                                value_suffix = "%",
                                value_digits = 0) {
    # Hidden ("+N other traits") tiles and traits with no positives carry no
    # organism list.
    if (is.null(x_text) || is.null(z_text) || is.na(x_text) || is.na(z_text) ||
       x_text == "" || z_text == "") {
      return(NULL)
    }
    
    # Build a flat data frame for sorting and the tooltip
    positives <- data.frame(
      x_text = trimws(strsplit(x_text, ";", fixed = TRUE)[[1]]),
      z_text = suppressWarnings(as.numeric(trimws(strsplit(z_text, ";", fixed = TRUE)[[1]]))),
      stringsAsFactors = FALSE
    )
    
    # Drop any malformed pairs created by missing or nonnumeric values.
    positives <- positives[!is.na(positives$x_text) & positives$x_text != "" & !is.na(positives$z_text), ]
    
    if (nrow(positives) == 0) {
      return(NULL)
    }
    
    # List the most confident organisms first so the cap keeps the strongest
    # positives.
    positives <- positives[
      order(-positives$z_text, tolower(positives$x_text)),
    ]
    
    # Format the values for the hover text. Probabilities are usually converted
    # from 0-1 to 0-100 and shown with %, while fluxes can be shown as raw values.
    values <- positives$z_text * value_scale
    if (!is.null(value_digits)) {
      values <- round(values, value_digits)
    }
    
    # Format each line as "Organism (Label: value)".
    lines <- paste0(
      positives$x_text,
      " (", value_label, ": ", values, value_suffix, ")"
    )
    
    # Show the first organisms, then summarize how many were left off.
    n_extra <- length(lines) - max_shown
    
    if (n_extra > 0) {
      lines <- c(
        lines[seq_len(max_shown)],
        paste0(
          "+ ", n_extra,
          ifelse(n_extra == 1, " more organism", " more organisms")
        )
      )
    }
    
    paste(lines, collapse = "<br>")
  }
  
  #' Format Tiles for Plotting
  #'
  #' This function prepares the category labels, tiles, and hover text for a
  #' tile plot.
  #'
  #' @param groups A list of data frames containing packed tile positions.
  #' @param palettes A named list of color palettes. Each trait category has a
  #'   palette representing values from 0 to 100.
  #' @param start_y The vertical position where the first category begins.
  #' @param plot_width The width of the plot in pixels.
  #' @param label_width The width reserved for category labels.
  #' @param font_size The font size for labels.
  #' @param box_height The height of each tile.
  #' @param row_gap The space between rows.
  #' @param group_gap The space below each category.
  #' @param hover_value_label The label shown before each value in the hover text.
  #' @param hover_value_scale The number each hover value is multiplied by before display.
  #' @param hover_value_suffix Text shown after each formatted hover value.
  #' @param hover_value_digits Number of digits used by `round()`. Use NULL to
  #'   keep the scaled value as-is.
  #' @return A list containing shapes, annotations, hover data, and the final
  #'   vertical position.
  #' @export
  #' @importFrom dplyr bind_rows
  format_tiles <- function(groups,
                           palettes,
                           start_y,
                           plot_width = 900,
                           label_width = 190,
                           font_size = 13,
                           box_height = 42,
                           row_gap = 6,
                           group_gap = 16,
                           hover_value_label = "Probability",
                           hover_value_scale = 100,
                           hover_value_suffix = "%",
                           hover_value_digits = 0) {
    # Set colors
    value_to_fill <- function(value, group_name) {
      palettes[[group_name]][pmax(0, pmin(100, round(value))) + 1]
    }
    
    value_to_text_color <- function(value) {
      scaled_value <- pmax(0, pmin(1, (value - 0) / 5))
      
      grDevices::colorRamp(
        c("grey100", "#FFFFFF")
      )(scaled_value) |>
        grDevices::rgb(maxColorValue = 255)
    }
    
    other_fill <- muted_fill
    other_text <- muted_text
    
    # Set up plot contents
    shapes <- list()
    annotations <- list()
    hover_data <- list()
    current_y <- start_y
    
    # Add categories and tiles
    for (group_name in names(groups)) {
      group_data <- groups[[group_name]]
      n_rows <- max(group_data$tile_row)
      
      group_height <- (
        n_rows * box_height +
          max(n_rows - 1, 0) * row_gap +
          group_gap
      )
      
      # Add line above category
      shapes[[length(shapes) + 1]] <- list(
        type = "line",
        x0 = 0,
        x1 = plot_width,
        y0 = current_y,
        y1 = current_y,
        line = list(color = "#d7d7d7", width = 1)
      )
      
      # Add category label
      # Wrap long category names so they stay within the label column instead
      # of running into the tiles
      annotations[[length(annotations) + 1]] <- list(
        x = 0,
        y = current_y + box_height / 2,
        text = wrap_text(group_name, label_width - 12, font_size),
        xanchor = "left",
        yanchor = "middle",
        align = "left",
        showarrow = FALSE,
        font = list(size = font_size, color = "#333333")
      )
      
      # Add tiles
      for (i in seq_len(nrow(group_data))) {
        x0 <- label_width + group_data$tile_x[i]
        x1 <- x0 + group_data$tile_width[i]
        y0 <- current_y + (group_data$tile_row[i] - 1) * (box_height + row_gap) + 4
        y1 <- y0 + box_height - 8
        
        if (group_data$hidden[i]) {
          fill <- other_fill
          text_color <- other_text
          label <- paste0("<i>", group_data$y[i], "</i>")
          hover <- paste0("<b>", group_data$y_full[i], "</b><br>", group_name)
        } else {
          fill <- value_to_fill(group_data$z[i], group_name)
          text_color <- value_to_text_color(group_data$z[i])
          label <- paste0(group_data$y[i], "<br><b>", round(group_data$z[i]), "%</b>")
          
          # List positive organisms
          hover <- format_organism_text(
            x_text = group_data$x_text[i],
            z_text = group_data$z_text[i],
            value_label = hover_value_label,
            value_scale = hover_value_scale,
            value_suffix = hover_value_suffix,
            value_digits = hover_value_digits
          )
          
          if (is.null(hover)) {
            hover <- paste0(
              "Trait category: ", group_name, 
              "<br>Trait: ", group_data$y_full[i],
              "<br>No positive organisms"
              )
          } else {
            hover <- paste0(
              "Trait category: ", group_name, 
              "<br>Trait: ", group_data$y_full[i],
              "<br>Positive organisms: <br>", 
              hover
            )
          }
          
        }
        
        # Add tile
        shapes[[length(shapes) + 1]] <- list(
          type = "path",
          path = create_rounded_rectangle(x0, y0, x1, y1),
          line = list(
            color = colorspace::darken(fill, amount = 0.25),
            width = 1
          ),
          fillcolor = fill,
          layer = "below"
        )
        
        # Add tile label
        annotations[[length(annotations) + 1]] <- list(
          x = (x0 + x1) / 2,
          y = (y0 + y1) / 2,
          text = label,
          xanchor = "center",
          yanchor = "middle",
          showarrow = FALSE,
          font = list(size = font_size, color = text_color)
        )
        
        # Add hover text
        hover_data[[length(hover_data) + 1]] <- data.frame(
          x = (x0 + x1) / 2,
          y = (y0 + y1) / 2,
          hover = hover,
          stringsAsFactors = FALSE
        )
      }
      
      current_y <- current_y + group_height
    }
    
    return(list(
      shapes = shapes,
      annotations = annotations,
      hover_data = dplyr::bind_rows(hover_data),
      current_y = current_y
    ))
  }
  
  #' Make Tile Plot
  #'
  #' This function displays variables as grouped tiles. Tile width is based on the
  #' label, and tiles move onto another row when needed. variables are grouped by
  #' category (`var`) and shaded by the share of organisms predicted positive
  #' (`z`). This replaces `plot_summary()` as the overview of prediction results.
  #'
  #' Tiles reflow with the plot width, so the caller supplies the rendered pixel
  #' width (e.g. from `get_plotly_dimensions()`). The plot renders at its natural
  #' content height so tiles keep their designed size and text always fits; in
  #' the app the surrounding card scrolls when that height exceeds its maximum.
  #'
  #' @param df A data frame produced by `results_to_plot()` with
  #'   `plot_type = "tile"` (columns `var`, `y`, `z`).
  #' @param display_threshold The minimum percentage required to show a trait.
  #'   variables below this are collapsed into a single "+N other variables" tile.
  #' @param plot_width The width of the plot in pixels.
  #' @param label_width The width reserved for category labels.
  #' @param font_size The font size for labels.
  #' @param reference_width The width at which `font_size` is used unadjusted.
  #' @param min_font_size The smallest font size allowed.
  #' @param max_font_size The largest font size allowed.
  #' @param box_height The height of each tile.
  #' @param box_gap The space between tiles.
  #' @param row_gap The space between rows.
  #' @param group_gap The space below each category.
  #' @param min_plot_height The minimum plot height in pixels. The natural height
  #'   is the height at which tiles render at `box_height` and text fits; this
  #'   floors that value so sparse profiles are not drawn too short.
  #' @param showlegend A logical value indicating whether to show the legend.
  #' @param legend_title The title shown above the color scale.
  #' @param right_margin The width of the right margin reserved for the legend.
  #' @param legend_gap The horizontal space between the tile area and the legend,
  #'   in pixels. It is taken out of the tile area rather than added to the plot,
  #'   so the legend stays inside the drawn width. Use NULL to scale it with the
  #'   font size.
  #' @param label_header The heading shown above the category-label column.
  #' @param tile_header The heading shown above the tile area.
  #' @param hover_value_label The label shown before each value in the hover text.
  #' @param hover_value_scale The number each hover value is multiplied by before display.
  #' @param hover_value_suffix Text shown after each formatted hover value.
  #' @param hover_value_digits Number of digits used by `round()`. Use NULL to
  #'   keep the scaled value as-is.
  #' @return A plotly plot object displaying the tiles. The natural
  #'   (data-unit) height is both used to size the widget and attached as
  #'   `attr(plot, "plot_height")` for callers that need it.
  #' @export
  #' @importFrom plotly config layout plot_ly
  plot_tile <- function(df,
                        display_threshold = 1,
                        plot_width = 900,
                        label_width = 190,
                        font_size = 13,
                        reference_width = 900,
                        min_font_size = 10,
                        max_font_size = 18,
                        box_height = 42,
                        box_gap = 6,
                        row_gap = 6,
                        group_gap = 16,
                        min_plot_height = 0,
                        showlegend = TRUE,
                        legend_title = "% organisms positive",
                        right_margin = 200,
                        legend_gap = NULL,
                        label_header = NULL,
                        tile_header = NULL,
                        hover_value_label = "Probability",
                        hover_value_scale = 100,
                        hover_value_suffix = "%",
                        hover_value_digits = 0,
                        truncate_labels = TRUE) {
    # Prepare data
    df <- set_trait_visibility(df, display_threshold = display_threshold)
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    font_size <- scale_font_size(
      font_size = font_size,
      plot_width = plot_width,
      reference_width = reference_width,
      min_font_size = min_font_size,
      max_font_size = max_font_size
    )
    box_height <- max(box_height, font_size * 3.2)
    
    # Set plot dimensions
    if (is.null(legend_gap)) {
      legend_gap <- round(font_size * 1.5)
    }
    legend_reserved <- if (showlegend) right_margin + legend_gap else 0
    available_width <- max(plot_width - label_width - legend_reserved, 120)
    content_width <- label_width + available_width
    
    header <- prepare_tile_header(
      label_header = label_header,
      tile_header = tile_header,
      plot_width = content_width,
      label_width = label_width,
      font_size = font_size
    )
    content_top <- max(12, header$height)
    
    legend_ticks <- c(0, 25, 50, 75, 100)
    legend_labels <- as.character(legend_ticks)
    legend_height <- if (showlegend) {
      get_legend_height(
        legend_labels = legend_labels,
        legend_title = legend_title,
        font_size = font_size,
        right_margin = right_margin
      )
    } else {
      0
    }
    
    # Size and pack tiles
    df$y_full <- df$y
    if (truncate_labels) {
      df$y <- vapply(
        df$y,
        shorten_labels,
        FUN.VALUE = character(1),
        max_pixels = available_width - 20,
        font_size = font_size,
        USE.NAMES = FALSE
      )
    }
    
    df$tile_width <- vapply(
      seq_len(nrow(df)),
      FUN.VALUE = numeric(1),
      FUN = function(i) {
        label_width_px <- measure_text_width(df$y[i], font_size)
        if (df$hidden[i]) {
          label_width_px + 20
        } else {
          max(label_width_px, measure_text_width("100%", font_size)) + 20
        }
      }
    )
    
    groups <- split(df, df$var, drop = TRUE)
    groups <- lapply(
      groups,
      pack_trait_tiles,
      available_width = available_width,
      box_gap = box_gap
    )
    
    # Set colors
    category_names <- names(groups)
    category_colors <- ggplot_hue_colors(length(category_names))
    names(category_colors) <- category_names
    
    palettes <- lapply(
      category_colors,
      function(color) {
        generate_color_palette(101, colorspace::lighten(color, amount = 0.75), color)
      }
    )
    legend_box_colors <- lapply(palettes, function(palette) palette[legend_ticks + 1])
    
    # Build plot contents
    tiles <- format_tiles(
      groups = groups,
      palettes = palettes,
      start_y = content_top,
      plot_width = content_width,
      label_width = label_width,
      font_size = font_size,
      box_height = box_height,
      row_gap = row_gap,
      group_gap = group_gap,
      hover_value_label = hover_value_label,
      hover_value_scale = hover_value_scale,
      hover_value_suffix = hover_value_suffix,
      hover_value_digits = hover_value_digits
    )
    
    shapes <- c(header$shapes, tiles$shapes)
    annotations <- c(header$annotations, tiles$annotations)
    plot_height <- max(
      tiles$current_y + 6,
      content_top + legend_height + 6,
      min_plot_height
    )
    
    # Make plot
    plot <- plotly::plot_ly(
      data = tiles$hover_data,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "markers",
      text = ~hover,
      hoverinfo = "text",
      height = plot_height,
      marker = list(size = 24, opacity = 0.01)
    ) |>
      plotly::layout(
        autosize = TRUE,
        shapes = shapes,
        annotations = annotations,
        xaxis = list(
          range = c(0, plot_width),
          visible = FALSE,
          fixedrange = TRUE
        ),
        yaxis = list(
          range = c(plot_height, 0),
          visible = FALSE,
          fixedrange = TRUE
        ),
        margin = list(l = 4, r = 4, t = 4, b = 4),
        showlegend = FALSE,
        hovermode = "closest",
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) |>
      plotly::config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
    
    # Add legend
    if (showlegend) {
      plot <- add_categorical_legend(
        plot,
        legend_labels = legend_labels,
        box_colors = legend_box_colors,
        legend_title = legend_title,
        coords = "data",
        legend_start_x = content_width + legend_gap,
        legend_start_y = content_top,
        plot_width = plot_width,
        right_margin = right_margin,
        font_size = font_size
      )
    }
    
    attr(plot, "plot_height") <- plot_height
    return(plot)
  }
  

# --- Plot heatmap ---
  #' Build Heatmap Hover Customdata and Template
  #'
  #' This function prepares the per-cell customdata and the hovertemplate a heatmap trace needs
  #' to show its row label, and any uploaded metadata for that row, in the hover.  It is the
  #' heatmap's counterpart to adding metadata columns to a point's label: the row label always
  #' shows, and the metadata lines are added underneath when there are any.
  #'
  #' The customdata is a grid the size of the matrix, so every cell of a row carries that row's
  #' label and metadata.  The template is rewritten to read them: %{y} becomes the label held in
  #' customdata, since the axis is drawn with shortened labels while the hover should show the
  #' full one, and the metadata is spliced in before the <extra> tag when there is any.
  #'
  #' @param mat The matrix being plotted, used only for its number of rows and columns.
  #' @param row_labels The full (unshortened) label of each row, shown in the hover.
  #' @param metadata A data frame of uploaded metadata, as returned by get_metadata_from_upload(),
  #'   or NULL.  Its rows are matched to row_labels and its columns added to the hover.
  #' @param metadata_id_col The metadata column matched against row_labels, or NULL to take it
  #'   from the metadata's id_col attribute or its first column.
  #' @param hovertemplate The hovertemplate to rewrite, or NULL to leave it unset.
  #' @return A list with customdata (a row-by-column list) and hovertemplate (the rewritten
  #'   string, or NULL).
  #' @export
  build_heatmap_hover <- function(mat, row_labels, metadata = NULL, metadata_id_col = NULL,
                                  hovertemplate = NULL) {
    # Format optional metadata
    metadata_rows <- align_metadata_rows(
      organism_keys = row_labels,
      metadata = metadata,
      id_col = metadata_id_col
    )
    
    meta_rows <- if (is.null(metadata_rows)) {
      rep("", length(row_labels))
    } else {
      format_row_hover(
        metadata_rows,
        leading = "<br>"
      )
    }
    
    # Build data for each cell
    customdata <- lapply(seq_len(nrow(mat)), function(i) {
      lapply(seq_len(ncol(mat)), function(j) {
        list(row_labels[[i]], meta_rows[[i]])
      })
    })
    
    # Put metadata immediately after organism
    if (!is.null(hovertemplate)) {
      hovertemplate <- gsub(
        pattern = "%{y}",
        replacement = "%{customdata[0]}%{customdata[1]}",
        x = hovertemplate,
        fixed = TRUE
      )
    }
    
    return(list(customdata = customdata, hovertemplate = hovertemplate))
  }
  
  #' Make Main Heatmap Plot
  #' 
  #' This function generates a heatmap plot based on the provided data frame, allowing customization of colors, labels, and layout.
  #' 
  #' @param df A data frame containing the values to be plotted.
  #' @param title The title of the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param horizontal_border The size of the horizontal border between cells.
  #' @param vertical_border The size of the vertical border between cells.
  #' @param showlegend A logical value indicating whether to display a color legend for the plot.
  #' @param max_label_width The width in pixels each axis label may occupy. Labels wider than this are shortened with an ellipsis for display, while the full label is kept underneath so hover text still shows the complete name.
  #' @param label_font_size The font size the axis labels are measured at when deciding where to shorten them.
  #' @param min_color The color to use for the minimum value in the matrix.
  #' @param max_color The color to use for the maximum value in the matrix.
  #' @param hovertemplate The template for the hover text that appears when hovering over a cell in the plot.
  #' @param values_are_binary A logical value indicating whether to show each value as present or absent rather than as a number. Sub-threshold values have already been set to zero upstream, so a cell counts as present when it sits above zmin. Present cells take the max color and absent cells the min color, and the hover shows a plus or a minus in place of the value.
  #' @param binary_value_label What the value is called in the hover and the legend when values are shown as present or absent, such as "Value". NULL keeps the label the hover template and legend_title already carry, so the label only changes when a binary label is given.
  #' @param metadata A data frame of uploaded organism metadata, as returned by get_metadata_from_upload(), or NULL. Its identifier column is matched against the row labels of the heatmap (the organism names), and its remaining columns are added to each cell's hover text, one line each in the form "Header: value". A row whose organism has no metadata is left unchanged.
  #' @param metadata_id_col The metadata column matched against the row labels, or NULL to take it from the metadata's id_col attribute or, failing that, its first column.
  #' @param legend_labels The labels to display in the color legend.
  #' @param legend_title The title of the color legend.
  #' @param legend_box_size The size of the color legend boxes.
  #' @param legend_spacing_y The vertical spacing between color legend boxes.
  #' @param legend_start_x The x-coordinate where the color legend should start.
  #' @param right_margin The width of the right margin to leave for the color legend.
  #' @param zmin The minimum value for the color scale.
  #' @param zmax The maximum value for the color scale.
  #' @return A plotly plot object displaying the heatmap.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout hide_colorbar
  #' @importFrom dplyr select filter
  #' @importFrom colorspace lighten darken
  plot_heatmap <- function(df, title = NULL, coord_fixed = TRUE, x_to_y_ratio = 1, horizontal_border = 1, vertical_border = 1, showlegend = FALSE, max_label_width = 110, label_font_size = 12, max_char = 20, min_color = "black", max_color = "green", 
                           hovertemplate = "<b>X: %{x}</b><br><b>Y: %{y}</b><br><extra></extra>", values_are_binary = FALSE, binary_value_label = NULL, 
                           metadata = NULL, metadata_id_col = NULL,
                           legend_labels = c("0","1"), legend_title = NULL, legend_box_size = 2, legend_spacing_y = 1, legend_start_x = 1.05, right_margin = 200,
                           zmin = 0, zmax= 100) {
    # Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Format data
    if ("x" %in% colnames(df)) {
      mat <- as.matrix(df |> dplyr::select(-x))
    } else {
      mat <- as.matrix(df)
    }
    
    # Format axis labels
    full_x_labels <- colnames(mat)
    full_y_labels <- as.character(df$x)
    short_y_labels <- shorten_labels(full_y_labels, max_char)
    
    # Build hover customdata and template
    hover <- build_heatmap_hover(mat, full_y_labels, metadata = metadata,
                                 metadata_id_col = metadata_id_col, hovertemplate = hovertemplate)
    hover_customdata <- hover$customdata
    hovertemplate <- hover$hovertemplate
    
    # No per-cell text by default; the hover only draws it in binary mode
    text <- NULL
    
    # Collapse the values to presence and absence when asked, showing a plus or
    # a minus in place of the number
    if (isTRUE(values_are_binary)) {
      mat <- binarize_z(mat, zmin, zmax)
      text <- binary_symbols(mat, zmin)
      hovertemplate <- set_binary_hover_value(hovertemplate, binary_value_label)
    }
    
    # Set fill colors for main plot
    gradient_fill <- to_plotly_colorscale(min_color, max_color)
    
    # Set fill colors for background plot (endpoints nudged to provide a border)
    gradient_fill_background <- to_plotly_colorscale(
      colorspace::lighten(col = min_color, amount = 0.1),
      colorspace::darken(col = max_color, amount = 0.1)
    )
    
    # Set borders
    xgap <- horizontal_border
    ygap <- vertical_border
    
    # Make plot
    plot <- plotly::plot_ly() |>
      # Add background (provides border)
      plotly::add_trace(
        type = "heatmap",
        z = mat,
        x = full_x_labels,
        y = short_y_labels,
        colorscale = gradient_fill_background,
        showscale = showlegend,
        zmin = zmin,
        zmax = zmax,
        hoverinfo = "none"
      ) |>
      # Add main plot
      plotly::add_trace(
        type = "heatmap",
        z = mat,
        x = full_x_labels,
        y = short_y_labels,
        colorscale = gradient_fill,
        showscale = showlegend,
        zmin = zmin,
        zmax = zmax,
        xgap = xgap,
        ygap = ygap,
        text = text,
        customdata = hover_customdata,
        hovertemplate = hovertemplate
      )
    
    # Apply layout
    axis_settings <- generate_axis_settings(
      coord_fixed = coord_fixed,
      x_to_y_ratio = x_to_y_ratio,
      type = "category",   
      categoryorder = "array", 
      categoryarray.x = full_x_labels,
      categoryarray.y = short_y_labels,
      tickmode.x = "array",
      tickvals.x = seq_along(full_x_labels) - 1,
      ticktext.x = full_x_labels,
      tickfont.x = list(size = label_font_size),
      tickfont.y = list(size = label_font_size)
    ) 
    
    plot <- plot |>
      plotly::layout(
        title = title,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    # Format  x-axis tick labels
    plot <- plot |>
      htmlwidgets::onRender(sprintf(
        "function(el) { truncateAxisTickLabels(el, {axes: %s, full: %s}); }",
        jsonlite::toJSON(c("x")),
        jsonlite::toJSON(list(x = full_x_labels))
      ))
    
    # Format legend title
    if (isTRUE(values_are_binary)) {
      legend_labels <- c("-", "+")
      if (!is.null(binary_value_label) && nzchar(binary_value_label)) legend_title <- binary_value_label
    }
    
    plot <- add_categorical_legend(plot, legend_labels = legend_labels, min_color = min_color, max_color = max_color, legend_title = legend_title, 
                                   legend_box_size = legend_box_size, legend_spacing_y = legend_spacing_y, legend_start_x = legend_start_x, right_margin = right_margin)
    
    return(plot)
  }
  
# --- Plot treemap ---
  #' Pad Strings to a Given Length
  #'
  #' This helper function pads strings with spaces to achieve a specified target length.
  #'
  #' @param x A vector of strings to be padded.
  #' @param target_length The target length for each string after padding.
  #' @return A vector of padded strings, retaining factor status if input was a factor.
  #' @export
  #' @importFrom stringr str_pad
  pad_to_length <- function(x, target_length = 8) {
    was_factor <- is.factor(x) # Check if input is a factor
    
    x <- as.character(x) # Convert to character if a factor
    
    padded_strings <- sapply(x, function(str) {
      n <- nchar(str)
      if (n >= target_length) {
        return(str)
      }
      padding <- (target_length - n) / 2
      left_padding <- floor(padding)
      right_padding <- ceiling(padding)
      paste0(strrep(" ", left_padding), str, strrep(" ", right_padding))
    })
    
    if (was_factor) {
      padded_strings <- factor(padded_strings, levels = unique(padded_strings)) # Convert back to factor
    }
    
    return(padded_strings)
  }

  #' Make Main Treemap Plot
  #' 
  #' This function generates a treemap plot based on the provided data frame, allowing customization of labels, colors, and layout.
  #' 
  #' @param df A data frame containing the values to be plotted.
  #' @param title The title of the plot.
  #' @param max_colors The maximum number of colors to use in the treemap.
  #' @param hovertemplate The template for the hover text that appears when hovering over a cell in the plot.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @return A plotly plot object displaying the treemap.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  #' @importFrom colorspace rainbow_hcl
  plot_treemap <- function(df, title = NULL, max_colors = 8, 
                           hovertemplate = "<b>X: %{x}</b><br><b>Y: %{y}</b><br><extra></extra>",
                           coord_fixed = FALSE, x_to_y_ratio = 1) {
    # Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Format data
    df$y <- sapply(df$y, pad_to_length, target_length = 10)
    
    # Set colors
    num_colors <- min(nrow(df), max_colors)
    colors <- colorspace::rainbow_hcl(num_colors)
    df$color <- rep(colors, length.out = nrow(df))
    
    # Make plot
    plot <- plotly::plot_ly() |>
      plotly::add_trace(
        type = "treemap",
        ids = df$y,
        labels = df$y,
        parents = "",
        values = df$z,
        textinfo = "label",
        hovertemplate = hovertemplate,
        marker = list(colors = df$color, line = list(width = 2, color = "black")),
        insidetextfont = list(size = 40), 
        outsidetextfont = list(size = 0),  
        textposition = "middle center",
        tiling = list(pad = 0)
      ) 
    
    # Remove border for parent cell
    plot <- plot |>
    htmlwidgets::onRender("
      function(el) {
        function isBlack(s) {
          if (!s) return false;
          return s.replace(/\\s/g,'') === 'rgb(0,0,0)';
        }
        var obs = new MutationObserver(function() {
          el.querySelectorAll('svg path.surface').forEach(function(p) {
            if (isBlack(p.style.fill)) {
              p.style.stroke = 'none';
              p.style.fill   = 'none';
            }
          });
        });
        obs.observe(el, {subtree: true, attributes: true,
                         attributeFilter: ['style']});
      }
    ")
    
    # Apply layout
    axis_settings <- generate_axis_settings() 
    
    plot <- plot |> 
      plotly::layout(
        title = title,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    return(plot)
  }    
  
# --- Plot metabolic network ---
  #' Extract Graph Attributes
  #' 
  #' This helper function extracts vertex and edge attributes from an igraph object, returning them as a list.
  #' 
  #' @param graph An igraph object from which to extract attributes.
  #' @return A list containing vertex and edge attributes.
  #' @export
  #' @importFrom igraph vertex_attr edge_attr
  extract_graph_attr <- function(graph) {
    # Vertex attributes with defaults
    vertex_attr_defaults <- list(
      size = 15,
      size2 = 15,
      color = "SkyBlue2",
      frame.color = "black",
      frame.width = 1,
      shape = "circle",
      opacity = 1,
      name = NA,
      label = NA,
      label.family = "serif",
      label.font = 1,
      label.cex = 1,
      label.dist = 0,
      label.degree = -pi/4,
      label.color = "black"
    )
    
    # Edge attributes with defaults
    edge_attr_defaults <- list(
      color = "darkgrey",
      width = 1,
      opacity = 1,
      arrow.size = 1,
      arrow.width = 1,
      lty = 1,
      label = NA,
      ec = NA,
      eq = NA,
      rn = NA,
      ko = NA,
      label.family = "serif",
      label.font = 1,
      label.cex = 1,
      label.color = "black"
    )
    
    # Extract vertex attributes
    vertices <- lapply(names(vertex_attr_defaults), function(attr) {
      if (is.null(igraph::vertex_attr(graph, attr))) {
        rep(vertex_attr_defaults[[attr]], igraph::vcount(graph))
      } else {
        igraph::vertex_attr(graph, attr)
      }
    })
    names(vertices) <- names(vertex_attr_defaults)
    
    # Extract edge attributes
    edges <- lapply(names(edge_attr_defaults), function(attr) {
      if (is.null(igraph::edge_attr(graph, attr))) {
        rep(edge_attr_defaults[[attr]], igraph::ecount(graph))
      } else {
        igraph::edge_attr(graph, attr)
      }
    })
    names(edges) <- names(edge_attr_defaults)
    
    list(vertices = vertices, edges = edges)
  }
  
  #' Get Edge Data from igraph Object
  #' 
  #' This helper function retrieves data for edges from an igraph object, including coordinates and attributes.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A data frame containing edge data, including coordinates and attributes.
  #' @export
  #' @importFrom dplyr bind_cols
  get_edge_data <- function(graph, layout, spread = 0.1) {
    # Get attributes from graph
    attributes = extract_graph_attr(graph)
    edge_data <- as.data.frame(attributes$edges)
    
    # Format labels for edges
    edge_data$label <- paste0(
      "<br>", edge_data$eq,
      "<br>EC: ", edge_data$ec,
      "<br>rn: ", edge_data$rn,
      "<br>KO: ", edge_data$ko
    )
    
    # Adjust edge color for opacity (opacity not directly supported)
    edge_data$color <- hex_to_rgba(edge_data$color, alpha = edge_data$opacity)
    
    # Get  x, y, and z coordinates
    edge_coordinates <- generate_edge_coordinates(graph = graph, layout = layout, spread = spread)
    edge_data = cbind(edge_coordinates, edge_data) 
    
    return(edge_data)
  }
  
  #' Get Vertex Data from igraph Object
  #' 
  #' This helper function retrieves data for vertices from an igraph object, including coordinates and attributes.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @return A data frame containing vertex data, including coordinates and attributes.
  #' @export
  #' @importFrom dplyr bind_cols
  get_vertex_data <- function(graph, layout) {
    # Get attributes from graph
    attributes = extract_graph_attr(graph)
    vertex_data <- as.data.frame(attributes$vertices)
    
    # Get x, y, and z coordinates of vertices and edges
    vertex_coordinates <- if (ncol(layout) == 2) {
      data.frame(x = layout[, 1], y = layout[, 2])
    } else if (ncol(layout) == 3) {
      data.frame(x = layout[, 1], y = layout[, 2], z = layout[, 3])
    }
    vertex_data = cbind(vertex_coordinates, vertex_data)
    
    
    return(vertex_data)
  }
  
  #' Convert igraph Object to Plotly Plot
  #' 
  #' This function converts an igraph object to a Plotly plot, allowing customization of the layout, colors, and labels.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param showlabels A logical value indicating whether to display labels for the nodes.
  #' @return A plotly plot object displaying the network.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout add_segments add_text
  #' @importFrom dplyr distinct filter arrange
  igraph_to_plotly <- function(graph, layout, spread = 0.1, coord_fixed = FALSE, x_to_y_ratio = 1, showlegend = FALSE, showlabels = TRUE) {
    # Get attributes for plot
    if (!inherits(graph, "igraph")) {
      stop("Please provide a graph as an igraph object.")
    }
    
    vertex_data <- get_vertex_data(graph, layout)
    edge_data <- get_edge_data(graph, layout, spread = spread)
    
    # Set groups for plotting
    vertex_groups <- vertex_data |> dplyr::distinct(opacity)
    vertex_groups = vertex_groups |> dplyr::arrange(opacity)
    
    edge_groups <- edge_data |> dplyr::distinct(color, width, opacity)
    edge_groups = edge_groups |> dplyr::arrange(opacity)
    
    # Make plot
    plot <- plotly::plot_ly() 
    
    # Add edges   
    # Add by groups of attributes
    for (i in 1:nrow(edge_groups)) {
      filtered <- edge_data |>
        dplyr::filter(color == edge_groups$color[i], width == edge_groups$width[i], opacity == edge_groups$opacity[i])
      
      trace_name <- paste0("edge_", i)
      
      # 2D plots
      if (ncol(layout) == 2) {
        x_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$x1[j], filtered$xc[j], filtered$x2[j], NA)))
        y_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$y1[j], filtered$yc[j], filtered$y2[j], NA)))
        
        plot <- plot |>
          plotly::add_trace(
            x = x_coords,
            y = y_coords,
            type = "scatter",
            mode = "lines",
            line = list(shape = "spline", color = edge_groups$color[i], width = edge_groups$width[i]),
            name =  trace_name,
            text = rep(filtered$label, each = 4),
            hoverinfo = "text"
          )
      } else if (ncol(layout) == 3) {
        # 3D plots
        x_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$x1[j], filtered$xc[j], filtered$x2[j], NA)))
        y_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$y1[j], filtered$yc[j], filtered$y2[j], NA)))
        z_coords <- unlist(lapply(1:nrow(filtered), function(j) c(filtered$z1[j], filtered$zc[j], filtered$z2[j], NA)))
        
        plot <- plot |>
          plotly::add_trace(
            x = x_coords,
            y = y_coords,
            z = z_coords,
            type = "scatter3d",
            mode = "lines",
            line = list(shape = "spline", color = edge_groups$color[i], width = edge_groups$width[i]),
            name =  trace_name,
            text = rep(filtered$label, each = 4),
            hoverinfo = "text"
          )
      }
    }
    
    # Add vertices
    # Add by groups of attributes
    for (i in 1:nrow(vertex_groups)) {
      filtered <- vertex_data |>
        dplyr::filter(opacity == vertex_groups$opacity[i])
      
      trace_name <- paste0("vertex_", i)
      
      # 2D plots
      if (ncol(layout) == 2) {
        plot <- plot |>
          plotly::add_trace(x = filtered$x,
                            y = filtered$y,
                            type = "scatter", 
                            mode = "markers", 
                            marker = list(symbol = "circle", 
                                          size = filtered$size,
                                          color = filtered$color, 
                                          line = list(color = filtered$frame.color, width = filtered$frame.width),
                                          opacity = vertex_groups$opacity[i]
                            ),
                            name =  trace_name,
                            text = filtered$name, hoverinfo = "text") 
        if (showlabels){
          plot <- plot |>
            plotly::add_text(
              x = filtered$x,
              y = filtered$y,
              z = filtered$z,
              text = filtered$label,
              textposition = "top center",
              showlegend = FALSE
            )
        }
      } else if (ncol(layout) == 3) {
        # 3D plots
        plot <- plot |>
          plotly::add_trace(x = filtered$x,
                            y = filtered$y,
                            z = filtered$z,
                            type = "scatter3d", mode = "markers", 
                            marker = list(symbol = "circle", 
                                          size = filtered$size, 
                                          color = filtered$color, 
                                          line = list(color =  filtered$frame.color, width =  filtered$frame.width),
                                          opacity = vertex_groups$opacity[i]
                            ),
                            name =  trace_name,
                            text = filtered$name, hoverinfo = "text") 
        if (showlabels){
          plot <- plot |>
            plotly::add_text(
              x = filtered$x,
              y = filtered$y,
              z = filtered$z,
              text = filtered$label,
              textposition = "top center",
              showlegend = FALSE
            )
        }
      }
    }
    
    # Apply layout
    if (ncol(layout) == 2) {
      axis_settings <- generate_axis_settings(
        ticklen.x = 0,
        ticklen.y = 0,
        showticklabels.x = FALSE,
        showticklabels.y = FALSE,
        title.x = "",
        title.y = "",
        coord_fixed = coord_fixed
      )
      
      plot <- plot |>
        plotly::layout(
          showlegend = showlegend,
          xaxis = axis_settings$xaxis,
          yaxis = axis_settings$yaxis,
          margin = list(t = 100),
          hovermode = "closest"
        )
    } else if (ncol(layout) == 3) {
      axis_settings <- generate_axis_settings(
        include_z = TRUE,
        showgrid.x = TRUE,
        showgrid.y = TRUE,
        showgrid.z = TRUE,
        showline.x = TRUE,
        showline.y = TRUE,
        showline.z = TRUE,
        showticklabels.x = FALSE,
        showticklabels.y = FALSE,
        showticklabels.z = FALSE,
        coord_fixed = coord_fixed
      )
      
      plot <- plot |>
        plotly::layout(
          scene = list(
            showlegend = showlegend,
            xaxis = axis_settings$xaxis,
            yaxis = axis_settings$yaxis,
            zaxis = axis_settings$zaxis
          ),
          margin = list(t = 100),
          hovermode = "closest"
        )
    }
    
    return(plot)
  }
  
  #' Generate Control Points for Bezier Curves
  #' 
  #' This helper function generates control points for Bezier curves in 2D or 3D space, used for plotting curved edges in a network plot.
  #' 
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param v1 The index of the first node in the edge.
  #' @param v2 The index of the second node in the edge.
  #' @param n The total number of edges between the two nodes.
  #' @param index The index of the current edge among the n edges.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A vector representing the coordinates of the control point.
  #' @export
  control_points <- function(layout, v1, v2, n, index, spread = 0.1) {
    midpoint <- (layout[v1, ] + layout[v2, ]) / 2
    
    if (ncol(layout) == 2) {
      angle <- atan2(layout[v2, 2] - layout[v1, 2], layout[v2, 1] - layout[v1, 1]) + pi / 2
      control_dist <- spread * sqrt(sum((layout[v1, ] - layout[v2, ])^2)) * (index - (n / 2))
      control_point <- midpoint + control_dist * c(cos(angle), sin(angle))
      return(control_point)
    } else if (ncol(layout) == 3) {
      vector <- layout[v2, ] - layout[v1, ]
      
      perp_vector1 <- if (vector[3] == 0) {
        c(-vector[2], vector[1], 0)
      } else {
        c(-vector[2], vector[1], 0)
      }
      perp_vector1 <- perp_vector1 / sqrt(sum(perp_vector1^2))
      
      angle <- 2 * pi / n * (index - 1)
      control_point <- midpoint + spread * sqrt(sum((layout[v1, ] - layout[v2, ])^2)) * (cos(angle) * perp_vector1)
      
      return(control_point)
    }
  }
  
  #' Generate Edge Coordinates
  #' 
  #' This helper function generates coordinates for edges in a network plot, including control points for Bezier curves in 2D or 3D space.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @return A data frame containing coordinates for edges, including control points for Bezier curves.
  #' @export
  #' @importFrom dplyr bind_cols select filter
  #' @importFrom igraph as_data_frame V
  generate_edge_coordinates <- function(graph, layout, spread = 0.1) {
    df <- igraph::as_data_frame(graph)
    
    edges_coords <- lapply(1:nrow(df), function(i) {
      layout_df <- as.data.frame(layout)
      names(layout_df) <- if (ncol(layout) == 2) c("x", "y") else c("x", "y", "z")
      layout_df$id <- igraph::V(graph)$name
      layout <- as.matrix(layout_df[, -ncol(layout_df)])
      rownames(layout) <- layout_df$id
      
      edges <- df |> dplyr::select(to, from)
      
      v1 <- as.character(edges[i, "from"])
      v2 <- as.character(edges[i, "to"])
      edge_pair <- which((edges[, "from"] == v1 & edges[, "to"] == v2) | (edges[, "to"] == v1 & edges[, "from"] == v2))
      n <- length(edge_pair)
      index <- which(edge_pair == i)
      
      control_pt <- control_points(layout, v1, v2, n, index, spread)
      
      if (ncol(layout) == 2) {
        return(data.frame(
          x1 = layout[v1, "x"],
          xc = control_pt[1],
          x2 = layout[v2, "x"],
          y1 = layout[v1, "y"],
          yc = control_pt[2],
          y2 = layout[v2, "y"]
        ))
      } else if (ncol(layout) == 3) {
        return(data.frame(
          x1 = layout[v1, "x"],
          xc = control_pt[1],
          x2 = layout[v2, "x"],
          y1 = layout[v1, "y"],
          yc = control_pt[2],
          y2 = layout[v2, "y"],
          z1 = layout[v1, "z"],
          zc = control_pt[3],
          z2 = layout[v2, "z"]
        ))
      }
    })
    
    do.call(rbind, edges_coords)
  }
  
  #' Get Trace Attributes from Plotly Object
  #' 
  #' This helper function retrieves attributes (e.g., line color and width) from the traces in a Plotly plot.
  #' 
  #' @param plot A plotly plot object from which to extract trace attributes.
  #' @return A data frame containing the names, line colors, and line widths of the traces.
  #' @export
  #' @importFrom plotly plotly_build
  get_trace_attributes <- function(plot) {
    # Ensure the plotly object is properly built
    if (is.null(plot$x$data)) {
      plot <- plotly::plotly_build(plot)
    }
    
    # Initialize lists to store attributes
    trace_names <- c()
    trace_colors <- c()
    trace_widths <- c()
    
    # Iterate over the traces in the plot and extract attributes
    for (trace in plot$x$data) {
      # Handle missing name
      trace_names <- c(trace_names, if (!is.null(trace$name)) trace$name else NA)
      
      # Handle missing line attributes
      if (!is.null(trace$line)) {
        trace_colors <- c(trace_colors, if (!is.null(trace$line$color)) trace$line$color else NA)
        trace_widths <- c(trace_widths, if (!is.null(trace$line$width)) trace$line$width else NA)
      } else {
        trace_colors <- c(trace_colors, NA)
        trace_widths <- c(trace_widths, NA)
      }
    }
    
    # Combine the extracted attributes into a dataframe
    attributes_df <- data.frame(
      name = trace_names,
      line_color = trace_colors,
      line_width = trace_widths,
      stringsAsFactors = FALSE
    )
    
    return(attributes_df)
  }
  
  #' Find Matching Trace
  #' 
  #' This helper function finds a trace in a Plotly plot that matches a given color and line width.
  #' 
  #' @param color_hex A hexadecimal color code to match.
  #' @param line_width The line width to match.
  #' @param trace_attributes A data frame of trace attributes obtained from a Plotly plot.
  #' @return The name of the matching trace, or NA if no match is found.
  #' @export
  #' @importFrom dplyr filter
  get_matching_trace <- function(color_hex, line_width, trace_attributes) {
    # Convert the hex color to rgba
    query_color <- hex_to_rgba(color_hex)
    query_width = line_width
    
    # Find the matching trace based on line color and width
    match <- trace_attributes |>
      dplyr::filter(
        line_color == query_color &
          line_width == query_width
      )
    
    # Return the matching trace name
    if (nrow(match) == 1) {
      return(match$name)
    } else {
      return(NA)
    }
  }
  
  #' Generate Network Legend Labels
  #' 
  #' This helper function generates labels for the network legend based on the provided legend key and trace attributes.
  #' 
  #' @param network_legend_key A key used to generate labels for the network legend.
  #' @param trace_attributes A data frame of trace attributes obtained from a Plotly plot.
  #' @return A named vector of legend labels.
  #' @export
  generate_network_legend_labels <- function(network_legend_key, trace_attributes) {
    legend_labels <- c()
    
    for (i in seq_len(nrow(network_legend_key))) {
      matching_trace <- get_matching_trace(network_legend_key$line_color[i], network_legend_key$line_width[i], trace_attributes)
      
      # If a match is found, add it to the legend_labels vector
      if (!is.na(matching_trace)) {
        legend_labels[network_legend_key$name[i]] <- matching_trace
      }
    }
    
    return(legend_labels)
  }
  
  #' Reorder Network Legend
  #' 
  #' This function reorders the legend items in a Plotly network plot based on the provided labels.
  #' 
  #' @param plot A plotly plot object representing the network.
  #' @param legend_labels A named vector of labels used to reorder the legend items.
  #' @return A plotly plot object with the reordered legend.
  #' @export
  #' @importFrom plotly plotly_build add_trace layout
  reorder_network_legend <- function(plot, legend_labels) {
    # Check if the plotly object is properly built
    if (is.null(plot$x$data)) {
      plot <- plotly::plotly_build(plot)
    }
    
    # Extract the trace names from the plotly object
    trace_names <- sapply(plot$x$data, function(trace) trace$name)
    
    # Initialize a list to store reordered traces
    reordered_traces <- list()
    
    # Track indices of reordered traces
    reordered_indices <- integer(0)
    
    # Reorder and rename traces according to legend_labels
    for (i in seq_along(legend_labels)) {
      trace_index <- which(trace_names == legend_labels[i])
      if (length(trace_index) > 0) {
        # Rename the trace
        plot$x$data[[trace_index]]$name <- names(legend_labels)[i]
        # Add the trace to reordered list
        reordered_traces[[length(reordered_traces) + 1]] <- plot$x$data[[trace_index]]
        # Record the index as reordered
        reordered_indices <- c(reordered_indices, trace_index)
      }
    }
    
    # Add back any traces not included in the legend_labels, preserving their original order
    remaining_indices <- setdiff(seq_along(plot$x$data), reordered_indices)
    for (j in remaining_indices) {
      plot$x$data[[j]]$showlegend <- FALSE
      reordered_traces[[length(reordered_traces) + 1]] <- plot$x$data[[j]]
    }
    
    # Assign the reordered traces back to the plot
    plot$x$data <- reordered_traces
    
    return(plot)
  }
  
  #' Generate Annotations for Network Plot
  #' 
  #' This helper function generates annotations (e.g., labels and arrows) for nodes in a network plot.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param label_color The color of the node labels.
  #' @param arrow_color The color of the arrows used for edges.
  #' @param font_size The font size of the node labels.
  #' @param bgcolor_opacity The opacity of the background color for the node labels.
  #' @return A list of annotations for the Plotly layout.
  #' @export
  generate_annotations <- function(graph, layout, label_color = "red", arrow_color = "red", font_size = 10, bgcolor_opacity = 0.6) {
    # Extract graph attributes
    vertex_data <- get_vertex_data(graph, layout)
    
    annotations_list <- vector("list", nrow(vertex_data))
    
    for (i in 1:nrow(vertex_data)) {
      if (!is.na(vertex_data$label[i])){
        annotation <- list(
          x = vertex_data$x[i],
          y = vertex_data$y[i],
          text = vertex_data$label[i],
          font = list(color = label_color, size = font_size),
          bgcolor = paste0("rgba(255, 255, 255, ", bgcolor_opacity, ")"),
          showarrow = TRUE,
          arrowhead = 0,
          arrowcolor = arrow_color,
          xanchor = "center",
          yanchor = "bottom",
          ay = -30
        )
        
        if (ncol(layout) == 3) {
          annotation$z <- vertex_data$z[i]
        }
        
        annotations_list[[i]] <- annotation
      }
    }
    
    return(annotations_list)
  }
  
  #' Make Main Network Plot
  #' 
  #' This function generates a network plot based on the provided igraph object and layout, allowing customization of labels, colors, and layout.
  #' 
  #' @param graph An igraph object representing the network.
  #' @param layout A matrix or data frame specifying the layout coordinates for the nodes.
  #' @param spread The spread of the network, used to adjust the layout.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param network_legend_key A key used to generate labels for the network legend.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param showlabels A logical value indicating whether to display labels for the nodes.
  #' @param label_color The color of the node labels.
  #' @param arrow_color The color of the arrows used for edges.
  #' @param font_size The font size of the node labels.
  #' @param bgcolor_opacity The opacity of the background color for the node labels.
  #' @return A plotly plot object displaying the network.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  #' @importFrom igraph vcount ecount vertex_attr edge_attr as_data_frame
  plot_network <- function(graph, layout, 
                            spread = 0.05, coord_fixed = TRUE, 
                            network_legend_key = NULL,
                            showlegend = TRUE, showlabels = FALSE,
                            label_color = "red", arrow_color = "red", 
                            font_size = 10, bgcolor_opacity = 0.6) {
    # Check if input graph is empty and return a message if so
    if (igraph::vcount(graph) == 0 || igraph::ecount(graph) == 0) {
      return(plot_message(message = "No network built"))
    }
    
    # Create the plot using igraph_to_plotly
    plot <- igraph_to_plotly(
      graph = graph, 
      layout = layout, 
      spread = spread, 
      coord_fixed = coord_fixed, 
      showlegend = showlegend, 
      showlabels = showlabels
    )
    
    # Get trace attributes for legend reordering
    trace_attributes <- get_trace_attributes(plot)
    
    # Generate legend labels
    legend_labels <- generate_network_legend_labels(
      network_legend_key = network_legend_key, 
      trace_attributes = trace_attributes
    )
    
    # Reorder the network legend
    plot <- reorder_network_legend(plot, legend_labels)
    
    # Add annotations to the network plot
    annotations <- generate_annotations(
      graph = graph, 
      layout = layout, 
      label_color = label_color, 
      arrow_color = arrow_color, 
      font_size = font_size, 
      bgcolor_opacity = bgcolor_opacity
    )
    
    # Apply annotations
    if (ncol(layout) == 2) {
      plot <- plot |> plotly::layout(annotations = annotations)
    } else if (ncol(layout) == 3) {
      plot <- plot |> plotly::layout(scene = list(annotations = annotations))
    }
    
    return(plot)
  }

# --- Plot phylogenetic tree ---
  #' Get Nodes from Tips to Root
  #' 
  #' This function retrieves the nodes along the path from each tip to the root in a phylogenetic tree.
  #' 
  #' @param tree A phylogenetic tree object.
  #' @param layout A data frame specifying the layout coordinates for the nodes (optional).
  #' @return A data frame containing the parent-child node pairs for each tip in the tree.
  #' @export
  #' @importFrom ape nodepath getMRCA as.phylo
  get_nodes_to_root <- function(tree = NULL, layout = NULL) {
    # Check if tree or layout is provided
    if (!is.null(tree)) {
      tree <- tree
    } else if (is.null(tree) & !is.null(layout)) {
      tree <- ape::as.phylo(layout)
    } else {
      stop("Please provide either a tree or a layout")
    }
    
    # Find the root node
    root <- ape::getMRCA(tree, 1:length(tree$tip.label))
    
    # Get the tips from the tree
    tips <- 1:length(tree$tip.label)  # Tips are usually numbered 1 to N
    
    # Initialize an empty dataframe to store results
    result <- data.frame()
    
    # Loop over each tip and find the path to the root
    for (i in 1:length(tips)) {
      nodes_to_root <- ape::nodepath(phy = tree, from = tips[i], to = root)
      
      # Get the tip label and tip node
      tip_label <- tree$tip.label[i]
      tip_node <- nodes_to_root[1]
      
      # Loop over the nodes to create parent-child pairs
      for (j in 1:(length(nodes_to_root) - 1)) {
        parent_child <- data.frame(
          tip_label = tip_label,
          tip_node = tip_node,
          parent_node = nodes_to_root[j + 1],
          child_node = nodes_to_root[j]
        )
        result <- rbind(result, parent_child)
      }
    }
    
    return(result)
  }
  
  #' Get Layout of Phylogenetic Tree
  #' 
  #' This helper function retrieves the layout coordinates of a phylogenetic tree, using ggtree to generate the layout.
  #' 
  #' @param tree A phylogenetic tree object to be converted to a layout.
  #' @param layout_type The type of layout ("rectangular", "circular", etc.).
  #' @return A data frame containing the layout coordinates for the tree.
  #' @export
  #' @importFrom ggtree ggtree
  get_tree_layout <- function(tree, layout_type="rectangular") {
    p = ggtree::ggtree(tr = tree , layout = layout_type)
    layout = p$data
    
    return(layout)
  }
  
  #' Get Parameters for a Tree Layout
  #' 
  #' This function returns the drawing parameters that go with a named tree layout. A single lookup gives the layout name to build the layout with, the branch shape to draw it with, how the axes are scaled, and the plot options the layout starts with.
  #' 
  #' @param layout_type The name of the layout, as shown to the user.
  #' @return A list with the layout name to pass to ggtree, the branch shape to pass to get_branch_coordinates(), whether the axes share a scale, the x-to-y ratio to use when they do, and the starting settings for the x axis and the scale bar. The ratio is NULL when the axes are not fixed together. Only the rectangular tree measures x as distance from the root, so it is the only layout that starts with an axis, and the others start with a scale bar instead.
  #' @export
  get_layout_param <- function(layout_type) {
    list(
      ggtree_layout = switch(layout_type,
                             "Rectangular" = "rectangular",
                             "Slanted"     = "rectangular",
                             "Circular"    = "rectangular",
                             "Daylight"    = "daylight",
                             "Equal angle" = "equal_angle",
                             "Ape"         = "ape"
      ),
      shape = switch(layout_type,
                     "Rectangular" = "rectangular",
                     "Slanted"     = "straight",
                     "Circular"    = "circular",
                     "Daylight"    = "straight",
                     "Equal angle" = "straight",
                     "Ape"         = "straight"
      ),
      coord_fixed = switch(layout_type,
                           "Rectangular" = FALSE,
                           "Slanted"     = FALSE,
                           "Circular"    = TRUE,
                           "Daylight"    = TRUE,
                           "Equal angle" = TRUE,
                           "Ape"         = TRUE
      ),
      x_to_y_ratio = switch(layout_type,
                            "Rectangular" = NULL,
                            "Slanted"     = NULL,
                            "Circular"    = 1,
                            "Daylight"    = 0.8,
                            "Equal angle" = 0.8,
                            "Ape"         = 0.8
      ),
      axis_start = switch(layout_type,
                          "Rectangular" = "Backward",
                          "Slanted"     = "Backward",
                          "Circular"    = "Off",
                          "Daylight"    = "Off",
                          "Equal angle" = "Off",
                          "Ape"         = "Off"
      ),
      scale_bar_start = switch(layout_type,
                               "Rectangular" = FALSE,
                               "Slanted"     = FALSE,
                               "Circular"    = TRUE,
                               "Daylight"    = TRUE,
                               "Equal angle" = TRUE,
                               "Ape"         = TRUE
      )
    )
  }
  
  #' Project a Tree Layout into Drawing Coordinates
  #' 
  #' This function turns a rectangular layout into the coordinates a circular tree is drawn at. The position of a tip in the tip order (y) becomes an angle, and its distance from the root (x) becomes a radius, which is how ggtree draws a circular tree from the same rectangular layout. Sweeping less than a full turn gives a fan.
  #' 
  #' The projected coordinates are written back into x and y, so that everything downstream reads them as it always has, and the radius and angle are kept in r and theta for get_branch_coordinates() to draw arcs with. Because the radius is the original x, a scale bar measured on the projected x is still in branch-length units. Layouts of any other shape are returned untouched.
  #' 
  #' @param layout A data frame of layout coordinates, as returned by get_tree_layout().
  #' @param shape The branch shape the layout is being drawn with. Only "circular" is projected.
  #' @param angle_start The angle of the first tip, in degrees.
  #' @param angle_extent The angle swept by all the tips, in degrees. A full 360 gives a circle and anything less gives a fan.
  #' @param inward A logical value indicating whether to put the root on the outside and the tips in the middle.
  #' @return A data frame of layout coordinates, with x and y projected and with r and theta added.
  #' @export
  project_tree_layout <- function(layout, shape = "rectangular", angle_start = 0, angle_extent = 360, inward = FALSE) {
    # Check if the layout is empty or needs no projection and return it unchanged if so
    if (is.null(layout) || !is.data.frame(layout) || nrow(layout) == 0) {
      return(layout)
    }
    if (shape != "circular") {
      return(layout)
    }
    
    # Get the angle of each node from its place in the tip order, with a full turn needing one extra slot
    n_tips <- sum(layout$isTip, na.rm = TRUE)
    divisor <- if (angle_extent >= 359.9) n_tips else max(n_tips - 1, 1)
    layout$theta <- angle_start + ((layout$y - 1) / divisor) * angle_extent
    
    # Get the radius of each node from its distance from the root
    layout$r <- if (inward) (max(layout$x, na.rm = TRUE) - layout$x) else layout$x
    
    # Convert to the coordinates the tree is drawn at
    theta_radians <- layout$theta * pi / 180
    layout$x <- layout$r * cos(theta_radians)
    layout$y <- layout$r * sin(theta_radians)
    
    return(layout)
  }
  
  #' Prepare a Tree Layout for Plotting
  #' 
  #' This function turns a tree into the coordinates it is drawn at, in one step. It builds the layout the named shape needs and then projects it, which is what a circular tree needs doing between the two. A tree of NULL gives NULL, so a module with no tree uploaded can call it without checking first.
  #' 
  #' @param tree A phylo object, or NULL when no tree is available.
  #' @param layout_type The name of the layout, as shown to the user.
  #' @return A data frame of layout coordinates ready to pass to plot_tree(), or NULL.
  #' @export
  prepare_tree_layout <- function(tree, layout_type = "Rectangular") {
    if (is.null(tree)) {
      return(NULL)
    }
    
    param <- get_layout_param(layout_type)
    
    layout <- get_tree_layout(tree, layout_type = param$ggtree_layout)
    
    layout <- project_tree_layout(layout, shape = param$shape)
    
    return(layout)
  }
  
  #' Get Coordinates of Tree Branches
  #' 
  #' This function builds the drawing coordinates of every branch in a tree layout. Coordinates are returned as flat vectors with NA between branches, which is the form a plotly line trace expects. Alongside them are the layout rows the branch runs between, given for every point rather than for every branch, so that branches can be grouped by color with a single subset instead of a loop.
  #' 
  #' Branches are drawn from the child toward the parent, matching the order used before. The root is dropped, as is any branch whose parent is missing from the layout, so a filtered layout draws only the branches it still has both ends of.
  #' 
  #' @param layout A data frame of layout coordinates, as returned by get_tree_layout(). Must contain parent, node, x, and y. The circular shape also needs the r and theta columns added by project_tree_layout().
  #' @param shape The shape of the branches. "rectangular" draws elbows, "straight" draws direct lines and is used by the unrooted layouts, and "circular" draws an arc at the parent's radius followed by a radial line out to the child.
  #' @param split A logical value indicating whether to cut each branch at its midpoint, so the half by the parent and the half by the child can be drawn in different colors.
  #' @param arc_step The spacing of points along circular arcs, in degrees. Smaller values give smoother arcs and more points to draw.
  #' @return A list with x and y coordinates, and child, parent, and part vectors of the same length. child and parent give the layout row of the branch's child and of its parent, and part gives which half of the branch a point belongs to ("child" or "parent" when split is TRUE, "whole" when it is FALSE).
  #' @export
  get_branch_coordinates <- function(layout, shape = "rectangular", split = FALSE, arc_step = 2) {
    # Check if the layout is empty and return empty coordinates if so
    if (is.null(layout) || !is.data.frame(layout) || nrow(layout) == 0) {
      return(list(x = numeric(0), y = numeric(0), child = integer(0),
                  parent = integer(0), part = character(0)))
    }
    
    # Get branches to draw, dropping the root and any branch whose parent is missing
    parent_idx <- match(layout$parent, layout$node)
    keep <- which((layout$parent != layout$node) & !is.na(parent_idx))
    
    if (length(keep) == 0) {
      return(list(x = numeric(0), y = numeric(0), child = integer(0),
                  parent = integer(0), part = character(0)))
    }
    
    child_row <- keep
    parent_row <- parent_idx[keep]
    
    # Get positions of the two ends of each branch
    x_child <- layout$x[child_row]
    y_child <- layout$y[child_row]
    x_parent <- layout$x[parent_row]
    y_parent <- layout$y[parent_row]
    
    if (shape == "circular") {
      # Check that the layout has been projected and stop if it has not
      if (is.null(layout$r) || is.null(layout$theta)) {
        stop("A circular shape needs the r and theta columns added by project_tree_layout().")
      }
      
      r_child <- layout$r[child_row]
      theta_child <- layout$theta[child_row]
      r_parent <- layout$r[parent_row]
      theta_parent <- layout$theta[parent_row]
      
      # Sample each arc separately, since arcs differ in how many points they need
      arcs <- Map(function(t1, t2) {
        n_points <- max(2L, as.integer(ceiling(abs(t2 - t1) / arc_step)) + 1L)
        seq(t1, t2, length.out = n_points) * pi / 180
      }, theta_parent, theta_child)
      
      theta_child_radians <- theta_child * pi / 180
      
      if (split) {
        # Arc at the parent's radius, then a radial line in to the midpoint
        r_mid <- (r_parent + r_child) / 2
        
        parent_x <- lapply(seq_along(keep), function(i) {
          c(r_parent[i] * cos(arcs[[i]]), r_mid[i] * cos(theta_child_radians[i]), NA_real_)
        })
        parent_y <- lapply(seq_along(keep), function(i) {
          c(r_parent[i] * sin(arcs[[i]]), r_mid[i] * sin(theta_child_radians[i]), NA_real_)
        })
        
        # Radial line from the midpoint out to the child
        child_x <- as.vector(rbind(r_mid * cos(theta_child_radians),
                                   r_child * cos(theta_child_radians), NA_real_))
        child_y <- as.vector(rbind(r_mid * sin(theta_child_radians),
                                   r_child * sin(theta_child_radians), NA_real_))
        
        parent_lengths <- lengths(parent_x)
        
        x <- c(unlist(parent_x, use.names = FALSE), child_x)
        y <- c(unlist(parent_y, use.names = FALSE), child_y)
        child <- c(rep(child_row, times = parent_lengths), rep(child_row, each = 3))
        parent <- c(rep(parent_row, times = parent_lengths), rep(parent_row, each = 3))
        part <- c(rep("parent", sum(parent_lengths)), rep("child", length(child_x)))
      } else {
        # Arc at the parent's radius, then a radial line out to the child
        branch_x <- lapply(seq_along(keep), function(i) {
          c(r_parent[i] * cos(arcs[[i]]), r_child[i] * cos(theta_child_radians[i]), NA_real_)
        })
        branch_y <- lapply(seq_along(keep), function(i) {
          c(r_parent[i] * sin(arcs[[i]]), r_child[i] * sin(theta_child_radians[i]), NA_real_)
        })
        
        branch_lengths <- lengths(branch_x)
        
        x <- unlist(branch_x, use.names = FALSE)
        y <- unlist(branch_y, use.names = FALSE)
        child <- rep(child_row, times = branch_lengths)
        parent <- rep(parent_row, times = branch_lengths)
        part <- rep("whole", length(x))
      }
    } else if (shape == "rectangular") {
      if (split) {
        # Along the branch as far as its midpoint, then the corner and the parent's connector
        x_mid <- (x_child + x_parent) / 2
        
        child_x <- as.vector(rbind(x_child, x_mid, NA_real_))
        child_y <- as.vector(rbind(y_child, y_child, NA_real_))
        parent_x <- as.vector(rbind(x_mid, x_parent, x_parent, NA_real_))
        parent_y <- as.vector(rbind(y_child, y_child, y_parent, NA_real_))
        
        x <- c(child_x, parent_x)
        y <- c(child_y, parent_y)
        child <- c(rep(child_row, each = 3), rep(child_row, each = 4))
        parent <- c(rep(parent_row, each = 3), rep(parent_row, each = 4))
        part <- c(rep("child", length(child_x)), rep("parent", length(parent_x)))
      } else {
        # (child) -> (parent_x, child_y) -> (parent), with NA between branches
        x <- as.vector(rbind(x_child, x_parent, x_parent, NA_real_))
        y <- as.vector(rbind(y_child, y_child, y_parent, NA_real_))
        child <- rep(child_row, each = 4)
        parent <- rep(parent_row, each = 4)
        part <- rep("whole", length(x))
      }
    } else {
      if (split) {
        # Half of the direct line each
        x_mid <- (x_child + x_parent) / 2
        y_mid <- (y_child + y_parent) / 2
        
        child_x <- as.vector(rbind(x_child, x_mid, NA_real_))
        child_y <- as.vector(rbind(y_child, y_mid, NA_real_))
        parent_x <- as.vector(rbind(x_mid, x_parent, NA_real_))
        parent_y <- as.vector(rbind(y_mid, y_parent, NA_real_))
        
        x <- c(child_x, parent_x)
        y <- c(child_y, parent_y)
        child <- c(rep(child_row, each = 3), rep(child_row, each = 3))
        parent <- c(rep(parent_row, each = 3), rep(parent_row, each = 3))
        part <- c(rep("child", length(child_x)), rep("parent", length(parent_x)))
      } else {
        # (child) -> (parent), with NA between branches
        x <- as.vector(rbind(x_child, x_parent, NA_real_))
        y <- as.vector(rbind(y_child, y_parent, NA_real_))
        child <- rep(child_row, each = 3)
        parent <- rep(parent_row, each = 3)
        part <- rep("whole", length(x))
      }
    }
    
    return(list(x = x, y = y, child = child, parent = parent, part = part))
  }
  
  #' Add a Scale Bar to a Plot
  #' 
  #' This function adds a horizontal bar of a known length in branch-length units, placed below the tree. A length of NULL picks a round number near a fifth of the width of the tree. Shapes and annotations already on the plot are kept, so the bar can be added after plots have been overlaid.
  #' 
  #' The bar sits below the lowest tip, outside the space the tree itself takes up. Plotly sizes an axis from the traces on it and not from the shapes, and it then clips the shapes to that size, so a bar drawn below the tree is cut away while its label, which is an annotation and is never clipped, stays behind on its own. An invisible point is therefore drawn at the foot of the bar, which brings the bar into the range the axis is given and leaves it visible.
  #' 
  #' @param plot A plotly plot object holding the tree.
  #' @param layout A data frame of layout coordinates, used to size and place the bar.
  #' @param bar_length The length of the bar in branch-length units, or NULL to choose one.
  #' @param reverse_x A logical value indicating whether the x axis runs from right to left, in which case the bar is anchored at the right-hand end of the tree so that it is still drawn at the left of the picture.
  #' @param color The color of the bar and its label.
  #' @param linewidth The width of the bar.
  #' @param font_size The font size of the label.
  #' @return A plotly plot object with the scale bar added.
  #' @export
  #' @importFrom plotly plotly_build add_trace layout
  add_scale_bar <- function(plot, layout, bar_length = NULL, reverse_x = FALSE,
                            color = "black", linewidth = 2, font_size = 11) {
    # Check if the layout has no width and return the plot unchanged if so
    x_range <- range(layout$x, na.rm = TRUE)
    y_range <- range(layout$y, na.rm = TRUE)
    x_span <- diff(x_range)
    y_span <- diff(y_range)
    
    if (!is.finite(x_span) || x_span <= 0) {
      return(plot)
    }
    
    # Get length of the bar
    if (is.null(bar_length) || is.na(bar_length) || bar_length <= 0) {
      bar_length <- signif(x_span / 5, 1)
    }
    
    # Get position of the bar
    if (isTRUE(reverse_x)) {
      x_start <- x_range[2]
      x_end <- x_start - bar_length
    } else {
      x_start <- x_range[1]
      x_end <- x_start + bar_length
    }
    
    y_position <- y_range[1] - 0.02 * y_span
    tick_height <- 0.01 * y_span
    label_position <- y_position - 2.5 * tick_height
    
    # Draw the bar and its end ticks
    bar_shapes <- list(
      list(type = "line", x0 = x_start, x1 = x_end, y0 = y_position, y1 = y_position,
           line = list(color = color, width = linewidth)),
      list(type = "line", x0 = x_start, x1 = x_start,
           y0 = y_position - tick_height, y1 = y_position + tick_height,
           line = list(color = color, width = linewidth)),
      list(type = "line", x0 = x_end, x1 = x_end,
           y0 = y_position - tick_height, y1 = y_position + tick_height,
           line = list(color = color, width = linewidth))
    )
    
    # Label the bar with its length
    bar_annotations <- list(
      list(x = (x_start + x_end) / 2, y = label_position,
           text = format(bar_length, trim = TRUE),
           showarrow = FALSE, yanchor = "top",
           font = list(size = font_size, color = color))
    )
    
    # Reserve room for bar
    plot <- plot |>
      plotly::add_trace(
        x = c(x_start, x_end),
        y = rep(label_position - 2 * tick_height, 2),
        type = "scatter",
        mode = "markers",
        marker = list(size = 1, opacity = 0),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    
    # Keep any annotations already on the plot
    built <- plotly::plotly_build(plot)
    
    # Add the bar and its label
    plot <- add_shapes(plot, bar_shapes) |>
      plotly::layout(
        annotations = c(built$x$layout$annotations, bar_annotations)
      )
    
    return(plot)
  }
  
  #' Plot Tree Branches in Plotly
  #' 
  #' This function draws the branches of a tree as a plotly plot. Branches may all share one color, or take their color from the nodes they run between, which is how a tree is colored by a predicted trait. Branches sharing a color are drawn as a single trace, so the number of traces is the number of colors rather than the number of branches.
  #' 
  #' @param p A ggtree object to take the layout and styling from. Ignored when layout is given.
  #' @param layout A data frame of layout coordinates, as returned by get_tree_layout() and, for a circular tree, project_tree_layout().
  #' @param linewidth The width of the tree branches.
  #' @param color The color of the tree branches. A single color, or one color per row of layout when color_from is not "fixed".
  #' @param color_from Where a branch takes its color. "fixed" uses color as a single color, "child" and "parent" take the color of the node at that end of the branch, and "split" cuts the branch at its midpoint so each half takes the color of its own end.
  #' @param order A value per row of layout saying which branches are drawn last, or NULL to draw them in the order they appear. Branches are grouped into traces by color, so a trace is drawn in the position of the lowest value in it, and a trace holding nothing but NA is drawn first.
  #' @param alpha The transparency of the tree branches.
  #' @param type The type of tree layout ("rectangular", "circular", or an unrooted layout such as "daylight").
  #' @param webgl Whether to draw the branches with the WebGL renderer (scattergl), which stays fast on large trees. Set to FALSE to use SVG.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param arc_step The spacing of points along circular arcs, in degrees.
  #' @return A plotly plot object displaying the branches of the tree.
  #' @export
  #' @importFrom plotly plot_ly add_trace layout
  ggtree_to_plotly <- function(p = NULL, layout = NULL, linewidth = 1, color = "black", color_from = "fixed",
                               order = NULL, alpha = 1, type = "rectangular", webgl = TRUE,
                               coord_fixed = FALSE, x_to_y_ratio = 1, arc_step = 2) {
    # Get data from ggtree object
    # Get layout
    if (!is.null(layout)) {
      layout <- layout
    } else if (!is.null(p) && inherits(p, "ggtree")) {
      layout <- p$data
    } else if (is.data.frame(p)) {
      layout <- p
    } else {
      stop("Please provide either a ggtree object or a layout dataframe.")
    }
    
    # Extract style attributes from ggtree object
    linewidth <- p$theme$line$linewidth %||% linewidth
    color <- p$theme$line$colour %||% color
    alpha <- p$theme$line$alpha %||% alpha
    
    # Get the branch shape that goes with this layout
    shape <- switch(type,
                    "rectangular" = "rectangular",
                    "circular" = "circular",
                    "straight")
    
    # Get branch coordinates
    coordinates <- get_branch_coordinates(layout = layout, shape = shape,
                                          split = identical(color_from, "split"),
                                          arc_step = arc_step)
    
    # Get point colors and drawing order
    if (length(color) == 1 || identical(color_from, "fixed")) {
      point_color <- rep(color[1], length(coordinates$x))
      point_order <- rep(0, length(coordinates$x))
    } else {
      if (length(color) != nrow(layout)) {
        stop("color must be a single color or one color per row of layout.")
      }
      
      if (is.null(order)) {
        order <- seq_len(nrow(layout))
      } else if (length(order) != nrow(layout)) {
        stop("order must be NULL or one value per row of layout.")
      }
      
      # Take color and drawing order from the same branch end
      take_from_branch_end <- function(values) {
        switch(color_from,
               "child"  = values[coordinates$child],
               "parent" = values[coordinates$parent],
               "split"  = ifelse(coordinates$part == "parent",
                                 values[coordinates$parent],
                                 values[coordinates$child]),
               rep(values[1], length(coordinates$x))
        )
      }
      
      point_color <- take_from_branch_end(color)
      point_order <- take_from_branch_end(order)
    }
    
    # Order traces so higher values draw on top
    trace_colors <- unique(point_color)
    trace_order <- vapply(trace_colors, function(this_color) {
      values <- point_order[point_color == this_color]
      
      if (all(is.na(values))) -Inf else min(values, na.rm = TRUE)
    }, numeric(1))

    trace_colors <- trace_colors[base::order(trace_order)]
    
    # Make plot
    plot <- plotly::plot_ly()
    
    for (this_color in trace_colors) {
      keep <- which(point_color == this_color)
      
      plot <- plot |>
        plotly::add_trace(
          x = coordinates$x[keep], y = coordinates$y[keep],
          type = if (webgl) "scattergl" else "scatter",
          mode = "lines",
          line = list(color = this_color, width = linewidth),
          opacity = alpha,
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
    
    # Apply layout
    axis_settings <- generate_axis_settings(
      showticklabels.x = FALSE,
      showticklabels.y = FALSE,
      ticks.x = "",
      ticks.y = "",
      ticklen.x = 0,
      ticklen.y = 0,
      coord_fixed = coord_fixed,
      x_to_y_ratio = x_to_y_ratio
    )
    
    plot <- plot |>
      plotly::layout(
        title = FALSE,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    return(plot)
  }
  
  #' Plot Phylogenetic Tree in Plotly
  #' 
  #' This function draws a phylogenetic tree, with a point at every node and a branch between them. When a data frame of predictions is given, points are colored on the same scale the heatmap uses, and branches may be colored to match. Nodes with no matching prediction are drawn in the NA color.
  #' 
  #' @param layout A data frame of layout coordinates, as returned by get_tree_layout() and, for a circular tree, project_tree_layout().
  #' @param df A data frame of predictions, as returned by results_to_tree_data().
  #' @param point_data A data frame of points to draw, if not every node.
  #' @param fill Point fill color(s); a single color or one per row of layout. Ignored when df is given.
  #' @param color Point border color. Ignored when node_border_follows_branch is TRUE.
  #' @param size Point size.
  #' @param stroke Point border width.
  #' @param shape Point shape.
  #' @param label Column name(s) in layout to show in the point hover text. "match_key" and "z" are renamed to match_hover_label and z_hover_label.
  #' @param match_hover_label The name the matched identifier is hovered under, which is what the tip is called in the words of the module rather than in the words of the matching.
  #' @param context_hover A named list of constant fields shown after optional metadata and before the value.
  #' @param z_hover_label The name the value is hovered under, such as "Probability (%)" or "Flux".
  #' @param metadata A data frame of uploaded organism metadata, as returned by get_metadata_from_upload(), or NULL. Its identifier column is matched against tree tip labels the same way predictions are, and its remaining columns are added to the hover text, one line each in the form "Header: value". Metadata is independent of predictions, so it shows on a tree with none.
  #' @param metadata_id_col The metadata column matched against tip labels, or NULL to take it from the metadata's id_col attribute or, failing that, its first column.
  #' @param type Tree layout type passed to ggtree_to_plotly.
  #' @param linewidth Branch line width.
  #' @param branch_color The color of the branches when they are not colored by prediction.
  #' @param branch_color_mode Where a branch takes its color, passed to ggtree_to_plotly as color_from. "fixed" draws every branch in branch_color.
  #' @param color_bins The number of bins the color scale is divided into when branches are colored by prediction. Each bin costs one trace.
  #' @param node_border_follows_branch A logical value indicating whether each point takes the color of its own branch as its border, so that a node and its branch stay in step.
  #' @param lighten_fill How much to lighten the point fill, from 0 to 1, leaving the border at full strength. Lightening mixes the fill toward white rather than making it transparent, so points stay opaque.
  #' @param coord_fixed A logical value indicating whether the x and y axes share a scale.
  #' @param x_to_y_ratio The x-to-y scale ratio when coord_fixed is TRUE.
  #' @param webgl Whether to draw with the WebGL renderer. Hover on the points is unreliable under WebGL, so it is off by default.
  #' @param min_color The color of the lowest value of z.
  #' @param max_color The color of the highest value of z.
  #' @param zmin The value of z mapped to min_color.
  #' @param zmax The value of z mapped to max_color.
  #' @param na_color The color of nodes with no matching value of z.
  #' @param values_are_binary A logical value indicating whether to show each node as present or absent rather than by its number. Sub-threshold values have already been set to zero upstream, so a node counts as present when it sits above zmin. Present nodes take the max color and absent nodes the min color, and the hover shows a plus or a minus in place of the value.
  #' @param show_axis A logical value indicating whether to show the x axis with tick numbers. The axis is only meaningful for a rectangular tree, where x is the distance from the root. When it is off, the tick labels, the tick marks, the axis line, and the title are all turned off, so nothing of the axis is left.
  #' @param axis_title The title of the x axis.
  #' @param reverse_x A logical value indicating whether the x axis runs from right to left. The tree is mirrored, so the root sits on the right and the tips on the left, and the numbers count down across the picture.
  #' @param axis_from_tips A logical value indicating whether the tick numbers count from the tips rather than from the root, which is how a dated tree is usually read. The tree is not moved; only the numbers change.
  #' @param show_scale_bar A logical value indicating whether to draw a scale bar.
  #' @param scale_bar_length The length of the scale bar in branch-length units, or NULL to choose one.
  #' @param arc_step The spacing of points along circular arcs, in degrees.
  #' @param show_legend A logical value indicating whether to draw a color legend showing what the node colors mean. The legend is only drawn when a data frame of predictions is given, since there is nothing to key without one.
  #' @param legend_title The title of the color legend, or NULL to use z_hover_label.
  #' @return A plotly plot object with branches and points.
  #' @export
  #' @importFrom plotly plotly_build layout
  #' @importFrom colorspace lighten
  #' @importFrom utils modifyList
  plot_tree <- function(layout = NULL, df = NULL, point_data = NULL, fill = "black",
                        color = "#00000055", size = 5, stroke = 0.5,
                        shape = "circle", label = NULL,
                        match_hover_label = "match_key", context_hover = NULL,
                        z_hover_label = "z", metadata = NULL, metadata_id_col = NULL,
                        type = "rectangular",
                        linewidth = 1, branch_color = "black", branch_color_mode = "fixed",
                        color_bins = 32, node_border_follows_branch = FALSE, lighten_fill = 0,
                        coord_fixed = FALSE, x_to_y_ratio = 1,
                        webgl = FALSE, min_color = "black", max_color = "green",
                        zmin = 0, zmax = 100, na_color = na_grey, values_are_binary = FALSE,
                        show_axis = FALSE, axis_title = "Distance",
                        reverse_x = FALSE, axis_from_tips = FALSE,
                        show_scale_bar = FALSE, scale_bar_length = NULL, arc_step = 2,
                        show_legend = TRUE, legend_title = NULL) {
    # Check input
    if (is.null(layout) || !is.data.frame(layout) || nrow(layout) == 0) {
      return(plot_message(message = "No tree found"))
    }
    if (!is.null(df) && nrow(df) == 0) {
      return(plot_message(message = "No predictions"))
    }
    
    # Prepare node data and hover text
    context_names <- character(0)
    metadata_names <- character(0)
    
    if (!is.null(df)) {
      layout$match_key <- ifelse(layout$isTip, layout$label, as.character(layout$node))
      layout$z <- df$z[match(layout$match_key, df$x)]
      z_hover_value <- layout$z
      
      if (isTRUE(values_are_binary)) {
        z_hover_value <- binary_symbols(layout$z, zmin, na = NA)
        layout$z <- binarize_z(layout$z, zmin, zmax)
      }
      
      layout[[match_hover_label]] <- layout$match_key
      layout[[z_hover_label]] <- z_hover_value
      
      if (!is.null(context_hover) && length(context_hover) > 0) {
        context_names <- names(context_hover)
        context_names <- context_names[!is.na(context_names) & nzchar(context_names)]
        
        for (context_name in context_names) {
          layout[[context_name]] <- rep(as.character(context_hover[[context_name]][[1]]), nrow(layout))
        }
      }
      
      fill <- scale_z_to_color(
        layout$z,
        min_color = min_color,
        max_color = max_color,
        zmin = zmin,
        zmax = zmax,
        na_color = na_color
      )
      point_data <- layout
    }
    
    if (!is.null(metadata) && is.data.frame(metadata) && nrow(metadata) > 0) {
      if (is.null(layout$match_key)) {
        layout$match_key <- ifelse(layout$isTip, layout$label, as.character(layout$node))
      }
      
      reserved <- c(names(layout), match_hover_label, context_names, z_hover_label)
      hover_cols <- align_metadata_rows(
        organism_keys = layout$match_key,
        metadata = metadata,
        id_col = metadata_id_col,
        reserved = reserved
      )
      
      if (!is.null(hover_cols) && ncol(hover_cols) > 0) {
        metadata_names <- names(hover_cols)
        for (column in metadata_names) {
          layout[[column]] <- hover_cols[[column]]
        }
        layout[[match_hover_label]] <- layout$match_key
        point_data <- layout
      }
    }
    
    if (!is.null(df) || length(metadata_names) > 0) {
      requested_labels <- label
      if (!is.null(requested_labels)) {
        requested_labels[requested_labels == "match_key"] <- match_hover_label
        requested_labels[requested_labels == "z"] <- z_hover_label
      }
      
      label <- c(
        match_hover_label,
        metadata_names,
        context_names,
        setdiff(requested_labels, c(match_hover_label, metadata_names, context_names, z_hover_label)),
        if (!is.null(df)) z_hover_label
      )
      label <- unique(label[!is.na(label) & nzchar(label)])
    }
    
    # Set node and branch colors
    if (lighten_fill > 0) {
      fill <- colorspace::lighten(fill, amount = lighten_fill)
    }
    
    if (is.null(df)) {
      branch_color_mode <- "fixed"
    }
    
    branch_colors <- if (identical(branch_color_mode, "fixed")) {
      branch_color
    } else {
      bin_z_to_color(
        layout$z,
        n_bins = color_bins,
        min_color = min_color,
        max_color = max_color,
        zmin = zmin,
        zmax = zmax,
        na_color = na_color
      )
    }
    
    if (node_border_follows_branch) {
      color <- branch_colors
    }
    
    # Make plot
    branches <- ggtree_to_plotly(
      layout = layout,
      type = type,
      color = branch_colors,
      color_from = branch_color_mode,
      order = if (is.null(df)) NULL else layout$z,
      linewidth = linewidth,
      webgl = webgl,
      coord_fixed = coord_fixed,
      x_to_y_ratio = x_to_y_ratio,
      arc_step = arc_step
    )
    
    points_to_draw <- if (is.null(point_data)) layout else point_data
    if (!is.null(df)) {
      draw_order <- order(points_to_draw$z, na.last = FALSE)
      points_to_draw <- points_to_draw[draw_order, , drop = FALSE]
      fill <- fill[draw_order]
      if (length(color) > 1) color <- color[draw_order]
    }
    
    points <- plot_scatterplot(
      df = points_to_draw,
      label = label,
      color = color,
      fill = fill,
      stroke = stroke,
      size = size,
      shape = shape,
      webgl = webgl,
      coord_fixed = coord_fixed,
      x_to_y_ratio = x_to_y_ratio
    )
    
    plot <- overlay_plots(branches, points)
    
    # Add legend and scale bar
    if (!is.null(df) && isTRUE(show_legend)) {
      tree_legend_labels <- if (isTRUE(values_are_binary)) {
        c("-", "+")
      } else {
        format(round(seq(zmin, zmax, length.out = 5)), trim = TRUE, big.mark = ",")
      }
      
      plot <- add_categorical_legend(
        plot,
        legend_labels = tree_legend_labels,
        min_color = min_color,
        max_color = max_color,
        legend_title = legend_title %||% z_hover_label
      )
    }
    
    if (show_scale_bar) {
      plot <- add_scale_bar(
        plot,
        layout = layout,
        bar_length = scale_bar_length,
        reverse_x = reverse_x
      )
    }
    
    # Apply x-axis settings
    built <- plotly::plotly_build(plot)
    x_axis <- built$x$layout$xaxis %||% list()
    
    if (show_axis) {
      axis_settings <- list(
        showticklabels = TRUE,
        ticks = "outside",
        ticklen = 4,
        showline = TRUE,
        title = axis_title
      )
      
      if (axis_from_tips) {
        x_values <- range(layout$x, na.rm = TRUE)
        tip_distance <- x_values[2] - x_values[1]
        label_values <- pretty(c(0, tip_distance))
        label_values <- label_values[label_values >= 0 & label_values <= tip_distance]
        axis_settings$tickmode <- "array"
        axis_settings$tickvals <- x_values[2] - label_values
        axis_settings$ticktext <- format(label_values, trim = TRUE, big.mark = ",")
      }
      
      x_axis <- utils::modifyList(x_axis, axis_settings)
    } else {
      x_axis <- utils::modifyList(
        x_axis,
        list(
          showticklabels = FALSE,
          ticks = "",
          ticklen = 0,
          showline = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          title = FALSE
        )
      )
    }
    
    if (reverse_x) {
      x_axis$range <- NULL
      x_axis <- utils::modifyList(x_axis, list(autorange = "reversed"))
    }
    
    plot <- plot |> plotly::layout(xaxis = x_axis)
    return(plot)
  }
  

# --- Plot scatter plot ---
  #' Add Taxonomy to Graph Layout
  #' 
  #' This helper function adds taxonomy information (e.g., Phylum, Class, Order) to a graph layout based on matching IDs.
  #' 
  #' @param layout A data frame containing the layout coordinates for the nodes.
  #' @param layout_ID The column name in the layout data frame to match with the taxonomy data.
  #' @param taxonomy A data frame containing the taxonomy information.
  #' @param taxonomy_ID The column name in the taxonomy data frame to match with the layout data.
  #' @return A data frame containing the layout coordinates with added taxonomy information.
  #' @export
  #' @importFrom dplyr filter
  add_taxonomy_to_layout <- function(layout, layout_ID = "label", taxonomy, taxonomy_ID = "IMG_Genome_ID_max_quality") {
    row_match <- match(x = layout[[layout_ID]], table = taxonomy[[taxonomy_ID]])
    
    layout$Phylum <- taxonomy$Phylum[row_match]
    layout$Class <- taxonomy$Class[row_match]
    layout$Order <- taxonomy$Order[row_match]
    layout$Family <- taxonomy$Family[row_match]
    layout$Genus <- taxonomy$Genus[row_match]
    layout$Species <- taxonomy$Species[row_match]
    
    layout <- layout |> dplyr::filter(!!rlang::sym(layout_ID) %in% taxonomy[[taxonomy_ID]])
    return(layout)
  }    
  
  #' Make Main Scatter Plot
  #' 
  #' This function generates a scatter plot based on the provided data frame, allowing customization of colors, labels, and layout.
  #' 
  #' @param df A data frame containing the x and y coordinates of the points to be plotted.
  #' @param label A vector of labels for the points (optional).
  #' @param color The color of the point borders.
  #' @param fill The fill color of the points.
  #' @param stroke The width of the point borders.
  #' @param size The size of the points.
  #' @param shape The shape of the points ("circle", "square", etc.).
  #' @param alpha The transparency of the points.
  #' @param ticklen.x The length of the x-axis ticks.
  #' @param ticklen.y The length of the y-axis ticks.
  #' @param showticklabels.x A logical value indicating whether to show x-axis tick labels.
  #' @param showticklabels.y A logical value indicating whether to show y-axis tick labels.
  #' @param title.x The title of the x-axis.
  #' @param title.y The title of the y-axis.
  #' @param var_name The name of the variable to be displayed as the plot title.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @param x_to_y_ratio The ratio of the x-axis scale to the y-axis scale.
  #' @param showlegend A logical value indicating whether to display a legend for the plot.
  #' @param webgl Whether to draw the markers with the WebGL renderer (scattergl), which stays fast for many points. Set to FALSE to use SVG.
  #' @return A plotly plot object displaying the scatter plot.
  #' @export 
  #' @importFrom plotly plot_ly add_markers layout hide_colorbar
  #' @importFrom dplyr select
  plot_scatterplot <- function(df, 
                               label = NULL, color="black", fill="black", stroke=1, size=5, shape="circle", alpha=1, 
                               ticklen.x = 0, ticklen.y = 0, showticklabels.x = FALSE, showticklabels.y = FALSE, title.x = "", title.y = "",
                               var_name = NULL, coord_fixed = FALSE, x_to_y_ratio = 1, webgl = TRUE, showlegend = FALSE) {
    # Check if input data frame is empty and return a message if so
    if (nrow(df) == 0) {
      return(plot_message(message = "No matching organisms", 
                          bgcolor = "rgba(255, 255, 255, 0.25)", rect_color = "rgba(0, 0, 0, 0)",rect_fillcolor = "rgba(0, 0, 0, 0)"))
    }
    
    # Get hover text
    if (!is.null(label)) {
      hover_text <- format_row_hover(df |> dplyr::select(dplyr::all_of(label)))
    } else {
      hover_text <- paste0("x: ", df$x, "<br>y: ", df$y)
    }
    
    # Make plot
    plot <- plotly::plot_ly() |>
      plotly::add_trace(
        data = df,
        x = ~x,
        y = ~y,
        type = if (webgl) "scattergl" else "scatter",
        mode = "markers",
        marker = list(
          size = size,
          color = fill,
          symbol = shape,
          line = list(color = color, width = stroke),
          opacity = alpha
        ),
        text = hover_text,
        hoverinfo = "text"
      )
    
    # Apply layout
    axis_settings <- generate_axis_settings(
      coord_fixed = coord_fixed,
      ticklen.x = ticklen.x,
      ticklen.y = ticklen.y,
      showticklabels.x = showticklabels.x,
      showticklabels.y = showticklabels.y,
      title.x = title.x,
      title.y = title.y
    ) 
    
    plot <- plot |>
      plotly::layout(
        title = var_name,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    if (!showlegend) {
      plot <- plotly::hide_colorbar(plot)
    }
    
    return(plot)
  }

# --- Make other plots ---
  #' Plot Confusion Matrix
  #'
  #' This function generates a Plotly heatmap to visualize a confusion matrix, highlighting correct and incorrect classifications with different colors.
  #'
  #' @param df A data frame containing the confusion matrix to be plotted. The rows represent the actual labels, and the columns represent the predicted labels.
  #' @param coord_fixed A logical value indicating whether the x and y axes should have the same scale.
  #' @return A Plotly plot object displaying the confusion matrix.
  #' @export
  #' @importFrom plotly plot_ly layout
  #' @examples
  #' conf_matrix <- table(Predicted = c('A', 'B', 'A', 'A', 'B', 'B'), Actual = c('A', 'B', 'B', 'A', 'A', 'B'))
  #' plot_confusion_matrix(data.frame(table = conf_matrix))
  plot_confusion_matrix <- function(df, coord_fixed = TRUE) {
    # Check if input data frame is empty and return a message if so
    if (is.null(df$table)) {
      return(plot_message(message = "No model to evaluate"))
    }
    
    # Extract counts from confusion matrix
    conf_matrix <- as.matrix(df$table)
    x_labels <- colnames(conf_matrix)
    y_labels <- rownames(conf_matrix)
    
    # Create a matrix indicating correct (1) or incorrect (0) classifications
    correct <- matrix(0, nrow = nrow(conf_matrix), ncol = ncol(conf_matrix))
    for (i in 1:nrow(conf_matrix)) {
      for (j in 1:ncol(conf_matrix)) {
        if (rownames(conf_matrix)[i] == colnames(conf_matrix)[j]) {
          correct[i, j] <- 1 # Correct classification
        } else {
          correct[i, j] <- 0 # Incorrect classification
        }
      }
    }
    
    # Set fill colors for main plot
    fill_main <- to_plotly_colorscale(
      colorspace::lighten(red_color, amount = 0.6),
      colorspace::lighten(green_color, amount = 0.6)
    )
    
    # Set fill colors for background plot
    fill_background <- to_plotly_colorscale(
      colorspace::lighten(red_color, amount = 0.3),
      colorspace::lighten(green_color, amount = 0.3)
    )
    
    # Set color for text
    text_correct = colorspace::lighten(green_color, amount = -0.3)
    text_incorrect = colorspace::lighten(red_color, amount = -0.3)
    
    # Make plot
    plot <- plotly::plot_ly() |>
      # Add background (provides border)
      plotly::add_trace(
        type = "heatmap",
        x = x_labels,
        y = y_labels,
        z = correct,
        colorscale = fill_background,
        xgap = 5,
        ygap = 5,
        showscale = FALSE,
        text = conf_matrix,
        texttemplate = "%{text}",
        textfont = list(color = "black", size = 16)
      ) |>
      # Add main plot
      plotly::add_trace(
        type = "heatmap",
        x = x_labels,
        y = y_labels,
        z = correct,
        colorscale = fill_main,
        xgap = 8,
        ygap = 8,
        showscale = FALSE
      )
    
    # Add annotations for each cell with different colors
    annotations <- list()
    for (i in 1:nrow(conf_matrix)) {
      for (j in 1:ncol(conf_matrix)) {
        color <- ifelse(correct[i, j] == 1, text_correct, text_incorrect)
        annotations <- append(annotations, list(
          list(
            x = x_labels[j],
            y = y_labels[i],
            text = conf_matrix[i, j],
            showarrow = FALSE,
            font = list(
              color = color,
              size = 16
            )
          )
        ))
      }
    }
    
    # Apply layout
    axis_settings <- generate_axis_settings(
      coord_fixed = coord_fixed,   
      x_to_y_ratio = 1,
      showticklabels.x = TRUE,
      showticklabels.y = TRUE,
      ticklen.x = 4,
      ticklen.y = 4,
      title.x = "Predicted",
      title.y = "Actual",
      side.x = "top",
      autorange.y = "reversed",
      tickangle.y = -90,
      type = "category",   
      categoryorder = "array", 
      categoryarray.x = x_labels,
      categoryarray.y = y_labels
    )
    
    plot <- plot |>
      plotly::layout(
        annotations = annotations,
        xaxis = axis_settings$xaxis,
        yaxis = axis_settings$yaxis,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    return(plot)
  }

  #' Plot Metrics Table
  #'
  #' This function generates a Plotly table to display key evaluation metrics for 
  #' model performance, including accuracy, balanced accuracy, sensitivity, 
  #' specificity, precision, and F1 score.The values are color-coded using a 
  #' gradient, with 0 in red and 1 in green, to provide visual feedback on the model's performance.
  #'
  #' @param df A confusion matrix object containing overall and byClass metrics.
  #' @param header_color The background color to use for the table header.
  #' @param cell_color The background color to use for the table cells.
  #' @param low_color The color to use for low values in the gradient.
  #' @param high_color The color to use for high values in the gradient.
  #' @return A Plotly table object displaying the metrics.
  #' @export
  #' @importFrom plotly plot_ly
  #' @importFrom scales rescale
  #' @importFrom colorspace lighten
  plot_metrics_table <- function(df, header_color = "#E9F0FF", cell_color = "#FDFDFF", low_color = red_color, high_color = green_color) {
    # Return NULL if there are no metrics to display
    if (is.null(df$overall) || is.null(df$byClass)) {
      return(NULL)
    }
    
    # Extract the relevant metrics from df
    metrics <- data.frame(
      Metric = c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity", "Precision", "F1 Score"),
      Value = c(
        df$overall["Accuracy"],
        df$byClass["Balanced Accuracy"],
        df$byClass["Sensitivity"],
        df$byClass["Specificity"],
        df$byClass["Precision"],
        df$byClass["F1"]
      )
    )
    
    # Convert the metrics to a data frame for use in plotly
    metrics <- as.data.frame(metrics)
    
    # Round values to two decimal places and ensure proper formatting
    metrics$Value <- as.numeric(metrics$Value)
    metrics$Value <- round(metrics$Value, 2)
    
    # Create a color gradient from red to green based on the values
    colors <- generate_color_palette(101, low_color, high_color) # Gradient with 101 colors for values from 0 to 1
    value_colors <- colors[round(metrics$Value * 100) + 1] # Map values to colors
    
    # Format the values to two decimal places for display
    formatted_values <- format(metrics$Value, nsmall = 2)
    
    # Create the plotly table
    plot <- plotly::plot_ly(
      type = "table",
      header = list(
        values = c("<b>Metric</b>", "<b>Value</b>"),
        align = c("center", "center"),
        line = list(width = 1, color = "white"),
        fill = list(color = header_color),
        font = list(size = 12, color = "black")
      ),
      cells = list(
        values = rbind(metrics$Metric, formatted_values),
        align = c("center", "center"),
        line = list(color = "white", width = 1),
        fill = list(color = cell_color),
        font = list(size = 12, color = list(c("black"), value_colors))
      )
    )
    
    # Display the table
    return(plot)
  }
  
# --- Overlay plots ---
  #' Overlay Multiple Plotly Objects
  #' 
  #' This function overlays multiple Plotly plots, combining their traces and layout settings.
  #' 
  #' @param ... Multiple Plotly plot objects to be overlaid.
  #' @return A Plotly plot object combining the traces and layout settings of all input plots.
  #' @export
  #' @importFrom plotly plotly_build add_trace layout
  overlay_plots <- function(...) {
    plots <- list(...)
    
    if (length(plots) < 2) {
      stop("At least two plots are required for overlaying.")
    }
    
    # Start with the first plot
    combined_plot <- plots[[1]]
    
    # Build each plot and accumulate shapes and annotations
    all_shapes <- plotly::plotly_build(plots[[1]])$x$layout$shapes
    added_annotations <- list()
    
    for (i in 2:length(plots)) {
      built <- plotly::plotly_build(plots[[i]])
      
      # Add each trace from the next plot to the combined plot
      for (trace in built$x$data) {
        combined_plot <- combined_plot |> plotly::add_trace(
          x = trace$x,
          y = trace$y,
          mode = trace$mode %||% "markers",
          type = trace$type %||% "scatter",
          marker = trace$marker,
          line = trace$line,
          text = trace$text,
          hoverinfo = trace$hoverinfo,
          showlegend = FALSE
        )
      }
      
      # Accumulate layout shapes and annotations
      all_shapes <- c(all_shapes, built$x$layout$shapes)
      added_annotations <- c(added_annotations, built$x$layout$annotations)
    }
    
    # Apply the merged shapes and annotations
    if (length(added_annotations) == 0) added_annotations <- NULL
    combined_plot <- combined_plot |> plotly::layout(
      shapes = all_shapes,
      annotations = added_annotations
    )
    
    return(combined_plot)
  }
  
# --- Format results for plotting ---
  #' Format Prediction Results for Plots
  #'
  #' This function formats the results from app predictions into a format suitable 
  #' for different types of plots such as summary, heatmap, or treemap.
  #'
  #' @param df A data frame containing the machine learning prediction results.
  #' @param plot_type A character string specifying the type of plot ("summary", "heatmap", "treemap", or "tile").
  #' @param x_col A character string specifying the column name to use as the x-axis (default: "Organism number").
  #' @param y_col A character string specifying the column name to use as the y-axis (default: "Trait name").
  #' @param z_col A character string specifying the column name representing values (default: "Probability").
  #' @param var_col Optional. A character string specifying the variable column name.
  #' @param var_to_keep Optional. A value to filter the data frame by a specific value in `var_col`.
  #'   Leave it out to keep every group, which a heatmap reduces to the largest value of each cell.
  #' @param z_threshold A numeric value specifying the threshold below which `z` values will be set to 0 (default: 0).
  #' @param drop_extra_y A logical indicating whether to remove `y` groups where all `z` values are 0 (default: TRUE).
  #' @param z_percentage A logical indicating whether to convert `z` values to percentages (default: TRUE).
  #' 
  #' @return A formatted data frame ready for plotting.
  #' @export
  #' @importFrom dplyr rename mutate group_by filter ungroup arrange select summarize n
  #' @importFrom tidyr pivot_wider
  #' @importFrom rlang sym
  results_to_plot <- function(df, plot_type,
                              x_col = "Organism number", y_col = "Trait name", z_col = "Probability", 
                              var_col = NULL, var_to_keep = NULL, 
                              z_threshold = 0, drop_extra_y = TRUE, z_percentage = TRUE) {
    # Rename columns
    # Copy var first when it is the same column as x, y, or z, so that a single
    # column can serve as both the grouping variable and an axis.
    if (!is.null(var_col) && var_col %in% c(x_col, y_col, z_col)) {
      df$var <- df[[var_col]]
      df <- df |> dplyr::rename(
        x = !!rlang::sym(x_col), 
        y = !!rlang::sym(y_col), 
        z = !!rlang::sym(z_col)
      )
    } else if (!is.null(var_col)) {
      df <- df |> dplyr::rename(
        x = !!rlang::sym(x_col), 
        y = !!rlang::sym(y_col), 
        z = !!rlang::sym(z_col),
        var = !!rlang::sym(var_col)
      )
    } else {
      df <- df |> dplyr::rename(
        x = !!rlang::sym(x_col), 
        y = !!rlang::sym(y_col), 
        z = !!rlang::sym(z_col)
      )
    }
    
    # Remove extra columns
    df <- df |>
      dplyr::select(dplyr::any_of(c("var", "x", "y", "z")))
    
    # Get order of var
    if (!is.null(var_col)) {
      var_groups <- unique(df$var)
    }
    
    # Identify values of z below threshold and replace with 0
    if (!is.null(z_threshold)) {
      df <- df |> 
        dplyr::mutate(z = ifelse(z >= z_threshold, z, 0))
    }
    
    # Remove groups of y where all values of z are 0
    if (drop_extra_y) {
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::group_by(var, y) |>                     
          dplyr::filter(!(all(z == 0))) |>           
          dplyr::ungroup() 
      } else {
        df <- df |>
          dplyr::group_by(y) |>                     
          dplyr::filter(!(all(z == 0))) |>           
          dplyr::ungroup() 
      }
    }
    
    # Filter by var
    if (!is.null(var_col) && !is.null(var_to_keep)) {
      df <- df |>
        dplyr::filter(var == var_to_keep) 
    }
    
    # Order y alphabetically
    if (!is.null(var_col)) {
      df <- df |> 
        dplyr::mutate(var = factor(var, levels = var_groups)) |>
        dplyr::arrange(var, tolower(y)) |>
        dplyr::mutate(y = factor(y, levels = unique(y)))
    } else {
      df <- df |> 
        dplyr::arrange(tolower(y)) |>
        dplyr::mutate(y = factor(y, levels = unique(y)))
    }
    
    if (nrow(df)==0) {
      return(df)
    } else if (plot_type == "tile") {
      # Get z
      df <- df |>
        dplyr::mutate(
          z_raw = z,
          z = ifelse(z > 0, 100, 0)
        )

      # Get hover text
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::group_by(var, y) |>
          dplyr::summarize(
            x_text = paste(as.character(x[z > 0]), collapse = "; "),
            z_text = paste(as.character(z_raw[z > 0]), collapse = "; "),
            z = mean(z),
            .groups = "drop"
          )
      } else {
        df <- df |>
          dplyr::group_by(y) |>
          dplyr::summarize(
            x_text = paste(as.character(x[z > 0]), collapse = "; "),
            z_text = paste(as.character(z_raw[z > 0]), collapse = "; "),
            z = mean(z),
            .groups = "drop"
          )
      }

      # Sort by groups of var and z
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::mutate(var = factor(var, levels = var_groups)) |>
          dplyr::arrange(var, dplyr::desc(z), tolower(y))
      } else {
        df <- df |>
          dplyr::arrange(dplyr::desc(z), tolower(y))
      }
    } else if (plot_type == "summary") {
      # Remove var
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::select(-var)
      }
      
      # Further formatting
      df <- df |>
        dplyr::mutate(z = ifelse(z > 0, 100, 0))
      
      # Summarize
      df <- df |>
        dplyr::group_by(y) |>
        dplyr::summarize(z = mean(z), .groups = "drop") |>
        dplyr::ungroup()
      
      # Pivot wider
      df <- df |>
        tidyr::pivot_wider(names_from = y, values_from = z, values_fill = list(z = 0))
    } else if (plot_type == "heatmap") {
      # Remove var
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::select(-var)
      }
      
      # Convert to percentage
      if (z_percentage) {
        df$z <- df$z *100
      }
      
      # Factorize by x
      df$x <- factor(df$x, levels = unique(df$x))
      
      # Keep the largest value of each cell, which holds one value per group of
      # var when no group was filtered out
      if (!is.null(var_col) && is.null(var_to_keep)) {
        df <- df |>
          dplyr::group_by(x, y) |>
          dplyr::summarize(z = max(z), .groups = "drop")
      }
      
      # Pivot wider
      df <- df |>
        tidyr::pivot_wider(names_from = y, values_from = z)
      
    } else if (plot_type == "treemap") {
      # Remove var
      if (!is.null(var_col)) {
        df <- df |>
          dplyr::select(-var)
      }
      
      # Further formatting
      df <- df |>
        dplyr::mutate(z = ifelse(z>0, 100, 0))
      
      # Summarize
      df <- df |>
        dplyr::group_by(y) |>
        dplyr::summarize(z = mean(z), .groups = "drop") 
      
      # Normalize to percentage
      df <- df |>
        dplyr::mutate(z = (z / sum(z)) * 100)
    }
    
    return(df)
  }
  
# --- Other ---  
  #' Get Plotly Output Dimensions
  #'
  #' Retrieves the height and width of a Plotly plot from `session$clientData` in an RShiny session.
  #'
  #' @param session The Shiny session object. Defaults to the current reactive domain.
  #' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
  #' @param plot_id The ID of the plot output.
  #' @param default_height Default height if clientData is NULL (default: 500).
  #' @param default_width Default width if clientData is NULL (default: 500).
  #'
  #' @return A named list containing `height` and `width` of the plot.
  #'
  #' @examples
  #' get_plotly_dimensions(session, ns, "heatmap_plot")
  #' get_plotly_dimensions(session, ns, "treemap_plot", default_height = 600, default_width = 800)
  get_plotly_dimensions <- function(session = getDefaultReactiveDomain(), 
                                    ns, plot_id, default_height = 500, default_width = 500) {
    height_key <- paste0("output_", ns(plot_id), "_height")
    width_key <- paste0("output_", ns(plot_id), "_width")
    
    plot_height <- session$clientData[[height_key]] %||% default_height
    plot_width <- session$clientData[[width_key]] %||% default_width
    
    return(list(height = plot_height, width = plot_width))
  }
  
  #' Determine Whether to Use Fixed Coordinate Ratio in Heatmap
  #'
  #' Calculates whether to use `coord_fixed = TRUE` based on the height and width of cells 
  #' in a heatmap. Returns `TRUE` if cell height exceeds cell width, and `FALSE` otherwise.
  #'
  #' @param df A data frame to be plotted as a heatmap.
  #' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
  #' @param plot_id The ID of the Plotly plot.
  #'
  #' @return Logical value indicating whether to use `coord_fixed = TRUE`.
  #'
  #' @examples
  #' get_coord_heatmap(df, ns, "heatmap_plot")
  get_coord_heatmap <- function(df, ns, plot_id) {
    dims <- get_plotly_dimensions(ns = ns, plot_id = plot_id)
    
    cell_height <- dims$height / nrow(df)
    cell_width <- dims$width / ncol(df)
    
    return(cell_height > cell_width)
  }
  
  #' Calculate Heatmap Cell Border Widths
  #'
  #' Calculates the horizontal and vertical border widths for a heatmap, based on cell size.
  #' Each is rounded to the nearest preset value using `round_to_nearest()`.
  #'
  #' @param df A data frame to be plotted as a heatmap.
  #' @param ns A namespace function for module compatibility. Default is `identity` for non-modular use.
  #' @param plot_id The ID of the Plotly plot.
  #' @param presets A numeric vector of allowed border widths. Default is c(0, 0.01, 0.1, 0.5, 1, 2).
  #'
  #' @return A named list with `horizontal_border` and `vertical_border` values.
  #'
  #' @examples
  #' get_heatmap_border(df, ns, "heatmap_plot")
  get_heatmap_border <- function(df, ns, plot_id, presets = c(0, 0.01, 0.1, 0.5, 1, 2)) {
    dims <- get_plotly_dimensions(ns = ns, plot_id = plot_id)
    
    cell_height <- dims$height / nrow(df)
    cell_width <- dims$width / ncol(df)
    
    horizontal_border <- round_to_nearest(cell_width * 0.02, presets)
    vertical_border <- round_to_nearest(cell_height * 0.02, presets)
    
    return(list(horizontal_border = horizontal_border, vertical_border = vertical_border))
  }

#' Customized pretty ggplot theme
#' 
#' @param font Font family for ggplot text.
#' @param background_color Color for plot background.
#' @param strip_background_color Color for strip background (for
#'   \code{ggplot2::facet_grid()} and \code{ggplot2::facet_wrap()}).
#' @param grid_color Color of panel grid major axes or \code{NULL} if want no 
#'   major grid lines.
#' @param axis_line_width Width of x and y axes lines.
#' @param show_ticks Logical; whether or not to show axes tick marks.
#' @param x_text_angle Logical; whether or not to angle x text at 45 degrees.
#' @param size_theme One of "small", "normal", "large", "xlarge"; default sizes 
#'   for plot text and titles. If \code{NULL}, defaults to values specified by 
#'   \code{axis_title_size}, \code{axis_text_size}, \code{legend_title_size},
#'   \code{legend_text_size}, \code{strip_text_size}, \code{title_size}.
#' @param axis_title_size Font size of axis title. Ignored if \code{size_theme} 
#'   not \code{NULL}.
#' @param axis_text_size Font size of axis text. Ignored if \code{size_theme} 
#'   not \code{NULL}.
#' @param legend_title_size Font size of legend title. Ignored if 
#'   \code{size_theme} not \code{NULL}.
#' @param legend_text_size Font size of legend text/key. Ignored if 
#'   \code{size_theme} not \code{NULL}.
#' @param strip_text_size Font size of strip text. Ignored if \code{size_theme} 
#'   not \code{NULL}.
#' @param title_size Font size of plot title. Ignored if \code{size_theme}
#'   not \code{NULL}.
#' @param ... = other arguments to pass to \code{ggplot2::theme()}
#'
#' @return A \code{ggplot2::theme()} object.
#' 
#' @examples 
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width) +
#'   geom_point() +
#'   prettyGGplotTheme()
#'
#' @export
prettyGGplotTheme <- function(font = "Helvetica",
                              background_color = "grey98",
                              strip_background_color = "#2c3e50",
                              grid_color = "grey90",
                              axis_line_width = 1,
                              show_ticks = TRUE,
                              x_text_angle = FALSE,
                              size_theme = NULL,
                              axis_title_size = 10, axis_text_size = 7,
                              legend_title_size = 10, legend_text_size = 8,
                              strip_text_size = 9, title_size = 12, 
                              ...) {
  if (!is.null(size_theme)) {
    if (size_theme == "small") {
      axis_title_size <- 10
      axis_text_size <- 7
      legend_title_size <- 10
      legend_text_size <- 8
      strip_text_size <- 9
      title_size <- 12
    } else if (size_theme == "medium") {
      axis_title_size <- 14
      axis_text_size <- 10
      legend_title_size <- 14
      legend_text_size <- 10
      strip_text_size <- 12
      title_size <- 16
    } else if (stringr::str_detect(size_theme, "large")) {
      num_x <- stringr::str_count(size_theme, "x")
      axis_title_size <- 18 + num_x * 2
      axis_text_size <- 14 + num_x * 2
      legend_title_size <- 18 + num_x * 2
      legend_text_size <- 14 + num_x * 2
      strip_text_size <- 16 + num_x * 2
      title_size <- 20 + num_x * 2
    } else {
      stop("size_theme must be one of 'small', 'medium', 'large', 'xlarge', or NULL.")
    }
  }
  
  custom_theme <- ggplot2::theme(
    axis.title = ggplot2::element_text(family = font, 
                                       size = axis_title_size, 
                                       face = "bold"),
    axis.text = ggplot2::element_text(family = font, size = axis_text_size),
    axis.line = ggplot2::element_line(size = axis_line_width, color = "black"),
    axis.ticks = ggplot2::element_line(size = ifelse(show_ticks, 
                                                     ggplot2::rel(1), 0), 
                                       colour = "black"),
    axis.text.x = ggplot2::element_text(angle = ifelse(x_text_angle, 45, 0),
                                        hjust = ifelse(x_text_angle, 1, 0.5)),
    panel.grid.major = ggplot2::element_line(colour = grid_color, 
                                             size = ggplot2::rel(0.5)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = background_color),
    strip.background = ggplot2::element_rect(fill = strip_background_color,
                                             color = strip_background_color),
    strip.text = ggplot2::element_text(color = "white", face = "bold",
                                       size = strip_text_size),
    legend.key = ggplot2::element_rect(fill = "grey98"),
    legend.text = ggplot2::element_text(family = font, size = legend_text_size),
    legend.title = ggplot2::element_text(family = font, face = "bold",
                                         size = legend_title_size),
    plot.title = ggplot2::element_text(family = font, face = "bold",
                                       size = title_size),
    ...
  )
  
  return(custom_theme)
}

#' Customized pretty ggplot color and fill themes
#' 
#' @param color Vector used for color aesthetic. Should match 
#'   \code{ggplot2::aes(color = ...)} argument.
#' @param fill Vector used for fill aesthetic. Should match 
#'   \code{ggplot2::aes(fill = ...)} argument.
#' @param viridis Logical. Whether or not to use \code{viridis} scheme if using
#'   discrete color scheme.
#' @param option Argument indicating \code{viridis} palette name.
#' @param drop Logical; whether or not to drop factors with no observations.
#' @param ... Other arguments to pass to [ggplot2::scale_color_manual()],
#'   [ggplot2::scale_fill_manual()], [viridis::scale_colour_viridis()], or
#'   [viridis::scale_fill_viridis()].
#'
#' @return A ggplot color or fill theme object.
#' 
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
#'   geom_point() +
#'   prettyGGplotTheme() +
#'   prettyGGplotColor(color = iris$Species)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   prettyGGplotTheme() +
#'   prettyGGplotFill(fill = iris$Species)
#'   
#' @name prettyGGplotColorTheme
#' @rdname prettyGGplotColorTheme
#'   
NULL

#' @rdname prettyGGplotColorTheme
#' 
#' @export
prettyGGplotColor <- function(color, viridis = F, option = "plasma", 
                              drop = T, ...) {
  discrete <- is.factor(color)
  if (discrete) {
    if (nlevels(color) <= 8 & viridis == FALSE) {
      custom_palette <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_palette[2] <- custom_palette[1]
      custom_palette[1] <- "#FF9300"
      custom_color <- ggplot2::scale_color_manual(values = custom_palette, 
                                                  drop = drop, ...)
    } else {
      custom_color <- viridis::scale_colour_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    custom_color <- viridis::scale_colour_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  return(custom_color)
}


#' @rdname prettyGGplotColorTheme
#' 
#' @export
prettyGGplotFill <- function(fill, viridis = F, option = "plasma", 
                             drop = T, ...) {
  discrete <- is.factor(fill)
  if (discrete) {
    if (nlevels(fill) <= 8 & viridis == FALSE) {
      custom_palette <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
      custom_palette[2] <- custom_palette[1]
      custom_palette[1] <- "#FF9300"
      custom_fill <- ggplot2::scale_fill_manual(values = custom_palette, 
                                                drop = drop, ...)
    } else {
      custom_fill <- viridis::scale_fill_viridis(
        discrete = discrete, option = option,
        begin = 0, end = 0.95, drop = drop, ...
      )
    }
  } else {
    custom_fill <- viridis::scale_fill_viridis(
      discrete = discrete, option = option,
      begin = 0, end = 0.95, ...
    )
  }
  return(custom_fill)
}

#' Blank x-axis theme for ggplot objects.
#'
#' @return A ggplot theme object.
#' 
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_x_theme()
#'   
#' @export
blank_x_theme <- function() {
  ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(), 
                 axis.ticks.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank())
}

#' Blank y-axis theme for ggplot objects.
#'
#' @return A ggplot theme object.
#' 
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_y_theme()
#'   
#' @export
blank_y_theme <- function() {
  ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(), 
                 axis.ticks.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank())
}

#' Blank x- and y-axis theme for ggplot objects.
#'
#' @return A ggplot theme object.
#' 
#' @examples
#' require(ggplot2)
#' ggplot(iris) +
#'   aes(x = Sepal.Length, fill = Species) +
#'   geom_density() +
#'   blank_xy_theme()
#'   
#' @export
blank_xy_theme <- function() {
  ggplot2::theme(axis.line = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(), 
                 axis.ticks = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank())
}
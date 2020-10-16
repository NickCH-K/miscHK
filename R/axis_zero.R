#' ggplot2 theme-like object for forcing axes to sit at (0, 0)
#'
#' This is a ggplot2 theme designed for graphs in which axes are fixed at (0,0) or some other numbers. This is technically not a theme as it includes geometries as well. Use \code{theme()} to modify afterwards.
#'
#' Note that this breaks connection with ggplot things like axis titles and axis range, which will now require considerable fiddling.
#'
#' Only works if both x and y axes are numeric.
#'
#'                      digits = 0,
#'
#' @param x_axis The value of y at which the x-axis will be drawn.
#' @param y_axis The value of x at which the y-axis will be drawn.
#' @param x_min,x_max,y_min,y_max The edges of the graph on the x and y axes, for the purposes of figuring out how far to draw the axes and place the axis titles. Only relevant if positive/negative quadrants included, respectively.
#' @param title_distance,title_distance_x,title_distance_y The distance between the axis line and the title for that axis.
#' @param x_title_loc,y_title_loc The locations of the axis titles set explicitly, in terms of actual x and y values, overriding \code{title_distance}.
#' @param title_opts,title_opts_x,title_opts_y A list of options to be passed to \code{ggplot2::annotate} for the axis titles, for both axes or only x or y specifically.
#' @param digits,x_digits,y_digits Number of digits after the decimal place to include in axis labels.
#' @param axis_distance,axis_distance_x,axis_distance_y The distance between the axis line and the axis labels.
#' @param axis_opts,axis_opts_x,axis_opts_y A list of options to be passed to \code{ggplot2::annotate} for the axis labels, for both axes or only x or y specifically.
#' @param axis_format_x,axis_format_y The labeling function from the **scales** package to use to format the axis labels.
#' @param x_breaks,y_breaks The axis labels to include. Including the origin values in either of these is ill-advised, use \code{label_origin} instead.
#' @param label_origin Include an axis marker for the origin. Set to a character variable to specify the value.
#' @param axis_text_size The font size of the axis labels.
#' @param line_opts,line_opts_x,line_opts_y A list of options to be passed to \code{ggplot2::geom_segment} for the axis lines, for both axes or only x or y specifically.
#' @param base_theme The **ggplot2** theme on which to build the axis-zero graph. Note that if this is not \code{ggplot2::theme_void}, you will likely have to do a lot of adjustments in \code{ggplot2::theme()} afterwards to make it look good.
#' @param base_size Base font size
#' @param base_family Base font family
#' @param base_line_size Base size for line elements
#' @param base_rect_size size for rect elements
#'
#' @examples
#'
#' ggplot(iris,
#' aes(x = Sepal.Width, y = Sepal.Length)) +
#'   geom_point() +
#'   axis_zero(x_title = 'Sepal.Width',
#'             y_title = 'Sepal.Length',
#'             y_max = 8, x_max = 5,
#'             title_distance = .6, digits = 1)
#'
#' # A little messy - data overlaps axis labels. But a four-quadrant graph!
#'
#' ggplot(iris,
#' aes(x = Sepal.Width - mean(Sepal.Width), y = Sepal.Length - mean(Sepal.Length))) +
#'   geom_point() +
#'   axis_zero(x_title = 'Sepal.Width',
#'             y_title = 'Sepal.Length',
#'             y_max = 10, x_max = 3,, x_min = -3,
#'             quadrants = 1:4,
#'             title_distance_x = 1.2, title_distance_y = .5, digits = 1,
#'             axis_distance_x = .4, axis_opts = list(color = 'red'))
#'
#'
#' @export


axis_zero <- function(x_axis = 0, y_axis = 0,
                      quadrants = 1,
                      x_min = -10, y_min = -10,
                      x_max = 10, y_max = 10,
                      x_title = 'X', y_title = 'Y',
                      title_distance = 2,
                      title_distance_x = title_distance,
                      title_distance_y = title_distance,
                      x_title_loc = c(x_max, x_axis-title_distance_x),
                      y_title_loc = c(y_axis-title_distance_y,y_max),
                      title_opts = list(hjust = 1),
                      title_opts_x = title_opts,
                      title_opts_y = append(title_opts,list(angle = 90)),
                      digits = 0,
                      x_digits = digits,
                      y_digits = digits,
                      axis_distance = .2,
                      axis_distance_x = axis_distance,
                      axis_distance_y = axis_distance,
                      axis_opts = list(),
                      axis_opts_x = axis_opts,
                      axis_opts_y = axis_opts,
                      axis_format_x = function(x) scales::number(x, accuracy = 10^(-x_digits)),
                      axis_format_y = function(x) scales::number(x, accuracy = 10^(-y_digits)),
                      x_breaks = c(x_max*.9*.25*1:4, x_min*.9*.25*1:4) %>% round(digits = x_digits),
                      y_breaks = c(y_max*.9*.25*1:4, y_min*.9*.25*1:4) %>% round(digits = y_digits),
                      label_origin = TRUE,
                      axis_text_size = 12,
                      line_opts = list(),
                      line_opts_x = NULL,
                      line_opts_y = NULL,
                      base_theme = ggplot2::theme_void,
                      base_size = 18, base_family = "", base_line_size = base_size/10, base_rect_size = base_size/10) {

  # If specific x and y options weren't filled in, use the overall
  if (is.null(title_opts_x)) {
    title_opts_x <- title_opts
  }
  if (is.null(title_opts_y)) {
    title_opts_y <- title_opts
  }
  if (is.null(line_opts_x)) {
    line_opts_x <- line_opts
  }
  if (is.null(line_opts_y)) {
    line_opts_y <- line_opts
  }

  # Start with a void and the axis titles
  thm <- list(ggplot2::theme_void(base_size = base_size, base_family = base_family,
                         base_line_size = base_line_size, base_rect_size = base_rect_size),
              do.call(ggplot2::annotate,
                      append(list(geom='text',size=base_size/ggplot2::.pt,
                                  x=x_title_loc[1],y=x_title_loc[2],
                                  label=x_title),
                             title_opts_x)),
              do.call(ggplot2::annotate,
                      append(list(geom='text',size=base_size/ggplot2::.pt,
                                  x=y_title_loc[1],y=y_title_loc[2],
                                  label=y_title),
                             title_opts_y)))


  # Draw the y-axis.
  # If there's only a top half or a bottom half, draw to 0. Otherwise draw top to bottom
  if (!(any(c(3,4) %in% quadrants))) {
    y_bottom <- 0
    y_top <- y_max
    y_breaks <- y_breaks[y_breaks >= 0]
  } else if (!(any(c(1,2) %in% quadrants))) {
    y_bottom <- y_min
    y_top <- 0
    y_breaks <- y_breaks[y_breaks <= 0]
  } else {
    y_bottom <- y_min
    y_top <- y_max
  }
  # Same with the x-axis
  if (!(any(c(2,3) %in% quadrants))) {
    x_bottom <- 0
    x_top <- x_max
    x_breaks <- x_breaks[x_breaks >= 0]
  } else if (!(any(c(1,4) %in% quadrants))) {
    x_bottom <- x_min
    x_top <- 0
    x_breaks <- x_breaks[x_breaks <= 0]
  } else {
    x_bottom <- x_min
    x_top <- x_max
  }


  # Add the axis labels
  for (xb in x_breaks) {
    thm <- append(thm,
                  do.call(ggplot2::annotate,
                          append(list(geom='text',size=axis_text_size/ggplot2::.pt,
                                      x=xb,y=x_axis-axis_distance_x,
                                      label= axis_format_x(xb)),
                                 axis_opts_x)))
  }
  for (yb in y_breaks) {
    thm <- append(thm,
                  do.call(ggplot2::annotate,
                          append(list(geom='text',size=axis_text_size/ggplot2::.pt,
                                      x=y_axis-axis_distance_y,y=yb,
                                      label= axis_format_y(yb)),
                                 axis_opts_y)))
  }

  # Add the zero label
  if (is.logical(label_origin)) {
    if (label_origin) {
      label_origin <- paste0('(',x_axis,', ', y_axis, ')')
    }
  }
  if (is.character(label_origin)) {
    # Locate our zero
    if (identical(quadrants,1) | (sum(1:4 %in% quadrants) >= 3)) {
      z_y <- -axis_distance_x
      z_x <- -axis_distance_y
    }
    if (identical(quadrants,2)) {
      z_y <- -axis_distance_x
      z_x <- axis_distance_y
    }
    if (identical(quadrants,3)) {
      z_y <- axis_distance_x
      z_x <- axis_distance_y
    }
    if (identical(quadrants,4)) {
      z_y <- axis_distance_x
      z_x <- -axis_distance_y
    }
    if (sum(1:4 %in% quadrants == c(TRUE,TRUE,FALSE,FALSE)) == 4) {
      z_y <- -axis_distance_x
      z_x <- 0
    }
    if (sum(1:4 %in% quadrants == c(FALSE,FALSE, TRUE,TRUE)) == 4) {
      z_y <- axis_distance_x
      z_x <- 0
    }
    if (sum(1:4 %in% quadrants == c(TRUE, FALSE,FALSE, TRUE)) == 4) {
      z_y <- 0
      z_x <- -axis_distance_y
    }
    if (sum(1:4 %in% quadrants == c(FALSE, TRUE, TRUE, FALSE)) == 4) {
      z_y <- 0
      z_x <- axis_distance_y
    }

    # and draw it
    thm <- append(thm,
                  do.call(ggplot2::annotate,
                          append(list(geom='text',size=axis_text_size/ggplot2::.pt,
                                      x=x_axis + z_x,y= y_axis + z_y,
                                      label= label_origin),
                                 axis_opts_x)))

  }

  # Now add the axes
  thm <- append(thm,
                list(
                  do.call(ggplot2::geom_segment,
                          append(list(
                            mapping=ggplot2::aes(x=x_bottom,xend=x_top,y=y_axis,yend=y_axis),
                            size = 3*base_line_size/ggplot2::.stroke
                          ), line_opts_x)),
                  do.call(ggplot2::geom_segment,
                          append(list(
                            mapping=ggplot2::aes(x=x_axis,xend=x_axis,y=y_bottom,yend=y_top),
                            size = 3*base_line_size/ggplot2::.stroke
                          ), line_opts_y))
                ))


  return(thm)
}


#' Point labels for scatterplots
#' 
#' Draw text labels for all points of a scatterplot using directlabels package
#' 
#' @importFrom lattice trellis.par.get
#' @importFrom lattice panel.text
#' @importFrom lattice panel.segments
#' @importFrom lattice panel.rect
#' @importFrom directlabels smart.grid
#' @importFrom directlabels apply.method
#' @importFrom grid convertX
#' @importFrom grid convertY
#' 
#' @param x,y (numeric) vectors representing x and y coordinates of points
#' @param groups grouping variable passed down from xyplot (does not need to be specified)
#' @param subscripts subscripts passed down from xyplot (does not need to be specified)
#' @param labels (character) vector of labels to be plotted, if NULL, groups are used
#'   This resembles the behavior of the original \code{directlabels} functions
#' @param x_boundary (numeric) vector of length two, indicating the boundaries of
#'   x for which labels should be drawn (default: NULL) 
#' @param y_boundary (numeric) vector of length two, indicating the boundaries of
#'   y for which labels should be drawn (default: NULL)
#' @param col (charcater) color (vector) to be used for labels and lines
#' @param method (list) the positioning method, default is \code{directlabels::smart.grid}
#' @param draw_text (logical) whether to draw text labels or not (default: TRUE)
#' @param draw_line (logical) whether to draw a line to labels or not (default: TRUE)
#' @param draw_box (logical) whether to draw a box around labels or not (default: FALSE) 
#' @param box_fill (character) color of the box fill (default: light grey)
#' @param box_line (character) color of the box border (default: none)
#' @param ... other arguments passed to the function
#' 
#' @examples
#' library(grid)
#' library(lattice)
#' 
#' data("mtcars")
#' mtcars$car <- rownames(mtcars)
#' 
#' # A standard example using lattice grouping and paneling
#' xyplot(mpg ~ wt | factor(cyl), mtcars,
#'   groups = cyl, pch = 19, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1),
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.directlabel(x, y, draw_box = TRUE, ...)
#'   }
#' )
#' 
#' # A similar plot but without grouping. This requires explicit
#' # use of subscripts
#' xyplot(mpg ~ wt | factor(cyl), mtcars,
#'   pch = 19, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1),
#'   panel = function(x, y, subscripts, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.directlabel(x, y, draw_box = TRUE, subscripts = subscripts, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.directlabel <- function(
  x, y, groups = NULL, subscripts = NULL,
  labels = NULL, col = NULL, 
  x_boundary = NULL, y_boundary = NULL, 
  method = directlabels::smart.grid,
  draw_text = TRUE, draw_line = TRUE,
  draw_box = FALSE, box_fill = grey(0.95),
  box_line = NA, ...
) {
  
  # Filtering 
  # ----------
  # remove NAs if necessary
  valid <- !is.na(x) & !is.na(y)
  
  # remove values by user selection
  if (!is.null(x_boundary)) {
    if (length(x_boundary) == 2) {
      valid <- valid & x >= x_boundary[1] & x <= x_boundary[2]
    } else {
      stop("x_boundary or y_boundary must be length 2.")
    }
  }
  if (!is.null(y_boundary)) {
    if (length(y_boundary) == 2) {
      valid <- valid & y >= y_boundary[1] & y <= y_boundary[2]
    } else {
      stop("x_boundary or y_boundary must be length 2.")
    }
  }
  
  # apply filtering
  x <- x[valid]; y <- y[valid]
  if (!is.null(groups)) groups <- groups[valid]
  if (!is.null(subscripts)) subscripts <- subscripts[valid]
  
  
  # groups should be factor, otherwise coerce to it
  if (is.null(col)) {
    if (!is.null(groups)) {
      groups <- as.factor(groups)
      
      # determine graphical parameters from groups
      cols <- lattice::trellis.par.get()$superpose.symbol$col
      cols <- rep(cols, length.out = length(levels(groups)))
      col <- cols[as.numeric(groups)[subscripts]]  
      
    # default color if no groups is supplied
    } else {
      col <- lattice::trellis.par.get()$plot.symbol$col
    }
  }
  
  # if labels are not explicitly supplied, use group labels.
  # subscripts take care that correct labels per panel are selected
  if (is.null(labels)) {
    if (!is.null(groups)) {
      labels <- groups[subscripts]
    } else {
      stop("Neither labels nor groups supplied, no labels to draw.")
    }
  } else if (!is.null(subscripts)) {
    labels <- labels[subscripts]
  } else {
    stop("No subscripts argument supplied. Use 'panel = function(x, y, subscripts, ...)'")
  }
  
  # convert user coordinate units to cm (see apply.method manual)
  x_cm <- grid::convertX(unit(x, "native"), unitTo = "cm", valueOnly = TRUE)
  y_cm <- grid::convertY(unit(y, "native"), unitTo = "cm", valueOnly = TRUE)
  
  # apply label placing algorithm
  coords <- directlabels::apply.method(
    method, d = data.frame(x = x_cm, y = y_cm, groups = labels, index = 1:length(x)))
  # sort not by label number of characters, but by original order
  coords <- coords[order(coords$index), ]
  
  # convert back to native units
  coords[c("x", "w", "right", "left")] <-
    apply(coords[c("x", "w", "right", "left")], 2, function(x) {
      grid::convertX(unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  coords[c("y", "h", "top", "bottom")] <-
    apply(coords[c("y", "h", "top", "bottom")], 2, function(x) {
      grid::convertY(unit(x, "cm"), unitTo = "native", valueOnly = TRUE)
    })
  
  # draw label lines, boxes and text
  if (draw_line) {
    panel.segments(x, y, coords$x, coords$y, col = col, ...)
  }
  if (draw_box) {
    panel.rect(
      xleft = coords$left + coords$x - x, 
      ybottom = coords$bottom + coords$y - y, 
      xright = coords$right + coords$x - x, 
      ytop = coords$top + coords$y - y, 
      col = box_fill, border = box_line, ...)
  }
  if (draw_text) {
    panel.text(coords$x, coords$y, labels = coords$groups, col = col, cex = 0.8, ...)
  }
}

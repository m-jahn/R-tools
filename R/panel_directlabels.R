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
#' @param labels (character) vector of labels to be plotted, if NULL, groups are used.
#'   This resembles the behavior of the original \code{directlabels} functions.
#' @param method (list) the positioning method, default is \code{directlabels::smart.grid}
#' @param draw_text (logical) whether to draw text labels or not (default: TRUE)
#' @param draw_line (logical) whether to draw a line to labels or not (default: TRUE)
#' @param draw_box (logical) whether to draw a box around labels or not (default: FALSE) 
#' @param box_fill (character) color of the box fill (default: light grey)
#' @param box_line (character) color of the box border (default: none)
#' @param ... other arguments passed to the function
#' 
#' @examples
#' 
#' library(lattice)
#' 
#' mtcars$car <- rownames(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' 
#' 
#' xyplot(mpg ~ wt | cyl, mtcars, 
#'   pch = 19, groups = cyl, labels = mtcars$car,
#'   as.table = TRUE, layout = c(3, 1),
#'   panel = function(x, y, ...) {
#'     panel.xyplot(x, y, ...)
#'     panel.directlabel(x, y, draw_box = TRUE, ...)
#'   }
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
panel.directlabel <- function(
  x, y, groups, subscripts, labels = NULL,
  method = directlabels::smart.grid, 
  draw_text = TRUE, draw_line = TRUE,
  draw_box = FALSE, box_fill = grey(0.95),
  box_line = NA, ...
) {
  # if labels are not explicitly supplied, use group labels.
  # subscripts should take care that correct labels per panel are selected
  if (is.null(labels)) {
    labels = groups[subscripts]
  } else {
    labels = labels[subscripts]
  }
  
  # determine colors
  col = lattice::trellis.par.get()$superpose.symbol$col[groups[subscripts]]
  
  # convert user coordinate units to cm (see apply.method manual)
  x_cm <- grid::convertX(unit(x, "native"), unitTo = "cm", valueOnly = TRUE)
  y_cm <- grid::convertY(unit(y, "native"), unitTo = "cm", valueOnly = TRUE)
  
  # apply label placing algorithm
  coords <- directlabels::apply.method(
    method, d = data.frame(x = x_cm, y = y_cm, groups = labels))
  # sort not by label number of characters, but by original order
  coords <- coords[match(labels, as.character(coords$groups)), ]
  
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

#' Draw custom keys in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param labels (character) list of the labels to draw
#' @param which.panel (numeric) the panel(s) where key(s) should be drawn
#' @param points (logical) if points should be drawn
#' @param lines (logical) if lines should be drawn
#' @param rectangles (logical) if rectangles should be drawn
#' @param corner (numeric) vector of length 2 indicating the position of the key,
#'   in Normalised Parent Coordinates (0 to 1)
#' @param col,lwd,lty,pch,cex,point.cex graphical parameters to draw key labels
#'   and symbols
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
panel.key <- function (labels, which.panel = 1, pch = 1, cex = 0.8, 
  point.cex = NULL, points = TRUE, lines = FALSE, rectangles = FALSE,
  col = trellis.par.get()$superpose.polygon$col[1:length(labels)],
  lwd = trellis.par.get()$superpose.line$lwd[1],
  lty = trellis.par.get()$superpose.line$lty[1],
  corner = c(0.1, 0.9), ...)
{
  if (panel.number() %in% which.panel) {
    key <- simpleKey(labels, points = points, lines = lines, rectangles = rectangles,...)
    key$text$col <- col
    key$text$cex <- cex
    if (points == TRUE) {
      key$points$col <- col
      key$points$pch <- pch
      key$points$cex <- ifelse(!is.null(point.cex), point.cex, cex)
    }
    if (lines == TRUE) {
      key$lines$col <- col
      key$lines$lwd <- lwd
      key$lines$lty <- lty
    }
    key.gf <- draw.key(key, draw = FALSE)
    vp <- grid::viewport(
      x = unit(corner[1], "npc") + unit(0.5 - corner[1], "grobwidth", list(key.gf)), 
      y = unit(corner[2], "npc") + unit(0.5 - corner[2], "grobheight", list(key.gf))
    )
    grid::pushViewport(vp)
    grid::grid.draw(key.gf)
    grid::upViewport()
  }
}


#' Draw quadrants and quadrant statistics in lattice plots
#' 
#' Custom panel functions for lattice plots
#' 
#' @param x,y (numeric) variables to be plotted
#' @param h,v (numeric) position of the horizontal or vertical threshold for dividing the
#'   data into quadarants. Defaults to the median.
#' @param labels (character) One of 'percent', 'events', or 'none'. Controls what
#'   type of summary is plottted per quadrant
#' @param col,lwd graphical parameters for lines and labels
#' @param margin (numeric) margin of labels to the plot edges in Normalised Parent 
#'   Coordinates, default is 0.1
#' @param na.rm (logical) Should NA or Inf values be removed automatically? Default is FAlSE
#' @param ... other arguments passed to the function
#' @export
# ------------------------------------------------------------------------------
panel.quadrants <- function (x, y, h = NULL, v = NULL, 
  labels = "percent", col = grey(0.5), margin = 0.1,
  lwd = trellis.par.get()$superpose.polygon$lwd[1], 
  na.rm = FALSE, ...)
{ 
  
  # remove inf or NA values
  index = !(is.infinite(x*y) | is.na(x*y))
  if (!all(index)) {
    if (na.rm) {
      x = x[index]; y = y[index]
      cat(sum(!index), "NA / Inf values were removed\n")
    } else {
      cat(sum(!index), "NA / Inf values were not removed\n")
    }
  } 
  
  # drawing the horizontal and vertical lines
  if (is.null(h))
    h = median(y)
  if (is.null(v))
    v = median(x)
  panel.abline(h = h, v = v, lwd = lwd, col.line = col)
  
  # plot percentages of the 4 quadrants as text
  quadrant <- list(
    Q1 = sum(x <= v & y >  h),
    Q2 = sum(x >  v & y >  h),
    Q3 = sum(x >  v & y <= h),
    Q4 = sum(x <= v & y <= h)
  )
  
  # can either plot events or percentage, or no labels
  if (labels == "percent") {
    quadrant = sapply(quadrant, function(i) {
      paste0(round(i/length(x)*100, 1), "%")
    })
  } else if (labels == "events") {
    quadrant = paste0("n=", quadrant)
  } else if (labels == "none") {
    warning("no quadrant labels are plotted")
  } else {
    stop("labels must be one of 'percent', 'events', or 'none'")
  }
  
  # actual plotting of labels
  if (labels %in% c("percent", "events")) {
    with(current.panel.limits(), {
      xmargin = (xlim[2]-xlim[1])*margin
      ymargin = (ylim[2]-ylim[1])*margin
      panel.text(xlim[1]+xmargin, ylim[2]-ymargin, pos = 4, labels = quadrant[1], col = col)
      panel.text(xlim[2]-xmargin, ylim[2]-ymargin, pos = 2, labels = quadrant[2], col = col)
      panel.text(xlim[2]-xmargin, ylim[1]+ymargin, pos = 2, labels = quadrant[3], col = col)
      panel.text(xlim[1]+xmargin, ylim[1]+ymargin, pos = 4, labels = quadrant[4], col = col)
    })
  }
}

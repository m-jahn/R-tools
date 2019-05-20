# LOAD ADDITIONAL PACKAGES
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(grid)


# NEW PANEL FUNCTION FOR P-VALUE ANNOTATION
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# attention: y is measurement, x is conditional variable, std is the factor level
# that the other levels are compared to
panel.pvalue <- function(x, y, std, symbol = TRUE, cex.symbol = 1.5, offset = 1, 
  fixed.pos = NULL, verbose = TRUE, 
  col = trellis.par.get()$superpose.polygon$col[1], ...
  ) 
{ 
  pval <- tapply(y, x, function(z) t.test(z, y[x == std])$p.value)
  if (verbose) {
    cat("p-value for comparison of ", std, " with ", 
      as.character(unique(x)), " = ", pval, "\n")
  }
  
  if (symbol) {
    pval <- sapply(pval, function(x) {
      if (is.na(x)) "" else
      if (x <= 0.001) "***" else
      if (x <= 0.01 & x > 0.001) "**" else
      if (x <= 0.05 & x > 0.01) "*" else ""
    })
  } else {
    pval = paste0("p = ", round(pval, 5))
  }
  
  if (is.null(fixed.pos)) {
    ypos = tapply(y, x, function(x) median(x, na.rm = TRUE))
  } else if (is.numeric(fixed.pos)){
    ypos = fixed.pos
  } else {
    stop("fixed.pos must be NULL for automatic y positioning, or a numeric value.")
  }
  panel.text(1:length(pval), ypos, labels = pval, 
    cex = cex.symbol, pos = 3, offset = offset, col = col, ...)
}


# NEW PANEL FUNCTION FOR XYPLOT WITH ERROR BARS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.errbars <- function (x, y, ewidth = 0.08, 
  border = trellis.par.get()$superpose.polygon$col[1], ...)
{
  means <- tapply(y, x, function(x) mean(x, na.rm = TRUE))
  stdev <- tapply(y, x, function(x) sd(x, na.rm = TRUE))
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  ybottom <- current.panel.limits()$ylim[1]
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, col = border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, col = border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, col = border, ...)
  panel.xyplot(x, y, col = border, ...)
}


# NEW PANEL FUNCTION FOR BARPLOT WITH ERROR BARS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.barplot <- function (x, y, ewidth = 0.08, 
  border = trellis.par.get()$superpose.polygon$col[1], 
  fill = "white", ...)
{
  means <- tapply(y, x, function(x) mean(x, na.rm = TRUE))
  stdev <- tapply(y, x, function(x) sd(x, na.rm = TRUE))
  x <- unique(x)
  if (is.factor(x)) x <- sort(as.numeric(x))
  Y <- as.matrix(cbind(means, means-stdev, means+stdev))
  y <- Y[x, 1]
  y0 <- Y[x, 2]
  y1 <- Y[x, 3]
  offs <- ewidth/2
  ybottom <- current.panel.limits()$ylim[1]
  panel.segments(x0 = x, x1 = x, y0 = y0, y1 = y1, col = border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y0, y1 = y0, col = border, ...)
  panel.segments(x0 = x - offs, x1 = x + offs, y0 = y1, y1 = y1, col = border, ...)
  panel.rect(xleft=x - ewidth, ybottom = ybottom, xright = x + ewidth, ytop = y, 
    col = fill, border = border, ...)
}


# NEW PANEL FUNCTION DRAWING KEYS INSIDE PANELS
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.key <- function (labels, which.panel = 1, pch = 1, cex = 0.8,
  points = TRUE, lines = FALSE, rectangles = FALSE,
  col = trellis.par.get()$superpose.polygon$col[1:length(labels)],
  lwd = trellis.par.get()$superpose.line$lwd[1],
  lty = trellis.par.get()$superpose.line$lty[1],
  corner = c(0.1, 0.9), x = corner[1], y = corner[2], ...)
{
  if (panel.number() %in% which.panel) {
    key <- simpleKey(labels, points = points, lines = lines, rectangles = rectangles,...)
    key$text$col <- col
    key$text$cex <- cex
    if (points == TRUE) {
      key$points$col <- col
      key$points$pch <- pch
      key$points$cex <- cex
    }
    if (lines == TRUE) {
      key$lines$col <- col
      key$lines$lwd <- lwd
      key$lines$lty <- lty
    }
    key.gf <- draw.key(key, draw = FALSE)
    vp <- viewport(x = unit(x, "npc") + unit(0.5 - corner[1], 
      "grobwidth", list(key.gf)), y = unit(y, "npc") + unit(0.5 - 
      corner[2], "grobheight", list(key.gf)))
    pushViewport(vp)
    grid.draw(key.gf)
    upViewport()
  }
}


# NEW PANEL FUNCTION DRAWING QUADRANTS AND PERCENTAGES/QUADRANT
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
panel.quadrants <- function (x, y, h = NULL, v = NULL, 
  labels = "percent", col = grey(0.5), margin = 0.1,
  lwd = trellis.par.get()$superpose.polygon$lwd[1], ...)
{ 
  # remove inf or NA values
  index = !{is.infinite(x*y) | is.na(x*y)}
  x = x[index]; y = y[index]
  
  # drawing the horizontal and vertical lines
  if (is.null(h))
    h = median(y)
  if (is.null(v))
    v = median(x)
  panel.abline(h = h, v = v, lwd = lwd, col.line = col)
  
  
  # plot percentages of the 4 quadrants as text
  quadrant <- list(
    Q1 = {x < v & y > h} %>% sum,
    Q2 = {x > v & y > h} %>% sum,
    Q3 = {x > v & y < h} %>% sum,
    Q4 = {x < v & y < h} %>% sum
  )
  
  # can either plot events or percentage, or no labels
  if (labels == "percent") {
    quadrant = sapply(quadrant, function(i) {
      round(i/length(x)*100, 1) %>% paste0("%")
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

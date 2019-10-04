#' Custom scatterplot matrix (SPLOM)
#' 
#' Custom wrapper function around lattice splom with different upper and lower panel
#' 
#' @importFrom lattice splom
#' @importFrom stats cor
#' @importFrom grDevices colorRampPalette
#' 
#' @param df (data.frame) data frame whose columns are plotted against each other
#' @param pch (numeric) the plotting symbol to be used
#' @param col (character) the color to be used
#' @param cex (numeric) character size of the symbol
#' @param ... other arguments passed to the function
#' 
#' @export
# ------------------------------------------------------------------------------
custom_splom <- function(df, ...,
  pch = 19, col = grey(0.5), cex = 0.8
) {

  splom(~ df,
    pch = pch, col = col, cex = cex,
    lower.panel = panel.splom,
    upper.panel = function(x, y, ...) {
      common <- which(!is.na(x*y))
      x = x[common]; y = y[common]
      palette = grDevices::colorRampPalette(c("steelblue", grey(0.95), "darkorange"))(11)
      panel.fill(col = palette[1+abs(round(stats::cor(x, y)*10))])
      panel.lmline(x, y, fontfamily = "FreeSans")
      cpl <- current.panel.limits()
      panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y), 2), font=2)
    }
  )
  
}
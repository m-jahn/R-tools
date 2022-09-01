#' Cluster levels of a factor based on a response and a grouping variable
#' 
#' The function changes the order of levels of a factor by clustering levels
#' according to similarity of a second response variable, and an optional third grouping
#' variable.
#' 
#' @importFrom stats as.dendrogram
#' @importFrom stats order.dendrogram
#' @importFrom stats hclust
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr tibble
#' @importFrom tibble column_to_rownames
#' 
#' @param variable (vector)
#' @param group (character) 
#' @param value (vector)
#' @param method (character) The `hclust` method to use for clustering, see `help("hclust")`. Default is `"ward.D2"`.
#' 
#' @details More figuratively speaking, the function re-arranges data from response variable,
#' grouping variable, and factor levels to a matrix and orders the rows (factor levels)
#' of the matrix according to similarity.
#' The clustering is internally performed by `hclust()` and takes arguments for `method`.
#' The function is useful to reorder a factor in a data frame in order to plot it in form
#' of a clustered heatmap, see examples.
#' 
#' @return A reordered factor.
#' 
#' @examples
#' 
#' # set seed to obtain same values
#' set.seed(123)
#' 
#' # a data frame with 5 observations for 5 different groups (A to E)
#' df <- data.frame(
#'   fc = factor(rep(letters[1:5], 5)),
#'   group = rep(LETTERS[1:5], each = 5),
#'   response = rnorm(25)
#' )
#' 
#' # levels in alphabetical order
#' levels(df$fc)
#' 
#' # reorder levels of "fc" by clustering values in "response" over "groups"
#' df$fc <- with(df, fct_cluster(fc, group, response))
#' 
#' # levels ordered by similarity of responses
#' levels(df$fc)
#' 
#' @export
fct_cluster <- function(variable, group, value, method = "ward.D2") {
  df <- tidyr::tibble(variable = variable, group = group, value = value)
  df <- tidyr::pivot_wider(df, names_from = group, values_from = value)
  mat <- as.matrix(tibble::column_to_rownames(df, var = "variable"))
  cl <- stats::hclust(dist(mat), method = method)
  ord <- stats::order.dendrogram(stats::as.dendrogram(cl))
  factor(variable, unique(variable)[ord])
}


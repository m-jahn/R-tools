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
#' @importFrom tidyr complete
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
#' levels(with(df, fct_cluster(fc, group, response)))
#' 
#' # also works with NA or infinite values;
#' # infinite values are internally replaced with NA to allow clustering
#' df[c(1,6,7), "response"] <- -Inf
#' levels(with(df, fct_cluster(fc, group, response)))
#' 
#' # missing combinations of variables are completed with NA internally
#' df <- df[-c(1,6), ]
#' levels(with(df, fct_cluster(fc, group, response)))
#' 
#' # different order of factor level does not change result
#' df$fc <- factor(df$fc, c("c","b","e","d", "a"))
#' levels(with(df, fct_cluster(fc, group, response)))
#' 
#' @export
fct_cluster <- function(variable, group, value, method = "ward.D2") {
  variable <- as.character(variable)
  df <- tidyr::tibble(variable = variable, group = group, value = value)
  df <- tidyr::complete(df, variable, group)
  df <- tidyr::pivot_wider(df, names_from = group, values_from = value,
    values_fn = function(x){replace(x, is.infinite(x), NA)})
  mat <- as.matrix(tibble::column_to_rownames(df, var = "variable"))
  cl <- stats::hclust(dist(mat), method = method)
  ord <- stats::order.dendrogram(stats::as.dendrogram(cl))
  factor(variable, rownames(mat)[ord])
}

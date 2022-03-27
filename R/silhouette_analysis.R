#' Wrapper function to perform silhouette analysis on different cluster numbers
#' 
#' Silhouette analysis identifies the number of clusters that have highest
#' explanatory power. It tries to answer the question of how many different
#' clusters are required to optimally separate all clusters from their neighbors.
#' Good cluster separation results in a higher average silhouette width, the 
#' decisive metric to judge cluster number. This function applies silhouette
#' analysis iteratively for a vector of different cluster numbers and stores
#' the result in a list.
#' 
#' @importFrom cluster silhouette
#' @importFrom dendextend is.hclust
#' @importFrom stats cor
#' @importFrom stats kmeans
#' @importFrom grDevices grey
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename_with
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom lattice xyplot
#' @importFrom lattice panel.xyplot
#' @importFrom lattice panel.grid
#' @importFrom lattice panel.abline
#' @importFrom latticeExtra panel.ablineq
#' @importFrom latticeExtra ggplot2like
#' @importFrom latticeExtra panel.smoother
#' 
#' @param mat (numeric matrix) data matrix that clustering was performed on 
#'   (or will be performed using k-means clustering)
#' @param cluster_object (hclust) a cluster object obtained from running hclust(), optional
#' @param n_clusters (numeric) a vector of cluster numbers for which silhouette analysis is
#'   performed
#' @param n_repeats (numeric) scalar indicating the number of random permutations to perform
#'   analysis (default: 5)
#' @param plot (logical) if the function should return a list of summary plots also.
#'   Default is TRUE
#' 
#' @details Prerequesite for silhouette analysis is a cluster object that can be obtained
#'   by e.g. running hclust(d = dist(mat), method = "ward.D"). The alternative is to supply
#'   no cluster object, then the function performs a kmeans() clustering for the indicated
#'   number of clusters.
#' 
#' @return A list with five objects
#'   \code{data}: silhouette analysis data for each iteration, 
#'   \code{data_summary}: silhouette analysis data concise summary, 
#'   \code{optimal_n_clust}: optimal number of clusters, 
#'   \code{plot_clusters}: plot silhouette widths for all number of clusters separately, 
#'   \code{plot_summary}: plot silhouette widths summary
#' 
#' @examples
#' # generate a random matrix that we use for clustering with the 
#' # format of 100 rows (e.g. determined gene expression) and 10 
#' # columns (conditions)
#' mat <- matrix(rnorm(1000), ncol = 10)
#' 
#' # we can perform clustering on this matrix using e.g. hclust:
#' # there is clearly no good separation between different clusters of 'genes'
#' clust <- hclust(dist(mat))
#' plot(clust)
#' 
#' # perform silhouette analysis for 2 to 10 different clusters
#' sil_result <- silhouette_analysis(mat, n_clusters = 2:10)
#' 
#' # plot results
#' print(sil_result$plot_clusters, split = c(1,1,2,1), more = TRUE)
#' print(sil_result$plot_summary, split = c(2,1,2,1))
#' 
#' @export
# ------------------------------------------------------------------------------
silhouette_analysis <- function(
  mat,
  cluster_object = NULL,
  n_clusters = 2:10,
  n_repeats = 5,
  plot = TRUE
) {
  
  # check cluster_object properties
  if (!is.null(cluster_object)) {
    if (dendextend::is.hclust(cluster_object)) 
      print(cluster_object)
    else stop("cluster_object that was provided is not of class hclust\n")
  }
  
  if (!is.numeric(n_clusters))
    stop("n_clusters is not a numeric vector\n")
  if (any(n_clusters <= 1))
    stop("values of 1 or lower are not allowed for n_clusters\n")
  
  # all combinations of cluster numbers and repeats
  variables <- expand.grid(n_clusters, 1:n_repeats)
  
  # loop through silhouette function
  df <- mapply(
    n = variables$Var1,
    r = variables$Var2,
    SIMPLIFY = FALSE,
    FUN = function(n, r) {
      
      # if no cluster_object is provided use kmeans clustering
      if (is.null(cluster_object)) {
        kmeans_cluster <- kmeans(mat, centers = n, iter.max = 100)$cluster
        si.summary <- summary(silhouette(as.numeric(kmeans_cluster), dist(mat)))
      
      # else use the provided hclust object
      } else {
        si.summary <- silhouette(
          as.numeric(cutree(cluster_object, k = n)),
          dist(mat)
        ) %>% summary
      }
      
      data.frame(
        n_cluster = rep(n, n),
        n_repeat = rep(r, n),
        avg_width = rep(si.summary$avg.width, n),
        clus_sizes = si.summary$clus.sizes,
        clus_avg_widths = si.summary$clus.avg.widths
      )
    }
  )
  
  # merge list of data frames in one df
  df <- bind_rows(df)
  df <- rename_with(df, .fn = function(x) gsub("\\.", "_", x))
  
  if (plot) {
    # plot single cluster averages
    plot_clusters <- lattice::xyplot(get("clus_avg_widths") ~ 
        as.numeric(get("clus_sizes_cl")) |
        factor(get("n_cluster")), df, groups = get("n_repeat"),
      par.settings = latticeExtra::ggplot2like(),
      as.table = TRUE, border = FALSE, between = list(x = 0.5, y = 0.5),
      xlab = "individual cluster", ylab = "silhouette width",
      scales = list(alternating = FALSE),
      panel = function(x, y, ...) {
        lattice::panel.grid(h = -1, v = -1, col = "white")
        lattice::panel.xyplot(x, y, ...)
        latticeExtra::panel.ablineq(h = mean(y), lty = 2, col = grey(0.3),
          fontfamily = "FreeSans", pos = 3, ...)
      }
    )
  
    # and plot summary
    df_summary <- dplyr::distinct(
      dplyr::select(df, dplyr::all_of(c("n_cluster", "n_repeat", "avg_width"))))
    
    plot_summary <- lattice::xyplot(get("avg_width") ~ get("n_cluster"),
      df_summary,
      par.settings = latticeExtra::ggplot2like(),
      cex = 0.8, lwd = 1.5,
      xlab = "number of clusters", ylab = "average silhouette width",
      panel = function(x, y, ...) {
        lattice::panel.grid(h = -1, v = -1, col = "white")
        lattice::panel.xyplot(x, y, ...)
        latticeExtra::panel.smoother(x, y, ...)
        n_opt <- as.numeric(names(which.max(tapply(y, x, mean))))
        lattice::panel.abline(v = n_opt, lty = 2, col = grey(0.3), ...)
      }
    )
  }
  
  # determine optimal number of clusters
  optimal_n_clust <- dplyr::group_by(df_summary, .data[["n_cluster"]])
  optimal_n_clust <- dplyr::summarize(optimal_n_clust, avg_width = mean(.data[["avg_width"]]))
  optimal_n_clust <- dplyr::arrange(optimal_n_clust, dplyr::desc(.data[["avg_width"]]))
  optimal_n_clust <- optimal_n_clust[[1, "n_cluster"]]
  
  # return list with results
  result <- list(
    data = df,
    data_summary = df_summary,
    optimal_n_clust = optimal_n_clust
  )
  if (plot) {
    result$plot_clusters = plot_clusters
    result$plot_summary = plot_summary
  }
  return(result)
}

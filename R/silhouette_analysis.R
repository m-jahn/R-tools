#' Wrapper function to perform silhouette analysis on different cluster numbers
#' 
#' Silhouette analysis shows the clusters that have explanatory power.
#' That includes clusters that are best separated from the neighbours 
#' resulting in a higher average silhoutte width (the decisive metric to 
#' judge optimal cluster number). This function applies the silhouette analysis iteratively for a vector of 
#' different cluster numbers and stores results in a list.
#' 
#' @importFrom cluster silhouette
#' @importFrom dendextend is.hclust
#' @importFrom plyr ldply
#' @importFrom lattice xyplot
#' @importFrom stats cor
#' @importFrom stats kmeans
#' 
#' @param mat (numeric matrix) data matrix that clustering was performed on 
#'   (or will be performed using k-means clustering)
#' @param cluster_object (hclust) a cluster object obtained from running hclust(), optional
#' @param n_clusters (numeric) a vector of cluster numbers for which silhouette analysis is
#'   performed
#' 
#' @details Prerequesite for silhouette analysis is a cluster object that can be obtained
#'   by e.g. running hclust(d = dist(mat), method = "ward.D"). The alternative is to supply
#'   no cluster object, then the functions performs a kmeans() clustering for the indicated
#'   number of clusters.
#' 
#' @return a list with three objects, silhouette analysis data, and two summary plots
#'   obtained using lattice graphics.
#' 
#' @examples
#' 
#' # this function requires some additional packlages
#' library(lattice)
#' library(latticeExtra)
#' library(cluster)
#' library(dplyr)
#' 
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
#' print(sil_result$plot.clusters, split = c(1,1,2,1), more = TRUE)
#' print(sil_result$plot.summary, split = c(2,1,2,1))
#' 
#' @export
# ------------------------------------------------------------------------------
silhouette_analysis <- function(
  mat, 
  cluster_object = NULL,
  n_clusters
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
  
  # loop through silhouette function
  dat <- lapply(n_clusters, function(x) {
    
    # if no cluster_object is provided use kmeans clustering
    if (is.null(cluster_object)) {
      kmeans_cluster <- kmeans(mat, centers = x, iter.max = 100)$cluster
      si.summary <- silhouette(as.numeric(kmeans_cluster), dist(mat)) %>% summary
    
    # else use the provided hclust object
    } else {
      si.summary <- silhouette(
        as.numeric(cutree(cluster_object, k = x)),
        dist(mat)
      ) %>% summary
    }
    
    si.summary$n_clusters <- rep(x, x)
    si.summary[c("n_clusters", 
      "clus.avg.widths", 
      "avg.width", 
      "clus.sizes")] %>% 
    as.data.frame
    
  }) %>% plyr::ldply()
  
  # plot single cluster averages
  #as.numeric(clus.sizes.Freq)
  plot.clusters <- xyplot(clus.avg.widths ~ as.numeric(clus.sizes.cl)
      | factor(n_clusters), dat,
    par.settings = custom.lattice, as.table = TRUE, border = FALSE,
    xlab = "cluster elements", ylab = "silhouette width",
    panel = function(x, y, ...) {
      panel.grid(h = -1, v = -1, col = grey(0.9))
      panel.barplot(x, y, ewidth = 0.5, ...)
      panel.ablineq(h = mean(y), lty = 2, col = grey(0.3), fontfamily = "FreeSans", pos = 3)
    }
  )
  
  # and plot summary
  plot.summary <- xyplot(unique(dat$avg.width) ~ unique(dat$n_clusters),
    par.settings = custom.lattice, as.table = TRUE,
    type = c("p", "l"), pch = 19, col = grey(0.3),
    xlab = "number of clusters", ylab = "average silhouette width",
    panel = function(x, y, ...) {
      panel.grid(h = -1, v = -1, col = grey(0.9))
      panel.xyplot(x, y, ...)
    }
  )
  
  # return list with results
  cat("Silhouette analysis finished for clusters", min(n_clusters), "to", max(n_clusters))
  list(
    dat = dat,
    plot.clusters = plot.clusters,
    plot.summary = plot.summary
  )
}

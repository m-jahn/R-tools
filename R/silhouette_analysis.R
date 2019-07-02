#' Wrapper function to perform silhouette analysis on all clusters of a hclust object
#' 
#' Silhouette analysis shows the clusters that have explanatory power.
#' That includes clusters that are best separated from the neighbours (positive score)
#'
#' this function applies the iterative silhouette analysis
#' and stores results in a list
#' 
#' @importFrom cluster silhouette
#' @importFrom dendextend is.hclust
#' @importFrom plyr ldply
#' @importFrom lattice xyplot
#' 
#' @param mat (numeric matrix) the original data matrix that clustering was performed on
#' @param cluster_object (hclust) the cluster object obtained from running hclust()
#' @param n_clusters (numeric) a vector of cluster numbers for which silhouette analysis is
#'   performed
#' 
#' @details Prerequesite for silhouette analysis is a cluster object that can be obtained
#' by e.g. running hclust(d = dist(mat), method = "ward.D").
#' 
#' @return a list with three objects, silhouette analysis data, and two lattice result plots
#' 
#' @export
# ------------------------------------------------------------------------------
silhouetteAnalysis <- function(
  mat, 
  cluster_object, 
  n_clusters
) {
  
  # check object properties
  if (dendextend::is.hclust(cluster_object)) 
    print(cluster_object)
  else stop("cluster_object is not a hclust object\n")
  
  if (!is.numeric(n_clusters))
    stop("n_clusters is not a numeric vector\n")
  if (any(n_clusters <= 1))
    stop("values of 1 or lower are not allowed for n_clusters\n")
  
  # loop through silhouette function
  dat <- lapply(n_clusters, function(x) {
    
    si.summary <- silhouette(
      as.numeric(cutree(cluster_object, k = x)),
      dist(mat)
    ) %>% summary
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
    par.settings = custom.lattice, as.table = TRUE,
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

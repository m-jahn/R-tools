# SILHOUETTE ANALYSIS ON CLUSTER OBJECT
# 
# 
# author: Michael Jahn
# affiliation: Scilifelab - KTH, Stockholm
# date: 2019-01-23


# load required packages
library(cluster)
library(dendextend)
library(lattice)
library(latticeExtra)


# prerequesite for silhouette analysis is a cluster object that can be obtained
# e.g. by hclust(d=dist(mat), method="ward.D")
#
# The silhouette analysis will show us the clusters that have explanatory power;
# that includes clusters that are best separated from the neighbours (positive score)
#
# this function applies the iterative silhouette analysis
# and stores results in a list

silhouetteAnalysis <- function(cluster.object, n.clusters) {
  
  # check object properties
  if (is.hclust(cluster.object)) 
    print(cluster.object)
  else stop("cluster.object is not a hclust object\n")
  
  if (!is.numeric(n.clusters))
    stop("n.clusters is not a numeric vector\n")
  if (any(n.clusters <= 1))
    stop("values of 1 or lower are not allowed for n.clusters\n")
  
  # loop through silhouette function
  dat <- lapply(n.clusters, function(x) {
    
    si.summary <- silhouette(
      as.numeric(cutree(cluster.object, k = x)),
      dist(mat)
    ) %>% summary
    si.summary$n.clusters <- rep(x, x)
    si.summary[c("n.clusters", 
      "clus.avg.widths", 
      "avg.width", 
      "clus.sizes")] %>% 
    as.data.frame
    
  }) %>% plyr::ldply(.)
  
  # plot single cluster averages
  #as.numeric(clus.sizes.Freq)
  plot.clusters <- xyplot(clus.avg.widths ~ as.numeric(clus.sizes.cl)
      | factor(n.clusters), dat,
    par.settings = custom.lattice, as.table = TRUE,
    xlab = "cluster elements", ylab = "silhouette width",
    panel=function(x, y, ...) {
      panel.grid(h = -1, v = -1, col = grey(0.9))
      panel.barplot(x, y, ewidth = 0.5, border = NULL, fill = grey(0.7))
      panel.ablineq(h = mean(y), lty = 2, col = grey(0.3), fontfamily = "FreeSans", pos = 3)
    }
  )
  
  # and plot summary
  plot.summary <- xyplot(unique(dat$avg.width) ~ unique(dat$n.clusters),
    par.settings = custom.lattice, as.table = TRUE,
    type = c("p", "l"), pch = 19, col = grey(0.3),
    xlab = "number of clusters", ylab = "average silhouette width",
    panel = function(x, y, ...) {
      panel.grid(h = -1, v = -1, col = grey(0.9))
      panel.xyplot(x, y, ...)
    }
  )
  
  # return list with results
  cat("Silhouette analysis finished for clusters", min(n.clusters), "to", max(n.clusters))
  list(
    dat = dat,
    plot.clusters = plot.clusters,
    plot.summary = plot.summary
  )
}

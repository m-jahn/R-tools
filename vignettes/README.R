## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  devtools::install_github("https://github.com/m-jahn/R-tools")

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(Rtools)
library(lattice)
data(mtcars)

# mean and stdev error bars are drawn for
# common x values
xyplot(mpg ~ factor(cyl), mtcars, lwd = 2, 
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)

# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars, 
  groups = cyl, lwd = 2,
  panel = function(x, y, ...) {
    panel.barplot(x, y, ...)
  }
)

# we can also use different variables for the x var, grouping,
# and paneling. As a visual control that error bars are drawn
# for the correct groups we overlay the single data points.
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, auto.key = list(columns = 3),
  panel = function(x, y, ...) {
    panel.barplot(x, y, beside = TRUE, ...)
    panel.stripplot(x, y, jitter.data = TRUE,
      horizontal = FALSE, amount = 0.15, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
# simple example
df <- data.frame(
  Y = sample(1:10, 60, replace = TRUE), 
  X = factor(rep(1:3, each = 20))
)

xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)

# but with continuous Y variable, it doesn't work as expected
df$Y <- rnorm(60)
xyplot(Y ~ X, df, groups = X, panel = panel.beeswarm)

# for this purpose we can bin the Y variable into groups
xyplot(Y ~ X, df, groups = X, 
  panel = function(x, y, ...) {
    panel.xyplot(x, y, col = grey(0.6), ...)
    panel.beeswarm(x, y, bin_y = TRUE, breaks_y = 10, ...)
})

## ---- fig.height = 4, fig.width = 6-------------------------------------------
library(grid)
library(lattice)
library(directlabels)

data("mtcars")
mtcars$car <- rownames(mtcars)

# A standard example using lattice grouping and paneling;
# We can also draw boxes around labels and change label size
xyplot(mpg ~ wt | factor(cyl), mtcars,
  groups = cyl, pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1), cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

# A similar plot but without grouping. This requires explicit
# use of subscripts
xyplot(mpg ~ wt | factor(cyl), mtcars,
  pch = 19, labels = mtcars$car,
  as.table = TRUE, layout = c(3, 1),
  panel = function(x, y, subscripts, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, subscripts = subscripts, 
      draw_box = TRUE, box_fill = "white", ...)
  }
)

# An example without panels and more groups
xyplot(mpg ~ wt, mtcars,
  groups = hp, pch = 19, 
  labels = mtcars$wt, cex = 0.6,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.directlabel(x, y, draw_box = TRUE, box_line = TRUE, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# mean and stdev error bars are drawn for
# common x values
xyplot(mpg ~ factor(cyl), mtcars, 
  lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)

# using the same variable for x and grouping will
# result in typical lattice behavior
xyplot(mpg ~ factor(cyl), mtcars,
  groups = cyl, lwd = 2, pch = 19, cex = 1.5,
  panel = function(x, y, ...) {
    panel.errbars(x, y, ...)
  }
)

# we can also use different variables for the x var, grouping,
# and paneling. As a visual control that error bars are drawn 
# for the correct groups we overlay the single data points. 
xyplot(mpg ~ factor(cyl) | factor(vs), mtcars,
  groups = gear, lwd = 2, pch = 19, cex = 1.5,
  auto.key = list(columns = 3),
  panel = function(x, y, ...) {
    panel.stripplot(x, y, jitter.data = TRUE, 
      horizontal = FALSE, amount = 0.15, alpha = 0.3, ...)
    panel.errbars(x, y, beside = TRUE, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# Two examples for a custom lattice key
# inside a panel. The first with taking all arguments from the 
# top-level plotting function, the second with custom arguments.
xyplot(mpg ~ 1/wt | factor(vs), mtcars,
  groups = carb, pch = 19, cex = 1,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.key(...)
    panel.key(labels = letters[1:5], which.panel = 2, 
      corner = c(0.9, 0.1), col = 1:5, pch = 3, cex = 1)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(grid)
library(lattice)

data("USMortality")

# A simple example using lattice paneling
xyplot( ~ Rate | Sex, USMortality,
  main = "US mortality rates by sex",
  scales = list(draw = FALSE),
  panel = function(x, ...) {
    panel.piechart(x, ...)
  }
)

# A more advanced example using grouping and
# adjusting graphical parameters. The main variable 
# 'x' is now summed up for each value of 'groups'
xyplot( ~ Rate | Sex, USMortality,
  groups = gsub(" ", "\n", Cause), 
  col = heat.colors(10),
  border = grey(0.3), cex = 0.7,
  main = "US mortality rates by sex",
  scales = list(draw = FALSE),
  panel = function(x, ...) {
    panel.piechart(x, diameter_sector = 0.1, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# p-values are calculated between groups of x, grouping variable is ignored
xyplot(mpg ~ factor(cyl), mtcars, groups = cyl, pch = 19, cex = 0.7,
  panel = function(x, y, ...) {
    panel.stripplot(x, y, jitter.data = TRUE, 
      horizontal = FALSE, amount = 0.15, ...)
    panel.pvalue(x, y, std = 1, symbol = TRUE, pvalue = TRUE)
})

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# Default behavior for quadrants is to split x and y data
# at the respective median, and plot percentage of points 
# per quandrant
xyplot(mpg ~ 1/wt | factor(vs), mtcars,
  groups = carb, pch = 19, cex = 1,
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.quadrants(x, y, ...)
  }
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# colors are more subtle than default lattice theme
xyplot(mpg ~ factor(cyl) | gear, mtcars,
  groups = cyl, auto.key = list(columns = 3),
  par.settings = custom.ggplot()
)

## ---- fig.height = 3, fig.width = 5-------------------------------------------
library(lattice)
data(mtcars)

# colors are more subtle than default lattice theme
xyplot(mpg ~ factor(cyl) | gear, mtcars,
  groups = cyl, auto.key = list(columns = 3),
  par.settings = custom.lattice()
)

## ---- fig.width = 6, fig.height = 6-------------------------------------------
library(lattice)
data(mtcars)

# Draw a scatterplot matrix of all variables of a 
# data frame against each other.
custom_splom(mtcars[1:5])

# We can customize the scatterplot
custom_splom(
  mtcars[1:5],
  col_palette = rainbow(4),
  scales = 10, 
  xlab = "data points", ylab = "regression",
  pch = 1, col = 1, cex = 0.6
)

## -----------------------------------------------------------------------------
# load additional dependencies
library(dplyr)
library(tidyr)

# generate data frame
df <- data.frame(
  protein = c("A", "B", "C", "C/D", "C/D/E", "E", "F", "G"),
  n_protein = c(1,1,1,2,3,1,1,1),
  weight = rep(1,8),
  peptide = letters[1:8],
  ab1 = sample(1:100, 8),
  ab2 = sample(1:100, 8),
  ab3 = sample(1:100, 8)
)

aggregate_pep(
  data = df, 
  sample_cols = c("ab1", "ab2", "ab3"),
  protein_col = "protein",
  peptide_col = "peptide",
  n_protein_col = "n_protein",
  split_ambiguous = TRUE,
  split_char = "/",
  method = "sum"
)

## ---- message = FALSE, warning = FALSE----------------------------------------
df <- data.frame(
  protein = LETTERS[1:5],
  cond1 = sample(1:100, 5),
  cond2 = sample(1:100, 5),
  cond3 = sample(1:100, 5)
)

# normalize protein abundance to obtain identical 
# median expression of each column (condition)
# For this function we need to load one extra package
library(limma)

df_norm <- apply_norm(
  df, 
  norm_function = "normalizeMedianValues", 
  sample_cols = 2:ncol(df),
  ref_cols = NULL
)

# the data after normalization
print(df_norm)

# Has the normalization worked? We can compare column medians
# for original and normalized data
apply(df[2:4], 2, median)
apply(df_norm[2:4], 2, median)

## -----------------------------------------------------------------------------
# The get_topgo function will require the TopGO package
# as an additional dependency that is not automatically
# attached with this package.
library(topGO)

# a list of arbitrary GO terms
go_terms <- c(
  "GO:0006412", "GO:0015979", "GO:0046148", "GO:1901566", "GO:0042777", "GO:0006614",
  "GO:0016114", "GO:0006605", "GO:0090407", "GO:0031564", "GO:0032784", "GO:0052889",
  "GO:0032787", "GO:0043953", "GO:0046394", "GO:0042168", "GO:0009124", "GO:0006090",
  "GO:0016108", "GO:0016109", "GO:0016116", "GO:0016117", "GO:0065002", "GO:0006779",
  "GO:0072330", "GO:0046390", "GO:0006754", "GO:0018298", "GO:0006782", "GO:0022618",
  "GO:0042255", "GO:0046501", "GO:0070925", "GO:0071826", "GO:0006783", "GO:0009156"
)

# construct a sample data set with 26  different genes in 2 different groups
# and test which (randomly sampled) GO terms might be enriched in both groups.
# We randomly sample 1 to 3 GO terms per gene. They need to be formatted as one
# string of GO terms separated by "; ".
df <- data.frame(
  GeneID = LETTERS,
  cluster = rep(c(1, 2), each = 13),
  Gene.ontology.IDs = sapply(1:26,
    function(x) paste(sample(go_terms, sample(1:3, 1)), collapse = ";")
  ),
  stringsAsFactors = FALSE
)

# test if GO terms are enriched in group 1 against background
get_topgo(df, selected.cluster = 1, topNodes = 5)

## -----------------------------------------------------------------------------
# this function requires some additional packlages
library(lattice)
library(latticeExtra)
library(cluster)
library(dplyr)

# generate a random matrix that we use for clustering with the 
# format of 100 rows (e.g. determined gene expression) and 10 
# columns (conditions)
mat <- matrix(rnorm(1000), ncol = 10)

# we can perform clustering on this matrix using e.g. hclust:
# there is clearly no good separation between different clusters of 'genes'
clust <- hclust(dist(mat))
plot(clust)

# perform silhouette analysis for 2 to 10 different clusters
sil_result <- silhouette_analysis(mat, n_clusters = 2:10)

# plot results
print(sil_result$plot.clusters, split = c(1,1,2,1), more = TRUE)
print(sil_result$plot.summary, split = c(2,1,2,1))

## -----------------------------------------------------------------------------
# simulate growth according to the Baranyi growth model
# for a growth period of 100 hours
biomass <- baranyi_fun(
  LOG10N0 = -1, LOG10Nmax = 1,
  mumax = 0.1, lag = 10, t = 0:100)

# plot time versus biomass
plot(0:100, biomass)

## -----------------------------------------------------------------------------
# simulate growth according to the Baranyi growth model
# for a growth period of 100 hours
biomass <- gompertzm_fun(
  LOG10N0 = -1, LOG10Nmax = 1,
  mumax = 0.1, lag = 10, t = 0:100)

# plot time versus biomass
plot(0:100, biomass)


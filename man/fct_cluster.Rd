% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_cluster.R
\name{fct_cluster}
\alias{fct_cluster}
\title{Cluster levels of a factor based on a response and a grouping variable}
\usage{
fct_cluster(variable, group, value, method = "ward.D2")
}
\arguments{
\item{variable}{(vector)}

\item{group}{(character)}

\item{value}{(vector)}

\item{method}{(character) The `hclust` method to use for clustering, see `help("hclust")`. Default is `"ward.D2"`.}
}
\value{
A reordered factor.
}
\description{
The function changes the order of levels of a factor by clustering levels
according to similarity of a second response variable, and an optional third grouping
variable.
}
\details{
More figuratively speaking, the function re-arranges data from response variable,
grouping variable, and factor levels to a matrix and orders the rows (factor levels)
of the matrix according to similarity.
The clustering is internally performed by `hclust()` and takes arguments for `method`.
The function is useful to reorder a factor in a data frame in order to plot it in form
of a clustered heatmap, see examples.
}
\examples{

# set seed to obtain same values
set.seed(123)

# a data frame with 5 observations for 5 different groups (A to E)
df <- data.frame(
  fc = factor(rep(letters[1:5], 5)),
  group = rep(LETTERS[1:5], each = 5),
  response = rnorm(25)
)

# levels in alphabetical order
levels(df$fc)

# reorder levels of "fc" by clustering values in "response" over "groups"
levels(with(df, fct_cluster(fc, group, response)))

# also works with NA or infinite values;
# infinite values are internally replaced with NA to allow clustering
df[c(1,6,7), "response"] <- -Inf
levels(with(df, fct_cluster(fc, group, response)))

# missing combinations of variables are completed with NA internally
df <- df[-c(1,6), ]
levels(with(df, fct_cluster(fc, group, response)))

# different order of factor level does not change result
df$fc <- factor(df$fc, c("c","b","e","d", "a"))
levels(with(df, fct_cluster(fc, group, response)))

}

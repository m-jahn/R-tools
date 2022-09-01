## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  devtools::install_github("https://github.com/m-jahn/R-tools")

## ---- echo = FALSE------------------------------------------------------------
set.seed(123)

## ---- message = FALSE, warning = FALSE----------------------------------------
# load additional dependencies
library(Rtools)

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

# normalize protein abundance to obtain identical median;
# function borrowed from limma::normalizeMedianValues()
median_norm <- function(x) {
  cmed <- log(apply(x, 2, median, na.rm = TRUE))
  cmed <- exp(cmed - mean(cmed))
  t(t(x)/cmed)
}

df_norm <- apply_norm(
  df, 
  norm_function = median_norm, 
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
df$fc <- with(df, fct_cluster(fc, group, response))

# levels ordered by similarity of responses
levels(df$fc)

## ---- message = FALSE, warning = FALSE----------------------------------------
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
# set seed to obtain same values
set.seed(123)

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

## ---- message = FALSE, warning = FALSE----------------------------------------
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
print(sil_result$plot_clusters, split = c(1,1,2,1), more = TRUE)
print(sil_result$plot_summary, split = c(2,1,2,1))

## ---- fig.width = 4, fig.height = 3-------------------------------------------
# simulate growth according to the Baranyi growth model
# for a growth period of 100 hours
biomass <- baranyi_fun(
  LOG10N0 = -1, LOG10Nmax = 1,
  mumax = 0.1, lag = 10, t = 0:100)

# plot time versus biomass
plot(0:100, biomass)

## ---- fig.width = 4, fig.height = 3-------------------------------------------
# simulate growth according to the Baranyi growth model
# for a growth period of 100 hours
biomass <- gompertzm_fun(
  LOG10N0 = -1, LOG10Nmax = 1,
  mumax = 0.1, lag = 10, t = 0:100)

# plot time versus biomass
plot(0:100, biomass)


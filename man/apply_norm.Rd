% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_norm.R
\name{apply_norm}
\alias{apply_norm}
\title{Apply normalization based on different published methods}
\usage{
apply_norm(
  data,
  norm_function = "none",
  sample_cols = 1:ncol(data),
  ref_cols = NULL
)
}
\arguments{
\item{data}{the input data frame}

\item{norm_function}{(character) the normalization function to be applied (one of "none", 
"normalizeMedianValues", "normalize.quantiles", "normalize.quantiles.robust", 
"normalize.quantiles.in.blocks", "justvsn")}

\item{sample_cols}{(numeric) The sample columns of the input data frame to be normalized. Default is to use all columns}

\item{ref_cols}{(numeric) The reference to be normalized to. Does not work with all functions. The default is NULL}
}
\value{
a normalized data frame
}
\description{
This function is a wrapper applying different normalization functions from
other packages, such as `limma`, `justvsm` and `preprocesscore`.
These are not imported automatically but have to be installed separately.
Alternatively, it will apply any custom normalization functions that is
passed to the `norm_function` argument.
}
\details{
This function provides several normalization functions to re-scale the peptide
quantities of a sample to improve comparability and remove systematic bias,
like differences in total sample concentration or sample load.

A very simple normalization is based on aligning the median of all samples.
More sophisticated normalization functions include VSN normalization
that returns log2 transformed data. This is back-transformed to be
compatible with subsequent peptide aggregation.

More normalization functions are taken from package preProcessCore that
offers quantile normalization, robust quantile normalization, or block
quantile normalization. Quantile normalization uses a rank based approach
where intensities are rescaled between minimum, median and maximum. 
Rank-based normalizations don't keep original difference between intensities
within one sample. Therefore it is not advisable to use quantile-normalized
data for relative comparison within a sample.
}
\examples{
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

}

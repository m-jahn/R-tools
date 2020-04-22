#' Apply normalization based on different published methods
#' 
#' This function is a wrapper applying different normalization functions from
#' other packages, such as limma, justvsm and preprocesscore, see details.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom gtools quantcut
#' @importFrom stats cutree
#' @importFrom stats dist
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom stats t.test
#' @importFrom stats weighted.mean
#' 
#' @param data the input data frame
#' @param norm_function (character) the normalization function to be applied (one of "none", 
#'   "normalizeMedianValues", "normalize.quantiles", "normalize.quantiles.robust", 
#'   "normalize.quantiles.in.blocks", "justvsn")
#' @param sample_cols (numeric) The sample columns of the input data frame to be normalized. Default is to use all columns
#' @param ref_cols (numeric) The reference to be normalized to. Does not work with all functions. The default is NULL
#' 
#' @details This function provides several normalization functions to re-scale the peptide
#' quantities of a sample to improve comparability and remove systematic bias,
#' like differences in total sample concentration or sample load.
#'
#' A very simple normalization is based on aligning the median of all samples.
#' More sophisticated normalization functions include VSN normalization
#' that returns log2 transformed data. This is back-transformed to be
#' compatible with subsequent peptide aggregation.
#'
#' More normalization functions are taken from package preProcessCore that
#' offers quantile normalization, robust quantile normalization, or block
#' quantile normalization. Quantile normalization uses a rank based approach
#' where intensities are rescaled between minimum, median and maximum. 
#' Rank-based normalizations don't keep original difference between intensities
#' within one sample. Therefore it is not advisable to use quantile-normalized
#' data for relative comparison within a sample.
#' 
#' @return a normalized data frame
#' 
#' @examples
#' df <- data.frame(
#'   protein = LETTERS[1:5],
#'   cond1 = sample(1:100, 5),
#'   cond2 = sample(1:100, 5),
#'   cond3 = sample(1:100, 5)
#' )
#' 
#' # normalize protein abundance to obtain identical 
#' # median expression of each column (condition)
#' # For this function we need to load one extra package
#' library(limma)
#' 
#' df_norm <- apply_norm(
#'   df, 
#'   norm_function = "normalizeMedianValues", 
#'   sample_cols = 2:ncol(df),
#'   ref_cols = NULL
#' )
#' 
#' # the data after normalization
#' print(df_norm)
#' 
#' # Has the normalization worked? We can compare column medians
#' # for original and normalized data
#' apply(df[2:4], 2, median)
#' apply(df_norm[2:4], 2, median)
#' 
#' @export
# ------------------------------------------------------------------------------
apply_norm <- function(
  data, 
  norm_function = "normalizeMedianValues", 
  sample_cols = 1:ncol(data),
  ref_cols = NULL
) {
  
  if (norm_function == "normalize.quantiles.in.blocks") {
    
    # this function requires blocks. We define an arbitrary number of blocks
    # from the sorted median intensity of all samples
    blocks <- apply(data[sample_cols], 1, function(x) median(x, na.rm = TRUE)) %>% 
      quantcut(q = 20)
    data_norm <- data[sample_cols] %>% as.matrix %>%
      preprocessCore::normalize.quantiles.in.blocks(blocks)
    
  } else if (norm_function == "justvsn") {
      
    if (is.null(ref_cols)) {
      data_norm <- justvsn(as.matrix(data[sample_cols])) %>% 
        apply(2, function(x) 2^x)
    } else {
      data_norm <- justvsn(as.matrix(data[sample_cols]), 
        reference = vsn2(as.matrix(data[ref_cols]))) %>% 
        apply(2, function(x) 2^x)
    }
      
  } else if (norm_function != "none") {
      
    data_norm <- do.call(
      get(norm_function), 
      list(x = as.matrix(data[sample_cols]))
    )
      
  } else {
    data_norm <- data
  }
  # return normalized dataframe
  data_norm <- as.data.frame(data_norm) %>% setNames(colnames(data[sample_cols]))
  dplyr::bind_cols(data %>% dplyr::select(-sample_cols), data_norm)
}

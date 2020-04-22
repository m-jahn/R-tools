#' Aggregate peptide abundances to protein abundances
#' 
#' Similar to the openMS module ProteinQuantifier, this function provides different
#' methods to aggregate peptide intensities to their parent proteins. It is mainly
#' intended for the use with (raw) Diffacto results, a table of peptide intensities
#' and covariation scores (weights) that can be used to filter peptides before aggregating
#' them up to protein abundances.
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter_at
#' @importFrom dplyr select_at
#' @importFrom dplyr group_by_at
#' @importFrom dplyr all_vars
#' @importFrom tidyr separate_rows
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' 
#' @param data the input data frame
#' @param sample_cols (character) columns to be used for peptide aggregation
#' @param protein_col (character) column containing unique protein IDs/names
#' @param peptide_col (character) column containing unique peptide IDs/sequences
#' @param n_protein_col (character) column containing number of proteins annotated for this peptide.
#'   THis column indicates ambiguous peptides whose abundance are shared between n proteins.
#' @param split_ambiguous (logical) if those protein groups should be split into individual proteins or not
#' @param split_char (character) character by which to split protein groups
#' @param weight_col (character) the column containing weights or covariance scores
#' @param weight_threshold (numeric) covariance score (weight) cutoff, Diffacto's default is 0.5
#' @param method (character) aggregation method, one of ('sum', 'weightedsum', 'mean', 'weightedmean', 'wgeomean'). 
#'   The default is 'sum'
#'   
#' @return a data frame with aggregated protein intensities, one protein at a  row
#' 
#' @examples
#' # load additional dependencies
#' library(dplyr)
#' library(tidyr)
#' 
#' # generate data frame
#' df <- data.frame(
#'   protein = c("A", "B", "C", "C/D", "C/D/E", "E", "F", "G"),
#'   n_protein = c(1,1,1,2,3,1,1,1),
#'   weight = rep(1,8),
#'   peptide = letters[1:8],
#'   ab1 = sample(1:100, 8),
#'   ab2 = sample(1:100, 8),
#'   ab3 = sample(1:100, 8)
#' )
#' 
#' aggregate_pep(
#'   data = df, 
#'   sample_cols = c("ab1", "ab2", "ab3"),
#'   protein_col = "protein",
#'   peptide_col = "peptide",
#'   n_protein_col = "n_protein",
#'   split_ambiguous = TRUE,
#'   split_char = "/",
#'   method = "sum"
#' )
#' 
#' @export
# ------------------------------------------------------------------------------
aggregate_pep <- function(
  data, 
  sample_cols,
  protein_col,
  peptide_col,
  n_protein_col = NULL,
  split_ambiguous = FALSE,
  split_char = NULL,
  weight_col = NULL,
  weight_threshold = 0.5, 
  method = "sum"
) {
    
  # methods for peptide aggregation to proteins
  methods = list(
    sum = function(x, weight = NULL) sum(x, na.rm = TRUE),
    weightedsum = function(x, weight) sum(x*weight, na.rm = TRUE),
    mean = function(x, weight = NULL) mean(x, na.rm = TRUE),
    weightedmean = function(x, weight) weighted.mean(x, weight, na.rm = TRUE),
    wgeomean = function(x, weight) exp(weighted.mean(log(x), weight, na.rm = TRUE))
  )
  
  # optional preprocessing: should abundance of ambiguous peptides
  # be shared among two or more proteins? The default is to remove ambiguous
  # peptides (openMS ProteinQuantifier). But a simple way to include them
  # is to evenly disitribute ambiguous petide abundance between proteins.
  # More elaborate algorithms exist in the literature, but this problem usually 
  # plays a minor roll in microbes (fewer homologs, no splicing variants)
  if (split_ambiguous) {
    
    if (is.character(split_char) & is.character(n_protein_col)) {
      data <- data %>% 
        separate_rows(!!protein_col, sep = split_char) %>%
        mutate_at(sample_cols, function(x) x / .[[n_protein_col]])
    } else {
      stop("arguments 'split_char' and 'n_protein_col' must be character")
    }
  }
  
  # filter peptides by weight threshold
  # TODO add rule to keep all peptides if no peptide exceeds weight threshold
  if (!is.null(weight_col)) {
    data <- filter_at(data, weight_col, all_vars(. >= weight_threshold))
  }
  
  # add dummy column with unity weights if not supplied
  if (is.null(weight_col)) {
    weight_col = "weight"
    data <- mutate(data, weight = 1)
  }
  
  # rearrange data frame to long format, in order to summarise
  data %>% gather(key = sample, value = intensity, sample_cols) %>%
    
    # group by protein ID and sample
    group_by_at(c("sample", protein_col)) %>%
    
    # summarise by applying method of choice
    summarise(
      n_peptides = length(unique(get(peptide_col))),
      intensity = do.call(methods[[method]], list(intensity, get(weight_col)))
    ) %>%
    
    # spread samples on columns again
    spread(sample, intensity) %>%
    
    # change order to peptide abundances as last
    select_at(c(protein_col, "n_peptides", sample_cols))
  
}

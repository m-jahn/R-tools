#' Convenience wrapper to TopGO package (Rahnenfueher et al.)
#' 
#' This function carries out a TopGO gene ontology enrichment on a data set
#' with custom protein/gene IDs and GO terms. The function takes as main 
#' input a data frame with three specific columns: cluster numbers, Gene IDs, 
#' and GO terms. Alternatively, these can also be supplied as three individual 
#' lists.
#'  
#' @param df an (optional) data.frame with the three columns named as specified below (`GeneID`, 
#'   `Gene.ontology.IDs`, `cluster`)
#' @param GeneID (character) The column containing gene IDs, alternatively a vector
#' @param Gene.ontology.IDs (character) The column containing a list of GO terms for each gene, 
#'   alternatively a vector with same order and length as 'GeneID'
#' @param cluster (numeric, factor, character) the column containing a grouping variable, 
#'   alternatively a vector with same order and length as 'GeneID'
#' @param selected.cluster (character) the name of the group that is to be comapred to the background.
#'   Must be one of 'cluster'. If not specified, the first factor level is used (alphabetical order).
#' @param topNodes (numeric) the max number of GO terms (nodes) to be returned by the function.
#' 
#' @return a data.frame with TOpGO gene enrichment results
#' 
#' @examples 
#' 
#' # The get_topgo function will require the TopGO package 
#' # as an additional dependency that is not automatically 
#' # attached with this package.
#' library(topGO)
#' 
#' # a list of arbitrary GO terms
#' go_terms <- c(
#'   "GO:0006412", "GO:0015979", "GO:0046148", "GO:1901566", "GO:0042777", "GO:0006614",
#'   "GO:0016114", "GO:0006605", "GO:0090407", "GO:0031564", "GO:0032784", "GO:0052889",
#'   "GO:0032787", "GO:0043953", "GO:0046394", "GO:0042168", "GO:0009124", "GO:0006090",
#'   "GO:0016108", "GO:0016109", "GO:0016116", "GO:0016117", "GO:0065002", "GO:0006779",
#'   "GO:0072330", "GO:0046390", "GO:0006754", "GO:0018298", "GO:0006782", "GO:0022618",
#'   "GO:0042255", "GO:0046501", "GO:0070925", "GO:0071826", "GO:0006783", "GO:0009156"
#' )
#' 
#' # construct a sample data set with 26  different genes in 2 different groups
#' # and test which (randomly sampled) GO terms might be enriched in both groups.
#' # We randomly sample 1 to 3 GO terms per gene. They need to be formatted as one 
#' # string of GO terms separated by "; ".
#' df <- data.frame(
#'   GeneID = LETTERS,
#'   cluster = rep(c(1, 2), each = 13),
#'   Gene.ontology.IDs = sapply(1:26, 
#'     function(x) paste(sample(go_terms, sample(1:3, 1)), collapse = ";")
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # test if GO terms are enriched in group 1 against background
#' get_topgo(df, selected.cluster = 1, topNodes = 5)
#' 
#' @export
# ------------------------------------------------------------------------------
get_topgo <- function(df = NULL, GeneID = NULL, Gene.ontology.IDs = NULL, 
  cluster = NULL, selected.cluster, topNodes = 50
) {
  
  # prepare data structures
  # if a data.frame is passed as main data structure it must contain
  # three specific columns with cluster numbers, Gene IDs and GO terms.
  # GO terms is a character vector with go IDs separated by '; '
  #
  # if three separate lists are provided, they must be of same length
  # and order of geneIDs, cluster numbers and GO IDs must correspond 
  # to each other
  
  # function to collect and prepare input data
  generate.input <- function(cluster, GeneID, Gene.ontology.IDs) {
    
    # test for duplicate IDs
    if (any(duplicated(GeneID))) 
      stop("no duplicated GeneIDs allowed")
    
    # collect input
    genelist <- cluster; names(genelist) <- GeneID
    geneID2GO <- strsplit(Gene.ontology.IDs, "; ?")
    names(geneID2GO) <- GeneID
    
    # return two lists of genes and GO terms
    list(genelist = genelist, geneID2GO = geneID2GO)
    
  }
  
  if (class(df) == "data.frame" & 
    all(c("cluster", "GeneID", "Gene.ontology.IDs") %in% colnames(df))) {
    
    input <- with(df, 
      generate.input(cluster, GeneID, Gene.ontology.IDs)
    )
    
  } else if (!is.null(cluster) & !is.null(GeneID) & !is.null(Gene.ontology.IDs)) {
    input <- generate.input(cluster, GeneID, Gene.ontology.IDs)
    
  } else 
    stop("no data provided or data not sufficiently formatted")
  
  
  # create topGO object
  topGOdata <- new("topGOdata",
    allGenes = input$genelist, 
    annot = topGO::annFUN.gene2GO, 
    gene2GO = input$geneID2GO,
    geneSel = function(x) x == selected.cluster,
    ontology = "BP")

  # We can use e.g. two types of test statistics: Fisher’s exact test which is based 
  # on gene counts (e.g. genes from one cluster), and a Kolmogorov-Smirnov- 
  # like test which computes enrichment based on gene scores (p-values). 
  # Kolmogorov-Smirnov is only valid when p-values are provided, not a 
  # set of interesting genes (e.g. a cluster)!
  resultFisherClassic <- topGO::runTest(topGOdata, algorithm = "classic", statistic = "fisher")
  resultFisherWeight <- topGO::runTest(topGOdata, algorithm = "weight", statistic = "fisher")
  resultFisherElim <- topGO::runTest(topGOdata, algorithm = "elim", statistic = "fisher")
  
  # collect all test results in one table
  GenTab <- topGO::GenTable(topGOdata, 
    classicFisher = resultFisherClassic,
    weightedFisher = resultFisherWeight,
    elimFisher = resultFisherElim,
    orderBy = "elimFisher", ranksOf = "elimFisher", 
    topNodes = topNodes)
  
  # add gene names that are contained in the respective cluster/GO term
  GenTab$SigGenes <- sapply(GenTab$GO.ID, function(term){
    paste(collapse = ",",
      topGO::genesInTerm(topGOdata, term)[[1]][topGO::scoresInTerm(topGOdata, term)[[1]] == selected.cluster]
    )
  })
  GenTab
}

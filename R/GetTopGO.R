#' Convenience wrapper to TopGO package (Rahnenfueher et al.)
#' 
#' This function carries out a TopGO gene ontology enrichment on a data set
#' with custom protein/gene IDs and GO terms. The function takes as main 
#' input a data frame with three specific columns, cluster numbers, Gene IDs, 
#' and GO terms. Alternatively, these can also be supplied as three individual 
#' lists.
#'  
#' @param df an (optional) data.frame with the three columns specified below
#' @param GeneID (character) The column containing gene IDs, alternatively a vector
#' @param Gene.ontology.IDs (character) The column containing a list of GO terms for each gene, 
#'   alternatively a vector with same order and length as 'GeneID'
#' @param cluster (numeric, factor, character) the column containing a grouping variable, 
#'   alternatively a vector with same order and length as 'GeneID'
#' @param selected.cluster (character) the name of the group that is to be comapred to the background.
#'   Must be one of 'cluster'
#' @param topNodes (numeric) the max number of GO terms (nodes) to be returned by the function.
#' 
#' @return a data.frame with TOpGO gene enrichment results
#' 
#' @export
# ------------------------------------------------------------------------------

GetTopGO <- function(df = NULL, GeneID = NULL, Gene.ontology.IDs = NULL, 
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

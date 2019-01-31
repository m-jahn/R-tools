# CONVENIENCE FUNCTION TO TOPGO PACKAGE
# from Rahnenfueher et al.
# 
# author: Michael Jahn
# affiliation: Scilifelab - KTH, Stockholm
# date: 2019-01-23


# load required packages
library("topGO")

GetTopGO <- function(df=NULL, cluster=NULL, GeneID=NULL, 
  Gene.ontology.IDs=NULL, topNodes=50, selected.cluster) {
  
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
    list(genelist=genelist, geneID2GO=geneID2GO)
    
  }
  
  if (class(df)=="data.frame" & 
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
    allGenes=input$genelist, 
    annot=annFUN.gene2GO, 
    gene2GO=input$geneID2GO,
    geneSel=function(x) x==selected.cluster,
    ontology="BP")

  # We can use e.g. two types of test statistics: Fisher’s exact test which is based 
  # on gene counts (e.g. genes from one cluster), and a Kolmogorov-Smirnov- 
  # like test which computes enrichment based on gene scores (p-values). 
  # Kolmogorov-Smirnov is only valid when p-values are provided, not a 
  # set of interesting genes (e.g. a cluster)!
  resultFisherClassic <- runTest(topGOdata, algorithm="classic", statistic="fisher")
  resultFisherWeight <- runTest(topGOdata, algorithm="weight", statistic="fisher")
  resultFisherElim <- runTest(topGOdata, algorithm="elim", statistic="fisher")
  
  # collect all test results in one table
  GenTab <- GenTable(topGOdata, 
    classicFisher=resultFisherClassic,
    weightedFisher=resultFisherWeight,
    elimFisher=resultFisherElim,
    orderBy="elimFisher", ranksOf="elimFisher", 
    topNodes=topNodes)
  
  # add gene names that are contained in the respective cluster/GO term
  GenTab$SigGenes <- sapply(GenTab$GO.ID, function(term){
    paste(collapse=",",
      genesInTerm(topGOdata, term)[[1]][scoresInTerm(topGOdata, term)[[1]]==selected.cluster]
    )
  })
  GenTab
}
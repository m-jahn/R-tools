# CONVENIENCE FUNCTION TO TOPGO PACKAGE
# from Rahnenfueher et al.
# 
# author: Michael Jahn
# affiliation: Scilifelab - KTH, Stockholm
# date: 2019-01-23


# load required packages
library("topGO")

GetTopGO <- function(df=NULL, cluster.list=NULL, GeneID=NULL, 
  Gene.ontology.IDs=NULL, topNodes=50, cluster) {
  
  # prepare data structures
  # if a data.frame is passed as main data structure it must contain
  # three specific columns with cluster numbers, Gene IDs and GO terms.
  # GO terms is a character vector with go IDs separated by '; '
  if (class(df)=="data.frame" & 
    all(c("cluster", "GeneID", "Gene.ontology.IDs") %in% colnames(df))) {

    if (any(duplicated(df$GeneID))) 
      stop("no duplicated Gene IDs allowed in data frame")
    
    genelist <- df$cluster; names(genelist) <- df$GeneID
    geneID2GO <- strsplit(df$Gene.ontology.IDs, ";? ")
    names(geneID2GO) <- df$GeneID
    
  } else if (!is.null(cluster.list) & !is.null(GeneID) & !is.null(Gene.ontology.IDs)) {
    
    genelist <- cluster.list; names(genelist) <- GeneID
    geneID2GO <- strsplit(Gene.ontology.IDs, ";? ")
    names(geneID2GO) <- GeneID
    
  } else 
    stop("no data provided or data not sufficiently formatted")
  
  # create topGO object
  topGOdata <- new("topGOdata",
    allGenes=genelist, 
    annot=annFUN.gene2GO, 
    gene2GO=geneID2GO,
    geneSel=function(x) x==cluster,
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
      genesInTerm(topGOdata, term)[[1]][scoresInTerm(topGOdata, term)[[1]]==cluster]
    )
  })
  GenTab
}
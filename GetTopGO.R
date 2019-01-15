library("topGO")

GetTopGO <- function(df, topNodes, cluster) {
  # prepare data structures
  genelist <- df$cluster; names(genelist) <- df$protein
  geneID2GO <- strsplit(df$Gene.ontology.IDs, "; ")
  names(geneID2GO) <- df$protein
  
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
    orderBy="classicFisher", ranksOf="classicFisher", 
    topNodes=topNodes)
  
  # add gene names that are contained in the respective cluster/GO term
  GenTab$SigGenes <- sapply(GenTab$GO.ID, function(term){
    paste(collapse=",",
      genesInTerm(topGOdata, term)[[1]][scoresInTerm(topGOdata, term)[[1]]==cluster]
    )
  })
  GenTab
}
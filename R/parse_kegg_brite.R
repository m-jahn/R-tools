#' Parse Kegg Brite xml files step-by-step
#' 
#' This script is a small utility to parse Kegg Brite XML files and return
#' a regular data frame instead.
#' 
#' @importFrom tidyr separate
#' 
#' @param data a data frame with 5 columns as input
#' 
#' @details Changes that need to be made to the Kegg XML file before applying function (!):
#' - replace double spaces \code{'  '} by commas \code{','} or tabs \code{'\t'}
#' - remove first lines until regular content begins
#' - possibly add some trailing tabs or commas to end of first line
#'   so that read.table knows how many columns to expect
#'   
#' @export
# ------------------------------------------------------------------------------
parse_kegg_brite <- function(data) {
  
  if (ncol(data) == 5) {
    colnames(data) <-	c("System","Process","Pathway","Protein", "EC-class")
  } else {
    stop("Data frame must have 5 columns: 'System','Process','Pathway','Protein', 'EC-class'")
  }
  
  # purge the table from tags and sybmbols
  data <- as.data.frame(stringsAsFactors = FALSE,
    apply(data, 2, function(x) {
      gsub("[A-D]$|[A-D]<?.b>|<?.b>| \\[.*\\]|^[0-9]{5} ", "", x)
    })
  )
  
  # fill empty cells columnwise
  data[1:3] <- as.data.frame(
    apply(data[1:3], 2, function(x){
      entries <- which(x != "")
      rep(x[entries], diff(c(0, entries[-1], length(x))))
    })
  )
  
  # split protein column in IDs and trivial name
  data <- tidyr::separate(data, Protein, c("Protein", "full_name"), sep="; ")
  data <- tidyr::separate(data, Protein, c("Protein", "short_name"), sep=" ")
  
  # remove rows without proteins or exp values
  data <- subset(data, Protein != "")
  
  # trim some filling material to more concise description
  data <- as.data.frame(
    apply(data, 2, function(x) {
      gsub("A[0-9]{5} (Brite Hierarchies)?|Protein families: | - prokaryotes", "", x)
    })
  )
  
  # optionally propagate some terms from second to first level if missing
  data[data$Process == "genetic information processing", "System"] <- "Genetic Information Processing"
  data[data$Process == "metabolism", "System"] <- "Metabolism"
  data[data$Process == "signaling and cellular processes", "System"] <- "Cellular Processes"
  
  # return re-formatted data frame
  data
  
}

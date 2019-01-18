#
# This script is a small utility to parse Kegg Brite XML files and return
# a regular data frame instead. The data frane can be merged with gene 
# expression data and be used to cluster such data based on Kegg Brite
# categories
# 
# Author: Michael Jahn, PhD
# Affiliation: Scilifelab (KTH), Stockholm
# Date: 2019-01-17
#
# 
# LIBRARIES FOR DATA MANIPULATION ++++++++++++++++++++++++++++++++++++++++++++++
library(tidyr)
library(plyr)


# CHANGES THAT NEED TO BE MADE TO THE KEGG XML FILE
# in Kegg text file, replace double spaces >  < by comma >,< or >\t<
# Remove first lines until regular content begins
# Possibly add some trailing tabs or commas to end of first line
# so that read.table knows how many columns to expect

brite	<- read.table("KEGG_Brite_20180910_reh00001.keg", sep="\t", 
  fill=TRUE, stringsAsFactors=FALSE, quote = "\"")
colnames(brite) <-	c("System","Process","Pathway","Protein", "EC-class")

# purge the table from tags and sybmbols
brite <- as.data.frame(stringsAsFactors=FALSE,
  apply(brite, 2, function(x) {
    gsub("[A-D]$|[A-D]<?.b>|<?.b>| \\[.*\\]|^[0-9]{5} ", "", x)
  })
)

# fill empty cells columnwise
brite[1:3] <- as.data.frame(
  apply(brite[1:3], 2, function(x){
    entries <- which(x != "")
    rep(x[entries], diff(c(0, entries[-1], length(x))))
  })
)

# split protein column in IDs and trivial name
brite <- separate(brite, Protein, c("Protein", "full_name"), sep="; ")
brite <- separate(brite, Protein, c("Protein", "short_name"), sep=" ")

# remove rows without proteins or exp values
brite <- subset(brite, Protein != "")

# trim some filling material to more concise description
brite <- apply(brite, 2, function(x) {
  gsub("A[0-9]{5} (Brite Hierarchies)?|Protein families: | - prokaryotes", "", x)
}) %>% as.data.frame

# optionally propagate some terms from second to first level if missing
brite[brite$Process=="genetic information processing", "System"] <- "Genetic Information Processing"
brite[brite$Process=="metabolism", "System"] <- "Metabolism"
brite[brite$Process=="signaling and cellular processes", "System"] <- "Cellular Processes"


# export re-formatted csv table
write.csv(brite, file="KEGG_Brite_20180910_reh00001.csv")

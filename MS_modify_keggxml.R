## DATA HANDLING
library(tidyr)
library(plyr)
#rm(list=ls())
setwd("Documents/SciLifeLab/Resources/MS/databases/")

# in Kegg text file: 
# replace all commas by semicolon or minus (e.g. in fructose-1,6-bisphosphate), 
# replace double space >  < by comma >,<
# replace tab >\t< by comma >,<

brite	<- read.table("KeggBrite.txt", sep=",", fill=TRUE, stringsAsFactors=FALSE, quote = "\"")
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

# split protein column in ID and trivial name
brite$GeneID <- substr(brite$Protein, 1, 7)
brite$Protein <- substr(brite$Protein, 8, 200)

# remove rows without proteins or exp values
brite <- subset(brite, GeneID != "")


# export re-formatted csv table
write.csv(brite, file="KeggBrite.csv")

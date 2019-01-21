
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# IMPORT AND ADJUST PEPTIDE QUANTIFICATION FROM DIFFACTO
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# author: Michael Jahn
# affiliation: Scilifelab (KTH), Stockholm
# date: 2019-01-14

# LOAD PACKAGES
library(tidyr)
library(plyr)
library(lattice)
library(latticeExtra)
library(preprocessCore)
library(limma)
library(gtools)
library(vsn)
source("custom.theme.R")


# DEFINE FUNCTION OPTIONS
#
# load raw data
df <- read.table("/media/ProteomicsCyano/MS_analysis/20181204/Diffacto/peptides_DeMixQ_Diffacto.tsv",
  header=TRUE, stringsAsFactors=FALSE, sep="\t")
# define value columns
sample.cols <- 4:ncol(df)
# replace '1' (DemixQ output for not-quantified) with NA
df[ , sample.cols] <- apply(df[sample.cols], 2, function(x) replace(x, x==1, NA))
# define normalization function (one of "none", "normalizeMedianValues",
# "normalize.quantiles", "normalize.quantiles.robust", 
# "normalize.quantiles.in.blocks", "justvsn")
norm.function <- "justvsn"
# flag for plot controls
plot.norm <- TRUE; plot.to.png=TRUE
# aggregation method, one of ("sum", "weightedsum", "mean", "weightedmean", "wgeomean")
method <- "weightedsum"
# filename prefix
filename="diffacto_vsnNorm_"
# define the output directory(s) where tables are saved
output_dir <- list(
  "/media/ProteomicsCyano/MS_analysis/20181204/Diffacto/quantification/",
  "~/Documents/SciLifeLab/Experiments/20181204_MS_Ralstonia/quantification/"
)


# OPTIONAL NORMALIZATION OF PEPTIDE QUANTITIES +++++++++++++++++++++++++++++++++
#
# this step is similar to the openMS node ConsensusMapNormalizer
# it uses one of several normalization functions to re-scale the peptide
# quantities of a sample to improve comparability and remove systematic bias
# like different total sample concentration
#
# A simple normalization is based on aligning median of all samples.
# Another normalization that was identified as superior is VSN normalization
# that returns log2 transformed data. This is back-transformed to be
# compatible with subsequent peptide aggregation.
#
# Some normalization functions are taken from package preProcessCore that
# offers quantile normalization, robust quantile normalization, or block
# quantile normalization. Quantile normalization uses a rank based approach
# where intensities are rescaled between min, median and max. Use with care: 
# The ranks replace intensities so that differences _within one sample_ 
# are distorted

apply.norm <- function(df, norm.function, sample.cols) {
  
  if (norm.function=="normalize.quantiles.in.blocks") {
    # this function requires blocks. We define an arbitrary number of blocks
    # from the sorted median intensity of all samples
    blocks <- apply(df[sample.cols], 1, FUN=function(x) median(x, na.rm=TRUE)) %>% 
      quantcut(q=20)
    df[1:nrow(df), sample.cols] <- 
    as.matrix(df[sample.cols]) %>%
    normalize.quantiles.in.blocks(., blocks) %>%
    as.data.frame
    df
  }
  else {
    df[1:nrow(df), sample.cols] <- 
    do.call(
      get(norm.function), 
      list(x=as.matrix(df[sample.cols]))
    ) %>%
    as.data.frame
    if (norm.function=="justvsn") {
      df[sample.cols] <- apply(df[sample.cols], 2, function(x) 2^x)
    }
    df
  }
}


# FUNCTION TO AVERAGE PEPTIDES +++++++++++++++++++++++++++++++++++++++++++++++++
#
# define a set of different quantification methods
methods <- list(
  sum=function(x, weight) sum(x, na.rm=TRUE),
  weightedsum=function(x, weight) sum(x*weight, na.rm=TRUE),
  mean=function(x, weight) mean(x, na.rm=TRUE),
  weightedmean=function(x, weight) weighted.mean(x, weight, na.rm=TRUE),
  wgeomean=function(x, weight) exp(weighted.mean(log(x), weight, na.rm=TRUE))
)

# function to aggregate peptide abundances and collect in new df
aggregate.pep <- function(data, sample.cols, weight.threshold, method) {
  ldply(
    by(data, data$protein, function(subdf) {
      # filter peptides by weight threshold, but keep all peptides if no 
      # peptide exceeds weight threshold
      # case 1: all peptides exceed threshold, data taken as is
      if (all(subdf$weight >= weight.threshold)) {
        #cat("") # optional terminal output
      }
      # case 2: some peptides are lower than threshold
      else if (any(subdf$weight >= weight.threshold) & 
        !all(subdf$weight >= weight.threshold)) {
        cat("filtering by weight: ", sum(subdf$weight>=weight.threshold),
          " out of ", nrow(subdf), "accepted \n")
        subdf <- subset(subdf, weight>=weight.threshold)
      }
      else cat("protein ", unique(subdf$protein), " has no valid peptide \n")
      # apply aggregation method per protein, optional weight argument
      apply(subdf[sample.cols], 2, method, weight=subdf$weight)
    })
  )
}


# OPTIONALLY PLOT NORMALIZED INTENSITIES FOR VISUAL INSPECTION
#
plot.function <- function(df, df.norm) {
  
  # reduce data size by random sampling to n points per sample
  if (nrow(df) > 1000) {
    sam = sample(1:nrow(df), 1000)
    cat("Reducing plot size to 1000 data points per sample...\n")
  } else {
    sam = nrow(df.norm)
  }
  # combine original and normalized (reduced) data frame
  rbind(df[sam, ], df.norm[sam, ]) %>% 
  # add serial number and group (original vs normalized)
  mutate(., number=rep(1:length(sam),2)) %>%
  mutate(., normalized=rep(c("not normalized", "normalized"), each=length(sam))) %>%
    
  # gather to long format
  gather(., sample, intensity, -peptide, -protein, -weight, -number, -normalized) %>%
    
  # and plot using lattice
  xyplot(log10(intensity) ~ number | sample, .,
    par.settings=custom.lattice, type="l",
    as.table=TRUE,
    groups=normalized, auto.key=list(columns=2, lines=TRUE, points=FALSE),
    panel=function(x, y, ...) {
      panel.grid(h=-1, v=-1, col=grey(0.9))
      panel.superpose(x, y, ...)
    },
    panel.groups=function(x, y, ...) {
      panel.xyplot(x, sort(y, na.last=TRUE), ...)
    }
  )
}


# MAIN +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# apply optional normalization function
if (norm.function!="none") {
  df.norm <- apply.norm(df, norm.function, sample.cols)
}

# optional plot of normalized versus non-normalized data
if (plot.norm & norm.function != "none") {
  normplot <- plot.function(df, df.norm)
  if (plot.to.png) {
    for (out in output_dir) {
      fullfilename <- paste0(filename, method, ".png")
      png(paste0(out, fullfilename), width=1000, height=1000, res=110)
      print(normplot)
      cat("writing", paste0(out, fullfilename), "...\n")
      dev.off()
    }
  } else
  print(normplot)
}

# apply aggregation with parameters for method, samples, 
# weight cutoff (Diffacto's default is 0.5)
df.result <- aggregate.pep(
  data = {if (norm.function=="none") df else df.norm}, 
  sample.cols, 
  weight.threshold=0.5, 
  method=methods[[method]]
)
  
# change column name
colnames(df.result)[1] <- "protein"

# remove ambiguous entries
df.result <- subset(df.result, !grepl("CONT_|XXX_", protein))
# and replace (near-) zeros with NA
df.result[-1] <- apply(df.result[-1], 2, function(x) replace(x, x<=10, NA))

# save results as CSV file
fullfilename <- paste0(filename, method, ".csv")

for (out in output_dir) {
  cat("writing", paste0(out, fullfilename), "...\n")
  write.csv(df.result, file=paste0(out, fullfilename))
}

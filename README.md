# R-tools
often used R scripts for bio-informatics work and plotting

### custom.panel.functions.R

Some custom R lattice panel functions that extend the lattice plotting capabilities.
For example functions to create error bars on barcharts, only errorbars, and other.

### custom.theme.R

A small collection of custom themes for R lattice plotting.

### GetTopGO.R

Script to use the TopGO gene ontology package. This can be used to apply
a gene enrichment analysis to high throughput gene expression data such as
proteomics or transcriptomics data.

### growth_rate.R

Some functions to determine the growth rate by fitting different models to a growth curve.

### MS_importDiffacto.R

A script containing several functions to
- load proteomics data from DemixQ/Diffacto. This is a matrix of quantified peptides and samples.
- filter NA values
- normalize raw data based on median or quantile normalization
- plot a comparison between normalized and non-normalized data
- aggregate peptides to proteins using one of five different strategies

### MS_modify_keggxml.R

Function to parse a KEGG Brite XML file and convert to a conventional table.
KEGG Brite is a useful hierarchical gene annotation, such as: protein -> sub-pathway -> pathway.

### sectorplot.R

Function to plot sunburst treemaps, a circular hierarchical map, similar to Voronoi treemaps.

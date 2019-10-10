Rtools
================
Michael Jahn,
2019-10-10

-----

Utility and wrapper functions for bioinformatics work

## Description

This package contains utility and wrapper functions that I often use. It
is not intended to be usable by the general public but is maintained as
a package for accessability. Feel free to copy, fork or source functions
you find useful.

## Installation

To install the package directly from github, use this function from
`devtools` package in your R session:

``` r
require(devtools)
devtools::install_github("https://github.com/m-jahn/R-tools")
```

## Utility functions

#### aggregate\_pep

Aggregate peptide abundances to protein abundances

#### apply\_norm

Apply normalization based on different published methods

#### baranyi\_fun

Baranyi growth model

#### custom.ggplot

Custom ggplot2 like theme

#### custom.lattice

Custom grey lattice theme

#### custom\_splom

Custom scatterplot matrix (SPLOM)

#### GetTopGO

Convenience wrapper to TopGO package (Rahnenfueher et al.)

#### gompertzm\_fun

Modified Gompertz growth model

#### mutate\_cond

Conditional mutate(), mutating only selected observations (rows)

#### OD\_corr

Optical density conversion of plate reader to photometer measurements
(OD 600 nm)

#### panel.barplot

Draw barplot with error bars in lattice plots

#### panel.beeswarm

Panel function for beeswarm plots

#### panel.errbars

Calculate and draw error bars in lattice plots

#### panel.key

Draw custom keys in lattice plots

#### panel.pvalue

Calculate and draw p-values in lattice plots

#### panel.quadrants

Draw quadrants and quadrant statistics in lattice plots

#### parse\_kegg\_brite

Parse Kegg Brite xml files step-by-step

#### silhouetteAnalysis

Wrapper function to perform silhouette analysis on all clusters of a

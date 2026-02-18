# OAtools

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL3](https://img.shields.io/badge/license-GPL_3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/github/last-commit/uwvirology-ngs/OAtools.svg)](https://github.com/uwvirology-ngs/OAtools/commits/main)
[![](https://img.shields.io/badge/devel%20version-0.99.10-purple.svg)](https://github.com/uwvirology-ngs/OAtools)
![](https://img.shields.io/badge/R->=%204.6-lightblue.svg)

<!-- badges: end -->

## Overview

OAtools is an R package for analyzing OpenArray gene expression experiments 
motivated by a desire to support open-source and shareable data analyses in 
public health and research environments. 

OAtools offers a public API for the following: 

- converting run data into ergonomic Bioconductor class objects
- analyzing OpenArray experiments by fitting logistic models to PCR curves
- rendering pre-packaged plots to visualize experiment outcomes
- generating an HTML document summarizing run results for reporting

## Installation

You can install the development version of OAtools from the [UW Virology NGS GitHub](https://github.com/uwvirology-ngs):

``` r
# Install the devtools package
install.packages("devtools")

# Install the development version of OAtools from the UW Virology NGS GitHub
devtools::install_github(
    repo = "uwvirology-ngs/OAtools", 
    dependencies = TRUE, 
    build_vignettes = TRUE
)
```

## Documentation

The full documentation for OAtools may be found in the package vignette. Once OAtools is installed, the vignette is accessible by running the following command in the R console: 

``` r
browseVignettes(package = "OAtools")
```

## Example

Here we demonstrate a minimal example of the OAtools workflow. Please refer to 
the package vignette for more comprehensive documentation. 

### Importing Run Data

Once data has been exported in excel format from QuantStudio 12K Flex Software, 
we can load the experiment into a SummarizedExperiment container. 

``` r
# save filepath to example OpenArray gene expression run data
path = system.file(
    "extdata", 
    "oa_gene_expression_1.xlsx", 
    package = "OAtools"
)

# transform the run data into a SummarizedExperiment
se <- excelToSE(excel_path = path)
```

### Analyzing PCR with logistic regressions

Next, we run an optimizer to fit logistic regressions to each amplification 
curve and use the resulting model equation to derive PCR results. A pre-made 
key defines thresholds that separate curves into positive and negative results.

```r
# optimize model curves to each PCR reaction
se <- computeModels(
    se = se,
    assay_name = "fluo_reporter"
)

# save filepath to assay target key
key_path = system.file(
    "extdata", 
    "target_threshold_key.xlsx", 
    package = "OAtools"
)

# assign a PCR result according to the key
se <- determinePCRResults(
    se = se, 
    key_path = key_path
)
```

### Communicating the Results

Finally, we dynamically generate an HTML run report to summarize the outcome 
of the experiment. 

```r
# generate a .html report from the run data
generateReport(se = se)
```

## Interoperability

*OAtools* supports interoperability with the twin packages *ReadqPCR* and 
*NormqPCR*, which normalize RT-qPCR gene expression data from platforms
like OpenArray. Refer to the package vignette for example code or the official
[NormqPCR Documentation](https://www.bioconductor.org/packages/release/bioc/html/NormqPCR.html)
for normalization workflows. 

## Getting Help

For feature suggestions or bug reports, please file an issue on the project [GitHub](https://github.com/uwvirology-ngs/OAtools/issues)

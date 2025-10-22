# OAtools

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/github/last-commit/uwvirology-ngs/OAtools.svg)](https://github.com/uwvirology-ngs/OAtools/commits/main)
[![](https://img.shields.io/badge/devel%20version-0.99.0-purple.svg)](https://github.com/uwvirology-ngs/OAtools)
![](https://img.shields.io/badge/R->=%204.5-lightblue.svg)

<!-- badges: end -->

An R package for wrangling gene expression data generated on Thermo Fisher's OpenArray platform. OAtools streamlines every step of the data analysis pipeline, from initial loading and cleaning to reporting and communication. The motivation behind OAtools is to support the open-source and shareable analaysis of such data in research environments. 

## Installation

You can install the development version of OAtools from [GitHub](https://github.com/):

``` r
# first install pak, which facilitates downloads from GitHub
install.packages("pak")

# then install the development version of OAtools
pak::pak("uwvirology-ngs/OAtools")
```

## Example

Here we run the core OAtools pipeline on the example package data. OAtools first converts raw OpenArray qPCR data into a tidy tibble, then optimizes a model curve to the fluorescence vs. cycle data for each reaction. Reactions may be labeled as positive, inconclusive, or negative depending on comparison of curve features to numeric thresholds, which are described in the target_threshold_key for each assay. Finally, the user may generate a dynamic .html document to serve as a run summary. This example, and further documentation of OAtools, may be found within the package vignette. 

``` r
library(OAtools)

# locates example raw OpenArray qPCR run data
path = system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")

# locates example key pairing assay targets with numeric thresholds
key_path = system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")

# runs the core OAtools pipeline on the example package data
tidy_run_data <- tidy_gene_expression_data(path = path, num_results = 96) |> 
  append_fit_results(linear_threshold = 400) |>     # fit a model curve to each qPCR reaction
  assign_calls_with_key(key_path = key_path)        # interpret results based on model features

# dynamically generates experiment run report (.html)
generate_report(data = tidy_run_data, path = ".", analysis = "curve-fitting")
```

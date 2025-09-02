
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

You can install the development version of OAtools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("uwvirology-ngs/OAtools")
```

## Example

The core analytical pipeline converts raw qPCR data to reportable results in 4 simple function calls. 

``` r
library(OAtools)

# raw qPCR data exported from QuantStudio 12K Flex Software
path = system.file("extdata", "gene_expression_run_data.xlsx", package = "OAtools")

# key associating each assay target with thresholds for interpretation
key_path = system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")

# core analytical pipeline; begins by loading raw data into Tidyverse-friendly tibble
results <- tidy_gene_expression_data(path = path, num_results = 2688) |>
  append_fit_results(linear_threshold = 400) |>         # add model data from curve-fitting with scipy.optimize
  assign_calls_with_key(key_path = key_path) |>         # interpret model data using thresholds from key
  format_results(include_fluorescence_data = FALSE)     # reformat results for export
```

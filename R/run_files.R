#' Run data from a gene expression experiment
#'
#' @description
#' This Excel file (.xlsx) contains an example subset of OpenArray 
#' gene expression run data intended for use with package examples 
#' and unit testing. This dataset includes twelve specimens 
#' tested on four targets.
#'
#' @details
#' Example run data is stored in the `inst/extdata/` directory of the package. 
#' Access this file with:
#'
#' `system.file("extdata", "oa_gene_expression_batch1.xlsx", 
#'              package = "OAtools")`.
#'
#' @format An Excel file (`.xlsx`) with two tabs:
#' \describe{
#'   \item{Multicomponent Data}{Multicomponent fluorescence values for each 
#'   well position and cycle number}
#'   \item{Results}{Run results and QC metrics output by QuantStudio Software}
#' }
#'
#' @source
#' Exported from QuantStudio 12K Flex Software after a gene expression run, 
#' then filtered down to a smaller file size.
#'
#' @keywords
#' data
#'
#' @name
#' oa_gene_expression_batch1
NULL

#' Run data from a second gene expression experiment
#'
#' @description
#' This Excel file (.xlsx) contains an example subset of OpenArray gene 
#' expression run data intended for use with package examples and 
#' unit testing. This dataset includes twelve new specimens tested 
#' separately on the same four targets.
#'
#' @details
#' Example run data is stored in the `inst/extdata/` directory of the package. 
#' Access this file with:
#'
#' `system.file("extdata", "oa_gene_expression_batch2.xlsx", 
#'              package = "OAtools")`.
#'
#' @format An Excel file (`.xlsx`) with two tabs:
#' \describe{
#'   \item{Multicomponent Data}{Multicomponent fluorescence values for each 
#'   well position and cycle number}
#'   \item{Results}{Run results and QC metrics output by QuantStudio Software}
#' }
#'
#' @source
#' Exported from QuantStudio 12K Flex Software after a gene expression run, 
#' then filtered down to a smaller file size.
#'
#' @keywords
#' data
#'
#' @name
#' oa_gene_expression_batch2
NULL

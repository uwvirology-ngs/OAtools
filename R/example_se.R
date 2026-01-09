#' Example OpenArray Gene Expression Data Contained in a SummarizedExperiment
#' 
#' @docType data
#' 
#' @description
#' A sample of OpenArray gene expression data from respiratory tract microbiota
#' profiling experiments conducted at the UW Virology research lab on human 
#' nasal swabs. This file (.rda) stores the PCR data in a SummarizedExperiment
#' container and is intended for use with package examples and unit testing.
#' 
#' @details
#' 
#' Load this object into the environment with: `data(example_se)`
#' 
#' @format a SummarizedExperiment object with the following:
#' \describe{
#'     \item{colData}{information associated with each PCR well}
#'     \item{rowData}{cycle numbers}
#'     \item{assays}{matrices of fluorescence values by PCR well and cycle}
#' }
#' 
#' @source 
#' This object contains PCR data imported from the initial Excel QuantStudio 
#' output. Logistic and linear models were fit to the PCR data and stored as
#' metadata.
#' 
#' The object can be reproduced from the package example data by running 
#' the following commands in the R console:
#' 
#' \preformatted{
#' path <- system.file(
#'     "extdata", 
#'     "oa_gene_expression_1.xlsx", 
#'     package = "OAtools"
#' )
#' 
#' se <- excelToSE(excel_path = path) |> 
#'     computeModels(assay_name = "fluo_normalized") |> 
#'     computeModels(assay_name = "fluo_reporter")
#' }
#' 
#' @keywords
#' data
#' 
#' @name
#' example_se
NULL
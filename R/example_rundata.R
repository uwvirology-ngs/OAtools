#' Example Raw OpenArray Gene Expression Data
#'
#' @docType data
#'
#' @description
#' The first of two Excel files storing run data from separate OpenArray 
#' gene expression experiments. The context behind the experiment was 
#' respiratory tract microbiota profiling on human nasopharyngeal swabs from 
#' patients experiencing respiratory syndromes. 
#'
#' @details
#' Access this file with:
#' `system.file("extdata", "oa_gene_expression_1.xlsx", package = "OAtools")`
#'
#' @format An Excel file (`.xlsx`) with three tabs:
#' \describe{
#'     \item{Amplification Data}{normalized fluorescence by cycle number}
#'     \item{Multicomponent Data}{spectral contribution of the reporter dye
#'     by cycle number}
#'     \item{Results}{PCR results, sample metadata, and QC metrics}
#' }
#'
#' @source
#' This file was exported from QuantStudio 12K Flex Software after a gene 
#' expression run, then filtered down to include 12 samples and 4 genes for 
#' file size concerns. 
#'
#' @keywords
#' data
#'
#' @name
#' oa_gene_expression_1
NULL

#' Example Raw OpenArray Gene Expression Data
#'
#' @docType data
#'
#' @description
#' The second of two Excel files storing run data from separate OpenArray 
#' gene expression experiments. The context behind the experiment was 
#' respiratory tract microbiota profiling on human nasopharyngeal swabs on 
#' patients experiencing respiratory syndromes. 
#'
#' @details
#' Access this file with:
#' `system.file("extdata", "oa_gene_expression_2.xlsx", package = "OAtools")`
#'
#' @format An Excel file (`.xlsx`) with three tabs:
#' \describe{
#'     \item{Amplification Data}{normalized fluorescence by cycle number}
#'     \item{Multicomponent Data}{spectral contribution of the reporter dye
#'     by cycle number}
#'     \item{Results}{PCR results, sample metadata, and QC metrics}
#' }
#'
#' @source
#' This file was exported from QuantStudio 12K Flex Software after a gene 
#' expression run, then filtered down to include 12 samples and 4 genes for 
#' file size concerns. 
#'
#' @keywords
#' data
#'
#' @name
#' oa_gene_expression_2
NULL

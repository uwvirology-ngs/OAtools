#' Target Threshold Key
#'
#' @description
#' This Excel file (.xlsx) contains a table associating each assay target 
#' contained in the example gene expression run data with thresholds values. 
#' This key is optionally used to aid in concurrently interpreting data 
#' including numerous targets with dissimilar behaviors.
#'
#' @details
#' The example key is stored in the `inst/extdata/` directory of the package. 
#' Access this file with:
#'
#' `system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")`.
#'
#' @format An Excel sheet (`.xlsx`) with four columns:
#' \describe{
#'   \item{`target`}{the target assay ID}
#'   \item{`slope_threshold`}{minimum acceptable slope in the exponential 
#'   phase for a positive result}
#'   \item{`delta_threshold`}{minimum acceptable overall change in 
#'   fluorescence for a positive result}
#'   \item{`crt_threshold`}{maximum acceptable crt for a positive result}
#' }
#'
#' @source
#' Written manually for use with resulting functions.
#'
#' @keywords
#' data
#'
#' @name
#' target_threshold_key
NULL

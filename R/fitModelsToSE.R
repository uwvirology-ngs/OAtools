#' Fit Models to SummarizedExperiment
#'
#' This function invokes the run_fit_curve() function for each PCR reaction 
#' contained in the given SummarizedExperiment object, adding parameters and 
#' features of the curves to the dataset. This function expects as input the 
#' result of the excel_to_summarized_experiment() function of the package. 
#' 
#' PCR reactions with a change-in-fluorescence exceeding the linear_threshold 
#' are fit to a logistic regression. To decrease computing time, a curves that 
#' fail to meet this threshold are considered trivially negative and fit instead 
#' to a linear model. 
#'
#' @param se A SummarizedExperiment object with OpenArray fluorescence data
#' @param linear_threshold a numeric defining the minimal change-in-fluorescence 
#' to fit a logistic model. 
#'
#' @returns a SummarizedExperiment
#' 
#' @import SummarizedExperiment
#' 
#' @importFrom rlang .data
#' @importFrom S4Vectors DataFrame
#' 
#' @export
#'
#' @examples
#' path = system.file(
#'     "extdata", 
#'     "oa_gene_expression_batch1.xlsx", 
#'     package = "OAtools"
#' )
#' 
#' se <- excelToSummarizedExperiment(path = path, num_results = 96)
#' se <- fitModelsToSE(se = se, linear_threshold = 500)
fitModelsToSE <- function(se, linear_threshold) {
    
    # input validation
    stopifnot(
        inherits(se, "SummarizedExperiment"),
        "fluo" %in% assayNames(se)
    )
    
    # pull observed fluorescence data for the experiment
    wells <- colnames(se)
    cycles <- rowData(se)$cycle
    fluo <- assay(se, "fluo")
    
    # optimize a model to each well, save as a nested list
    fits <- purrr::map(
        seq_along(wells),
        ~ runFitCurve(
            data.frame(
                cycle = cycles, 
                fam = as.numeric(fluo[, .x])
            ),
            linear_threshold
        )
    )
    
    # build matrix of predicted fluorescence values 
    fluo_pred <- vapply(
        fits,
        function(res) as.numeric(res$y_pred),
        numeric(length(cycles))
    )
    
    # add predicted fluorescence values as an assay to the SummarizedExperiment
    dimnames(fluo_pred) <- list(rownames(se), wells)
    assays(se)$fluo_pred <- fluo_pred
    
    # construct new coldata from model list
    col_results <- S4Vectors::DataFrame(
        regression_type =       vapply(fits, `[[`, character(1), "regression"),
        midpoint_cycle  = round(vapply(fits, `[[`, numeric(1), "x_mid"), 3),
        midpoint_slope  = round(vapply(fits, `[[`, numeric(1), "slope"), 3),
        delta_fluo      = round(vapply(fits, `[[`, numeric(1), "delta"), 3),
        row.names = wells
    )
    
    # append new colData to the SummarizedExperiment
    colData(se) <- cbind(colData(se), col_results)
    
    return(se)
}
#' Fit Models to SummarizedExperiment
#'
#' This function invokes the run_fit_curve() function for each PCR reaction 
#' contained in the given SummarizedExperiment object, adding parameters and 
#' features of the curves to the dataset. This function expects as input the 
#' result of the excel_to_summarized_experiment() function of the package. 
#' 
#' PCR reactions with a change-in-fluorescence exceeding the linear_threshold 
#' are fit to a logistic regression. To decrease computing time, a curves that 
#' fail to meet this threshold are considered trivially negative and fit 
#' instead to a linear model. 
#'
#' @param se A SummarizedExperiment object with OpenArray fluorescence data
#' @param linear_threshold a numeric defining the minimal 
#' change-in-fluorescence to fit a logistic model. 
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
    
    # optimize a model to each well, saving each result to a list
    models <- purrr::map(
        seq_along(wells),
        function(well) {
            runFitCurve(
                data.frame(cycle = cycles, fam = as.numeric(fluo[, well])),
                linear_threshold
            )
        }
    )
    
    # build matrix of predicted fluorescence values 
    fluo_pred_mat <- models |> vapply(
        FUN = function(model) { as.numeric(model$y_pred) },
        FUN.VALUE = numeric(length(cycles))
    )
    dimnames(fluo_pred_mat) <- list(rownames(se), wells)
    
    # add predicted fluorescence values as an assay to the SummarizedExperiment
    assays(se)$fluo_pred <- fluo_pred_mat
    
    # construct new coldata from model list
    model_data <- DataFrame(
        regression_type = vapply(models, `[[`, character(1), "regression"),
        r_squared       = vapply(models, `[[`, numeric(1), "r_squared"),
        midpoint_cycle  = vapply(models, `[[`, numeric(1), "x_mid"),
        midpoint_fluo   = vapply(models, `[[`, numeric(1), "y_mid"),
        midpoint_slope  = vapply(models, `[[`, numeric(1), "slope"),
        delta_fluo      = vapply(models, `[[`, numeric(1), "delta"),
        row.names       = wells
    )
    
    # append new colData to the SummarizedExperiment
    colData(se) <- cbind(colData(se), model_data)
    
    return(se)
}
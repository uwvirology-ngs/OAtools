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
#' @importFrom rlang .data
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
    base::stopifnot(
        base::inherits(se, "SummarizedExperiment"),
        "fluo" %in% SummarizedExperiment::assayNames(se)
    )
    
    # extract well and cycle names
    wells <- base::colnames(se)
    cycles <- SummarizedExperiment::rowData(se)$cycle
    n_cycles <- base::length(cycles)
    
    # setup empty matrix for fluo_pred assay
    fluo_pred <- base::matrix(
        data = NA_real_,
        nrow = n_cycles,
        ncol = base::length(wells),
        dimnames = base::list(base::rownames(se), wells)
    )
    
    # setup empty data frame for new coldata
    col_results <- S4Vectors::DataFrame(
        regression_type = rep(NA_character_, base::length(wells)),
        x_mid = base::rep(NA_real_, base::length(wells)),
        slope = base::rep(NA_real_, base::length(wells)),
        delta = base::rep(NA_real_, base::length(wells)),
        row.names = wells
    )
    
    # for each well
    for (i in base::seq_along(wells)) {
        
        # save output for run_fit_curve() function
        result <- runFitCurve(
            data.frame(
                cycle = cycles, 
                fam = base::as.numeric(
                    SummarizedExperiment::assay(se, "fluo")[, i]
                )
            ),
            linear_threshold
        )
        
        # fill in assay matrix
        fluo_pred[, i] <- base::as.numeric(result$y_pred)
        
        # fill in new coldata
        col_results$regression_type[i] <- result$regression
        col_results$x_mid[i] <- base::round(result$x_mid, 3)
        col_results$slope[i] <- base::round(result$slope, 3)
        col_results$delta[i] <- base::round(result$delta, 3)
    }
    
    # attach results to SummarizedExperiment
    SummarizedExperiment::assays(se)$fluo_pred <- fluo_pred
    SummarizedExperiment::colData(se) <- base::cbind(
        SummarizedExperiment::colData(se),
        col_results
    )
    
    return(se)
}
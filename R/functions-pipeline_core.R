utils::globalVariables("fit_curve")

#' Run curve-fitting function
#'
#' An R wrapper for the internal python function, fit_curve(), which attempts 
#' to fit two-dimensional multicomponent fluorescence vs. cycle number data to 
#' a 5-parameter logistic regression. For reactions with a total change in 
#' fluorescence below the linear_threshold, returns instead a linear regression
#'
#' @param data A tibble of multicomponent fluorescence data vs. 
#' cycle number for one reaction
#' @param linear_threshold An integer describing the maximum overall 
#' change in fluorescence at which fluorescence curves will be trivially 
#' fit to a linear regression
#'
#' @returns A list object describing the model
#' @export
#'
#' @examples
run_fit_curve <- function(data, linear_threshold) {
    basilisk::basiliskRun(
        env = OAtools_env,
        fun = function() {
            reticulate::source_python(system.file(
                "python", 
                "fit_curve.py", 
                package = "OAtools"
            ))
            return(fit_curve(data, linear_threshold))
        }
    )
}

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
#' se <- excel_to_summarized_experiment(path = path, num_results = 96)
#' se2 <- fit_models_to_se(se = se, linear_threshold = 500)
fit_models_to_se <- function(se, linear_threshold) {
    
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
        result <- run_fit_curve(
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

#' Assign Final Results to Each Well
#' 
#' Assigns positive or negative results to every PCR reaction depending on the 
#' features of the model curve and the thresholds defined for the relevant 
#' gene. These calls are appended to the colData of the SummarizedExperiment. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR Data
#' @param key_path file path to the target-threshold key
#'
#' @returns a SummarizedExperiment
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' key_path = system.file(
#'     "extdata", 
#'     "target_threshold_key.xlsx", 
#'     package = "OAtools"
#' )
#' 
#' se <- example_se |> 
#'     fit_models_to_se(linear_threshold = 500) |> 
#'     assign_calls_to_se(key_path = key_path)
assign_calls_to_se <- function(se, key_path) {
    
    # load in coldata
    data <- as.data.frame(SummarizedExperiment::colData(se))
    
    # generate key to associate targets with thresholds
    key <- readxl::read_excel(path = key_path, na = "NA") |>
        janitor::clean_names()
    
    key <- key |>
        dplyr::mutate(target = as.factor(.data$target))
    
    # join the key to the tibble
    data <- data |>
        dplyr::left_join(key, by = c("target_name" = "target"))
    
    # apply the rules of the key and append the results to the tibble
    results <- data |>
        dplyr::mutate(
            crt_pass   = is.na(.data$crt_threshold) | 
                (!is.na(.data$crt) & .data$crt < .data$crt_threshold),
            
            slope_pass = is.na(.data$slope_threshold) | 
                .data$slope > .data$slope_threshold,
            
            delta_pass = is.na(.data$delta_threshold) | 
                .data$delta > .data$delta_threshold,
            
            result = dplyr::case_when(
                is.na(.data$crt_threshold) & 
                    is.na(.data$slope_threshold) & 
                    is.na(.data$delta_threshold) ~ "TBD",
                
                crt_pass & slope_pass & delta_pass ~ "positive",
                
                TRUE ~ "negative"
            )
        )
    
    SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(results)
    
    return(se)
}
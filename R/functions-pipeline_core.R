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
#' data(tidy_run_data)
#' data <- dplyr::filter(tidy_run_data, well == 2794)
#' 
#' curve <- run_fit_curve(data, linear_threshold = 400)
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

#' Append fit results
#'
#' This function separately calls the run_fit_curve() function on all 
#' reactions contained in a tibble and appends the model curve parameters 
#' and features to the data set. This workflow enables batch computation 
#' for model curves optimized to the qPCR data.
#'
#' @param data A tibble containing multicomponent fluorescence 'fam' and 
#' cycle number 'cycle' data separated by well_position and batch_name
#' @param linear_threshold A numeric defining the minimal overall 
#' change-in-fluorescence required to attempt to optimize to a 
#' 5-parameter logistic regression
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data(tidy_run_data)
#' filt <- tidy_run_data |> dplyr::filter(sample_name == "Sample-102")
#' 
#' curve_fit_data <- append_fit_results(data = filt, linear_threshold = 400)
append_fit_results <- function(data, linear_threshold) {
    
    # define wrapper function to append curve-fitting results
    wrapper <- function(df) {
        result <- run_fit_curve(df, linear_threshold)
        
        df$regression_type <- result$regression   # append model type
        
        fam_pred <- as.numeric(result$y_pred)     # append predicted fam
        if (length(fam_pred) != nrow(df)) {
            stop(
                "Array length returned by fit_curve() mismatches nrows ",
                "in data frame passed to wrapper."
            )
        }
        df$fam_pred <- as.numeric(result$y_pred)
        
        df$x_mid <- round(result$x_mid, 3)        # append midpoint data
        df$slope <- round(result$slope, 3)        # append slope data
        df$delta <- round(result$delta, 3)        # append change-in-fam
        return(df)
    }
    
    # apply wrapper function to each unique well_position and batch_name
    data <- data |>
        dplyr::group_by(.data$well_position, .data$batch_name) |>
        dplyr::group_split() |>
        purrr::map(wrapper) |>
        purrr::list_rbind() |>
        dplyr::arrange(.data$batch_name, .data$well)
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


#' Assign calls with key
#'
#' Assigns positive and negative calls to each reaction depending on how the 
#' features of the model regression compare to the thresholds defined for the 
#' respective assay target. Appends these calls to the data frame and 
#' returns the modified tibble.
#'
#' @param data A tibble containing qPCR and model curve data for resulting
#' @param key_path file path to the target-threshold key associating thresholds
#' for each assay target
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data(curve_fit_data)
#' key_path = system.file(
#'   "extdata", 
#'   "target_threshold_key.xlsx", 
#'   package = "OAtools"
#' )
#' 
#' result_data <- assign_calls_with_key(
#'   data = curve_fit_data, 
#'   key_path = key_path
#' )
assign_calls_with_key <- function(data, key_path) {
    
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
    
    return(results)
}

#' Format results for reporting
#'
#' Reformats analyzed OpenArray qPCR data, converting the long tidy tibble 
#' into a wider, human-readable one in which each row reflects one reaction. 
#' Fluorescence data for each cycle may be optionally included.
#'
#' @param data A tibble containing analyzed qPCR data
#' @param include_fluorescence_data logical determining whether fluorescence 
#' vs. cycle data is included
#'
#' @returns A reformatted tibble for reporting
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data(curve_fit_data)
#' key_path = system.file(
#'   "extdata", 
#'   "target_threshold_key.xlsx", 
#'   package = "OAtools"
#' )
#' result_data <- assign_calls_with_key(
#'   data = curve_fit_data, 
#'   key_path = key_path
#' )
#' 
#' formatted_data <- format_results(
#'   data = result_data, 
#'   include_fluorescence_data = TRUE
#' )
format_results <- function(data, include_fluorescence_data) {
    
    if (include_fluorescence_data) {
        
        fluorescence_cols <- intersect(c("fam", "fam_pred"), names(data))
        
        data <- data |>
            tidyr::pivot_wider(
                names_from = "cycle",
                values_from = tidyselect::all_of(fluorescence_cols),
                names_glue = "{.value}_cycle{cycle}"
            )
        
    } else {
        data <- data |>
            dplyr::distinct(.keep_all = TRUE) |>
            dplyr::select(-dplyr::matches("fam|cycle"))
    }
    
    return(data)
}

#' Determine PCR results from fit curves
#' 
#' Assigns positive or negative PCR results to each reaction depending on the
#' equation of the model optimized to observed multicomponent fluorescence 
#' values. The reporter dye fluorescence without normalization is used to 
#' distinguish between real amplification and false positives.
#' 
#' @details
#' The key is an Excel file storing values used by `determinePCRResults()` to 
#' categorize PCR curves as `positive` or `negative`. In the key, each gene is 
#' associated with threshold values for Crt, change-in-fluorescence, and slope
#' at the reaction midpoint.
#' 
#' For more specifics, input `?target_threshold_key` into the R console. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR Data
#' @param key_path file path to the target-threshold key
#'
#' @returns a SummarizedExperiment
#' 
#' @import SummarizedExperiment
#' 
#' @importFrom rlang .data
#' @importFrom readxl read_excel
#' @importFrom S4Vectors DataFrame
#' @importFrom purrr map_dbl map_chr
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' 
#' key_path = system.file(
#'     "extdata", 
#'     "target_threshold_key.xlsx", 
#'     package = "OAtools"
#' )
#' 
#' se <- example_se |> 
#'     determinePCRResults(key_path = key_path)
determinePCRResults <- function(se, key_path) {
    
    # pull critical curve fitting metadata and append to colData
    models <- metadata(se)$fluo_reporter_models
    
    data <- as.data.frame(colData(se)) |>
        dplyr::mutate(regression_type = map_chr(
            paste0("well_", .data$well),
            function(well) { models[[well]]$regression_type }
        )) |> 
        dplyr::mutate(r_squared = map_dbl(
            paste0("well_", .data$well),
            function(well) { models[[well]]$r_squared }
        )) |> 
        dplyr::mutate(delta_fluo = map_dbl(
            paste0("well_", .data$well), 
            function(well) { models[[well]]$delta_y }
        )) |> 
        dplyr::mutate(midpoint_slope = map_dbl(
            paste0("well_", .data$well), 
            function(well) { models[[well]]$slope_midpoint }
        ))

    # merge experiment colData with thresholds defined by key
    key <- read_excel(path = key_path, na = "NA") |> 
        janitor::clean_names() |> 
        dplyr::mutate(target = as.factor(.data$target))
    
    data <- data |> dplyr::left_join(key, by = c("target_name" = "target"))
    
    # determine PCR results and append to the colData
    results <- .applyResults(data)    
    colData(se) <- DataFrame(results)
    
    return(se)
}

# adds PCR results to the tibble, applying thresholds defined by the key
.applyResults <- function(data) {
    
    # add pass/fail cols for crt, midpoint slope, and change-in-fluorescence,
    # then require positive results to pass on all three metrics. if no 
    # threshold values are defined at all, result is TBD.
    results <- data |>
        dplyr::mutate(
            crt_pass   = is.na(.data$crt_threshold) | 
                (!is.na(.data$crt) & .data$crt < .data$crt_threshold),

            slope_pass = is.na(.data$slope_threshold) | 
                .data$midpoint_slope > .data$slope_threshold,
            
            delta_pass = is.na(.data$delta_threshold) | 
                .data$delta_fluo > .data$delta_threshold,
            
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
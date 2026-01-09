#' Determine PCR results from features of multicomponent fluorescence curves
#' 
#' Assigns positive or negative PCR results to every PCR reaction depending on
#' the equation of the model curve optimized to the fluorescence curve. In 
#' particular, the reporter dye fluorescence without normalization is used to 
#' distinguish between real amplification and false positives. 
#' 
#' @details
#' The target-threshold key is an Excel file which stores threshold values 
#' used to separate PCR curves. These thresholds are defined across metrics 
#' such as cycle threshold, overall change-in-fluorescence, and slope of the 
#' fluorescence curve during the exponential phase. The thresholds are 
#' separately defined for each gene to accommodate inherent differences in
#' assays.
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR Data
#' @param key_path file path to the target-threshold key
#'
#' @returns a SummarizedExperiment
#' 
#' @import SummarizedExperiment
#' 
#' @importFrom rlang .data
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
    
    # import target-threshold key
    key <- readxl::read_excel(path = key_path, na = "NA") |> 
        janitor::clean_names() |> 
        dplyr::mutate(target = as.factor(.data$target))
    
    # load in models
    models <- metadata(se)$fluo_reporter_models
    
    # merge existing coldata with thresholds and model features
    data <- as.data.frame(colData(se)) |>
        dplyr::left_join(key, by = c("target_name" = "target")) |> 
        dplyr::mutate(midpoint_slope = purrr::map_dbl(
            paste0("well_", .data$well), function(well) { models[[well]]$slope }
        )) |> 
        dplyr::mutate(delta_fluo = purrr::map_dbl(
            paste0("well_", .data$well), function(well) { models[[well]]$delta }
        ))
    
    # apply the rules of the key and append the results to the tibble
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
    
    SummarizedExperiment::colData(se) <- S4Vectors::DataFrame(results)
    
    return(se)
}
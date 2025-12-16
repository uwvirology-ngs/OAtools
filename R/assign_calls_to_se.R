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
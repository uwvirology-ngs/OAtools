#' Assign calls with key
#'
#' Assigns positive and negative calls to each reaction depending on how the features
#' of the model regression compare to the thresholds defined for the respective assay target.
#' Appends these calls to the data frame and returns the modified tibble.
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
#' key_path = system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")
#' results <- assign_calls_with_key(data = curve_fit_data, key_path = key_path)
assign_calls_with_key <- function(data, key_path) {
  key <- readxl::read_excel(path = key_path, na = "NA") |>
    janitor::clean_names()

  data <- data |>
    dplyr::left_join(key, by = c("target_name" = "target"))

  results <- data |>
    dplyr::mutate(
      crt_pass   = is.na(.data$crt_threshold) | (!is.na(.data$crt) & .data$crt < .data$crt_threshold),
      slope_pass = is.na(.data$slope_threshold) | .data$slope > .data$slope_threshold,
      delta_pass = is.na(.data$delta_threshold) | .data$delta > .data$delta_threshold,
      all_thresh_na = is.na(.data$crt_threshold) & is.na(.data$slope_threshold) & is.na(.data$delta_threshold),
      result = dplyr::case_when(
        all_thresh_na ~ "TBD",
        crt_pass & slope_pass & delta_pass ~ "positive",
        TRUE ~ "negative"
      )
    ) |>
    dplyr::select(-.data$all_thresh_na)

  return(results)
}

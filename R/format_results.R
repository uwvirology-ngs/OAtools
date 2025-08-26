#' Format results for reporting
#'
#' Reformats analyzed OpenArray qPCR data, converting the long tidy tibble into a wider,
#' human-readable one in which each row reflects one reaction. Fluorescence data for each
#' cycle may be optionally included.
#'
#' @param data A tibble containing analyzed qPCR data
#' @param include_fluorescence_data logical determining whether fluorescence vs. cycle data is included
#'
#' @returns A reformatted tibble for reporting
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' formatted_results <- format_results(data = result_data, include_fluorescence_data = TRUE)

format_results <- function(data, include_fluorescence_data) {

  if (include_fluorescence_data) {

    fluorescence_cols <- base::intersect(c("fam", "fam_pred"), base::names(data))

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

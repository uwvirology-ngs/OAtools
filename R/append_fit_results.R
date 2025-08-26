#' Append fit results
#'
#' This function separately calls the run_fit_curve() function on all reactions contained
#' in a tibble and appends the model curve parameters and features to the data set. This
#' workflow enables batch computation for model curves optimized to the qPCR data.
#'
#' @param data A tibble containing multicomponent fluorescence 'fam' and cycle number 'cycle' data
#' separated by well_position and batch_name
#' @param linear_threshold A numeric defining the minimal overall change-in-fluorescence required to
#' attempt to optimize to a 5-parameter logistic regression
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data <- tidy_run_data |> dplyr::filter(target_name == "S. pneumoniae_Ba06439619_s1")
#' curve_fit_data <- append_fit_results(data = data, linear_threshold = 400)
append_fit_results <- function(data, linear_threshold) {

  # define wrapper function to append curve-fitting results
  wrapper <- function(df) {
    result <- run_fit_curve(df, linear_threshold)

    df$regression_type <- result$regression       # append model type

    fam_pred <- as.numeric(result$y_pred)         # append fluorescence values predicted by model
    if (length(fam_pred) != nrow(df)) {
      stop("Array length returned by fit_curve() mismatches nrows in data frame passed to wrapper.")
    }
    df$fam_pred <- as.numeric(result$y_pred)

    df$x_mid <- round(result$x_mid, 3)            # append midpoint data
    df$slope <- round(result$slope, 3)            # append slope data
    df$delta <- round(result$delta, 3)            # append change-in-fluorescence data
    return(df)
  }

  # apply wrapper function to each unique combination of well_position and batch_name
  data <- data |>
    dplyr::group_by(.data$well_position, .data$batch_name) |>
    dplyr::group_split() |>
    purrr::map(wrapper) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$batch_name, .data$well)
}

utils::globalVariables("fit_curve")

#' Run curve-fitting function
#'
#' An R wrapper for the internal python function, fit_curve(), which attempts to fit two-dimensional
#' multicomponent fluorescence vs. cycle number data to a 5-parameter logistic regression. For reactions
#' with a total change in fluorescence below the linear_threshold, returns instead
#'
#' @param data A tibble of multicomponent fluorescence data vs. cycle number for one reaction
#' @param linear_threshold An integer describing the maximum overall change in fluorescence at which fluroescence curves will trivially be fitted to a linear regression
#'
#' @returns A list object describing the model
#' @export
#'
#' @examples
#' data <- dplyr::filter(tidy_run_data, well == 2794)
#' curve <- run_fit_curve(data, linear_threshold = 400)
run_fit_curve <- function(data, linear_threshold) {
  basilisk::basiliskRun(
    env = OAtools_env,
    fun = function() {
      reticulate::source_python(system.file("python", "fit_curve.py", package = "OAtools"))
      return(fit_curve(data, linear_threshold))
    }
  )
}

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
#' filt <- tidy_run_data |> dplyr::filter(sample_name == "Sample-102")
#' curve_fit_data <- append_fit_results(data = filt, linear_threshold = 400)
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
#' result_data <- assign_calls_with_key(data = curve_fit_data, key_path = key_path)
assign_calls_with_key <- function(data, key_path) {
  key <- readxl::read_excel(path = key_path, na = "NA") |>
    janitor::clean_names()

  key <- key |>
    dplyr::mutate(target = as.factor(.data$target))

  data <- data |>
    dplyr::left_join(key, by = c("target_name" = "target"))

  results <- data |>
    dplyr::mutate(
      crt_pass   = is.na(.data$crt_threshold) | (!is.na(.data$crt) & .data$crt < .data$crt_threshold),
      slope_pass = is.na(.data$slope_threshold) | .data$slope > .data$slope_threshold,
      delta_pass = is.na(.data$delta_threshold) | .data$delta > .data$delta_threshold,
      result = dplyr::case_when(
        is.na(.data$crt_threshold) & is.na(.data$slope_threshold) & is.na(.data$delta_threshold) ~ "TBD",
        crt_pass & slope_pass & delta_pass ~ "positive",
        TRUE ~ "negative"
      )
    )

  return(results)
}

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
#' key_path = system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")
#' result_data <- assign_calls_with_key(data = curve_fit_data, key_path = key_path)
#' formatted_data <- format_results(data = result_data, include_fluorescence_data = TRUE)
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

test_that("append_fit_results returns expected results", {

  # create tidy_run_data as in function documentation
  targets <- c("S. pneumoniae_Ba06439619_s1", "RNAse_P_Pa04930436_g1")
  data <- tidy_run_data_cumulative |> dplyr::filter(target_name %in% targets)
  result <- append_fit_results(data = data, linear_threshold = 400)

  # verify function return type
  expect_true(tibble::is_tibble(result))

  # test column names
  new_cols <- c("regression_type", "x_mid", "slope", "delta")
  expect_true(all(new_cols %in% colnames(result)))

  # test column types
  expected_types <- c(
    regression_type = "character",
    x_mid = "numeric",
    slope = "numeric",
    delta = "numeric"
  )
  observed_types <- sapply(result, function(col) class(col)[1])
  expect_equal(observed_types[names(expected_types)], expected_types)

  # ensure result is equivalent to example package data
  expect_equal(result, curve_fit_data, tolerance = 1e-3)

})

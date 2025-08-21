test_that("append_fit_results returns expected results", {

  # create tidy_run_data as in function documentation
  key_path = system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")
  result <- assign_calls_with_key(data = curve_fit_data, key_path = key_path)

  # verify function return type
  expect_true(tibble::is_tibble(result))

  # test column names
  new_cols <- c("slope_threshold", "delta_threshold", "crt_threshold", "slope_pass", "delta_pass", "crt_pass", "result")
  expect_true(all(new_cols %in% colnames(result)))

  # test column types
  expected_types <- c(
    slope_threshold = "numeric",
    delta_threshold = "numeric",
    crt_threshold = "numeric",
    slope_pass = "logical",
    delta_pass = "logical",
    crt_pass = "logical",
    result = "character"
  )
  observed_types <- sapply(result, function(col) class(col)[1])
  expect_equal(observed_types[names(expected_types)], expected_types)

  # ensure result is identical to example package data
  expect_identical(result, result_data)

})

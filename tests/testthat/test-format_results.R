test_that("format_results returns expected results", {

  # create formatted_data as in function documentation
  result <- format_results(data = result_data, include_fluorescence_data = TRUE)

  # verify function return type
  expect_true(tibble::is_tibble(result))

  # ensure result is identical to example package data
  expect_identical(result, formatted_data)

})

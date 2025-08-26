test_that("tidy_gene_expression_data returns expected results", {

  # create tidy_run_data as in function documentation
  path = system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")
  result <- tidy_gene_expression_data(path = path, num_results = 96)

  # verify function return type
  expect_true(tibble::is_tibble(result))

  # test column names
  expect_equal(
    colnames(result),
    c("well", "well_position", "sample_name", "target_name", "crt", "amp_score",
      "cq_conf", "amp_status", "cycle", "fam", "batch_name")
  )

  # test column types
  expected_types <- c(
    well = "integer",
    well_position = "factor",
    sample_name = "factor",
    target_name = "factor",
    crt = "numeric",
    amp_score = "numeric",
    cq_conf = "numeric",
    amp_status = "factor",
    cycle = "integer",
    fam = "numeric",
    batch_name = "factor"
  )
  observed_types <- sapply(result, function(col) class(col)[1])
  expect_equal(observed_types, expected_types)

  # ensure result is identical to example package data
  expect_identical(result, tidy_run_data)

})

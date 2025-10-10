test_that("fn returns tibble with expected columns and coltypes", {
  
  # load in example OpenArray data
  path <- system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")
  result <- tidy_gene_expression_data(path = path, num_results = 96)
  
  # validate return type
  expect_true(tibble::is_tibble(result))
  
  # validate column names
  expected_cols <- c(
    "well", "well_position", "sample_name", "target_name", "crt", 
    "amp_score", "cq_conf", "amp_status", "cycle", "fam", "batch_name"
  )
  expect_true(all(expected_cols %in% colnames(result)))
  
  # validate column types
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
  
  # validate number of rows
  expect_equal(nrow(result), 96 * 40)
})

test_that("fn throws error when metadata inappropriately included in tibble", {
  path <- system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")
  expect_error(tidy_gene_expression_data(path = path, num_results = 97), regexp = "Well column is not numeric*")
})

test_that("append_fit_results has expected return type and acceptably fits 5pl model", {

  # generate fit data for unit testing
  path <- system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")
  targets <- c("RV_1of2_Vi99990016_po", "RNAse_P_Pa04930436_g1", "S. pneumoniae_Ba06439619_s1")
  linear_threshold <- 400
  
  result <- tidy_gene_expression_data(path = path, num_results = 96) |> 
    dplyr::filter(sample_name == "Sample-102" & target_name %in% targets) |> 
    append_fit_results(linear_threshold = linear_threshold)

  # validate return type
  expect_true(tibble::is_tibble(result))

  # validate column names
  expected_cols <- c("regression_type", "x_mid", "slope", "delta")
  expect_true(all(expected_cols %in% colnames(result)))

  # validate column types
  expected_types <- c(
    regression_type = "character",
    x_mid = "numeric",
    slope = "numeric",
    delta = "numeric"
  )
  observed_types <- sapply(result, function(col) class(col)[1])
  expect_equal(observed_types[names(expected_types)], expected_types)
  
  # validate regression type
  df <- result |> dplyr::distinct(well, batch_name, .keep_all = TRUE)
  expect_true(all(
    (df$delta >= 400 & df$regression_type == "5pl") | 
    (df$delta < 400 & df$regression_type == "lin")
  ))
  
  # ensure quality of 5pl fit
  df <- result |> dplyr::filter(well == "2385")
  mse <- mean((df$fam - df$fam_pred)^2)
  expect_true(all(df$regression_type == "5pl"))
  expect_lt(mse, 2500)
  
})

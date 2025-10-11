  test_that("assign_calls_with_key returns expected results", {
  
    # assign_calls_with_key has expected return type
    data_path <- system.file("extdata", "oa_gene_expression_batch1.xlsx", package = "OAtools")
    key_path <- system.file("extdata", "target_threshold_key.xlsx", package = "OAtools")
    targets <- c("RV_1of2_Vi99990016_po", "S. pneumoniae_Ba06439619_s1")
    
    result <- tidy_gene_expression_data(path = data_path, num_results = 96) |> 
      dplyr::filter(sample_name == "Sample-102") |> 
      dplyr::filter(target_name %in% targets) |> 
      append_fit_results(linear_threshold = 400) |> 
      assign_calls_with_key(key_path = key_path)
  
    # validate return type
    expect_true(tibble::is_tibble(result))
  
    # validate column names
    new_cols <- c("slope_threshold", "delta_threshold", "crt_threshold", "slope_pass", "delta_pass", "crt_pass", "result")
    expect_true(all(new_cols %in% colnames(result)))
  
    # validate column types
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
    
    # confirm wells are assigned correct results
    expect_true(all(
      (result$target_name == "RV_1of2_Vi99990016_po" & result$result == "positive") |
      (result$target_name == "S. pneumoniae_Ba06439619_s1" & result$result == "negative")
    ))
  
  })
# Setup -------------------------------------------------------------------

data(example_se)

key_path = system.file(
    "extdata", 
    "target_threshold_key.xlsx", 
    package = "OAtools"
)

se <- example_se |> 
    determinePCRResults(key_path = key_path)

col_data <- colData(se)


# Data Structure ----------------------------------------------------------

test_that("required columns are added", {
    required_columns <- c(
        "run_date", "regression_type", "r_squared", 
        "delta_fluo", "midpoint_slope"
    )
    expect_true(all(required_columns %in% colnames(colData(se))))
})

# Positive Result ---------------------------------------------------------
test_that("well with no amplification is marked negative", {
    expect_equal(col_data[col_data$well == "2321", "result"], "negative")
})

# Negative Result ---------------------------------------------------------
test_that("well with strong amplification is marked positive", {
    expect_equal(col_data[col_data$well == "2778", "result"], "positive")
})
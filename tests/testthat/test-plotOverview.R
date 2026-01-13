# Setup -------------------------------------------------------------------

data(example_se)

fig <- plotOverview(example_se)

# tests -------------------------------------------------------------------

test_that("figure is a ggplot", {
    expect_s3_class(fig, "ggplot")
})

test_that("figure is composed of expected geoms", {
    expect_true("geom_tile" %in% names(fig$layers))
})

test_that("figure data has expected colnames", {
    expected_colnames <- c("sample_name", "target_name", "category")
    expect_true(all(expected_colnames %in% colnames(fig$data)))
})

test_that("control is positive for all antigens with correct factor levels", {
    control_data <- fig$data |> dplyr::filter(sample_name == "Pos_Control_A")
    expected_levels <- c("Positive", "Inconclusive", "Negative")
        
    expect_true(all(expected_levels %in% levels(control_data$category)))
    expect_true(all(control_data$category == "Positive"))
})



# Setup -------------------------------------------------------------------

data(example_se)

plot <- plotQC(example_se)

# tests -------------------------------------------------------------------

test_that("plot is a plotly graphic", {
    expect_s3_class(plot, "plotly")
})
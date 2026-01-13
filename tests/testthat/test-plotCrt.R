# Setup -------------------------------------------------------------------

data(example_se)

fig <- plotCrt(example_se)

# tests -------------------------------------------------------------------

test_that("figure is a ggplot", {
    expect_s3_class(fig, "ggplot")
})

test_that("figure is composed of expected geoms", {
    expect_true(all(c("geom_boxplot", "geom_point") %in% names(fig$layers)))
})

test_that("crt column is numeric", {
    expect_true(is.numeric(fig$data$crt))
})
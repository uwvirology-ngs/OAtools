# Setup -------------------------------------------------------------------

data(example_se)

fig <- plotModel(
    example_se, 
    well_id = "well_2665", 
    assay_name = "fluo_reporter",
    include_mdpt_tangent = TRUE,
    include_coldata_annotation = TRUE
)

# Tests -------------------------------------------------------------------

test_that("figure is a ggplot", {
    expect_s3_class(fig, "ggplot")
})

test_that("figure is composed of expected geoms", {
    expect_true(
        all(c("geom_point", "geom_line", "geom_abline") %in% names(fig$layers))
    )
})

test_that("underlying data has expected colnames", {
    expect_true(
        all(c("cycle", "fluo", "fluo_pred") %in% colnames(fig$data))
    )
})

test_that("underlying data has expected length", {
    expect_equal(nrow(fig$data), nrow(assay(example_se)))
})

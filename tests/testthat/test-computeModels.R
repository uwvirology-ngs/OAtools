# Setup -------------------------------------------------------------------

path <- system.file(
    "extdata", 
    "oa_gene_expression_1.xlsx", 
    package = "OAtools"
)

se <- excelToSE(excel_path = path) |> 
    computeModels(assay_name = "fluo_normalized") |> 
    computeModels(assay_name = "fluo_reporter")

# model storage -----------------------------------------------------------

test_that("models are stored in metadata", {
    expect_true("fluo_normalized_models" %in% names(metadata(se)))
    expect_true("fluo_reporter_models" %in% names(metadata(se)))
})

test_that("models map to PCR wells", {
    expect_equal(names(metadata(se)$fluo_normalized_models), colnames(se))
    expect_equal(names(metadata(se)$fluo_reporter_models), colnames(se))
})

# model structure ---------------------------------------------------------

test_that("models store the correct fields and data types", {
    model_5pl <- metadata(se)$fluo_normalized_models[["well_2665"]]
    
    expected_fields <- c("regression_type", "r_squared", "delta_y", 
                         "x_obs", "y_obs", "y_pred", "parameters", 
                         "x_midpoint", "y_midpoint", "slope_midpoint")
    
    expect_true(all(expected_fields %in% names(model_5pl)))
    
    expect_true(is.character(model_5pl$regression_type))
    expect_true(is.numeric(model_5pl$r_squared))
    expect_true(is.numeric(model_5pl$delta_y))
    expect_true(is.numeric(model_5pl$x_obs))
    expect_true(is.numeric(model_5pl$y_obs))
    expect_true(is.numeric(model_5pl$y_pred))
    expect_true(is.list(model_5pl$parameters))
    expect_true(is.numeric(model_5pl$x_midpoint))
    expect_true(is.numeric(model_5pl$y_midpoint))
    expect_true(is.numeric(model_5pl$slope_midpoint))
})
# Setup -------------------------------------------------------------------

path = system.file(
    "extdata", 
    "oa_gene_expression_1.xlsx", 
    package = "OAtools"
)

se <- excelToSE(excel_path = path)

cycles <- rowData(se)$cycle
fluo <- assay(se, "fluo_reporter")

# Negative PCR Reaction ---------------------------------------------------

model_for_2329 <- .runFitCurve(
    pcr_data = data.frame(
        cycle = cycles, 
        fluo = as.numeric(fluo[, "well_2329"])
    ),
    linear_threshold = 400
)

test_that("total change in fluorescence below linear threshold", {
    expect_true(model_for_2329$delta_y < 400)
})

test_that("linear model computed for trivially negative PCR curve", {
    expect_equal(model_for_2329$regression_type, "lin")
})

# Weak Amplification ------------------------------------------------------

model_for_2713 <- .runFitCurve(
    pcr_data = data.frame(
        cycle = cycles, 
        fluo = as.numeric(fluo[, "well_2713"])
    ),
    linear_threshold = 400
)

test_that("r_squared value is acceptable", {
    expect_true(model_for_2713$r_squared > 0.99)
})

test_that("5pl model computed for weak amplification PCR curve", {
    expect_equal(model_for_2713$regression_type, "5pl")
})

# Strong Amplification ----------------------------------------------------

model_for_2778 <- .runFitCurve(
    pcr_data = data.frame(
        cycle = cycles, 
        fluo = as.numeric(fluo[, "well_2778"])
    ),
    linear_threshold = 400
)

test_that("r_squared value is acceptable", {
    expect_true(model_for_2778$r_squared > 0.99)
})

test_that("5pl model computed for strong amplification PCR curve", {
    expect_equal(model_for_2778$regression_type, "5pl")
})
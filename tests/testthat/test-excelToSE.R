# Setup -------------------------------------------------------------------

path <- system.file(
    "extdata", 
    "oa_gene_expression_1.xlsx", 
    package = "OAtools"
)

se <- excelToSE(excel_path = path)

# Baseline functionality --------------------------------------------------

test_that("excelToSE fails when provided an incorrect file path", {
    expect_error(excelToSE(excel_path = "does_not_exist.xlsx"))
})

test_that("excelToSE returns a valid SummarizedExperiment object", {
    expect_s4_class(se, "SummarizedExperiment")
})

# SE data structure - assays ----------------------------------------------

test_that("SE object stores fluorescence data in assays list", {
    expect_true("fluo_normalized" %in% assayNames(se))
    expect_true("fluo_reporter" %in% assayNames(se))
})

test_that("matrix dimensions are consistent across assays", {
    expect_equal(
        dim(assay(se, "fluo_normalized")),
        dim(assay(se, "fluo_reporter"))
    )
})

test_that("rownames (cycles) and colnames (wells) have expected naming", {
    expect_true(all(grepl("^cycle_\\d+$", rownames(se))))
    expect_true(all(grepl("^well_\\d+$", colnames(se))))
})

# SE data structure - colData ---------------------------------------------

test_that("colData contains the expected columns", {
    required_columns <- c(
        "well", "well_position", "omit", "sample_name", "target_name", 
        "task", "reporter", "quencher", "crt", "crt_mean", "crt_sd",
        "amp_score", "cq_conf", "amp_status", "highsd", "rox_signal"
    )
    expect_true(all(required_columns %in% colnames(colData(se))))
})

test_that("colData rows are equal in number to assay columns", {
    expect_equal(nrow(colData(se)), ncol(assay(se, "fluo_normalized")))
})

test_that("PCR wells are properly sorted and uniquely defined", {
    expect_true(all(colData(se)$well == sort(colData(se)$well)))
    expect_false(any(duplicated(colData(se)$well)))
})

# SE data structure - rowData ---------------------------------------------

test_that("rowData contains the `cycle` column", {
    expect_true("cycle" %in% colnames(rowData(se)))
})

test_that("rowData rows are equal in number to assay rows", {
    expect_equal(nrow(rowData(se)), nrow(assay(se, "fluo_normalized")))
})

test_that("PCR cycles are propertly sorted and uniquely defined", {
    expect_true(all(rowData(se)$cycle == sort(rowData(se)$cycle)))
    expect_false(any(duplicated(rowData(se)$cycle)))
})

# SE data structure - metadata --------------------------------------------

test_that("metadata stores source filepath and run information", {
    expect_true("source_file" %in% names(metadata(se)))
    expect_true("run_info" %in% names(metadata(se)))
})

test_that("path stored in metadata matches path used to generate SE object", {
    expect_equal(metadata(se)$source_file, path)
})

test_that("run metadata is stored as a data frame", {
    expect_true(is.data.frame(metadata(se)$run_info))
})

# Data synchronization ----------------------------------------------------

test_that("rowData PCR cycles map to cycle numbers stored in assay matrices", {
    expect_equal(
        rowData(se)$cycle,
        as.numeric(gsub("cycle_", "", rownames(se)))
    )
})

test_that("colData PCR wells map to well IDs stored in assay matrices", {
    expect_equal(
        colData(se)$well,
        as.numeric(gsub("well_", "", colnames(se)))
    )
})

# Compatibility across datasets -------------------------------------------

test_that("excelToSE returns an SE object without error with both datasets", {
    se2 <- excelToSE(excel_path = system.file(
        "extdata",
        "oa_gene_expression_2.xlsx",
        package = "OAtools"
    ))
    expect_s4_class(se2, "SummarizedExperiment")
})
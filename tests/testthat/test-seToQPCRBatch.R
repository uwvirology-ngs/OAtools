# Setup -------------------------------------------------------------------

path = system.file(
    "extdata", 
    "oa_gene_expression_1.xlsx", 
    package = "OAtools"
)

se <- excelToSE(excel_path = path)

qpcr <- seToQPCRBatch(se)

# Data Storage ------------------------------------------------------------

test_that("gene names are stored in the qPCRBatch", {
    expect_setequal(
        unique(colData(se)$target_name), 
        rownames(Biobase::exprs(qpcr))
    )
})

test_that("sample names are stored in the qPCRBatch", {
    expect_setequal(
        unique(colData(se)$sample_name), 
        colnames(Biobase::exprs(qpcr))
    )
})

test_that("a valid qPCRBatch object is returned", {
    expect_s4_class(qpcr, "qPCRBatch")
})

test_that("expression data is stored as a matrix of numeric values", {
    expression_data <- Biobase::exprs(qpcr)
    
    expect_contains(class(expression_data), "matrix")
    expect_identical(typeof(expression_data), "double")
})

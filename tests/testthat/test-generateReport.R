# Setup -------------------------------------------------------------------

data(example_se)

# Tests -------------------------------------------------------------------

test_that("an HTML file is succesfully generated", {
    path <- tempdir()
    
    generateReport(se = example_se, path = path)
    
    expect_true(file.exists(file.path(path, "report.html")))
})

test_that("an error is thrown when kableExtra and DT are not installed", {
    path <- tempdir()
    
    local_mocked_bindings(
        requireNamespace = function(pkg, quietly = TRUE) FALSE,
        .package = "base"
    )
    
    expect_error(
        generateReport(se = example_se, path = path)
    )
})

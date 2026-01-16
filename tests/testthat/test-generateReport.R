# Setup -------------------------------------------------------------------

data(example_se)

# Tests -------------------------------------------------------------------

test_that("an HTML file is succesfully generated", {
    path <- tempdir()
    
    generateReport(se = example_se, path = path)
    
    expect_true(file.exists(file.path(path, "report.html")))
})

#' Generate a report
#'
#' Generates a .html report summarizing the OpenArray experiment.
#'
#' @param se a SummarizedExperiment containing OpenArray qPCR data
#' @param path intended outfile path
#'
#' @returns an .html report summarizing the OpenArray run results
#'
#' @export
#'
#' @examples
#' data(example_se)
#' generateReport(se = example_se, path = file.path("..", "report"))
generateReport <- function(se, path) {
    
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        stop(
            "Package 'kableExtra' is required to generate the PCR report.\n",
            "Please install it with install.packages('kableExtra')."
        )
    }
    
    data_file <- tempfile(fileext = ".rds")
    saveRDS(se, data_file)
    
    rmarkdown::render(
        input = system.file("reports", "pcr_report.rmd", package = "OAtools"),
        output_file = "report.html",
        output_dir = path,
        params = list(data_path = data_file),
        envir = environment()
    )
}
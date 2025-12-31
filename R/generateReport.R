#' Generate a PCR Report
#'
#' Knits an HTML report summarizing the OpenArray experiment and saves to the 
#' specified directory.
#'
#' @param se a SummarizedExperiment containing OpenArray qPCR data
#' @param path intended outfile path, defaults to a temporary directory
#' @param model_results boolean value indicating whether to include the 
#' `results` column, which is created when deriving results using the 
#' curve-fitting method
#' 
#' @returns An HTML Report summarizing the OpenArray experiment
#'
#' @export
#'
#' @examples
#' data(example_se)
#' generateReport(se = example_se)
generateReport <- function(se, path = tempdir(), model_results = FALSE) {
    
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
        params = list(
            data_path = data_file,
            model_results = model_results
        ),
        envir = environment()
    )
}
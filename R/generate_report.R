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
generate_report_from_se <- function(se, path) {
    
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
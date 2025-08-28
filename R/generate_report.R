#' Generate a report
#'
#' Generates a .html report
#'
#' @param data a tibble of qPCR data ready for reporting
#' @param path intended outfile path
#'
#' @returns an .html report summarizing the OpenArray run results
#'
#' @export
#'
#' @examples
#' path = file.path("C:", "Users", "aidants", "Downloads")
#' generate_report(result_data, path)
generate_report <- function(data, path) {

  data_file <- tempfile(fileext = ".rds")
  saveRDS(data, data_file)

  rmarkdown::render(
    input = system.file("reports", "pcr_report.rmd", package = "OAtools"),
    output_file = "report.html",
    output_dir = path,
    params = list(data_path = data_file),
    envir = environment()
  )
}

#' Generate a report
#'
#' Generates a .html report
#'
#' @param data a tibble of qPCR data ready for reporting
#'
#' @returns an .html report summarizing the OpenArray run results
#'
#' @export
#'
#' @examples
#' generate_report(result_data)
generate_report <- function(data) {

  data_file <- tempfile(fileext = ".rds")
  saveRDS(data, data_file)

  rmarkdown::render(
    input = system.file("reports", "pcr_report.rmd", package = "OAtools"),
    output_file = "report.html",
    params = list(data_path = data_file)
  )
}

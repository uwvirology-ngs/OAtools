generate_report <- function(data) {

  data_file <- tempfile(fileext = ".rds")
  saveRDS(data, data_file)

  quarto::quarto_render(
    input = system.file("quarto", "pcr_report.qmd", package = "OAtools"),
    output_file = "report.html",
    execute_params = list(data_path = data_file)
  )

}

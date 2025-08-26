#' Tidy cumulative gene expression data
#'
#' This function transforms bulk raw gene expression run data exported in .xlsx format from
#' the OpenArray QuantStudio 12K Flex qPCR platform into a tidy R tibble.
#'
#' @param directory file path to run data directory, which may hold several .xlsx files exported from QuantStudio 12K Flex Software
#' @param num_results an integer reflecting the number of rows on each 'results' sheet
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' dir = system.file("extdata", package = "OAtools")
#' cumulative_run_data <- tidy_cumulative_gene_expression_data(directory = dir, num_results = 96)
tidy_cumulative_gene_expression_data <- function(directory, num_results = 2688) {

  # generate list of files ending in 'data.xlsx'
  files <- base::list.files(
    path = directory,
    pattern = "data\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # raise error in case that no files are loaded
  if (length(files) == 0) {
    stop("no '_data.xlsx' files found, please review the selected directory")
  }

  # bind the outputs of running the standard loading function on each file
  cumulative_data <- base::lapply(files, function(file) {
    OAtools::tidy_gene_expression_data(path = file, num_results = num_results)
  }) |> dplyr::bind_rows()

  return(cumulative_data)
}

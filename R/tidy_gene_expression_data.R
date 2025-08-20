#' Tidy gene expression data
#'
#' This function transforms raw gene expression run data exported in .xlsx format from
#' the OpenArray QuantStudio 12K Flex qPCR platform into a tidy R tibble.
#'
#' @param path file path to the run data (.xlsx)
#' @param num_results an integer reflecting the number of rows on the 'results' sheet
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' path = system.file("extdata", "gene_expression_run_data.xlsx", package = "OAtools")
#' tidy_run_data <- tidy_gene_expression_data(path = path, num_results = 96)
tidy_gene_expression_data <- function(path, num_results = 2688) {

  # import results sheet
  results_data <- readxl::read_excel(path, skip = 19, sheet = "Results", na = "Undetermined", n_max = num_results) |>
    dplyr::select("Well", "Well Position", "Sample Name", "Target Name", "Crt", "Amp Score", "Cq Conf", "Amp Status") |>
    janitor::clean_names()

  # import multicomponent data sheet
  multicomponent_data <- readxl::read_excel(path, skip = 19, sheet = "Multicomponent Data") |>
    dplyr::select("Well", "Cycle", "FAM") |>
    tidyr::drop_na() |>
    janitor::clean_names()

  # merge and arrange results and multicomponent data sheets
  gene_expression_data <- dplyr::full_join(results_data, multicomponent_data, by="well") |>
    dplyr::arrange(.data$well, .data$cycle) |>
    dplyr::mutate("batch_name" = base::basename(path))

  # enforce column types
  gene_expression_data <- gene_expression_data |>
    dplyr::mutate(
      well = as.integer(.data$well),
      well_position = as.factor(.data$well_position),
      sample_name = as.factor(.data$sample_name),
      target_name = as.factor(.data$target_name),
      crt = as.double(.data$crt),
      amp_score = as.double(.data$amp_score),
      cq_conf = as.double(.data$cq_conf),
      amp_status = as.factor(.data$amp_status),
      cycle = as.integer(.data$cycle),
      fam = as.double(.data$fam),
      batch_name = as.factor(.data$batch_name)
    )

  # round decimals for legibility
  gene_expression_data <- gene_expression_data |> dplyr::mutate(dplyr::across(dplyr::where(is.double), ~round(., 3)))

  # enforce return type
  return(tibble::as_tibble(gene_expression_data))
}

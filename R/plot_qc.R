#' Plot a 3-dimensional quality control graphic
#'
#' This function generates a 3-dimensional QC plot comparing the amplification status to the
#' crt, Cq conf, and amplification score output by QuantStudio 12K Flex software.
#'
#' @param data a tibble of qPCR data; either a single batch or a cumulative tibble with the batch parameter specified
#' @param batch character string for input batch for plotting
#'
#' @returns a plotly figure
#' @export
#'
#' @examples
#' fig <- plot_qc(tidy_run_data)
plot_qc <- function(data, batch = NA)
{

  # filter by batch number if given
  if (!is.na(batch)) {
    data <- data |>
      dplyr::filter(.data$batch_name == batch)
  }

  data <- data |> dplyr::distinct(.data$well_position, .keep_all = TRUE)

  fig <- data |>
    dplyr::mutate(crt = replace(.data$crt, is.na(.data$crt), 40)) |>
    plotly::plot_ly(
      x = ~.data$crt, y = ~.data$amp_score, z = ~.data$cq_conf,
      color = ~.data$amp_status, colors = c('#BF382A', '#0C4B8E'),
      text = paste(data$batch_name, data$target_name, data$well_position, sep = "\n")
    ) |>
    plotly::add_markers() |>
    plotly::layout(scene = list(xaxis = list(title = 'Crt', range = c(0, 40)),
                                yaxis = list(title = 'Amp Score', range = c(0, 2)),
                                zaxis = list(title = 'Cq Conf', range = c(0, 1)))
    ) |>
    plotly::layout(title = unique(data$batch_name))

  return(fig)
}

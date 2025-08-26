#' Plot overview heat map
#'
#' This function generates a heat map summarizing the amplification status results for
#' technical replicates grouped by sample, assay target, and batch name.
#'
#' @param data a tibble of qPCR data; either a single batch or a cumulative tibble with the batch parameter specified
#' @param sample_list optional character list of samples with which to complete heat map
#' @param target_list optional character list of assay targets with which to complete heat map
#' @param batch character string for input batch for plotting
#'
#' @returns a ggplot2 figure
#' @export
#'
#' @examples
#' fig <- plot_overview(tidy_run_data)
plot_overview <- function(data, sample_list = NA, target_list = NA, batch = NA)
{

  # filter by batch number if given
  if (!is.na(batch)) {
    data <- data |>
      dplyr::filter(.data$batch_name == batch)
  }

  # tabulate summary of results
  summary <- data |>
    dplyr::group_by(.data$sample_name, .data$target_name) |>
    dplyr::summarize(
      category = dplyr::case_when(
        any(.data$amp_status == "Amp") ~ "Amp",
        any(.data$amp_status == "Inconclusive") ~ "Inconclusive",
        TRUE ~ "No Amp"
      ),
      .groups = "drop"
    )

  # complete heat map with full sample and target lists if given
  if (!is.na(sample_list[1]) & !is.na(target_list[1])) {
    summary <- summary |>
      tidyr::complete(
        sample_name = sample_list,
        target_name = target_list,
        fill = list(category = "No Amp")
      )
  }

  # generate heatmap figure
  heatmap <- summary |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$target_name, y = .data$sample_name, fill = category)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("No Amp" = "white", "Inconclusive" = "blue", "Amp" = "green")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Amplification Status per Sample/Target Combination",
      x = "Target",
      y = "Sample",
      fill = "Highest Amplification Status"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 20, face = "bold"),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14)
    )

  return(heatmap)
}

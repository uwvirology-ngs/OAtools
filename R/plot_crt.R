#' Plot crt values
#'
#' This function generates a box and whisker plot visualizing the distribution of
#' observed crt values for each assay target in a given run.
#'
#' @param data a tibble of qPCR data; either a single batch or a cumulative tibble with the batch parameter specified
#' @param batch character string for input batch for plotting
#'
#' @returns a ggplot2 figure
#' @export
#'
#' @examples
#' fig <- plot_crt(tidy_run_data)
plot_crt <- function(data, batch = NA)
{

  # filter by batch number if given
  if (!is.na(batch)) {
    data <- data |>
      dplyr::filter(.data$batch_name == batch)
  }

  # collapse data frame for unique reactions
  data <- data |>
    dplyr::distinct(.data$well_position, .keep_all = TRUE)

  # generate box and whisker plot
  fig <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data$target_name, y = .data$crt, fill = .data$target_name)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_jitter(
      position = ggplot2::position_jitter(width = 0.3, seed = 387),
      size = 0.7,
      alpha = 0.5,
      color = "black",
      na.rm = TRUE
    ) +
    ggplot2::scale_x_discrete(limits = rev(levels(data$target_name)), drop = FALSE) +
    ggplot2::scale_y_reverse(limits = c(40,0)) +
    ggplot2::scale_fill_viridis_d(option = "C", drop = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Crt Distribution per Assay Target",
      x = "Target",
      y = "Crt"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "none"
    )

  return(fig)
}

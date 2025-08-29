#' Plot overview heat map
#'
#' This function generates a heat map summarizing the amplification status results for
#' technical replicates grouped by sample, assay target, and batch name.
#'
#' @param data a tibble of qPCR data; either a single batch or a cumulative tibble with the batch parameter specified
#' @param sample_list optional character list of samples with which to complete heat map
#' @param target_list optional character list of assay targets with which to complete heat map
#' @param batch character string for input batch for plotting
#' @param analysis character string for analytic method, supports 'native' and 'curve-fitting' approaches
#'
#' @returns a ggplot2 figure
#' @export
#'
#' @examples
#' fig <- plot_overview(tidy_run_data)
plot_overview <- function(data, sample_list = NA, target_list = NA, batch = NA, analysis = 'native')
{

  # filter by batch number if given
  if (!is.na(batch)) {
    data <- data |>
      dplyr::filter(.data$batch_name == batch)
  }

  # tabulate summary of results
  summary <- data |> dplyr::group_by(.data$sample_name, .data$target_name)

  if (analysis == 'native') {
    summary <- summary |>
      dplyr::summarize(
        category = dplyr::case_when(
          any(.data$amp_status == "Amp") ~ "Positive",
          any(.data$amp_status == "Inconclusive") ~ "Inconclusive",
          TRUE ~ "Negative"
        ),
        .groups = "drop"
      )
  } else if (analysis == 'curve-fitting') {
    summary <- summary |>
      dplyr::summarize(
        category = dplyr::case_when(
          any(.data$result == "positive") ~ "Positive",
          any(.data$result == "inconclusive") ~ "Inconclusive",
          TRUE ~ "Negative"
        ),
        .groups = "drop"
      )
  } else {
    stop("invalid analysis type")
  }


  # complete heat map with full sample and target lists if given
  if (!is.na(sample_list[1]) & !is.na(target_list[1])) {
    summary <- summary |>
      tidyr::complete(
        sample_name = sample_list,
        target_name = target_list,
        fill = list(category = "Negative")
      )
  }

  # generate heatmap figure
  heatmap <- summary |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$target_name, y = .data$sample_name, fill = .data$category)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("Negative" = "white", "Inconclusive" = "blue", "Positive" = "green")) +
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

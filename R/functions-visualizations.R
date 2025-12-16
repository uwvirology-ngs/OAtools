#' Plot Overview of Amplifications as a Heat Map
#' 
#' This function generates a heat map summarizing the amplification status 
#' results for each sample on the OpenArray plate by target gene. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#'
#' @returns a ggplot2 figure
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' plot_overview_from_se(example_se)
plot_overview_from_se <- function(se) {
    
    # load in and format se to Data Frame
    data <- as.data.frame(SummarizedExperiment::colData(se))
    summary <- data |> 
        dplyr::group_by(.data$sample_name, .data$target_name) |> 
        dplyr::summarize(
            category = dplyr::case_when(
                any(.data$amp_status == "Amp") ~ "Positive",
                any(.data$amp_status == "Inconclusive") ~ "Inconclusive",
                TRUE ~ "Negative"
            ),
            .groups = "drop"
        )
    
    # generate heatmap figure
    heatmap <- summary |>
        ggplot2::ggplot(mapping = ggplot2::aes(
            x = .data$target_name, 
            y = .data$sample_name, 
            fill = .data$category
        )) +
        ggplot2::geom_tile(color = "black") +
        ggplot2::scale_fill_manual(values = c(
            "Negative" = "white", 
            "Inconclusive" = "blue", 
            "Positive" = "green"
        )) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Amplification Status per Sample/Target Combination",
            x = "Target",
            y = "Sample",
            fill = "Highest Amplification Status"
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 45, 
                hjust = 1, 
                size = 12
            ),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 16),
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            legend.title = ggplot2::element_text(size = 16),
            legend.text = ggplot2::element_text(size = 14)
        )
    
    return(heatmap)
}

#' Plot a 3D Quality Control Graphic from a SummarizedExperiment
#' 
#' Generates a 3-dimensional quality control plot comparing the amplification 
#' status to the crt, Cq conf, and amplification score metrics output by 
#' QuantStudio 12K Flex Software. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#'
#' @returns a plotly figure
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' plot_qc_from_se(example_se)
plot_qc_from_se <- function(se) {
    data <- as.data.frame(SummarizedExperiment::colData(se))
        
    data <- data |> dplyr::distinct(.data$well_position, .keep_all = TRUE)
    
    fig <- data |>
        dplyr::mutate(crt = replace(.data$crt, is.na(.data$crt), 40)) |>
        plotly::plot_ly(
            x = ~.data$crt, y = ~.data$amp_score, z = ~.data$cq_conf,
            color = ~.data$amp_status, colors = c('#BF382A', '#0C4B8E'),
            text = paste(
                data$batch_name, 
                data$target_name, 
                data$well_position, 
                sep = "\n"
            )
        ) |>
        plotly::add_markers() |>
        plotly::layout(scene = list(
            xaxis = list(title = 'Crt', range = c(0, 40)),
            yaxis = list(title = 'Amp Score', range = c(0, 2)),
            zaxis = list(title = 'Cq Conf', range = c(0, 1)))
        ) |>
        plotly::layout(title = unique(data$batch_name))
    
    return(fig)
}

#' Plot Crt Values from SummarizedExperiment
#' 
#' Generates a box and whisker plot to visualize the distribution of observed
#' Crt values for each assay target in an OpenArray plate. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#'
#' @returns a ggplot2 figure
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' plot_crt_from_se(example_se)
plot_crt_from_se <- function(se) {
    data <- as.data.frame(SummarizedExperiment::colData(se))
    
    # collapse data frame for unique reactions
    data <- data |>
        dplyr::distinct(.data$well_position, .keep_all = TRUE)
    
    # generate box and whisker plot
    fig <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(
        x = .data$target_name, 
        y = .data$crt, 
        fill = .data$target_name
    )) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.8, na.rm = TRUE) +
        ggplot2::geom_jitter(
            position = ggplot2::position_jitter(width = 0.3, seed = 387),
            size = 0.7,
            alpha = 0.5,
            color = "black",
            na.rm = TRUE
        ) +
        ggplot2::scale_x_discrete(limits = rev(
            levels(data$target_name)), 
            drop = FALSE
        ) +
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
            axis.text.x = ggplot2::element_text(
                angle = 45, 
                hjust = 1, 
                size = 8
            ), 
            legend.position = "none"
        )
    
    return(fig)
}
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
#' plot_crt(example_se)
plot_crt <- function(se) {
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
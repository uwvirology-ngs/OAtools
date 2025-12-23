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
#' plotOverview(example_se)
plotOverview <- function(se) {
    
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
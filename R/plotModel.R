#' Plot Fluorescence Values Predicted by Model
#' 
#' Juxtaposes the fluorescence values predicted by the model optimized to the 
#' measured fluorescence vs. cycle data for a particular well. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#' @param well_id a character representing the name of the well to plot as 
#' listed in the assay matrix
#'
#' @returns a ggplot2 figure
#' 
#' @import ggplot2
#' 
#' @importFrom rlang .data
#' @importFrom S4Vectors DataFrame
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' 
#' plotModel(example_se, well_id = "well_2665")
plotModel <- function(se, well_id) {
    
    cycles <- seq_along(assays(se)$fluo[, well_id])
    
    well_data <- DataFrame(
        cycle = cycles,
        fluo = assays(se)$fluo[, well_id],
        fluo_pred = ffluo_pred <- assays(se)$fluo_pred[, well_id]
    )
    
    fig <- well_data |> 
        ggplot(mapping = aes(x = .data$cycle)) +
        scale_x_continuous(
            breaks = seq(0, max(cycles), by = 5),
            limits = c(min(cycles), max(cycles))
        ) +
        scale_y_continuous(
            breaks = seq(0, 12000, by = 3000),
            limits = c(0, 12000)
        ) +
        scale_color_manual(
            name = "Source",
            breaks = c("Observed", "Predicted"),
            values = c("Observed" = "blue", "Predicted" = "red")
        ) +
        geom_point(
            aes(y = .data$fluo, colour = "Observed"),
            size = 1.2, alpha = 1
        ) +
        geom_line(
            aes(y = .data$fluo_pred, colour = "Predicted"), 
            linewidth = 0.8, alpha = 0.7
        ) +
        labs(
            x = "Cycle",
            y = "Fluorescence",
            title = "Model Prediction vs. Observed Fluorescence"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )
    
    return(fig)
    
}
#' Plot Relative Cycle Threshold Values by Gene
#' 
#' Generates a box and whisker plot visualizing the distribution of Crt values
#' measured on an OpenArray plate by Gene.
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#'
#' @returns a ggplot2 figure
#' 
#' @import ggplot2
#' 
#' @importFrom rlang .data
#' @importFrom SummarizedExperiment colData
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' 
#' plotCrt(example_se)
plotCrt <- function(se) {
    
    # extract coldata
    coldata <- as.data.frame(colData(se))
    
    # generate box and whisker plot
    fig <- coldata |> 
        ggplot(data = coldata, mapping = aes(
            x = .data$crt, 
            y = .data$target_name,
            fill = .data$target_name
        )) +
        scale_x_reverse(limits = c(40,0)) +
        scale_y_discrete(
            limits = rev(levels(factor(coldata$target_name)))
        ) +
        scale_fill_viridis_d(option = "C") +
        geom_boxplot(alpha = 0.7, na.rm = TRUE) +
        geom_point(
            position = position_jitter(width = 0, height = 0.3, seed = 387),
            na.rm = TRUE
        ) +
        labs(
            title = "Crt Distribution by Gene",
            x = "Crt",
            y = "Gene"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.position = "none",
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        ) 
    
    return(fig)
}
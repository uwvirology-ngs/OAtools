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
#' plotQC(example_se)
plotQC <- function(se) {
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
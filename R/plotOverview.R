#' Plot Amplification Status by Sample and Gene
#' 
#' Generates an overview graphic summarizing qPCR results for each combination 
#' of sample and gene on an OpenArray experiment. 
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
#' plotOverview(example_se)
plotOverview <- function(se) {
    
    # extract amplification status data from the SummarizedExperiment
    summary <- .summarizeBySampleAndGene(se)
    
    # generate ggplot2 figure
    fig <- summary |>
        ggplot(mapping = aes(
            x = .data$target_name, 
            y = .data$sample_name, 
            fill = .data$category
        )) +
        scale_y_discrete(
            limits = rev(levels(factor(summary$sample_name)))
        ) +
        geom_tile(color = "black") +
        scale_fill_manual(values = c(
            "Negative"     = "white", 
            "Inconclusive" = "blue", 
            "Positive"     = "green"
        )) +
        labs(
            title = "Amplification by Sample & Gene",
            x = "Gene",
            y = "Sample",
            fill = "Highest Amp Status"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        ) 
    
    return(fig)
}

# organize amplification data from SummarizedExperiment by sample and gene
.summarizeBySampleAndGene <- function(se) {
    
    summary <- as.data.frame(colData(se)) |> 
        dplyr::group_by(.data$sample_name, .data$target_name) |> 
        dplyr::summarize(
            category = dplyr::case_when(
                any(.data$amp_status == "Amp") ~ "Positive",
                any(.data$amp_status == "Inconclusive") ~ "Inconclusive",
                TRUE ~ "Negative"
            ),
            .groups = "drop"
        ) |> 
        dplyr::mutate(category = factor(
            .data$category, levels = c("Positive", "Inconclusive", "Negative")
        ))
    
    return(summary)
}
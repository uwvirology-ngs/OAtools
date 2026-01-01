#' Plot Fluorescence Values Predicted by Model
#' 
#' Juxtaposes the fluorescence values predicted by the model optimized to the 
#' measured fluorescence vs. cycle data for a particular well. 
#'
#' @param se a SummarizedExperiment object containing OpenArray qPCR data
#' @param well_id a character representing the name of the well to plot as 
#' listed in the assay matrix
#' @param include_mdpt_tangent boolean determines whether to annotate the 
#' midpoint of the reaction and draw a tangent line to the model curve at 
#' that point
#' @param include_coldata_annotation boolean determines whether to annotate 
#' the coldata onto the top left of the plot. 
#'
#' @returns a ggplot2 figure
#' 
#' @import ggplot2
#' @import SummarizedExperiment
#' 
#' @importFrom rlang .data
#' @importFrom S4Vectors DataFrame
#' 
#' @export
#'
#' @examples
#' data(example_se)
#' 
#' plotModel(
#'     example_se, 
#'     well_id = "well_2665", 
#'     include_mdpt_tangent = TRUE
#' )
plotModel <- function(se, well_id, include_mdpt_tangent = FALSE,
                        include_coldata_annotation = FALSE) {
    
    # pull well metadata from colData
    col_data <- as.data.frame(colData(se))
    sample <- col_data[well_id, "sample_name"]
    gene   <- col_data[well_id, "target_name"]
    
    
    # pull fluorescence data and cycle numbers from assay matrix
    cycles <- seq_along(assays(se)$fluo[, well_id])
    
    well_data <- DataFrame(
        cycle = cycles,
        fluo = assays(se)$fluo[, well_id],
        fluo_pred = assays(se)$fluo_pred[, well_id]
    )
    
    # build model plot with predicted vs. observed fluorescence values
    fig <- .constructModelPlot(
        well_data = well_data, 
        cycles = cycles, 
        well_id = well_id,
        sample = sample, 
        gene = gene
    )
    
    # optionally annotate the midpoint with a point and tangent line
    if (include_mdpt_tangent) {
        fig <- .annotateMidpoint(
            col_data = col_data, 
            fig = fig, 
            well_id = well_id
        )
    }

    # optionally annotates the plot with information from the coldata    
    if (include_coldata_annotation) {
        fig <- .annotateWithColdata(
            col_data = col_data, 
            fig = fig, 
            well_id = well_id
        )
    }
    
    return(fig)
}

# build model plot with predicted vs. observed fluorescence values
.constructModelPlot <- function(well_data, cycles, well_id, sample, gene) {
    
    fig <- well_data |> 
        ggplot(mapping = aes(x = .data$cycle)) +
        scale_x_continuous(
            breaks = seq(0, max(cycles), by = 5),
            limits = c(min(cycles), max(cycles))
        ) +
        scale_y_continuous(
            breaks = seq(0, 15000, by = 3000),
            limits = c(0, 15000)
        ) +
        scale_color_manual(
            name = "Source",
            breaks = c("Observed", "Predicted", "Tangent"),
            values = c(
                "Observed" = "blue", 
                "Predicted" = "red",
                "Tangent" = "green"
            )
        ) +
        geom_point(
            aes(y = .data$fluo, colour = "Observed"),
            size = 1.2, alpha = 1
        ) +
        geom_line(
            aes(y = .data$fluo_pred, colour = "Predicted"), 
            linewidth = 0.8, alpha = 0.8
        ) +
        labs(
            x = "Cycle",
            y = "Fluorescence",
            title = "Model Prediction vs. Observed Fluorescence",
            subtitle = paste0(sample, "        ", gene, "        ", well_id)
        ) +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5)
        )
    
    return(fig)
}

# optionally annotate the midpoint with a point and tangent line
.annotateMidpoint <- function(col_data, fig, well_id) {
    
    mdpt_cycle <- col_data[well_id, "midpoint_cycle"]
    mdpt_fluo  <- col_data[well_id, "midpoint_fluo"]
    mdpt_slope <- col_data[well_id, "midpoint_slope"]
    
    fig <- fig + 
        geom_abline(
            aes(
                slope = mdpt_slope,
                intercept = mdpt_fluo - (mdpt_slope * mdpt_cycle),
                colour = "Tangent"
            ),
            alpha = 0.6
        ) +
        annotate(
            "point",
            x = mdpt_cycle, 
            y = mdpt_fluo,
            color = "darkgreen",
            size = 3, alpha = 0.6
        )
    
    return(fig)
}

# optionally annotates the plot with information from the coldata   
.annotateWithColdata <- function(col_data, fig, well_id) {
    
    delta_fluo <- col_data[well_id, "delta_fluo"]
    mdpt_cycle <- col_data[well_id, "midpoint_cycle"]
    mdpt_slope <- col_data[well_id, "midpoint_slope"]
    
    text <- paste0(
        "Change in Fluorescence: ", round(delta_fluo, 1), "\n",
        "Midpoint Cycle: ", round(mdpt_cycle, 1), "\n",
        "Midpoint Slope: ", round(mdpt_slope, 1)
    )
    
    fig <- fig +
        annotate(
            "text",
            x = 3, y = Inf,
            label = text, 
            hjust = 0, vjust = 1.5,
            size = 4, 
            fontface = "italic",
            color = "black"
        )
    
    return(fig)
}
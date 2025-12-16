#' Convert OpenArray data from Excel to a Summarized Experiment
#' 
#' Transforms raw gene expression run data exported from the OpenArray
#' QuantStudio 12K Flex Software from .xlsx format into an instance of the 
#' SummarizedExperiment class from Bioconductor. 
#'
#' @param path file path to the run data
#' @param num_results an integer representing the number of rows on the
#' 'results' sheet to be imported. 
#'
#' @returns A SummarizedExperiment object
#' 
#' @export
#'
#' @examples
#' path = system.file(
#'     "extdata", 
#'     "oa_gene_expression_batch1.xlsx", 
#'     package = "OAtools"
#' )
#' se <- excel_to_summarized_experiment(path = path, num_results = 96)
excel_to_summarized_experiment <- function(path = path, num_results = 96) {
    
    # import results sheet
    results_data <- readxl::read_excel(
        path = path, skip = 19, sheet = "Results",
        na = "Undetermined", n_max = num_results
    ) |> 
        dplyr::select(
            "Well", "Well Position", "Omit", "Sample Name", "Target Name", 
            "Task", "Reporter", "Quencher", "Crt", "Crt Mean", "Crt SD",
            "Amp Score", "Cq Conf", "Amp Status", "HIGHSD", "ROX Signal"
        ) |> 
        janitor::clean_names()
    
    # throw error in case of inappropriate inclusion of metadata in tibble
    if (!is.numeric(results_data$well)) {
        stop(
            "Well column is not numeric. This can often happen when the ",
            "num_results parameter exceeds the number of rows of data ",
            "on the 'results' tab of the input excel file."
        )
    }
    
    # import multicomponent data sheet
    multicomponent_data <- readxl::read_excel(
        path, skip = 19, sheet = "Multicomponent Data"
    ) |>
        dplyr::select("Well", "Cycle", "FAM") |>
        tidyr::drop_na() |>
        janitor::clean_names()
    
    # prepare fluorescence data
    fluo_data <- multicomponent_data |> 
        dplyr::mutate(
            well = factor(.data$well, levels = sort(unique(.data$well)))
        ) |> 
        tidyr::pivot_wider(
            names_from = .data$well,
            values_from = .data$fam
        ) |> 
        dplyr::arrange(.data$cycle)
    
    # create assay matrix
    assay <- fluo_data |> 
        dplyr::select(-.data$cycle) |> 
        base::as.matrix()
    
    rownames(assay) <- base::paste0("cycle_", fluo_data$cycle)
    colnames(assay) <- base::paste0("well_", colnames(assay))
    
    # prepare coldata
    col_data <- results_data |> 
        dplyr::arrange(.data$well) |> 
        S4Vectors::DataFrame(row.names = paste0("well_", results_data$well))
    
    # prepare rowdata
    row_data <- S4Vectors::DataFrame(
        cycle = fluo_data$cycle,
        row.names = paste0("cycle_", fluo_data$cycle)
    )
        
    # construct SummarizedExperiment
    se <- SummarizedExperiment::SummarizedExperiment(
        assays   = S4Vectors::SimpleList(fluo = assay),
        rowData = row_data,
        colData = col_data,
        metadata = list(
            source_file = path,
            num_wells = num_results
        )
    )
    
    return(se)
}
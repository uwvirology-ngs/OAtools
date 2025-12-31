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
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom rlang .data
#' 
#' @export
#'
#' @examples
#' path = system.file(
#'     "extdata", 
#'     "oa_gene_expression_batch1.xlsx", 
#'     package = "OAtools"
#' )
#' se <- excelToSummarizedExperiment(path = path, num_results = 96)
excelToSummarizedExperiment <- function(path, num_results = 96) {
    
    # scrape data from results tab
    results_data <- .importResultsTab(path = path, num_results = num_results)
    
    # scrape data from multicomponent tab
    multicomponent_data <- .importMulticomponentTab(path = path)
    
    # 1. create assay matrix (cell values represent fluorescence measurements)
    assay_matrix <- multicomponent_data |> 
        dplyr::select(-.data$cycle) |> 
        as.matrix()
    
    rownames(assay_matrix) <- paste0("cycle_", multicomponent_data$cycle)
    colnames(assay_matrix) <- paste0("well_", colnames(assay_matrix))
    
    # 2. create colData (columns represent wells)
    col_data <- results_data |> 
        dplyr::arrange(.data$well) |> 
        DataFrame(row.names = paste0("well_", results_data$well))
    
    # 3. create rowData (rows represent PCR cycles)
    row_data <- DataFrame(
        cycle = multicomponent_data$cycle,
        row.names = paste0("cycle_", multicomponent_data$cycle)
    )
    
    # construct SummarizedExperiment
    se <- SummarizedExperiment(
        assays = SimpleList(fluo = assay_matrix),
        rowData = row_data,
        colData = col_data,
        metadata = list(
            source_file = path,
            num_wells = num_results
        )
    )
    
    return(se)
}

# import results sheet from excel and return data as a tibble
.importResultsTab <- function(path, num_results = 96) {
    
    # read in results tab
    results_data <- readxl::read_excel(
        path = path, skip = 19, sheet = "Results",
        na = "Undetermined", n_max = num_results
    ) |> 
        dplyr::select(
            "Well", "Well Position", "Omit", "Sample Name", "Target Name", 
            "Task", "Reporter", "Quencher", "Crt", "Crt Mean", "Crt SD",
            "Amp Score", "Cq Conf", "Amp Status", "HIGHSD", "ROX Signal"
        ) |> 
        dplyr::mutate(
            `Crt Mean` = as.numeric(.data$`Crt Mean`),
            `Crt SD`   = as.numeric(.data$`Crt SD`)            
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
    
    return(results_data)
}

# import multicomponent sheet from excel and return data as a tibble
.importMulticomponentTab <- function(path) {
    
    # read in multicomponent tab
    multicomponent_data <- readxl::read_excel(
        path, skip = 19, sheet = "Multicomponent Data"
    ) |>
        dplyr::select("Well", "Cycle", "FAM") |>
        tidyr::drop_na() |>
        janitor::clean_names()
    
    # transform multicomponent data into tidy format
    fluo_data <- multicomponent_data |> 
        tidyr::pivot_wider(
            names_from = .data$well,
            values_from = .data$fam
        ) |> 
        dplyr::arrange(.data$cycle)
    
    return(fluo_data)
}
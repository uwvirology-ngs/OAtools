#' Convert OpenArray data from Excel to a Summarized Experiment
#' 
#' Transforms raw gene expression run data exported from the OpenArray
#' QuantStudio 12K Flex Software from .xlsx format into an instance of the 
#' SummarizedExperiment class from Bioconductor. 
#'
#' @param excel_path file path to the Excel document containing the PCR data
#' @param header_rows number of rows of run metadata to read in from the header
#' @param skip number of rows to skip when reading fluorescence data or results
#'
#' @returns OpenArray PCR data as a SummarizedExperiment object
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
#'     "oa_gene_expression_1.xlsx", 
#'     package = "OAtools"
#' )
#' 
#' se <- excelToSE(excel_path = path)
excelToSE <- function(excel_path, header_rows = 17, skip = 19) {
    
    # 1. import OpenArray PCR data from excel file of interest
    amplification_data <- .importAmplificationTab(
        excel_path = excel_path, skip = skip
    )
    multicomponent_data <- .importMulticomponentTab(
        excel_path = excel_path, skip = skip
    )
    results_data <- .importResultsTab(
        excel_path = excel_path, skip = skip
    )
    
    # 2. build assay matrices (cell values ~ fluorescence measurements)
    amp_matrix <- amplification_data |> 
        dplyr::select(-.data$cycle) |> as.matrix() 
    
    rownames(amp_matrix) <- paste0("cycle_", amplification_data$cycle)
    colnames(amp_matrix) <- paste0("well_", colnames(amp_matrix))
    
    multi_matrix <- multicomponent_data |> 
        dplyr::select(-.data$cycle) |> as.matrix()
    
    rownames(multi_matrix) <- paste0("cycle_", multicomponent_data$cycle)
    colnames(multi_matrix) <- paste0("well_", colnames(multi_matrix))
    
    # 3. build colData (PCR wells) and rowData (PCR cycles)
    col_data <- results_data |> dplyr::arrange(.data$well)
    col_data <- DataFrame(col_data, row.names = paste0("well_", col_data$well))
    
    row_data <- DataFrame(
        cycle = amplification_data$cycle,
        row.names = paste0("cycle_", amplification_data$cycle)
    )
    
    # 4. construct SummarizedExperiment container for OpenArray experiment
    se <- SummarizedExperiment(
        assays = SimpleList(
            fluo_normalized = amp_matrix,
            fluo_reporter = multi_matrix
        ),
        rowData = row_data, colData = col_data,
        metadata = list(
            source_file = excel_path, 
            run_info = .importRunInfo(path = excel_path, nrows = header_rows)
        )
    )
    return(se)
}

# import `Amplification Data` tab from Excel and return data as a tibble
.importAmplificationTab <- function(excel_path, skip) {
    
    # load `Amplification Data` tab into a tibble
    amplification_data <- readxl::read_excel(
        path = excel_path, skip = skip, sheet = "Amplification Data"
    ) |> 
        dplyr::select("Well", "Cycle", "Delta R") |> 
        tidyr::drop_na() |> 
        janitor::clean_names()
    
    # transform normalized fluorescence data into tidy format
    amplification_data <- amplification_data |> 
        tidyr::pivot_wider(
            names_from = .data$well,
            values_from = .data$delta_r
        ) |> 
        dplyr::arrange(.data$cycle)
    
    return(amplification_data)
}

# import `Multicomponent Data` tab from Excel and return data as a tibble
.importMulticomponentTab <- function(excel_path, skip) {
    
    # load `Multicomponent Data` tab into a tibble
    multicomponent_data <- readxl::read_excel(
        path = excel_path, skip = skip, sheet = "Multicomponent Data"
    ) |>
        dplyr::select("Well", "Cycle", "FAM") |>
        tidyr::drop_na() |> 
        janitor::clean_names()
    
    # transform multicomponent fluorescence data into tidy format
    multicomponent_data <- multicomponent_data |> 
        tidyr::pivot_wider(
            names_from = .data$well,
            values_from = .data$fam
        ) |> 
        dplyr::arrange(.data$cycle)
    
    return(multicomponent_data)
}

# import `Results` sheet from Excel and return data as a tibble
.importResultsTab <- function(excel_path, skip) {
    
    # load `Results` tab into a tibble
    results_data <- readxl::read_excel(
        path = excel_path, skip = skip, sheet = "Results", na = "Undetermined"
    ) |> 
        dplyr::select(
            "Well", "Well Position", "Omit", "Sample Name", "Target Name", 
            "Task", "Reporter", "Quencher", "Crt", "Crt Mean", "Crt SD",
            "Amp Score", "Cq Conf", "Amp Status", "HIGHSD", "ROX Signal"
        ) |> 
        dplyr::filter(grepl("^\\d+$", .data$`Well`)) |> 
        dplyr::mutate(
            `Well`     = as.numeric(.data$`Well`),
            `Crt Mean` = as.numeric(.data$`Crt Mean`),
            `Crt SD`   = as.numeric(.data$`Crt SD`)            
        ) |> 
        janitor::clean_names()
    
    return(results_data)
}

# Import metadata from Excel and return data as a tibble
.importRunInfo <- function(path, nrows) {
    
    # Load run metadata into a tibble
    metadata <- readxl::read_excel(
        path = path, n_max = nrows, sheet = "Results", 
        col_names = c("Field", "Data")
    )
    
    return(metadata)
}
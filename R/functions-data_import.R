#' Tidy gene expression data
#'
#' This function transforms raw gene expression run data exported in .xlsx 
#' format from the OpenArray QuantStudio 12K Flex qPCR platform into a 
#' tidy R tibble.
#'
#' @param path file path to the run data (.xlsx)
#' @param num_results an integer reflecting the number of rows 
#' on the 'results' sheet
#'
#' @returns A tibble
#'
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
#' 
#' tidy_run_data <- tidy_gene_expression_data(path = path, num_results = 96)
tidy_gene_expression_data <- function(path, num_results = 2688) {
    
    # import results sheet
    results_data <- readxl::read_excel(
        path, skip = 19, sheet = "Results", 
        na = "Undetermined", n_max = num_results
    ) |>
        dplyr::select(
            "Well", "Well Position", "Sample Name", "Target Name", 
            "Crt", "Amp Score", "Cq Conf", "Amp Status"
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
    
    # merge and arrange results and multicomponent data sheets
    gene_expression_data <- dplyr::full_join(
        results_data, 
        multicomponent_data, 
        by="well"
    ) |>
        dplyr::arrange(.data$well, .data$cycle) |>
        dplyr::mutate("batch_name" = base::basename(path))
    
    # enforce column types
    gene_expression_data <- gene_expression_data |>
        dplyr::mutate(
            well = as.integer(.data$well),
            well_position = as.factor(.data$well_position),
            sample_name = as.factor(.data$sample_name),
            target_name = as.factor(.data$target_name),
            crt = as.double(.data$crt),
            amp_score = as.double(.data$amp_score),
            cq_conf = as.double(.data$cq_conf),
            amp_status = as.factor(.data$amp_status),
            cycle = as.integer(.data$cycle),
            fam = as.double(.data$fam),
            batch_name = as.factor(.data$batch_name)
        )
    
    # round decimals for legibility
    gene_expression_data <- gene_expression_data |> 
        dplyr::mutate(dplyr::across(dplyr::where(is.double), ~round(., 3)))
    
    # enforce return type
    return(tibble::as_tibble(gene_expression_data))
}

#' Tidy cumulative gene expression data
#'
#' This function transforms bulk raw gene expression run data exported 
#' in .xlsx format from the OpenArray QuantStudio 12K Flex qPCR platform 
#' into a tidy R tibble.
#'
#' @param directory file path to run data directory, which may hold several 
#' .xlsx files exported from QuantStudio 12K Flex Software
#' @param num_results an integer reflecting the number of rows 
#' on each 'results' sheet
#'
#' @returns A tibble
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' dir = system.file("extdata", package = "OAtools")
#' 
#' tidy_run_data_cumulative <- tidy_cumulative_gene_expression_data(
#'     directory = dir, 
#'     num_results = 96
#' )
tidy_cumulative_gene_expression_data <- function(
        directory, num_results = 2688) {
    
    # generate list of files ending in 'data.xlsx'
    files <- base::list.files(
        path = directory,
        pattern = "batch[0-9]+\\.xlsx$",
        full.names = TRUE,
        ignore.case = TRUE
    )
    
    # raise error in case that no files are loaded
    if (length(files) == 0) {
        stop("no '_data.xlsx' files found, please review the selected dir.")
    }
    
    # bind the outputs of running the standard loading function on each file
    cumulative_data <- base::lapply(files, function(file) {
        OAtools::tidy_gene_expression_data(
            path = file, 
            num_results = num_results
        )
    }) |> dplyr::bind_rows()
    
    return(cumulative_data)
}

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
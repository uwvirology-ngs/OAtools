#' Convert to qPCRBatch Object
#' 
#' Transforms OpenArray run data contained within the SummarizedExperiment 
#' container into a qPCRBatch object. This conversion allows for convenient 
#' gene expression analyses with the NormqPCR package. 
#'
#' @param se A SummarizedExperiment object with OpenArray run data
#' 
#' @importClassesFrom ReadqPCR qPCRBatch
#' 
#' @importFrom methods new
#'
#' @returns a qPCRBatch object
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
#' se <- excelToSummarizedExperiment(path = path, num_results = 96)
#' 
#' qpcr <- seToQPCRBatch(se)
seToQPCRBatch <- function(se) {
    coldata <- as.data.frame(SummarizedExperiment::colData(se))
    
    # create Cq matrix, averaging over technical replicates
    cq_mat <- coldata |> 
        dplyr::group_by(.data$target_name, .data$sample_name) |> 
        dplyr::summarise(
            cq = mean(.data$crt, na.rm = TRUE),
            .groups = "drop"
        ) |> 
        tidyr::pivot_wider(
            names_from = .data$sample_name,
            values_from = .data$cq
        ) |> 
        tibble::column_to_rownames("target_name") |> 
        as.matrix()
    
    # create phenoData
    pheno_data <- Biobase::AnnotatedDataFrame(
        data = data.frame(
            row.names = colnames(cq_mat),
            sample = colnames(cq_mat)
        )
    )
    
    # call qPCRBatch constructor
    return(
        new(
            "qPCRBatch",
            exprs = cq_mat,
            phenoData = pheno_data
        )
    )
}
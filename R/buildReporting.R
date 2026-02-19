#' Reporting UI
#'
#' @param id identifier unique to `Reporting` page
#'
#' @returns The `Reporting` page
#'
#' @import shiny
.uiReporting <- function(id) {
    ns <- NS(id)
    tabPanel("Reporting",
             titlePanel("Export the Experimental Results"),
             sidebarLayout(
                 sidebarPanel(
                     p("Download PCR Report (HTML)"),
                     downloadButton(
                         outputId = ns("download_report"),
                         label = "download report"
                     ),
                     p(),
                     p("Download Run Data (.xlsx)"),
                     downloadButton(
                         outputId = ns("download_xlsx"),
                         label = "download data"
                     )
                 ),
                 mainPanel(
                     
                 )
             )
    )
}

#' Reporting Server
#'
#' @param id identifier unique to `Reporting` page
#' @param se SummarizedExperiment with OpenArray assay data
#'
#' @returns The `Reporting` server component
#'
#' @import shiny
#' 
#' @importFrom S4Vectors metadata
#' @importFrom rlang .data
#' @importFrom SummarizedExperiment colData
.serverReporting <- function(id, se) {
    moduleServer(id, function(input, output, session) {
        
        # download HTML PCR report
        output$download_report <- downloadHandler(
            filename = function() {
                oa_plate_id <- metadata(se())$run_info |> 
                    dplyr::filter(.data$Field == "Experiment Name") |> 
                    dplyr::pull("Data")
                paste0("Report_", oa_plate_id, "_", Sys.Date(), ".html") 
            },
            content = function(file) {
                req(se())
                tempfile <- tempdir()
                generateReport(se(), path = tempfile)
                file.copy(file.path(tempfile, "report.html"), file)
            }
        )
        
        # download Excel run data
        output$download_xlsx <- downloadHandler(
            filename = function() {
                oa_plate_id <- metadata(se())$run_info |> 
                    dplyr::filter(.data$Field == "Experiment Name") |> 
                    dplyr::pull("Data")
                paste0("OAtools_", oa_plate_id, "_", Sys.Date(), ".xlsx") 
            },
            content = function(file) {
                req(se())
                oatools_data <- as.data.frame(colData(se()))
                writexl::write_xlsx(oatools_data, path = file)
            }
        )
    })
}
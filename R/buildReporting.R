#' Reporting UI
#'
#' @param id identifier unique to `Reporting` page
#'
#' @returns The `Reporting` page
#'
#' @import shiny
#' 
#' @importFrom DT DTOutput
.uiReporting <- function(id) {
    ns <- NS(id)
    tabPanel("Reporting",
        titlePanel("Export the Experimental Results"),
        sidebarLayout(
            sidebarPanel(
                fileInput(
                    inputId = ns("keyfile"),
                    label = "Import Key for Resulting",
                    accept = ".xlsx",
                    buttonLabel = "select keyfile",
                    placeholder = "no file selected"
                ),
                p("Please upload a key which demarcates thresholds across 
                    the Crt value, total change in fluorescence, and 
                    midpoint slope for each gene. For details, input 
                    `?target_threshold_key` into the R console."),
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
                p("Once uploaded, preview the keyfile below: "),
                DTOutput(outputId = ns("keyfile_preview"))
            )
        )
    )
}

#' Reporting Server
#'
#' @param id identifier unique to `Reporting` page
#' @param se_fit SummarizedExperiment with OpenArray assay data
#'
#' @returns The `Reporting` server component
#'
#' @import shiny
#' 
#' @importFrom S4Vectors metadata
#' @importFrom rlang .data
#' @importFrom SummarizedExperiment colData
#' @importFrom DT renderDT datatable
.serverReporting <- function(id, se_fit) {
    moduleServer(id, function(input, output, session) {
        
        # load key on import
        key <- reactive({
            req(input$keyfile)
            readxl::read_excel(input$keyfile$datapath)
        })
        
        # render keyfile preview
        output$keyfile_preview <- renderDT({
            req(key())
            datatable(
                key(), rownames = FALSE, caption = "Key", 
                options = list(
                    pageLength = 4, autoWidth = TRUE, columnDefs = list(
                        list(className = 'dt-center', targets = "_all")
                    )
                )
            )
        })
        
        # build PCR report
        se_report <- reactive({
            req(se_fit())
            req(input$keyfile)
            se_fit() |> determinePCRResults(key_path = input$keyfile$datapath)
        })
        
        # download HTML PCR report
        output$download_report <- downloadHandler(
            filename = function() {
                se_out <- req(se_report())
                oa_plate_id <- metadata(se_out)$run_info |> 
                    dplyr::filter(.data$Field == "Experiment Name") |> 
                    dplyr::pull("Data")
                paste0("Report_", oa_plate_id, "_", Sys.Date(), ".html") 
            },
            content = function(file) {
                se_out <- req(se_report())
                temp_dir <- tempdir()
                generateReport(se_out, path = temp_dir)
                file.copy(file.path(temp_dir, "report.html"), file)
            }
        )
        
        # download Excel run data
        output$download_xlsx <- downloadHandler(
            filename = function() {
                se_out <- req(se_report())
                oa_plate_id <- metadata(se_out)$run_info |> 
                    dplyr::filter(.data$Field == "Experiment Name") |> 
                    dplyr::pull("Data")
                paste0("OAtools_", oa_plate_id, "_", Sys.Date(), ".xlsx") 
            },
            content = function(file) {
                se_out <- req(se_report())
                oatools_data <- as.data.frame(colData(se_out))
                writexl::write_xlsx(oatools_data, path = file)
            }
        )
    })
}
#' Data Import UI
#'
#' @param id identifier unique to `Data Import` page
#'
#' @returns The `Data Import` page
#' 
#' @import shiny
#' 
#' @importFrom DT DTOutput
.uiDataImport <- function(id) {
    ns <- NS(id)
    tabPanel("Data Import",
        titlePanel("Start Here!"),
        sidebarLayout(
            sidebarPanel(
                fileInput(
                    inputId = ns("infile"),
                    label = "Import OpenArray Run Data",
                    accept = ".xlsx",
                    buttonLabel = "select file",
                    placeholder = "no file selected"
                ),
                p("Please upload the Excel file exported from QuantStudio 12K
                    Flex Software after an OpenArray experiment. The following 
                    tabs are required: Amplification Data, Multicomponent Data,
                    Results.")
            ),
            mainPanel(
                p("Upon succesful upload, a data table will appear below: "),
                DTOutput(outputId = ns("run_data_preview"))
            )
        )
    )
}

#' Data Import Server
#'
#' @param id id identifier unique to `Data Import` page
#'
#' @returns The `Data Import` server component
#' 
#' @import shiny
#' 
#' @importFrom DT renderDT datatable
.serverDataImport <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # load assay data into SummarizedExperiment container upon import
        se <- reactive({
            req(input$infile)
            excelToSE(excel_path = input$infile$datapath)
        })
        
        # upon successful upload, render a preview table to the web page
        output$run_data_preview <- renderDT({
            req(se())
            
            preview_table <- as.data.frame(colData(se())) |> 
                dplyr::select(
                    "well", "sample_name", "target_name", "crt", "amp_status"
                )
            
            datatable(preview_table, rownames = FALSE, options = list(
                pageLength = 12,
                autoWidth = TRUE,
                columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                )
            ))
        })
        
        return(se)
    })
}
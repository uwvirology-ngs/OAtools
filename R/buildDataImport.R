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
        titlePanel("Import the QuantStudio 12K Flex file"),
        sidebarLayout(
            sidebarPanel(
                fileInput(
                    inputId = ns("runfile"),
                    label = "Import OpenArray Run Data",
                    accept = ".xlsx",
                    buttonLabel = "select runfile",
                    placeholder = "no file selected"
                ),
                p("Please upload the Excel file exported from QuantStudio 12K
                    Flex Software after an OpenArray experiment. The following 
                    tabs are required: "),
                p("* Amplification Data"),
                p("* Multicomponent Data"),
                p("* Results")
            ),
            mainPanel(
                p("Once uploaded, preview run data below: "),
                DTOutput(outputId = ns("runfile_preview"))
            )
        )
    )
}

#' Data Import Server
#'
#' @param id identifier unique to `Data Import` page
#'
#' @returns The `Data Import` server component
#' 
#' @import shiny
#' 
#' @importFrom DT renderDT datatable
.serverDataImport <- function(id) {
    moduleServer(id, function(input, output, session) {
        
        # build SummarizedExperiment object from input file
        se <- reactive({
            req(input$runfile)
            excelToSE(excel_path = input$runfile$datapath)
        })
        
        # preview the SummarizedExperiment
        output$runfile_preview <- renderDT({
            req(se())
            preview_table <- as.data.frame(colData(se())) |> 
                dplyr::select(
                    "well", "sample_name", "target_name", "crt", "amp_status"
                )
            datatable(
                preview_table, rownames = FALSE, caption = "Run Data",
                options = list(
                    pageLength = 12, autoWidth = TRUE, columnDefs = list(
                        list(className = 'dt-center', targets = "_all")
                    )
                )
            )
        })
        
        return(se)
    })
}
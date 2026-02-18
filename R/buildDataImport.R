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
                    inputId = ns("keyfile"),
                    label = "Import Key for Resulting",
                    accept = ".xlsx",
                    buttonLabel = "select keyfile",
                    placeholder = "no file selected"
                ),
                p("Please upload a key which demarcates thresholds across the 
                    Crt value, total change in fluorescence, and midpoint slope
                    for each gene. For details, input `?target_threshold_key`
                    into the R console."),
                numericInput(
                    inputId = ns("select_lin_threshold"),
                    label = "Select Linear Threshold",
                    value = 400,
                    min = 0,
                    max = 1000
                ),
                selectInput(
                    inputId = ns("select_fluo"),
                    label = "Select Fluorescence Readout",
                    choices = c("fluo_reporter"),
                    selected = "fluo_reporter"
                ),
                fileInput(
                    inputId = ns("runfile"),
                    label = "Import OpenArray Run Data",
                    accept = ".xlsx",
                    buttonLabel = "select runfile",
                    placeholder = "no file selected"
                ),
                p("Please upload the Excel file exported from QuantStudio 12K
                    Flex Software after an OpenArray experiment. The following 
                    tabs are required: Amplification Data, Multicomponent Data,
                    Results.")
            ),
            mainPanel(
                p("Once uploaded, preview the key and run data below: "),
                DTOutput(outputId = ns("keyfile_preview")),
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
        
        # load key and assay data upon import
        key <- reactive({
            req(input$keyfile)
            readxl::read_excel(input$keyfile$datapath)
        })
        se <- reactive({
            req(input$runfile)
            excelToSE(excel_path = input$runfile$datapath) |> 
                computeModels(
                    assay_name = input$select_fluo, 
                    linear_threshold = input$select_lin_threshold
                ) |> 
                determinePCRResults(key_path = input$keyfile$datapath)
        })
        
        # render a preview of the key and assay data to the webpage
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
        output$runfile_preview <- renderDT({
            req(se())
            preview_table <- as.data.frame(colData(se())) |> 
                dplyr::select(
                    "well", "sample_name", "target_name", 
                    "crt", "amp_status", "result"
                )
            datatable(
                preview_table, rownames = FALSE, caption = "Run Data",
                options = list(
                    pageLength = 4, autoWidth = TRUE, columnDefs = list(
                        list(className = 'dt-center', targets = "_all")
                    )
                )
            )
        })
        
        return(se)
    })
}
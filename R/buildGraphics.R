#' Graphics UI
#'
#' @param id identifier unique to `Graphics` page
#'
#' @returns The `Graphics` page
#' 
#' @import shiny
#' 
#' @importFrom plotly plotlyOutput
.uiGraphics <- function(id) {
    ns <- NS(id)
    tabPanel("Graphics",
        titlePanel("A Graphical Overview"),
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = ns("select_view"),
                    label = "Select View: ",
                    choices = c("Amp Status", "Crt Distribution", "Run QC")
                )
            ),
            mainPanel(
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'Amp Status'", ns("select_view")
                    ),
                    plotOutput(outputId = ns("overview_plot"), height = "800px")
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'Crt Distribution'", ns("select_view")
                    ),
                    plotOutput(outputId = ns("crt_plot"), height = "800px")
                ),
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] == 'Run QC'", ns("select_view")
                    ),
                    plotlyOutput(outputId = ns("qc_plot"), height = "600px")
                )
            )
        )
    )
}

#' Graphics Server
#'
#' @param id identifier unique to `Graphics` page
#' @param se SummarizedExperiment with OpenArray assay data
#'
#' @returns The `Graphics` server component
#' 
#' @import shiny
#' 
#' @importFrom plotly renderPlotly
.serverGraphics <- function(id, se) {
    moduleServer(id, function(input, output, session) {
        
        # render categorial matrix plot summarizing amp status
        output$overview_plot <- renderPlot({
            req(se())
            plotOverview(se())
        })
        
        # render box and whisker plot summarizing Crt distribution
        output$crt_plot <- renderPlot({
            req(se())
            plotCrt(se())
        })
        
        # render 3D plot summarizing QC metrics
        output$qc_plot <- renderPlotly({
            req(se())
            plotQC(se())
        })
    })
}
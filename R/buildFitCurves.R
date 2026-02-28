#' Curve Fitting UI
#'
#' @param id identifier unique to `Fit Curves` page
#'
#' @returns The `Fit Curves` page
#' 
#' @import shiny
.uiFitCurves <- function(id) {
    ns <- NS(id)
    tabPanel("Fit Curves",
        titlePanel("Fit 5PL Curves to the PCR Data"),
        sidebarLayout(
            sidebarPanel(
                numericInput(
                    inputId = ns("select_lin_threshold"),
                    label = "Select Linear Threshold",
                    value = 400,
                    min = 0,
                    max = 1000
                ),
                p("Please select a value for the linear threshold, which 
                    determines the cutoff value below which all curves will
                    automatically be marked NDET for the amplicon tested."),
                selectInput(
                    inputId = ns("select_fluo"),
                    label = "Select Fluorescence Readout",
                    choices = c("fluo_reporter"),
                    selected = "fluo_reporter"
                ),
                p("The reporter dye fluorescence is selected as it offers 
                    the greatest resolution with which to distinguish real 
                    amplification against false positives."),
                actionButton(
                    inputId = ns("fit_curves"),
                    label = "Run Curve Fitting"
                ),
                p("Run the curve fitting algorithm! This step may take up to
                    a few minutes to run."),
                selectInput(
                    inputId = ns("select_well"),
                    label = "Select well for display",
                    choices = c()
                )
            ),
            mainPanel(
                plotOutput(
                    outputId = ns("model_plot"),
                    height = "600px"
                )
            )
        )
    )
}

#' Curve Fitting Server
#'
#' @param id identifier unique to `Fit Curves` page
#' @param se SummarizedExperiment with OpenArray assay data
#'
#' @returns The `Fit Curves` server component
#' 
#' @import shiny
.serverFitCurves <- function(id, se) {
    moduleServer(id, function(input, output, session) {
        
        # run curve fitting on button press
        se_fit <- eventReactive(input$fit_curves, {
            req(se())
            showModal(modalDialog("Running curve fitting..."))
            result <- se() |> computeModels(
                assay_name = input$select_fluo,
                linear_threshold = input$select_lin_threshold
            )
            removeModal()
            result
        })
        
        # update well selection
        observe({
            req(se_fit())
            updateSelectInput(
                session = session,
                inputId = "select_well",
                choices = colnames(assay(se_fit()))
            )
        })
        
        # build 5PL model plot
        output$model_plot <- renderPlot({
            req(se_fit())
            req(input$select_well)
            plotModel(
                se_fit(),
                well_id = input$select_well,
                assay_name = "fluo_reporter",
                include_mdpt_tangent = TRUE,
                include_coldata_annotation = TRUE
            )
        })
        
        return(se_fit)
    })
}
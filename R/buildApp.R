options(shiny.maxRequestSize = 30 * 1024^2)

#' A Shiny App for OAtools
#' 
#' Builds a Shiny application for interactively running OAtools
#'
#' @returns A Shiny web application
#' 
#' @import shiny
#' 
#' @export
#' 
#' @examples
#' app <- buildApp()
buildApp <- function() {
    ui <- .buildUI()
    server <- function(input, output, session) {
        .buildServer(input, output, session)
    }
    app <- shinyApp(ui = ui, server = server)
}

#' User Interface
#' 
#' Helper method to buildApp() responsible for construction
#' of the graphical user interface
#'
#' @returns A Shiny UI
#' 
#' @import shiny
.buildUI <- function() {
    navbarPage(
        "OATools",
        .uiDataImport("dataImport"),
        .uiGraphics("graphics"),
        .uiReporting("reporting")
    )
}

#' Web Server
#' 
#' Helper method to buildApp() responsible for construction
#' of the Shiny web server
#' 
#' @param input reactive values from UI widgets
#' @param output render functions for results
#' @param session environment for user session
#'
#' @returns A Shiny web server
#' 
#' @import shiny
.buildServer <- function(input, output, session) {
    se <- .serverDataImport("dataImport")
    .serverGraphics("graphics", se)
    .serverReporting("reporting", se)
}
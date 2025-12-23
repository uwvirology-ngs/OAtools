utils::globalVariables("fit_curve")

#' Run curve-fitting function
#'
#' An R wrapper for the internal python function, fit_curve(), which attempts 
#' to fit two-dimensional multicomponent fluorescence vs. cycle number data to 
#' a 5-parameter logistic regression. For reactions with a total change in 
#' fluorescence below the linear_threshold, returns instead a linear regression
#'
#' @param data A tibble of multicomponent fluorescence data vs. 
#' cycle number for one reaction
#' @param linear_threshold An integer describing the maximum overall 
#' change in fluorescence at which fluorescence curves will be trivially 
#' fit to a linear regression
#'
#' @returns A list object describing the model
#' @export
#'
#' @examples
#' data(example_se)
#' 
#' df <- as.data.frame(SummarizedExperiment::assay(example_se)) |> 
#'     dplyr::select(well_2385)
#' 
#' model <- data.frame(
#'     cycle = as.integer(sub("cycle_", "", rownames(df))),
#'     fam = as.numeric(df$well_2385)
#' )
runFitCurve <- function(data, linear_threshold) {
    basilisk::basiliskRun(
        env = OAtools_env,
        fun = function() {
            reticulate::source_python(system.file(
                "python", 
                "fit_curve.py", 
                package = "OAtools"
            ))
            return(fit_curve(data, linear_threshold))
        }
    )
}
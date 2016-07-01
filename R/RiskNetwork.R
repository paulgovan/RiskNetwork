#' Risk network modeling and analysis.
#'
#' RiskNetwork is a Shiny web application for risk network modeling and
#' analysis.
#' @import shiny
#' @import shinydashboard
#' @export
RiskNetwork <- function() {
  shiny::runApp(system.file('rn', package='RiskNetwork'))
}

#' Risk network modeling and analysis.
#'
#' RiskNetwork is a Shiny web application for risk network modeling and
#' analysis.
#' @import bnlearn
#' @import d3heatmap
#' @import lattice
#' @import networkD3
#' @import rhandsontable
#' @import shiny
#' @import shinydashboard
#' @export
#' @seealso \url{http://paulgovan.github.io/RiskNetwork/}
#' @examples
#' if (interactive()) {
#'   RiskNetwork()
#' }
RiskNetwork <- function() {
  shiny::runApp(system.file('rn', package='RiskNetwork'))
}

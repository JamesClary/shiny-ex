#' Module for UI outputs
#'
#' Module for UI outputs
#'
#' @param id Namespace
#'
#' @returns Shiny Module

mod_ui_outputs <- function(id){

  ns <- NS(id)

  tagList(
    layout_columns(
      card(
        card_header(HTML('<b>Simulated PK Profile')),
        card_body(plotOutput(outputId = ns('simPlot')))
      ),
      card(
        card_header(HTML('<b>Simulated PD Profile')),
        card_body(plotOutput(outputId = ns('simPlotE')))
        )
    ),
    card(
      card_header(HTML('<b>Simulated Dose vs Time Plot')),
      card_body(plotOutput(outputId = ns('simPlotDose'))))
  )

}

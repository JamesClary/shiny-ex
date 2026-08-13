#' Module for UI inputs
#'
#' Module for UI inputs
#'
#' @param id Namespace
#'
#' @returns Shiny Module

mod_ui_inputs <- function(id){

  ns <- NS(id)

  tagList(
    h4('Simulation Details'),
    hr(),
    tags$b('Number of Simulations'),
    numericInput(
      inputId = ns("nsim"),
      label = NULL,
      value = 10,
      min = 1,
      max = 1000
    ),
    tags$b("Adaptive Dosing List (Length 5)"),
    textInput(
      inputId = ns('dosevec'),
      label = NULL,
      value = '25, 50, 75, 100, 125'
    ),
    tags$b("Interdose Interval"),
    numericInput(
      inputId = ns('interval'),
      label = NULL,
      value = 24
    ),
    tags$b("Simulation Time"),
    numericInput(
      inputId = ns('simtime'),
      label = NULL,
      value = 336
    ),
    conditionalPanel(
      condition = "input.nsim > 1",
      tags$b('Prediction Interval'),
      selectInput(
        inputId = ns('perc'),
        label = NULL,
        choices = c('80', '90', '95'),
        selected = '90',
        multiple = FALSE
      ),
      ns = NS(id)
    ),
    br(),
    h4('Population Covariate'),
    hr(),
    tags$b('Weight (kg)'),
    fluidRow(
      column(6,
             numericInput(
               inputId = ns('WTKGmin'),
               label = 'Minimum Weight',
               value = 50
             ),
      ),
      column(6,
             numericInput(
               inputId = ns('WTKGmax'),
               label = 'Maximum Weight',
               value = 90
               )
             )
      ),

    actionBttn(
      inputId = ns('btn'),
      label = 'Simulate'
    )
    )


}

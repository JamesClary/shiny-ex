library(shiny)
library(shinyMatrix)
library(shinyWidgets)
library(dplyr)
library(mrgsolve)
library(mrgmisc)
library(PKNCA)
library(knitr)
library(kableExtra)
library(ggplot2)
library(bslib)
library(truncnorm)
# library(survival)
# library(survminer)

run_app <- function() {

  set.seed(12345)

  light <- bs_theme(version = 5)

  shinyApp(

    #Application UI
    ui = tagList(
      fluidPage(
        navbarPage(
          title = 'Adaptive Dosing Simulations',
          theme = light,
          h5('For Internal discussion ONLY'),
          h5('Based on Leeds, et al. Antimicrob Agents Chemother. 2013 Mar;57(3):1136–1143')
        ),
        br(),
        sidebarLayout(
          sidebarPanel(
            mod_ui_inputs('sim')
          ),
          mainPanel(
            mod_ui_outputs('sim')
          )
        )
      )
    ),

    server = function(input, output, session){

      set.seed(12345)
      dosetab <- mod_server_mkdoseev('sim')
      mod_out <- mod_server_runsim('sim', dosedat = dosetab)
      mod_server_mkplot('sim', simdat_raw = mod_out)

    }

  )

}

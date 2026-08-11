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

run_app <- function() {

  shinyApp(

    #Application UI
    ui = tagList(
      fluidPage(
        navbarPage(
          title = 'FIH Simulator',
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
      mod_server_dosetable('sim')
      dosetab <- mod_server_mkdoseev('sim')
      mod_out <- mod_server_runsim('sim', dosedat = dosetab)
      mod_server_mkplot('sim', simdat_raw = mod_out)
      mod_server_mktable('sim', simdat_raw = mod_out)

    }

  )

}

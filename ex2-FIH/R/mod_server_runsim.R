#' Simulation Server
#'
#' @param id Namespace
#' @param dosedat dosing data.frame
#'
#' @returns Shiny server module


mod_server_runsim  <- function(id, dosedat){

  moduleServer(id, function(input, output, session){

    mod <- eventReactive(input$modtype, {

      if(input$modtype == 'Infected'){
        mods <- mread(system.file('extdata', 'FIH-sim2.cpp', package = 'ex2-FIH'))
      } else {
        mods <- mread(system.file('extdata', 'FIH-sim.cpp', package = 'ex2-FIH'))
      }

      return(mods)

    })

    sim_dataframe <- eventReactive(input$btn, {

      df <- dosedat()

      if(input$nsim == 1){

        out <- mod() %>%
          data_set(df) %>%
          zero_re()%>%
          mrgsim_df(tgrid = seq(0, input$simtime, 0.1), carry_out = c('ii2', 'group'))

      } else {

        out <- mod() %>%
          data_set(df) %>%
          mrgsim_df(tgrid = seq(0, input$simtime, 0.1), carry_out = c('ii2', 'group'))

      }

      return(out)



    })
    return(reactive({sim_dataframe()}))

  })
}

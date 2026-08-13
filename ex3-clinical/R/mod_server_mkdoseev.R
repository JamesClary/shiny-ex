#' Dosing Server
#'
#' @param id Namespace
#'
#' @returns Shiny server module


mod_server_mkdoseev  <- function(id){

  moduleServer(id, function(input, output, session){

    dosing_dataframe <- eventReactive(input$btn, {

      minwtkg <- input$WTKGmin
      maxwtkg <- input$WTKGmax

      wtkg <- rtruncnorm(input$nsim, a = minwtkg, b = maxwtkg, mean = 70, sd = 5)

      dosing <- data.frame(matrix(nrow = 0, ncol = 11))

      names(dosing) <- c('ID', 'time', 'amt', 'cmt', 'evid', 'WTKG')

      dosetimes <- seq(0, input$simtime, input$interval)

      for(i in 1:input$nsim){

          temp <- data.frame(ID = i,
                             time = dosetimes,
                             amt = 1,
                             evid = 1,
                             cmt = 1,
                             WTKG = rep(wtkg[i], length(dosetimes)))

          dosing <- rbind(dosing, temp)

        }

      return(dosing)

    })


    return(reactive({dosing_dataframe()}))

  })

}

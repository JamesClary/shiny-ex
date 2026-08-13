#' Dosing Server
#'
#' @param id Namespace
#'
#' @returns Shiny server module


mod_server_mkdoseev  <- function(id){

  moduleServer(id, function(input, output, session){

    dosing_dataframe <- eventReactive(input$btn, {

      df <- input$dosetab

      df2 <- df %>%
        as.data.frame() %>%
        mutate(Dose = as.numeric(as.character(Dose)),
               Doses = as.numeric(as.character(Doses)),
               Interval = as.numeric(as.character(Interval)))

      dosing <- data.frame(matrix(nrow = 0, ncol = 11))

      names(dosing) <- c('ID', 'time', 'amt', 'rate', 'ii2', 'evid', 'cmt', 'WTKG', 'DOSE', 'DOSEKG', 'group')

      for(i in 1:nrow(df)){

        dosing1 <- data.frame()

        dosetimes <- seq(0, as.numeric(unname(df2[i, 2])) * as.numeric(unname(df2[i, 3])), as.numeric(unname(df2[i, 3])))
        dosetimes <- dosetimes[1:(length(dosetimes)-1)]

        for(j in 1:input$nsim){

          temp <- data.frame(ID = j+(i-1)*1000,
                             time = dosetimes,
                             amt = df2[i,1],
                             ii2 = df2[i, 3],
                             evid = 1,
                             cmt = 1,
                             WTKG = input$WTKG,
                             DOSE = df2[i,1],
                             DOSEKG = df2[i,1]/input$WTKG,
                             group = i)

          dosing1 <- rbind(dosing1, temp)

        }

        dosing <- rbind(dosing, dosing1)

      }

      return(dosing)

    })


    return(reactive({dosing_dataframe()}))

  })

}

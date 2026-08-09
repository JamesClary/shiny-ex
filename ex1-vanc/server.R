#Reuse of Wojeciechowski et al for demonstration purposes
#Assessment of optimal initial dosing regimen with vancomycin pharmacokinetics model in very low birth weight neonates
#Author Hideo Kato

#Load package libraries
library(shiny)
library(deSolve)
library(ggplot2)

#Function containing differential equations for amount in each compartment	
DES <- function(T, A, THETA) {
  
  KA  <- THETA[1]
  K10 <- THETA[2]                                   			
  
  dA <- vector(length = 2)
  dA[1] = -KA*A[1]			#Absorption compartment
  dA[2] =  KA*A[1] -K10*A[2]	#Central compartment  
  
  list(dA)			
}

#Make a TIME range (0 to 240 hours [10 days] at increments of 0.1 hours)
#To change the duration evaluated time-period, only the value for the "to" argument
#needs to be altered in the whole script
TIME <- seq(from = 0, to = 240, by = 0.1)

#TIMElast is used in later functions for assigning dose events	
TIMElast <- max(TIME)

#-----------------------------------------------------------------------------------
#Define user-input dependent functions for output	
shinyServer(function(input, output) {
  
  #Reactive expression to generate the plot
  #This is called whenever the input changes
  sim.data <- reactive({
    
    #Input patient data
    SCR <- input$SCR
    INF <- input$INF
    
    #Make a parameter vector for input into DES function
    #Exponent values are large to have greater impact on the plot when input changes
    KA <- 1
    CL <- 0.054*((SCR/0.59)^-0.8)*((INF/159.3)^0.98)
    V <- 1.19
    
    #Calculate rate constants for the differential equations
    KA <- KA
    K10 <- CL/V
    
    #Parameter vector
    THETAlist <- c(KA,K10)
    
    #----------------------------------------------------------------------------------
    #Input doses specific to dosing frequency
    #Input dosing regimen
    FREQ <- input$FREQ
    
    #Input prescribed dose
    DOSE <- input$DOSE
    
    #Once daily dosing		
    if (FREQ == 1) {
      
      ndoses <- TIMElast/24 + 1
      
      #Dose event data for once daily dosing (for deSolve)
      DOSEdata <- data.frame(var    = rep(1, times = ndoses),
                             time   = seq(0,TIMElast,24),
                             value  = rep(DOSE, times = ndoses),
                             method = rep("add", times = ndoses))
      

      
    }
    
    #Twice daily dosing					
    if (FREQ == 2) {
      
      ndoses <- 2*TIMElast/24 + 1
      
      #Dose event data for twice daily dosing	
      DOSEdata <- data.frame(var    = rep(1, times = ndoses),
                             time   = seq(0,TIMElast,12),
                             value  = rep(DOSE, times = ndoses),
                             method = rep("add", times = ndoses))
      

    }
    
    #Three times daily dosing					
    if (FREQ == 3) {
      
      ndoses <- 3*TIMElast/24 + 1
      
      #Dose event data for twice daily dosing	
      DOSEdata <- data.frame(var    = rep(1, times = ndoses),
                             time   = seq(0,TIMElast,8),
                             value  = rep(DOSE, times = ndoses),
                             method = rep("add", times = ndoses))

      
    }		
    
    #Set initial conditions in each compartment
    A_0 <- c(A1 = 0, A2 = 0)
    
    #Run differential equation solver (deSolve package)	
    sim.data.df <- lsoda(A_0, TIME, DES, THETAlist, events = list(data=DOSEdata))
    
    #Process the simulated output	
    sim.data.df <- as.data.frame(sim.data.df)
    sim.data.df$CONC <- sim.data.df$A2/V
    sim.data.df$DAYS <- sim.data.df$time/24
    
    sim.data.df <- as.data.frame(sim.data.df)
    
  })	#Brackets closing "reactive" expression
  
  #----------------------------------------------------------------------------------
  #Generate a plot of the data
  #Also uses the inputs to build the plot (ggplot2 package)	
  output$plotCONC <- renderPlot({	
    
    plotobj <- ggplot(sim.data()) + theme_bw()
    plotobj <- plotobj + geom_abline(aes(slope = 0, intercept = 15), linetype = "dashed", size = 1)
    plotobj <- plotobj + geom_abline(aes(slope = 0, intercept = 20), linetype = "dashed", size = 1)
    plotobj <- plotobj + geom_line(aes(x = DAYS, y = CONC), colour = "red", size = 1)
    plotobj <- plotobj + annotate("text", x = 9, y = 13, label = "Lower range", colour = "black", size = 6)
    plotobj <- plotobj + annotate("text", x = 9, y = 22, label = "Upper Range", colour = "black", size = 6)
    plotobj <- plotobj + scale_y_continuous("Concentration (mg/L) \n", lim = c(0,32))
    plotobj <- plotobj + scale_x_continuous("\nTime (days)", breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))
    print(plotobj)
    
  })	#Brackets closing "renderPlot" expression
  
})	#Brackets closing "shinyServer" function

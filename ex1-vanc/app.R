source('server.R')
shiny::shinyApp(
  ui = fixedPage(
  
  #Logo and Application Title
  fixedRow(
    column(10,
           h2("What dose and dosing schedule gives you therapeutic concentrations", align = "center"), offset = 1)	
           ),	#Brackets closing "fixedRow"
  
  #Acknowledgemnt
  fixedRow(
    column(10,
           h4("Based on Kato et al. 2017 J Infect Chemother.", align = "center"), offset = 1),	
  ),	#Brackets closing "fixedRow"
  
  hr(),	#Add a break with a horizontal line
  
  #Sidebar Panel with Widgets
  sidebarLayout(
    sidebarPanel(
      
      #Heading
      h4("Patient Information"),
      
      #Slider input for patient weight
      sliderInput("SCR",
                  "Serum Creatanine (mg/dL):",
                  min = .29,
                  max = .95,
                  value = .62,
                  step = .03),
      
      #Slider input for patient age
      sliderInput("INF",
                  "Volume Infused (mL):",
                  min = 135,
                  max = 200,
                  value = 150,
                  step = 5),
      
      br(), #Add a blank space between sections
      
      #Heading
      h4("Dosing Information"),
      
      #Slider input for prescribed dose
      sliderInput("DOSE",
                  "Prescribed dose (mg/kg):",
                  min = 0,
                  max = 17,
                  value = 7,
                  step = 1),
      
      br(),
      
      #Selection box for dosing regimen
      selectInput("FREQ",
                  "Dose Frequency:",
                  choices = list("Once daily" = 1,
                                 "Twice daily" = 2,
                                 "Three times daily" = 3),
                  selected = 2),
      
      
      #Text
      h6("Helpful hint: refresh browser to reset values")
      
    ),	#Brackets closing "siderbarPanel"
    
    #Plot output	
    mainPanel(
      plotOutput("plotCONC", height = 600, width = 800)
      
    )	#Brackets closing "mainPanel"
    
  )	#Brackets closing "sidebarLayout"
  
  ),	#Brackets closing "fixedPage"
  server = function(input, output) {
  
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
  
}
)

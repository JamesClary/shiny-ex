#Reuse of Wojeciechowski et al for demonstration purposes

#Define UI for Preliminary Patient Education Tool Application (Example 2)
fixedPage(
  
  #Logo and Application Title
  fixedRow(
    column(10,
           h2("What dose and dosing schedule gives you therapeutic concentrations", align = "center"), offset = 1),	
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
  
  )	#Brackets closing "fixedPage"
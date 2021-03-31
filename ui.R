# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("GTFS Explorer"),
  helpText("v1.0"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      h4("Input Options:"),
      fileInput("selectFile", "Select GTFS File:",
                multiple = FALSE,
                accept = ".zip"),
      hr(),
      helpText("This tool is designed to extract, parse and analyze GTFS files. Files are parsed locally in a browser-based containter and analyzed."),
      hr(),
      helpText("PTA CEO Office | March 2021"),
      width = 2
    ),
    
    # Create a spot for the barplot
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           fluidRow(
                             valueBoxOutput("staticBox_modes", width = 2),
                             valueBoxOutput("staticBox_stops", width = 2),
                             valueBoxOutput("staticBox_routes", width = 2),
                             valueBoxOutput("staticBox_trips", width = 2)
                           ),
                           hr(),
                           leafletOutput("stopmap")),
                  tabPanel("Route Explorer",
                           helpText("Please note that the GTFS processing may take a few minutes to complete."),
                           fluidRow(
                             column(3,uiOutput("ModeDropDown")),
                             column(3,uiOutput("RouteDropDown")),
                             column(3,uiOutput("DirectionDropDown"))
                           ),
                           leafletOutput("routemap")),
                  tabPanel("Departure Board"
                           ,
                           column(6,dateInput("date","Select Date:"),
                           leafletOutput("map")),
                           column(6,plotOutput("departurePlot"))
                           ),
                  tabPanel("Validation",
                           tableOutput("validationTable"))
      )
    )
  )
)

library(shiny)

# Define UI for the dashboard.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cafe Data Predictor"),
  
  tabsetPanel(
    tabPanel("Customer Predictor",
  # Sidebar inputs used to predict the amount of customers at a given time.
  sidebarLayout(
    sidebarPanel(
      #tags$head(
      #  tags$script(src = "modifyMinute.js")
      #),
      
      # DayOfMonth input (numeric slider)
      sliderInput("dayOfMonth", "Day of the month:",
                  min = 1,
                  max = 31,
                  value = 1),
      br(),
      
      # DayOfWeek input (drop down)
      selectInput("dayOfWeek", "Day of the week:",
                   c("Monday" = "MONDAY",
                     "Tuesday" = "TUESDAY",
                     "Wednesday" = "WEDNESDAY",
                     "Thursday" = "THURSDAY",
                     "Friday" = "FRIDAY",
                     "Saturday" = "SATURDAY",
                     "Sunday" = "SUNDAY")),
      br(),
      
      # Temperature input (numeric text box)
      numericInput("temp", "Temperature (F):",
                   value = 0),
      br(),
      
      # Precipitation input (drop down)
      selectInput("prec", "Precipitation:",
                  c("Clear" = "Clear",
                    "Cloudy" = "Clouds",
                    "Drizzling" = "Drizzle",
                    "Fog" = "Fog",
                    "Misty" = "Mist",
                    "Raining" = "Rain",
                    "Snowing" = "Snow")),
      br(),
      
      # Minute input (double-ended numeric slider)
      sliderInput("time",
                  "Time",
                  min = strptime("00:00", "%H:%M", tz = "UTC"),
                  max = strptime("23:59", "%H:%M", tz = "UTC"),
                  #value = c(11*60, 13*60),
                  value = c(strptime("11:00", "%H:%M", tz = "UTC"), strptime("13:00", "%H:%M", tz = "UTC")),
                  step = 60*5,
                  timeFormat = "%H:%M",
                  timezone = "+0000")
      
      
      # Minute input
      #numericInput("hourLower", "Time Range", value = 11),
      #numericInput("minuteLower", "", value = 0),
      #numericInput("hourUpper", "", value = 1),
      #numericInput("minuteUpper", "", value = 0)
      
    ),
    
    # Create a panel to display both the plot and text outputs defined in the server logic.
    mainPanel(
      plotOutput("plot", width = "100%"),
      textOutput("text")
    )
  )
    ),
  tabPanel("Healthy Predictor"),
  tabPanel("Advertisement Predictor"),
  tabPanel("Demographic Predictor")
  )
))
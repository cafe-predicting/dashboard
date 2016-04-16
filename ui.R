library(shiny)

# Define UI for the dashboard.
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cafe Data Predictor"),
  
  # Create a panel to contain the tabs for each predictor.
  tabsetPanel(
    # Customer predictor: user enters values on the current time and weather and a plot is created containing
    # the predicted amount of customers.
    tabPanel("Customer Predictor",
      # Sidebar inputs used to predict the amount of customers at a given time.
      sidebarLayout(
        sidebarPanel(
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
          
          # Time input (double-ended datetime slider)
          # Uses only the time part of R's datetime object POSIXlt in the UTC timezone so that the user's timezone
          # does not affect the values. Steps the slider for every 5 minutes of time.
          sliderInput("time",
                      "Time",
                      min = strptime("00:00", "%H:%M", tz = "UTC"),
                      max = strptime("23:59", "%H:%M", tz = "UTC"),
                      value = c(strptime("11:00", "%H:%M", tz = "UTC"), strptime("13:00", "%H:%M", tz = "UTC")),
                      step = 60*5,
                      timeFormat = "%H:%M",
                      timezone = "+0000") # UTC
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
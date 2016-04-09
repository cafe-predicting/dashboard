library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Test Dashboard"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      sliderInput("dayOfMonth", "Day of the month:",
                  min = 1,
                  max = 31,
                  value = 1),
      br(),
      selectInput("dayOfWeek", "Day of the week:",
                   c("Monday" = "MONDAY",
                     "Tuesday" = "TUESDAY",
                     "Wednesday" = "WEDNESDAY",
                     "Thursday" = "THURSDAY",
                     "Friday" = "FRIDAY",
                     "Saturday" = "SATURDAY",
                     "Sunday" = "SUNDAY")),
      br(),
      numericInput("temp", "Temperature (F):",
                   value = 0),
      br(),
      selectInput("prec", "Precipitation:",
                  c("Clear" = "Clear",
                    "Cloudy" = "Clouds",
                    "Drizzling" = "Drizzle",
                    "Fog" = "Fog",
                    "Misty" = "Mist",
                    "Raining" = "Rain",
                    "Snowing" = "Snow")),
      br(),
      sliderInput("minute",
                  "Minute",
                  min = 0,
                  max = 24*60,
                  value = c(11*60, 13*60),
                  step = 5)
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      plotOutput("plot", width = "100%"),
      textOutput("text")
    )
  )
))
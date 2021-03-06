library(shiny)

# Define UI for the dashboard.
shinyUI(fluidPage(
  
  # JavaScript to submit forms on ENTER (specific to these input ids).
  # Add CSS to change the font color of the login and sign up error messages, and sign up's display message.
  tags$head(
    tags$script(src = "formSubmit.js"),
    tags$style("#loginErrorMessage { color: red; }"),
    tags$style("#signupErrorMessage { color: red; }"),
    tags$style("#signupDisplayMessage { color: #428bca; }"),
    tags$style("#healthyText { text-align: center; margin-top: 12px; }"),
    tags$style("#advertiseText { text-align: center; margin-top: 12px; }")
  ),
  
  # Application title
  titlePanel("Cafe Data Predictor"),
  
  # Output the user's login status as a div (used with conditionalPanels)
  textOutput("loggedIn"),
  
  # Hide the div created above.
  # Note: display: 'none' cannot be used as the javascript expressions used by the conditionalPanels below will not be able to read 
  # the value for loggedIn.
  tags$script("
    $('#loggedIn').css('height', '0');
    $('#loggedIn').css('overflow', 'hidden');
  "),
  
  # Displays that the user is logged in and their username.
  textOutput("loginStatus"),
  
  # This panel is only displayed if the user is not logged in (Login screen)
  conditionalPanel(condition = "output.loggedIn == '0'", # This condition parameter requires a javascript expression.
    tabsetPanel(
      # Login page
      tabPanel("Login",
        mainPanel(
          # Username and password input with a submit button
         textInput("loginUsername", "Username:"),
         br(),
         passwordInput("loginPassword", "Password:"),
         actionButton("loginButton", "Submit"),
         br(), br(),
         # Used to display if the user's credintials were incorrect.
         textOutput("loginErrorMessage")
        )
      ),
      # Sign up page
      tabPanel("Sign Up",
        mainPanel(
          textInput("signupUsername", "Username:"),
          br(),
          passwordInput("signupPassword1", "Password:"),
          br(),
          passwordInput("signupPassword2", "Re-enter password:"),
          #br(),
          actionButton("signupButton", "Sign Up"),
          br(), br(),
          htmlOutput("signupErrorMessage"),
          htmlOutput("signupDisplayMessage")
        )
      )
    )
  ),
  
  # This panel is only displayed if the user is logged in (Predictors)
  conditionalPanel(condition = "output.loggedIn == '1'",
  
    actionLink("logoutButton", "Logout"),
    tags$script("
      $('#logoutButton').css('float', 'right');
      $('#logoutButton').css('display', 'in-line');
    "),

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
      # Healthy predictor: user enters values on a customer to see the probability that they will buy a healthy item.
      tabPanel("Healthy Predictor",
        sidebarLayout(
          sidebarPanel(
            # DayOfWeek input (drop down)
            selectInput("healthyDayOfWeek", "Day of the week:",
                        c("Monday" = "Monday",
                          "Tuesday" = "Tuesday",
                          "Wednesday" = "Wednesday",
                          "Thursday" = "Thursday",
                          "Friday" = "Friday")),
            br(),
            # Hour input (numeric slider)
            sliderInput("healthyHour","Hour of day:",
                        value = 12,
                        min = 0, max = 23),
            br(),
            # Gender input (drop down)
            selectInput("healthyGender","Gender:",
                        c("Female" = "Female",
                          "Male" = "Male")),
            br(),
            # Age input (drop down)
            selectInput("healthyAge","Age:",
                        c("Adult" = "Adult",
                          "Child" = "Child",
                          "Senior" = "Senior",
                          "Young Adult" = "Young Adult")),
            br(),
            # Advertised Item's Health input (drop down)
            selectInput("healthyAdvHealth","Advertised Item's Healthiness:",
                        c("Healthy" = "Healthy",
                          "Unhealthy" = "Unhealthy")),
            br(),
            # Advertised Item's Temperature input (drop down)
            selectInput("healthyAdvTemp","Advertised Item's Temperature:",
                        c("Cold" = "Cold",
                          "Hot" = "Hot")),
            br(),
            # Precipitation input (drop down)
            selectInput("healthyPrecipitation", "Precipitation outside:",
                        c("Clear" = "Clear",
                          "Cloudy" = "Clouds",
                          "Drizzling" = "Drizzle",
                          "Fog" = "Fog",
                          "Misty" = "Mist",
                          "Raining" = "Rain",
                          "Snowing" = "Snow"))
          ),
          # Create a panel to display the output of the predictor
          mainPanel(
            htmlOutput("healthyText")
          )
        )
      ),
      tabPanel("Advertisement Predictor",
        sidebarLayout(
          sidebarPanel(
            #Advertised Item Temperature input (drop down)
            selectInput("advProdTemp", "Advertised Item Temperature:",
                          c("Hot" = "Hot",
                            "Cold" = "Cold")),
            br(),
            #Advertised Item Health input (drop down)
            selectInput("advProdHealth", "Advertised Item Health:",
                          c("Healthy" = "Healty",
                            "Unhealthy" = "Unhealthy"))
          ),
          # Main Panel Displays Output of Prediction
          mainPanel(
            htmlOutput("advertiseText")
          )
        )
      )
    )
  )
))

library(shiny)
library(PKI)
source("customerPredictor.R")
source("healthyPredictor.R")

# Define server logic for the dashboard.
shinyServer(function(input, output, session) {
  
  # Create a SHA256 key
  key <- PKI.digest(charToRaw("cafe-data-predicting"), "SHA256")
  
  # Set the default user values for their session to being logged out.
  # Note: This method for checking a user's login status is needed as Shiny does not support user permissions without purchasing Shiny Server Pro.
  output$loggedIn <- renderText({paste("0")})
  output$loginStatus <- renderText({paste("")})
  loggedIn <- FALSE
  
  # Executes everytime the login button is pressed
  login <- observeEvent(input$loginButton, {
    # Check if user is logged in:
    if (!is.null(input$loginUsername) && !loggedIn) {
      # Encrypt the username and password the user enters with the key defined above and save as a string.
      username <- paste(PKI.encrypt(charToRaw(input$loginUsername), key, "aes256"), collapse = "")
      password <- paste(PKI.encrypt(charToRaw(input$loginPassword), key, "aes256"), collapse = "")
      
      # Read all users into a data frame object.
      users <- as.data.frame(read.csv("users.csv"))
      
      # Run through the users data frame object.
      for (i in 1:nrow(users)) {
        # Check that the entered username and password match the current entry
        if (identical(username, as.character(users$Username[i])) && identical(password, as.character(users$Password[i]))) {
          # If they do, set the client's variables to logged in status.
          output$loggedIn <- renderText({paste("1")})
          loggedIn <- TRUE
          output$loginStatus <- renderText({paste("Welcome ", input$loginUsername, "!", sep = "")})
        }
      }
      
      # If the user is still not logged in after checking each username/password in the users data frame, output that
      # their username/password combo does not match any existing users.
      if (!loggedIn) {
        output$loginErrorMessage <- renderText({
          paste("Incorrect username or password.")
        })
      }
    }
  })
  
  # Executes everytime the sign up button is pressed
  signUp <- observeEvent(input$signupButton, {
    # Check if user is logged in:
    if (!is.null(input$signupUsername) && !loggedIn) {
      
      # String used to build the final display message output for the sign up page.
      displayMessage <- ""
      errorMessage <- ""
      
      # Encrypt the username with the key defined above and save as a string.
      username <- paste(PKI.encrypt(charToRaw(input$signupUsername), key, "aes256"), collapse = "")
      
      validUsername <- TRUE
      if (nchar(input$signupUsername) < 4) {
        validUsername <- FALSE
        errorMessage <- paste(errorMessage, "Username must be 4 or more characters.<br/>", sep = "")
      }
      
      # Read all users into a data frame object.
      users <- as.data.frame(read.csv("users.csv"))
      
      newUser <- TRUE
      # Run through the users data frame object.
      for (i in 1:nrow(users)) {
        # Check if the username the user wants to sign up with is already registered
        if (identical(username, as.character(users$Username[i]))) {
          # This username is taken, so display to this message to the user and exit the loop.
          newUser <- FALSE
          errorMessage <- paste(errorMessage, input$signupUsername, " is already taken.<br/>", sep = "")
          break
        }
      }
      
      # Encrypt the two passwords the user entered so they may be compared and saved.
      password1 <- paste(PKI.encrypt(charToRaw(input$signupPassword1), key, "aes256"), collapse = "")
      password2 <- paste(PKI.encrypt(charToRaw(input$signupPassword2), key, "aes256"), collapse = "")
      
      # Checks that the passwords match and that it is at least 8 characters.
      validPassword <- FALSE
      if (identical(password1, password2)) {
        # Calculates the number of characters in the actual password, not the encrypted one.
        if (nchar(input$signupPassword1) >= 8) {
          validPassword <- TRUE
        } else {
          errorMessage <- paste(errorMessage, "Password must be at least 8 characters.<br/>", sep = "")
        }
      } else {
        errorMessage <- paste(errorMessage, "Passwords do not match.<br/>", sep="")
      }
      
      # Executed if the username is not already in the users database and both passwords match and are at least 8 characters.
      if (newUser && validUsername && validPassword) {
        # Add the new entry to the users.csv file.
        write(paste(username, ",", password1, sep=""), file="users.csv", append=TRUE)
        # Notify the user their account has been added.
        displayMessage <- paste(displayMessage, "Your account has registered successfully!<br/>", sep="")
        
        # log the user in?
      }
      
      # Set the display and error messages.
      output$signupErrorMessage <- renderUI({HTML(paste(errorMessage))})
      output$signupDisplayMessage <- renderText({HTML(paste(displayMessage))})
    }
  })
  
  # Executed when the user presses the log out button.
  logout <- observeEvent(input$logoutButton, {
    output$loggedIn <- renderText({paste("0")})
    loggedIn <- FALSE
    output$loginStatus <- renderText({paste("Logout successful!")})
  })
  
  # Reactive expression called whenever the inputs changed.
  # Used to calculate the minutes set and the machine learning function.
  data <- reactive({
    # Default temperature value
    temperature <- 0
    
    # Checks that the user entered a value temperature
    if (!is.na(input$temp)) {
      temperature <- input$temp
    }
    
    # Convert the time input from a POSIXct object to an integer of minutes for the upper and lower bounds of the range.
    minuteLower <- as.numeric((as.POSIXlt(input$time[1])$hour * 60)) + as.numeric(as.POSIXlt(input$time[1])$min)
    minuteUpper <- as.numeric((as.POSIXlt(input$time[2])$hour * 60)) + as.numeric(as.POSIXlt(input$time[2])$min)
    
    # Create a vector of integers from minuteLower to minuteUpper.
    minutes <- c(minuteLower)
    if (minuteLower + 1 < minuteUpper) {
      for (i in (minuteLower+1):minuteUpper) {
        minutes <- append(minutes, i)
      }
    } else if (minuteLower + 1 == minuteUpper) {
      minutes <- append(minutes, minuteUpper)
    }
    
    # Transform this vector of minutes into a data frame.
    customerData <- as.data.frame(minutes)
    
    # Apply the function predictCustomerAmount() to every minute in the data frame (where x is the minute)
    customerData$customerCount <-
      apply(as.data.frame(minutes), 1, 
            function(x) predictCustomerAmount(input$dayOfWeek, x, temperature, input$prec))
    
    # predictCustomerAmount calculates the amount of customers over the next 15 minutes from the minute inputted
    # so divide this number by 15.
    customerData$customerCount <- customerData$customerCount / 15
    
    # "Output" the data so that it is put into the 'data' variable.
    customerData
  })
  
  healthyData <- reactive({
    truePrediction <- healthyPredictor(TRUE,
      input$healthyDayOfWeek,
      input$healthyHour,
      input$healthyGender,
      input$healthyAge,
      input$healthyAdvHealth,
      input$healthyAdvTemp,
      input$healthyPrecipitation)
    
    falsePrediction <- healthyPredictor(FALSE,
     input$healthyDayOfWeek,
     input$healthyHour,
     input$healthyGender,
     input$healthyAge,
     input$healthyAdvHealth,
     input$healthyAdvTemp,
     input$healthyPrecipitation)
    
    abs(truePrediction - falsePrediction)
  })
  
  # Generate a graph of the data that is gathered in the 'data' variable above.
  output$plot <- renderPlot({
    # Sets the max y-limit for the graph to 5, unless there is greater than 5 customers predicted in one minute.
    maxLim <- ifelse(max(data()$customerCount) > 5, max(data()$customerCount), 5)
    
    # Output the plot.
    # Outputs time (x-axis) vs customer amount (y-axis) as a line graph.
    # Sets the graph's title and labels, and turns their axes off (to be defined later).
    plot(data(), 
         type="l", 
         ylim=c(0,maxLim), 
         main="Predicted Amount of Customers", 
         xlab="Time", ylab="Amount of Customers (per minute)", 
         axes = FALSE,
         col = "#428bca")
    
    # Create a subset of what ticks will be used for the x-axis. Creates a tick for every 15 minutes.
    ticks <- subset(data()$minutes, ((data()$minutes - floor(data()$minutes/60)*60)) %% 15 == 0)
    
    # Check if the previous subset call did not return any ticks
    if (length(ticks) == 0) {
      # Only a single minute is selected
      minute <- min(data()$minutes)
      # Create a tick before the single minute
      ticks <- c(minute - (minute %% 15))
    } else {
      # If the lower bound of the time is not on a quarter hour, prepends a tick before the value.
      if (min(ticks) > min(data()$minutes)) {
        ticks <- append(min(ticks) - 15, ticks)
      }
      
      # If the upper bound of the time is not on a quarter hour, append a tick after the value.
      if (max(ticks) < max(data()$minutes)) {
        ticks <- append(ticks, max(ticks) + 15)
      }
    }
    
    # Create the x-axis with the defined ticks and a custom label to convert the amount of minutes to a HH:MM format.
    axis(1, at = ticks,
         labels = paste(floor(ticks/60), ":",
                        formatC(ticks - (floor(ticks/60)*60), width = 2, format = "d", flag = "0"),
                        sep = ""))
    
    # Creates the y-axis from 0 to maxLim
    axis(2, at = 0:maxLim)
    
    # Used to draw the left and right lines of a box over the x- and y-axes (there is a problem of the two axes not touching)
    box(bty = "L")
  })
  
  # Sum up all the customers predicted for each minute and output it as text below the plot.
  output$text <- renderText({
    paste("Total amount of customers: ", format(sum(data()$customerCount), digits = 6), sep = "")
  })
  
  output$healthyText <- renderUI({
    # Default values for text output - should be overriden
    textColor <- "red"
    textContent <- "Unexpected error"
    
    # Probability value gathered from healthyPredictor function in reactive expression above.
    probability <- healthyData() * 100
    # If greater than 50% chance, we'll say the customer will buy a healthy item
    # Output this probability chance to the user
    if (probability >= 50) {
      textColor <- "green"
      textContent <- "Bought healthy food!"
    # If less than 50% chance, we'll say the customer will not buy a healthy item
    } else {
      textColor <- "#FF6D0C" # shade of orange
      textContent <- "Bought all unhealthy food."
      # Invert the chance for probability the customer will not buy healthy.
      probability <- 100 - probability
    }
    
    # Bound predictions to actual percentages.
    if (probability < 0) {
      probability <- 0
    }
    else if (probability > 100) {
      probability <- 100
    }
    
    # Output the information gathered above using the correct font color and message
    HTML(paste(
      "<p><span style='color:", textColor, "; font-size: 16pt;'>",
        textContent,
      "</span><p>",
      "<p>Probability: ", format(probability, digits = 5), "%</p>", sep = ""))
  })
})
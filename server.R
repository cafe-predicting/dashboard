library(shiny)
source("customerPredictor.R")

# Define server logic for the dashboard.
shinyServer(function(input, output) {
  
  # Reactive expression called whenever the inputs changed.
  # Used to calculate the minutes set and the machine learning function.
  data <- reactive({
    # Convert the time input from a POSIXct object to an integer of minutes for the upper and lower bounds of the range.
    minuteLower <- as.numeric((as.POSIXlt(input$time[1])$hour * 60)) + as.numeric(as.POSIXlt(input$time[1])$min)
    minuteUpper <- as.numeric((as.POSIXlt(input$time[2])$hour * 60)) + as.numeric(as.POSIXlt(input$time[2])$min)
    
    # Create a vector of integers from minuteLower to minuteUpper.
    minutes <- c(minuteLower)
    if (minuteLower + 1 < minuteUpper) {
      for (i in (minuteLower+1):minuteUpper) {
        minutes <- append(minutes, i)
      }
    } else {
      minutes <- append(minutes, minuteUpper)
    }
    
    # Transform this vector of minutes into a data frame.
    customerData <- as.data.frame(minutes)
    
    # Apply the function predictCustomerAmount() to every minute in the data frame (where x is the minute)
    customerData$customerCount <-
      apply(as.data.frame(minutes), 1, 
            function(x) predictCustomerAmount(input$dayOfMonth, input$dayOfWeek, x, input$temp, input$prec))
    
    # predictCustomerAmount calculates the amount of customers over the next 15 minutes from the minute inputted
    # so divide this number by 15.
    customerData$customerCount <- customerData$customerCount / 15
    
    # "Output" the data so that it is put into the 'data' variable.
    customerData
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
         ylim=c(1,maxLim), 
         main="Predicted Amount of Customers", 
         xlab="Time", ylab="Amount of Customers", 
         axes = FALSE)
    
    # Create a subset of what ticks will be used for the x-axis. Creates a tick for every 15 minutes.
    ticks <- subset(data()$minutes, ((data()$minutes - floor(data()$minutes/60)*60)) %% 15 == 0)

    # If the lower bound of the time is not on a quarter hour, prepends a tick before the value.
    if (min(ticks) > min(data()$minutes)) {
      ticks <- append(min(ticks) - 15, ticks)
    }
    
    # If the upper bound of the time is not on a quarter hour, append a tick after the value.
    if (max(ticks) < max(data()$minutes)) {
      ticks <- append(ticks, max(ticks) + 15)
    }
    
    # Create the x-axis with the defined ticks and a custom label to convert the amount of minutes to a HH:MM format.
    axis(1, at = ticks,
         labels = paste(floor(ticks/60), ":",
                        formatC(ticks - (floor(ticks/60)*60), width = 2, format = "d", flag = "0"),
                        sep = ""))
    
    # Creates the y-axis from 0 to maxLim
    axis(2, at = 1:maxLim)
    
    # Used to draw the left and right lines of a box over the x- and y-axes (there is a problem of the two axes not touching)
    box(bty = "L")
  })
  
  # Sum up all the customers predicted for each minute and output it as text below the plot.
  output$text <- renderText({
    paste(format(sum(data()$customerCount), digits = 6), " customers")
  })
})
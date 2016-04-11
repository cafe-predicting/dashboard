library(shiny)
source("customerPredictor.R")

# Define server logic for the dashboard.
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  # Reactive expression called whenever the inputs changed.
  # Used to calculate the minutes set and the machine learning function.
  data <- reactive({
    # Create a set based on the two bounds from the minute input
    minutes <- c(input$minute[1])
    if (input$minute[1]+1 < input$minute[2]) {
      for (i in (input$minute[1]+1):input$minute[2]) {
        minutes <- append(minutes, i)
      }
    } else {
      minutes <- append(minutes, input$minute[2])
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
    # Sets the max y-limit for the graph to 10, unless there is greater than 10 customers predicted in one minute.
    maxLim <- ifelse(max(data()$customerCount) > 10, max(data()$customerCount), 10)
    
    # Output the plot using the data defined in the reaction function above.
    plot(data(), type="l", ylim=c(1,maxLim), main="Predicted Amount of Customers", xlab="Time", ylab="Amount of Customers")
    
    # Test stuff
    ticks <- subset(data()$minutes, ((data()$minutes - floor(data()$minutes/60)*60)) %% 15 == 0)
    if (min(ticks) > min(data()$minutes)) {
      ticks <- c(min(ticks) - 15, ticks)
    }
    if (max(ticks) < max(data()$minutes)) {
      ticks <- c(ticks, max(ticks) + 15)
    }
    
    axis(1,at = ticks, labels = paste(floor(ticks/60),":",ticks - (floor(ticks/60)*60),sep = ""))
    # End test stuff.
  })
  
  # Sum up all the customers predicted for each minute and output it as text below the plot.
  output$text <- renderText({
    paste(sum(data()$customerCount), " customers")
  })
})
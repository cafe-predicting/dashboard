library(shiny)
source("customerPredictor.R")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  #data <- reactive({
  # dow <- switch(input$dow,
  #                 norm = rnorm,
  #                 unif = runif,
  #                 lnorm = rlnorm,
  #                 exp = rexp,
  #                 rnorm)
  #  
  #  dow(input$hour)
  #})
  
  #test
  data <- reactive({
    minutes <- c(input$minute[1])
    if (input$minute[1]+1 < input$minute[2]) {
      for (i in (input$minute[1]+1):input$minute[2]) {
        minutes <- append(minutes, i)
      }
    } else {
      minutes <- append(minutes, input$minute[2])
    }
    customerData <- as.data.frame(minutes)
    
    #predictCustomerAmount(input$dayOfMonth, input$dayOfWeek, input$minute, input$temp, input$prec)
    customerData$customerCount <- apply(as.data.frame(minutes), 1, function(x) predictCustomerAmount(input$dayOfMonth, input$dayOfWeek, x, input$temp, input$prec))
    
    customerData$customerCount <- customerData$customerCount / 15
    
    customerData
  })
  #data <- as.data.frame(data)
  #data$Minute <- input$hour*60 + inpute$minute
  #data$InCount <- data$data
  #data <- subset(data, select=c("Minute", "InCount"))
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    maxLim <- ifelse(max(data()$customerCount) > 10, max(data()$customerCount), 10)
    plot(data(), type="l", ylim=c(1,maxLim), main="Predicted Amount of Customers", xlab="Time", ylab="Amount of Customers")
    
    ticks <- subset(data()$minutes, ((data()$minutes - floor(data()$minutes/60)*60)) %% 15 == 0)
    if (min(ticks) > min(data()$minutes)) {
      ticks <- c(min(ticks) - 15, ticks)
    }
    if (max(ticks) < max(data()$minutes)) {
      ticks <- c(ticks, max(ticks) + 15)
    }
    
  axis(1,at = ticks, labels = paste(floor(ticks/60),":",ticks - (floor(ticks/60)*60),sep = ""))
  })
  
  output$text <- renderText({
    paste(sum(data()$customerCount), " customers")
  })
  
  # Generate a summary of the data
  #output$summary <- renderPrint({
  #  summary(data())
  #})
  
  # Generate an HTML table view of the data
  #output$table <- renderTable({
  #  data.frame(x=data())
  #})
  
})
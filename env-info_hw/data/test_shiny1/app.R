library(shiny)
ui = fluidPage('Hello world',
             selectInput(inputId = "n_breaks",
                           label = "Number of bins in histogram (approximate):",
                           choices = c(10, 20, 35, 50),
                           selected = 20),
               
             plotOutput(outputId = "main_plot", height = "300px")
               )
server = function(input, output) {
  
  output$main_plot <- renderPlot({
    
    hist(faithful$eruptions,
         probability = TRUE,
         breaks = as.numeric(input$n_breaks),
         xlab = "Duration (minutes)",
         main = "Geyser eruption duration")
  })
}
shinyApp(ui, server)





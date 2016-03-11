#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Week 8 - Shiny!!"),
   sidebarLayout(
     sidebarPanel(
       helpText("Create demographic maps with 
                information from the 2010 US Census."),
       
       selectInput(inputId = "var", 
                   label = "Choose a variable to display",
                   choices = c("Percent White", "Percent Black",
                               "Percent Hispanic", "Percent Asian"),
                   selected = "Percent White"),
       
       sliderInput(inputId = "range", 
                   label = "Range of interest:",
                   min = 0, max = 100, value = c(0, 100))
       ),
     
     mainPanel(
       plotOutput("map")
     )
   )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(var = data, 
                color = color, 
                legend.title = legend, 
                max = input$range[2], 
                min = input$range[1])
  })
}
)

# Run the application 
shinyApp(ui = ui, server = server)


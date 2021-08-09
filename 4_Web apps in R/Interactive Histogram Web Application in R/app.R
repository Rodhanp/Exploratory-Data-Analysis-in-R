# Modified from https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)
data("airquality")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Max Temperature at La Guardia Airport (May to September 1973)."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 0,
                  max = 50,
                  value = 20,
                  step = 2)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    x    <- airquality$Temp
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "violet", border = "black",
         xlab = "Ozone level",
         main = "Histogram of Maximum daily temperature in degrees Fahrenheit at La Guardia Airport.")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

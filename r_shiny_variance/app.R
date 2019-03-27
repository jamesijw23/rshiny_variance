#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        
       #mu_1 = 41; sd_1 = 3;n_1 = 20; new_value = 32; num_sd = 2
        fluidRow(
          column(5,
                 numericInput("mu", 
                              label = h6("Population mean:"),
                              
                              value = 5.25,
                              width ='100%')) ,
          
          column(5,
                 numericInput("sd", 
                              label = h6("Population Standard Deviation:"),
                              value = 5,
                              width ='100%')),
          column(5,
                 numericInput("n", 
                              label = h6("Sample Size:"),
                              value = 25,
                              width ='75%')),
          column(5,
                 numericInput("num_sd", 
                              label = h6("The Number of Standard Deviations:"),
                              value = 1.5,
                              width ='75%'))
          
        ), 
        
        
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
         
         
         
         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


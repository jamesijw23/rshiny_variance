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
                     value = 30),
        
        actionButton("button","Calculate")
         
         
         
         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("text1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  
  inform <- eventReactive(input$button, {
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Gather mean, standard devaition, sample size, and number of sds
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mu_1 = input$mu
    sd_1 = input$sd 
    n_1 = input$n
    num_sd_1 = input$num_sd
    return(c(mu_1,sd_1,n_1,num_sd_1))
 
  })
  
  
  output$distPlot <- renderPlot({
    
    df = data.frame(Var1 = rnorm(n_1, mu_1, sd_1),
                    Var2 = rep('Trees',n_1))
    
    
    ## Just the data
    p_just_data = ggplot(df, aes(x = Var2, y = Var1, color=Var2)) +
      geom_jitter(width = 0.1,size=3)  +
      ggtitle(paste0("Palm Tree Heights", sep="")) +
      ylab("Heights") +
      scale_color_manual(values = c('Trees' = 'red')) +
      xlab("Trees") +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold")) +
      labs(colour = "Type of Info")
    
    
    ## Data and new point: Is this value an outlier compared to the rest of the data?
    p_with_point = p_just_data +
      geom_point(size = 3,
                 aes(x='Trees', y=new_value), 
                 colour="blue",show.legend = T)
    
    
    
    if(analysis == T ){
    max_value = mean(df$Var1) + num_sd*sd(df$Var1)
    min_value = mean(df$Var1) - num_sd*sd(df$Var1)
    } else{
      max_value = mean(df$Var1) + num_sd* (sd(df$Var1)/sqrt(n_1))
      min_value = mean(df$Var1) - num_sd*sd(df$Var1) 
    }
    
    ## With points and region of plausible values
    p_with_point + annotate("rect", xmin = -Inf, xmax = Inf,
                            ymin=min_value,
                            ymax=max_value,
                            fill="green",alpha = 0.1) +
      geom_hline(yintercept=mean(df$Var1),color="black",lwd=2) +
      geom_hline(yintercept=c(min_value,max_value),color="black",lwd=1,
                 linetype="dashed") 
    
  })
  
  
  output$text1 <- renderPrint({
    
    inform<-inform()
    ## Load Important Information
    ht_inform_p = inform$ht_info_p
    ht_inform_x = inform$ht_info_x
    ci_inform_p = inform$ci_info_p
    ci_inform_x = inform$ci_info_x
    
    imp_info    = inform$imp_info
    
    
    
    
    
  })
  
  
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)


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
         sliderInput("repetitions",
                     "Number of repetitions",
                     min = 1,
                     max = 5000,
                     value = 100)
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
     box1 <- c('blue', 'blue', 'red')
     box2 <- c('blue', 'blue', 'red', 'red', 'red', 'white')
     repetitions <- input$repetitions
     drawn_balls <- matrix(drawn_balls, nrow = repetitions, ncol = 4)
     for(i in 1:repetitions){
       x = runif(1)
       if(x > .5){
         sample = sample(box1, 4, replace = TRUE)
         for(j in 1:4){
           drawn_balls[i,j] = sample[j]
         }
       }
       else{
         sample = sample(box2, 4, replace = FALSE)
         for(j in 1:4){
           drawn_balls[i,j] = sample[j]
         }
       }
     }
     num_rows = 0
     prop0 = vector(mode = 'integer', length = repetitions)
     prop1 = vector(mode = 'integer', length = repetitions)
     prop2 = vector(mode = 'integer', length = repetitions)
     prop3 = vector(mode = 'integer', length = repetitions)
     prop4 = vector(mode = 'integer', length = repetitions)
     sum(drawn_balls[1,]=='blue')
     for(i in 1:repetitions){
       row = drawn_balls[i,]
       if(sum(row == 'blue')==0){
         prop0[i+1] = (prop0[i] * (i-1) + 1)/i
         prop1[i+1] = (prop1[i] * (i-1))/i
         prop2[i+1] = (prop2[i] * (i-1))/i
         prop3[i+1] = (prop3[i] * (i-1))/i
         prop4[i+1] = (prop4[i] * (i-1))/i
       }
       if(sum(row == 'blue')==1){
         prop0[i+1] = (prop0[i]*(i-1))/i
         prop1[i+1] = (prop1[i]*(i-1)+1)/i
         prop2[i+1] =(prop2[i] * (i-1))/i
         prop3[i+1] = (prop3[i] * (i-1))/i
         prop4[i+1] = (prop4[i] * (i-1))/i
       }
       if(sum(row == 'blue')==2){
         prop0[i+1] = (prop0[i]*(i-1))/i
         prop1[i+1] = (prop1[i] * (i-1))/i
         prop2[i+1] = (prop2[i]*(i-1)+1)/i
         prop3[i+1] = (prop3[i] * (i-1))/i
         prop4[i+1] =  (prop4[i] * (i-1))/i
       }
       if(sum(row == 'blue')==3){
         prop0[i+1] = (prop0[i]*(i-1))/i
         prop1[i+1] = (prop1[i] *(i-1))/i
         prop2[i+1] = (prop2[i]*(i-1))/i
         prop3[i+1] = (prop3[i]*(i-1)+1)/i
         prop4[i+1] =  (prop4[i] * (i-1))/i
       }
       if(sum(row == 'blue')==4){
         prop0[i+1] = (prop0[i]*(i-1))/i
         prop1[i+1] = (prop1[i] * (i-1))/i
         prop2[i+1] = (prop2[i]*(i-1))/i
         prop3[i+1] = (prop3[i] * (i-1))/i
         prop4[i+1] = (prop4[i]*(i-1)+1)/i
       }
     }
     ball_frame = data.frame(prop0 = prop0, 
                             prop1 = prop1, 
                             prop2 = prop2, 
                             prop3 = prop3,
                             prop4 = prop4)
     ggplot(ball_frame)+
       geom_line(aes(x = 0:repetitions, y=prop0, color = '0'))+
       geom_line(aes(x = 0:repetitions, y=prop1, color = '1'))+
       geom_line(aes(x = 0:repetitions, y=prop2, color = '2'))+
       geom_line(aes(x = 0:repetitions, y=prop3, color = '3'))+
       geom_line(aes(x = 0:repetitions, y=prop4, color = '4'))+
       labs(title = 'Relative frequencies of number of blue balls', y = 'proportion', x = 'repetitions')+
       scale_color_manual(name="Proportion", 
                          labels = c("0", 
                                     "1", 
                                     "2", 
                                     "3", 
                                     "4"), 
                          values = c("0"="red", 
                                     "1"="green", 
                                     "2"="turquoise", 
                                     "3"="green", 
                                     "4"="pink"))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Example discussed in lecture April-08-2019

library(shiny)
library(ggplot2)
library(rsconnect)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Simulation"),
   

     fluidRow(
       column(4,
         sliderInput("initial",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000, step = 500, pre = '$')),
       column(4,
              sliderInput('return', "Return Rate (in %)", min = 0, max = 20, value = 5, step = .1)),
       column(4,
         sliderInput("years", "Years",
                     min = 1, max = 50,
                     value = 10, step = 1)),
       column(4,
              sliderInput("annual", "Annual Contribution",
                          min = 0, max = 50000, value = 2000, step = 500, pre = '$')),
       column(4,
         sliderInput('growth', "Growth Rate (in %)", min = 0, max = 20, value = 2, step = .1)),
       column(4,
         selectInput('facet', 'Facet', choices = c('Yes', 'No')))
),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot"),
         tableOutput("returns")
      ))

# Define server logic required to draw a histogram
server <- function(input, output){
savings <- reactive({
future_value <- 
    function(amount = input$initial, 
             rate = input$return/100, 
            years){
    return(amount * ((1 + rate) ** years))
    }
annuity <- 
  function(contrib = input$annual, 
           rate = input$return/100, 
           years) {
    multiplier <- ((1 + rate) ** years - 1) / rate
    return (multiplier * contrib)
  }

growing_annuity <- 
  function(contrib = input$annual, 
           rate = input$return/100, 
           growth = input$growth/100, years) {
    multiplier <- ((1 + rate) ** years - (1 + growth) ** years) / (rate - growth)
    return (multiplier * contrib)
  }

  
  no_contrib_regular <- c()
  for(i in 0:input$years){
    value = future_value(years = i) 
    no_contrib_regular[i+1] <- value
  }


  
  fixed_contrib_regular <- c()
  for(i in 0:input$years){
    value <- future_value(years = i) + annuity(years = i)
    fixed_contrib_regular[i+1] <- value
  }

  
  
  growing_contrib_regular <- c()
  for(i in 0:input$years){
    value <- future_value(years = i) + growing_annuity(years = i)
    growing_contrib_regular[i+1] <- value
  }
  savings = data.frame(year = c(0:input$years), no_contrib = no_contrib_regular, 
                       fixed_contrib = fixed_contrib_regular, 
                       growing_contrib = growing_contrib_regular)
  return(savings)
})

savings2 <- reactive({
  savings = savings()
  new_save <- data.frame(year = rep(savings$year, 3), 
                         value = c(savings$no_contrib, savings$fixed_contrib, savings$growing_contrib),
                         type = c(rep('no_contrib', length(savings$year)), rep('fixed_contrib', length(savings$year)), rep('growing_contrib', length(savings$year))))
  new_save$type <- factor(new_save$type, levels = c('no_contrib', 'fixed_contrib', 'growing_contrib'))
  return(new_save)
})

   output$returns <- renderTable(savings())
   output$plot <- renderPlot(
     if(input$facet == 'Yes'){
       ggplot(savings2(), aes(x = year, y = value, group = type, color = type, fill = type))+
       geom_line()+
       geom_point()+
       geom_area(alpha = .4)+
       facet_wrap(~type)+
       labs(title = 'Three modes of investing')
     }
     else{
       ggplot(savings())+
         geom_line(aes(x = year, y = no_contrib, color = 'no_contrib'))+
         geom_point(aes(x = year, y = no_contrib, color = 'no_contrib'))+
         geom_line(aes(x = year, y = fixed_contrib, color = 'fixed_contrib'))+
         geom_point(aes(x = year, y = fixed_contrib, color = 'fixed_contrib'))+
         geom_line(aes(x = year, y = growing_contrib, color = 'growing_contrib'))+
         geom_point(aes(x = year, y = growing_contrib, color = 'growing_contrib'))+
         labs(x = 'year', y = 'value', title = 'Three modes of investing')+
         scale_colour_manual(name = 'variable', 
                             breaks = c('no_contrib', 'fixed_contrib', 'growing_contrib'),
                             values = c('no_contrib' = 'red',
                                        'fixed_contrib' = 'blue',
                                        'growing_contrib' = 'green'))
     }
)
}

# Run the application 
shinyApp(ui = ui, server = server)


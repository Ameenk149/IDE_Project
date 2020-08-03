library(shiny)

load(file = "Final_Data_Group_73.rdata")

ameen <- Final_Data_Group_73
ui <- fluidPage(sidebarPanel(
                  selectInput('xcol', 'Select Representation from the year', c('Per Month'= '30','Quater'= '90','Year'='360')))
                
                ,plotOutput("line")
                ,plotOutput("bar")
)
server <- function(input,output) {
  output$line <- renderPlot({plot(rnorm(input$xcol),type = "S")})
  
  output$bar <- renderPlot({hist(rnorm(input$xcol))})
  
}
shinyApp(ui = ui, server = server)
 
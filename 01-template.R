
library(shiny)
library(tidyverse)
library(gapminder)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)
library(plotly)

ui <- fluidPage( 
  tags$head(
    tags$style(
      HTML("#line{margin-bottom:250px;}")
    )
  ),
                headerPanel("New Application"),
                sidebarPanel(
                   selectInput('xcol', 'Select Representation from the year', c('Per Month'= '1 month','Quater'= '4 months','Year'='12 months'))
                  ,selectInput('car_type', 'Select car type', c('All'= 0,'Type 21'= 21,'Type 22'= 22)))
                  ,plotOutput("line")
                  ,br()
                  ,br()
                  ,plotOutput("bar")
                  ,plotOutput("exp")
)
server <- function(input,output) {
 filterNA <- Final_Data_Group_73 %>%
    filter(!is.na(min_Fehlerhaft_Fahrleistung))
  

  
 
  
 
  
  x<-reactive({input$xcol})
  y<-reactive({input$car_type})
  
  
    output$bar <- renderPlot({
     
        
    if (x() == "12 months" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
          group_by(Year) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Year), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
    }
      else if (x() == "1 month" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          group_by(Month) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Month), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else if (x() == "4 months" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
          group_by(Year) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Year), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      
         else if (x() == "12 months"){
          Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
          group_by(Year) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Year), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else if (x() == "1 month") {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          group_by(Month) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Month), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
          group_by(Quarter) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Quarter), y = Failures)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
        
        
      }
      
        })
    output$line <- renderPlot({
     
      if (x() == "12 months" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
          group_by(Year) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Year), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else if (x() == "1 month" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          group_by(Month) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Month), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else if (x() == "4 months" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
          group_by(Quarter) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Quarter), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      
      
      
      
      else if (x() == "12 months"){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
          group_by(Year) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Year), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else if (x() == "1 month") {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          group_by(Month) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Month), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
      }
      else {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          filter(Body_Type == y()) %>%
          mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
          group_by(Quarter) %>%
          summarise(Failures = n()) %>%
          ggplot(aes(x = (Quarter), y = Failures, group =1)) +
          geom_point() +
          geom_line() +
          theme(axis.text.x=element_text(angle = -90, hjust = 0))
        
        
      }
    })

    
}
shinyApp(ui = ui, server = server)
 
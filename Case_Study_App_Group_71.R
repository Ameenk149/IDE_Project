if(!require("tidyverse")){
  install.packages("tidyverse")
}
library("tidyverse")

if(!require("shiny")){
  install.packages("shiny")
}
library("shiny")

if(!require("gapminder")){
  install.packages("gapminder")
}
library("gapminder")

if(!require("dplyr")){
  install.packages("dplyr")
}
library("dplyr")

if(!require("ggplot2")){
  install.packages("ggplot2")
}
library("ggplot2")

if(!require("scales")){
  install.packages("scales")
}
library("scales")

if(!require("zoo")){
  install.packages("zoo")
}
library("zoo")

if(!require("plotly")){
  install.packages("plotly")
}
library("plotly")


if(!require("shinyWidgets")){
  install.packages("shinyWidgets")
}
library("shinyWidgets")

if(!require("markdown")){
  install.packages("markdown")
}
library("markdown")

if(!require("DT")){
  install.packages("DT")
}
library("DT")

if(!require("leaflet")){
  install.packages("leaflet")
}
library("leaflet")



ui <- navbarPage("Group 71"
                 ,tabPanel("Plot",
                           sidebarLayout(
                             # =========== Creating sidebar and applying inputs such as monthly quarterly and yearly, =========== #
                             # =========== Input of Body_Type which will create interface to control the filter. =========== #
                             sidebarPanel(id = "sidebar",
                                          selectInput('xcol', 'Representation Type', c('Monthly'= '1 month','Quaterly'= '4 months','Yearly'='12 months'))
                                          ,checkboxGroupInput('car_type', 'Car Type', c('Type 21'= 21,'Type 22'= 22), selected = c(21,22))
                             )
                             ,
                             # Main panel which creates output plot as line and bar
                             mainPanel(
                               plotlyOutput("line")
                               ,plotlyOutput("bar",width = "95%")
                               ,plotlyOutput("exp",width = "95%")
                               ,tags$head(tags$style( HTML("
                                #bar {
                                margin-left: 15px;
                                margin-bottom:  15px;               
                                margin-top : -400px;
                               }
                                #exp {
                                margin-left: 15px;
                                margin-top:  15px;
                                }
                                #main {  top = 0px  }              "
                               )))
                             )
                           )
                 ),
                 # =========== Setting background color to grass green as required by the case study. =========== #
                 setBackgroundColor(
                   color = c("#2B5329", "#7EC850"),
                   gradient = "linear",
                   direction = "bottom"
                 ),
                 tabPanel(id = "sum","Map",
                          verbatimTextOutput("Map"),
                          h1("Map of car production areas"),
                          leafletOutput("mymap")
                          
                 ),tabPanel(id = "dat","Data Values",
                            verbatimTextOutput("Data Values Used"),
                            h1("Used columns from the data"),
                            h3("Used to represent combined plot of bar and line chart for all the failed cars."),
                            DT::dataTableOutput("table1"), 
                            h3("Used to represent additional chart showing average mileage per car type.."),
                            DT::dataTableOutput("table2") 
                 ),
                 # =========== styling for different interface and controls. =========== #
                 tags$head(
                   tags$style(HTML("
                   @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');

                   #asd {
                     font-family: 'Lobster', cursive;
                     font-weight: 500;
                     line-height: 1.1;
                     color: white;
                   }
                   #img{
                   margin-left : -450px;
                   }


                    #sidebar {
                            font-family: 'Lobster', cursive;
                     font-weight: 500;
                     line-height: 1.1;
                     color: #2B5329;
                    }
                  .navbar-default .navbar-brand {
                              font-family: 'Lobster', cursive;
                     font-weight: 500;
                     line-height: 1.1;
                     color: white;
                      
                  }
                  h1, h3{
                            font-family: 'Lobster', cursive;
                     font-weight: 500;
                     line-height: 1.1;
                     color: white;
                    }
                .navbar  {
                 background-color: #154418;
                }
                
              
                
              a {
              color: red;
              }
              ")))
                 #=========== setting navbar on top of the page to add multiple page support. =========== #
                 # navbarMenu("More",
                 #            tabPanel("Table",
                 #                     DT::dataTableOutput("table")
                 #            ),
                 #            tabPanel("About",
                 #                     fluidRow(
                 #                       column(6,
                 #                              includeMarkdown("")
                 #                       ),
                 #                       column(3,
                 #                              img(class="img-polaroid",
                 #                                  src="imp.png"),
                 #                              tags$small(
                 #                                "Source: Photographed at the Bay State Antique ",
                 #                                "Automobile Club's July 10, 2005 show at the ",
                 #                                "Endicott Estate in Dedham, MA by ",
                 #                                a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                 #                                  "User:Sfoskett")
                 #                              )
                 #                       )
                 #                     )
                 #            )
                 # )
)



server <- function(input,output) {
  
  
  
  x<-reactive({input$xcol})
  y<-reactive({validate(
    need(input$car_type != "", 'Please choose at least one Car Type')
  )
    input$car_type})
  
  #=======================Loading Dataset============================================
  withProgress(message = 'Loading', value = 0, {
    
    #Loading Final_Data_Group_71
    load("Final_Data_Group_71.RData")
    
    # Function for filtering inside Plot
    pick <- function(condition){
      if(condition != 0){
        function(d) d %>% filter_(condition)
      }else
        function(d) d
    }
    
    # Preparing Data for plotting Failure over time
    #Yearly
    yearly_data <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Year, Body_Type) %>%
      summarise(Failures = n()) %>%
      ggplot() +
      geom_bar(data = . %>% summarize(Failures = sum(Failures)),aes(x = Year, y = Failures), stat = "identity") +
      geom_point(data = pick(~Body_Type == y()), aes(x = Year, y = Failures, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()), aes(x = Year, y = Failures, group = Body_Type, color = Body_Type))+
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures Over Time") + theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    #Mothly
    monthly_data <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Month, Body_Type) %>%
      summarise(Failures = n()) %>%
      ggplot() +
      geom_bar(data = . %>% summarize(Failures = sum(Failures)),aes(x = Month, y = Failures), stat = "identity") +
      geom_point(data = pick(~Body_Type == y()), aes(x = Month, y = Failures, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()), aes(x = Month, y = Failures, group = Body_Type, color = Body_Type))+
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures Over Time") + theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    #Quaterly
    quaterly_data <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Quarter, Body_Type) %>%
      summarise(Failures = n()) %>%
      ggplot() +
      geom_bar(data = . %>% summarize(Failures = sum(Failures)),aes(x = Quarter, y = Failures), stat = "identity") +
      geom_point(data = pick(~Body_Type == y()), aes(x = Quarter, y = Failures, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()), aes(x = Quarter, y = Failures, group = Body_Type, color = Body_Type))+
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures Over Time")+ theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    # Preparing Data for plotting Mileage over time
    #Yearly
    yearly_mileage <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Year, Body_Type) %>% 
      summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
      ggplot() +
      geom_point(data = pick(~Body_Type == y()),aes(x = Year, y = Mileage, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()),aes(x = Year, y = Mileage, group = Body_Type, color = Body_Type)) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Average Mileage Over Time")+ theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    #Monthly
    monthly_mileage <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Month, Body_Type) %>% 
      summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
      ggplot() +
      geom_point(data = pick(~Body_Type == y()), aes(x = Month, y = Mileage, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()), aes(x = Month, y = Mileage, group = Body_Type, color = Body_Type)) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Average Mileage Over Time")+ theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    #Quaterly
    quaterly_mileage <- Final_Data_Group_71 %>%
      filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
      mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
      mutate(Body_Type = as.factor(Body_Type)) %>%
      group_by(Quarter, Body_Type) %>% 
      summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
      ggplot() +
      geom_point(data = pick(~Body_Type == y()), aes(x = Quarter, y = Mileage, group = Body_Type, color = Body_Type))+
      geom_line(data = pick(~Body_Type == y()), aes(x = Quarter, y = Mileage, group = Body_Type, color = Body_Type)) +
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Average Mileage Over Time")+ theme(
        plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
      )
    
    incProgress(amount = 0.1)
    
    
    
    
  })
  
  
  # =========== rendering bar and line plots forFailure over time  =========== #
  output$bar <- renderPlotly({
    
    # =========== rendering plot for yearly =========== #
    if (x() == "12 months"){
      ggplotly(yearly_data, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
      
      
    }
    #=========== rendering plot for monthly  =========== #
    else if (x() == "1 month"){
      ggplotly(monthly_data, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
      
      
    }
    #=========== rendering plot for quarterly =========== #
    else if (x() == "4 months"){
      ggplotly(quaterly_data, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
      
    }
    
  })
  
  output$exp <- renderPlotly({
    
    # =========== rendering mileage plot =========== #
    #=========== rendering plot for yearly  =========== #
    
    if (x() == "12 months"){
      ggplotly(yearly_mileage, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
    }
    #=========== rendering plot for monthly  =========== #
    else if (x() == "1 month" ){
      ggplotly(monthly_mileage, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
    }
    #=========== rendering plot for quarterlys  =========== #
    else {
      ggplotly(quaterly_mileage, tooltip = c("x","y","colour")) %>% config(displayModeBar = F) %>% 
        layout(legend = list(font = list(size = 20),x = 100, y = 0.5))
    }
    
    
  })
  
  output$table1 = DT::renderDataTable({
     Final_Data_Group_71 %>% select(1,2,3,5)
  })
  
  output$table2 = DT::renderDataTable({
    Final_Data_Group_71 %>% select(Laengengrad_Fahrzeug 
                                   ,Laengengrad_Karosserie
                                   ,Laengengrad_Schaltung
                                   ,Laengengrad_Sitze 
                                   ,Laengengrad_Motor 
                                   ,Laengengrad_Part_1_Karosserie
                                   ,Laengengrad_Part_2_Karosserie
                                   ,Laengengrad_Part_3_Karosserie
                                   ,Laengengrad_Part_4_Karosserie
                                   ,Laengengrad_Part_5_Karosserie
                                   ,Laengengrad_Part_1_Schaltung
                                   ,Laengengrad_Part_2_Schaltung
                                   ,Laengengrad_Part_3_Schaltung
                                   ,Laengengrad_Part_1_Sitze
                                   ,Laengengrad_Part_2_Sitze
                                   ,Laengengrad_Part_3_Sitze
                                   ,Laengengrad_Part_1_Motor
                                   ,Laengengrad_Part_2_Motor
                                   ,Laengengrad_Part_3_Motor
                                   ,Laengengrad_Part_4_Motor
                                   
                                   ,Breitengrad_Fahrzeug 
                                   ,Breitengrad_Karosserie
                                   ,Breitengrad_Schaltung
                                   ,Breitengrad_Sitze 
                                   ,Breitengrad_Motor 
                                   ,Breitengrad_Part_1_Karosserie
                                   ,Breitengrad_Part_2_Karosserie
                                   ,Breitengrad_Part_3_Karosserie
                                   ,Breitengrad_Part_4_Karosserie
                                   ,Breitengrad_Part_5_Karosserie
                                   ,Breitengrad_Part_1_Schaltung
                                   ,Breitengrad_Part_2_Schaltung
                                   ,Breitengrad_Part_3_Schaltung
                                   ,Breitengrad_Part_1_Sitze
                                   ,Breitengrad_Part_2_Sitze
                                   ,Breitengrad_Part_3_Sitze
                                   ,Breitengrad_Part_1_Motor
                                   ,Breitengrad_Part_2_Motor
                                   ,Breitengrad_Part_3_Motor
                                   ,Breitengrad_Part_4_Motor
                                   
                                   
                                   ,PLZ_Fahrzeug 
                                   ,PLZ_Karosserie
                                   ,PLZ_Schaltung
                                   ,PLZ_Sitze 
                                   ,PLZ_Motor 
                                   ,PLZ_Part_1_Karosserie
                                   ,PLZ_Part_2_Karosserie
                                   ,PLZ_Part_3_Karosserie
                                   ,PLZ_Part_4_Karosserie
                                   ,PLZ_Part_5_Karosserie
                                   ,PLZ_Part_1_Schaltung
                                   ,PLZ_Part_2_Schaltung
                                   ,PLZ_Part_3_Schaltung
                                   ,PLZ_Part_1_Sitze
                                   ,PLZ_Part_2_Sitze
                                   ,PLZ_Part_3_Sitze
                                   ,PLZ_Part_1_Motor
                                   ,PLZ_Part_2_Motor
                                   ,PLZ_Part_3_Motor
                                   ,PLZ_Part_4_Motor
                                   
                                   
                                   ,ORT_Fahrzeug 
                                   ,ORT_Karosserie
                                   ,ORT_Schaltung
                                   ,ORT_Sitze 
                                   ,ORT_Motor 
                                   ,ORT_Part_1_Karosserie
                                   ,ORT_Part_2_Karosserie
                                   ,ORT_Part_3_Karosserie
                                   ,ORT_Part_4_Karosserie
                                   ,ORT_Part_5_Karosserie
                                   ,ORT_Part_1_Schaltung
                                   ,ORT_Part_2_Schaltung
                                   ,ORT_Part_3_Schaltung
                                   ,ORT_Part_1_Sitze
                                   ,ORT_Part_2_Sitze
                                   ,ORT_Part_3_Sitze
                                   ,ORT_Part_1_Motor
                                   ,ORT_Part_2_Motor
                                   ,ORT_Part_3_Motor
                                   ,ORT_Part_4_Motor
                                   
                                   ,Fehlerhaft_Fahrzeug 
                                   ,Fehlerhaft_Karosserie
                                   ,Fehlerhaft_Schaltung
                                   ,Fehlerhaft_Sitze 
                                   ,Fehlerhaft_Motor 
                                   ,Fehlerhaft_Part_1_Karosserie
                                   ,Fehlerhaft_Part_2_Karosserie
                                   ,Fehlerhaft_Part_3_Karosserie
                                   ,Fehlerhaft_Part_4_Karosserie
                                   ,Fehlerhaft_Part_5_Karosserie
                                   ,Fehlerhaft_Part_1_Schaltung
                                   ,Fehlerhaft_Part_2_Schaltung
                                   ,Fehlerhaft_Part_3_Schaltung
                                   ,Fehlerhaft_Part_1_Sitze
                                   ,Fehlerhaft_Part_2_Sitze
                                   ,Fehlerhaft_Part_3_Sitze
                                   ,Fehlerhaft_Part_1_Motor
                                   ,Fehlerhaft_Part_2_Motor
                                   ,Fehlerhaft_Part_3_Motor
                                   ,Fehlerhaft_Part_4_Motor
    )
  })
  
  
  
  
  output$mymap <- renderLeaflet({ 
    
    
    
    #---------------------------------------------------------------------------
    
    #Summarising the locations of manufacturing (Fahrzeug) and calculating the total number of produced and failed cars 
    Fahrzeug <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Fahrzeug, Breitengrad_Fahrzeug, PLZ_Fahrzeug, ORT_Fahrzeug) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Fahrzeug ==1))
    
    #Karosserie-----------------------------------------------------------------
    #Summarising the locations of Karosserie
    Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Karosserie, Breitengrad_Karosserie, PLZ_Karosserie, ORT_Karosserie) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Karosserie ==1))
    
    #Summarising the locations of Part_1_Karosserie
    Part_1_Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_1_Karosserie, Breitengrad_Part_1_Karosserie, PLZ_Part_1_Karosserie, ORT_Part_1_Karosserie) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Karosserie ==1))
    
    #Summarising the locations of Part_2_Karosserie
    Part_2_Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_2_Karosserie, Breitengrad_Part_2_Karosserie, PLZ_Part_2_Karosserie, ORT_Part_2_Karosserie) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Karosserie ==1))
    
    #Summarising the locations of Part_3_Karosserie
    Part_3_Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_3_Karosserie, Breitengrad_Part_3_Karosserie, PLZ_Part_3_Karosserie, ORT_Part_3_Karosserie) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Karosserie ==1))
    
    #Summarising the locations of Part_4_Karosserie
    Part_4_Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_4_Karosserie, Breitengrad_Part_4_Karosserie, PLZ_Part_4_Karosserie, ORT_Part_4_Karosserie) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_4_Karosserie ==1))
    
    
    #Summarising the locations of Part_5_Karosserie
    Part_5_Karosserie <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_5_Karosserie, Breitengrad_Part_5_Karosserie, PLZ_Part_5_Karosserie, ORT_Part_5_Karosserie) %>%
      filter(!is.na(Laengengrad_Part_5_Karosserie)) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_5_Karosserie ==1))
    
    #----------------------------------------------------------------------
    #Summarising the locations of Schaltung
    Schaltung <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Schaltung, Breitengrad_Schaltung, PLZ_Schaltung, ORT_Schaltung) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Schaltung ==1))
    
    #Summarising the locations of Part_1_Schaltung
    Part_1_Schaltung <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_1_Schaltung, Breitengrad_Part_1_Schaltung, PLZ_Part_1_Schaltung, ORT_Part_1_Schaltung) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Schaltung ==1))
    
    #Summarising the locations of Part_2_Schaltung
    Part_2_Schaltung <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_2_Schaltung, Breitengrad_Part_2_Schaltung, PLZ_Part_2_Schaltung, ORT_Part_2_Schaltung) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Schaltung ==1))
    
    #Summarising the locations of Part_3_Schaltung
    Part_3_Schaltung <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_3_Schaltung, Breitengrad_Part_3_Schaltung, PLZ_Part_3_Schaltung, ORT_Part_3_Schaltung) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Schaltung ==1))
    
    #----------------------------------------------------------------------
    #Summarising the locations of Sitze
    Sitze <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Sitze, Breitengrad_Sitze, PLZ_Sitze, ORT_Sitze) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Sitze ==1))
    
    #Summarising the locations of Part_1_Sitze
    Part_1_Sitze <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_1_Sitze, Breitengrad_Part_1_Sitze, PLZ_Part_1_Sitze, ORT_Part_1_Sitze) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Sitze ==1))
    
    #Summarising the locations of Part_2_Sitze
    Part_2_Sitze <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_2_Sitze, Breitengrad_Part_2_Sitze, PLZ_Part_2_Sitze, ORT_Part_2_Sitze) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Sitze ==1))
    
    #Summarising the locations of Part_3_Sitze
    Part_3_Sitze <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_3_Sitze, Breitengrad_Part_3_Sitze, PLZ_Part_3_Sitze, ORT_Part_3_Sitze) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Sitze ==1))
    
    #----------------------------------------------------------------------
    #Summarising the locations of motor
    Motor <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Motor, Breitengrad_Motor, PLZ_Motor, ORT_Motor) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Motor ==1))
    
    #Summarising the locations of Part_1_Motor
    Part_1_Motor <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_1_Motor, Breitengrad_Part_1_Motor, PLZ_Part_1_Motor, ORT_Part_1_Motor) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Motor ==1))
    
    #Summarising the locations of Part_2_Motor
    Part_2_Motor <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_2_Motor, Breitengrad_Part_2_Motor, PLZ_Part_2_Motor, ORT_Part_2_Motor) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Motor ==1))
    
    #Summarising the locations of Part_3_Motor
    Part_3_Motor <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_3_Motor, Breitengrad_Part_3_Motor, PLZ_Part_3_Motor, ORT_Part_3_Motor) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Motor ==1))
    
    #Summarising the locations of Part_4_Motor
    Part_4_Motor <- Final_Data_Group_71 %>%
      group_by(Laengengrad_Part_4_Motor, Breitengrad_Part_4_Motor, PLZ_Part_4_Motor, ORT_Part_4_Motor) %>%
      summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_4_Motor ==1))
    
    
    #-------------------------------------------------------------------------------
    #Showing icons on map
    
    IconSet <- awesomeIconList(
      "icon_Fahrzeug" = makeAwesomeIcon(icon= if_else((Fahrzeug$Failed/Fahrzeug$Produced) >.1,'times','car'), markerColor = 'red', iconColor = 'white', library = 'fa'),
      "icon_Karosserie" = makeAwesomeIcon(icon= if_else((Karosserie$Failed/Karosserie$Produced) >.1,'times','car'), markerColor = 'green', iconColor = 'white', library = 'fa'),
      "icon_Schaltung" = makeAwesomeIcon(icon= if_else((Schaltung$Failed/Schaltung$Produced) >.1,'times','car'), markerColor = 'green', iconColor = 'white', library = 'fa'),
      "icon_Sitze" = makeAwesomeIcon(icon= if_else((Sitze$Failed/Sitze$Produced) >.1,'times','car'), markerColor = 'green', iconColor = 'white', library = 'fa'),
      "icon_Motor" = makeAwesomeIcon(icon= if_else((Motor$Failed/Motor$Produced) >.1,'times','car'), markerColor = 'green', iconColor = 'white', library = 'fa'),
      
      #Karosseri
      "icon_Part_1_Karosserie" = makeAwesomeIcon(icon= if_else((Part_1_Karosserie$Failed/Part_1_Karosserie$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_2_Karosserie" = makeAwesomeIcon(icon= if_else((Part_2_Karosserie$Failed/Part_2_Karosserie$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_3_Karosserie" = makeAwesomeIcon(icon= if_else((Part_3_Karosserie$Failed/Part_3_Karosserie$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_4_Karosserie" = makeAwesomeIcon(icon= if_else((Part_4_Karosserie$Failed/Part_4_Karosserie$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_5_Karosserie" = makeAwesomeIcon(icon= if_else((Part_5_Karosserie$Failed/Part_5_Karosserie$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      
      #Schaltung
      "icon_Part_1_Schaltung" = makeAwesomeIcon(icon= if_else((Part_1_Schaltung$Failed/Part_1_Schaltung$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_2_Schaltung" = makeAwesomeIcon(icon= if_else((Part_2_Schaltung$Failed/Part_2_Schaltung$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_3_Schaltung" = makeAwesomeIcon(icon= if_else((Part_3_Schaltung$Failed/Part_3_Schaltung$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      
      #Sitze
      "icon_Part_1_Sitze" = makeAwesomeIcon(icon= if_else((Part_1_Sitze$Failed/Part_1_Sitze$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_2_Sitze" = makeAwesomeIcon(icon= if_else((Part_2_Sitze$Failed/Part_2_Sitze$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_3_Sitze" = makeAwesomeIcon(icon= if_else((Part_3_Sitze$Failed/Part_3_Sitze$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      
      #Motor
      "icon_Part_1_Motor" = makeAwesomeIcon(icon= if_else((Part_1_Motor$Failed/Part_1_Motor$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_2_Motor" = makeAwesomeIcon(icon= if_else((Part_2_Motor$Failed/Part_2_Motor$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_3_Motor" = makeAwesomeIcon(icon= if_else((Part_3_Motor$Failed/Part_3_Motor$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa'),
      "icon_Part_4_Motor" = makeAwesomeIcon(icon= if_else((Part_4_Motor$Failed/Part_4_Motor$Produced) >.1,'times','car'), markerColor = 'blue', iconColor = 'white', library = 'fa')
    )
    
    markerLegendHTML <- "<div style='width: 200px; height: auto'>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-red awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-car fa-inverse'></i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Cars</label>
    <br>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-green awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-car fa-inverse'></i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Components</label>
    <br>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-blue awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-car fa-inverse'></i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Parts</label>
    <br>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-red awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-times fa-inverse'></i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Cars (failure > 10%)</label>
    <br>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-green awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-times fa-inverse'></i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Components (failure > 10%)</label>
    <br>
    <div style='position: relative; display: inline-block; width: 36px; height: 45px'
        class='awesome-marker-icon-blue awesome-marker'>
        <i style='margin-left: 5px; margin-top: 11px;' class='fa fa-times fa-inverse'>
        </i>
    </div>
    <label style='display: block;font-size: 12px; margin-top:-31px;font-size: 12px;margin-left:41px;'>Parts (failure > 10%)</label>
</div>"
    
    #-------------------------------------------------------------------------------
    #Showing map markers for vehicle and components.
    
    #Fahrzeug Markers
    show_markers <- leaflet() %>% addTiles() %>%
      addAwesomeMarkers(lat = Fahrzeug$Breitengrad_Fahrzeug, lng = Fahrzeug$Laengengrad_Fahrzeug, 
                        popup = paste("Produced : ", Fahrzeug$Produced, "<br>", "Failed : ", Fahrzeug$Failed, "<br>"), 
                        label = paste(Fahrzeug$PLZ_Fahrzeug," ",Fahrzeug$ORT_Fahrzeug), icon = IconSet["icon_Fahrzeug"]) %>%
      ##Components
      ##"Fahrzeug","Karosserie","Schaltung","Sitze","Motor"
      addAwesomeMarkers(lat = Karosserie$Breitengrad_Karosserie, lng = Karosserie$Laengengrad_Karosserie, 
                        popup = paste("Produced : ", Karosserie$Produced, "<br>", "Failed : ", Karosserie$Failed, "<br>"), 
                        label = paste(Karosserie$PLZ_Karosserie," ",Karosserie$ORT_Karosserie), icon = IconSet["icon_Karosserie"]) %>%
      
      addAwesomeMarkers(lat = Schaltung$Breitengrad_Schaltung, lng = Schaltung$Laengengrad_Schaltung, 
                        popup = paste("Produced : ", Schaltung$Produced, "<br>", "Failed : ", Schaltung$Failed, "<br>"), 
                        label = paste(Schaltung$PLZ_Schaltung," ",Schaltung$ORT_Schaltung), icon = IconSet["icon_Schaltung"]) %>%
      
      addAwesomeMarkers(lat = Sitze$Breitengrad_Sitze, lng = Sitze$Laengengrad_Sitze, 
                        popup = paste("Produced : ", Sitze$Produced, "<br>", "Failed : ", Sitze$Failed, "<br>"), 
                        label = paste(Sitze$PLZ_Sitze," ",Sitze$ORT_Sitze), icon = IconSet["icon_Sitze"]) %>%
      
      addAwesomeMarkers(lat = Motor$Breitengrad_Motor, lng = Motor$Laengengrad_Motor, 
                        popup = paste("Produced : ", Motor$Produced, "<br>", "Failed : ", Motor$Failed, "<br>"), 
                        label = paste(Motor$PLZ_Motor," ",Motor$ORT_Motor), icon = IconSet["icon_Motor"]) %>%
      
      ####### Parts
      addAwesomeMarkers(lat = Part_1_Karosserie$Breitengrad_Part_1_Karosserie, lng = Part_1_Karosserie$Laengengrad_Part_1_Karosserie, 
                        popup = paste("Produced : ", Part_1_Karosserie$Produced, "<br>",
                                      "Failed : ", Part_1_Karosserie$Failed, "<br>"), 
                        label = paste(Part_1_Karosserie$PLZ_Part_1_Karosserie," ",Part_1_Karosserie$ORT_Part_1_Karosserie), icon = IconSet["icon_Part_1_Karosserie"])  %>%
      
      addAwesomeMarkers(lat = Part_2_Karosserie$Breitengrad_Part_2_Karosserie, lng = Part_2_Karosserie$Laengengrad_Part_2_Karosserie, 
                        popup = paste("Produced : ", Part_2_Karosserie$Produced, "<br>",
                                      "Failed : ", Part_2_Karosserie$Failed, "<br>"), 
                        label = paste(Part_2_Karosserie$PLZ_Part_2_Karosserie," ",Part_2_Karosserie$ORT_Part_2_Karosserie),
                        icon = IconSet["icon_Part_2_Karosserie"]) %>%
      
      addAwesomeMarkers(lat = Part_3_Karosserie$Breitengrad_Part_3_Karosserie, lng = Part_3_Karosserie$Laengengrad_Part_3_Karosserie, 
                        popup = paste("Produced : ", Part_3_Karosserie$Produced, "<br>",
                                      "Failed : ", Part_3_Karosserie$Failed, "<br>"), 
                        label = paste(Part_3_Karosserie$PLZ_Part_3_Karosserie," ",Part_3_Karosserie$ORT_Part_3_Karosserie),
                        icon = IconSet["icon_Part_3_Karosserie"])  %>%
      
      addAwesomeMarkers(lat = Part_4_Karosserie$Breitengrad_Part_4_Karosserie, lng = Part_4_Karosserie$Laengengrad_Part_4_Karosserie, 
                        popup = paste("Produced : ", Part_4_Karosserie$Produced, "<br>",
                                      "Failed : ", Part_4_Karosserie$Failed, "<br>"), 
                        label = paste(Part_4_Karosserie$PLZ_Part_4_Karosserie," ",Part_4_Karosserie$ORT_Part_4_Karosserie),
                        icon = IconSet["icon_Part_4_Karosserie"])  %>%
      
      addAwesomeMarkers(lat = Part_5_Karosserie$Breitengrad_Part_5_Karosserie, lng = Part_5_Karosserie$Laengengrad_Part_5_Karosserie, 
                        popup = paste("Produced : ", Part_5_Karosserie$Produced, "<br>",
                                      "Failed : ", Part_5_Karosserie$Failed, "<br>"), 
                        label = paste(Part_5_Karosserie$PLZ_Part_5_Karosserie," ",Part_5_Karosserie$ORT_Part_5_Karosserie),
                        icon = IconSet["icon_Part_5_Karosserie"]) %>%
      
      addAwesomeMarkers(lat = Part_1_Schaltung$Breitengrad_Part_1_Schaltung, lng = Part_1_Schaltung$Laengengrad_Part_1_Schaltung, 
                        popup = paste("Produced : ", Part_1_Schaltung$Produced, "<br>",
                                      "Failed : ", Part_1_Schaltung$Failed, "<br>"), 
                        label = paste(Part_1_Schaltung$PLZ_Part_1_Schaltung," ",Part_1_Schaltung$ORT_Part_1_Schaltung),
                        icon = IconSet["icon_Part_1_Schaltung"]) %>%
      
      addAwesomeMarkers(lat = Part_2_Schaltung$Breitengrad_Part_2_Schaltung, lng = Part_2_Schaltung$Laengengrad_Part_2_Schaltung, 
                        popup = paste("Produced : ", Part_2_Schaltung$Produced, "<br>",
                                      "<center>Failed : ", Part_2_Schaltung$Failed, "<br></center>"), 
                        label = paste(Part_2_Schaltung$PLZ_Part_2_Schaltung," ",Part_2_Schaltung$ORT_Part_2_Schaltung),
                        icon = IconSet["icon_Part_2_Schaltung"]) %>%
      
      addAwesomeMarkers(lat = Part_3_Schaltung$Breitengrad_Part_3_Schaltung, lng = Part_3_Schaltung$Laengengrad_Part_3_Schaltung, 
                        popup = paste("Produced : ", Part_3_Schaltung$Produced, "<br>",
                                      "Failed : ", Part_3_Schaltung$Failed, "<br>"), 
                        label = paste(Part_3_Schaltung$PLZ_Part_3_Schaltung," ",Part_3_Schaltung$ORT_Part_3_Schaltung),
                        icon = IconSet["icon_Part_3_Schaltung"]) %>%
      addAwesomeMarkers(lat = Part_1_Sitze$Breitengrad_Part_1_Sitze, lng = Part_1_Sitze$Laengengrad_Part_1_Sitze, 
                        popup = paste("Produced : ", Part_1_Sitze$Produced, "<br>",
                                      "Failed : ", Part_1_Sitze$Failed, "<br>"), 
                        label = paste(Part_1_Sitze$PLZ_Part_1_Sitze," ",Part_1_Sitze$ORT_Part_1_Sitze),
                        icon = IconSet["icon_Part_1_Sitze"]) %>%
      
      addAwesomeMarkers(lat = Part_2_Sitze$Breitengrad_Part_2_Sitze, lng = Part_2_Sitze$Laengengrad_Part_2_Sitze, 
                        popup = paste("Produced : ", Part_2_Sitze$Produced, "<br>",
                                      "Failed : ", Part_2_Sitze$Failed, "<br>"), 
                        label = paste(Part_2_Sitze$PLZ_Part_2_Sitze," ",Part_2_Sitze$ORT_Part_2_Sitze),
                        icon = IconSet["icon_Part_2_Sitze"]) %>%
      
      addAwesomeMarkers(lat = Part_3_Sitze$Breitengrad_Part_3_Sitze, lng = Part_3_Sitze$Laengengrad_Part_3_Sitze, 
                        popup = paste("Produced : ", Part_3_Sitze$Produced, "<br>",
                                      "Failed : ", Part_3_Sitze$Failed, "<br>"), 
                        label = paste(Part_3_Sitze$PLZ_Part_3_Sitze," ",Part_3_Sitze$ORT_Part_3_Sitze),
                        icon = IconSet["icon_Part_3_Sitze"]) %>%
      
      addAwesomeMarkers(lat = Part_1_Motor$Breitengrad_Part_1_Motor, lng = Part_1_Motor$Laengengrad_Part_1_Motor, 
                        popup = paste("Produced : ", Part_1_Motor$Produced, "<br>",
                                      "Failed : ", Part_1_Motor$Failed, "<br>"), 
                        label = paste(Part_1_Motor$PLZ_Part_1_Motor," ",Part_1_Motor$ORT_Part_1_Motor),
                        icon = IconSet["icon_Part_1_Motor"]) %>%
      
      addAwesomeMarkers(lat = Part_2_Motor$Breitengrad_Part_2_Motor, lng = Part_2_Motor$Laengengrad_Part_2_Motor, 
                        popup = paste("Produced : ", Part_2_Motor$Produced, "<br>",
                                      "Failed : ", Part_2_Motor$Failed, "<br>"), 
                        label = paste(Part_2_Motor$PLZ_Part_2_Motor," ",Part_2_Motor$ORT_Part_2_Motor),
                        icon = IconSet["icon_Part_2_Motor"]) %>%
      
      addAwesomeMarkers(lat = Part_3_Motor$Breitengrad_Part_3_Motor, lng = Part_3_Motor$Laengengrad_Part_3_Motor, 
                        popup = paste("Produced : ", Part_3_Motor$Produced, "<br>",
                                      "Failed : ", Part_3_Motor$Failed, "<br>"), 
                        label = paste(Part_3_Motor$PLZ_Part_3_Motor," ",Part_3_Motor$ORT_Part_3_Motor),
                        icon = IconSet["icon_Part_3_Motor"]) %>%
      
      addAwesomeMarkers(lat = Part_4_Motor$Breitengrad_Part_4_Motor, lng = Part_4_Motor$Laengengrad_Part_4_Motor, 
                        popup = paste("Produced : ", Part_4_Motor$Produced, "<br>",
                                      "Failed : ", Part_4_Motor$Failed, "<br>"), 
                        label = paste(Part_4_Motor$PLZ_Part_4_Motor," ",Part_4_Motor$ORT_Part_4_Motor), 
                        icon = IconSet["icon_Part_4_Motor"]) %>% 
      
      setView(lng = 10.45, lat = 51.16, zoom = 6) %>% 
      
      addControl(html = markerLegendHTML, position = "bottomright")
    
    
    })
  
  
  
  
  
  
  
  
}
shinyApp(ui = ui, server = server)

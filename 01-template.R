
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



ui <- navbarPage("Group 71"
           ,tabPanel("Plot",
                    sidebarLayout(
 # =========== Creating sidebar and applying inputs such as monthly quarterly and yearly, =========== #
      # =========== Input of Body_Type which will create interface to control the filter. =========== #
                      sidebarPanel(id = "sidebar",
                                   selectInput('xcol', 'Select Representation from the year', c('Per Month'= '1 month','Quater'= '4 months','Year'='12 months'))
                                   ,selectInput('car_type', 'Select car type', c('All'= 0,'Type 21'= 21,'Type 22'= 22)))
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
                .navbar  {
                 background-color: #154418;
                }
                
              a {
              color: red;
              }
              "))),
           tabPanel(id = "sum","Summary",
                    verbatimTextOutput("summary")
           ),
    #=========== setting navbar on top of the page to add multiple page support. =========== #
           navbarMenu("More",
                      tabPanel("Table",
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        includeMarkdown("")
                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src="imp.png"),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                               )
                      )
           )
)



server <- function(input,output) {
 filterNA <- Final_Data_Group_73 %>%
    filter(!is.na(min_Fehlerhaft_Fahrleistung))
  

  
 
 pick <- function(condition){
   function(d) d %>% filter_(condition)
 }
 
  
  x<-reactive({input$xcol})
  y<-reactive({input$car_type})
  
  # =========== rendering bar and line plots combined =========== #
    output$bar <- renderPlotly({
     
    # =========== rendering plot for year with the body type selected as all =========== #
    if (x() == "12 months" & y() == 0){
     Final_Data_Group_73 %>%
        filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
        mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
        mutate(Body_Type = as.factor(Body_Type)) %>%
        group_by(Year, Body_Type) %>%
        summarise(Failures = n()) %>%
        ggplot() +
        geom_bar(aes(x = (Year), y = Failures), stat = "identity") +
        geom_point(data = pick(~Body_Type == 21), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
        geom_line(data = pick(~Body_Type == 21), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
        geom_point(data = pick(~Body_Type == 22), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
        geom_line(data = pick(~Body_Type == 22), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time") + theme(
          plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
        )
      
    }
      #=========== rendering plot for monthly with the body type selected as all =========== #
      else if (x() == "1 month" & y() == 0){
       Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          mutate(Body_Type = as.factor(Body_Type)) %>%
          group_by(Month, Body_Type) %>%
          summarise(Failures = n()) %>%
          ggplot() +
          geom_bar(aes(x = (Month), y = Failures), stat = "identity") +
          geom_point(data = pick(~Body_Type == 21), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == 21), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          geom_point(data = pick(~Body_Type == 22), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == 22), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time") + theme(
            plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
          )
        
      }
      #=========== rendering plot for quarter with the body type selected as all =========== #
      else if (x() == "4 months" & y() == 0){
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
          mutate(Body_Type = as.factor(Body_Type)) %>%
          group_by(Quarter, Body_Type) %>%
          summarise(Failures = n()) %>%
          ggplot() +
          geom_bar(aes(x = (Quarter), y = Failures), stat = "identity") +
          geom_point(data = pick(~Body_Type == 21), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == 21), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          geom_point(data = pick(~Body_Type == 22), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == 22), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time")+ theme(
            plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
          )
      }
      #=========== rendering plot for yearly with the body type selected as one of two given =========== #
         else if (x() == "12 months"){
           Final_Data_Group_73 %>%
             filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
             mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
             mutate(Body_Type = as.factor(Body_Type)) %>%
             group_by(Year, Body_Type) %>%
             summarise(Failures = n()) %>%
             ggplot() +
             geom_bar(aes(x = (Year), y = Failures), stat = "identity") +
             geom_point(data = pick(~Body_Type == y()), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
             geom_line(data = pick(~Body_Type == y()), aes(x = (Year), y = Failures, group = Body_Type, color = Body_Type))+
             theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time")+ theme(
               plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
             )
         }
      #=========== rendering plot for monthly with the body type selected as one of two given =========== #
      else if (x() == "1 month") {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
          mutate(Body_Type = as.factor(Body_Type)) %>%
          group_by(Month, Body_Type) %>%
          summarise(Failures = n()) %>%
          ggplot() +
          geom_bar(aes(x = (Month), y = Failures), stat = "identity") +
          geom_point(data = pick(~Body_Type == y()), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == y()), aes(x = (Month), y = Failures, group = Body_Type, color = Body_Type))+
          theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time")+ theme(
            plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
          )
        
      }
      #=========== rendering plot for quarterly with the body type selected as one of two given =========== #
      else {
        Final_Data_Group_73 %>%
          filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
          mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
          mutate(Body_Type = as.factor(Body_Type)) %>%
          group_by(Quarter, Body_Type) %>%
          summarise(Failures = n()) %>%
          ggplot() +
          geom_bar(aes(x = (Quarter), y = Failures), stat = "identity") +
          geom_point(data = pick(~Body_Type == y()), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          geom_line(data = pick(~Body_Type == y()), aes(x = (Quarter), y = Failures, group = Body_Type, color = Body_Type))+
          theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Number of Failures per time")+ theme(
            plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
          )
        
      }
      
        })
   
   output$exp <- renderPlotly({
   
     
     # =========== rendering line plot =========== #
         #=========== rendering plot for yearly  =========== #
     
     if (x() == "12 months"){
       Final_Data_Group_73 %>%
         filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
         mutate(Year = as.factor(format(Produktionsdatum,"%Y"))) %>%
         mutate(Body_Type = as.factor(Body_Type)) %>%
         group_by(Year, Body_Type) %>% 
         summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
         ggplot(aes(x = (Year), y = Mileage, group = Body_Type, color = Body_Type)) +
         geom_point()+
         geom_line() +
         theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Mileage per time")+ theme(
           plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
         )
       
     }
         #=========== rendering plot for monthly  =========== #
     else if (x() == "1 month" ){
       Final_Data_Group_73 %>%
         filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
         mutate(Month = as.factor(as.yearmon(Produktionsdatum))) %>%
         mutate(Body_Type = as.factor(Body_Type)) %>%
         group_by(Month, Body_Type) %>% 
         summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
         ggplot(aes(x = (Month), y = Mileage, group = Body_Type, color = Body_Type)) +
         geom_point()+
         geom_line() +
         theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Mileage per time")+ theme(
           plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
         )
       
     }
        #=========== rendering plot for quarterlys  =========== #
     else {
       Final_Data_Group_73 %>%
         filter(!is.na(min_Fehlerhaft_Fahrleistung)) %>%
         mutate(Quarter = as.factor(as.yearqtr(Produktionsdatum))) %>%
         mutate(Body_Type = as.factor(Body_Type)) %>%
         group_by(Quarter, Body_Type) %>% 
         summarise(Mileage = mean(min_Fehlerhaft_Fahrleistung))%>% 
         ggplot(aes(x = (Quarter), y = Mileage, group = Body_Type, color = Body_Type)) +
         geom_point()+
         geom_line() +
         theme(axis.text.x=element_text(angle = -90, hjust = 0)) + ggtitle(label = "Mileage per time")+ theme(
           plot.title = element_text(color = "#2B5329", size = 12, face = "bold.italic")
         )
     
     }
     
     
     
     
     
     
     
     
     
     
     
     })
  
    
}
shinyApp(ui = ui, server = server)
 
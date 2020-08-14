library(tidyverse)
library(leaflet)

IconSet <- awesomeIconList(
  
  "Fahrzeug"   = makeAwesomeIcon(icon= if_else((Fahrzeug$Failed/Fahrzeug$Produced) >.1,'times','map-marker-alt'), markerColor = 'red', iconColor = 'white', library = "fa"),
  "Karosserie"   = makeAwesomeIcon(icon= if_else((Karosserie$Failed/Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'green', iconColor = 'white', library = "fa"),
  "Schaltung"   = makeAwesomeIcon(icon= if_else((Schaltung$Failed/Schaltung$Produced) >.1,'times','map-marker-alt'), markerColor = 'green', iconColor = 'white', library = "fa"),
  "Sitze"   = makeAwesomeIcon(icon= if_else((Sitze$Failed/Sitze$Produced) >.1,'times','map-marker-alt'), markerColor = 'green', iconColor = 'white', library = "fa"),
  "Motor"   = makeAwesomeIcon(icon= if_else((Motor$Failed/Motor$Produced) >.1,'times','map-marker-alt'), markerColor = 'green', iconColor = 'white', library = "fa"),
  
  #Karosseri
  "Part_1_Karosserie" = makeAwesomeIcon(icon= if_else((Part_1_Karosserie$Failed/Part_1_Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_2_Karosserie" = makeAwesomeIcon(icon= if_else((Part_2_Karosserie$Failed/Part_2_Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_3_Karosserie" = makeAwesomeIcon(icon= if_else((Part_3_Karosserie$Failed/Part_3_Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_4_Karosserie" = makeAwesomeIcon(icon= if_else((Part_4_Karosserie$Failed/Part_4_Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_5_Karosserie" = makeAwesomeIcon(icon= if_else((Part_5_Karosserie$Failed/Part_5_Karosserie$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  
  #Schaltung
  "Part_1_Schaltung" = makeAwesomeIcon(icon= if_else((Part_1_Schaltung$Failed/Part_1_Schaltung$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_2_Schaltung" = makeAwesomeIcon(icon= if_else((Part_2_Schaltung$Failed/Part_2_Schaltung$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_3_Schaltung" = makeAwesomeIcon(icon= if_else((Part_3_Schaltung$Failed/Part_3_Schaltung$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  
  #Sitze
  "Part_1_Sitze" = makeAwesomeIcon(icon= if_else((Part_1_Sitze$Failed/Part_1_Sitze$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_2_Sitze" = makeAwesomeIcon(icon= if_else((Part_2_Sitze$Failed/Part_2_Sitze$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_3_Sitze" = makeAwesomeIcon(icon= if_else((Part_3_Sitze$Failed/Part_3_Sitze$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  
  #Motor
  "Part_1_Motor"= makeAwesomeIcon(icon= if_else((Part_1_Motor$Failed/Part_1_Motor$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_2_Motor"= makeAwesomeIcon(icon= if_else((Part_2_Motor$Failed/Part_2_Motor$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_3_Motor"= makeAwesomeIcon(icon= if_else((Part_3_Motor$Failed/Part_3_Motor$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa"),
  "Part_4_Motor"= makeAwesomeIcon(icon= if_else((Part_4_Motor$Failed/Part_4_Motor$Produced) >.1,'times','map-marker-alt'), markerColor = 'blue', iconColor = 'white', library = "fa")
)
#---------------------------------------------------------------------------

#Summarising the locations of manufacturing (Fahrzeug) and calculating the total number of produced and failed cars 
Fahrzeug <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Fahrzeug, Breitengrad_Fahrzeug, PLZ_Fahrzeug, ORT_Fahrzeug) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Fahrzeug ==1))

#Karosserie-----------------------------------------------------------------
#Summarising the locations of Karosserie
Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Karosserie, Breitengrad_Karosserie, PLZ_Karosserie, ORT_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Karosserie ==1))

#Summarising the locations of Part_1_Karosserie
Part_1_Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_1_Karosserie, Breitengrad_Part_1_Karosserie, PLZ_Part_1_Karosserie, ORT_Part_1_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Karosserie ==1))

#Summarising the locations of Part_2_Karosserie
Part_2_Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_2_Karosserie, Breitengrad_Part_2_Karosserie, PLZ_Part_2_Karosserie, ORT_Part_2_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Karosserie ==1))

#Summarising the locations of Part_3_Karosserie
Part_3_Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_3_Karosserie, Breitengrad_Part_3_Karosserie, PLZ_Part_3_Karosserie, ORT_Part_3_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Karosserie ==1))

#Summarising the locations of Part_4_Karosserie
Part_4_Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_4_Karosserie, Breitengrad_Part_4_Karosserie, PLZ_Part_4_Karosserie, ORT_Part_4_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_4_Karosserie ==1))


#Summarising the locations of Part_5_Karosserie
Part_5_Karosserie <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_5_Karosserie, Breitengrad_Part_5_Karosserie, PLZ_Part_5_Karosserie, ORT_Part_5_Karosserie) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_5_Karosserie ==1))

#----------------------------------------------------------------------
#Summarising the locations of Schaltung
Schaltung <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Schaltung, Breitengrad_Schaltung, PLZ_Schaltung, ORT_Schaltung) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Schaltung ==1))

#Summarising the locations of Part_1_Schaltung
Part_1_Schaltung <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_1_Schaltung, Breitengrad_Part_1_Schaltung, PLZ_Part_1_Schaltung, ORT_Part_1_Schaltung) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Schaltung ==1))

#Summarising the locations of Part_2_Schaltung
Part_2_Schaltung <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_2_Schaltung, Breitengrad_Part_2_Schaltung, PLZ_Part_2_Schaltung, ORT_Part_2_Schaltung) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Schaltung ==1))

#Summarising the locations of Part_3_Schaltung
Part_3_Schaltung <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_3_Schaltung, Breitengrad_Part_3_Schaltung, PLZ_Part_3_Schaltung, ORT_Part_3_Schaltung) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Schaltung ==1))

#----------------------------------------------------------------------
#Summarising the locations of Sitze
Sitze <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Sitze, Breitengrad_Sitze, PLZ_Sitze, ORT_Sitze) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Sitze ==1))

#Summarising the locations of Part_1_Sitze
Part_1_Sitze <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_1_Sitze, Breitengrad_Part_1_Sitze, PLZ_Part_1_Sitze, ORT_Part_1_Sitze) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Sitze ==1))

#Summarising the locations of Part_2_Sitze
Part_2_Sitze <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_2_Sitze, Breitengrad_Part_2_Sitze, PLZ_Part_2_Sitze, ORT_Part_2_Sitze) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Sitze ==1))

#Summarising the locations of Part_3_Sitze
Part_3_Sitze <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_3_Sitze, Breitengrad_Part_3_Sitze, PLZ_Part_3_Sitze, ORT_Part_3_Sitze) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Sitze ==1))

#----------------------------------------------------------------------
#Summarising the locations of motor
Motor <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Motor, Breitengrad_Motor, PLZ_Motor, ORT_Motor) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Motor ==1))

#Summarising the locations of Part_1_Motor
Part_1_Motor <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_1_Motor, Breitengrad_Part_1_Motor, PLZ_Part_1_Motor, ORT_Part_1_Motor) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_1_Motor ==1))

#Summarising the locations of Part_2_Motor
Part_2_Motor <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_2_Motor, Breitengrad_Part_2_Motor, PLZ_Part_2_Motor, ORT_Part_2_Motor) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_2_Motor ==1))

#Summarising the locations of Part_3_Motor
Part_3_Motor <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_3_Motor, Breitengrad_Part_3_Motor, PLZ_Part_3_Motor, ORT_Part_3_Motor) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_3_Motor ==1))

#Summarising the locations of Part_4_Motor
Part_4_Motor <- Final_Data_Group_73 %>%
  group_by(Laengengrad_Part_4_Motor, Breitengrad_Part_4_Motor, PLZ_Part_4_Motor, ORT_Part_4_Motor) %>%
  summarise(Produced = n(),Failed = sum(Fehlerhaft_Part_4_Motor ==1))

#-------------------------------------------------------------------------------
#Showing map markers for vehicle and components.

#Fahrzeug Markers
show_markers <- leaflet() %>% addTiles() %>%
  addAwesomeMarkers(lat = Fahrzeug$Breitengrad_Fahrzeug, lng = Fahrzeug$Laengengrad_Fahrzeug, 
                    popup = paste("Produced : ", Fahrzeug$Produced, "<br>", "Failed : ", Fahrzeug$Failed, "<br>"), 
                    label = paste(Fahrzeug$PLZ_Fahrzeug," ",Fahrzeug$ORT_Fahrzeug), icon = IconSet["Fahrzeug"]) %>%
  ##Components
  ##"Fahrzeug","Karosserie","Schaltung","Sitze","Motor"
  addAwesomeMarkers(lat = Karosserie$Breitengrad_Karosserie, lng = Karosserie$Laengengrad_Karosserie, 
                    popup = paste("Produced : ", Karosserie$Produced, "<br>", "Failed : ", Karosserie$Failed, "<br>"), 
                    label = paste(Karosserie$PLZ_Karosserie," ",Karosserie$ORT_Karosserie), icon = IconSet["Karosserie"]) %>%
  
  addAwesomeMarkers(lat = Schaltung$Breitengrad_Schaltung, lng = Schaltung$Laengengrad_Schaltung, 
                    popup = paste("Produced : ", Schaltung$Produced, "<br>", "Failed : ", Schaltung$Failed, "<br>"), 
                    label = paste(Schaltung$PLZ_Schaltung," ",Schaltung$ORT_Schaltung), icon = IconSet["Schaltung"]) %>%
  
  addAwesomeMarkers(lat = Sitze$Breitengrad_Sitze, lng = Sitze$Laengengrad_Sitze, 
                    popup = paste("Produced : ", Sitze$Produced, "<br>", "Failed : ", Sitze$Failed, "<br>"), 
                    label = paste(Sitze$PLZ_Sitze," ",Sitze$ORT_Sitze), icon = IconSet["Sitze"]) %>%
  
  addAwesomeMarkers(lat = Motor$Breitengrad_Motor, lng = Motor$Laengengrad_Motor, 
                    popup = paste("Produced : ", Motor$Produced, "<br>", "Failed : ", Motor$Failed, "<br>"), 
                    label = paste(Motor$PLZ_Motor," ",Motor$ORT_Motor), icon = IconSet["Motor"]) %>%
  
  ####### Parts
  addAwesomeMarkers(lat = Part_1_Karosserie$Breitengrad_Part_1_Karosserie, lng = Part_1_Karosserie$Laengengrad_Part_1_Karosserie, 
                    popup = paste("Produced : ", Part_1_Karosserie$Produced, "<br>",
                                  "Failed : ", Part_1_Karosserie$Failed, "<br>"), 
                    label = paste(Part_1_Karosserie$ORT_Part_1_Karosserie),
                    IconSet["Part_1_Karosserie"])  %>%
  
  addAwesomeMarkers(lat = Part_2_Karosserie$Breitengrad_Part_2_Karosserie, lng = Part_2_Karosserie$Laengengrad_Part_2_Karosserie, 
                    popup = paste("Produced : ", Part_2_Karosserie$Produced, "<br>",
                                  "Failed : ", Part_2_Karosserie$Failed, "<br>"), 
                    label = paste(Part_2_Karosserie$ORT_Part_2_Karosserie),
                    IconSet["Part_2_Karosserie"]) %>%
  
  addAwesomeMarkers(lat = Part_3_Karosserie$Breitengrad_Part_3_Karosserie, lng = Part_3_Karosserie$Laengengrad_Part_3_Karosserie, 
                    popup = paste("Produced : ", Part_3_Karosserie$Produced, "<br>",
                                  "Failed : ", Part_3_Karosserie$Failed, "<br>"), 
                    label = paste(Part_3_Karosserie$ORT_Part_3_Karosserie),
                    IconSet["Part_3_Karosserie"])  %>%
  
  addAwesomeMarkers(lat = Part_4_Karosserie$Breitengrad_Part_4_Karosserie, lng = Part_4_Karosserie$Laengengrad_Part_4_Karosserie, 
                    popup = paste("Produced : ", Part_4_Karosserie$Produced, "<br>",
                                  "Failed : ", Part_4_Karosserie$Failed, "<br>"), 
                    label = paste(Part_4_Karosserie$ORT_Part_4_Karosserie),
                    IconSet["Part_4_Karosserie"])  %>%
  
  addAwesomeMarkers(lat = Part_5_Karosserie$Breitengrad_Part_5_Karosserie, lng = Part_5_Karosserie$Laengengrad_Part_5_Karosserie, 
                    popup = paste("Produced : ", Part_5_Karosserie$Produced, "<br>",
                                  "Failed : ", Part_5_Karosserie$Failed, "<br>"), 
                    label = paste(Part_5_Karosserie$ORT_Part_5_Karosserie),
                    IconSet["Part_5_Karosserie"]) %>%
  
  addAwesomeMarkers(lat = Part_1_Schaltung$Breitengrad_Part_1_Schaltung, lng = Part_1_Schaltung$Laengengrad_Part_1_Schaltung, 
                    popup = paste("Produced : ", Part_1_Schaltung$Produced, "<br>",
                                  "Failed : ", Part_1_Schaltung$Failed, "<br>"), 
                    label = paste(Part_1_Schaltung$ORT_Part_1_Schaltung),
                    IconSet["Part_1_Schaltung"]) %>%
  
  addAwesomeMarkers(lat = Part_2_Schaltung$Breitengrad_Part_2_Schaltung, lng = Part_2_Schaltung$Laengengrad_Part_2_Schaltung, 
                    popup = paste("Produced : ", Part_2_Schaltung$Produced, "<br>",
                                  "<center>Failed : ", Part_2_Schaltung$Failed, "<br></center>"), 
                    label = paste(Part_2_Schaltung$ORT_Part_2_Schaltung),
                    IconSet["Part_2_Schaltung"]) %>%
  
  addAwesomeMarkers(lat = Part_3_Schaltung$Breitengrad_Part_3_Schaltung, lng = Part_3_Schaltung$Laengengrad_Part_3_Schaltung, 
                    popup = paste("Produced : ", Part_3_Schaltung$Produced, "<br>",
                                  "Failed : ", Part_3_Schaltung$Failed, "<br>"), 
                    label = paste(Part_3_Schaltung$ORT_Part_3_Schaltung),
                    IconSet["Part_3_Schaltung"]) %>%
  addAwesomeMarkers(lat = Part_1_Sitze$Breitengrad_Part_1_Sitze, lng = Part_1_Sitze$Laengengrad_Part_1_Sitze, 
                    popup = paste("Produced : ", Part_1_Sitze$Produced, "<br>",
                                  "Failed : ", Part_1_Sitze$Failed, "<br>"), 
                    label = paste(Part_1_Sitze$ORT_Part_1_Sitze),
                    IconSet["Part_1_Sitze"]) %>%
  
  addAwesomeMarkers(lat = Part_2_Sitze$Breitengrad_Part_2_Sitze, lng = Part_2_Sitze$Laengengrad_Part_2_Sitze, 
                    popup = paste("Produced : ", Part_2_Sitze$Produced, "<br>",
                                  "Failed : ", Part_2_Sitze$Failed, "<br>"), 
                    label = paste(Part_2_Sitze$ORT_Part_2_Sitze),
                    IconSet["Part_2_Sitze"]) %>%
  
  addAwesomeMarkers(lat = Part_3_Sitze$Breitengrad_Part_3_Sitze, lng = Part_3_Sitze$Laengengrad_Part_3_Sitze, 
                    popup = paste("Produced : ", Part_3_Sitze$Produced, "<br>",
                                  "Failed : ", Part_3_Sitze$Failed, "<br>"), 
                    label = paste(Part_3_Sitze$ORT_Part_3_Sitze),
                    IconSet["Part_3_Sitze"]) %>%
  
  addAwesomeMarkers(lat = Part_1_Motor$Breitengrad_Part_1_Motor, lng = Part_1_Motor$Laengengrad_Part_1_Motor, 
                    popup = paste("Produced : ", Part_1_Motor$Produced, "<br>",
                                  "Failed : ", Part_1_Motor$Failed, "<br>"), 
                    label = paste(Part_1_Motor$ORT_Part_1_Motor),
                    IconSet["Part_1_Motor"]) %>%
  
  addAwesomeMarkers(lat = Part_2_Motor$Breitengrad_Part_2_Motor, lng = Part_2_Motor$Laengengrad_Part_2_Motor, 
                    popup = paste("Produced : ", Part_2_Motor$Produced, "<br>",
                                  "Failed : ", Part_2_Motor$Failed, "<br>"), 
                    label = paste(Part_2_Motor$ORT_Part_2_Motor),
                    IconSet["Part_2_Motor"]) %>%
  
  addAwesomeMarkers(lat = Part_3_Motor$Breitengrad_Part_3_Motor, lng = Part_3_Motor$Laengengrad_Part_3_Motor, 
                    popup = paste("Produced : ", Part_3_Motor$Produced, "<br>",
                                  "Failed : ", Part_3_Motor$Failed, "<br>"), 
                    label = paste(Part_3_Motor$ORT_Part_3_Motor),
                    IconSet["Part_3_Motor"]) %>%
  
  addAwesomeMarkers(lat = Part_4_Motor$Breitengrad_Part_4_Motor, lng = Part_4_Motor$Laengengrad_Part_4_Motor, 
                    popup = paste("Produced : ", Part_4_Motor$Produced, "<br>",
                                  "Failed : ", Part_4_Motor$Failed, "<br>"), 
                    label = paste(Part_4_Motor$ORT_Part_4_Motor), 
                    IconSet["Part_4_Motor"]) %>% 
  
  setView(lng = 10.45, lat = 51.16, zoom = 6) %>% 
  
  addLegend("bottomright", 
            colors =c("Red", "Green", "Blue"),
            labels= c("Cars", "Components","Parts"),
            title= "OEM and Suppliers",
            opacity = 1)

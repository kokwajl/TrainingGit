library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tibbletime)
library(shinyWidgets)
library(shinythemes)
library(scales)


library(RColorBrewer)
library(fresh)

## Make a new dataframe with formated date
make_datetime_new <- function(df) {
  Year=year(df[,1])
  Month=month(df[,1])
  Day=day(df[,1])
  Hour=hour(df[,1])
  Min=minute(df[,1])
  
  df <- df %>%
    mutate(date = make_datetime(Year, Month, Day, Hour, Min))
}

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
#cls_names = c(as.character(unique(gg_reactive_db()$station)))
cls_names = c("ABOBO-MAIRIE", "ATTECOUBE-MAIRIE", "BINGERVILLE", "COCODY-RIVIERA-GO", "COCODY-ST-VIATEUR", "KOUMASSI", "MARCORY","SONGON-MAIRIE","YOPOUGON_ZONE")
station_cols = cls[1:length(cls_names)]
names(station_cols) = cls_names

### cumulative rain plot function bis ##period=c("1", "2", "3","4", "5", "6", "7", "8")
cumulative_rain_plot = function(gg_data) {
  
  g = ggplot(gg_data, aes(x = date, y = cumsum(rain) , colour = station, group = 1,
                          text = paste0(format(date, "%d %B %Y %H:%M"), "\n", "Station :", station, "\n", "Cumul :",cumsum(rain), " mm"))) #+
  # xlim(as.POSIXct(strptime(c(lastDateInFile_24h, lastDateInFile), format = "%Y-%m-%d %H:%M"), tz = 'UTM')) + xlab("Date")
  
  #g1 = g + geom_line(alpha = 0.8) + geom_smooth(span = .2, method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) # To smooth line
  g1 = g + geom_line(alpha = 0.8) + #geom_point(size = 1, alpha = 0.8) +
    ylab("Cumul (mm)") + theme_bw() +
    scale_y_continuous(limits=c(0, max(cumsum(gg_data$rain))+1)) +#limits=c(0, max(cumsum(st$rain))+1) +
    scale_x_datetime(date_labels = "%d %b %Y %H:%M", breaks = date_breaks("1 day"),minor_breaks = date_breaks("1 hour")) +
    #scale_x_datetime(date_labels = "%d %b %Y %H:%M", date_breaks = "1 week", date_minor_breaks = "1 day") +
    scale_colour_manual(values = station_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10))
  # theme(axis.text.x = element_text(angle=45, hjust = 1), legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10))
  ggplotly(g1 + labs(title = (paste("Station",unique(gg_data$station)))), tooltip = c("text")) %>% layout(showlegend = FALSE, legend = list(font = list(size = 11)))
}
# span(tags$g(h4("Pas de pluie enregistrée au cours de la période")), style = "color:red")
### daily rain plot function bis
daily_rain_plot = function(gg_data) {
  
  g = ggplot(gg_data, aes(x = date, y = rain, fill = station, group = 1,
                          text = paste0(format(date, "%d %B %Y %H:%M"), "\n", "Station :", station, "\n", "Pluviométrie :",rain*60, " mm/h"))) #+
  # xlim(as.POSIXct(strptime(c(lastDateInFile_12h, lastDateInFile), format = "%Y-%m-%d %H:%M"), tz = 'UTM')) + xlab("Date")
  
  g1 = g + geom_bar(position = "stack", stat = "identity") +
    ylab("Pluviométrie (mm/h)") + theme_bw() +
    scale_fill_manual(values = station_cols) +
    scale_y_continuous(limits = c(0, max(gg_data$rain) + 1), labels = function(l) {trans = 60*l ; paste0(trans, " mm/h")}) +
    scale_x_datetime(date_labels = "%d %b %Y %H:%M", breaks = date_breaks("1 day"),minor_breaks = date_breaks("1 hour")) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size = 10))
  ggplotly(g1 + labs(title = (paste("Station",unique(gg_data$station)))), tooltip = c("text")) %>% layout(showlegend = FALSE, legend = list( font = list(size = 11)))
}

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # titlePanel(windowTitle = "MetSAP-Abidjan",
  #   fluidRow(
  #     column(4, "MetSAP-Abidjan"),
  #     column(4, offset = 4, img(height = 55, width = 300, src = "logosodex.png"),align = "right")
  #   )),
  # titlePanel(windowTitle = "MetSAP-Abidjan", div(style ="padding: 0px 0px 0px 0px","MetSAP-Abidjan",img(height = 50, width = 300, src = "logosodex.png", class = "pull-right"))),
  # titlePanel(windowTitle = "MetSAP-Abidjan", div(img(height = 50, width = 300, src = "logosodex.png"),"MetSAP-Abidjan")),
  titlePanel(windowTitle = "MetSAP-Abidjan", div(img(height = 50, width = 300, src = "logosodex.png", class = "pull-right"),"MetSAP-Abidjan")),
  
 #  br(), 
 # h5(img(src = "logosodex.png", height = 50, width = 300), align = "center"),
  # hr(),
  use_theme(create_theme(
    theme = "default",
    bs_vars_global(
      body_bg = "#87CEFA",
      text_color = "#000"
    ),
    bs_vars_wells(
      bg = "#4682B4"#"#CEECF5"#"#2E2E2E"
    )
  )),
  
  # App title ----
  # titlePanel("MetSAP-Abidjan"),
  
  # wellPanel(p("Column width 6"),fluidRow(style = "height:200px; background-color: white;",plotlyOutput("rain_plot_cumulative", height = "200px", width = "100%")))
  
  fluidRow(
    column(6, absolutePanel(
      top = 35, left = 80, draggable = FALSE, width = "40%", style = "z-index:500; min-width: 305px;",
             # br(),
             # br(),
             selectInput("select", label = "",#h4("Veuillez sélectionner une période"), 
                         choices = list("Pluie enregistrée au cours des 30 dernières minutes" = 1, "Pluie enregistrée au cours de la dernière heure" = 2, 
                                        "Pluie enregistrée au cours des 2 dernières heures" = 3, "Pluie enregistrée au cours des 4 dernières heures" = 4, 
                                        "Pluie enregistrée au cours des 12 dernières heures" = 5, "Pluie enregistrée au cours des 24 dernières heures" = 6,
                                        "Pluie enregistrée au cours des 48 dernières heures" = 7, "Pluie enregistrée au cours des 72 dernières heures" = 8), 
                         selected = 1, width = '100%')
           ), 
      wellPanel(strong(p("Carte interactive")),div(style = "height:700px;background-color: white;",leafletOutput("map", width = "100%", height = "100%")))
      
           # div(style = "height:800px;background-color: white;",leafletOutput("map", width = "100%", height = "100%")),
           ),
    column(width = 6, wellPanel(strong(p("Cumul pluviométrique")),fluidRow(style = "height:300px; background-color: white;",plotlyOutput("rain_plot_cumulative", height = "250px", width = "100%")))),
      # fluidRow(style = "height:400px; background-color: white;",plotlyOutput("rain_plot_cumulative", height = "300px", width = "100%")),
      br(),
    column(width = 6, wellPanel(strong(p("Intensité de la pluie")),fluidRow(style = "height:300px; background-color: white;",plotlyOutput("rain_plot", height = "250px", width = "100%")))),
      # fluidRow(style = "height:400px; background-color: white;",plotlyOutput("rain_plot", height = "300px", width = "100%")),
    ),
  )
  

# Define server logic required to deal with reactive map and plots ----
server <- function(input, output) {
  #autoInvalidate <- reactiveTimer(60000)
  
  # setwd("/home/koffi/WeatherRain/testnew/newinterface/InputData/") 
  # setwd("/home/previ/DATAVIS_DIR/DATA_IN/")
  #setwd("/home/previ/ShinyDataVis/InputData/doc/")
  setwd("C:/Users/H P/Documents/shiny-mini_code/Dat/") 
  
  filesName = list.files(pattern = "CI_")
  
#  gg_reactive_db <- reactive({
#  
#  invalidateLater(300000) #refresh each 300000 ms = 5 mn
#    
#    data_set = do.call(rbind, lapply(filesName, function(x) read.csv(x, stringsAsFactors = FALSE)))
#    data_set$date = as.POSIXct(data_set$date, origin = "1970-01-01")
#    db <- as_tbl_time(data_set, index = date)
#    
#    station_name <- unique(db$station)
#    lat_val <- unique(db$lat)
#    lon_val <- unique(db$lon)
#    
#    # k = c(0.5*60*60, 1*60*60, 2*60*60, 4*60*60, 12*60*60, 24*60*60, 48*60*60, 72*60*60)
#    k = c(0.5, 1.0, 2.0, 4.0, 12.0, 24.0, 48.0, 72.0)
#    Nb_lines = c(30, 60, 120, 240, 720, 1440, 2880, 4320)
#    
#    for (i in 1:length(k)) {
#      if (as.numeric(input$select) == i) { #input$select==as.numeric("i")
#        st_list <- list()
#        for (j in 1:9) {
#          st <- db %>% filter(station == station_name[j])
#          currentDate = Sys.time()
#          lastDateInFile = st$date[length(st$date)]
#          periodDate = currentDate - k[i]*60*60 
#          st <- st %>% filter_time(periodDate ~ currentDate)
#          nb_lines = length(st$rain)
#          if (nb_lines == 0) {
#            date = seq(from=floor_date(as.POSIXct(periodDate + 60), '1 minutes') ,to=floor_date(as.POSIXct(currentDate),'1 minutes'),by="min")
#            station = rep(station_name[j] , length = k[i]*60)
#            lat = rep(lat_val[j], length = k[i]*60)
#            lon = rep(lon_val[j], length = k[i]*60)
#            rain = rep(0, length = k[i]*60)
#            st = data.frame(date = date, station = station, lat = lat, lon = lon, rain = rain)
#          }else if (nb_lines != Nb_lines[i]) {
#          date = seq(from=floor_date(as.POSIXct(lastDateInFile + 60), '1 minutes') ,to=floor_date(as.POSIXct(currentDate),'1 minutes'),by="min")
#          station = rep(station_name[j] , length = length(date))
#          lat = rep(lat_val[j], length = length(date))
#          lon = rep(lon_val[j], length = length(date))
#          rain = rep(0, length = length(date))
#          st1 = data.frame(date = date, station = station, lat = lat, lon = lon, rain = rain)
#          st = rbind(st,st1)
#          }
#          st_list[[j]] <- st
#        }
#        df <- do.call("rbind",st_list) 
#      }
#    }
#    
#    # for (i in 1:length(k)) {
#    #   if (as.numeric(input$select) == i) { #input$select==as.numeric("i")
#    #     st_list <- list()
#    #     for (j in 1:9) {
#    #       st <- db %>% filter(station == station_name[j])
#    #       lastDateInFile = st$date[length(st$date)]
#    #       # lastDateInFile = Sys.time()
#    #       periodDate = lastDateInFile - k[i]*60*60 
#    #       st <- st %>% filter_time(periodDate ~ lastDateInFile)
#    #       st_list[[j]] <- st
#    #     }
#    #     df <- do.call("rbind",st_list) 
#    #   }
#    # }
#    df$rain[is.na(df$rain)]=0
#    df$station[which(df$station == "YOPOUGON_ZONE")] = "YOPOUGON-ZONE"
#    df$station[which(df$station == "COCODY-RIVIERA-GO")] = "COCODY-RIVIERA"
#    
#    
#    station = c("ABOBO-MAIRIE", "ATTECOUBE-MAIRIE", "BINGERVILLE", "COCODY-RIVIERA", "COCODY-ST-VIATEUR", "KOUMASSI", "MARCORY","SONGON-MAIRIE", "YOPOUGON-ZONE")
#    
#    for (i in 1:9) {
#      
#      if (is.null(data$clickedMarker$id) )
#      {
#        return(subset(df,station == "ABOBO-MAIRIE"))
#      } else return(subset(df,station == data$clickedMarker$id))
#    }
#  })

gg_reactive_db <- reactive({
  #autoInvalidate()
  
  invalidateLater(1*60*1000) #refresh each 300000 ms = 5 mn 
    
    data_set = do.call(rbind, lapply(filesName, function(x) read.csv(x, stringsAsFactors = FALSE)))
    #dateN = as.numeric(as.character(data_set$date))
    #data_set$date = as.POSIXct(as.numeric(as.character(data_set$date)), origin = "1970-01-01")
    #data_set$date = as.POSIXct(dateN, origin = "1970-01-01")
    
    data_set = make_datetime_new(data_set)
    
    db <- as_tbl_time(data_set, index = date)
    
    db$rain[is.na(db$rain)]=0
    db$station[which(db$station == "YOPOUGON_ZONE")] = "YOPOUGON-ZONE"
    db$station[which(db$station == "COCODY-RIVIERA-GO")] = "COCODY-RIVIERA"
    
#    station_name <- unique(db$station)
#    lat_val <- unique(db$lat)
#    lon_val <- unique(db$lon)
    
    # k = c(0.5*60*60, 1*60*60, 2*60*60, 4*60*60, 12*60*60, 24*60*60, 48*60*60, 72*60*60 )
    k = c(0.5, 1.0, 2.0, 4.0, 12.0, 24.0, 48.0, 72.0)
#    Nb_lines = c(30, 60, 120, 240, 720, 1440, 2880, 4320)
    
    if (is.null(data$clickedMarker$id) )
      {
        stat = "ABOBO-MAIRIE"
      } else stat = data$clickedMarker$id
      
     #currentDate = Sys.time() 
     currentDate = as.Date("2023/12/19 12:03")
     lastDate = currentDate - k[as.numeric(input$select)]*60*60  
     
     st <- db %>% 
     filter(station == stat) %>% 
     filter_time(lastDate ~ currentDate)     
       
#    station = c("ABOBO-MAIRIE", "ATTECOUBE-MAIRIE", "BINGERVILLE", "COCODY-RIVIERA", "COCODY-ST-VIATEUR", "KOUMASSI", "MARCORY","SONGON-MAIRIE", "YOPOUGON-ZONE")  
  })
  
  ### DATA PARSE FUNCTION
  Datanal = function(iFile) {
    #autoInvalidate()
    
    data_set = do.call(rbind, lapply(iFile, function(x) read.csv(x, stringsAsFactors = FALSE)))
    #data_set$date = as.POSIXct(as.numeric(as.character(data_set$date)), origin = "1970-01-01")
    #data_set$date = as.POSIXct(data_set$date, origin = "1970-01-01")
    
    data_set = make_datetime_new(data_set)
    
    db <- as_tbl_time(data_set, index = date)
    
    station_name <- unique(db$station)
    
    statname <- list()
    for (i in 1:length(filesName)) {
      ### get stations name ###
      A = (gsub(pattern = "CI_", replacement = "", filesName[i]))
      B = gsub(pattern = ".csv", replacement = "", unlist(strsplit(A, "./")))
      statname[[i]] = B
    }
    
    stat_lat <- unique(db$lat)
    stat_lon <- unique(db$lon)
    
    nb_min = c(0.5*60, 1.*60, 2.*60, 4.*60, 12.*60, 24.*60, 48.*60, 72.*60)
    
    for (i in 1:length(nb_min)) {
      if (as.numeric(input$select) == i) { #as.numeric(input$select) == i
        st_list <- list()
        for (j in 1:length(filesName)) {
          st <- db %>% filter(station == station_name[j])
          # lastDateInFile = st$date[length(st$date)]
          lastDateInFile = Sys.time()
          periodDate = lastDateInFile - 60*nb_min[i] 
          st <- st %>% filter_time(periodDate ~ lastDateInFile)
          st_list[[j]] <- sum(st$rain, na.rm = TRUE)
        }
        df <- do.call("rbind",st_list)
      }
    }
    
    A = data.frame(Station = unlist(statname), Lat = stat_lat, Lon = stat_lon, Rain = df)
    
    return(A)
    
  }
  
  filteredData <- reactive({
    #autoInvalidate()
    
    invalidateLater(1*60*1000) #refresh each 300000 ms = 5 mn
    
    Datanal(filesName)
    
  })
  
  output$map <- renderLeaflet(
    #autoInvalidate(),
    leaflet() %>% addTiles(attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      #addProviderTiles("CartoDB.Positron") %>%
      fitBounds(-4.4, 5.25, -3.6, 5.52) #%>% setView(-4.0, 5.37, zoom = 18) #%>%
    #addCircleMarkers(lat = myData$Lat, lng = myData$Lon, radius = myData$Rain, layerId = myData$Station) 
  )
  
  observe({
   
    myData <- filteredData()
    
    # couleurs <- colorNumeric("Blues", myData$Rain, n = 5)
    
    popup1 = paste("Pas de pluie sur la période")
    
    popup2 = paste(
      "Station: ", myData$Station , "<br/>",
      "qté de pluie: ", myData$Rain, " mm", "<br/>",
      sep = "")
    
    rval = 0
    rval2 = 0
    ve = seq(1:length(myData$Rain))
    for (i in ve) {
      rval[i] = myData$Rain[i]
      rval2 = rval
    }
    
    rvalcompare =  rep_len(0,length(myData$Rain))
    
    # lt = 5.27
    # ln = -4.09
    
    popop = ""
    if (all(rval2 == rvalcompare) )
    { popop = "Pas de pluie pour la période sélectionnée" } else
    {popop = paste(
      "Station: ", myData$Station , "<br/>",
      "qté de pluie: ", myData$Rain, " mm", "<br/>",
      sep = "")}
    
    
    couleurs <- colorNumeric("Blues", myData$Rain, n = 5)
    
    content <- paste(
      # strong("Pas de pluie enregistrée au cours de la période" )
      span(tags$g(h4("Pas de pluie enregistrée au cours de la période")), style = "color:red")
    )
    
    mytext = popop %>%

      # mytext <- paste(
      #   "Station: ", myData$Station , "<br/>",
      #   "qté de pluie: ", myData$Rain, " mm", "<br/>",
      #   sep = "") %>%
      lapply(htmltools::HTML)
    
    proxy <- leafletProxy("map", data = myData)
    
    # output$map <- renderLeaflet(
    #   leaflet() %>% addTiles(attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    #     #addProviderTiles("CartoDB.Positron") %>%
    #     fitBounds(-4.4, 5.25, -3.6, 5.52) #%>% setView(-4.0, 5.37, zoom = 18) #%>%
    #   #addCircleMarkers(lat = myData$Lat, lng = myData$Lon, radius = myData$Rain, layerId = myData$Station) 
    # )
    
    proxy <- leafletProxy("map", data = myData)
    
    proxy  %>% clearControls() %>% clearShapes()
    
    if (all(rval2 == rvalcompare)) 
    {
      # proxy <- leafletProxy("map", data = myData)
      # proxy  %>% clearControls() %>% clearShapes() %>%
      proxy %>% clearShapes() %>% clearControls() %>% addCircleMarkers(lng = ~Lon, lat = ~Lat,
                                                                       stroke = TRUE, radius = ~sqrt(Rain)*3, layerId = myData$Station,label = mytext,
                                                                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px",
                                                                                                                 "color" = "red",
                                                                                                                 "font-family" = "serif",
                                                                                                                 "font-style" = "italic",
                                                                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                                                                 "font-size" = "12px",
                                                                                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                                                                                 "text-align" = "center"),
                                                                                                    textsize = "13px", direction = "top"),
                                                                       weight = 2, opacity = 0.8, fillOpacity = 0.8, #popup = popop, #popup = ~paste(Station, ":", Rain, "mm"),
                                                                       color = "black", fillColor = ~couleurs(Rain)) %>%
        addPopups(-4.0, 5.6, content, options = popupOptions(closeButton = FALSE, closeOnClick = FALSE, minWidth = 100,
                                                            maxWidth = 400))} 
    else
      
      # proxy %>% clearShapes() %>% clearControls() %>% 
      proxy %>% clearPopups() %>% addCircleMarkers(lng = ~Lon, lat = ~Lat,
                                                   stroke = TRUE, radius = ~sqrt(Rain)*3, layerId = myData$Station,label = mytext,
                                                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px",
                                                                                             "color" = "red",
                                                                                             "font-family" = "serif",
                                                                                             "font-style" = "italic",
                                                                                             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                                             "font-size" = "12px",
                                                                                             "border-color" = "rgba(0,0,0,0.5)",
                                                                                             "text-align" = "center"),
                                                                                textsize = "13px", direction = "top"),
                                                   weight = 2, opacity = 0.8, fillOpacity = 0.8, #popup = popop, #popup = ~paste(Station, ":", Rain, "mm"),
                                                   color = "black", fillColor = ~couleurs(Rain)) %>%
      addLegend(pal = couleurs, values = ~Rain, opacity = 0.9, title = "Précipitation",layerId = "colorLegend")
    
    # proxy %>%
    #   clearShapes() %>%
    # 
    #   addPopups(-4, 5.5, content,
    #             options = popupOptions(closeButton = FALSE)
    #   ) %>%
    # 
    #   addCircleMarkers(lng = ~Lon, lat = ~Lat,
    #                    stroke = TRUE, radius = ~sqrt(Rain)*3, layerId = myData$Station,label = mytext,
    #                    labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px",
    #                                                              "color" = "red",
    #                                                              "font-family" = "serif",
    #                                                              "font-style" = "italic",
    #                                                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
    #                                                              "font-size" = "12px",
    #                                                              "border-color" = "rgba(0,0,0,0.5)",
    #                                                              "text-align" = "center"),
    #                                                 textsize = "13px", direction = "top"),
    #                    weight = 2, opacity = 0.8, fillOpacity = 0.8, #popup = popop, #popup = ~paste(Station, ":", Rain, "mm"),
    #                    color = "black", fillColor = ~couleurs(Rain)) %>%
    #   addLegend(pal = couleurs, values = ~Rain, opacity = 0.9, title = "Précipitation",layerId = "colorLegend")
    
    
  })
  
  
  observe({

    # # assign colours to countries to ensure consistency between plots
    # cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
    # cls_names = c(as.character(unique(gg_reactive_db()$station)))
    # station_cols = cls[1:length(cls_names)]
    # names(station_cols) = cls_names

    # rainfall-specific plots
    output$rain_plot <- renderPlotly({
      daily_rain_plot(gg_reactive_db())
    })

    # cumulative-specific plots
    output$rain_plot_cumulative <- renderPlotly({
      cumulative_rain_plot(gg_reactive_db())
    })

  })
  
  data <- reactiveValues(clickedMarker = NULL)
  # produce the basic leaflet map with single marker
  
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$map_marker_click,{
    # ids = as.character(sapply(filteredData(),"[[",1))
    # Lat = as.numeric(sapply(filteredData(),"[[",2))
    # Lon = as.numeric(sapply(filteredData(),"[[",3))
    # Rain_val = as.numeric(sapply(filteredData(),"[[",4))
    
    # myData <- data.frame(Station = ids, Lat = Lat, Lon = Lon, Rain = Rain_val)
    myData <- filteredData()
    # #print(myData)
    # print("observed map_marker_click")
    data$clickedMarker <- input$map_marker_click
    # print(data$clickedMarker)
    
    # str(data)
    ######### get ggplot data by click
    dat = 0
    val = 0
    #station = c("ABOBO_Mairie", "ATTECOUBE_Mairie", "BINGERVILLE_Mairie", "COCODY_Mairie", "COCODY_ONEP", "KOUMASSI_Mairie", "MARCORY_Mairie","SONGON-MAIRIE","YOPOUGON_ZONE")
    station = c("ABOBO-MAIRIE", "ATTECOUBE-MAIRIE", "BINGERVILLE", "COCODY-RIVIERA", "COCODY-ST-VIATEUR", "KOUMASSI", "MARCORY","SONGON-MAIRIE","YOPOUGON-ZONE")
    
    for (i in 1:9) {

      if (data$clickedMarker$id == station[i])
      {
        station_name = station[i]
        # dat = as_datetime(as.numeric(filteredData()[[i]][6:10]))
        # val = as.numeric(filteredData()[[i]][11:15])
      }
    }
    # 
    # gg_data = data.frame(dt_agg = dat, rain_agg = val)
    stat_id = station_name
    # 
    # print(c(stat_id, gg_data))
    
    output$myTable <- renderTable({
      return(
        subset(myData,Station == data$clickedMarker$id)
      )
    })
   
    output$Station_name <- renderText({
      paste0("Station " , prettyNum(stat_id, big.mark = ","))
    })
    
    # output$rain_plot_cumulative <- renderPlotly({
    #   cumulative_rain_plot(gg_reactive_db())
    # })
  })
 
}

shinyApp(ui = ui, server = server)

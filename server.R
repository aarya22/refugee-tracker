library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")
library("dplyr")
library("ggplot2")

server <- function(input, output) {
  
  graph.values <- read.csv('data/time_series.csv', stringsAsFactors = FALSE, fileEncoding
                           = "UTF-8-BOM")
  world <- geojsonio::geojson_read("json/countries.geo.json", what = "sp")
  
  # Examples to demonstrate lines
  mydf <- data.frame(Observation = c("A", "B"),
                     InitialLat = c(62.469722,48.0975),
                     InitialLong = c(6.187194, 16.3108),
                     NewLat = c(51.4749, 51.4882),
                     NewLong = c(-0.221619, -0.302621),
                     stringsAsFactors = FALSE)
  
  mydf2 <- data.frame(group = c("A", "B"),
                      lat = c(mydf$InitialLat, mydf$NewLat),
                      long = c(mydf$InitialLong, mydf$NewLong))
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/aarya22/cje65iy6k03db2rs272v1h8ld/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFyeWEyMiIsImEiOiJjamU2NWNvNWUwMDVrMzNvY3Q0NGluZ3o0In0.v3dVCyNNhVq-vFtL237dGw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.33251, lat = -7.324879e+00, zoom = 3) %>%
      addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group) %>%
      addPolygons(
         weight = 1,
         opacity = 0.7,
         dashArray = "3",
         color = "#d6d6d6",
         fillOpacity = 0.3,
         highlight = highlightOptions(
           weight = 5,
           color = "#666",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE)
      )
  })
  
  time.series <- read.csv('data/time_series.csv', stringsAsFactors = FALSE, fileEncoding
                          = "UTF-8-BOM")
  time.series[time.series=="*"]<-NA
  time.series <- na.omit(time.series)
  #browser()
  
  # How to organize the dataframe
  # 1. Combine all of the values for the same countries regardless of the type of refugeee for EACH YEAR
  # 2. Sort by descending order (Highest to lowest)
  new.order <- arrange(time.series, Value) 
  #new.order(complete.cases(1,),)
    
  # Create a table
  output$ranking <- renderTable({
    # If user clicks kilotons in the widget
    if (input$Direction == 'In') {
      # Filter the columns of interest
      in.year <- filter(time.series, Year == input$year, Population.type == input$Type)
  
      if (nrow(in.year) < 1){
        showNotification("No data available", type = "error")
        return()
      }
      in.year[,5] <- sapply(in.year[,5], as.numeric)
      in.year <- aggregate(x = in.year$Value, by = list(in.year$Year, in.year$Country...territory.of.asylum.residence), FUN = sum)
      colnames(in.year)[2] <- "Country"
      colnames(in.year)[3] <- "Value"
      in.data <- arrange(in.year, desc(Value))
      in.data <- in.data[1:20,]
      order.Value <- in.data$Value
      in.data$Rank <- NA
      in.data$Rank <- 1:nrow(in.data)
      in.data[,3] <- sapply(in.data[,3], as.character)
      in.data <- select(in.data, Rank, Country, Value)
      return(in.data)
      
      # User clicks "outgoing" in the widget
    } else {
      # Filter the columns of interest
      #browser()
      out.year <- filter(time.series, Year == input$year, Population.type == input$Type)
      if (nrow(out.year) < 1) {
        showNotification("No data available", type = "error")
        return()
      }
      out.year[,5] <- sapply(out.year[,5], as.numeric)
      out.year <- aggregate(x = out.year$Value, by = list(out.year$Year, out.year$Country...territory.of.asylum.residence), FUN = sum)
      colnames(out.year)[2] <- "Country"
      colnames(out.year)[3] <- "Value"
      out.data <- arrange(out.year, desc(Value))
      out.data <- out.data[1:20, ]
      order.Value <- out.data$Value
      out.data$Rank <- NA
      out.data$Rank <- 1:nrow(out.data)
      colnames(out.data)[2] <- "Origin"
      colnames(out.data)[3] <- "Leaving"
      out.data[,3] <- sapply(out.data[,3], as.character)
      out.data <- select(out.data, Rank, Origin, Leaving)
      return(out.data)
    }
  })
  
  output$graph <- renderPlot({
    graph.values <- filter(time.series, time.series$Country...territory.of.asylum.residence == input$Country,
                           time.series$Population.type == input$Type)
    if(nrow(graph.values) < 1) {
      showNotification("No data available", type = "error")
      return()
    }
    
    graph.values[,5] <- sapply(graph.values[,5], as.numeric)
    
    graph.values.grouped <- aggregate(x = graph.values$Value, by = list(graph.values$Year), FUN = sum)%>%
      na.omit(graph.values.grouped)
    colnames(graph.values.grouped)[1] <- "Year"
    colnames(graph.values.grouped)[2] <- "Value"
    
    #graph.values[,1] <- sapply(graph.values[,1], as.numeric)
    
    ggplot(data = graph.values.grouped) +
      geom_point(mapping = aes(x = Year, y = Value), color = "blue") +
      geom_line(mapping = aes(x = Year, y = Value), color = "blue")
  })
}
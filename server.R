library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")
library("dplyr")

server <- function(input, output) {
    
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
      # browser()
      in.year <- filter(time.series, Year == input$year, Population.type == input$Type)
      if (nrow(in.year) == 0){
        none <- "No data available"
        return(none)
      }
      in.year[,5] <- sapply(in.year[,5], as.numeric)
      in.data <- arrange(in.year, desc(Value))
      order.Value <- in.data$Value
      in.data$Rank <- NA
      in.data$Rank <- 1:nrow(in.data)
      colnames(in.data)[2] <- "Country"
      in.data[,5] <- sapply(in.data[,5], as.character)
      in.data <- select(in.data, Rank, Country, Population.type, Value)
      return(in.data)
      
      # User clicks "outgoing" in the widget
    } else {
      # Filter the columns of interest
      out.year <- filter(time.series, Year == input$year, Population.type == input$Type)
      if (nrow(out.year) == 0){
        none <- "No data available"
        return(none)
      }
      out.year[,5] <- sapply(out.year[,5], as.numeric)
      out.data <- arrange(out.year, desc(Value))
      order.Value <- out.data$Value
      out.data$Rank <- NA
      out.data$Rank <- 1:nrow(out.data)
      colnames(out.data)[2] <- "Country"
      colnames(out.data)[5] <- "Leaving"
      out.data[,5] <- sapply(out.data[,5], as.character)
      out.data <- select(out.data, Rank, Origin, Population.type, Leaving)
      return(out.data)
    }
  })
  
  output$graph <- renderTable({
    graph.values <- read.csv('data/time_series.csv', stringsAsFactors = FALSE, fileEncoding
                            = "UTF-8-BOM")
    graph.values <- filter(time.series, time.series$Country...territory.of.asylum.residence == input$Country,
                          graph.values$Population.type == input$Type)
    
    graph.values.grouped <- aggregate(x = graph.values$Value, by = list(graph.values$Year), FUN = sum)%>%
      na.omit(graph.values.grouped)
    colnames(graph.values.grouped)[1] <- "Year"
    colnames(graph.values.grouped)[2] <- "Value"
    
    graph.values[,1] <- sapply(graph.values[,1], as.numeric)
    graph.values[,5] <- sapply(graph.values[,5], as.numeric)
    
    ggplot(data = graph.values.grouped) +
      geom_point(mapping = aes(x = Year, y = Value), color = "blue") +
      geom_line(mapping = aes(x = Year, y = Value), color = "blue")
  })
}
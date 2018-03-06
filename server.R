library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")

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
  
  output$unHelped <- renderText({
    refugee.data <- read.csv('data/asylum_seekers.csv', stringsAsFactors = FALSE) 
    yearFiltered <- filter(refugee.data, Year == input$yearInput)
    un.assist <- sapply(yearFiltered$of.which.UNHCR.assisted.end.year., as.numeric)
    total.applied <- sapply(yearFiltered$Applied.during.year, as.numeric)
    percent.un.assist <- (sum(un.assist, na.rm = T) /
                                  sum(total.applied, na.rm = T)) * 100
    
    
  })
  
}
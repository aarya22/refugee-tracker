library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")
library("maps")
library("countrycode")

server <- function(input, output) {
  
  source("points_to_line.R")
  
  data("world.cities")
  world.cities$country.etc <- replace(world.cities$country.etc, 
                                      world.cities$country.etc == "Serbia and Montenegro", "Serbia")
  world.cities$country.etc <- countrycode(world.cities$country.etc,origin = "country.name", 
                                          destination = "iso3c")
  
  refugee <- read.csv("data/resettlement.csv", stringsAsFactors = FALSE)
  refugee$Country...territory.of.asylum.residence <- countrycode(refugee$Country...territory.of.asylum.residence,
                origin = "country.name", destination = "iso3c")
  refugee$Origin <- countrycode(refugee$Origin,origin = "country.name", destination = "iso3c")
  
  world <- geojsonio::geojson_read("json/countries.geo.json", what = "sp")
  
  # # Examples to demonstrate lines
  # mydf <- data.frame(Observation = c("A", "B"),
  #                    InitialLat = c(62.469722,48.0975),
  #                    InitialLong = c(6.187194, 16.3108),
  #                    NewLat = c(51.4749, 51.4882),
  #                    NewLong = c(-0.221619, -0.302621),
  #                    stringsAsFactors = FALSE)
  # 
  # mydf2 <- data.frame(group = c("A", "B"),
  #                     lat = c(mydf$InitialLat, mydf$NewLat),
  #                     long = c(mydf$InitialLong, mydf$NewLong))
  
  # Create the map
  output$rmap <- renderLeaflet({
    leaflet(world) %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/aarya22/cje65iy6k03db2rs272v1h8ld/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFyeWEyMiIsImEiOiJjamU2NWNvNWUwMDVrMzNvY3Q0NGluZ3o0In0.v3dVCyNNhVq-vFtL237dGw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.33251, lat = -7.324879e+00, zoom = 3) %>%
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
  
  # Gets the coordinates of a given country
  get_coords <- function(country) {
    host.coords <- filter(world.cities, country.etc == country, capital == "1") %>% select(lat, long)
    return(host.coords)
  }
  
  # Takes a query from the refugee file and transforms it into a dataframe
  getLatLong <- function(query, direction) {
    
    if (direction == "Incoming") {
      origin.countries <- query$Origin
      country <- query$Country...territory.of.asylum.residence[1]
    } else if (direction == "Outgoing") {
      origin.countries <- query$Country...territory.of.asylum.residence
      country <- query$Origin
    }
    
    # Get host country coordinates
    host.coords <- get_coords(country)
    
    # Get origin countries lat/long
    origin.coords <- lapply(origin.countries, get_coords)
    
    # Remove empty coordinates and respective countries
    empty.vals <- sapply(origin.coords, function(x) dim(x)[1]) > 0
    origin.coords <- origin.coords[empty.vals]
    origin.countries <- origin.countries[empty.vals]
    
    # Make Long/Lat dataframe
    origin.coords <- as.data.frame(origin.coords)
    originLat <- unname(unlist(origin.coords[ , grepl( "lat" , names( origin.coords ) ) ]))
    originLong <- unname(unlist(origin.coords[ , grepl( "long" , names( origin.coords ) ) ]))
    hostLat <- rep(host.coords$lat, length(originLat))
    hostLong <- rep(host.coords$long, length(originLong))
    coord.df <- data.frame(group = c(origin.countries),
                           lat = c(originLat, hostLat),
                           long = c(originLong, hostLong))
    return(coord.df)
  }
  
  showRefugeeLines <- function(country, year, direction) {
    query.in <- as.data.frame(filter(refugee, 
                                     refugee$Country...territory.of.asylum.residence == country, 
                                     refugee$Year==year))
    query.out <- as.data.frame(filter(refugee, 
                                      refugee$Origin == country, 
                                      refugee$Year==year))
    
    if (length(query.in$Origin) == 0 && direction == "Incoming") {
      # Have to output no data available
      return()
    }
    
    if (length(query.out$Origin) == 0 && direction == "Outgoing") {
      # Have to output no data available
      return ()
    }
    
    if ((length(query.out$Origin) == 0 || length(query.in$Origin) == 0) && direction == "Both") {
      # Have to output no data available
      return ()
    }
    
    if (direction == "Incoming") {
      coord.df.in <- getLatLong(query.in, direction)
      lines.in <- points_to_line(coord.df.in, "long", "lat", "group")
      
      leafletProxy("rmap") %>% addPolylines(data=lines.in, group = "in-lines", color = "red")
    } else if (direction == "Outgoing") {
      coord.df.out <- getLatLong(query.out, direction)
      browser()
      lines.out <- points_to_line(coord.df.out, "long", "lat", "group")
      
      leafletProxy("rmap") %>% addPolylines(data=lines.out, group = "out-lines", color = "blue")
    } else {
      coord.df.in <- getLatLong(query.in, "Incoming")
      lines.in <- points_to_line(coord.df.in, "long", "lat", "group")
      coord.df.out <- getLatLong(query.out, "Outgoing")
      lines.out <- points_to_line(coord.df.out, "long", "lat", "group")
      
      leafletProxy("rmap") %>% addPolylines(data=lines.in, group = "in-lines", color = "red")
      leafletProxy("rmap") %>% addPolylines(data=lines.out, group = "out-lines", color = "blue")
    }
    
  }
  
  observe({
    event <- input$rmap_shape_click
    if (is.null(event))
      return()
    leafletProxy("rmap") %>% clearGroup("in-lines") %>% clearGroup("out-lines")
    country <- map.where(x = event$lng, y = event$lat)
    country <- countrycode(country, origin = "country.name", destination = "iso3c")
    isolate({
      showRefugeeLines(country, input$Years, input$Direction)
    })
  })
  
  output$amap <- renderLeaflet({
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
  
}
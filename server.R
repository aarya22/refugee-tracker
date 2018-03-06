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
  
  # refugee <- read.csv("data/resettlement.csv", stringsAsFactors = FALSE)
  # refugee$Country...territory.of.asylum.residence <- countrycode(refugee$Country...territory.of.asylum.residence,
  #               origin = "country.name", destination = "iso3c")
  # refugee$Origin <- countrycode(refugee$Origin,origin = "country.name", destination = "iso3c")
  # 
  # asylum <- read.csv("data/asylum_seekers.csv", stringsAsFactors = FALSE)
  # asylum$Country...territory.of.asylum.residence <- countrycode(asylum$Country...territory.of.asylum.residence,
  #                                                               origin = "country.name", destination = "iso3c")
  # asylum$Origin <- countrycode(asylum$Origin,origin = "country.name", destination = "iso3c")
  # asylum[, 4:14] <- sapply(asylum[, 4:14], as.numeric)
  
  refugee <- read.csv("data/resettlement_map.csv", stringsAsFactors = FALSE)
  asylum <- read.csv("data/asylum_seekers_map.csv", stringsAsFactors = FALSE)
  
  
  asylum <- mutate(asylum, 
                   un.assist = of.which.UNHCR.assisted.start.year. + of.which.UNHCR.assisted.end.year.,
                   pending = Tota.pending.start.year + Total.pending.end.year,
                   recognized = decisions_recognized + decisions_other,
                   rejected = Rejected + Otherwise.closed)
  
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
           fillOpacity = 0.7)
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
    #browser()
    
    query.in <- as.data.frame(filter(refugee, 
                                     refugee$Country...territory.of.asylum.residence == country, 
                                     refugee$Year==year))
    query.out <- as.data.frame(filter(refugee, 
                                      refugee$Origin == country, 
                                      refugee$Year==year))
    
    if (length(query.in$Origin) == 0 && direction != "Outgoing") {
      # Have to output no data available
      print("No Incoming Data Available")
    } else if(direction != "Outgoing") {
      coord.df.in <- getLatLong(query.in, direction)
      lines.in <- points_to_line(coord.df.in, "long", "lat", "group")
      
      dec <- select(query.in, Value)
      dec <- as.numeric(unlist(dec))
      
      cnames <- countrycode(query.in$Origin, "iso3c", "country.name")
      
      labels = sprintf("<strong>%s</strong><br/>%g refugees incoming", cnames, dec) %>% lapply(htmltools::HTML)
      
      leafletProxy("rmap") %>% clearGroup("in-lines") %>% clearGroup("out-lines") 
      leafletProxy("rmap") %>% addPolylines(data=lines.in, group = "in-lines", color = "red",
                                            highlight = highlightOptions(
                                              weight = 10,
                                              color = "#666",
                                              dashArray = "",
                                              fillOpacity = 0.5,
                                              bringToFront = TRUE),
                                            label = labels,
                                            labelOptions = labelOptions(
                                              style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")
                                           )
    }
    
    if (length(query.out$Origin) == 0 && direction != "Incoming") {
      # Have to output no data available
      print("No Outgoing Data Available")
    } else if(direction != "Incoming") {
      coord.df.out <- getLatLong(query.out, direction)
      lines.out <- points_to_line(coord.df.out, "long", "lat", "group")
      
      dec <- select(query.out, Value)
      dec <- as.numeric(unlist(dec))
      
      cnames <- countrycode(query.out$Origin, "iso3c", "country.name")
      
      labels = sprintf("<strong>%s</strong><br/>%g refugees outgoing", cnames, dec) %>% lapply(htmltools::HTML)
      
      leafletProxy("rmap") %>% clearGroup("out-lines") %>% clearGroup("in-lines")
      leafletProxy("rmap") %>% addPolylines(data=lines.out, group = "out-lines", color = "blue",
                                            highlight = highlightOptions(
                                              weight = 10,
                                              color = "#666",
                                              dashArray = "",
                                              fillOpacity = 0.5,
                                              bringToFront = TRUE),
                                            label = labels,
                                            labelOptions = labelOptions(
                                              style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")
                                           ) 
    }
  }
  
  observe({
    event <- input$rmap_shape_click
    if (is.null(event))
      return()
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
          fillOpacity = 0.7)
      )
  })
  
  observe({
    event <- input$amap_shape_click
    if (is.null(event))
      return()
    country <- map.where(x = event$lng, y = event$lat)
    country <- countrycode(country, origin = "country.name", destination = "iso3c")
    isolate({
      showAsylumLines(country, input$asYears, input$Decisions)
    })
  })
  
  showAsylumLines <- function(country, year, decision) {
    
    if (decision == "All") {
      decision = "Total.decisions" #Make all into sum of recognized, rejected, un.assist and pending
    } else if (decision == "Recognized") {
      decision = "recognized"
    } else if (decision == "Rejected") {
      decision = "rejected"
    } else if (decision == "UN-Assisted") {
      decision = "un.assist"
    } else if (decision == "Pending") {
      decision = "pending"
    }
    
    query <- as.data.frame(filter(asylum, 
                                  asylum$Country...territory.of.asylum.residence == country, 
                                  asylum$Year==year) %>% select(Year, Country...territory.of.asylum.residence, Origin, decision))
    query <- query[complete.cases(query), ]
    
    names(query)[4] <- "decision"
    
    query <- aggregate(. ~ Country...territory.of.asylum.residence + Origin + Year, data = query, sum)
    query <- filter(query, decision > 0)
    
    if (length(query$Origin) == 0) {
      print("No Data Available")
      return()
    }
    
    #browser()
    coord.df <- getLatLong(query, "Incoming")
    lines <- points_to_line(coord.df, "long", "lat", "group")
    
    dec <- select(query, decision)
    dec <- as.numeric(unlist(dec))
    
    cnames <- countrycode(query$Origin, "iso3c", "country.name")
    
    labels = sprintf("<strong>%s</strong><br/>%g applications", cnames, dec) %>% lapply(htmltools::HTML)
    
    leafletProxy("amap") %>% clearGroup("as-lines") 
    leafletProxy("amap") %>% addPolylines(data=lines, group = "as-lines", color = "green",
                                          highlight = highlightOptions(
                                            weight = 10,
                                            color = "#666",
                                            dashArray = "",
                                            fillOpacity = 0.5,
                                            bringToFront = TRUE),
                                          label = labels,
                                          labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")
                                          )
  }
  
}
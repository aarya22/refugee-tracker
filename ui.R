library("leaflet")
library("shiny")

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("Refugee-Tracker", id="nav",
           
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
      ),
                
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("ZIP explorer"),
                    
                    selectInput("color", "Color", vars),
                    selectInput("size", "Size", vars, selected = "adultpop")
                    
                    # plotOutput("histCentile", height = 200),
                    # plotOutput("scatterCollegeIncome", height = 250)
      )
    )
  ),
  
  tabPanel("Ranking", tableOutput("ranking"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
         width = 330, height = "auto",
                         
         h2("ZIP explorer"),
                         
         # Input: dropdown menu to select the year
         selectInput(input = 'year', label = "Select Year:", 
            choices = list('1951' = '1951', 
                           "1952" = '1952',
                           "1953" = '1953',
                           "1954" = '1954',
                           "1955" = '1955',
                           "1956" = '1956',
                           "1957" = '1957',
                           "1958" = '1958',
                           "1959" = '1959',
                           "1960" = '1960',
                           "1961" = '1961',
                           "1962" = '1962',
                           "1963" = '1963',
                           "1964" = '1964',
                           "1965" = '1965',
                           "1966" = '1966',
                           "1967" = '1967',
                           "1968" = '1968',
                           "1969" = '1969',
                           "1970" = '1970',
                           "1971" = '1971',
                           "1972" = '1972',
                           "1973" = '1973',
                           "1974" = '1974',
                           "1975" = '1975',                                                
                           "1976" = '1976',
                           "1977" = '1977',
                           "1978" = '1978',
                           "1979" = '1979',
                           "1980" = '1980',
                           "1981" = '1981',
                           "1982" = '1982',
                           "1983" = '1983',
                           "1984" = '1984',
                           "1985" = '1985',
                           "1986" = '1986',
                           "1987" = '1987',
                           "1988" = '1988',
                           "1989" = '1989',
                           "1990" = '1990',
                           "1991" = '1991',
                           "1992" = '1992',
                           "1993" = '1993',
                           "1994" = '1994',
                           "1995" = '1995',
                           "1996" = '1996',
                           "1997" = '1997',
                           "1998" = '1998',
                           "1999" = '1999',
                           "2000" = '2000',
                           "2001" = '2001',
                           "2002" = '2002',
                           "2003" = '2003',
                           "2004" = '2004',
                           "2005" = '2005',
                           "2006" = '2006',
                           "2007" = '2007',
                           "2008" = '2008',
                           "2009" = '2009',
                           "2010" = '2010',
                           "2011" = '2011',
                           "2012" = '2012',
                           "2013" = '2013',
                           "2014" = '2014',
                           "2015" = '2015',
                           "2016" = '2016')),
                         
    # Input: Use dropdown to select unit of measurement
    selectInput(input = 'Direction', label = "Select outgoing or incoming refugee data:", 
                choices = list('In' = 'In', 
                               'Out'= 'Out')),
    
    # Input: Use dropdown to select unit of measurement
    selectInput(input = 'Type', label = "Select type of refugee:", 
                choices = list('Refugees (incl. refugee-like situations)' = 'Refugees (incl. refugee-like situations)',
                               "Returnees" = 'Returnees',
                               "Returned IDPs" = 'Returned IDPs',
                               "Stateless" = 'Stateless',
                               "Asylum-seekers" = 'Asylum-seekers',
                               "Others of concern" = 'Others of concern',
                               'Internally displaced persons'= 'Internally displaced persons'))
      )
    )
)

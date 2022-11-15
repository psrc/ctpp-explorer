library(shinythemes)
library(sf)
library(scales)
library(leaflet)

table.name.selector <- selectInput("tbl_name", 
                                   "Select a table to map:", 
                                   table.names()
                                   )

map.sidebar <- sidebarPanel(width = 3,
                            table.name.selector,
                            p("Push this:"),
                            actionButton("go", "Do the thing" )
                            )

map.panel <- mainPanel(width = 9,
                       leafletOutput("sov_shares")
                       #p('The leaflet map will go here.')
                       )



ui <- fluidPage(
  titlePanel("", windowTitle = "CTPP Explorer"),
  theme = "bootstrap_united.css",
  navbarPage("CTPP Explorer",
        sidebarLayout(
          map.sidebar,
          map.panel
        )
  )
)
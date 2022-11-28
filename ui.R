table.name.selector <- selectInput("tbl_name", 
                                   "Select a table to map:", 
                                   table.names()
                                   )

map.sidebar <- sidebarPanel(width = 3,
                            table.name.selector,
                            actionButton("go", "Map it" )
                            )

map.panel <- mainPanel(width = 9,
                       leafletOutput("sov_shares", height='85vh')
                       #p('The leaflet map will go here.')
                       )



ui <- fluidPage(
  titlePanel("", windowTitle = "Worker Commute Data Exploration from Census Transportation Package"),
  theme = "bootstrap_united.css",
  navbarPage("Worker Commute Data from the Census Transportation Planning Package(CTPP)",
        sidebarLayout(
          map.sidebar,
          map.panel
        )
  )
)
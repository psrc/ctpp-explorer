table.name.selector <- selectInput("tbl_name", 
                                   "Select a table to map:", 
                                   table.names()
                                   )

map.sidebar <- sidebarPanel(width = 3,
                            table.name.selector,
                            actionButton("go", "Map it" ),
                            uiOutput('downloadData'))
                            

map.panel <- mainPanel(width = 9,
                       div(img(src='ctpp-aashto.png', width="10%", height ="10%", style = "align:top"),
                           img(src='psrc_logo.png', width="20%", height ="20%", style = "align:top") ),
                       uiOutput("ctpp_url"),
                       uiOutput("psrc_url"),
                       leafletOutput("sov_shares", height='85vh'),
                       #p('The leaflet map will go here.'),
                       )


ui <- fluidPage(
  titlePanel("", windowTitle = "Commute Data Exploration from Census Transportation Planning Package"),
  theme = "bootstrap_united.css",
  navbarPage("Commute Data in the Puget Sound Region from the Census Transportation Planning Package(CTPP)",

        sidebarLayout(
          map.sidebar,
          map.panel
  ),
  
  )
)

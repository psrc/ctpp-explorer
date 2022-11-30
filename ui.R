table.name.selector <- selectInput("tbl_name", 
                                   "Select a table to map", 
                                   table.names()
                                   )

card.agencies <- card(
  card_header("Sources"),
  div(
  a(img(src='ctpp-aashto.png', width="20%"), href="https://ctpp.transportation.org/"),
  a(img(src='psrc_logo.png', width="60%"), href="https://www.psrc.org/"),
  class = 'text-center')
      
)

map.sidebar <- sidebarPanel(width = 3,
                            table.name.selector,
                            actionButton("go", "Map it" ),
                            br(),
                            br(),
                            downloadButton("download", "Download Data"),
                            br(),
                            br(),
                            layout_column_wrap(
                              width = 1,
                              height = 200,
                              card.agencies
                            )
)
                            

map.panel <- mainPanel(width = 9,
                       leafletOutput("sov_shares", height='85vh')
                       )



ui <- fluidPage(
  navbarPage(HTML("Commute Data from the Census Transportation Planning Package <br/> 
             in Puget Sound Regional Growth Centers"),
             theme = bs_theme(bootswatch = "united",
                              base_font = font_google("Poppins")),
             useShinyjs(),
             
             windowTitle = "Commute Data Exploration from CTPP",
             
             sidebarLayout(
               map.sidebar,
               map.panel
             ),
             
  )
)

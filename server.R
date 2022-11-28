
server <- function(input, output, session){
  
  summary_per_rgc <- eventReactive(input$go, {
    
    ctpp_df<-get_psrc_ctpp_api(table_code= input$tbl_name,  scale = 'tract', dyear=2016)
    centers_tracts <- rgc_tracts(2010)
    ctpp_df<-transform_geoid(ctpp_df)
    ctpp_df$estimate<- as.numeric(gsub(",", "", ctpp_df$estimate))

    ctpp_df <- ctpp_df %>%
      inner_join(centers_tracts, by=c('GEOID' = 'geoid')) %>%
      filter(category %in% c('Total', 'Car, truck, or van -- Drove alone')) %>%
      mutate(short.category = ifelse(category %in% c('Car, truck, or van -- Drove alone'), 'sov', category)) %>%
      rename(rgc=name) %>%
      group_by(short.category, rgc) %>%
      summarize(tot_estimate=sum(estimate)) %>%
      pivot_wider(names_from=short.category, values_from=tot_estimate) %>%
      #rename(sov = Car, truck, or van -- Drove alone) %>%
      mutate(sov_share = sov / Total)
    
    
    centers_tracts <- rgc_tracts(2010)
    rgc.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
    rgc.lyr <- st_read(rgc.url)
    
    create_rgc_map_ctpp(rgc.tbl=ctpp_df, 
                          rgc.lyr=rgc.lyr, 
                          map.title=names(table.names()[table.names() == input$tbl_name]),
                          legend.title='Percent of Workers', 
                          legend.subtitle='Using Single Occupancy Vehicles (SOV)')
      
  })
  
  
  transform_geoid <- function(df){
    #rename work_geoid or res_geoid to just "GEOID"
    if('work_geoid' %in% colnames(df)) {
      out_df <- df %>%
        rename(GEOID=work_geoid)
    }  
    if('res_geoid' %in% colnames(df)) {
      out_df <- df %>%
        rename(GEOID=res_geoid)
    }
    out_df
  }
  
  
  rgc_tracts <- function(cen_year) {
    
    sqlite_connection <- dbConnect(RSQLite::SQLite(), './data/ctpp_explorer.db')
    tracts_sql<- paste0("select geoid, min([name]) as name from [dbo.v_rgc_tracts] where census_year = ", cen_year, " group by geoid")
    tracts_df <-  dbGetQuery(sqlite_connection, SQL(tracts_sql))
    tracts_df 
  }
  
  output$sov_shares <- renderLeaflet(
    summary_per_rgc()
  )
  
  
  create_rgc_map_ctpp <- function(rgc.tbl, rgc.lyr,
                                    map.title = NULL, map.subtitle = NULL,
                                    map.title.position = "topright",
                                    legend.title = NULL, legend.subtitle = NULL,
                                    map.lat=47.615, map.lon=-122.257, map.zoom=8.5, wgs84=4326){
    
    
    tbl <- rgc.tbl 
    
    c.layer <- dplyr::left_join(rgc.lyr,tbl, by = c("name"="rgc")) %>%
      sf::st_transform(wgs84)
    
    purples_inc = c("#E3C9E3", "#C388C2", "#AD5CAB", "#91268F", "#630460", "#4A0048")
    color.ramp <- colorRamp(purples_inc, interpolate="spline")
    pal <- leaflet::colorNumeric(palette=color.ramp, domain = c.layer$sov_shares)
    
    
    labels <- paste0( "Center: ", c.layer$name, "<p></p>",
                     "SOV Share: ", percent(c.layer$sov_share, accuracy=1)) %>% 
                        lapply(htmltools::HTML)
    
    tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
     }
    "))
    
    title <- tags$div(
      tag.map.title, HTML("Regional Growth Center Worker Single Occupancy Mode Shares, 2012-2016")
    )
    
    
    m <- leaflet::leaflet() %>%
      leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
      
      leaflet::addControl(html = paste(map.title, '<br/>', map.subtitle),
                          position = map.title.position,
                          layerId = 'mapTitle') %>%
      
      leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
      leaflet::addProviderTiles("CartoDB.PositronNoLabels") %>%
      leaflet::addProviderTiles("CartoDB.PositronOnlyLabels",
                                options = leaflet::leafletOptions(pane = "maplabels"),
                                group = "Labels") %>%
      
      leaflet::addEasyButton(leaflet::easyButton(icon="fa-globe",
                                                 title="Region",
                                                 onClick=leaflet::JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
      leaflet::addPolygons(data=c.layer,
                           fillOpacity = 0.9,
                           fillColor = pal(c.layer$sov_share),
                           weight = 0.7,
                           color = "#BCBEC0",
                           group="Population",
                           opacity = 0,
                           stroke=FALSE,
                           options = leaflet::leafletOptions(pane = "polygons"),
                           dashArray = "",
                           highlight = leaflet::highlightOptions(
                             weight =5,
                             color = "76787A",
                             dashArray ="",
                             fillOpacity = 0.9,
                             bringToFront = TRUE),
                           label = labels,
                           labelOptions = leaflet::labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto")) %>%
      
      leaflet::addLegend(pal = pal,
                         values = c.layer$sov_share,
                         labFormat = labelFormat(
                           suffix = "%",
                           transform = function(x) 100 * x
                         ),
                         position = "bottomright",
                         title = paste(legend.title, '<br>', legend.subtitle)) %>%
      
      leaflet::addLayersControl(baseGroups = "CartoDB.PositronNoLabels",
                                overlayGroups = c("Labels", "Population")) %>%
      
      addControl(title, position = "topleft", className="map-title")%>%
      
      addTiles(attribution = 'CTPP 2012-2016 Journey to Work Data')%>%
      
      leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)
    
    return(m)
    
  } 
}
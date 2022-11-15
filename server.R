library(dplyr)
library(ggplot2)
library(psrcctpp)
library(leaflet)
library(odbc)
library(DBI)

server <- function(input, output, session){
  
  
  workers_per_tract <- eventReactive(input$go, {
    ctpp_df<-get_psrc_ctpp(dyear=2016, data_table = input$tbl_name, scale = 'tract')
    ctpp_df<-transform_geoid(ctpp_df)
    ctpp_df$estimate<- as.numeric(gsub(",", "", ctpp_df$estimate))
    ctpp_sov<- ctpp_df %>%
      filter(category=='Car, truck, or van -- Drove alone' )
    ctpp_total<-ctpp_df %>%
      filter(category=='Total')
    workers_mode_sov_share<-merge(ctpp_sov, ctpp_total,by= 'GEOID', suffixes=c('_sov','_total')) %>%
      mutate(estimate = estimate_sov/estimate_total)
    
    centers_tracts <- rgc_tracts(2010)
    tract.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/tract2010_nowater/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
    tract.lyr <- st_read(tract.url)
    tract.lyr <- tract.lyr %>%
      inner_join(centers_tracts, by=c('GEOID10' = 'geoid'))
    
    
    create_tract_map_ctpp(tract.tbl=workers_mode_sov_share, 
                          tract.lyr=tract.lyr, 
                          map.title='Drive Alone Share by Work Location', 
                          legend.title='Percent of Workers', 
                          legend.subtitle='by Work Tract')
      
  })
  
  transform_geoid <- function(df){
    #rename work_geoid or res_geoid to just "GEOID"
    work.geoid.sum <- sum(df$work_geoid == "")
    res.geoid.sum  <- sum(df$res_geoid  == "")
    if(work.geoid.sum == 0) {
      out_df <- df %>%
        rename(GEOID=work_geoid)
    } else if(res.geoid.sum == 0) {
      out_df <- df %>%
        rename(GEOID=res_geoid)
    }
    out_df
  }
  
  
  rgc_tracts <- function(cen_year) {
    
    elmergeo_connection <- dbConnect(odbc::odbc(),
                                  driver = "SQL Server",
                                  server = "AWS-PROD-SQL\\Sockeye",
                                  database = "ElmerGeo",
                                  trusted_connection = "yes"
    ) 
    tracts_df <-  dbReadTable(elmergeo_connection, SQL("dbo.v_rgc_tracts"))
    tracts_df %>%
      filter(census_year == cen_year)
  }
  
  output$sov_shares <- renderLeaflet(
    workers_per_tract()
  )
  
  create_tract_map_ctpp <- function(tract.tbl, tract.lyr,
                                    map.title = NULL, map.subtitle = NULL,
                                    map.title.position = NULL,
                                    legend.title = NULL, legend.subtitle = NULL,
                                    map.lat=47.615, map.lon=-122.257, map.zoom=8.5, wgs84=4326){
    
    
    # Summarize and Aggregate Tract Data by Year and Attribute to Map and join to tract layer for mapping
    # rename census value column to estimate to match ACS
    # also allow for the easy mapping of equity geographies
    tbl <- tract.tbl %>%
      dplyr::select(.data$GEOID,.data$estimate) %>%
      dplyr::mutate(dplyr::across(c('GEOID'), as.character))%>%
      dplyr::group_by(.data$GEOID) %>%
      dplyr::summarise(Total=sum(.data$estimate))
    
    tract.lyr<-tract.lyr%>%
      # make geo names across 2010 and 2020
      dplyr::rename_at(dplyr::vars(matches("GEOID10")),function(x) "geoid") %>%
      dplyr::rename_at(dplyr::vars(matches("GEOID20")),function(x) "geoid")
    
    c.layer <- dplyr::left_join(tract.lyr,tbl, by = c("geoid"="GEOID")) %>%
      sf::st_transform(wgs84)
    
    pal <- leaflet::colorNumeric(palette="Purples", domain = c.layer$Total)
    
    
    labels <- paste0("Census Tract: ", c.layer$geoid, '<p></p>',
                     'SOV Share: ', c.layer$Total) %>% lapply(htmltools::HTML)
    
    m <- leaflet::leaflet() %>%
      leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
      leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
      
      leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                                options = leaflet::leafletOptions(pane = "maplabels"),
                                group = "Labels") %>%
      
      leaflet::addEasyButton(leaflet::easyButton(icon="fa-globe",
                                                 title="Region",
                                                 onClick=leaflet::JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
      leaflet::addPolygons(data=c.layer,
                           fillOpacity = 0.7,
                           fillColor = pal(c.layer$Total),
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
                             fillOpacity = 0.7,
                             bringToFront = TRUE),
                           label = labels,
                           labelOptions = leaflet::labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto")) %>%
      
      leaflet::addLegend(pal = pal,
                         values = c.layer$Total,
                         position = "bottomright",
                         title = paste(legend.title, '<br>', legend.subtitle)) %>%
      
      leaflet::addControl(html = paste(map.title, '<br>', map.subtitle),
                          position = map.title.position,
                          layerId = 'mapTitle') %>%
      
      leaflet::addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                                overlayGroups = c("Labels", "Population")) %>%
      
      leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)
    
    return(m)
    
  } 
}
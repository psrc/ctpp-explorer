# library(dplyr)
# library(ggplot2)
# library(psrcctpp)
# library(leaflet)
# library(odbc)
# library(DBI)
# library(glue)
# library(tidyr)

server <- function(input, output, session){
  
  summary_per_rgc <- eventReactive(input$go, {
    ctpp_df<-get_psrc_ctpp(dyear=2016, data_table = input$tbl_name, scale = 'tract')
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
                          map.title='Drive Alone Share by Work Location', 
                          legend.title='Percent of Workers', 
                          legend.subtitle='by Tract')
      
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
    tracts_sql<- "select geoid, min([name]) as name from dbo.v_rgc_tracts where census_year = {cen_year} group by geoid"
    tracts_df <-  dbGetQuery(elmergeo_connection, SQL("select geoid, min([name]) as name from dbo.v_rgc_tracts group by geoid"))
    tracts_df 
  }
  
  output$sov_shares <- renderLeaflet(
    summary_per_rgc()
  )
  
  
  create_rgc_map_ctpp <- function(rgc.tbl, rgc.lyr,
                                    map.title = NULL, map.subtitle = NULL,
                                    map.title.position = NULL,
                                    legend.title = NULL, legend.subtitle = NULL,
                                    map.lat=47.615, map.lon=-122.257, map.zoom=8.5, wgs84=4326){
    
    
    tbl <- rgc.tbl 
    
    c.layer <- dplyr::left_join(rgc.lyr,tbl, by = c("name"="rgc")) %>%
      sf::st_transform(wgs84)
    
    color.ramp <- colorRamp(c("#d0f7da", "#077363"), interpolate="spline")
    pal <- leaflet::colorNumeric(palette=color.ramp, domain = c.layer$sov_shares)
    
    
    labels <- paste0( "Center: ", c.layer$name, "<p></p>",
                     "SOV Share: ", c.layer$sov_share) %>% 
                        lapply(htmltools::HTML)
    
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
                             fillOpacity = 0.7,
                             bringToFront = TRUE),
                           label = labels,
                           labelOptions = leaflet::labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto")) %>%
      
      leaflet::addLegend(pal = pal,
                         values = c.layer$sov_share,
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
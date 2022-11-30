library(shiny)
library(DBI)
library(dplyr)
library(ggplot2)
library(glue)
library(leaflet)
library(odbc)
library(psrcctpp)
library(scales)
library(sf)
library(shinythemes)
library(tidyr)
library(RSQLite)
library(gdata)
library(shinyjs)
library(bslib)

# remotes::install_github("rstudio/bslib")

table.names <- function() {
  c("Workplace: Means of Transportation"= "A202105", "Residence: Means of Transportation" = "A102106")
}

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
                           direction = "auto",
                           font="Poppins")) %>%
    
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
    
    leaflet::setView(lng=map.lon, lat=map.lat, zoom=map.zoom)
  
  return(m)
} 


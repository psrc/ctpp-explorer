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



table.names <- function() {
  c("Workplace: Means of Transportation"= "A202105", "Residence: Means of Transportation" = "A102106")
}


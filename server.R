server <- function(input, output, session){
  # bs_themer()
  
  summary_per_rgc <- eventReactive(input$go, {
    
    ctpp_df <- get_psrc_ctpp(table_code= input$tbl_name,  scale = 'tract', dyear=2016)
    centers_tracts <- rgc_tracts(2010)
    ctpp_df <- transform_geoid(input$tbl_name, ctpp_df)
    ctpp_df$estimate <- as.numeric(gsub(",", "", ctpp_df$estimate))

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
    
  })
  
  map_rgcs<-eventReactive(input$go,{
    ctpp_df<-summary_per_rgc()
    centers_tracts <- rgc_tracts(2010)
    rgc.url <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson"
    rgc.lyr <- st_read(rgc.url)
    
    create_rgc_map_ctpp(rgc.tbl=ctpp_df, 
                          rgc.lyr=rgc.lyr, 
                          map.title=names(table.names()[table.names() == input$tbl_name]),
                          legend.title='Percent of Workers', 
                          legend.subtitle='Using Single Occupancy Vehicles (SOV)')
    
    
  })
  
  transform_geoid <- function(table_code, df){
    #rename work_geoid or res_geoid to just "GEOID"

    if(startsWith(table_code, 'A1') | startsWith(table_code, 'B1')) {
      out_df <- df %>%
        rename(GEOID=res_geoid)
    } else if(startsWith(table_code, 'A2') | startsWith(table_code, 'B2')){
      out_df <- df %>%
        rename(GEOID=work_geoid)
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
    map_rgcs()
  )
  
  # Enable/Disable Download button ----
  
  v <- reactiveValues(t = NULL,
                      go = 0)
  
  observeEvent(input$go, {
    # update reactiveValues when 'Map it' button is clicked
    
    v$t <- input$tbl_name
    v$go <- v$go + 1
  })

  observe({
    # disable and enable download button if inputs change or 'Map it' button is clicked
    
    if (v$go == 0 || (v$t != input$tbl_name) ) {
      disable("download")
    } else if (v$go > 0) {
      enable("download")  
    }
  })
  
  output$download<- downloadHandler(filename= function(){
    paste0('ctpp_2016_psrc_rgc_',input$tbl_name, '.csv')},
                                         content= function(file){
                                         write.csv(summary_per_rgc(), file)})
  
}
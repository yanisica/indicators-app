rm(list=ls())

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Resources used: https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html

library(shiny)
#library(shinycssloaders)
#library(shinybusy)

library(sf)
library(DT)

library(leaflet)
library(rgdal)
library(rmapshaper)
library(sf)

library(dplyr)
library(tidyr)
library(data.table)

source('C:/Users/yanis/Documents/scripts/checks_taxotools.R')

# dir
data_dir = 'C:/Users/yanis/Documents/scripts/caspian-app/data'

# Prep data----
# 
# #species lists
# nspez <- fread(paste0(data_dir,"/species/Neobiota_ist_nSpez_Gemeinden.gz"))
# liste <- fread(paste0(data_dir,"/species/Neobiota_IstPot_Liste_Gemeinden.gz"))
# 
# # regions
# regions <- read_sf(paste0(data_dir,"/regions/DEU_ADM_3.shp"))
# 
# # Build joins
# regions_nspez = regions %>% 
#   inner_join(nspez, by = 'NAME_3') %>% 
#   dplyr::select(NAME_3, nSpez, NAME_2,geometry) %>% 
#   # cleaning step that should be removed
#   distinct(NAME_3, .keep_all = TRUE)
# 
# write_sf(regions_nspez, 'C:/Users/yanis/Documents/scripts/caspian-app/data/regions/regions_nspez.shp')
# 
# # simplify geometry
# regions_nspez_simp <- ms_simplify(regions_nspez)
# write_sf(regions_nspez_simp, 'C:/Users/yanis/Documents/scripts/caspian-app/data/regions/regions_nspez_simp.shp')
# 
# # species occurrences
# point_data <- fread(paste0(data_dir,"/species/Neobiota_AllOccurrences.gz")) 
# 
# ab_sp = filter(point_data %>%  group_by(Taxon) %>% count(), n >  100718)
# 
# point_data2 = point_data %>% 
#   distinct()
# 
# point_data3 = point_data %>% 
#   filter(!Taxon %in% ab_sp$Taxon) %>% 
#   rbind(point_data2)


# Shiny app----

## load data----

# species list (by region)
data <- fread(paste0(data_dir,"/species/Neobiota_IstPot_Liste_Gemeinden.gz"))

# regions
map <- readOGR("C:/Users/yanis/Documents/scripts/caspian-app/data/regions/regions_nspez.shp")
map_simp <- readOGR("C:/Users/yanis/Documents/scripts/caspian-app/data/regions/regions_nspez_simp.shp")

# species occurrences
point_data <- fread(paste0(data_dir,"/species/Neobiota_AllOccurrences.gz")) %>% 
  distinct()


## build app----
# ui object (ui <- fluidPage( ))
ui <- fluidPage(
  
  ### Title
  #titlePanel("title"),
  titlePanel(p("Interactive map", style = "color:#3474A7")),
  
  ### Layout
  sidebarLayout(
    ## Sidebar panel
    #sidebarPanel("sidebar panel for inputs"),
    sidebarPanel(
      
      # add menu for selection
      selectizeInput(
        inputId = "municipalitySelected",
        label = "Select municipality",
        choices = c("All",unique(map@data$NAME_3))
      ),
      selectizeInput(
        inputId = "spSelected",
        label = "Select species",
        choices = c("All",unique(data$Taxon))
      ),
      #Warning message: The select input "municipalitySelected" contains a large number of options; 
      #consider using server-side selectize for massively improved performance. See the Details section of the ?selectizeInput help topic. 
      
      # adds text
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), "."),
      
    ),
    
    ## Main panel
    #mainPanel("main panel for outputs")
    mainPanel(
      # interactive map
      leafletOutput(outputId = "map"),
      # output table
      DTOutput(outputId = "table")
    )
  )
)


# server function (server <- function(input, output){})
server <- function(input, output){
  
  # render the output table 
  output$table <- renderDT(
    # show only species in the selected municipality
    datafiltered <- data[which(data$NAME_3 == input$municipalitySelected),2:5 ]
                           )

  # render interactive map
  output$map <- renderLeaflet({
    
    # add data to map
    if(input$municipalitySelected == "All"){
      mapfiltered <- map_simp
    }else {
      mapfiltered <- map[which(map@data$NAME_3 == input$municipalitySelected), ]
    }

    # create leaflet
    pal <- colorBin("YlOrRd", domain = mapfiltered$nSpez, bins = 7)
    
    labels <- sprintf("%s: %g", mapfiltered$NAME_3, mapfiltered$nSpez) %>%
      lapply(htmltools::HTML)
    
    #plot the base map
    leaflet(mapfiltered) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(nSpez),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels) %>%
      leaflet::addLegend(
        pal = pal, values = ~nSpez,
        opacity = 0.7, title = "Observed species")
  })
  
  # add species occurrences
  
  observeEvent(input$spSelected, {

    spfiltered <- point_data[which(point_data$Taxon == input$spSelected), ]

    leafletProxy("map", data = spfiltered) %>%
      clearMarkers() %>% 
      addMarkers(
        lat = spfiltered$Breitengrad,
        lng = spfiltered$Laengengrad
      )
      # addCircles(
      #   lat = spfiltered$Breitengrad,
      #   lng = spfiltered$Laengengrad,
      #   color = "#000000",
      #   fillColor = "#000000",
      #   weight = 5
      # )

})
  
}

# shinyApp()
shinyApp(ui = ui, server = server)

# Run the app-----
runApp("C:/Users/yanis/Documents/scripts/caspian-app/shiny-app")

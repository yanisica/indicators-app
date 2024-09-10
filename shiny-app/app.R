rm(list=ls())

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
#library(shinycssloaders)
#library(shinybusy)
library(DT) # interface to the JavaScript library DataTables. It allows you to display R dataframes (or matrices) as interactive tables in HTML pages, such as in a Shiny app.

library(leaflet)
#library(rgdal)
#library(rmapshaper)
library(sf)

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(stringr)


# dir
data_dir = 'C:/Users/JLU-SU/Documents/GitHub/indicators-app/data'

# Prep data----
# Detailed workflows in indicator repo: https://github.com/yanisica/indicators


# Shiny app----

## load data----

# all indicators extracted
dataset <- fread(paste0(data_dir,"/IPBES-TSU-KND-2023-Indicators-Dataset-v1.1.csv"))
names(dataset)
#DT::datatable(dataset)

# manipulate indicators dataset
dataset_beauty <- dataset %>% 
  # remove entries that were wrongly extracted
  filter(!is.na(Categories)) %>% 
  # indic from assessments
  mutate(ga = gsub(1, 'GA_IPBES', ga)) %>% 
  mutate(sua = gsub(1, 'SUA_IPBES', sua)) %>% 
  mutate(va = gsub(1, 'VA_IPBES', va)) %>% 
  mutate(ias = gsub(1, 'IAS_IPBES', ias)) %>% 
  mutate(geo = gsub(1, 'GEO6', geo)) %>% 
  mutate(ipcc = gsub(1, 'AR6_WG1_IPCC', ipcc)) %>% 
  rowwise() %>% mutate(Assessments = paste(ga,sua,va,ias,geo,ipcc, sep = ",")) %>% 
  rowwise() %>% mutate(Assessments = gsub("NA,", '', Assessments)) %>% 
  rowwise() %>% mutate(Assessments = gsub(",NA", '', Assessments)) %>% 
  rowwise() %>% mutate(Assessments = trimws(Assessments, whitespace = ",")) %>% 
  rowwise() %>% mutate(Assessments = gsub(",,", '', Assessments)) %>% 
  rowwise() %>% mutate(Assessments = gsub("NA$", '', Assessments)) %>% 
  
  # indic from MEAs
  mutate(km_gbf = gsub(1, 'GBF', km_gbf)) %>% 
  mutate(sdg = gsub(1, 'SDG', sdg)) %>% 
  mutate(cites = gsub(1, 'CITES', cites)) %>% 
  mutate(cms = gsub(1, 'CMS', cms)) %>% 
  mutate(iccwc = gsub(1, 'ICCWC', iccwc)) %>% 
  mutate(ramsar = gsub(1, 'RAMSAR', ramsar)) %>% 
  mutate(unccd = gsub(1, 'UNCCD', unccd)) %>% 
  rowwise() %>% mutate(MEAs = paste(km_gbf,sdg,cites,cms,iccwc,ramsar, unccd, sep = ",")) %>% 
  rowwise() %>% mutate(MEAs = gsub("NA,", '', MEAs)) %>% 
  rowwise() %>% mutate(MEAs = gsub(",NA", '', MEAs)) %>% 
  rowwise() %>% mutate(MEAs = trimws(MEAs, whitespace = ",")) %>% 
  rowwise() %>% mutate(MEAs = gsub(",,", '', MEAs)) %>%   
  rowwise() %>% mutate(MEAs = gsub("NA$", '', MEAs)) %>% 
  # way of extraction
  rowwise() %>% mutate(indic_ext = gsub(1, 'tables', indic_ext)) %>% 
  rowwise() %>% mutate(var_ext = gsub(1, 'text', var_ext)) %>% 
  rowwise() %>% mutate(extraction_type = paste(indic_ext,var_ext, sep = ",")) %>% 
  rowwise() %>% mutate(extraction_type = gsub("NA,", '', extraction_type)) %>% 
  rowwise() %>% mutate(extraction_type = gsub(",NA", '', extraction_type)) %>% 
  rowwise() %>% mutate(extraction_type = gsub("NA$", '', extraction_type)) %>% 
  # clean dataset
  rowwise() %>% mutate(Categories = stringr::str_to_sentence(Categories)) %>% 
  rowwise() %>% mutate(Categories_2 = stringr::str_to_sentence(Categories_2)) %>% 
  rowwise() %>% mutate(Subcategories = stringr::str_to_sentence(Subcategories)) %>% 
  rowwise() %>% mutate(Subcategories_2 = stringr::str_to_sentence(Subcategories_2)) %>% 
  dplyr::select( "Indicators harmonized"="indicators_harmonized","Assessments","MEAs","Frequency of use"="usage",
                 "Main category"="Categories","Main subcategory"="Subcategories",
                "Second. category"="Categories_2","Second. subcategory"="Subcategories_2","Extraction type" = "extraction_type")

## build app----


ui <- fluidPage(titlePanel("Indicators used in assessments and MEAs"),
                mainPanel(width = 12,
                          DT::dataTableOutput("mytable")))

server <- function(input, output) {
  output$mytable <- DT::renderDataTable(dataset_beauty,
                                        options = list(paging = TRUE,    ## paginate the output
                                                       pageLength = 10,  ## number of rows to output for each page
                                                       scrollX = TRUE,   ## enable scrolling on X axis
                                                       scrollY = TRUE,   ## enable scrolling on Y axis
                                                       autoWidth = TRUE, ## use smart column width handling
                                                       server = FALSE,   ## use client-side processing (if TRUE the browser receives only the displayed data)
                                                       # allow user download
                                                       dom = 'Bfrtip', #B:Button, f:filter, r:processing display element, t:table, i:table information summary, p:pagination control
                                                       buttons = c('csv', 'excel'),
                                                       columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                                                         list(targets = c(0, 8, 9), visible = FALSE))
                                        ),
                                        extensions = 'Buttons', # allow user download (either the currently visible data or the entire table, depending on the server option)
                                        selection = 'multiple', ## enable selection of multiple rows ("none", "single")
                                        filter = 'top',              ## include column filters at the bottom
                                        rownames = TRUE                ## don't show row numbers/names
  )
}

# Run the application
shinyApp(ui = ui, server = server)


# Run the app-----
#runApp("C:/Users/JLU-SU/Documents/GitHub/indicators-app/shiny-app")

rm(list=ls())

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# NOTE: run integration2.R first (once, or whenever the source CSVs are
# refreshed) - it builds ../data/indicators_prepped.csv, which this app
# reads directly. app.R itself does no joining/derivation.


library(shiny)
#library(shinycssloaders)
#library(shinybusy)
library(DT) # interface to the JavaScript library DataTables. It allows you to display R dataframes (or matrices) as interactive tables in HTML pages, such as in a Shiny app.

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(stringr)

# Prep data----
# Detailed workflows in indicator repo: https://github.com/yanisica/indicators_public

# Load data----
setwd(file.path("indicators-app","shiny-app"))

# Loads the table prepped by integration2.R (adds verbatim_indicator,
# Frequency of use, and MEA/assess on top of the harmonized/classified
# indicators). See integration2.R for how those columns are built.
data_path <- file.path("..", "data", "indicators_prepped_app.csv")
if (!file.exists(data_path)) {
  data_path <- "indicators_prepped_app.csv"
}
if (!file.exists(data_path)) {
  stop(
    "Could not find 'indicators_prepped_app.csv'. Run integration.R first - ",
    "it builds this file from all_indicators_classified_20052025.csv and ",
    "all_orig_indicators.csv."
  )
}

dataset <- fread(data_path, encoding = "UTF-8", na.strings = c("NA", ""))
names(dataset)
#DT::datatable(dataset)

# small helper: "NA"/"" -> NA, everything else -> Sentence case
clean_cat <- function(x) {
  x <- ifelse(is.na(x), NA_character_, str_to_sentence(x))
  x
}

# manipulate indicators dataset
dataset_beauty <- dataset %>%
  # remove entries that were wrongly extracted / unclassified
  filter(!is.na(Elements)) %>%

  # sources: semicolon-separated framework codes (GBF, SDG, IPBES, IPCC,
  # GEO, RAMSAR, CITES, CMS, UNCCD, ...) -> readable comma list
  mutate(sources = str_replace_all(Sources, ";", ", ")) %>%

  # clean/sentence-case category & subcategory fields
  mutate(
    Elements      = clean_cat(Elements),
    Elements_2    = clean_cat(Elements_2),
    Subelements   = clean_cat(Subelements),
    Subelements_2 = clean_cat(Subelements_2)
  ) 

DT::datatable(dataset_beauty)


# Shiny app----

ui <- fluidPage(titlePanel("Indicators used in assessments and policy frameworks"),
                #titlePanel("Download options"),
                mainPanel(width = 20,
                          h3(paste0("Download options")),
                          DT::dataTableOutput("mytable")))

server <- function(input, output) {
  output$mytable <- DT::renderDataTable(dataset_beauty,
                                        options = list(paging = TRUE,    ## paginate the output
                                                       pageLength = 15,  ## number of rows to output for each page
                                                       scrollX = TRUE,   ## enable scrolling on X axis
                                                       scrollY = TRUE,   ## enable scrolling on Y axis
                                                       autoWidth = TRUE, ## use smart column width handling
                                                       server = FALSE,   ## use client-side processing (if TRUE the browser receives only the displayed data)
                                                       # allow user download
                                                       dom = 'Bfrtip', #B:Button, f:filter, r:processing display element, t:table, i:table information summary, p:pagination control
                                                       buttons = c('csv', 'excel'),
                                                       columnDefs = list(
                                                         # fixed width for 1st col
                                                         list(targets = 1, width = '300px'),
                                                         # row number not visible
                                                         list(targets = 0, visible = FALSE)
                                                         )
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
#shiny::runGitHub(repo="indicators-app", subdir="shiny-app",username="yanisica")

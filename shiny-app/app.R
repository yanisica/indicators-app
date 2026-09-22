rm(list=ls())

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# NOTE: run integration.R (https://github.com/yanisica/indicators) first (once, or whenever the source CSVs are
# refreshed) - it builds ../data/indicators_prepped.csv, which this app
# reads directly. app.R itself does no joining/derivation.


library(shiny)
#library(shinycssloaders)
#library(shinybusy)
library(DT) # interface to the JavaScript library DataTables. It allows you to display R dataframes (or matrices) as interactive tables in HTML pages, such as in a Shiny app.

library(dplyr)
library(tidyr)
library(data.table)  # only used here for fread()
library(ggplot2)
library(stringr)


# Load data----
#setwd(file.path("indicators-app","shiny-app"))

# Loads the app-ready table
data_path <- "indicators_prepped_app.csv"
if (!file.exists(data_path)) {
  data_path <- file.path("..", "data", "indicators_prepped_app.csv")
}
if (!file.exists(data_path)) {
  stop(
    "Could not find 'indicators_prepped_app.csv'. For deployment, it must ",
    "sit directly next to app.R (rsconnect only uploads the app folder ",
    "itself). '../data/' is only checked as a local-development fallback."
  )
}

dataset <- fread(data_path, encoding = "UTF-8", na.strings = c("NA", ""))
names(dataset)

# ---- Palette (same colors used in the paper's figures) --------------------
grid_color <- c(
  # Assessments
  IPBES = "#BAE4B3", IPCC = "#74C476", GEO = "#31A354",
  # MEAs
  GBF = "#DEEBF7", SDG = "#C6DBEF", RAMSAR = "#9ECAE1",
  CITES = "#6BAED6", CMS = "#4292C6", UNCCD = "#2171B5"
)

# ---- Full names behind each acronym (for the source-legend panels below) --
# Order matches grid_color: assessments first, then MEAs.
source_full_name <- c(
  IPBES  = "Intergovernmental Science-Policy Platform on Biodiversity and Ecosystem Services",
  IPCC   = "Intergovernmental Panel on Climate Change",
  GEO    = "Global Environment Outlook",
  GBF    = "Global Biodiversity Framework (Convention on Biological Diversity)",
  SDG    = "Sustainable Development Goals",
  RAMSAR = "Ramsar Convention on Wetlands",
  CITES  = "Convention on International Trade in Endangered Species of Wild Fauna and Flora",
  CMS    = "Convention on the Conservation of Migratory Species of Wild Animals",
  UNCCD  = "United Nations Convention to Combat Desertification"
)

# Which acronyms are Global Assessments vs. Multilateral Environmental
# Agreements (MEAs) - used to split the two legend columns below.
assessment_sources <- c("IPBES", "IPCC", "GEO")
mea_sources         <- c("GBF", "SDG", "RAMSAR", "CITES", "CMS", "UNCCD")

# small helper: "NA"/"" -> NA, everything else -> Sentence case
clean_cat <- function(x) {
  x <- ifelse(is.na(x), NA_character_, str_to_sentence(x))
  x
}

# small helper: "GBF;IPBES" -> colored HTML badges, using grid_color.
# Any source not in grid_color falls back to a neutral grey badge rather
# than erroring, so the app still works if a new framework shows up.
make_source_badges <- function(sources_string) {
  toks <- trimws(unlist(strsplit(sources_string, ";")))
  toks <- toks[toks != ""]
  badges <- vapply(toks, function(tok) {
    col <- unname(grid_color[tok])
    if (is.na(col)) col <- "#E5E5E5"
    sprintf(
      paste0(
        '<span style="display:inline-block;background:%s;color:#333;',
        'padding:2px 8px;border-radius:10px;font-size:11px;',
        'font-weight:600;white-space:nowrap;">%s</span>'
      ),
      col, tok
    )
  }, character(1))
  # joined with a literal space (not just CSS margin) so CSV/Excel export,
  # which reads the rendered cell's plain text, still separates tokens
  paste(badges, collapse = " ")
}

# manipulate indicators dataset ------------------------------------------
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
  ) %>%
  
  # "ILK" (an acronym for Indigenous and Local Knowledge) becomes "Ilk"
  # after clean_cat()'s sentence-casing above; spell it out in full
  # wherever it appears as its own category/subcategory value. (This
  # only catches the exact value "Ilk" - the longer phrase "Specific
  # information on ILK and IPLC" gets fully lowercased by clean_cat()'s
  # str_to_sentence() the same way any multi-word value does, which is a
  # separate, pre-existing quirk not touched here.)
  mutate(
    across(
      c(Elements, Elements_2, Subelements, Subelements_2),
      ~ if_else(.x == "Ilk", "Indigenous and Local Knowledge", .x)
    )
  ) %>%
  
  # colored source badges, built from the original (semicolon-separated)
  # Sources column, before it becomes the comma-joined `sources` above
  mutate(`Source(s)` = vapply(Sources, make_source_badges, character(1))) %>%
  
  dplyr::select(
    "Indicator"             = `Indicators harmonized`,
    "Source(s)",
    "Frequency of use",
    "MEA/assess",
    "Main category"         = Elements,
    "Main subcategory"      = Subelements,
    "Second. category"      = Elements_2,
    "Second. subcategory"   = Subelements_2,
    "Verbatim indicator"    = `Indicators original name`
  )

# Columns DT should HTML-escape - every column except "Source(s)" (which
# holds the colored badge <span> markup and must render as HTML).
#
# NOTE: we deliberately use column NAMES here, not a numeric index. With
# rownames = TRUE (set below), DT shifts numeric column positions by one
# to make room for the row-names column, so a numeric index computed from
# dataset_beauty's own column order (e.g. via which()) ends up pointing at
# the WRONG column once rownames are added - which is exactly what caused
# the raw "<span style=...>" text to show up instead of a colored badge.
# Column names sidestep that offset entirely.
escape_cols <- setdiff(names(dataset_beauty), "Source(s)")

# ---- Raw estimates (computed once, from the full table) -------------------

total_n <- nrow(dataset_beauty)

mea_assess_summary <- dataset_beauty %>% dplyr::count(`MEA/assess`)
get_n <- function(val) {
  n <- mea_assess_summary$n[mea_assess_summary$`MEA/assess` == val]
  if (length(n)) n else 0
}
assess_only_n <- get_n("assessment")
mea_only_n    <- get_n("mea")
both_n        <- get_n("both")

multi_source_n <- sum(dataset_beauty$`Frequency of use` > 1)

# indicator count per framework (an indicator used in 2 frameworks counts
# once toward each), used in the two legend panels below
source_counts <- dataset %>%
  filter(!is.na(Elements)) %>%
  tidyr::separate_rows(Sources, sep = ";") %>%
  mutate(Sources = trimws(Sources)) %>%
  filter(Sources != "") %>%
  dplyr::count(Sources, name = "n_indicators")

# named lookup: "IPBES" -> 797, etc. (0 for a framework with no rows, so
# source_legend_row() never errors on a missing name)
source_n <- setNames(rep(0L, length(grid_color)), names(grid_color))
source_n[source_counts$Sources] <- source_counts$n_indicators

DT::datatable(dataset_beauty)


# Shiny app----

stat_box <- function(value, label) {
  div(
    class = "stat-box",
    div(class = "stat-value", value),
    div(class = "stat-label", label)
  )
}

# one legend row: colored acronym badge + its full name + how many
# indicators use it, reusing make_source_badges() so the badge styling
# always matches the table
source_legend_row <- function(acronym) {
  div(
    class = "legend-row",
    HTML(make_source_badges(acronym)),
    span(source_full_name[[acronym]]),
    span(class = "legend-count", sprintf("%s indicators", source_n[[acronym]]))
  )
}

ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { background: #f5f6f2; }
    .stat-box {
      background: #fff; border: 1px solid #ddd; border-radius: 6px;
      padding: 8px 6px; text-align: center;
    }
    .stat-value { font-size: 18px; font-weight: 700; color: #33513f; }
    .stat-label {
      font-size: 9px; color: #666; margin-top: 2px;
      text-transform: uppercase; letter-spacing: .03em;
    }
    .legend-panel {
      background: #fff; border: 1px solid #ddd; border-radius: 6px;
      padding: 14px 18px; margin-top: 6px; height: 100%;
    }
    .legend-panel h4 {
      margin: 0 0 10px; font-size: 13px; color: #666;
      text-transform: uppercase; letter-spacing: .03em;
    }
    .legend-row {
      display: flex; align-items: baseline; gap: 10px;
      padding: 3px 0; font-size: 13px; color: #333;
    }
    .legend-row span { line-height: 1.3; }
    .legend-count {
      margin-left: auto; padding-left: 12px; white-space: nowrap;
      color: #888; font-size: 12px;
    }
    .footer-note {
      color: #888; font-size: 12px; margin-top: 24px; padding-top: 12px;
      border-top: 1px solid #ddd;
    }
    .explore-heading {
      margin-bottom: 8px;
    }
    /* The CSV/Excel buttons (div.dt-buttons), moved below the table via
       the dom string in server(); ::before adds the label without
       needing a separate Shiny UI element to keep it in sync. */
    .downloadline {
      display: flex; align-items: center; gap: 10px;
      margin: 10px 0 6px;
    }
    .downloadline::before {
      content: 'Download options:';
      font-weight: 600; font-size: 13px; color: #555;
    }
  "))),
  
  titlePanel("Environmental indicators used in assessments and policy frameworks"),
  
  fluidRow(
    column(3, stat_box(total_n, "Total indicators")),
    column(3, stat_box(assess_only_n, "Assessment-only")),
    column(3, stat_box(mea_only_n, "MEA-only")),
    column(3, stat_box(multi_source_n, "Used in >1 source"))
  ),
  
  fluidRow(
    column(
      6,
      div(
        class = "legend-panel",
        h4("Multilateral Environmental Agreements"),
        lapply(mea_sources, source_legend_row)
      )
    ),
    column(
      6,
      div(
        class = "legend-panel",
        h4("Global Assessments"),
        lapply(assessment_sources, source_legend_row)
      )
    )
  ),
  
  br(),
  h3("Explore the indicators and metrics:", class = "explore-heading"),
  fluidRow(
    column(12, DT::dataTableOutput("mytable"))
  ),
  
  p(
    class = "footer-note",
    HTML(sprintf(
      paste0(
        "Dataset accompanying scientific article ",
        "(<a href=\"https://doi.org/10.5281/zenodo.17281329\" target=\"_blank\">",
        "https://doi.org/10.5281/zenodo.17281329</a>). Contact Yanina V. Sica ",
        "for more information (<a href=\"mailto:%s\">%s</a>)."
      ),
      "yanina.sica@gmail.com", "yanina.sica@gmail.com"
    ))
  )
)

server <- function(input, output) {
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(
      dataset_beauty,
      options = list(paging = TRUE,    ## paginate the output
                     pageLength = 15,  ## number of rows to output for each page
                     scrollX = TRUE,   ## enable scrolling on X axis
                     scrollY = TRUE,   ## enable scrolling on Y axis
                     # autoWidth is OFF on purpose: combining it with a
                     # manually fixed pixel width on one column (as the
                     # old columnDefs below used to do, via
                     # list(targets = 1, width = '300px')) is a known
                     # DataTables quirk that can badly miscalculate the
                     # NEXT column's width and collapse it to ~0px - which
                     # is exactly why "Verbatim indicator" was rendering
                     # as if it were blank, even though its cells had
                     # text. Letting DataTables size columns from content
                     # instead avoids that.
                     autoWidth = FALSE,
                     server = FALSE,   ## use client-side processing (if TRUE the browser receives only the displayed data)
                     # allow user download
                     # dom layout, read left to right = top-to-bottom on
                     # the page: f (search box - pulled up next to the
                     # "Explore the indicators and metrics:" heading via
                     # CSS, see .dataTables_filter above) then r
                     # (processing indicator) then t (the table itself),
                     # THEN the buttons B, now wrapped in a custom
                     # "downloadline" div so they render below the table
                     # instead of above it, with a "Download options:"
                     # label (added via CSS ::before, see .downloadline
                     # above) - then i (row-count text) and p (pagination).
                     dom = 'frt<"downloadline"B>ip',
                     buttons = c('csv', 'excel'),
                     columnDefs = list(
                       # row number not visible
                       list(targets = 0, visible = FALSE)
                     )
      ),
      extensions = 'Buttons', # allow user download (either the currently visible data or the entire table, depending on the server option)
      selection = 'multiple', ## enable selection of multiple rows ("none", "single")
      filter = 'top',              ## include column filters at the bottom
      rownames = TRUE,             ## don't show row numbers/names
      escape = escape_cols         ## escape every column by name EXCEPT "Source(s)" (see note above re: rownames offset)
    ) %>%
      DT::formatStyle(
        "MEA/assess",
        backgroundColor = DT::styleEqual(c("assessment", "mea", "both"), c("#E5F5E0", "#EFF3FF", "#FDE4C8"))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Run the app-----
#shiny::runGitHub(repo="indicators-app", subdir="shiny-app",username="yanisica")

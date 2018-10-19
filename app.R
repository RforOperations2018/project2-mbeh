# Project 2
# By Min Yan BEH (mbeh)

library(shiny)
library(shinyjs)
library(shinydashboard)
library(reshape2)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(rgdal)

# UI configuration
pdf(NULL)
sidebarWidth <- 300

# Define header, sidebar and body of shinydashboard
header <- dashboardHeader(title = "HDB Singapore Census", titleWidth = sidebarWidth)
sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    # Sidebar Menu for Charts/Maps & DataTable
    menuItem("Visualizations", icon = icon("bar-chart"), tabName = "charts"),
    menuItem("Database", icon = icon("table"), tabName = "table")
  )
)
body <- dashboardBody(
  useShinyjs(),  # set up shinyjs
  # import custom css stylesheet and favicon
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$link(rel = "shortcut icon", href = "favicon-map.ico")
  ),
  width = 400,
  tabItems(
    # Charts page, to be displayed when "Visualizations" is clicked on sidebar
    tabItem("charts",
            fluidRow(
              column(4, plotlyOutput("most.populated.chart", height = 350)),
              column(8, leafletOutput("map", height = 350))
            ),
            fluidRow(
              column(4, plotlyOutput("land.area.vs.units", height = 275)),
              column(8, plotlyOutput("unit.types.breakdown", height = 275))
            )
    ),
    # DataTable page, to be displayed when "Table" is clicked on sidebar
    tabItem("table",
            fluidPage(
              div(class = "btn-download", downloadButton("downloadRawData","Download Data")),
              box(title = textOutput("dataTableTitle"), 
                  DT::dataTableOutput("dataTable"), width = 12))
    )
  )
)

# Define ui comprising of header, sidebar and body (defined above)
ui <- dashboardPage(title = "Singapore Housing Data",
                    header, sidebar, body, skin = "green")

# Define server logic
server <- function(input, output, session = session) {
}

# Run the application 
shinyApp(ui = ui, server = server)
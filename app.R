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

# Helper functions for formatting numbers and decimals in charts/maps
format.num <- function(number){
  return(prettyNum(number, big.mark=","))
}
format.density <- function(density){
  return(round(density, 2))
}

# Get data from API
source('./pull_api_data.R')
all.towns <- unlist(unique(data.load$town))
all.unit.types <- names(hdb.units.spread)[3:(length(hdb.units.spread)-1)]
all.years <- unlist(unique(data.load$financial_year))

# Define header, sidebar and body of shinydashboard
header <- dashboardHeader(title = "HDB Singapore Census", titleWidth = sidebarWidth)
sidebar <- dashboardSidebar(
  width = sidebarWidth,
  sidebarMenu(
    id = "tabs",
    # Sidebar Menu for Charts/Maps & DataTable
    menuItem("Visualizations", icon = icon("bar-chart"), tabName = "charts"),
    menuItem("Database", icon = icon("table"), tabName = "table"),
    # Selection of Dataset Year
    selectInput("yearSelect", "Year:", 
                width = sidebarWidth - 20,
                choices = sort(all.years, decreasing = TRUE),
                selected = max(all.years)),
    # Multi-Select Input of Towns
    selectInput("townSelect", "Towns:",
                width = sidebarWidth - 20,
                choices = sort(unique(all.towns)),
                multiple = TRUE,
                selectize = TRUE,
                # select top 6 most populated towns from housing data
                selected = unlist(head(data.load %>% arrange(-total_units) %>% distinct(town)))),
    # Button for selecting all towns
    actionButton("selectAllTowns", "Select All Towns", icon = icon("hand-pointer-o")),
    # Radio Selection of Data Attribute to color towns by
    radioButtons("colorByAttribute", "Color Towns By:",
                 choices = list("Number of Apartment Units" = "total_units", 
                                "Density (per hectare)" = "density"),
                 selected = "total_units"),
    # Checkbox Selection of Unit Types filter for breakdown chart
    checkboxGroupInput("unitTypeSelect", "Unit Types for Breakdown Chart:",
                       choices = all.unit.types,
                       selected = c("3-room", "4-room", "5-room"))
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
  
  # Reactively define numeric range of color palette based on colorByAttribute
  get.color.palette.range <- reactive({
    column.to.color.by <- data.load[input$colorByAttribute]
    return(c(min(column.to.color.by), max(column.to.color.by)))
  })
  # Reactively define towns selected (if none selected, display all towns)
  selected.towns <- reactive({
    if(length(input$townSelect) == 0){
      return(all.towns)
    }
    return(input$townSelect)
  })
  # Subset Polygon data (only selected town names) with reactive method
  subset.polygon.data <- reactive({
    polygons.subset <- polygons.load[polygons.load$Name %in% selected.towns(), ]
    return(polygons.subset[order(polygons.subset$Name),]) # order polygons by alphabetical Town name
  })
  # Subset Tabular HDB data by towns with reactive method
  subset.tabular.data <- reactive({
    data.subset <- data.load %>% 
      filter(financial_year == input$yearSelect) %>%     # filter by year selected
      filter(town %in% selected.towns()) %>%             # filter by towns selected
      select(-financial_year)
    # dynamically decide color scheme based on input
    data.subset$coloredColumn <- data.subset$total_units
    if(input$colorByAttribute == "density"){
      data.subset$coloredColumn <- data.subset$density
    }
    return(data.subset)
  })
  # Subset HDB data by selected unit types as well - only for UnitTypes Breakdown Chart & Data Table
  subset.data.plus.unit.types <- reactive({
    selected.unit.types <- input$unitTypeSelect
    if(length(input$unitTypeSelect) == 0){ # display all unit types if none was selected
      selected.unit.types <- all.unit.types
    }
    unit.types.subset <- hdb.units.load %>% 
      filter(financial_year == input$yearSelect) %>%      # filter by year selected
      filter(town %in% selected.towns()) %>%              # filter by towns selected
      filter(flat_type %in% selected.unit.types) %>%      # filter by unit type
      select(town, flat_type, total_units) %>%
      spread(key = flat_type, value = total_units) %>% 
      mutate(total_units = rowSums(.[2:length(.)]))
    land.area.subset <- land.area.raw %>% filter(financial_year == input$yearSelect) %>% # filter by year
      select(-financial_year)
    # remerge with land area data after only filtered by unit types
    merged.data <- merge(unit.types.subset, land.area.subset, by = "town") %>%
      mutate(density = total_units / total_land_area)
    return(merged.data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
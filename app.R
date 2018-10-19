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
  # Render leaflet map based on subset data
  output$map <- renderLeaflet({
    # get data and palette from reactive methods
    reactive.polygon.data <- subset.polygon.data()
    reactive.table.data <- subset.tabular.data()
    palette <- colorBin("YlOrRd", domain = get.color.palette.range(), bins = 5)
    
    # define display attributes of table
    attr.title <- "Number of Apartments"
    attr.suffix <- " units"
    color.values <- unlist(reactive.table.data$coloredColumn)
    if(input$colorByAttribute == "density"){
      attr.title <- "Density"
      attr.suffix <- " units per ha"
    }
    tooltip.labels <- sprintf(
      "<strong>%s</strong><br/>%s apartment units<br/>%g units per hectare",
      reactive.polygon.data$Name, 
      format.num(reactive.table.data$total_units), 
      format.density(reactive.table.data$density)
    ) %>% lapply(htmltools::HTML)
    
    # render and return leaflet map
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga",
               attribution = "Google") %>%
      # Overlay map layer 1: Singapore Railway Network as lines
      addPolylines(data = rail.lines.load, color = 'black', opacity = 1, weight = 2, group = "Rail Network") %>%
      # Overlay map layer 2: Singapore Towns as polygons
      addPolygons(data = reactive.polygon.data, group = "Towns",
                  fillColor = ~palette(color.values),
                  color = "white",
                  dashArray = 3,
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 4,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                  label = tooltip.labels, labelOptions = labelOptions(direction = "top"), 
                  weight = 2, opacity = 1) %>%
      addLegend(pal = palette, title = attr.title, position = "topright", values = color.values,
                labFormat = labelFormat(suffix = attr.suffix),
                group = "Towns") %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Towns", "Rail Network"),
        position = "bottomright",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Render scatter plot of land area vs number of units
  output$land.area.vs.units <- renderPlotly({
    scatter.data <- subset.tabular.data()
    scatter.plot <- ggplot(scatter.data, aes(x = total_land_area, y = total_units, color = coloredColumn,
                                             text = paste0('<b> ', town, '</b>',
                                                           '<br><b> Area: </b>', total_land_area, ' hectares',
                                                           '<br><b> Total Units: </b>', format.num(total_units),
                                                           '<br><b>', format.density(density), ' apartments/Ha</b>'))) + 
      geom_point(size = 3) +
      xlab("Land Area in Hectares") + ylab("Total Apartment Units") +
      scale_colour_distiller(palette = "YlOrRd", limits = get.color.palette.range(), direction = 1) +
      theme(legend.position = "none")
    ggplotly(scatter.plot, tooltip = "text")
  })
  
  # Render Data Table of HDB data (based on reactive selection)
  output$dataTable <- renderDataTable({
    raw.data <- subset.data.plus.unit.types() %>% select(town, total_land_area, everything(), -density) %>%
      rename(Town = town, LandArea = total_land_area, Total = total_units)
    datatable(raw.data, rownames = FALSE, options = list(pageLength = 25, lengthMenu = c(10, 15, 25))) %>%
      formatStyle('Town', fontWeight = 'bold') %>% 
      formatCurrency(columns=c('LandArea'), digits = 0, currency = " Hectares", before = FALSE) %>% 
      formatCurrency(columns=c('Total'), digits = 0, currency = "")
  })
  # Reactively parse Data Table title (based on year selection)
  output$dataTableTitle <- renderText({ 
    paste0("Raw Data Selection for Year ", input$yearSelect)
  })
  # Download filtered HDB data from datatable
  output$downloadRawData <- downloadHandler(
    filename = function(){
      paste0('singapore-', input$yearSelect, '-housing-data-', Sys.Date(), '.csv')
    },
    content = function(file) {
      dataForDownload <- subset.data.plus.unit.types() %>% select(town, total_land_area, everything(), -density)
      write.csv(dataForDownload, file, row.names=FALSE)
    }
  )
  # Observe 'townSelect' input for addition of towns to control hide/show 'Select All' button
  observeEvent(input$townSelect, {
    # Show 'Select All Towns' button if some towns have not been selected
    if (length(input$townSelect) < length(all.towns)){
      shinyjs::show(id = "selectAllTowns", anim = TRUE, animType = "fade", time = 0.3)
    } else {
      hide(id = "selectAllTowns", anim = TRUE, animType = "fade", time = 0.2)
    }
  })
  # Observe clicks on 'Select All Towns' button to fill input with all towns
  observeEvent(input$selectAllTowns, {
    updateSelectInput(session, "townSelect", selected = all.towns)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
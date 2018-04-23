library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)

data <- read.csv("data/data.csv")
map <- readOGR("data/fe_2007_39_county/fe_2007_39_county.shp")

# ui object
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variableselected", label = "Select variable",
                  choices = c("cases", "population")),
      selectInput(inputId = "yearselected", label = "Select year",
                  choices = 1968:1988),
      
      p("Made with", a("Shiny", href = "http://shiny.rstudio.com"), "."),
      img(src = "imageShiny.png", width = "70px", height = "70px")
    ),
    
    mainPanel(
      leafletOutput(outputId = "map"),
      dygraphOutput(outputId = "timetrend"),
      DTOutput(outputId = "table")
    )
  )
)

# server()
server <- function(input, output){
  output$table <- renderDT(data)
  
  output$timetrend <- renderDygraph({
    
    dataxts <- NULL
    counties <- unique(data$county)
    for(l in 1:length(counties)){
      datacounty <- data[data$county == counties[l],]
      dd <- xts(datacounty[, input$variableselected], as.Date(paste0(datacounty$year,"-01-01")))
      dataxts <- cbind(dataxts, dd)
    }
    colnames(dataxts) <- counties
    
    dygraph(dataxts) %>% dyHighlight(highlightSeriesBackgroundAlpha = 0.2)-> d1
    
    d1$x$css = "
 .dygraph-legend > span {display:none;}
 .dygraph-legend > span.highlight { display: inline; }
 "
    d1
  })
  
  output$map <- renderLeaflet({
    
    # Add data to map
    datafiltered <- data[which(data$year == input$yearselected), ] # CHANGE 1980 by input$yearselected
    ordercounties <- match(map@data$NAME, datafiltered$county)
    map@data <- datafiltered[ordercounties, ]
    
    # Create variableplot
    map$variableplot <- as.numeric(map@data[, input$variableselected]) # ADD this to create variableplot
    
    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7) # CHANGE map$cases by map$variableplot
    
    labels <- sprintf("%s: %g", map$county, map$variableplot) %>% lapply(htmltools::HTML) # CHANGE map$cases by map$variableplot
    
    l <- leaflet(map) %>% addTiles() %>% addPolygons(
      fillColor = ~pal(variableplot), # CHANGE cases by variableplot
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      label = labels) %>%
      leaflet::addLegend(pal = pal, values = ~variableplot, opacity = 0.7, title = NULL) # CHANGE cases by variableplot
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)





















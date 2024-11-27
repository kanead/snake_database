# https://r-charts.com/spatial/interactive-maps-leaflet/
# https://www.appsilon.com/post/leaflet-vs-tmap-build-interactive-maps-with-r-shiny

rm(list=ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(rsconnect)

# For Testing
# myFun <- function(n = 5000) {
#   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
#   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
# }
# # Sample data
# data <- data.frame(
#   lat = rnorm(100,0,10),
#   lng = rnorm(100,0,10),
#   group = myFun(100)
# )

data <-
  read_csv("Snake database_cleaned_Sept 2024.csv")
names(data)

data <- data |> 
  unite(group, c(Genus, Species), sep = " ", remove = FALSE)

data <- rename(data, lat = Latitude, lng = Longitude)

data <- data |>  drop_na(lat, lng)

head(data)

ui <- fluidPage(
  titlePanel("Snake Species Locations"),
  sidebarLayout(sidebarPanel(
    selectInput(
      "group",
      "Select Species",
      choices = unique(data$group),
      selected = sample(data$group, 1),
      multiple = TRUE
    )
    # when displaying the dropdown box set selected = unique(data$group) to show everything
    # set selected = NULL to show nothing
    #
  ),
  mainPanel(leafletOutput("map")))
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data|> filter(group %in% input$group)
  })
  
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      addCircleMarkers(
        data = filtered_data(),
        lat = ~ lat,
        lng = ~ lng,
        color = ~ group
      )
  })
  
  observe({
    leafletProxy("map", data = filtered_data()) |> 
      clearMarkers() |> 
      addCircleMarkers(
        lat = ~ lat,
        lng = ~ lng,
        fillOpacity = 0.7,
        #color = ~ pal(Family),
        color = "red",
        radius = 3,
        stroke = FALSE
      )
  })
}

shinyApp(ui = ui, server = server)

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(leaflet)
library(stringr)

# Read in compressed data
energy <- read.csv(file = "data/formatted.csv", sep = ",", header=TRUE)
# Give proper names to the columns
names(energy) <- c("state", "plant_name", "plant_latitude", "plant_longitude", "coal", "oil", "gas", "nuclear", "hydro", "biomass", "wind", "solar", "geothermal", "other")

# Calculate other columns
energy$total <- energy$coal+energy$oil+energy$gas+energy$nuclear+energy$hydro+energy$biomass+energy$wind+energy$solar+energy$geothermal+energy$other
energy$renewable <- energy$hydro+energy$biomass+energy$wind+energy$solar+energy$geothermal
energy$non_renewable <- energy$coal+energy$oil+energy$gas+energy$nuclear+energy$other

# Calculate percent of total columns
energy$percent_coal <- ifelse(energy$total == 0, 0.0, (energy$coal/energy$total) * 100 )
energy$percent_oil <- ifelse(energy$total == 0, 0.0, (energy$oil / energy$total) * 100)
energy$percent_gas <- ifelse(energy$total == 0, 0.0, (energy$gas / energy$total) * 100)
energy$percent_nuclear <- ifelse(energy$total == 0, 0.0, (energy$nuclear / energy$total) * 100)
energy$percent_hydro <- ifelse(energy$total == 0, 0.0, (energy$hydro / energy$total) * 100)
energy$percent_biomass <- ifelse(energy$total == 0, 0.0, (energy$biomass / energy$total) * 100)
energy$percent_wind <- ifelse(energy$total == 0, 0.0, (energy$wind / energy$total) * 100)
energy$percent_solar <- ifelse(energy$total == 0, 0.0, (energy$solar / energy$total) * 100)
energy$percent_geothermal <- ifelse(energy$total == 0, 0.0, (energy$geothermal / energy$total) * 100)
energy$percent_other <- ifelse(energy$total == 0, 0.0, (energy$other / energy$total) * 100)

energy$percent_renewable <- ifelse(energy$total == 0, 0.0, (energy$renewable / energy$total) * 100)
energy$percent_non_renewable <- ifelse(energy$total == 0, 0.0, (energy$non_renewable / energy$total) * 100)

il_plants <- subset(energy, energy$state == "IL")

# getColor <- function(plants) {
#   sapply(plants, function(mag) {
#     if (plants$coal > 0.0) {
#         "#E41A1C"
#     }
#     else if (plants$oil > 0.0) {
#         "#377EB8"
#     }
#     else if (plants$gas > 0.0) {
#         "#4DAF4A"
#     }
#     else if (plants$nuclear > 0.0) {
#         "#984EA3"
#     }
#     else if (plants$hydro > 0.0) {
#         "#FF7F00"
#     }
#     else if (plants$biomass > 0.0) {
#         "#111111"
#     }
#     else if (plants$wind > 0.0) {
#         "#A65628"
#     }
#     else if (plants$solar > 0.0) {
#         "#F781BF"
#     }
#     else if (plants$geothermal > 0.0) {
#         "#999999"
#     }
#     else if (plants$other > 0.0) {
#         "#FF2233"
#     }
#   })
# }

# pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
pal <- colorFactor(c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#111111","#A65628","#F781BF","#999999","#FF2233"), domain=c("coal","oil","gas","nuclear","hydro","biomass","wind","solar","geothermal","other"))


ui <- fluidPage(
    title = "CS 424: Project 2",
    lang="en",
    p(),
    navbarPage("Project 2", position = c("static-top"), collapsible = FALSE, fluid = TRUE,
        tabPanel("Map",
            leafletOutput("testMap", height="calc(100vh - 78px)"),
        )
    )
)

server <- function(input, output, session) {
  points <- eventReactive(input$recalc, { cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)}, ignoreNULL = FALSE)

  output$testMap <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
          options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addCircleMarkers(
          lng=il_plants$plant_longitude,
          lat=il_plants$plant_latitude,
          color = ~pal(type),
          stroke = FALSE
        )
      # addMarkers(lng=il_plants$plant_longitude, lat=il_plants$plant_latitude, popup=il_plants$plant_name)
  })
}

shinyApp(ui, server)

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

# NEED TO FIX -- Energy source may have a max of 0
energy$dominant_source <- colnames(energy)[5:14][apply((energy)[5:14],1,which.max)]

il_plants <- subset(energy, energy$state == "IL")

getColor <- function(plants) {
  res <- sapply(plants$dominant_source, function(dominant) {
    if (dominant == "coal") {
        "red"
    }
    else if (dominant == "oil") {
        "black"
    }
    else if (dominant == "gas") {
        "green"
    }
    else if (dominant == "nuclear") {
        "purple"
    }
    else if (dominant == "hydro") {
        "blue"
    }
    else if (dominant == "biomass") {
        "orange"
    }
    else if (dominant == "wind") {
        "white"
    }
    else if (dominant == "solar") {
        "yellow"
    }
    else if (dominant == "geothermal") {
        "magenta"
    }
    else if (dominant == "other") {
        "magenta"
    }
    else {
      "magenta"
    }
  })

  names(res) <- NULL
  res
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(il_plants)
)

# pal <- colorFactor(c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#111111","#A65628","#F781BF","#999999","#FF2233"), domain=c("coal","oil","gas","nuclear","hydro","biomass","wind","solar","geothermal","other"))

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
        addProviderTiles(providers$OpenStreetMap.Mapnik,
          options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addAwesomeMarkers(lng=il_plants$plant_longitude, lat=il_plants$plant_latitude, icon=icons, popup=paste(il_plants$plant_name, "<br/>", il_plants$dominant_source))
  })
}

shinyApp(ui, server)

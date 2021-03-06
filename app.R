# Jonathon Repta
# CS 424 Project 2

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
energy$percent_coal <- ifelse(energy$total == 0, 0.0, (energy$coal / energy$total) * 100 )
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

# NEED TO FIX -- Doesn't handle multiple energy sources
energy$dominant_source <- colnames(energy)[5:14][apply((energy)[5:14],1,which.max)]

# Filter for Illinois energy plants
il_plants <- subset(energy, energy$state == "IL")

# Filter plants that don't generate any energy
il_plants <- rbind( subset(il_plants, percent_renewable > 0), subset(il_plants, percent_non_renewable > 0) )

getColor <- function(plants) {
  res <- sapply(plants$dominant_source, function(dominant) {
    if (dominant == "coal") { "red" }
    else if (dominant == "oil") { "black" }
    else if (dominant == "gas") { "green" }
    else if (dominant == "nuclear") { "purple" }
    else if (dominant == "hydro") { "blue" }
    else if (dominant == "biomass") { "orange" }
    else if (dominant == "wind") { "white" }
    else if (dominant == "solar") { "beige" }
    else if (dominant == "geothermal") { "darkpurple" }
    else if (dominant == "other") { "lightgray" }
    else { "cadetblue" } # shouldn't reach this case
  })

  names(res) <- NULL
  res
}

pal <- colorFactor(c("orange","red","green","#800080","blue","purple","black","lightgray","beige","white"),
            domain=c("coal","oil","gas","nuclear","hydro","biomass","wind","solar","geothermal","other"))

ui <- fluidPage(
    title = "CS 424: Project 2",
    lang="en",
    p(),
    navbarPage("Project 2", position = c("static-top"), collapsible = FALSE, fluid = TRUE,
      tabPanel("Map",
        sidebarLayout(
          sidebarPanel(width=2,
            checkboxInput("checkbox_all", "All", TRUE),
            checkboxInput("checkbox_coal", "Coal", FALSE),
            checkboxInput("checkbox_oil", "Oil", FALSE),
            checkboxInput("checkbox_gas", "Gas", FALSE),
            checkboxInput("checkbox_nuclear", "Nuclear", FALSE),
            checkboxInput("checkbox_hydro", "Hydro", FALSE),
            checkboxInput("checkbox_biomass", "Biomass", FALSE),
            checkboxInput("checkbox_wind", "Wind", FALSE),
            checkboxInput("checkbox_solar", "Solar", FALSE),
            checkboxInput("checkbox_geothermal", "Geothermal", FALSE),
            checkboxInput("checkbox_other", "Other", FALSE),
            checkboxInput("checkbox_non_renewable", "Non renewable", FALSE),
            checkboxInput("checkbox_renewable", "Renewable", FALSE),
            actionButton("resetButton", "Reset Map")
          ),
          mainPanel(width=10, leafletOutput("testMap", height="calc(100vh - 95px)") )
        )
      ),
      tabPanel("About",
        verbatimTextOutput("name"),
        verbatimTextOutput("date"),
        verbatimTextOutput("dataset")
      )
    )
)

server <- function(input, output, session) {

  # Reset button
  observeEvent(input$resetButton, {
    updateCheckboxInput(
      inputId = "checkbox_all",
      session = session,
      value = TRUE
    )
    updateCheckboxInput(
      inputId = "checkbox_coal",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_oil",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_gas",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_nuclear",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_hydro",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_biomass",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_wind",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_solar",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_geothermal",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_other",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_non_renewable",
      session = session,
      value = FALSE
    )
    updateCheckboxInput(
      inputId = "checkbox_renewable",
      session = session,
      value = FALSE
    )
  })

  # Handle map marker colors
  reactiveIcons <- reactive({
    awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(activeEnergySources())
    )
  })

  # Create label for markers
  determineLabel <- reactive({
    active <- activeEnergySources()
    paste("<b>", active$plant_name, "</b>", "<br/>", active$dominant)
  })

  # Handle filtering of energy types
  activeEnergySources <- reactive({
    toReturn <- NULL

    if (input$checkbox_all)
    {
      toReturn <- il_plants
    }
    else
    {
      if (input$checkbox_coal) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "coal"))
      }
      if (input$checkbox_oil) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "oil"))
      }
      if (input$checkbox_gas) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "gas"))
      }
      if (input$checkbox_nuclear) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "nuclear"))
      }
      if (input$checkbox_hydro) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "hydro"))
      }
      if (input$checkbox_biomass) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "biomass"))
      }
      if (input$checkbox_wind) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "wind"))
      }
      if (input$checkbox_solar) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "solar"))
      }
      if (input$checkbox_geothermal) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "geothermal"))
      }
      if (input$checkbox_other) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "other"))
      }
      if (input$checkbox_non_renewable) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "coal"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "oil"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "gas"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "nuclear"))
      }
      if (input$checkbox_renewable) {
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "hydro"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "biomass"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "wind"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "solar"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "geothermal"))
        toReturn <- rbind(toReturn, subset(il_plants, il_plants$dominant_source == "other"))
      }
    }

    toReturn
  })

  # Create map
  output$testMap <- renderLeaflet({
    leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik,
          options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addAwesomeMarkers(data=activeEnergySources(),
          lng=~plant_longitude,
          lat=~plant_latitude,
          icon=reactiveIcons(),
          popup=determineLabel()
        ) %>%
        addLegend("bottomright",
            pal = pal,
            values = names(il_plants)[5:14],
            title = "Energy Source",
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1
        )
  })

  # About page
  output$name <- renderPrint({
    "Created by: Jonathon Repta"
  })
  output$date <- renderPrint({
    "Created on: March 14, 2021"
  })
  output$dataset <- renderPrint({
    "Data from: https://www.epa.gov/egrid/download-data, eGRID2018v2 Data File (XLSX)"
  })
}

shinyApp(ui, server)

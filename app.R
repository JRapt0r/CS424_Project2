# Jonathon Repta
# CS 424 Project 2

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(leaflet)
library(stringr)
library(leaflet.providers)

# Read in compressed data
energy_2000 <- read.csv(file = "data/egrid_2000_formatted.csv", sep = ",", header=TRUE)
energy_2010 <- read.csv(file = "data/egrid_2010_formatted.csv", sep = ",", header=TRUE)
energy_2018 <- read.csv(file = "data/egrid_2018_formatted.csv", sep = ",", header=TRUE)

computeRows <- function(energy) {
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


  # Determine size groupings
  sml <- unname(quantile(subset(energy, energy$total > 0)$total))
  small <- sml[2:2]
  med <- sml[3:3]
  large <- sml[4:4]

  # Create marker sizes
  energy$marker_size <- sapply(energy$total, function(total) {
      if      (total >= large) { 20 }
      else if (total >= med)   { 15 }
      else if (total >= small) { 10 }
      else if (total >  0)     { 5 }
      else { 1 }
  })

  # Filter plants that don't generate any energy
  energy <- rbind( subset(energy, percent_renewable > 0), subset(energy, percent_non_renewable > 0) )

  # NEED TO FIX -- Doesn't handle multiple energy sources
  energy$dominant_source <- colnames(energy)[5:14][apply((energy)[5:14],1,which.max)]

  energy
}

energy_2000 <- computeRows(energy_2000)
energy_2010 <- computeRows(energy_2010)
energy_2018 <- computeRows(energy_2018)

# Filter for Illinois energy plants
il_plants <- subset(energy_2018, energy_2018$state == "IL")

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

pal <- colorFactor(c("#00AB66","#36454F","#5E548D","#DBA480","#8CACD6","#A52A2A","#3D0C02","#C88CA4","#F5D68F","#D7DFD8"),
            domain=c("coal","oil","gas","nuclear","hydro","biomass","wind","solar","geothermal","other"))

ui <- fluidPage(
    title = "CS 424: Project 2",
    lang="en",
    p(),
    navbarPage("Project 2", position = c("static-top"), collapsible = FALSE, fluid = TRUE,
      tabPanel("Compare States",
            fluidPage(
              column(6,
                fluidRow(column(6,
                  selectInput(inputId="stateSelect1", width="100%", label="State", choices=
                  c("Alabama" = "AL",
                  "Alaska" = "AK",
                  "Arizona" = "AZ",
                  "Arkansas" = "AR",
                  "California" = "CA",
                  "Colorado" = "CO",
                  "Connecticut" = "CT",
                  "Delaware" = "DE",
                  "Florida" = "FL",
                  "Georgia" = "GA",
                  "Hawaii" = "HI",
                  "Idaho" = "ID",
                  "Illinois" = "IL",
                  "Indiana" = "IN",
                  "Iowa" = "IA",
                  "Kansas" = "KS",
                  "Kentucky" = "KY",
                  "Louisiana" = "LA",
                  "Maine" = "ME",
                  "Maryland" = "MD",
                  "Massachusetts" = "MA",
                  "Michigan" = "MI",
                  "Minnesota" = "MN",
                  "Mississippi" = "MS",
                  "Missouri" = "MO",
                  "Montana" = "MT",
                  "Nebraska" = "NE",
                  "Nevada" = "NV",
                  "New Hampshire" = "NH",
                  "New Jersey" = "NJ",
                  "New Mexico" = "NM",
                  "New York" = "NY",
                  "North Carolina" = "NC",
                  "North Dakota" = "ND",
                  "Ohio" = "OH",
                  "Oklahoma" = "OK",
                  "Oregon" = "OR",
                  "Pennsylvania" = "PA",
                  "Rhode Island" = "RI",
                  "South Carolina" = "SC",
                  "South Dakota" = "SD",
                  "Tennessee" = "TN",
                  "Texas" = "TX",
                  "Utah" = "UT",
                  "Vermont" = "VT",
                  "Virginia" = "VA",
                  "Washington" = "WA",
                  "West Virginia" = "WV",
                  "Wisconsin" = "WI",
                  "Wyoming" = "WY",
                  "Washington DC" = "DC"),
                  selected="IL")),
                  column(6,selectInput("yearSelect", "Year", c("2000" = 2000, "2010" = 2010, "2018" = 2018), selected=2000, width="100%")),
                ),
                leafletOutput("testMap", height="calc(100vh - 245px)"),
                absolutePanel(top = 90, left=25, actionButton("resetButton1", "Reset Map")),
                checkboxGroupInput(inputId="checkboxGroup1",label="Energy Sources",inline=TRUE, width="100%", selected="all", choices =
                  c("All" = "all",
                    "Coal" = "coal",
                    "Oil" = "oil",
                    "Gas" = "gas",
                    "Nuclear" = "nuclear",
                    "Hydro" = "hydro",
                    "Biomass" = "biomass",
                    "Wind" = "wind",
                    "Solar" = "solar",
                    "Geothermal" = "geothermal",
                    "Other" = "other",
                    "Non renewable" = "non_renewable",
                    "Renewable" = "renewable")
                )
              ),
              column(6,
                fluidRow(column(6,
                  selectInput(inputId="stateSelect2", width="100%", label="State", choices=
                  c("Alabama" = "AL",
                  "Alaska" = "AK",
                  "Arizona" = "AZ",
                  "Arkansas" = "AR",
                  "California" = "CA",
                  "Colorado" = "CO",
                  "Connecticut" = "CT",
                  "Delaware" = "DE",
                  "Florida" = "FL",
                  "Georgia" = "GA",
                  "Hawaii" = "HI",
                  "Idaho" = "ID",
                  "Illinois" = "IL",
                  "Indiana" = "IN",
                  "Iowa" = "IA",
                  "Kansas" = "KS",
                  "Kentucky" = "KY",
                  "Louisiana" = "LA",
                  "Maine" = "ME",
                  "Maryland" = "MD",
                  "Massachusetts" = "MA",
                  "Michigan" = "MI",
                  "Minnesota" = "MN",
                  "Mississippi" = "MS",
                  "Missouri" = "MO",
                  "Montana" = "MT",
                  "Nebraska" = "NE",
                  "Nevada" = "NV",
                  "New Hampshire" = "NH",
                  "New Jersey" = "NJ",
                  "New Mexico" = "NM",
                  "New York" = "NY",
                  "North Carolina" = "NC",
                  "North Dakota" = "ND",
                  "Ohio" = "OH",
                  "Oklahoma" = "OK",
                  "Oregon" = "OR",
                  "Pennsylvania" = "PA",
                  "Rhode Island" = "RI",
                  "South Carolina" = "SC",
                  "South Dakota" = "SD",
                  "Tennessee" = "TN",
                  "Texas" = "TX",
                  "Utah" = "UT",
                  "Vermont" = "VT",
                  "Virginia" = "VA",
                  "Washington" = "WA",
                  "West Virginia" = "WV",
                  "Wisconsin" = "WI",
                  "Wyoming" = "WY",
                  "Washington DC" = "DC"),
                  selected="IL")),
                  column(6,selectInput("yearSelect2", "Year", c("2000" = 2000, "2010" = 2010, "2018" = 2018), selected=2018, width="100%"))
                ),
                leafletOutput("testMap2", height="calc(100vh - 245px)"),
                absolutePanel(top = 90, left=25, actionButton("resetButton2", "Reset Map")),
                checkboxGroupInput(inputId="checkboxGroup2",label="Energy Sources",inline=TRUE, width="100%", selected="all", choices =
                  c("All" = "all",
                    "Coal" = "coal",
                    "Oil" = "oil",
                    "Gas" = "gas",
                    "Nuclear" = "nuclear",
                    "Hydro" = "hydro",
                    "Biomass" = "biomass",
                    "Wind" = "wind",
                    "Solar" = "solar",
                    "Geothermal" = "geothermal",
                    "Other" = "other",
                    "Non renewable" = "non_renewable",
                    "Renewable" = "renewable")
                )
              )
            )
        ),
      tabPanel("Country Overview",
        fluidPage(
          sidebarLayout(
            sidebarPanel(width=2,
              checkboxGroupInput(inputId="checkboxGroup3",label=h4("Select Energy Source"),inline=FALSE, choices =
                c("All" = "all",
                  "Coal" = "coal",
                  "Oil" = "oil",
                  "Gas" = "gas",
                  "Nuclear" = "nuclear",
                  "Hydro" = "hydro",
                  "Biomass" = "biomass",
                  "Wind" = "wind",
                  "Solar" = "solar",
                  "Geothermal" = "geothermal",
                  "Other" = "other",
                  "Non renewable" = "non_renewable",
                  "Renewable" = "renewable")
              ),
              sliderInput("energySlider", h4("Generation Range (MWh):"), min = 0, max = max(energy_2018$total), value = c(0, max(energy_2018$total))),
              selectInput("yearSelect3", h4("Year"), c("2000" = 2000, "2010" = 2010, "2018" = 2018), selected=2018, width="100%"),
              actionButton("resetButton3", "Reset Map")
            ),
            mainPanel(leafletOutput("testMap3", height="calc(100vh - 90px)"), width=10)
          )
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
  observeEvent(input$resetButton1, {
    # Update checkboxe
    updateCheckboxGroupInput(session, "checkboxGroup1", selected = "all")

    # Update year select
    updateSelectInput(session, "yearSelect",
      selected = 2000
    )

    # Update state select
    updateSelectInput(session, "stateSelect1",
      selected = "IL"
    )
  })

  observeEvent(input$resetButton2, {
    # Update checkboxe
    updateCheckboxGroupInput(session, "checkboxGroup2", selected = "all")

    # Update year select
    updateSelectInput(session, "yearSelect2",
      selected = 2018
    )

    # Update state select
    updateSelectInput(session, "stateSelect2",
      selected = "IL"
    )
  })

  observeEvent(input$resetButton3, {
    # Update checkboxe
    updateCheckboxGroupInput(session, "checkboxGroup3", selected = "")

    updateSliderInput(session, "energySlider",
        min = 0,
        max = max(energy_2018$total),
        value = c(0, max(energy_2018$total))
      )

    # Update year select
    updateSelectInput(session, "yearSelect3",
      selected = 2018
    )
  })

  # Create label for markers
  determineLabel <- reactive({
    active <- activeEnergySources()
    paste("<b>", active$plant_name, "</b>",
          "<br/>", active$dominant,"<br/>",
          "Total capacity:" , formatC(active$total, format="f", big.mark=",", digits=0), "<br/>",
          "Percent renewable:" , format(active$percent_renewable, digits=2),"<br/>",
          "Percent non-renewable:", format(active$percent_non_renewable, digits=2) )
  })

  determineLabel2 <- reactive({
    active <- activeEnergySources2()
    paste("<b>", active$plant_name, "</b>",
          "<br/>", active$dominant,"<br/>",
          "Total capacity:" , formatC(active$total, format="f", big.mark=",", digits=0), "<br/>",
          "Percent renewable:" , format(active$percent_renewable, digits=2),"<br/>",
          "Percent non-renewable:", format(active$percent_non_renewable, digits=2) )
  })

  determineLabel3 <- reactive({
    active <- activeEnergySources3()
    paste("<b>", active$plant_name, "</b>",
          "<br/>", active$dominant,"<br/>",
          "Total capacity:" , formatC(active$total, format="f", big.mark=",", digits=0), "<br/>",
          "Percent renewable:" , format(active$percent_renewable, digits=2),"<br/>",
          "Percent non-renewable:", format(active$percent_non_renewable, digits=2) )
  })

  # Handle filtering of energy types
  activeEnergySources <- reactive({
    if (input$yearSelect == 2000) {
      selectedState <- subset(energy_2000, energy_2000$state == input$stateSelect1)
    }
    if (input$yearSelect == 2010) {
      selectedState <- subset(energy_2010, energy_2010$state == input$stateSelect1)
    }
    if (input$yearSelect == 2018) {
      selectedState <- subset(energy_2018, energy_2018$state == input$stateSelect1)
    }

    toReturn <- NULL

    if ('all' %in% input$checkboxGroup1) {
      toReturn <- selectedState
    }
    else {
      if ('non_renewable' %in% input$checkboxGroup1) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("coal","oil","gas","nuclear"), ])
      }
      if ('renewable' %in% input$checkboxGroup1) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("hydro","biomass","wind","solar","geothermal","other"), ])
      }

      toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% input$checkboxGroup1, ])
    }

    toReturn
  })

  activeEnergySources2 <- reactive({
    if (input$yearSelect2 == 2000) {
      selectedState <- subset(energy_2000, energy_2000$state == input$stateSelect2)
    }
    if (input$yearSelect2 == 2010) {
      selectedState <- subset(energy_2010, energy_2010$state == input$stateSelect2)
    }
    if (input$yearSelect2 == 2018) {
      selectedState <- subset(energy_2018, energy_2018$state == input$stateSelect2)
    }

    toReturn <- NULL

    if ('all' %in% input$checkboxGroup2) {
      toReturn <- selectedState
    }
    else {
      if ('non_renewable' %in% input$checkboxGroup2) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("coal","oil","gas","nuclear"), ])
      }
      if ('renewable' %in% input$checkboxGroup2) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("hydro","biomass","wind","solar","geothermal","other"), ])
      }

      toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% input$checkboxGroup2, ])
    }

    toReturn
  })

  activeEnergySources3 <- reactive({
    if (input$yearSelect3 == 2000) {
      selectedState <- energy_2000

      updateSliderInput(session, "energySlider",
        min = 0,
        max = max(energy_2000$total)
      )
    }
    if (input$yearSelect3 == 2010) {
      selectedState <- energy_2010

      updateSliderInput(session, "energySlider",
        min = 0,
        max = max(energy_2010$total)
      )
    }
    if (input$yearSelect3 == 2018) {
      selectedState <- energy_2018

      updateSliderInput(session, "energySlider",
        min = 0,
        max = max(energy_2018$total)
      )
    }

    toReturn <- NULL

    if ('all' %in% input$checkboxGroup3) {
      toReturn <- selectedState
    }
    else {
      if ('non_renewable' %in% input$checkboxGroup3) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("coal","oil","gas","nuclear"), ])
      }
      if ('renewable' %in% input$checkboxGroup3) {
        toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% c("hydro","biomass","wind","solar","geothermal","other"), ])
      }

      toReturn <- rbind(toReturn, selectedState[selectedState$dominant_source %in% input$checkboxGroup3, ])
    }

    # toReturn
    subset(toReturn, total >= input$energySlider[1] & total <= input$energySlider[2])
  })

  # Create maps
  output$testMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri WorldGrayCanvas") %>%
      addCircleMarkers(
        data=activeEnergySources(),
        radius=~marker_size,
        lng=~plant_longitude,
        lat=~plant_latitude,
        color = ~pal(dominant_source),
        popup=determineLabel(),
        stroke = TRUE, fillOpacity = 0.75
      ) %>%
      addLegend("bottomright",
          pal = pal,
          values = names(il_plants)[5:14],
          title = "Energy Source",
          opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("OSM", "Esri WorldTopoMap", "Esri WorldGrayCanvas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$testMap2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri WorldGrayCanvas") %>%
      addCircleMarkers(
        data=activeEnergySources2(),
        radius=~marker_size,
        lng=~plant_longitude,
        lat=~plant_latitude,
        color = ~pal(dominant_source),
        popup=determineLabel2(),
        stroke = TRUE, fillOpacity = 0.75
      ) %>%
      addLegend("bottomright",
          pal = pal,
          values = names(il_plants)[5:14],
          title = "Energy Source",
          opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("OSM", "Esri WorldTopoMap", "Esri WorldGrayCanvas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$testMap3 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri WorldGrayCanvas") %>%
      addCircleMarkers(
        data=activeEnergySources3(),
        radius=~marker_size,
        lng=~plant_longitude,
        lat=~plant_latitude,
        color = ~pal(dominant_source),
        popup=determineLabel3(),
        stroke = TRUE, fillOpacity = 0.75
      ) %>%
      addLegend("bottomright",
          pal = pal,
          values = names(il_plants)[5:14],
          title = "Energy Source",
          opacity = 1
      ) %>%
      addLayersControl(
        baseGroups = c("OSM", "Esri WorldTopoMap", "Esri WorldGrayCanvas"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$rangeOutput <- renderPrint({rangeLabel()})

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

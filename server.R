#beta

library(dplyr)
library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(cartography)

lugares <- readRDS(file = "lugares.rds")
x <- readRDS(file = "rutas.rds")
y <- readRDS(file = "y.rds")

function(input, output, session) {
  
  filtro <- reactive({input$dest})
  origen <- reactive({input$orig})
  
  dataosrm <- reactive({
    if (origen() == "Select your origin" || filtro() == "Select your destination") { 
      z <- data.frame()
    } else {
      zmpo <- x %>% filter(src == input$orig &
                   dst == input$dest)
    }
  })
  
  coords <- reactive({
    if (origen() == "Select your origin" || filtro() == "Select your destination") { 
      z <- data.frame()
    } else {
      zmpf <- y[y[, "lugaro"] == input$orig & y[, "lugard"] == input$dest, ]
      
    }
  })
  
  output$lf <- renderLeaflet({
    if (origen() == "Select your origin" || filtro() == "Select your destination") {
      content <- paste("<strong>- This is your reference map. The red mark is your origin
                       and the green mark is your destination.</strong>")
      
      
      m <- leaflet() %>%
        addTiles() %>%
        setView(lng = -98.7459, lat = 20.1153, zoom = 12) %>%
        addPopups(lng = -98.7459, lat = 20.1153, content,
                  options = popupOptions(closeButton = FALSE))
    } else {
      leaflet(dataosrm()) %>% 
        addProviderTiles(providers$OpenStreetMap, group = "OSM") %>% 
        addPolylines(color = "purple") %>%
        addMarkers(as.numeric(coords()$lono), as.numeric(coords()$lato), icon = list(
          iconUrl = 'https://cdn3.iconfinder.com/data/icons/social-messaging-ui-color-line/254000/87-512.png',
          iconSize = c(30, 30)
        )) %>%
        addMarkers(as.numeric(coords()$lond), as.numeric(coords()$latd), icon = list(
          iconUrl = 'https://cdn3.iconfinder.com/data/icons/social-messaging-ui-color-line/254000/176-512.png',
          iconSize = c(30, 30)
        ))  
    }
    
  })
  
  output$distancia <- renderValueBox({
    if (origen() == "Select your origin" || filtro() == "Select your destination") {
      valueBox("-.- km(s)", "Distance", icon = icon("car"),
               color = "purple")
    } else {
    valueBox(paste0(round(as.numeric(dataosrm()$distance), digits = 1), " km(s)."), "Distance", icon = icon("car"),
             color = "purple")
    }
  })
  
  output$cobron <- renderValueBox({
    if (origen() == "Select your origin" || filtro() == "Select your destination") {
      valueBox("--.-- MXN", "Night Rate +20% (aprox.)", icon = icon("moon"),
               color = "purple")
    } else {
      valueBox(paste0("$", ifelse(dataosrm()$distance <= 4, 32.5*1.2, (32.5 + 
                                                                (round(dataosrm()$distance) - 4)*3)*1.2), " MXN")
               , "Night Rate +20% (aprox.)", icon = icon("moon"),
               color = "purple")
    }
  })
  
  output$cobro <- renderValueBox({
    if (origen() == "Select your origin" || filtro() == "Select your destination") {
      valueBox("--.-- MXN", "Standard rate (aprox.)", icon = icon("sun"),
               color = "purple")
    } else {
      valueBox(paste0("$", ifelse(dataosrm()$distance <= 4, 32.5, 32.5 + 
                                    (round(dataosrm()$distance) - 4)*3), " MXN")
               , "Standard rate (aprox.)", icon = icon("sun"),
               color = "purple")
    }
  })

  
  output$lf2 <- renderLeaflet({
    
      zmpf <- lugares
      leaflet(data = zmpf) %>% addTiles() %>%
        addMarkers(~as.numeric(lono), ~as.numeric(lato), popup = ~as.character(lugaro),
                   icon = list(
                     iconUrl = 'https://cdn0.iconfinder.com/data/icons/shift-travel/32/Location_Pin-512.png',
                     iconSize = c(20, 25))
        )    
    
  })
  
}
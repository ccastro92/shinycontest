#beta 

library(dplyr)
library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(cartography)

lugares <- readRDS(file = "lugares.rds")
x <- readRDS(file = "rutas.rds")

dashboardPage(title="TIS | Pachuca", skin = "purple", 
  dashboardHeader(
    title = tags$img(src="logof.png", height='50', width='140'),
    
    tags$li(class = "dropdown",
            tags$a(
              #href = "", 
                   target = "_blank",
                   tags$img(height = "14px", 
                            icon("home")))),
    
    tags$li(class = "dropdown",
            tags$a(
              href = "https://www.facebook.com/tarifataxi", 
                   target = "_blank", 
                   tags$img(height = "14px", 
                            icon("facebook")))),
    
    tags$li(class = "dropdown",
            tags$a(
              href = "https://twitter.com/tarifa_taxi",
                   target = "_blank",
                   tags$img(height = "14px", 
                            icon("twitter"))))
  ),
  dashboardSidebar(
    sidebarMenu(menuItem("Home", tabName = "inicio", icon = icon("dashboard")),
                menuItem("About the Project", icon = icon("th"), tabName = "acerca"),
                menuItem("Our Coverage", tabName = "cobertura", icon = icon("map")),
                menuItem("Official Fare - SEMOT", tabName = "tarifa", icon = icon("money"))
                )
  ),
  dashboardBody(tags$head(tags$link(rel = "shortcut icon", href = "lg.png")),
                tabItems(
                  tabItem(tabName = "inicio",
                          fluidRow(tags$style(".nav-tabs {
  background-color: #FFF;
                          }
    

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: transparent;
border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #605ca8;
}"),
              tabItem("Route",
                      box( 
                       selectInput("orig", "Origin", 
                                   choices = unique(c("Select your origin",
                                                      as.character(lugares$lugaro[2:length(lugares$lugaro)]))), 
                                   width = "100%"),
                       selectInput("dest", "Destination", 
                                   choices = unique(c("Select your destination",
                                                      as.character(lugares$lugaro[2:length(lugares$lugaro)]))), 
                                   width = "100%"), width = 12),
                      width = 12),
       valueBoxOutput("distancia"), 
       valueBoxOutput("cobro"), 
       valueBoxOutput("cobron"),
       leafletOutput("lf", width = "100%"))),
      tabItem(tabName = "acerca",
              box(
                span("History", style = "color:purple"),
                p("The app is developed with the intention of eliminating a common problem by using the taxi service in the Metropolitan Area of Pachuca City (Hidalgo State, Mexico). This problem (among many others) is the unjustified high price for the service, although we know that there is a fee established by the Ministry of Mobility and Transportation of Hidalgo, the operators of the units rarely respect it, which generally increases the cost of the transfer or, in some cases, users don't want to pay the right cost for the journey that they have made."),
                span("A posible solution", style = "color:purple"),
                p("This project use R packages like Shiny, OSRM, Leaflet and others, to get the approximate cost of a trip according to the starting point and destination point based on the rate approved by the Conventional Transportation System of Hidalgo with the intention of avoiding any problematic situation between the user and the service provider.", 
                  style="text-align:justify"),
                span("In the future", style = "color:purple"),
                p("The objectives that it seeks to fulfill, either directly or indirectly, are issues such as the fair cost for the taxi service, improving the service by avoiding any type of confrontation due to disagreements with the payment, encouraging greater use of taxis since it generally decreases the cost in short journeys also, which is projected in a more advanced phase of the application, to be able to share through the interface the real-time location, information about the driver and personalized request of the service that minimizes the losses for the operator. ALL OF THESE USING R!.", style="text-align:justify"),
                h4("¡Thanks!", align = "center"),
                a("To check the original project, click on this link", href = "https://tarifainteligentedetaxi.shinyapps.io/beta/")
              , width = 12)
      ),
      tabItem(tabName = "cobertura",
              box(p("This map represents the effort of our team to define a catalog of strategic destinations for use in the application. We recognize, we need to expand the catalog of locations, which is why we work every day to have more destinations, so we value your collaboration by suggesting more locations that we will gladly add and get down to work!", 
                    style="text-align:justify"), width = 12),
              leafletOutput("lf2", width = "100%")),
      tabItem(tabName = "tarifa",
              box(HTML('<img src="semot.JPG" width="180">'),
              p(span("#NoTeDejesEngañar", style = "color:blue"), ", if someone wants to charge you more than the rate authorized by", span("#SEMOT", style = "color:blue"), "raise your complaint in the following ways: attention line 018005032002, social networks and email quejas_transporte@hidalgo.gob.mx", span("#SemotContigo", style = "color:blue")), 
                  a("Ir al tweet", href = "https://twitter.com/MovilidadHgo/status/1149430789211213825"),
                  width = 12),
              HTML('<center><img src="SEMOT tarifa.jpg" width="97%"></center>')
              )
    ),
    tags$footer("© Carlos Arturo Castro del Ángel, 
    2020.", 
                align = "center")
                
  )
)
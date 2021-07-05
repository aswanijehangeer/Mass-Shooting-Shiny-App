library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)

mass_shooting <- read_csv("Mother Jones - Mass Shootings Database, 1982 - 2021 - Sheet1.csv")
text_about <- "This data was compiled by Mother Jones, non profit founded in 1976.
Originally covering cases from 1982-2012, this database has since been expanded numerous times to remain current"

# Define UI for application
ui <- bootstrapPage(
    theme = shinythemes::shinytheme(theme = "simplex"),
    leaflet::leafletOutput("map"),
    absolutePanel(top = 10, right = 10, id = "controls",
                  sliderInput("n_fatalities", "Minimum Fatalities", 
                              min = 1, max = 40, value = 10),
                  dateRangeInput("date_range", "Select Date",
                                 start = "2010-01-01", end = "2019-12-01"),
                  actionButton("action_button", "About")),
    
    tags$style(type = "text/css",
               "html, body{width = 100%;height = 100%}
               #controls{background-color:white;padding:20px}")
)


# Define server logic 
server <- function(input, output) {
    
    rval_mass_shootings <- reactive({ 
        mass_shooting %>% 
            filter( 
                date >= input$date_range[1], 
                date <= input$date_range[2], 
                fatalities >= input$n_fatalities 
            )
    }) 
    
    output$map <- leaflet::renderLeaflet({
        rval_mass_shootings() %>% 
        leaflet() %>% 
            addTiles() %>% 
            setView(-98.82, 39.82, zoom = 5) %>% 
            addCircleMarkers(
                popup = ~ summary,
                radius = ~ fatalities, 
                fillColor = "red", color = "red", weight = 1)
            
    })
        
    observeEvent(input$action_button, {
        showModal(modalDialog(text_about, title = "About"))
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

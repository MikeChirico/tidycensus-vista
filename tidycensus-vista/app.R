#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(mapview)

{
    
    # vt <- get_acs(geography = "zcta", 
    #               variables = c(med_income = "B19013_001", 
    #                             broadband = "B28002_004",
    #                             cash_assistance = "DP03_0072PE",
    #                             food_stamps = "B22001_001",
    #                             snap = "DP03_0074E"), 
    #               state = "VT", 
    #               geometry = TRUE,
    #               year = 2019)
    # 
    # toy <- spread(vt, variable, estimate) %>% 
    #     select(-moe)  %>% 
    #     gather(var, val, -c(GEOID, NAME, geometry), na.rm = TRUE) %>%
    #     group_by(GEOID, NAME, var) %>%
    #     distinct(val) %>%
    #     spread(var, val)
    
    census_data <- readRDS("test_data.rds")

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("VISTA Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
    
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        
        pal <- colorQuantile(palette = "viridis", domain = census_data$food_stamps, n = 10)
        
        census_data %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(food_stamps)) %>%
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~ food_stamps,
                      title = "Broadband percentiles",
                      opacity = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

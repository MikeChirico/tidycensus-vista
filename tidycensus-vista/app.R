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
    
    vt <- get_acs(geography = "zcta",
                  variables = c(med_income = "B19013_001",
                                broadband = "B28002_004",
                                cash_assistance = "DP03_0072PE",
                                food_stamps = "B22001_001",
                                snap = "DP03_0074PE"),
                  state = "VT",
                  geometry = TRUE,
                  year = 2019)
    # 
    # toy <- spread(vt, variable, estimate) %>%
    #     select(-moe)  %>%
    #     gather(var, val, -c(GEOID, NAME, geometry), na.rm = TRUE) %>%
    #     group_by(GEOID, NAME, var) %>%
    #     distinct(val) %>%
    #     spread(var, val)
    # 
    # saveRDS(toy, "test_data.rds")
    # saveRDS(vt, "test_data2.rds")
    # 
    census_data <- readRDS("test_data.rds")
    census_data2 <- readRDS("test_data2.rds")

}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("VISTA Map"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "map_filter",
                label = "Select Data:",
                choices = c(
                    "Broadband" = 'broadband',
                    "Food Stamps" = 'food_stamps',
                    "Median Income" = 'med_income',
                    "Cash Assistance" = 'DP03_0072P',
                    "SNAP" = 'DP03_0074'
                ),
                selected = 'broadband',
                inline = T
            )
            
    
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map"),
           textOutput("test")
        ),
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        
        #pal <- colorQuantile(palette = "viridis", domain = census_data$input$map_filter, n = 10)
        
        
        census_data %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7#,
                        #color = ~ pal(input$map_filter)
                        ) %>%
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~ .$input$map_filter,
                      title = 'foo',
                      opacity = 1)
    })
    
    #output$test <- renderText({toString(input$map_filter)})
}

# Run the application 
shinyApp(ui = ui, server = server)

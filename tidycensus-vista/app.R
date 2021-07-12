#
# has actual income in tooltip
#MEdian income number
#toggle zip
#adding in other communities
#add descriptions

#madlibs


library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(mapview)
library(sf)

{
    
#     vt_tract <- get_acs(geography = "tract",
#                   variables = c(med_income = "B19013_001",
#                                 broadband = "B28002_001",
#                                 SSI = "B09010_001"),
#                   state = "VT",
#                   geometry = TRUE,
#                   year = 2019)

    # vt_zip <- get_acs(geography = "zcta",
    #                     variables = c(med_income = "B19013_001",
    #                                   broadband = "B28002_001",
    #                                   SSI = "B09010_001"),
    #                     state = "VT",
    #                     geometry = TRUE,
    #                     year = 2019)
    # # #
    # toy <- spread(vt_zip, variable, estimate) %>%
    #     select(-moe)  %>%
    #     gather(var, val, -c(GEOID, NAME, geometry), na.rm = TRUE) %>%
    #     group_by(GEOID, NAME, var) %>%
    #     distinct(val) %>%
    #     spread(var, val)
# # 
#     saveRDS(toy, "test_data.rds")
#     saveRDS(vt_tract, "test_data2.rds")
#     saveRDS(vt_zip, "test_data3.rds")
    #saveRDS(toy, 'newformat.rds')
# 
#     census_data <- readRDS("test_data.rds")
#     census_data2 <- readRDS("test_data2.rds")
#     census_data3 <- readRDS("test_data3.rds")
    new_census_data <- readRDS('newformat.rds')
    
    true_zips <- census_data3$NAME %>% str_sub(7,11)
    
    
    
    fooo <- census_data2 %>% mutate(type = 'tract') 
    fooo2 <- census_data3 %>% mutate(type = 'zip')
    fooo2$NAME <- true_zips
    
    both_census_data <- rbind(fooo, fooo2)
}


ui <- fluidPage(
    
    # Application title
    titlePanel("VISTA Map"),
    
    
    ################################################################################
    tabsetPanel(################################################################################
                ##Makes 'map' tab
                ################################################################################
                tabPanel(
                    "Map",
                    sidebarPanel(
                        radioButtons(
                            inputId = "map_filter",
                            label = "Select Data:",
                            choices = c(
                                "Broadband" = 'broadband',
                                "Median Income" = 'med_income',
                                "Cash Assistance" = 'SSI'
                            ),
                            selected = 'broadband',
                            inline = F
                        ),
####################################################################################
#Tract or zip selector
                        radioButtons(
                            inputId = "tract_zip",
                            label = "ZIP Code or Census Tract",
                            choices = c("ZIP Code" = 'zip',
                                        "Census Tract" = 'tract'),
                            selected = 'zip',
                            inline = F
                        )),
                        mainPanel(leafletOutput("map"))
                    ),
 ################################################################################
##Makes 'madlib' tab
################################################################################
                    tabPanel("Madlibs", textOutput("foobar"))
                )
)

# Server
server <- function(input, output) {

    output$map <- renderLeaflet({
        
        select_data <- both_census_data %>% filter(type == input$tract_zip)
        
        #filters data based on input
        filtered_data <- select_data %>% filter(variable == input$map_filter)
        

        #Creates color palette based on selected variable
        pal <- colorQuantile(palette = "viridis", domain = census_data$input$map_filter, n = 10)
        
        
        #making the actual map
        filtered_data %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(estimate)) %>%
            addLegend("bottomright", 
                      pal = pal, 
                      values = ~ estimate,
                      title = filtered_data$variable[1],
                      opacity = 1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

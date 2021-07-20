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
library(bootstraplib)
library(sf)

{
    census_format <- function(state) {
        state_zip <- get_acs(
            geography = "zcta",
            variables = c(
                med_income = "B19013_001",
                broadband = "B28002_001",
                SSI = "B09010_001"
            ),
            state = state,
            geometry = TRUE,
            year = 2019
        ) %>%
            mutate(STATE = state) %>%
            mutate(type = "ZIP")
        
        state_zip2 <- get_acs(
            geography = "tract",
            variables = c(
                med_income = "B19013_001",
                broadband = "B28002_001",
                SSI = "B09010_001"
            ),
            state = state,
            geometry = TRUE,
            year = 2019
        ) %>%
            mutate(STATE = state) %>%
            mutate(type = "tract")
        
        true_zips <- state_zip$NAME %>% str_sub(7, 11)
        
        state_zip$NAME <- true_zips
        
        state_zip_final <- rbind(state_zip, state_zip2)
        
        return(state_zip_final)
    }
    {
        
        community <- c("AR", "MI", "NM", "OH", "OR", "VT")
        
        latty <- c(34.225144200039, 
                 46.597158844198965, 
                 36.410414694018414, 
                 38.73228468898352, 
                 44.85247381476704, 
                 43.29915952802458)
        
        longg <- c(-92.00326861792746, 
                  -87.3933541201746, 
                  -105.57427805765356, 
                  -82.99674689875215,
                  -123.18471615622822, 
                  -72.48528945998511)
        
        name <- c("The Generator", 
                  "Innovate Marquette Smartzone", 
                  "UNM-Taos HIVE", 
                  "Kricker Innovation Hub", 
                  "Indy Commons", 
                  "The Black River Innovation Campus")
        
        
        sup_df <- tibble(comm = community,
                               lat = latty,
                               long = longg)
        
        sup_df2 <- tibble(name = c("broadband", "med_income", "SSI"),
                          fancy_name = c("Broadband Access Percentile",
                                         "Median Income Percentile",
                                         "Receipt of SSI Percentile"))
        
        
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
        
        
        # new_census_data <- readRDS('newformat.rds')
        #
        # true_zips <- census_data3$NAME %>% str_sub(7,11)
        #
        #
        #
        # fooo <- census_data2 %>% mutate(type = 'tract')
        # fooo2 <- census_data3 %>% mutate(type = 'zip')
        # fooo2$NAME <- true_zips
        #
        # both_census_data <- rbind(fooo, fooo2)
    }
    ########################################################################
    #Current method of getting all the communities data
    {
        # pb <- census_format("AR")
        # marq <- census_format("MI")
        # taos <- census_format("NM")
        # portsmouth <- census_format("OH")
        # indy <- census_format("OR")
        # springfield <- census_format("VT")
        # all_comms <- rbind(pb, marq, taos, portsmouth, indy, springfield)
    }
    
    #saveRDS(all_comms, "ALL_COMMUNITIES.rds")
    all_comms <- readRDS("ALL_COMMUNITIES.rds")
    
    
}

{description_html <- '<h2 style="color: #2e6c80;">Purpose of this tool:</h2>
<p>This tool was created to help new AmeriCorps VISTA members understand the communities they will be serving in. As well as provide them some guidance about where to focus their outreach efforts.</p>
<p>The goals tracking tab is designed to help VISTAs track their poverty alleviation goals. VISTAs can input the amount of people who received services and the ZIP code those people are in. Using this we can get an estimate of the number of people in poverty that have received services. This number can be used for tracking and reporting goals for AmeriCorps,</p>
<h2 style="color: #2e6c80;">Census data used:</h2>
<ul>
<li>Broadband
<ul>
<li>Table B28002</li>
<li>Presence and Types of Internet Subscriptions in Household</li>
</ul>
</li>
<li>Median Income
<ul>
<li>Table B19013</li>
<li>Median Household Income</li>
</ul>
</li>
<li>SSI<br />
<ul>
<li>Table B09010</li>
<li>Receipt of Supplemental Security Income (SSI), Cash Public Assistance Income, or Food Stamps/SNAP by Household Type for Children in Households</li>
</ul>
</li>
</ul>
<p>&nbsp;</p>'}

ui <- fluidPage(# Application title
    
    # meta() %>%
    #     meta_social(
    #         title = "RIN Community Explorer",
    #         description = "<meta> A tool for understanding RIN communities"),
    
    titlePanel("RIN Community Explorer"),
    
    
    ################################################################################
    tabsetPanel(
        ################################################################################
        ##Makes 'map' tab
        ################################################################################
        tabPanel(
            "Community Explorer",
            sidebarPanel(
                radioButtons(
                    inputId = "map_filter",
                    label = "Select Data:",
                    choices = c(
                        "Broadband" = 'broadband',
                        "Median Income" = 'med_income',
                        "Cash Assistance" = 'SSI'
                    ),
                    selected = 'SSI',
                    inline = F
                ),
                ####################################################################################
                #Community selector
                radioButtons(
                    inputId = "community_select",
                    label = "Select A Community:",
                    choices = c(
                        "Arkansas (Pine Bluff)" = "AR",
                        "Michigan (Marquette)" = "MI",
                        "New Mexico (Taos)" = "NM",
                        "Ohio (Portsmouth)" = "OH",
                        "Oregon (Independence)" = "OR",
                        "Vermont (Springfield)" = 'VT'
                    ),
                    selected = 'VT',
                    inline = F
                ),
                #Tract or zip selector
                radioButtons(
                    inputId = "tract_zip",
                    label = "ZIP Code or Census Tract:",
                    choices = c("ZIP Code" = 'ZIP',
                                "Census Tract" = 'tract'),
                    selected = 'tract',
                    inline = F
                )
            ),
            
            mainPanel(leafletOutput("map"))
        ),
        ################################################################################
        ##Makes 'Poverty Alleviation Goals Tracker' tab
        ################################################################################
        tabPanel("Poverty Alleviation Goals Tracker", HTML('<h5>Coming soon</h5>')),
        tabPanel("Description", HTML(description_html))
    ))

# Server
server <- function(input, output) {
    output$map <- renderLeaflet({
        select_data <- all_comms %>% 
            filter(STATE == input$community_select) %>% 
            filter(type == input$tract_zip)
        
        markers <- sup_df %>% filter(comm == input$community_select)
        
        legend_title <- sup_df2 %>% filter(name == input$map_filter)
        
        #filters data based on input
        filtered_data <-
            select_data %>% filter(variable == input$map_filter)
        
        
        #Creates color palette based on selected variable
        pal <-
            colorQuantile(
                palette = "viridis",
                domain = select_data$input$map_filter,
                n = 10
            )
        
        
        #making the actual map
        filtered_data %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(
                popup = ~ str_extract(NAME, "^([^,]*)"),
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(estimate)
            ) %>%
            addLegend(
                "bottomright",
                pal = pal,
                values = ~ estimate,
                title = legend_title$fancy_name[1],
                opacity = 1
            ) %>% 
            addMarkers(lng = markers$long,
                       lat = markers$lat,
                       label = markers$name)
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)

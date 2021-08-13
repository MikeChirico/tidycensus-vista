
####################################################################################################
##All packages downloaded and used for this project
####################################################################################################
library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(mapview)
library(bootstraplib)
library(kableExtra)
library(sf)

{
    ####################################################################################################
    ##Function for getting data and formatting
    ##currently gets data in wide format and not long
    ##Something to change if someone wants to improve and expand the app
    ####################################################################################################
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
    
    ####################################################################################################
    ##Important setup pieces
    ##Reading in data and hardcoded variables
    ####################################################################################################
    {
        ####################################################################################################
        ##A bunch of hardcoded variables used for getting state names, 
        ##locations and names of community partners and fancy names of variables
        ####################################################################################################
        {community <- c("AR", "MI", "NM", "OH", "OR", "VT")
        
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
                                         "Receipt of SSI Percentile"))}
        
        ####################################################################################################
        ##long string of html used in the description page
        ####################################################################################################
        {description_html <- '<h2 style="color: #2e6c80;">Purpose of this tool:</h2>
        <p>This tool was created to help new AmeriCorps VISTA members understand the communities they will be serving in. As well as provide them some guidance about where to focus their outreach efforts.</p>
        <p>The goals tracking tab is designed to help VISTAs track their poverty alleviation goals. VISTAs can input the amount of people who received services and the ZIP code those people are in. Using this we can get an estimate of the number of people in poverty that have received services. This number can be used for tracking and reporting goals for AmeriCorps,</p>
        <h2 style="color: #2e6c80;">Census Data Key:</h2> <ul> <ul> <li>Broadband<br /> <ul>
        <li>Presence and Types of Internet Subscriptions in Household</li> <li>Table B28002</li> </ul> </li> <li>Median Income<br /> <ul>
        <li>Median Household Income</li> <li>Table B19013</li> </ul> </li> <li>SSI<br /> <ul>
        <li>Receipt of Supplemental Security Income (SSI), Cash Public Assistance Income, or Food Stamps/SNAP by Household Type for Children in Households</li>
        <li>Table B09010</li> </ul> </li> </ul> </ul> <h2 style="color: #2e6c80;">Goal Tracker Tutorial:</h2>
        <p>This tab can help you calculate how many people you have reached with your programs qualify as low income. It takes the number of people you have help in each zip code and multiplies that by the proportion of people in that zip code receiving supplemental income.&nbsp;</p>
        <p>This tool only uses zip codes from states that VISTAs are serving in (Arkansas, Michigan, New Mexico, Ohio, Oregon, and Vermont).</p>'}
        
        ####################################################################################################
        ##Old code and tests
        ##some methods in here for reformatting data in long form
        ####################################################################################################
        {#     vt_tract <- get_acs(geography = "tract",
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
    }
    ####################################################################################################
    ##Current method of getting all the communities data
    ##Commented out so it doesnt run everytime code is run
    ####################################################################################################
    {
        # pb <- census_format("AR")
        # marq <- census_format("MI")
        # taos <- census_format("NM")
        # portsmouth <- census_format("OH")
        # indy <- census_format("OR")
        # springfield <- census_format("VT")
        # all_comms <- rbind(pb, marq, taos, portsmouth, indy, springfield)
        #saveRDS(all_comms, "ALL_COMMUNITIES.rds")
    }
    
    ####################################################################################################
    ##Reading in data files
    ####################################################################################################
    {all_comms <- readRDS("ALL_COMMUNITIES.rds")
    small <- readRDS("small_zip_prop.rds")}
        
}


####################################################################################################
##UI page
##creates front end of the app
####################################################################################################
ui <- fluidPage(
    ####################################################################################################
    ##Title
    ####################################################################################################
    titlePanel("RIN Community Explorer"),
    
    
    ####################################################################################################
    ##Creating the tab panels
    ####################################################################################################
    tabsetPanel(
        ####################################################################################################
        ##The map tab
        ####################################################################################################
        tabPanel(
            "Community Explorer",
            ####################################################################################################
            ##Sidebar inputs and buttons
            ####################################################################################################
            sidebarPanel(
                ####################################################################################################
                ##Variable selector buttons
                ####################################################################################################
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
                ####################################################################################################
                ##Community selector buttons
                ####################################################################################################
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
                ####################################################################################################
                ##Tract or zip selector
                ####################################################################################################
                radioButtons(
                    inputId = "tract_zip",
                    label = "ZIP Code or Census Tract:",
                    choices = c("ZIP Code" = 'ZIP',
                                "Census Tract" = 'tract'),
                    selected = 'tract',
                    inline = F
                )
            ),
            ####################################################################################################
            ##Main panel with the map
            ####################################################################################################
            mainPanel(leafletOutput("map"))
        ),
        ################################################################################
        ##Makes 'Poverty Alleviation Goals Tracker' tab
        ################################################################################
        tabPanel("Poverty Alleviation Goals Tracker", 
            sidebarPanel(
                ####################################################################################################
                ##Inputs for zip code, copied and pasted 5 times
                ####################################################################################################
                splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(
                        inputId = "num_people_input1",
                        label = "# of People Reached",
                        value = 25,
                        min = 0,
                        max = NA),
                    textInput(
                        inputId = "zip_input1",
                        label = "ZIP Code",
                        value = "05156")
                    ),
                splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(
                        inputId = "num_people_input2",
                        label = "# of People Reached",
                        value = 0,
                        min = 0,
                        max = NA),
                    textInput(
                        inputId = "zip_input2",
                        label = "ZIP Code"
                        )
                ),
                splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(
                        inputId = "num_people_input3",
                        label = "# of People Reached",
                        value = 0,
                        min = 0,
                        max = NA),
                    textInput(
                        inputId = "zip_input3",
                        label = "ZIP Code")
                ),
                splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(
                        inputId = "num_people_input4",
                        label = "# of People Reached",
                        value = 0,
                        min = 0,
                        max = NA),
                    textInput(
                        inputId = "zip_input4",
                        label = "ZIP Code")
                ),
                splitLayout(
                    cellWidths = c("50%", "50%"),
                    numericInput(
                        inputId = "num_people_input5",
                        label = "# of People Reached",
                        value = 0,
                        min = 0,
                        max = NA),
                    textInput(
                        inputId = "zip_input5",
                        label = "ZIP Code")
                )
                ),
            ####################################################################################################
            ##Actual table for tracking
            ##Is html output bc it is made with kable and has extra grand total attached to that
            ####################################################################################################
            mainPanel(htmlOutput("tracker_table"),
                      )
            ),
        ####################################################################################################
        ##Description tab, created in the setup. 
        ##Long string of static html for description
        ##THIS IS EASY TO EDIT
        ####################################################################################################
        tabPanel("Description", HTML(description_html))
    ))

####################################################################################################
##Server side
## creates reactive elements like map and table
####################################################################################################
server <- function(input, output) {
    
    ####################################################################################################
    ##making map
    ####################################################################################################
    output$map <- renderLeaflet({
        
        ####################################################################################################
        ##Filtering data from imported file by community
        ####################################################################################################
        select_data <- all_comms %>% 
            filter(STATE == input$community_select) %>% 
            filter(type == input$tract_zip)
        
        ####################################################################################################
        ##adding markers for partner orgs
        ####################################################################################################
        markers <- sup_df %>% filter(comm == input$community_select)
        
        ####################################################################################################
        ##fixing names
        ####################################################################################################
        legend_title <- sup_df2 %>% filter(name == input$map_filter)
        
        ####################################################################################################
        ##filtering data based on community
        ####################################################################################################
        filtered_data <-
            select_data %>% filter(variable == input$map_filter)
        
        # 
        # ####################################################################################################
        # ##creating palette based on selected data
        # ####################################################################################################
        pal <-
            colorQuantile(
                palette = "viridis",
                domain = select_data$input$map_filter,
                n = 10
            )
        
        ###################################################################################################
        #making the actual map
        ###################################################################################################
        filtered_data %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(
                popup = ~ str_extract(NAME, "^([^,]*)"),
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(estimate),
                highlight = highlightOptions()
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
    
    ###################################################################################################
    #making the tracker table
    ###################################################################################################
    output$tracker_table <- renderText({
        
        zip_people <- c(input$num_people_input1,
                      input$num_people_input2,
                      input$num_people_input3,
                      input$num_people_input4,
                      input$num_people_input5)
        
        zip_list <- c(input$zip_input1,
                      input$zip_input2,
                      input$zip_input3,
                      input$zip_input4,
                      input$zip_input5)
        ####################################################################################################
        ##dummy data for testing
        ####################################################################################################
        {
        # zip_people2 <- c(6,
        #                 30,
        #                 20,
        #                 8)
        # 
        # zip_list2 <- c('05156',
        #                '05150',
        #                '45241',
        #                '72438')
    }
        
        foo <- rbind(small %>% 
                  filter(ZIP == zip_list[1]) %>% 
                  mutate(`Number of People` = zip_people[1]) %>% 
                  mutate(Total = `Proportion on SSI` * `Number of People`),
              small %>% 
                  filter(ZIP == zip_list[2]) %>% 
                  mutate(`Number of People` = zip_people[2]) %>% 
                  mutate(Total = `Proportion on SSI` * `Number of People`),
              small %>% 
                  filter(ZIP == zip_list[3]) %>% 
                  mutate(`Number of People` = zip_people[3]) %>% 
                  mutate(Total = `Proportion on SSI` * `Number of People`),
              small %>% 
                  filter(ZIP == zip_list[4]) %>% 
                  mutate(`Number of People` = zip_people[4]) %>% 
                  mutate(Total = `Proportion on SSI` * `Number of People`),
              small %>% 
                  filter(ZIP == zip_list[5]) %>% 
                  mutate(`Number of People` = zip_people[5]) %>% 
                  mutate(Total = `Proportion on SSI` * `Number of People`)) %>% 
            select(-Population, -SSI) %>% 
            select(ZIP,
                   `Number of People`,
                   `Proportion on SSI`,
                   Total)
        
        fooKable <- foo %>% 
            kable("html") %>% 
            kable_styling("striped", full_width = TRUE)
        
        
final <- paste0(fooKable,'<tr> <td style="0text-align: left;">&nbsp;</td>
<td style="0text-align: right;">&nbsp;</td>
<td style="0text-align: right;">&nbsp;</td>
<td style="0text-align: right;">', 'Total Number of People Reached: ', sum(foo$Total),'</td>
</tr>')


    })
    
}

# Run the application
shinyApp(ui = ui, server = server)


library(tidyverse)
library(tidycensus)
library(leaflet)
library(mapview)
library(sf)


#api 5bc1aeed953e5cf836804a8bff7c0cd4f5059dd2

#census_api_key("5bc1aeed953e5cf836804a8bff7c0cd4f5059dd2", install = TRUE)

v17 <- load_variables(2017, "acs5", cache = TRUE)

v19 <- load_variables(2019, "acs5", cache = TRUE)
View(v17)

age10 <- get_decennial(geography = "state", 
                       year = 2010)

vt <- get_acs(geography = "zcta", 
              variables = c(med_income = "B19013_001", 
                            broadband = "B28002_004",
                            cash_assistance = "DP03_0072PE",
                            food_stamps = "B22001_001",
                            snap = "DP03_0074E"), 
              state = "VT", 
              geometry = TRUE,
              year = 2019)



vt2 <- get_acs(geography = "zcta", #geography = "county" will do by county
              variables = c(broadband = "B28002_004"),
              state = "VT", 
              geometry = TRUE,
              year = 2019)
  
#creates color palette
pal <- colorQuantile(palette = "viridis", domain = vt2$estimate, n = 10)

vt2 %>%
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
            title = "Broadband percentiles",
            opacity = 1)
  

vtfood <- get_acs(geography = "county", 
                    variables = c(food_stamps = "B22001_001"),
                    geometry = TRUE,
                    year = 2018)


toy <- spread(vt, variable, estimate) %>% 
  select(-moe)  %>% 
  gather(var, val, -c(GEOID, NAME, geometry), na.rm = TRUE) %>%
  group_by(GEOID, NAME, var) %>%
  distinct(val) %>%
  spread(var, val)

saveRDS(toy, "test_data.rds")

toy %>%
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






windsor <- get_acs(state = "VT", county = "Windsor", geography = "tract", 
                   variables = "B19013_001", geometry = TRUE)

pal <- colorNumeric(palette = "viridis", 
                    domain = windsor$estimate)

windsor %>%
    #st_transform(crs = "+init=epsg:4326") %>%
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
              title = "Household Income",
              opacity = 1)


#Maps by Rachael Below:

#Broadband Data


vtbb <- get_acs(state = "VT", geography = "tract", 
                    variables = "B28002_004", geometry = TRUE)

#Broadband Map

pal <- colorQuantile(palette = "plasma", domain = vtbb$estimate, n = 10)


vtbb %>%
    #st_transform(crs = "+init=epsg:4326") %>%
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
              title = "Broadband Sub.",
              opacity = 1)

#SNAP Data

vtSNAP <- get_acs(state = "VT", geography = "tract", 
                   variables = "DP03_0074PE", geometry = TRUE)

#SNAP MAP


pal <- colorQuantile(palette = "plasma", domain = vtSNAP$estimate, n = 10)

vtSNAP %>%
    #st_transform(crs = "+init=epsg:4326") %>%
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
              title = "Perc. SNAP",
              opacity = 1)

#Cash Assitance Data

vtassistanceblock <- get_acs(state = "VT", geography = "tract", 
                   variables = "DP03_0072PE", geometry = TRUE)

#Cash Assitance Map

pal <- colorQuantile(palette = "plasma", domain = vtassistanceblock$estimate, n = 10)

vtassistanceblock %>%
    #st_transform(crs = "+init=epsg:4326") %>%
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
              title = "Perc. w Cash Assistance",
              opacity = 1)

#Median Income Data

vtincome <- get_acs(state = "VT", geography = "tract", 
                   variables = "B19013_001", geometry = TRUE)

#Median Income Map

pal <- colorNumeric(palette = "plasma", 
                    domain = vtincome$estimate)

vtincome %>%
    #st_transform(crs = "+init=epsg:4326") %>%
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
              title = "Household Income",
              opacity = 1)


#B19058_001 food stamps
#Percent of Households with Broadband Internet Subscription TableID: GCT2801 / 1	B28002_004
#Percent of Households with Cash assistance GCT1904
#Median Household Income GCT1901 / B19013_001
#Percent of Households that receive food stamps/SNAP GCT2201 / 8	B22001_001

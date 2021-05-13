
library(tidyverse)
library(tidycensus)
library(leaflet)
library(mapview)


#api 5bc1aeed953e5cf836804a8bff7c0cd4f5059dd2

census_api_key("5bc1aeed953e5cf836804a8bff7c0cd4f5059dd2", install = TRUE)

v17 <- load_variables(2017, "acs5", cache = TRUE)

v19 <- load_variables(2019, "acs5", cache = TRUE)
View(v17)

age10 <- get_decennial(geography = "state", 
                       year = 2010)

vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001", 
                            broadband = "B28002_004",
                            food_stamps = "B22001_001"), 
              state = "VT", 
              geometry = TRUE,
              year = 2018)

vt2 <- get_acs(geography = "county", 
              variables = c(broadband = "B28002_004"),
              geometry = TRUE,
              year = 2018)

vtfood <- get_acs(geography = "county", 
                    variables = c(food_stamps = "B22001_001"),
                    geometry = TRUE,
                    year = 2018)




#B19058_001 food stamps
#Percent of Households with Broadband Internet Subscription TableID: GCT2801 / 1	B28002_004
#Percent of Households with Cash assistance GCT1904
#Median Household Income GCT1901 / B19013_001
#Percent of Households that receive food stamps/SNAP GCT2201 / 8	B22001_001

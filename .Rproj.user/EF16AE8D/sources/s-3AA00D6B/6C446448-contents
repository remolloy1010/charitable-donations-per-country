library(acs)
library(tigris)
library(leaflet)
library(mapview)
library(stringr)
library(sf)
library(tidyverse)
library(tidycensus)

initialize <- function(){
  # Install API key to get access to census data
  # Source: https://api.census.gov/data/key_signup.html
  api.key.install(key="0b5de1b0030f5b45d191c313a872de3a1ba1c362")
  
  ### Fetch spatial shapefile for giving area ###
  # Find state and county codes from ACS package with geo.lookup
  geo.lookup(state="UT", place="Sandy") # To find which county a city is a part of
  geo.lookup(state="UT", county="Salt Lake County") # To find FIPS codes for state and county
  
  # From tigris package; finds FIPS code for state and county
  lookup_code(state="UT",county="Salt Lake County")
  
  # Find spatial shapefile data using tracts from tigris
  shapefile <- tracts(state='49', county='035')
  plot(shapefile)
  
  
  census_api_key("37ac73a1c944172445bf6a7366224526db63ac38", overwrite=TRUE)
  
  # Viewing census data variables:
  v17 <- load_variables(2017, "acs5", cache = TRUE)
}

get_county_geom_data <- function(){
  
  utah_pop <- get_acs(geography = "county", 
                      variables = "B01003_001", 
                      state = "UT",
                      geometry = TRUE)
  utah_pop %>%
    mutate(NAME = gsub(" County, Utah", "", NAME))
  return(utah_pop)
}

import_donation_data <- function(){
  df_csv <- data.frame(read.csv('utah-counties.csv'))
  
  # Pull only data of interest
  state_fips <- df_csv$STATEFIPS
  state_abbrv <- df_csv$STATE
  county_fips <- df_csv$COUNTYFIPS
  county_name <- df_csv$COUNTYNAME
  adj_gross_income_id <- df_csv$agi_stub
  num_returns <- df_csv$N1
  num_dependents <- df_csv$NUMDEP
  adj_gross_income <- df_csv$A00100
  num_returns_w_total_income <- df_csv$N02650
  total_income_amt <- df_csv$A02650
  num_returns_donations <- df_csv$N19700
  total_amt_donations <- df_csv$A19700
  
  utah_df <- data.frame(state_fips,
                        state_abbrv,
                        county_fips,
                        county_name,
                        adj_gross_income_id,
                        num_returns,
                        num_dependents,
                        adj_gross_income,
                        num_returns_w_total_income,
                        total_income_amt,
                        num_returns_donations,
                        total_amt_donations)
  return(utah_df)
}

filter_on_income_level <- function(df, income_val){

  df_income_lvl <- filter(df, adj_gross_income_id==income_val)
  df_income_lvl$COUNTYNAME
  df <- df_income_lvl[df_income_lvl$county_name != "Utah",]
  return(df)
}
irs_df <- filter_on_income_level(import_donation_data(), 5)
irs_df <- irs_df[order(irs_df$county_name),]


gis_data <- get_county_geom_data() %>%
  mutate(NAME = gsub(", Utah", "", NAME))

gis_data <- gis_data[order(gis_data$NAME),]

add_donation_data_to_geom <- function(gis_df, irs_df){
  gis_df$total_amt_donations <- irs_df$total_amt_donations
  return(gis_df)
}
df <- add_donation_data_to_geom(gis_data, irs_df)
df

show_on_map <- function(df){
  # Show on map
  df %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(total_amt_donations)) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~ total_amt_donations,
              title = "Total Donations by County",
              opacity = 1)
  
}
show_on_map(df)
df[df$NAME == "Rich County",]

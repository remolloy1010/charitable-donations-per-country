library(acs)
library(tigris)
library(leaflet)
library(mapview)
library(stringr)
library(sf)
library(tidyverse)
library(tidycensus)

## TO DO:
# - update to in the thousands and add dollar sign to labels
# - clean up code

initialize_data <- function(){
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
  num_returns_w_donations <- df_csv$N19700
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
                        num_returns_w_donations,
                        total_amt_donations)
  utah_df$perct_amt_income_donated <- as.integer(
    (utah_df$total_amt_donations / utah_df$total_income_amt) * 100)
  utah_df$perct_returns_w_donations <- as.integer(
    (utah_df$num_returns_w_donations / utah_df$num_returns_w_total_income) * 100)
  
  return(utah_df)
}

filter_on_income_level <- function(df, income_val){

  df_income_lvl <- filter(df, adj_gross_income_id==income_val)
  df_income_lvl$COUNTYNAME
  df <- df_income_lvl[df_income_lvl$county_name != "Utah",]
  return(df)
}

add_donation_data_to_geom <- function(gis_df, irs_df){
  gis_df$total_amt_donations <- irs_df$total_amt_donations
  gis_df$perct_amt_income_donated <- irs_df$perct_amt_income_donated
  gis_df$perct_returns_w_donations <- irs_df$perct_returns_w_donations
  gis_df$num_returns <- irs_df$num_returns
  gis_df$num_returns_w_total_income <- irs_df$num_returns_w_total_income
  gis_df$total_income_amt <- irs_df$total_income_amt
  gis_df$adj_gross_income <- irs_df$adj_gross_income
  return(gis_df)
}


show_on_map <- function(df){
  
  pal <- colorNumeric(palette = "plasma", 
                      domain = df$perct_returns_w_donations)
  # Show on map
  df %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(perct_returns_w_donations)) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~ perct_returns_w_donations,
              title = "Total Donations by County",
              opacity = 1)

  
}

get_data <- function(income_lvl){
  # Get API key, data, etc.
  # initialize_data()
  
  # Get IRS data
  irs_df <- filter_on_income_level(import_donation_data(), income_lvl)
  irs_df <- irs_df[order(irs_df$county_name),]
  
  # Get GIS data
  gis_data <- get_county_geom_data() %>%
    mutate(NAME = gsub(", Utah", "", NAME))
  gis_data <- gis_data[order(gis_data$NAME),]
  
  df <- add_donation_data_to_geom(gis_data, irs_df)
  print(df)
  return(df)
}

income_lvl <- 6

df <- get_data(income_lvl)
top_6_perct_donated <- head(df[order(df$perct_amt_income_donated),])$NAME
top_6_perct_w_donations <- head(df[order(df$perct_returns_w_donations),])$NAME
top_6_perct_donated
top_6_perct_w_donations
show_on_map(df)

total_returns <- sum(import_donation_data()$num_returns[import_donation_data()$county_name == "Utah"] )
total_pop <- sum(gis_data$estimate)
perct_pop_w_returns <- (total_returns / total_pop) * 100
perct_pop_w_returns
# What I'd like to know:

# - Which income levels donate the highest % of their income on donations?
# - Which income levels donate the most? (perct_returns_w_donations)
# - Is there a relationship between other factors and high levels of donations? 
      #(religious communities, political party, etc.)
# - Are the counties that donate the highest % of their income also donating the most across the county population?
# - Percentage of population in this income level in this county** - must account for age and who does tax returns

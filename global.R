# Global for Carbon-travel-App
library(shiny)
library(bslib)
library(shinylogs)
library(tidyverse)
library(sf)
library(sfnetworks)
library(highcharter)
library(reactable)
library(leaflet)

source(here::here("R", "utils.R"))

# Set paths -------------------------------------------------------------

clean_path <- here::here("data", "clean")

# Setup -------------------------------------------------------------------
app_name <- "msf-carbon-app"
app_title <- "MSF Carbon Travel App"
sp_path <- Sys.getenv("SHINYPROXY_PUBLIC_PATH")
is_sp_env <- sp_path != ""

# Import data -------------------------------------------------------------

# Get the distance matrices
d_mat_flights <- rio::import(here::here(clean_path, "matrix", "distance_matrix_flights.rds"))
d_mat_cities <- read_rds(here::here(clean_path, "matrix", "distance_matrix_cities.rds"))
e_mat_flights <- rio::import(here::here(clean_path, "matrix", "emissions_matrix_flights.rds"))
e_mat_cities <- read_rds(here::here(clean_path, "matrix", "emissions_matrix_cities.rds"))

#load the network
f_net <- read_rds(here::here(clean_path, "network", "flights_network.rds"))
c_net <- read_rds(here::here(clean_path, "network", "cities_network.rds"))

#all possible destinations
cities_df <- read_rds(here::here(clean_path, "network", "dest_cities.rds"))

# get the conversion df - given by Maelle
df_conversion <- read_rds(here::here(clean_path, "cities", "conversion_df.rds"))

orig_cities <- cities_df |>
  shinyWidgets::prepare_choices(
    label = city_name,
    value = city_code,
    group_by = country_name
  )

# get the air_msf data
msf <- read_rds(here::here(clean_path, "cities", "unique_msf_clean.rds"))

msf_type_vec <- c(
  "MSF HQ OC",
  "MSF office",
  "Field Coordination",
  "Field Project",
  "MSF Supply Center",
  "Research center"
)

disconnected <- sever::sever_default(
  title = "Disconnected",
  subtitle = "Sorry your session timed-out or something went wrong",
  button = "Reconnect",
  button_class = "info"
)

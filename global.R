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

# Get the distance matrix
distance_mat <- read_rds(here::here(clean_path, "distance_matrix.rds"))
emissions_mat <- read_rds(here::here(clean_path, "emissions_matrix.rds"))

#all possible destinations
dest <- read_rds(here::here(clean_path, "dest_cities.rds"))

orig_cities <- dest |>
  shinyWidgets::prepare_choices(
    label = city_name,
    value = city_code,
    group_by = country_name
  )

# get the air_msf data
msf <- read_rds(here::here(clean_path, "unique_msf_clean.rds"))

# msf_type_vec <- read_rds(here::here(clean_path, "full_msf_clean.rds")) |>
#   distinct(msf_type) |>
#   pull(msf_type)

msf_type_vec <- c(
  "MSF HQ OC",
  "MSF office",
  "Field Coordination",
  "Field Project",
  "MSF Supply Center",
  "Research center"
)

# get the conversion df - given by Maelle
df_conversion <- read_rds(here::here(clean_path, "conversion_df.rds"))

# Get AMEX data
# df_amex <- read_rds(here::here(clean_path, "amex_clean_lon_lat.rds"))
# df_travels <- read_rds(here::here(clean_path, "full_amex_wagram_cwt.rds"))

#load the network 
net <- read_rds(here::here(clean_path, "flights_network.rds"))

#load the distance matrix for all cities
# cities_network <- read_rds(here::here(clean_path, "cities_network.rds"))

# date range
# init_year <- sort(unique(df_travels$year))
# init_date_range <- format(seq.Date(min(df_travels$invoice_date), max(df_travels$invoice_date), by = "month"), "%Y-%m")
# min_date <- min(init_date_range)
# max_date <- max(init_date_range)

# local disk cache
# shiny::shinyOptions(cache = cachem::cache_disk(here::here(".cache")))

disconnected <- sever::sever_default(
  title = "Disconnected",
  subtitle = "Sorry your session timed-out or something went wrong",
  button = "Reconnect",
  button_class = "info"
)
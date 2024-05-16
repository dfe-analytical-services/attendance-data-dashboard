# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ----------------------------------------------------------------------------------

library(rsconnect)
library(shinyGovstyle)
library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(openxlsx)
library(kableExtra)
library(metathis)
library(styler)
library(bit64)
library(DT)
library(raster)
library(leaflet)
library(sf)
library(checkmate)
library(dfeshiny)
library(shinytest2)
library(diffviewer)
library(RODBC)

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

source("R/support_links.R")
source("R/prerun_utils.R")

site_primary <- " https://department-for-education.shinyapps.io/pupil-attendance-in-schools"
site_overflow <- " https://department-for-education.shinyapps.io/pupil-attendance-in-schools-overflow"
site_c <- ""
google_analytics_key <- "DG7P4WLB0Y"

# Data manipulation ----------------------------------------------------------------------------
# Read in data
# attendance_data_raw <- fread("data/Weekly_dummy_data.csv")

#### SECTION 1 - date filters ####
start_date <- as.Date("2023-09-11")
end_date <- as.Date("2024-05-03")
# funeral_date <- as.Date("2022-09-19")
# strike_date_1 <- as.Date("2023-02-01")
# strike_date_2 <- as.Date("2023-03-15")
# strike_date_3 <- as.Date("2023-03-16")
# strike_date_4 <- as.Date("2023-04-27")
# strike_date_5 <- as.Date("2023-05-02")
# strike_date_6 <- as.Date("2023-07-05")
# strike_date_7 <- as.Date("2023-07-07")

# regional_strike_1 <- as.Date("2023-02-28")
# regional_strike_2 <- as.Date("2023-03-01")
# regional_strike_3 <- as.Date("2023-03-02")
autumn_start <- as.Date("2023-09-11")
autumn_end <- as.Date("2023-12-15")
spring_start <- as.Date("2024-01-02")
spring_end <- as.Date("2024-03-31")
# summer_start <- as.Date("2023-04-01")
# summer_end <- as.Date("2023-07-21")

most_recent_week_dates <- paste0("Latest week -", as.Date(end_date) - 4, " to ", as.Date(end_date))
ytd_dates <- paste0("Year to date -", as.Date(start_date), "to", as.Date(end_date))

#### SECTION 2 - reading in csvs to run dashboard ####
attendance_data <- read.csv("data/attendance_data_dashboard.csv")

message(paste("Finished processing steps, ", Sys.time()))

EES_daily_data <- read_ees_daily()

#### SECTION 3 - Lookups ####
# Add geog lookup
geog_lookup <- attendance_data %>%
  dplyr::select(geographic_level, region_name, la_name) %>%
  unique() %>%
  arrange(region_name, la_name) %>%
  mutate(la_name = case_when(
    geographic_level == "Regioinal" ~ "All",
    geographic_level != "Regional" ~ la_name
  ))

school_type_lookup <- attendance_data %>%
  dplyr::select(geographic_level, school_type) %>%
  unique() %>%
  arrange(geographic_level, school_type)

# Combined local authority and region list
la_list <- geog_lookup %>%
  dplyr::select(region_name, la_name) %>%
  filter(region_name != "All") %>%
  filter(la_name != "NA") %>%
  distinct() %>%
  arrange(region_name, la_name) %>%
  group_by(region_name) %>%
  dplyr::select(region_name, la_name) %>%
  group_split(.keep = FALSE) %>%
  unlist(recursive = FALSE)

names(la_list) <- geog_lookup %>%
  dplyr::select(region_name) %>%
  filter(region_name != "All") %>%
  distinct() %>%
  pull(region_name) %>%
  sort()

# date filter lookups
most_recent_week_lookup <- attendance_data %>%
  group_by(geographic_level, region_name, la_name) %>%
  filter(time_period == max(time_period)) %>%
  filter(time_identifier == max(time_identifier)) %>%
  mutate(
    week_start = min(attendance_date),
    week_end = max(attendance_date)
  ) %>%
  dplyr::select(geographic_level, region_name, la_name, week_start, week_end) %>%
  distinct()

year_lookup <- attendance_data %>%
  group_by(geographic_level, region_name, la_name) %>%
  mutate(
    year_start = min(attendance_date),
    year_end = max(attendance_date)
  ) %>%
  dplyr::select(geographic_level, region_name, la_name, year_start, year_end) %>%
  distinct()

# Notes tables----------------------------------

notesTableHeadlines <- fread("data/Tech_guidance_headlines.csv")
notesTableReasons <- fread("data/Tech_guidance_reasons.csv")
notesTableLa <- fread("data/Tech_guidance_la.csv")

geog_levels <- geog_lookup %>%
  dplyr::select(geographic_level) %>%
  unique() %>%
  as.data.table()

regions <- geog_lookup %>%
  filter(geographic_level == "Regional") %>%
  arrange(region_name) %>%
  pull(region_name) %>%
  unique()
las <- geog_lookup %>%
  filter(geographic_level == "Local authority") %>%
  arrange(region_name, la_name) %>%
  pull(la_name) %>%
  unique()

#### SECTION 4 - Functions ####
# Expandable dropdown function----------------------------------
expandable <- function(inputId, label, contents) {
  govDetails <- shiny::tags$details(
    class = "govuk-details", id = inputId,
    shiny::tags$summary(
      class = "govuk-details__summary",
      shiny::tags$span(
        class = "govuk-details__summary-text",
        label
      )
    ),
    shiny::tags$div(contents)
  )
}


# Map ---------------------------------------------------------------------------------

## Custom rounding function ################################################

roundFiveUp <- function(value, dp) {
  if (!is.numeric(value) && !is.numeric(dp)) stop("both inputs must be numeric")
  if (!is.numeric(value)) stop("the value to be rounded must be numeric")
  if (!is.numeric(dp)) stop("the decimal places value must be numeric")

  z <- abs(value) * 10^dp
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^dp
  return(z * sign(value))
}

#### SECTION 5 - Map ####
## Reading in data ##########################################################

# Read in shapefile and transform coordinates (because map reasons...)
mapshape <- st_read("data/CTYUA_MAY_2023_UK_BUC.shp") %>% st_transform(crs = 4326) # %>% mutate(CTYUA23CD = case_when(CTYUA23NM == "Somerset" ~ "E10000027", CTYUA23NM != "Somerset" ~ CTYUA23CD)) TEMP addition working around Somerset LA code change

# Process the joined files to refine our 'mapdata', not pretty yet and mostly done just cos it's how its done in global...

mapdata0 <- attendance_data %>%
  mutate(time_identifier = as.numeric(str_remove_all(time_identifier, "Week "))) %>%
  filter(time_period == max(time_period)) %>%
  filter(time_identifier == max(time_identifier)) %>%
  # filter(time_identifier == max(time_identifier) - 1) %>%
  filter(geographic_level == "Local authority") %>%
  filter(breakdown == "Weekly")


mapdata <- mapdata0 %>%
  mutate(CTYUA23CD = new_la_code) %>% # renaming to match to shapefile later
  filter(!is.na(region_name), !is.na(la_name))

mapdata <- mapdata %>%
  group_by(time_period, time_identifier, geographic_level, region_name, la_name, CTYUA23CD, school_type) %>%
  mutate(
    overall_label_LA = paste(la_name),
    overall_label_rate = paste(as.character(roundFiveUp(overall_absence_perc, 1)), "%", sep = ""),
    overall_label = paste0(overall_label_LA, " overall absence rate: ", overall_label_rate),
    auth_label_LA = paste(la_name),
    auth_label_rate = paste(as.character(roundFiveUp(authorised_absence_perc, 1)), "%", sep = ""),
    auth_label = paste0(auth_label_LA, " authorised absence rate: ", auth_label_rate),
    unauth_label_LA = paste(la_name),
    unauth_label_rate = paste(as.character(roundFiveUp(unauthorised_absence_perc, 1)), "%", sep = ""),
    unauth_label = paste0(unauth_label_LA, " unauthorised absence rate: ", unauth_label_rate)
  )

## Combine shapefile and data into mapdata ###############################################

# Merge the transformed shapefile with the processed source data ---------------
mapdata_shaped <- merge(mapshape, mapdata, by = "CTYUA23CD", duplicateGeoms = TRUE)

# Create colour bins and palette labels --------------------------------------

# Pull in the colours from another script
source("R/gov_colours.R")

# Create bins
overall_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$overall_abs_perc, n = 5)

auth_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$auth_abs_perc, n = 5)

unauth_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$unauth_abs_perc, n = 5)

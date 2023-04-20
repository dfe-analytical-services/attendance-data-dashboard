# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(shinytest)
library(shinydashboard)
library(shinyWidgets)
library(shinyGovstyle)
library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
# library(shinya11y)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(flexdashboard)
library(scales)
library(forcats)
library(openxlsx)
library(kableExtra)
library(metathis)
library(styler)
library(rsconnect)
library(bit64)
library(DT)
library(raster)
library(leaflet)
library(rgdal)
library(sf)
library(checkmate)

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
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
site_overflow <- " https://department-for-education.shinyapps.io/pupil-attendance-in-schools-mirror"
site_c <- ""

# Data manipulation ----------------------------------------------------------------------------
# Read in data
attendance_data_raw <- fread("data/sql_export_2023_04_18.csv")
pa_data_raw <- fread("data/export_pa_output_2023_04_18.csv")
# attendance_data_raw <- fread("data/Weekly_dummy_data.csv")
start_date <- as.Date("2022-09-12")
end_date <- as.Date("2023-04-07")
funeral_date <- as.Date("2022-09-19")
strike_date_1 <- as.Date("2023-02-01")
strike_date_2 <- as.Date("2023-03-15")
strike_date_3 <- as.Date("2023-03-16")
regional_strike_1 <- as.Date("2023-02-28")
regional_strike_2 <- as.Date("2023-03-01")
regional_strike_3 <- as.Date("2023-03-02")
autumn_only_pa_data_raw <- fread("data/export_autumn_pa_output_2023_04_18.csv")
autumn_start <- as.Date("2022-09-12")
autumn_end <- as.Date("2022-12-16")
spring_only_pa_data_raw <- fread("data/export_spring_pa_output_2023_04_18.csv")
spring_start <- as.Date("2023-01-03")
spring_end <- as.Date("2023-03-31")

school_freq_count <- fread("data/enrolments_schools_denominator.csv")

list_attendance <- process_attendance_data(attendance_data_raw, start_date, end_date, funeral_date)
attendance_data <- list_attendance$attendance_data
attendance_data_daily_totals <- list_attendance$daily_totals
attendance_data_weekly_totals <- list_attendance$weekly_totals
attendance_data_ytd_totals <- list_attendance$ytd_totals

list_attendance_autumn <- process_attendance_data_autumn(attendance_data_raw, autumn_start, autumn_end)
attendance_data_autumn <- list_attendance_autumn$attendance_data_autumn
attendance_data_autumn_totals <- list_attendance_autumn$autumn_totals

list_attendance_spring <- process_attendance_data_spring(attendance_data_raw, spring_start, spring_end)
attendance_data_spring <- list_attendance_spring$attendance_data_spring
attendance_data_spring_totals <- list_attendance_spring$spring_totals

EES_daily_data <- read_ees_daily()

# Add geog lookup
geog_lookup <- attendance_data_raw %>%
  dplyr::select(geographic_level, region_name, la_name) %>%
  unique() %>%
  arrange(region_name, la_name)

school_type_lookup <- attendance_data %>%
  dplyr::select(geographic_level, school_type) %>%
  unique() %>%
  arrange(geographic_level, school_type)


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

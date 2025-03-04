# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
#
# Install packages -----------------------------------------------
# Run renv::restore()
# If it doesn't work first time, maybe try renv::activate() and then renv::restore()


# Packages to work towards removing from the dashboard:
library(DT)
library(openxlsx)
library(kableExtra)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(RODBC)


# Library calls ----------------------------------------------------------------------------------
library(rsconnect)
library(shinyGovstyle)
library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(data.table)
library(ggplot2)
library(ggiraph)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(scales)
library(forcats)
library(metathis)
library(styler)
library(bit64)
library(raster)
library(leaflet)
library(sf)
library(checkmate)
library(dfeshiny)
library(reactable)
library(shinytest2)
library(diffviewer)
library(dplyr)
library(eesyapi)

# Functions ---------------------------------------------------------------------------------

plotting_font_family <- intersect(
  c("Gills Sans MT", "Helvetica", "Arial", "Noto Sans", "Open Sans", "FreeSans"),
  systemfonts::system_fonts() |>
    dplyr::filter(style == "Regular") |>
    dplyr::pull(family) |>
    unique()
) |>
  magrittr::extract(1)


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

ees_api_env <- "dev"

if (ees_api_env == "prod") {
  api_verbose <- TRUE
  api_environment <- "prod"
  reasons_dataset_id <- ""
  reasons_dataset_ref_version <- 1.0
  presistent_absence_dataset_id <- ""
  presistent_absence_ref_version <- 1.0
  submitting_schools_dataset_id <- ""
  submitting_schools_ref_version <- 1.0
} else if (ees_api_env == "dev") {
  api_verbose <- FALSE
  api_environment <- "dev"
  reasons_dataset_id <- "ab619501-50cf-1b70-b276-76d72a3c141c"
  reasons_dataset_ref_version <- 1.0
  presistent_absence_dataset_id <- "3b299501-2ab1-de76-a9b5-a3ae26ac0bcd"
  presistent_absence_ref_version <- 2.0
  submitting_schools_dataset_id <- "ac619501-eb0a-ff71-b03c-5330fe30349a"
  submitting_schools_ref_version <- 1.0
} else if (ees_api_env == "test") {
  api_verbose <- TRUE
  api_environment <- "test"
  reasons_dataset_id <- "8e8c9301-55c5-3e71-abbb-73ac64420c4a"
  reasons_dataset_ref_version <- 2.0
  presistent_absence_dataset_id <- ""
  presistent_absence_ref_version <- 1.0
  submitting_schools_dataset_id <- ""
  submitting_schools_ref_version <- 1.0
} else {
  stop("Invalid environment given in ees_api_env variable.")
}

source("R/prerun_utils.R")
source("R/fetch_data.R")

site_title <- "Pupil attendance and absence in schools in England"
site_primary <- "https://department-for-education.shinyapps.io/pupil-attendance-in-schools"
site_c <- ""
google_analytics_key <- "DG7P4WLB0Y"

ees_pub_name <- "Pupil attendance in schools"
ees_pub_slug <- "pupil-attendance-in-schools"
team_email <- "schools.statistics@education.gov.uk"

# Some standard geography lookups
region_la_lookup <- dfeR::wd_pcon_lad_la_rgn_ctry |>
  filter(country_name == "England") |>
  select(region_name, region_code, la_name, new_la_code) |>
  distinct() |>
  arrange(region_name)


# Pull in original data set api id look-up lists.
# This is fixed to a single reference point version and allows the code to use human readable
# parameters for the API calls. This is a bit that could cause some issues if there's some breaking
# changes to the data set. As long as there's no breaking changes, then the version number used
# here should not be changed.
reasons_sqids <- fetch_sqid_lookup(
  reasons_dataset_id,
  version = reasons_dataset_ref_version,
  ees_environment = ees_api_env
)

persistent_absence_sqids <- fetch_sqid_lookup(
  reasons_dataset_id,
  version = reasons_dataset_ref_version,
  ees_environment = ees_api_env
)



# Data manipulation ----------------------------------------------------------------------------
# Read in data
# attendance_data_raw <- fread("data/Weekly_dummy_data.csv")

#### SECTION 1 - date filters ####
date_stamp <- lubridate::stamp_date("20 March 2025")
start_date <- as.Date("2024-09-09")
end_date <- as.Date("2025-01-24")
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

autumn_start <- as.Date("2024-09-09")
autumn_end <- as.Date("2024-12-21")
# spring_start <- as.Date("2025-01-06")
# spring_end <- as.Date("2024-03-31")
# summer_start <- as.Date("2024-04-01")
# summer_end <- as.Date("2024-07-19")

most_recent_week_dates <- paste0("Latest week - ", as.Date(end_date) - 4, " to ", as.Date(end_date))
ytd_dates <- paste0("Year to date - ", as.Date(start_date), " to ", as.Date(end_date))

#### SECTION 2 - reading in csvs to run dashboard ####
attendance_data <- read.csv("data/attendance_data_dashboard.csv")
attendance_data$attendance_date <- as.Date(attendance_data$attendance_date)
attendance_data$week_commencing <- as.Date(attendance_data$week_commencing)

message(paste("Finished processing steps, ", Sys.time()))

EES_daily_data <- create_EES_daily_data(attendance_data)

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

# Pull in the colours from another script
source("R/gov_colours.R")

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

# Data manipulation ----------------------------------------------------------------------------
# Read in data
attendance_data_raw <- fread("data/Weekly_dummy_extended.csv")
attendance_data_raw <- attendance_data_raw %>% filter(time_identifier != "Week 52")
school_freq_count <- fread("data/school_frequency_count_202208_v2_enrolments.csv")

#Set up data for download
attendance_data_fordownload <- attendance_data_raw %>%
  mutate(across(.cols = 14:46, .fns = as.numeric)) %>%
  arrange(time_period, time_identifier) %>%
  filter(school_type %in% c("Primary", "Secondary", "Special"))

#Set up data for use across the app
#Take the raw data and make columns numeric and filter to only Primary, Secondary and Special
attendance_data <- attendance_data_raw %>%
  mutate(across(.cols = 14:45, .fns = as.numeric)) %>%
  mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>%
  mutate(across(.cols = 1:2, .fns = as.numeric)) %>%
  mutate(across(.cols = 12:12, .fns = as.numeric)) %>%
  arrange(time_period, time_identifier) %>%
  filter(school_type %in% c("Primary", "Secondary", "Special"))

#Calculate date
attendance_data <- attendance_data %>% mutate(date = as.Date(paste(attendance_data$time_period,
                                                                   attendance_data$time_identifier,
                                                                   attendance_data$day_number,
                                                                   sep = "-"
), "%Y-%U-%u"))

#Join school frequency count for proportion of schools reporting and pupil headcount for calculation of weighted totals
attendance_data <- left_join(attendance_data, select(school_freq_count, c(geographic_level, region_name, la_name, phase, total_num_schools, total_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "phase"))

#Calculate measures for use across app, grouping appropriately at each level then binding back together
attendance_data_daily <- attendance_data %>% 
  mutate(attendance_perc = (present_sessions / possible_sessions) * 100,
         overall_absence_perc = (overall_absence / possible_sessions) * 100,
         authorised_absence_perc = (authorised_absence / possible_sessions) * 100,
         unauthorised_absence_perc = (unauthorised_absence / possible_sessions) * 100,
         illness_perc = (reason_i_authorised_illness / possible_sessions) * 100,
         appointments_perc = (reason_m_authorised_medical_dental / possible_sessions) * 100,
         excluded_perc = (reason_e_authorised_excluded / possible_sessions) * 100,
         unauth_hol_perc = (reason_g_unauthorised_holiday / possible_sessions) * 100,
         unauth_oth_perc = (reason_o_other_unauthorised / possible_sessions) * 100,
         unauth_late_registers_closed_perc = (reason_u_unauthorised_late_after_registers_closed / possible_sessions) * 100,
         unauth_not_yet_perc = (reason_n_no_reason_yet / possible_sessions) * 100,
         auth_religious_perc = (reason_r_authorised_religious_observance / possible_sessions) * 100,
         auth_study_perc = (reason_s_authorised_study_leave / possible_sessions) * 100,
         auth_grt_perc = (reason_t_authorised_grt_absence / possible_sessions) * 100,
         auth_holiday_perc = (reason_h_authorised_holiday / possible_sessions) * 100,
         auth_excluded_perc = (reason_e_authorised_excluded / possible_sessions) * 100,
         auth_other_perc = (reason_c_authorised_other / possible_sessions) * 100,
         breakdown = "Daily")

attendance_data_weekly <- attendance_data %>%
  group_by(time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
  mutate(num_schools = sum(num_schools),
         enrolments = sum(enrolments),
         present_sessions = sum(present_sessions),
         approved_educational_activity = sum(approved_educational_activity),
         overall_absence = sum(overall_absence),
         authorised_absence = sum(authorised_absence),
         unauthorised_absence = sum(unauthorised_absence),
         late_sessions = sum(late_sessions),
         possible_sessions = sum(possible_sessions),
         reason_present_am = sum(reason_present_am),
         reason_present_pm = sum(reason_present_pm),
         reason_present = sum(reason_present),
         reason_l_present_late_before_registers_closed = sum(reason_l_present_late_before_registers_closed),
         reason_i_authorised_illness = sum(reason_i_authorised_illness),
         reason_m_authorised_medical_dental = sum(reason_m_authorised_medical_dental),
         reason_r_authorised_religious_observance = sum(reason_r_authorised_religious_observance),
         reason_s_authorised_study_leave = sum(reason_s_authorised_study_leave),
         reason_t_authorised_grt_absence = sum(reason_t_authorised_grt_absence),
         reason_h_authorised_holiday = sum(reason_h_authorised_holiday),
         reason_e_authorised_excluded = sum(reason_e_authorised_excluded),
         reason_c_authorised_other = sum(reason_c_authorised_other),
         reason_b_aea_education_off_site = sum(reason_b_aea_education_off_site),
         reason_d_aea_dual_registration = sum(reason_d_aea_dual_registration),
         reason_j_aea_interview = sum(reason_j_aea_interview),
         reason_p_aea_approved_sporting_activity = sum(reason_p_aea_approved_sporting_activity),
         reason_v_aea_educational_visit_trip = sum(reason_v_aea_educational_visit_trip),
         reason_w_aea_work_experience = sum(reason_w_aea_work_experience),
         reason_g_unauthorised_holiday = sum(reason_g_unauthorised_holiday),
         reason_u_unauthorised_late_after_registers_closed = sum(reason_u_unauthorised_late_after_registers_closed),
         reason_o_other_unauthorised = sum(reason_o_other_unauthorised),
         reason_n_no_reason_yet = sum(reason_n_no_reason_yet),
         reason_x_not_attending_covid_non_compulsory = sum(reason_x_not_attending_covid_non_compulsory),
         total_num_schools = sum(total_num_schools),
         total_enrolments = sum(total_enrolments),
         attendance_perc = (sum(present_sessions) / sum(possible_sessions)) *100,
         overall_absence_perc = (sum(overall_absence) / sum(possible_sessions)) * 100,
         authorised_absence_perc = (sum(authorised_absence) / sum(possible_sessions)) * 100,
         unauthorised_absence_perc = (sum(unauthorised_absence) / sum(possible_sessions)) * 100,
         illness_perc = (sum(reason_i_authorised_illness) / sum(possible_sessions)) * 100,
         appointments_perc = (sum(reason_m_authorised_medical_dental) / sum(possible_sessions)) * 100,
         excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
         unauth_hol_perc = (sum(reason_g_unauthorised_holiday) / sum(possible_sessions)) * 100,
         unauth_oth_perc = (sum(reason_o_other_unauthorised) / sum(possible_sessions)) * 100,
         unauth_late_registers_closed_perc = (sum(reason_u_unauthorised_late_after_registers_closed) / sum(possible_sessions)) * 100,
         unauth_not_yet_perc = (sum(reason_n_no_reason_yet) / sum(possible_sessions)) * 100,
         auth_religious_perc = (sum(reason_r_authorised_religious_observance) / sum(possible_sessions)) * 100,
         auth_study_perc = (sum(reason_s_authorised_study_leave) / sum(possible_sessions)) * 100,
         auth_grt_perc = (sum(reason_t_authorised_grt_absence) / sum(possible_sessions)) * 100,
         auth_holiday_perc = (sum(reason_h_authorised_holiday) / sum(possible_sessions)) * 100,
         auth_excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
         auth_other_perc = (sum(reason_c_authorised_other) / sum(possible_sessions)) * 100,
         breakdown = "Weekly") %>%
  distinct(time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all= TRUE)

attendance_data_ytd <- attendance_data %>%
  group_by(time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
  mutate(num_schools = sum(num_schools),
         enrolments = sum(enrolments),
         present_sessions = sum(present_sessions),
         approved_educational_activity = sum(approved_educational_activity),
         overall_absence = sum(overall_absence),
         authorised_absence = sum(authorised_absence),
         unauthorised_absence = sum(unauthorised_absence),
         late_sessions = sum(late_sessions),
         possible_sessions = sum(possible_sessions),
         reason_present_am = sum(reason_present_am),
         reason_present_pm = sum(reason_present_pm),
         reason_present = sum(reason_present),
         reason_l_present_late_before_registers_closed = sum(reason_l_present_late_before_registers_closed),
         reason_i_authorised_illness = sum(reason_i_authorised_illness),
         reason_m_authorised_medical_dental = sum(reason_m_authorised_medical_dental),
         reason_r_authorised_religious_observance = sum(reason_r_authorised_religious_observance),
         reason_s_authorised_study_leave = sum(reason_s_authorised_study_leave),
         reason_t_authorised_grt_absence = sum(reason_t_authorised_grt_absence),
         reason_h_authorised_holiday = sum(reason_h_authorised_holiday),
         reason_e_authorised_excluded = sum(reason_e_authorised_excluded),
         reason_c_authorised_other = sum(reason_c_authorised_other),
         reason_b_aea_education_off_site = sum(reason_b_aea_education_off_site),
         reason_d_aea_dual_registration = sum(reason_d_aea_dual_registration),
         reason_j_aea_interview = sum(reason_j_aea_interview),
         reason_p_aea_approved_sporting_activity = sum(reason_p_aea_approved_sporting_activity),
         reason_v_aea_educational_visit_trip = sum(reason_v_aea_educational_visit_trip),
         reason_w_aea_work_experience = sum(reason_w_aea_work_experience),
         reason_g_unauthorised_holiday = sum(reason_g_unauthorised_holiday),
         reason_u_unauthorised_late_after_registers_closed = sum(reason_u_unauthorised_late_after_registers_closed),
         reason_o_other_unauthorised = sum(reason_o_other_unauthorised),
         reason_n_no_reason_yet = sum(reason_n_no_reason_yet),
         reason_x_not_attending_covid_non_compulsory = sum(reason_x_not_attending_covid_non_compulsory),
         total_num_schools = sum(total_num_schools),
         total_enrolments = sum(total_enrolments),
         attendance_perc = (sum(present_sessions) / sum(possible_sessions)) * 100,
         overall_absence_perc = (sum(overall_absence) / sum(possible_sessions)) * 100,
         authorised_absence_perc = (sum(authorised_absence) / sum(possible_sessions)) * 100,
         unauthorised_absence_perc = (sum(unauthorised_absence) / sum(possible_sessions)) * 100,
         illness_perc = (sum(reason_i_authorised_illness) / sum(possible_sessions)) * 100,
         appointments_perc = (sum(reason_m_authorised_medical_dental) / sum(possible_sessions)) * 100,
         excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
         unauth_hol_perc = (sum(reason_g_unauthorised_holiday) / sum(possible_sessions)) * 100,
         unauth_oth_perc = (sum(reason_o_other_unauthorised) / sum(possible_sessions)) * 100,
         unauth_late_registers_closed_perc = (sum(reason_u_unauthorised_late_after_registers_closed) / sum(possible_sessions)) * 100,
         unauth_not_yet_perc = (sum(reason_n_no_reason_yet) / sum(possible_sessions)) * 100,
         auth_religious_perc = (sum(reason_r_authorised_religious_observance) / sum(possible_sessions)) * 100,
         auth_study_perc = (sum(reason_s_authorised_study_leave) / sum(possible_sessions)) * 100,
         auth_grt_perc = (sum(reason_t_authorised_grt_absence) / sum(possible_sessions)) * 100,
         auth_holiday_perc = (sum(reason_h_authorised_holiday) / sum(possible_sessions)) * 100,
         auth_excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
         auth_other_perc = (sum(reason_c_authorised_other) / sum(possible_sessions)) * 100,
         breakdown = "YTD") %>%
  distinct(time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all= TRUE)

attendance_data <- rbind(attendance_data_daily, attendance_data_weekly, attendance_data_ytd)

#Prep for calculation of totals by doing rates X census counts
attendance_data <- attendance_data %>% 
  mutate(attendance_perc_scaled = attendance_perc * total_enrolments,
         overall_absence_perc_scaled = overall_absence_perc * total_enrolments,
         authorised_absence_perc_scaled = authorised_absence_perc * total_enrolments,
         unauthorised_absence_perc_scaled = unauthorised_absence_perc * total_enrolments,
         illness_perc_scaled = illness_perc * total_enrolments,
         appointments_perc_scaled = appointments_perc * total_enrolments,
         excluded_perc_scaled = excluded_perc * total_enrolments,
         unauth_hol_perc_scaled = unauth_hol_perc * total_enrolments,
         unauth_oth_perc_scaled = unauth_oth_perc * total_enrolments,
         unauth_late_registers_closed_perc_scaled = unauth_late_registers_closed_perc * total_enrolments,
         unauth_not_yet_perc_scaled = unauth_not_yet_perc * total_enrolments,
         auth_religious_perc_scaled = auth_religious_perc * total_enrolments,
         auth_study_perc_scaled = auth_study_perc * total_enrolments,
         auth_grt_perc_scaled = auth_grt_perc * total_enrolments,
         auth_holiday_perc_scaled = auth_holiday_perc * total_enrolments,
         auth_excluded_perc_scaled = auth_excluded_perc * total_enrolments,
         auth_other_perc_scaled = auth_other_perc * total_enrolments)

#Calculate total as (Primary rate X primary census count) + (Secondary rate X secondary census count) + (Special rate X special census count) and divided all by total census count
attendance_data_daily_totals <- attendance_data %>%
  filter(breakdown == "Daily") %>%
  group_by(breakdown, time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, attendance_date, day_number, date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(school_type = "Total",
         enrolments_pa_10_exact = "z",
         attendance_perc = (sum(attendance_perc_scaled) / sum(total_enrolments)),
         overall_absence_perc = (sum(overall_absence_perc_scaled) / sum(total_enrolments)),
         authorised_absence_perc = (sum(authorised_absence_perc_scaled) / sum(total_enrolments)),
         unauthorised_absence_perc = (sum(unauthorised_absence_perc_scaled) / sum(total_enrolments)),
         illness_perc = (sum(illness_perc_scaled) / sum(total_enrolments)),
         appointments_perc = (sum(appointments_perc_scaled) / sum(total_enrolments)),
         excluded_perc = (sum(excluded_perc_scaled) / sum(total_enrolments)),
         unauth_hol_perc = (sum(unauth_hol_perc_scaled) / sum(total_enrolments)),
         unauth_oth_perc = (sum(unauth_oth_perc_scaled) / sum(total_enrolments)),
         unauth_late_registers_closed_perc = (sum(unauth_late_registers_closed_perc_scaled) / sum(total_enrolments)),
         unauth_not_yet_perc = (sum(unauth_not_yet_perc_scaled) / sum(total_enrolments)),
         auth_religious_perc = (sum(auth_religious_perc_scaled) / sum(total_enrolments)),
         auth_study_perc = (sum(auth_study_perc_scaled) / sum(total_enrolments)),
         auth_grt_perc = (sum(auth_grt_perc_scaled) / sum(total_enrolments)),
         auth_holiday_perc = (sum(auth_holiday_perc_scaled) / sum(total_enrolments)),
         auth_excluded_perc = (sum(auth_excluded_perc_scaled) / sum(total_enrolments)),
         auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments))
         )

attendance_data_weekly_totals <- attendance_data %>%
  filter(breakdown == "Weekly") %>%
  group_by(breakdown, time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
  summarise(across(matches("date"), min, na.rm = T),
            across(matches("attendance_date"), min, na.rm = T),
            across(matches("day_number"), min, na.rm = T),
            across(where(is.numeric)& !c(date, attendance_date, day_number), sum), na.rm = T) %>%
  mutate(school_type = "Total",
         enrolments_pa_10_exact = "z",
         attendance_perc = (sum(attendance_perc_scaled) / sum(total_enrolments)),
         overall_absence_perc = (sum(overall_absence_perc_scaled) / sum(total_enrolments)),
         authorised_absence_perc = (sum(authorised_absence_perc_scaled) / sum(total_enrolments)),
         unauthorised_absence_perc = (sum(unauthorised_absence_perc_scaled) / sum(total_enrolments)),
         illness_perc = (sum(illness_perc_scaled) / sum(total_enrolments)),
         appointments_perc = (sum(appointments_perc_scaled) / sum(total_enrolments)),
         excluded_perc = (sum(excluded_perc_scaled) / sum(total_enrolments)),
         unauth_hol_perc = (sum(unauth_hol_perc_scaled) / sum(total_enrolments)),
         unauth_oth_perc = (sum(unauth_oth_perc_scaled) / sum(total_enrolments)),
         unauth_late_registers_closed_perc = (sum(unauth_late_registers_closed_perc_scaled) / sum(total_enrolments)),
         unauth_not_yet_perc = (sum(unauth_not_yet_perc_scaled) / sum(total_enrolments)),
         auth_religious_perc = (sum(auth_religious_perc_scaled) / sum(total_enrolments)),
         auth_study_perc = (sum(auth_study_perc_scaled) / sum(total_enrolments)),
         auth_grt_perc = (sum(auth_grt_perc_scaled) / sum(total_enrolments)),
         auth_holiday_perc = (sum(auth_holiday_perc_scaled) / sum(total_enrolments)),
         auth_excluded_perc = (sum(auth_excluded_perc_scaled) / sum(total_enrolments)),
         auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments))
  )

attendance_data_ytd_totals <- attendance_data %>%
  filter(breakdown == "YTD") %>%
  group_by(breakdown, time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
  summarise(across(matches("time_identifier"), min, na.rm = T),
            across(matches("date"), min, na.rm = T),
            across(matches("attendance_date"), min, na.rm = T),
            across(matches("day_number"), min, na.rm = T),
            across(where(is.numeric)& !c(time_identifier, date, attendance_date, day_number), sum), na.rm = T) %>%
  mutate(school_type = "Total",
         enrolments_pa_10_exact = "z",
         attendance_perc = (sum(attendance_perc_scaled) / sum(total_enrolments)),
         overall_absence_perc = (sum(overall_absence_perc_scaled) / sum(total_enrolments)),
         authorised_absence_perc = (sum(authorised_absence_perc_scaled) / sum(total_enrolments)),
         unauthorised_absence_perc = (sum(unauthorised_absence_perc_scaled) / sum(total_enrolments)),
         illness_perc = (sum(illness_perc_scaled) / sum(total_enrolments)),
         appointments_perc = (sum(appointments_perc_scaled) / sum(total_enrolments)),
         excluded_perc = (sum(excluded_perc_scaled) / sum(total_enrolments)),
         unauth_hol_perc = (sum(unauth_hol_perc_scaled) / sum(total_enrolments)),
         unauth_oth_perc = (sum(unauth_oth_perc_scaled) / sum(total_enrolments)),
         unauth_late_registers_closed_perc = (sum(unauth_late_registers_closed_perc_scaled) / sum(total_enrolments)),
         unauth_not_yet_perc = (sum(unauth_not_yet_perc_scaled) / sum(total_enrolments)),
         auth_religious_perc = (sum(auth_religious_perc_scaled) / sum(total_enrolments)),
         auth_study_perc = (sum(auth_study_perc_scaled) / sum(total_enrolments)),
         auth_grt_perc = (sum(auth_grt_perc_scaled) / sum(total_enrolments)),
         auth_holiday_perc = (sum(auth_holiday_perc_scaled) / sum(total_enrolments)),
         auth_excluded_perc = (sum(auth_excluded_perc_scaled) / sum(total_enrolments)),
         auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments))
  )

#Add total onto Primary, Secondary, Special data
attendance_data <- rbind(attendance_data, attendance_data_daily_totals, attendance_data_weekly_totals, attendance_data_ytd_totals, fill = TRUE)

#
geog_lookup <- attendance_data_raw %>%
  select(geographic_level, region_name, la_name) %>%
  unique() %>%
  arrange(region_name)

# Notes tables----------------------------------

notesTableHeadlines <- fread("data/Tech_guidance_headlines.csv")
notesTableReasons <- fread("data/Tech_guidance_reasons.csv")
notesTableLa <- fread("data/Tech_guidance_la.csv")

geog_levels <- geog_lookup %>%
  select(geographic_level) %>%
  unique() %>%
  as.data.table()


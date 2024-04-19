run_data_update <- function() {
  # Run this to update the comparison EES data files for QA checks.
  # The input is the attendance_data df produced in global.R, so you'll need to
  # source global.R before running this script.

  #### SECTION 1 - Databricks connections and processing functions ####
  conn <- odbcConnect("Attendance Project Warehouse")
  sqlQuery(conn, "USE CATALOG catalog_40_copper")

  # school_freq_count <- fread("data/enrolments_schools_denominator_010224.csv")
  school_freq_count <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.enrolments_schools_denominator")
  school_freq_count$total_enrolments <- as.numeric(school_freq_count$total_enrolments)

  pa_fullyear_file <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.ytd_2324_pa_clean_delta")

  pa_autumn_file <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.aut_2324_pa_clean_delta")
  pa_spring_file <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.spr_2324_pa_clean_delta")
  # pa_summer_file <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.sum_2324_pa_clean_delta")

  attendance_data_raw <- sqlQuery(conn, "SELECT * FROM school_attendance_national_stats.ytd_2324_oa_clean_delta")

  attendance_data <- process_attendance_data(
    attendance_data_raw,
    start_date, end_date,
    pa_fullyear_file
  )

  # Write out dashboard data for the dashboard to use
  write.csv(attendance_data, "data/attendance_data_dashboard.csv", row.names = FALSE)

  # # Process and write out further data for EES tables
  attendance_data_autumn <- process_attendance_data_autumn(
    attendance_data_raw,
    autumn_start, autumn_end,
    pa_autumn_file
  )

  # Process and write out further data for EES tables
  attendance_data_spring <- process_attendance_data_spring(
    attendance_data_raw,
    spring_start, spring_end,
    pa_spring_file
  )
  #
  # # Process and write out further data for EES tables
  # attendance_data_summer <- process_attendance_data_summer(
  #   attendance_data_raw,
  #   summer_start, summer_end,
  #   pa_summer_file
  # )

  create_ees_tables(attendance_data)
  create_ees_tables_autumn(attendance_data_autumn)
  create_ees_tables_spring(attendance_data_spring)
  # create_ees_tables_summer(attendance_data_summer)
}

#### SECTION 2 - Processing daily, weekly and year to date ####
process_attendance_data <- function(attendance_data_raw, start_date, end_date, pa_fullyear_file) {
  # Set up data for use across the app
  # Take the raw data and make columns numeric and filter to only Primary, Secondary and Special
  message(paste("Processing attendance data,", Sys.time()))
  pa_data_raw <- pa_fullyear_file
  attendance_data <- attendance_data_raw %>%
    mutate(across(.cols = 15:51, .fns = as.numeric)) %>%
    mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>%
    mutate(across(.cols = 1:2, .fns = as.numeric)) %>%
    mutate(across(.cols = 12:12, .fns = as.numeric)) %>%
    arrange(time_period, time_identifier) %>%
    filter(school_type %in% c("Primary", "Secondary", "Special"))

  # Calculate date
  attendance_data <- attendance_data %>% mutate(attendance_date = as.Date(attendance_date, format = "%d/%m/%Y"))
  attendance_data <- arrange(attendance_data, time_identifier, attendance_date)
  attendance_data <- attendance_data %>% dplyr::filter(between(attendance_date, start_date, end_date))
  # attendance_data <- attendance_data %>% dplyr::filter(attendance_date != funeral_date)
  attendance_data <- attendance_data %>% mutate(week_commencing = as.Date(week_commencing, format = "%d/%m/%Y"))

  # Join school frequency count for proportion of schools reporting and pupil headcount for calculation of weighted totals
  attendance_data <- left_join(attendance_data, dplyr::select(school_freq_count, c(geographic_level, region_name, la_name, phase, total_num_schools, total_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "phase"))

  # Calculate measures for use across app, grouping appropriately at each level then binding back together
  attendance_data_daily <- attendance_data %>%
    mutate(
      attendance_perc = (overall_attendance / possible_sessions) * 100,
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
      breakdown = "Daily"
    )

  attendance_data_weekly <- attendance_data %>%
    group_by(time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
    mutate(
      num_schools = mean(num_schools),
      enrolments = mean(enrolments),
      present_sessions = sum(present_sessions),
      overall_attendance = sum(overall_attendance),
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
      reason_not_attending_planned_closed = sum(reason_not_attending_planned_closed),
      reason_y_not_attending_enforced_closure = sum(reason_y_not_attending_enforced_closure),
      reason_z_not_attending_not_on_roll = sum(reason_z_not_attending_not_on_roll),
      reason_f_legacy_family_holiday = sum(reason_f_legacy_family_holiday),
      total_num_schools = mean(total_num_schools),
      total_enrolments = mean(total_enrolments),
      attendance_perc = (sum(overall_attendance) / sum(possible_sessions)) * 100,
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
      breakdown = "Weekly"
    ) %>%
    distinct(time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all = TRUE)

  attendance_data_ytd <- attendance_data %>%
    group_by(academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
    mutate(
      academic_year = min(academic_year),
      time_period = min(time_period),
      time_identifier = 37,
      num_schools = mean(num_schools),
      enrolments = mean(enrolments),
      present_sessions = sum(present_sessions),
      overall_attendance = sum(overall_attendance),
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
      reason_not_attending_planned_closed = sum(reason_not_attending_planned_closed),
      reason_y_not_attending_enforced_closure = sum(reason_y_not_attending_enforced_closure),
      reason_z_not_attending_not_on_roll = sum(reason_z_not_attending_not_on_roll),
      reason_f_legacy_family_holiday = sum(reason_f_legacy_family_holiday),
      total_num_schools = mean(total_num_schools),
      total_enrolments = mean(total_enrolments),
      attendance_perc = (sum(overall_attendance) / sum(possible_sessions)) * 100,
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
      breakdown = "YTD"
    ) %>%
    distinct(academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all = TRUE)

  # If prior to week 2 publication, comment the line below out
  attendance_data_ytd <- left_join(attendance_data_ytd, dplyr::select(pa_data_raw, c(geographic_level, region_name, la_name, school_type, pa_flag, ytd_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "school_type"))

  attendance_data <- bind_rows(attendance_data_daily, attendance_data_weekly, attendance_data_ytd)

  # If prior to week 2 publication, comment the line below out
  attendance_data <- attendance_data %>% mutate(pa_perc = (pa_flag / ytd_enrolments) * 100)

  # Prep for calculation of totals by doing rates X census counts
  attendance_data <- attendance_data %>%
    mutate(
      attendance_perc_scaled = attendance_perc * total_enrolments,
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
      auth_other_perc_scaled = auth_other_perc * total_enrolments,
      pa_perc_scaled = pa_perc * total_enrolments
    )

  # Calculate total as (Primary rate X primary census count) + (Secondary rate X secondary census count) + (Special rate X special census count) and divided all by total census count
  attendance_data_daily_totals <- attendance_data %>%
    filter(breakdown == "Daily") %>%
    group_by(breakdown, time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, attendance_date, day_number) %>%
    summarise(across(where(is.numeric), sum), .groups = "keep") %>%
    mutate(
      school_type = "Total",
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
      auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
      pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
    )

  attendance_data_weekly_totals <- attendance_data %>%
    filter(breakdown == "Weekly") %>%
    group_by(breakdown, time_period, time_identifier, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
    summarise(across(matches("attendance_date"), ~ min(.x, na.rm = T)),
      across(matches("day_number"), ~ min(.x, na.rm = T)),
      across(matches("week_commencing"), ~ min(.x, na.rm = T)),
      across(where(is.numeric) & !c(attendance_date, day_number), ~ sum(.x, na.rm = T)),
      .groups = "keep"
    ) %>%
    mutate(
      school_type = "Total",
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
      auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
      pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
    )

  attendance_data_ytd_totals <- attendance_data %>%
    filter(breakdown == "YTD") %>%
    group_by(breakdown, academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
    summarise(across(matches("time_period"), ~ min(.x, na.rm = T)),
      across(matches("time_identifier"), ~ min(.x, na.rm = T)),
      across(matches("attendance_date"), ~ min(.x, na.rm = T)),
      across(matches("week_commencing"), ~ min(.x, na.rm = T)),
      across(matches("day_number"), ~ min(.x, na.rm = T)),
      across(where(is.numeric) & !c(time_identifier, attendance_date, day_number), ~ sum(.x, na.rm = T)),
      .groups = "keep"
    ) %>%
    mutate(
      school_type = "Total",
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
      auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
      pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
    )

  # Add total onto Primary, Secondary, Special data
  attendance_data <- bind_rows(attendance_data, attendance_data_daily_totals, attendance_data_weekly_totals, attendance_data_ytd_totals)

  attendance_data <- attendance_data %>%
    dplyr::filter(!(geographic_level == "Local authority" & school_type == "Total")) %>%
    arrange(time_period, time_identifier)

  # #Handle strike days
  # attendance_data <- attendance_data %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_1)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_2)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_3)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_4)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_5)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_6)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == strike_date_7)) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_1 & region_name %in% c("North East", "North West", "Yorkshire and The Humber"))) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_2 & region_name %in% c("East Midlands", "West Midlands", "East of England"))) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_2 & la_name %in% c("Buckinghamshire", "Milton Keynes"))) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_3 & region_name %in% c("London", "South East", "South West"))) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_1 & geographic_level =="National")) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_2 & geographic_level =="National")) %>%
  #   dplyr::filter(!(breakdown == "Daily" & attendance_date == regional_strike_3 & geographic_level =="National"))

  # Data suppression
  attendance_data <- attendance_data %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      reason_not_attending_planned_closed,
      reason_y_not_attending_enforced_closure,
      reason_z_not_attending_not_on_roll,
      reason_f_legacy_family_holiday,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_flag,
      ytd_enrolments,
      pa_perc,
      attendance_perc_scaled,
      overall_absence_perc_scaled,
      authorised_absence_perc_scaled,
      unauthorised_absence_perc_scaled,
      illness_perc_scaled,
      appointments_perc_scaled,
      excluded_perc_scaled,
      unauth_hol_perc_scaled,
      unauth_oth_perc_scaled,
      unauth_late_registers_closed_perc_scaled,
      unauth_not_yet_perc_scaled,
      auth_religious_perc_scaled,
      auth_study_perc_scaled,
      auth_grt_perc_scaled,
      auth_holiday_perc_scaled,
      auth_excluded_perc_scaled,
      auth_other_perc_scaled,
      pa_perc_scaled
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, NA))

  # return(
  #   list(
  #     attendance_data=attendance_data,
  #     daily_totals=attendance_data_daily_totals,
  #     weekly_totals=attendance_data_weekly_totals,
  #     ytd_totals=attendance_data_ytd_totals)
  # )
}

#### SECTION 3 - Processing Autumn ####
process_attendance_data_autumn <- function(attendance_data_raw, autumn_start, autumn_end, pa_autumn_file) {
  # Set up data for use across the app
  # Take the raw data and make columns numeric and filter to only Primary, Secondary and Special
  message(paste("Processing Autumn attendance data,", Sys.time()))
  autumn_only_pa_data_raw <- pa_autumn_file
  attendance_data_autumn <- attendance_data_raw %>%
    mutate(across(.cols = 15:51, .fns = as.numeric)) %>%
    mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>%
    mutate(across(.cols = 1:2, .fns = as.numeric)) %>%
    mutate(across(.cols = 12:12, .fns = as.numeric)) %>%
    arrange(time_period, time_identifier) %>%
    filter(school_type %in% c("Primary", "Secondary", "Special"))

  # Calculate date
  attendance_data_autumn <- attendance_data_autumn %>% mutate(attendance_date = as.Date(attendance_date, format = "%d/%m/%Y"))
  attendance_data_autumn <- arrange(attendance_data_autumn, time_identifier, attendance_date)
  attendance_data_autumn <- attendance_data_autumn %>% dplyr::filter(between(attendance_date, autumn_start, autumn_end))
  attendance_data_autumn <- attendance_data_autumn %>% mutate(week_commencing = as.Date(week_commencing, format = "%d/%m/%Y"))

  # Join school frequency count for proportion of schools reporting and pupil headcount for calculation of weighted totals
  attendance_data_autumn <- left_join(attendance_data_autumn, dplyr::select(school_freq_count, c(geographic_level, region_name, la_name, phase, total_num_schools, total_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "phase"))

  # Calculate measures for use across app, grouping appropriately at each level then binding back together
  attendance_data_autumn <- attendance_data_autumn %>%
    group_by(academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
    mutate(
      academic_year = min(academic_year),
      time_period = min(time_period),
      time_identifier = 37,
      week_commencing = as.Date("2023-09-11"),
      num_schools = mean(num_schools),
      enrolments = mean(enrolments),
      present_sessions = sum(present_sessions),
      overall_attendance = sum(overall_attendance),
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
      reason_not_attending_planned_closed = sum(reason_not_attending_planned_closed),
      reason_y_not_attending_enforced_closure = sum(reason_y_not_attending_enforced_closure),
      reason_z_not_attending_not_on_roll = sum(reason_z_not_attending_not_on_roll),
      reason_f_legacy_family_holiday = sum(reason_f_legacy_family_holiday),
      total_num_schools = mean(total_num_schools),
      total_enrolments = mean(total_enrolments),
      attendance_perc = (sum(overall_attendance) / sum(possible_sessions)) * 100,
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
      breakdown = "AUT"
    ) %>%
    distinct(time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all = TRUE)

  attendance_data_autumn <- left_join(attendance_data_autumn, dplyr::select(autumn_only_pa_data_raw, c(geographic_level, region_name, la_name, school_type, pa_flag, autumn_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "school_type"))
  attendance_data_autumn <- attendance_data_autumn %>% mutate(pa_perc = (pa_flag / autumn_enrolments) * 100)

  # Prep for calculation of totals by doing rates X census counts
  attendance_data_autumn <- attendance_data_autumn %>%
    mutate(
      attendance_perc_scaled = attendance_perc * total_enrolments,
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
      auth_other_perc_scaled = auth_other_perc * total_enrolments,
      pa_perc_scaled = pa_perc * total_enrolments
    )

  attendance_data_autumn_totals <- attendance_data_autumn %>%
    filter(breakdown == "AUT") %>%
    group_by(breakdown, time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
    summarise(across(matches("time_period"), ~ min(.x, na.rm = T)),
      across(matches("time_identifier"), ~ min(.x, na.rm = T)),
      across(matches("attendance_date"), ~ min(.x, na.rm = T)),
      across(matches("week_commencing"), ~ min(.x, na.rm = T)),
      across(matches("day_number"), ~ min(.x, na.rm = T)),
      across(where(is.numeric) & !c(time_identifier, attendance_date, day_number), ~ sum(.x, na.rm = T)),
      .groups = "keep"
    ) %>%
    mutate(
      school_type = "Total",
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
      auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
      pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
    )

  # Add total onto Primary, Secondary, Special data
  attendance_data_autumn <- bind_rows(attendance_data_autumn, attendance_data_autumn_totals)

  attendance_data_autumn <- attendance_data_autumn %>%
    dplyr::filter(!(geographic_level == "Local authority" & school_type == "Total")) %>%
    arrange(time_period, time_identifier)

  # Data suppression
  attendance_data_autumn <- attendance_data_autumn %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      reason_not_attending_planned_closed,
      reason_y_not_attending_enforced_closure,
      reason_z_not_attending_not_on_roll,
      reason_f_legacy_family_holiday,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_flag,
      autumn_enrolments,
      pa_perc,
      attendance_perc_scaled,
      overall_absence_perc_scaled,
      authorised_absence_perc_scaled,
      unauthorised_absence_perc_scaled,
      illness_perc_scaled,
      appointments_perc_scaled,
      excluded_perc_scaled,
      unauth_hol_perc_scaled,
      unauth_oth_perc_scaled,
      unauth_late_registers_closed_perc_scaled,
      unauth_not_yet_perc_scaled,
      auth_religious_perc_scaled,
      auth_study_perc_scaled,
      auth_grt_perc_scaled,
      auth_holiday_perc_scaled,
      auth_excluded_perc_scaled,
      auth_other_perc_scaled,
      pa_perc_scaled
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, NA))

  # return(
  #   list(
  #     attendance_data_autumn=attendance_data_autumn,
  #     autumn_totals=attendance_data_autumn_totals)
  # )
}

#### SECTION 4 - Processing Spring ####
process_attendance_data_spring <- function(attendance_data_raw, spring_start, spring_end, pa_spring_file) {
  # Set up data for use across the app
  # Take the raw data and make columns numeric and filter to only Primary, Secondary and Special
  message(paste("Processing Spring attendance data,", Sys.time()))
  spring_only_pa_data_raw <- pa_spring_file
  attendance_data_spring <- attendance_data_raw %>%
    mutate(across(.cols = 15:51, .fns = as.numeric)) %>%
    mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>%
    mutate(across(.cols = 1:2, .fns = as.numeric)) %>%
    mutate(across(.cols = 12:12, .fns = as.numeric)) %>%
    arrange(time_period, time_identifier) %>%
    filter(school_type %in% c("Primary", "Secondary", "Special"))

  # Calculate date
  attendance_data_spring <- attendance_data_spring %>% mutate(attendance_date = as.Date(attendance_date, format = "%d/%m/%Y"))
  attendance_data_spring <- arrange(attendance_data_spring, time_identifier, attendance_date)
  attendance_data_spring <- attendance_data_spring %>% dplyr::filter(between(attendance_date, spring_start, spring_end))
  attendance_data_spring <- attendance_data_spring %>% mutate(week_commencing = as.Date(week_commencing, format = "%d/%m/%Y"))

  # Join school frequency count for proportion of schools reporting and pupil headcount for calculation of weighted totals
  attendance_data_spring <- left_join(attendance_data_spring, dplyr::select(school_freq_count, c(geographic_level, region_name, la_name, phase, total_num_schools, total_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "phase"))

  # Calculate measures for use across app, grouping appropriately at each level then binding back together
  attendance_data_spring <- attendance_data_spring %>%
    group_by(academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
    mutate(
      academic_year = min(academic_year),
      time_period = min(time_period),
      time_identifier = 37,
      week_commencing = as.Date("2023-01-03"),
      num_schools = mean(num_schools),
      enrolments = mean(enrolments),
      present_sessions = sum(present_sessions),
      overall_attendance = sum(overall_attendance),
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
      reason_not_attending_planned_closed = sum(reason_not_attending_planned_closed),
      reason_y_not_attending_enforced_closure = sum(reason_y_not_attending_enforced_closure),
      reason_z_not_attending_not_on_roll = sum(reason_z_not_attending_not_on_roll),
      reason_f_legacy_family_holiday = sum(reason_f_legacy_family_holiday),
      total_num_schools = mean(total_num_schools),
      total_enrolments = mean(total_enrolments),
      attendance_perc = (sum(overall_attendance) / sum(possible_sessions)) * 100,
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
      breakdown = "SPR"
    ) %>%
    distinct(time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all = TRUE)

  attendance_data_spring <- left_join(attendance_data_spring, dplyr::select(spring_only_pa_data_raw, c(geographic_level, region_name, la_name, school_type, pa_flag, spring_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "school_type"))
  attendance_data_spring <- attendance_data_spring %>% mutate(pa_perc = (pa_flag / spring_enrolments) * 100)

  # Prep for calculation of totals by doing rates X census counts
  attendance_data_spring <- attendance_data_spring %>%
    mutate(
      attendance_perc_scaled = attendance_perc * total_enrolments,
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
      auth_other_perc_scaled = auth_other_perc * total_enrolments,
      pa_perc_scaled = pa_perc * total_enrolments
    )

  attendance_data_spring_totals <- attendance_data_spring %>%
    filter(breakdown == "SPR") %>%
    group_by(breakdown, time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
    summarise(across(matches("time_period"), ~ min(.x, na.rm = T)),
      across(matches("time_identifier"), ~ min(.x, na.rm = T)),
      across(matches("attendance_date"), ~ min(.x, na.rm = T)),
      across(matches("week_commencing"), ~ min(.x, na.rm = T)),
      across(matches("day_number"), ~ min(.x, na.rm = T)),
      across(where(is.numeric) & !c(time_identifier, attendance_date, day_number), ~ sum(.x, na.rm = T)),
      .groups = "keep"
    ) %>%
    mutate(
      school_type = "Total",
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
      auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
      pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
    )

  # Add total onto Primary, Secondary, Special data
  attendance_data_spring <- bind_rows(attendance_data_spring, attendance_data_spring_totals)
  attendance_data_spring <- attendance_data_spring %>%
    dplyr::filter(!(geographic_level == "Local authority" & school_type == "Total")) %>%
    arrange(time_period, time_identifier)

  # Data suppression
  attendance_data_spring <- attendance_data_spring %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      reason_not_attending_planned_closed,
      reason_y_not_attending_enforced_closure,
      reason_z_not_attending_not_on_roll,
      reason_f_legacy_family_holiday,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_flag,
      spring_enrolments,
      pa_perc,
      attendance_perc_scaled,
      overall_absence_perc_scaled,
      authorised_absence_perc_scaled,
      unauthorised_absence_perc_scaled,
      illness_perc_scaled,
      appointments_perc_scaled,
      excluded_perc_scaled,
      unauth_hol_perc_scaled,
      unauth_oth_perc_scaled,
      unauth_late_registers_closed_perc_scaled,
      unauth_not_yet_perc_scaled,
      auth_religious_perc_scaled,
      auth_study_perc_scaled,
      auth_grt_perc_scaled,
      auth_holiday_perc_scaled,
      auth_excluded_perc_scaled,
      auth_other_perc_scaled,
      pa_perc_scaled
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, NA))

  # return(
  #   list(
  #     attendance_data_spring=attendance_data_spring,
  #     spring_totals=attendance_data_spring_totals)
  # )
}
#
#### SECTION 5 - Processing Summer ####
# process_attendance_data_summer <- function(attendance_data_raw, summer_start, summer_end, pa_summer_file){
#   #Set up data for use across the app
#   #Take the raw data and make columns numeric and filter to only Primary, Secondary and Special
#   message(paste("Processing Summer attendance data,",Sys.time()))
#   summer_only_pa_data_raw <- pa_summer_file
#   attendance_data_summer <- attendance_data_raw %>%
#     mutate(across(.cols = 15:51, .fns = as.numeric)) %>%
#     mutate(time_identifier = str_remove_all(time_identifier, "Week ")) %>%
#     mutate(across(.cols = 1:2, .fns = as.numeric)) %>%
#     mutate(across(.cols = 12:12, .fns = as.numeric)) %>%
#     arrange(time_period, time_identifier) %>%
#     filter(school_type %in% c("Primary", "Secondary", "Special"))
#
#   #Calculate date
#   attendance_data_summer <- attendance_data_summer %>% mutate(attendance_date = as.Date(attendance_date, format = "%d/%m/%Y"))
#   attendance_data_summer <- arrange(attendance_data_summer, time_identifier, attendance_date)
#   attendance_data_summer <- attendance_data_summer %>% dplyr::filter(between(attendance_date, summer_start, summer_end))
#   attendance_data_summer <- attendance_data_summer %>% mutate(week_commencing = as.Date(week_commencing, format = "%d/%m/%Y"))
#
#   #Join school frequency count for proportion of schools reporting and pupil headcount for calculation of weighted totals
#   attendance_data_summer <- left_join(attendance_data_summer, dplyr::select(school_freq_count, c(geographic_level, region_name, la_name, phase, total_num_schools, total_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "phase"))
#
#   #Calculate measures for use across app, grouping appropriately at each level then binding back together
#   attendance_data_summer <- attendance_data_summer %>%
#     group_by(academic_year, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type) %>%
#     mutate(academic_year = min(academic_year),
#            time_period = min(time_period),
#            time_identifier = 37,
#            week_commencing = as.Date("2023-04-01"),
#            num_schools = mean(num_schools),
#            enrolments = mean(enrolments),
#            present_sessions = sum(present_sessions),
#            overall_attendance = sum(overall_attendance),
#            approved_educational_activity = sum(approved_educational_activity),
#            overall_absence = sum(overall_absence),
#            authorised_absence = sum(authorised_absence),
#            unauthorised_absence = sum(unauthorised_absence),
#            late_sessions = sum(late_sessions),
#            possible_sessions = sum(possible_sessions),
#            reason_present_am = sum(reason_present_am),
#            reason_present_pm = sum(reason_present_pm),
#            reason_present = sum(reason_present),
#            reason_l_present_late_before_registers_closed = sum(reason_l_present_late_before_registers_closed),
#            reason_i_authorised_illness = sum(reason_i_authorised_illness),
#            reason_m_authorised_medical_dental = sum(reason_m_authorised_medical_dental),
#            reason_r_authorised_religious_observance = sum(reason_r_authorised_religious_observance),
#            reason_s_authorised_study_leave = sum(reason_s_authorised_study_leave),
#            reason_t_authorised_grt_absence = sum(reason_t_authorised_grt_absence),
#            reason_h_authorised_holiday = sum(reason_h_authorised_holiday),
#            reason_e_authorised_excluded = sum(reason_e_authorised_excluded),
#            reason_c_authorised_other = sum(reason_c_authorised_other),
#            reason_b_aea_education_off_site = sum(reason_b_aea_education_off_site),
#            reason_d_aea_dual_registration = sum(reason_d_aea_dual_registration),
#            reason_j_aea_interview = sum(reason_j_aea_interview),
#            reason_p_aea_approved_sporting_activity = sum(reason_p_aea_approved_sporting_activity),
#            reason_v_aea_educational_visit_trip = sum(reason_v_aea_educational_visit_trip),
#            reason_w_aea_work_experience = sum(reason_w_aea_work_experience),
#            reason_g_unauthorised_holiday = sum(reason_g_unauthorised_holiday),
#            reason_u_unauthorised_late_after_registers_closed = sum(reason_u_unauthorised_late_after_registers_closed),
#            reason_o_other_unauthorised = sum(reason_o_other_unauthorised),
#            reason_n_no_reason_yet = sum(reason_n_no_reason_yet),
#            reason_not_attending_planned_closed = sum(reason_not_attending_planned_closed),
#            reason_y_not_attending_enforced_closure = sum(reason_y_not_attending_enforced_closure),
#            reason_z_not_attending_not_on_roll = sum(reason_z_not_attending_not_on_roll),
#            reason_f_legacy_family_holiday = sum(reason_f_legacy_family_holiday),
#            total_num_schools = mean(total_num_schools),
#            total_enrolments = mean(total_enrolments),
#            attendance_perc = (sum(overall_attendance) / sum(possible_sessions)) * 100,
#            overall_absence_perc = (sum(overall_absence) / sum(possible_sessions)) * 100,
#            authorised_absence_perc = (sum(authorised_absence) / sum(possible_sessions)) * 100,
#            unauthorised_absence_perc = (sum(unauthorised_absence) / sum(possible_sessions)) * 100,
#            illness_perc = (sum(reason_i_authorised_illness) / sum(possible_sessions)) * 100,
#            appointments_perc = (sum(reason_m_authorised_medical_dental) / sum(possible_sessions)) * 100,
#            excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
#            unauth_hol_perc = (sum(reason_g_unauthorised_holiday) / sum(possible_sessions)) * 100,
#            unauth_oth_perc = (sum(reason_o_other_unauthorised) / sum(possible_sessions)) * 100,
#            unauth_late_registers_closed_perc = (sum(reason_u_unauthorised_late_after_registers_closed) / sum(possible_sessions)) * 100,
#            unauth_not_yet_perc = (sum(reason_n_no_reason_yet) / sum(possible_sessions)) * 100,
#            auth_religious_perc = (sum(reason_r_authorised_religious_observance) / sum(possible_sessions)) * 100,
#            auth_study_perc = (sum(reason_s_authorised_study_leave) / sum(possible_sessions)) * 100,
#            auth_grt_perc = (sum(reason_t_authorised_grt_absence) / sum(possible_sessions)) * 100,
#            auth_holiday_perc = (sum(reason_h_authorised_holiday) / sum(possible_sessions)) * 100,
#            auth_excluded_perc = (sum(reason_e_authorised_excluded) / sum(possible_sessions)) * 100,
#            auth_other_perc = (sum(reason_c_authorised_other) / sum(possible_sessions)) * 100,
#            breakdown = "SUM") %>%
#     distinct(time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code, school_type, .keep_all= TRUE)
#
#   attendance_data_summer <- left_join(attendance_data_summer, dplyr::select(summer_only_pa_data_raw, c(geographic_level, region_name, la_name, school_type, pa_flag, summer_enrolments)), by = c("geographic_level" = "geographic_level", "region_name" = "region_name", "la_name" = "la_name", "school_type" = "school_type"))
#   attendance_data_summer <- attendance_data_summer %>% mutate(pa_perc = (pa_flag/summer_enrolments)*100)
#
#   #Prep for calculation of totals by doing rates X census counts
#   attendance_data_summer <- attendance_data_summer %>%
#     mutate(attendance_perc_scaled = attendance_perc * total_enrolments,
#            overall_absence_perc_scaled = overall_absence_perc * total_enrolments,
#            authorised_absence_perc_scaled = authorised_absence_perc * total_enrolments,
#            unauthorised_absence_perc_scaled = unauthorised_absence_perc * total_enrolments,
#            illness_perc_scaled = illness_perc * total_enrolments,
#            appointments_perc_scaled = appointments_perc * total_enrolments,
#            excluded_perc_scaled = excluded_perc * total_enrolments,
#            unauth_hol_perc_scaled = unauth_hol_perc * total_enrolments,
#            unauth_oth_perc_scaled = unauth_oth_perc * total_enrolments,
#            unauth_late_registers_closed_perc_scaled = unauth_late_registers_closed_perc * total_enrolments,
#            unauth_not_yet_perc_scaled = unauth_not_yet_perc * total_enrolments,
#            auth_religious_perc_scaled = auth_religious_perc * total_enrolments,
#            auth_study_perc_scaled = auth_study_perc * total_enrolments,
#            auth_grt_perc_scaled = auth_grt_perc * total_enrolments,
#            auth_holiday_perc_scaled = auth_holiday_perc * total_enrolments,
#            auth_excluded_perc_scaled = auth_excluded_perc * total_enrolments,
#            auth_other_perc_scaled = auth_other_perc * total_enrolments,
#            pa_perc_scaled = pa_perc * total_enrolments)
#
#   attendance_data_summer_totals <- attendance_data_summer %>%
#     filter(breakdown == "SUM") %>%
#     group_by(breakdown, time_period, geographic_level, country_code, country_name, region_code, region_name, new_la_code, la_name, old_la_code) %>%
#     summarise(across(matches("time_period"), ~ min(.x, na.rm = T)),
#               across(matches("time_identifier"), ~ min(.x, na.rm = T)),
#               across(matches("attendance_date"), ~ min(.x, na.rm = T)),
#               across(matches("week_commencing"), ~ min(.x, na.rm = T)),
#               across(matches("day_number"), ~ min(.x, na.rm = T)),
#               across(where(is.numeric)& !c(time_identifier, attendance_date, day_number), ~ sum(.x, na.rm = T)),
#               .groups="keep") %>%
#     mutate(school_type = "Total",
#            enrolments_pa_10_exact = "z",
#            attendance_perc = (sum(attendance_perc_scaled) / sum(total_enrolments)),
#            overall_absence_perc = (sum(overall_absence_perc_scaled) / sum(total_enrolments)),
#            authorised_absence_perc = (sum(authorised_absence_perc_scaled) / sum(total_enrolments)),
#            unauthorised_absence_perc = (sum(unauthorised_absence_perc_scaled) / sum(total_enrolments)),
#            illness_perc = (sum(illness_perc_scaled) / sum(total_enrolments)),
#            appointments_perc = (sum(appointments_perc_scaled) / sum(total_enrolments)),
#            excluded_perc = (sum(excluded_perc_scaled) / sum(total_enrolments)),
#            unauth_hol_perc = (sum(unauth_hol_perc_scaled) / sum(total_enrolments)),
#            unauth_oth_perc = (sum(unauth_oth_perc_scaled) / sum(total_enrolments)),
#            unauth_late_registers_closed_perc = (sum(unauth_late_registers_closed_perc_scaled) / sum(total_enrolments)),
#            unauth_not_yet_perc = (sum(unauth_not_yet_perc_scaled) / sum(total_enrolments)),
#            auth_religious_perc = (sum(auth_religious_perc_scaled) / sum(total_enrolments)),
#            auth_study_perc = (sum(auth_study_perc_scaled) / sum(total_enrolments)),
#            auth_grt_perc = (sum(auth_grt_perc_scaled) / sum(total_enrolments)),
#            auth_holiday_perc = (sum(auth_holiday_perc_scaled) / sum(total_enrolments)),
#            auth_excluded_perc = (sum(auth_excluded_perc_scaled) / sum(total_enrolments)),
#            auth_other_perc = (sum(auth_other_perc_scaled) / sum(total_enrolments)),
#            pa_perc = (sum(pa_perc_scaled) / sum(total_enrolments))
#     )
#
#   #Add total onto Primary, Secondary, Special data
#   attendance_data_summer <- bind_rows(attendance_data_summer, attendance_data_summer_totals)
#   attendance_data_summer <- attendance_data_summer %>% dplyr::filter(!(geographic_level == "Local authority" & school_type == "Total")) %>% arrange(time_period, time_identifier)
#
#   #Data suppression
#   attendance_data_summer <- attendance_data_summer %>%
#     mutate_at(vars(enrolments,
#                    present_sessions,
#                    overall_attendance,
#                    approved_educational_activity,
#                    overall_absence,
#                    authorised_absence,
#                    unauthorised_absence,
#                    late_sessions,
#                    possible_sessions,
#                    reason_present_am,
#                    reason_present_pm,
#                    reason_present,
#                    reason_l_present_late_before_registers_closed,
#                    reason_i_authorised_illness,
#                    reason_m_authorised_medical_dental,
#                    reason_r_authorised_religious_observance,
#                    reason_s_authorised_study_leave,
#                    reason_t_authorised_grt_absence,
#                    reason_h_authorised_holiday,
#                    reason_e_authorised_excluded,
#                    reason_c_authorised_other,
#                    reason_b_aea_education_off_site,
#                    reason_d_aea_dual_registration,
#                    reason_j_aea_interview,
#                    reason_p_aea_approved_sporting_activity,
#                    reason_v_aea_educational_visit_trip,
#                    reason_w_aea_work_experience,
#                    reason_g_unauthorised_holiday,
#                    reason_u_unauthorised_late_after_registers_closed,
#                    reason_o_other_unauthorised,
#                    reason_n_no_reason_yet,
#                    reason_not_attending_planned_closed,
#                    reason_y_not_attending_enforced_closure,
#                    reason_z_not_attending_not_on_roll,
#                    reason_f_legacy_family_holiday,
#                    total_num_schools,
#                    total_enrolments,
#                    attendance_perc,
#                    overall_absence_perc,
#                    authorised_absence_perc,
#                    unauthorised_absence_perc,
#                    illness_perc,
#                    appointments_perc,
#                    excluded_perc,
#                    unauth_hol_perc,
#                    unauth_oth_perc,
#                    unauth_late_registers_closed_perc,
#                    unauth_not_yet_perc,
#                    auth_religious_perc,
#                    auth_study_perc,
#                    auth_grt_perc,
#                    auth_holiday_perc,
#                    auth_excluded_perc,
#                    auth_other_perc,
#                    pa_flag,
#                    summer_enrolments,
#                    pa_perc,
#                    attendance_perc_scaled,
#                    overall_absence_perc_scaled,
#                    authorised_absence_perc_scaled,
#                    unauthorised_absence_perc_scaled,
#                    illness_perc_scaled,
#                    appointments_perc_scaled,
#                    excluded_perc_scaled,
#                    unauth_hol_perc_scaled,
#                    unauth_oth_perc_scaled,
#                    unauth_late_registers_closed_perc_scaled,
#                    unauth_not_yet_perc_scaled,
#                    auth_religious_perc_scaled,
#                    auth_study_perc_scaled,
#                    auth_grt_perc_scaled,
#                    auth_holiday_perc_scaled,
#                    auth_excluded_perc_scaled,
#                    auth_other_perc_scaled,
#                    pa_perc_scaled), ~
#                 replace(., geographic_level == "Local authority" & num_schools == 1, NA))
#
#   # return(
#   #   list(
#   #     attendance_data_spring=attendance_data_spring,
#   #     spring_totals=attendance_data_spring_totals)
#   # )
# }

#### SECTION 6 - Creating EES tables for daily, weekly and year to date ####
create_ees_tables <- function(attendance_data) {
  # Set up data for download
  # EES daily data/download data
  EES_daily_data <- attendance_data %>%
    dplyr::filter(breakdown == "Daily") %>%
    dplyr::select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      region_code,
      region_name,
      new_la_code,
      la_name,
      old_la_code,
      attendance_date,
      school_type,
      num_schools,
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc
    ) %>%
    arrange(time_period, time_identifier, school_type) %>%
    mutate(time_identifier = paste("Week", time_identifier, sep = " ")) %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, "c"))

  # EES_daily_data[is.na(EES_daily_data)]<-"c"

  # EES weekly data
  EES_weekly_data <- attendance_data %>%
    dplyr::filter(breakdown == "Weekly") %>%
    dplyr::select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      region_code,
      region_name,
      new_la_code,
      la_name,
      old_la_code,
      school_type,
      num_schools,
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc
    ) %>%
    arrange(time_period, time_identifier, school_type) %>%
    mutate(time_identifier = paste("Week", time_identifier, sep = " ")) %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, "c"))

  # EES_weekly_data[is.na(EES_weekly_data)]<-"c"

  # EES ytd data
  EES_ytd_data <- attendance_data %>%
    dplyr::filter(breakdown == "YTD") %>%
    dplyr::select(
      time_period,
      geographic_level,
      country_code,
      country_name,
      region_code,
      region_name,
      new_la_code,
      la_name,
      old_la_code,
      school_type,
      num_schools,
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ) %>%
    arrange(time_period, school_type) %>%
    mutate(
      time_identifier = paste("Academic year"),
      time_period = paste("202324")
    ) %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, "c"))

  # EES_ytd_data[is.na(EES_ytd_data)]<-"c"

  write.csv(EES_daily_data, "data\\EES_daily_data.csv", row.names = FALSE)
  write.csv(EES_weekly_data, "data\\EES_weekly_data.csv", row.names = FALSE)
  write.csv(EES_ytd_data, "data\\EES_ytd_data.csv", row.names = FALSE)
}

#### SECTION 7 - Creating EES table for Autumn ####
create_ees_tables_autumn <- function(df_attendance_autumn) {
  # Set up data for download
  # EES ytd data
  EES_aut_data <- df_attendance_autumn %>%
    dplyr::filter(breakdown == "AUT") %>%
    dplyr::select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      region_code,
      region_name,
      new_la_code,
      la_name,
      old_la_code,
      school_type,
      num_schools,
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ) %>%
    arrange(time_period, school_type) %>%
    mutate(
      time_identifier = paste("Autumn term"),
      time_period = paste("202324"),
      academic_year = paste("202324")
    ) %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, "c"))

  # EES_aut_data[is.na(EES_aut_data)]<-"c"

  write.csv(EES_aut_data, "data\\EES_aut_data.csv", row.names = FALSE)
}

#### SECTION 8 - Creating EES table for Spring ####
create_ees_tables_spring <- function(df_attendance_spring) {
  # Set up data for download
  # EES ytd data
  EES_spr_data <- df_attendance_spring %>%
    dplyr::filter(breakdown == "SPR") %>%
    dplyr::select(
      time_period,
      time_identifier,
      geographic_level,
      country_code,
      country_name,
      region_code,
      region_name,
      new_la_code,
      la_name,
      old_la_code,
      school_type,
      num_schools,
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ) %>%
    arrange(time_period, school_type) %>%
    mutate(
      time_identifier = paste("Spring term"),
      time_period = paste("202324"),
      academic_year = paste("202324")
    ) %>%
    mutate_at(vars(
      enrolments,
      present_sessions,
      overall_attendance,
      approved_educational_activity,
      overall_absence,
      authorised_absence,
      unauthorised_absence,
      late_sessions,
      possible_sessions,
      reason_present_am,
      reason_present_pm,
      reason_present,
      reason_l_present_late_before_registers_closed,
      reason_i_authorised_illness,
      reason_m_authorised_medical_dental,
      reason_r_authorised_religious_observance,
      reason_s_authorised_study_leave,
      reason_t_authorised_grt_absence,
      reason_h_authorised_holiday,
      reason_e_authorised_excluded,
      reason_c_authorised_other,
      reason_b_aea_education_off_site,
      reason_d_aea_dual_registration,
      reason_j_aea_interview,
      reason_p_aea_approved_sporting_activity,
      reason_v_aea_educational_visit_trip,
      reason_w_aea_work_experience,
      reason_g_unauthorised_holiday,
      reason_u_unauthorised_late_after_registers_closed,
      reason_o_other_unauthorised,
      reason_n_no_reason_yet,
      total_num_schools,
      total_enrolments,
      attendance_perc,
      overall_absence_perc,
      authorised_absence_perc,
      unauthorised_absence_perc,
      illness_perc,
      appointments_perc,
      # excluded_perc,
      unauth_hol_perc,
      unauth_oth_perc,
      unauth_late_registers_closed_perc,
      unauth_not_yet_perc,
      auth_religious_perc,
      auth_study_perc,
      auth_grt_perc,
      auth_holiday_perc,
      auth_excluded_perc,
      auth_other_perc,
      pa_perc
    ), ~
      replace(., geographic_level == "Local authority" & num_schools == 1, "c"))

  # EES_aut_data[is.na(EES_aut_data)]<-"c"

  write.csv(EES_spr_data, "data\\EES_spr_data.csv", row.names = FALSE)
}

#### SECTION 9 - Creating EES table for Summer ####
# create_ees_tables_summer<- function(df_attendance_summer){
#   #Set up data for download
#   #EES ytd data
#   EES_sum_data <- df_attendance_summer %>%
#     dplyr::filter(breakdown == "SUM") %>%
#     dplyr::select(
#       time_period,
#       time_identifier,
#       geographic_level,
#       country_code,
#       country_name,
#       region_code,
#       region_name,
#       new_la_code,
#       la_name,
#       old_la_code,
#       school_type,
#       num_schools,
#       enrolments,
#       present_sessions,
#       overall_attendance,
#       approved_educational_activity,
#       overall_absence,
#       authorised_absence,
#       unauthorised_absence,
#       late_sessions,
#       possible_sessions,
#       reason_present_am,
#       reason_present_pm,
#       reason_present,
#       reason_l_present_late_before_registers_closed,
#       reason_i_authorised_illness,
#       reason_m_authorised_medical_dental,
#       reason_r_authorised_religious_observance,
#       reason_s_authorised_study_leave,
#       reason_t_authorised_grt_absence,
#       reason_h_authorised_holiday,
#       reason_e_authorised_excluded,
#       reason_c_authorised_other,
#       reason_b_aea_education_off_site,
#       reason_d_aea_dual_registration,
#       reason_j_aea_interview,
#       reason_p_aea_approved_sporting_activity,
#       reason_v_aea_educational_visit_trip,
#       reason_w_aea_work_experience,
#       reason_g_unauthorised_holiday,
#       reason_u_unauthorised_late_after_registers_closed,
#       reason_o_other_unauthorised,
#       reason_n_no_reason_yet,
#       total_num_schools,
#       total_enrolments,
#       attendance_perc,
#       overall_absence_perc,
#       authorised_absence_perc,
#       unauthorised_absence_perc,
#       illness_perc,
#       appointments_perc,
#       #excluded_perc,
#       unauth_hol_perc,
#       unauth_oth_perc,
#       unauth_late_registers_closed_perc,
#       unauth_not_yet_perc,
#       auth_religious_perc,
#       auth_study_perc,
#       auth_grt_perc,
#       auth_holiday_perc,
#       auth_excluded_perc,
#       auth_other_perc,
#       pa_perc
#     ) %>%
#     arrange(time_period, school_type) %>%
#     mutate(time_identifier = paste("Summer term"),
#            time_period = paste("202324"),
#            academic_year = paste("202324")) %>%
#     mutate_at(vars(enrolments,
#                    present_sessions,
#                    overall_attendance,
#                    approved_educational_activity,
#                    overall_absence,
#                    authorised_absence,
#                    unauthorised_absence,
#                    late_sessions,
#                    possible_sessions,
#                    reason_present_am,
#                    reason_present_pm,
#                    reason_present,
#                    reason_l_present_late_before_registers_closed,
#                    reason_i_authorised_illness,
#                    reason_m_authorised_medical_dental,
#                    reason_r_authorised_religious_observance,
#                    reason_s_authorised_study_leave,
#                    reason_t_authorised_grt_absence,
#                    reason_h_authorised_holiday,
#                    reason_e_authorised_excluded,
#                    reason_c_authorised_other,
#                    reason_b_aea_education_off_site,
#                    reason_d_aea_dual_registration,
#                    reason_j_aea_interview,
#                    reason_p_aea_approved_sporting_activity,
#                    reason_v_aea_educational_visit_trip,
#                    reason_w_aea_work_experience,
#                    reason_g_unauthorised_holiday,
#                    reason_u_unauthorised_late_after_registers_closed,
#                    reason_o_other_unauthorised,
#                    reason_n_no_reason_yet,
#                    total_num_schools,
#                    total_enrolments,
#                    attendance_perc,
#                    overall_absence_perc,
#                    authorised_absence_perc,
#                    unauthorised_absence_perc,
#                    illness_perc,
#                    appointments_perc,
#                    #excluded_perc,
#                    unauth_hol_perc,
#                    unauth_oth_perc,
#                    unauth_late_registers_closed_perc,
#                    unauth_not_yet_perc,
#                    auth_religious_perc,
#                    auth_study_perc,
#                    auth_grt_perc,
#                    auth_holiday_perc,
#                    auth_excluded_perc,
#                    auth_other_perc,
#                    pa_perc), ~
#                 replace(., geographic_level == "Local authority" & num_schools == 1, "c"))
#
#   #EES_aut_data[is.na(EES_aut_data)]<-"c"
#
#   write.csv(EES_sum_data, "data\\EES_sum_data.csv", row.names = FALSE)
#
# }

#### SECTION 10 - Using daily data to read in for app ####
read_ees_daily <- function() {
  read.csv("data/EES_daily_data.csv", stringsAsFactors = FALSE)
}

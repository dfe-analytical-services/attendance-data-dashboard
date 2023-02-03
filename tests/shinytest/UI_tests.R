app <- ShinyDriver$new("../../", loadTimeout = 1.e7)
app$snapshotInit("UI_tests", screenshot = FALSE)



listInputs <- c(
  "navlistPanel",
  "dash",
  "ts_choice",
  "school_choice",
  "geography_choice",
  "region_choice",
  "la_choice",
  "link_to_coverage",
  "link_to_headlines_tab",
  "link_to_la_tab",
  "link_to_reasons_tab",
  "link_to_technotes"
)


# Note - if timeout errors keep occurring, can include app$setInputs(argument 1, timeout_ = 1e+4)
# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
message("Test 1")
Sys.sleep(1)
app$snapshot()


# Checking headlines tab
# 2. Is the default setting National, Total?  --------------------------------------------
message("Test 2")
app$setInputs(navlistPanel = "dashboard", dash = "headlines")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_nat",
    "school_count_proportion",
    "weekly_attendance_rate_nat",
    "weekly_absence_rate_nat",
    "weekly_illness_rate_nat",
    "absence_rates_daily_plot"
  )
))

# 3. If phase is changed do outputs change?  --------------------------------------------
message("Test 3")
app$setInputs(navlistPanel = "dashboard", dash = "headlines", school_choice = "Secondary")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_nat",
    "school_count_proportion",
    "weekly_attendance_rate_nat",
    "weekly_absence_rate_nat",
    "weekly_illness_rate_nat",
    "absence_rates_daily_plot"
  )
))

# 4. If the time series is changed from default most recent week to year to date, are outputs produced?  --------------------------------------------
message("Test 4")
app$setInputs(ts_choice = "Year to date")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_nat",
    "school_count_proportion",
    "ytd_attendance_rate_nat",
    "ytd_absence_rate_nat",
    "ytd_illness_rate_nat",
    "absence_rates_timeseries_plot"
  )
))

# 5. If the geography is changed to regional, does this autofill with East Midlands and do outputs change?  --------------------------------------------
message("Test 5")
app$setInputs(geography_choice = "Regional")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_reg",
    "school_count_proportion",
    "ytd_attendance_rate_reg",
    "ytd_absence_rate_reg",
    "ytd_illness_rate_reg",
    "absence_rates_timeseries_plot"
  )
))

# 6. If the geography is changed to local authority, does this autofill and do outputs change?  --------------------------------------------
message("Test 6")
app$setInputs(geography_choice = "Local authority")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_la",
    "school_count_proportion",
    "ytd_attendance_rate_la",
    "ytd_absence_rate_la",
    "ytd_illness_rate_la",
    "absence_rates_timeseries_plot"
  )
))

# 7. If the LA is changed, do outputs change?  --------------------------------------------
message("Test 7")
app$setInputs(la_choice = "Lincolnshire")
app$snapshot(list(
  input = listInputs,
  output = c(
    "headline_bullet_title_la",
    "school_count_proportion",
    "ytd_attendance_rate_la",
    "ytd_absence_rate_la",
    "ytd_illness_rate_la",
    "absence_rates_timeseries_plot"
  )
))

# Checking reasons tab
# 8. Does the reasons tab load?  --------------------------------------------
message("Test 8")
app$setInputs(navlistPanel = "dashboard", dash = "reasons", school_choice = "Primary", geography_choice = "National")
app$snapshot(list(
  input = listInputs,
  output = c(
    "reasons_chart_title_nat",
    "absence_reasons_timeseries_plot",
    "headline_auth_rate_weekly",
    "headline_auth_rate_ytd",
    "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd",
    "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  )
))

# 9. If phase is changed do outputs change?  --------------------------------------------
message("Test 9")
app$setInputs(school_choice = "Secondary")
app$snapshot(list(
  input = listInputs,
  output = c(
    "reasons_chart_title_nat",
    "absence_reasons_timeseries_plot",
    "headline_auth_rate_weekly",
    "headline_auth_rate_ytd",
    "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd",
    "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  )
))

# 10. If the geography is changed to regional, does this autofill with East Midlands and do outputs change?  --------------------------------------------
message("Test 10")
app$setInputs(geography_choice = "Regional")
Sys.sleep(4)
app$snapshot(list(
  input = listInputs,
  output = c(
    "reasons_chart_title_reg",
    "absence_reasons_timeseries_plot",
    "headline_auth_rate_weekly",
    "headline_auth_rate_ytd",
    "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd",
    "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  )
))

# 11. If the geography is changed to local authority, does this autofill and do outputs change?  --------------------------------------------
message("Test 11")
app$setInputs(geography_choice = "Local authority")
Sys.sleep(4)
app$snapshot(list(
  input = listInputs,
  output = c(
    "reasons_chart_title_la",
    "absence_reasons_timeseries_plot",
    "headline_auth_rate_weekly",
    "headline_auth_rate_ytd",
    "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd",
    "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  )
))

# 12. If the LA is changed, do outputs change?  --------------------------------------------
message("Test 12")
app$setInputs(la_choice = "Lincolnshire")
Sys.sleep(4)
app$snapshot(list(
  input = listInputs,
  output = c(
    "reasons_chart_title_la",
    "absence_reasons_timeseries_plot",
    "headline_auth_rate_weekly",
    "headline_auth_rate_ytd",
    "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd",
    "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  )
))

# Checking la comparisons tab
# 13. Does the la comparisons tab load?  --------------------------------------------
message("Test 13")
app$setInputs(navlistPanel = "dashboard", dash = "la comparisons", school_choice = "Primary", timeout_ = 1.2e4)
app$snapshot(list(
  input = listInputs,
  output = c(
    "absence_reasons_la_table"
  )
))

# 14. If phase is changed do outputs change?  --------------------------------------------
message("Test 14")
app$setInputs(navlistPanel = "dashboard", school_choice = "Secondary", timeout_ = 1.2e4)
app$snapshot(list(
  input = listInputs,
  output = c(
    "absence_reasons_la_table"
  )
))

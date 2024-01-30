library(shinytest2)

test_that("Migrated shinytest test: UI_tests.R", {
  app <- AppDriver$new(load_timeout = 1e+07)



  listInputs <- c(
    "navlistPanel",
    "dash",
    "ts_choice",
    "school_choice",
    "geography_choice",
    "region_choice",
    "la_choice",
    "measure_choice",
    "link_to_coverage",
    "link_to_headlines_tab",
    "link_to_la_tab",
    "link_to_reasons_tab",
    "link_to_technotes"
  )


  # Note - if timeout errors keep occurring, can include app$setInputs(argument 1, timeout_ = 1e+4)
  # 1. Does it load  -------------------------------------------------------------------------------------------------------------------
  message("")
  message("Test 1")
  Sys.sleep(1)
  app$expect_values(input = listInputs, output = c(
    "daily_schools_count",
    "headline_update_date", "homepage_update_dates"
  ))


  # Checking headlines tab
  # 2. Is the default setting National, Primary?  --------------------------------------------
  message("Test 2")
  app$set_inputs(navlistPanel = "dashboard", dash = "headlines")
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_nat",
    "school_count_proportion", "weekly_attendance_rate_nat",
    "weekly_absence_rate_nat", "weekly_illness_rate_nat", "absence_rates_daily_plot"
  ))

  # 3. If phase is changed do outputs change?  --------------------------------------------
  message("Test 3")
  app$set_inputs(
    navlistPanel = "dashboard", dash = "headlines",
    school_choice = "Secondary"
  )
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_nat",
    "school_count_proportion", "weekly_attendance_rate_nat",
    "weekly_absence_rate_nat", "weekly_illness_rate_nat", "absence_rates_daily_plot"
  ))

  # 4. If the time series is changed from default most recent week to year to date, are outputs produced?  --------------------------------------------
  message("Test 4")
  app$set_inputs(ts_choice = "yeartodate")
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_nat",
    "school_count_proportion", "ytd_attendance_rate_nat", "ytd_absence_rate_nat",
    "ytd_illness_rate_nat", "absence_rates_timeseries_plot"
  ))

  # 5. If the geography is changed to regional, does this autofill with East Midlands and do outputs change?  --------------------------------------------
  message("Test 5")
  app$set_inputs(geography_choice = "Regional")
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_reg",
    "school_count_proportion", "ytd_attendance_rate_reg", "ytd_absence_rate_reg",
    "ytd_illness_rate_reg", "absence_rates_timeseries_plot"
  ))

  # 6. If the geography is changed to local authority, does this autofill and do outputs change?  --------------------------------------------
  message("Test 6")
  app$set_inputs(
    geography_choice = "Local authority"
  )
  Sys.sleep(4)
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_la",
    "school_count_proportion", "ytd_attendance_rate_la", "ytd_absence_rate_la",
    "ytd_illness_rate_la", "absence_rates_timeseries_plot"
  ))

  # 7. If the LA is changed, do outputs change?  --------------------------------------------
  message("Test 7")
  app$set_inputs(la_choice = "Lincolnshire")
  app$expect_values(input = listInputs, output = c(
    "headline_bullet_title_la",
    "school_count_proportion", "ytd_attendance_rate_la", "ytd_absence_rate_la",
    "ytd_illness_rate_la", "absence_rates_timeseries_plot"
  ))

  # Checking reasons tab
  # 8. Does the reasons tab load?  --------------------------------------------
  message("Test 8")
  app$set_inputs(
    navlistPanel = "dashboard", dash = "reasons",
    geography_choice = "National"
  )
  app$expect_values(input = listInputs, output = c(
    "reasons_chart_title_nat",
    "absence_reasons_timeseries_plot", "headline_auth_rate_weekly",
    "headline_auth_rate_ytd", "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd", "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  ))

  # 9. If phase is changed do outputs change?  --------------------------------------------
  message("Test 9")
  app$set_inputs(school_choice = "Primary")
  app$expect_values(input = listInputs, output = c(
    "reasons_chart_title_nat",
    "absence_reasons_timeseries_plot", "headline_auth_rate_weekly",
    "headline_auth_rate_ytd", "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd", "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  ))

  # 10. If the geography is changed to regional, does this autofill with East Midlands and do outputs change?  --------------------------------------------
  message("Test 10")
  app$set_inputs(geography_choice = "Regional", timeout_ = 12000)
  Sys.sleep(4)
  app$expect_values(input = listInputs, output = c(
    "reasons_chart_title_reg",
    "absence_reasons_timeseries_plot", "headline_auth_rate_weekly",
    "headline_auth_rate_ytd", "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd", "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  ))

  # 11. If the geography is changed to local authority, does this autofill and do outputs change?  --------------------------------------------
  message("Test 11")
  app$set_inputs(geography_choice = "Local authority")
  Sys.sleep(4)
  app$expect_values(input = listInputs, output = c(
    "reasons_chart_title_la",
    "absence_reasons_timeseries_plot", "headline_auth_rate_weekly",
    "headline_auth_rate_ytd", "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd", "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  ))

  # 12. If the LA is changed, do outputs change?  --------------------------------------------
  message("Test 12")
  app$set_inputs(la_choice = "Nottingham")
  Sys.sleep(4)
  app$expect_values(input = listInputs, output = c(
    "reasons_chart_title_la",
    "absence_reasons_timeseries_plot", "headline_auth_rate_weekly",
    "headline_auth_rate_ytd", "headline_unauth_rate_weekly",
    "headline_unauth_rate_ytd", "absence_auth_reasons_table",
    "absence_unauth_reasons_table"
  ))

  # Checking la comparisons tab
  # 13. Does the la comparisons tab load?  --------------------------------------------
  message("Test 13")
  app$set_inputs(
    navlistPanel = "dashboard", dash = "la comparisons",
    school_choice = "Primary", timeout_ = 12000
  )
  app$expect_values(input = listInputs, output = c("map_title", "absence_reasons_la_table"))

  # 14. If phase is changed do outputs change?  --------------------------------------------
  message("Test 14")
  app$set_inputs(
    navlistPanel = "dashboard", school_choice = "Secondary",
    timeout_ = 12000
  )
  app$expect_values(input = listInputs, output = c("map_title", "absence_reasons_la_table"))
})

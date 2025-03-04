# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Code for controlling cookie consent - do not delete. See dfeshiny guidance for how this works:
  # https://dfe-analytical-services.github.io/dfeshiny/articles/implementing-cookies.html
  # ==============================================================================================
  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )
  # ==============================================================================================

  time_frame_descriptors <- reactive(
    if (input$ts_choice == "latestweeks") {
      list(filter_string = "Week", display_string = "Latest week")
    } else {
      list(filter_string = "Year to date", display_string = "Year to date")
    }
  )



  # Retrieving data from the API
  reasons_data_version_info <- eventReactive(input$geography_choice, {
    eesyapi::get_dataset_versions(
      reasons_dataset_id,
      ees_environment = ees_api_env
    ) |>
      filter(version == max(version))
  })

  reasons_data <- reactive({
    message("Time period choice: ", input$ts_choice)
    if (input$ts_choice == "latestweeks") {
      time_period_query <- reasons_data_version_info()$time_period_end |>
        stringr::str_replace("Week ", "W") |>
        stringr::str_replace(" ", "|")
      time_frame_query <- c(
        reasons_sqids$filters$time_frame$week,
        reasons_sqids$filters$time_frame$monday,
        reasons_sqids$filters$time_frame$tuesday,
        reasons_sqids$filters$time_frame$wednesday,
        reasons_sqids$filters$time_frame$thursday,
        reasons_sqids$filters$time_frame$friday
      )
    } else {
      time_period_query <- NULL
      time_frame_query <- c(
        reasons_sqids$filters$time_frame$week,
        reasons_sqids$filters$time_frame$yeartodate
      )
    }
    eesyapi::query_dataset(
      reasons_dataset_id,
      time_periods = time_period_query,
      geographies = geography_query(input$geography_choice, input$region_choice, input$la_choice),
      filter_items = list(
        time_frame = time_frame_query,
        school_phase = reasons_sqids$filters$education_phase |>
          magrittr::extract2(tolower(input$school_choice) |> str_replace("total", "allschools")),
        attendance_reason = c(
          reasons_sqids$filters$attendance_reason$overallattendance,
          reasons_sqids$filters$attendance_reason$overallabsence,
          reasons_sqids$filters$attendance_reason$allauthorised,
          reasons_sqids$filters$attendance_reason$illness_i,
          reasons_sqids$filters$attendance_reason$religiousobservance_r,
          reasons_sqids$filters$attendance_reason$medicaldental_m,
          reasons_sqids$filters$attendance_reason$studyleave_s,
          reasons_sqids$filters$attendance_reason$mobilechild_t,
          reasons_sqids$filters$attendance_reason$excluded_e,
          reasons_sqids$filters$attendance_reason$temporaryreducedtimetable_c2,
          reasons_sqids$filters$attendance_reason$otherauthorised_c,
          reasons_sqids$filters$attendance_reason$allunauthorised,
          reasons_sqids$filters$attendance_reason$unauthorisedholiday_g,
          reasons_sqids$filters$attendance_reason$lateafterregistersclosed_u,
          reasons_sqids$filters$attendance_reason$otherunauthorised_o,
          reasons_sqids$filters$attendance_reason$noreasonyet_n
        )
      ),
      indicators = unlist(reasons_sqids$indicators, use.names = FALSE),
      ees_environment = ees_api_env,
      verbose = api_verbose
    ) |>
      mutate(
        reference_date = lubridate::ymd(reference_date)
      )
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$ts_choice,
      input$geography_choice,
      input$region_choice,
      input$la_choice,
      input$school_choice
    )

  la_data <- reactive({
    time_period_query <- reasons_data_version_info()$time_period_end |>
      stringr::str_replace("Week ", "W") |>
      stringr::str_replace(" ", "|")
    eesyapi::query_dataset(
      reasons_dataset_id,
      time_periods = time_period_query,
      geographies = "Local authority",
      filter_items = list(
        time_frame = reasons_sqids$filters$time_frame$week,
        school_phase = reasons_sqids$filters$education_phase |>
          magrittr::extract2(tolower(input$school_choice)),
        attendance_reason = c(
          reasons_sqids$filters$attendance_reason$overallabsence,
          reasons_sqids$filters$attendance_reason$allauthorised,
          reasons_sqids$filters$attendance_reason$allunauthorised
        )
      ),
      indicators = c(
        reasons_sqids$indicators$session_percent,
        reasons_sqids$indicators$reference_date
      ),
      ees_environment = ees_api_env,
      verbose = api_verbose
    )
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$school_choice
    )

  pa_data_version_info <- eventReactive(input$geography_choice, {
    eesyapi::get_dataset_versions(
      persistent_absence_dataset_id,
      ees_environment = ees_api_env
    ) |>
      filter(version == max(version))
  })

  pa_data <- reactive({
    eesyapi::query_dataset(
      persistent_absence_dataset_id,
      geographies = geography_query(input$geography_choice, input$region_choice, input$la_choice),
      filter_items = list(
        school_phase = persistent_absence_sqids$filters$education_phase |>
          magrittr::extract2(tolower(input$school_choice) |> str_replace("total", "allschools"))
      ),
      indicators = unlist(persistent_absence_sqids$indicators, use.names = FALSE),
      ees_environment = ees_api_env,
      verbose = api_verbose
    )
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$ts_choice,
      input$geography_choice,
      input$region_choice,
      input$la_choice,
      input$school_choice
    )

  observe({
    print(pa_data())
  })


  map_data <- reactive({
    merge(
      mapshape |> rename("la_code" = "CTYUA23CD"),
      la_data(),
      by = "la_code", duplicateGeoms = TRUE
    ) |>
      mutate(
        session_percent = as.numeric(session_percent),
        label = paste(
          la_name,
          input$measure_choice,
          "absence rate:",
          paste0(as.character(dfeR::round_five_up(session_percent, dp = 1)), "%")
        )
      )
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$school_choice
    )

  time_frame_string <- reactive({
    print(reasons_data())
    if (input$ts_choice == "latestweeks") {
      dates <- reasons_data() |>
        filter(time_frame != "Week") |>
        pull(reference_date)
      dates_minmax <- c(
        dates |> min(),
        dates |> max()
      )
      paste0(
        "latest week (",
        paste0(
          dates_minmax |>
            lubridate::ymd() |>
            date_stamp(),
          collapse = " to "
        ),
        ")"
      )
    } else {
      dates <- reasons_data() |>
        filter(time_frame != "Year to date") |>
        pull(reference_date)
      dates_minmax <- c(
        dates |> min(),
        dates |> max()
      )
      paste0(
        "year to date (",
        paste0(
          dates_minmax |>
            lubridate::ymd() |>
            date_stamp(),
          collapse = " to "
        ),
        ")"
      )
    }
  })

  # Navigation with links
  observeEvent(input$link_to_headlines_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dash", selected = "headlines")
  })

  observeEvent(input$link_to_reasons_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dash", selected = "reasons")
  })

  observeEvent(input$link_to_la_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
    updateTabsetPanel(session, "dash", selected = "la comparisons")
  })

  observeEvent(input$link_to_coverage, {
    updateTabsetPanel(session, "navlistPanel", selected = "technical notes")
    updateTabsetPanel(session, "tabs_tech_notes", selected = "Coverage")
  })

  observeEvent(input$link_to_technotes, {
    updateTabsetPanel(session, "navlistPanel", selected = "technical notes")
    updateTabsetPanel(session, "tabs_tech_notes", selected = "Absence statistics overview")
  })

  t <- list(
    family = "arial",
    size = 10,
    color = "grey"
  )


  # Setting up reactive levels for dropdown ------------------------------------------------------------


  # NEW LA FILTERING

  # Regional geographies updating based on LA
  observeEvent(input$region_choice, {
    if (input$geography_choice == "Regional") {
      las_in_region <- geog_lookup %>%
        filter(
          geographic_level == "Local authority",
          region_name == input$region_choice
        )
      if (input$la_choice %in% las_in_region) {
        selected_la <- input$la_choice
      } else {
        selected_la <- las_in_region %>%
          head(1) %>%
          pull(la_name)
      }
      updateSelectizeInput(session, "la_choice",
        selected = selected_la
      )
    }
  })

  observeEvent(input$la_choice, {
    if (input$geography_choice == "Local authority") {
      parent_region <- geog_lookup %>%
        filter(la_name == input$la_choice) %>%
        pull(region_name) %>%
        unique()
      if (parent_region != input$region_choice) {
        updateSelectizeInput(session, "region_choice", selected = parent_region)
      }
    }
  })

  # School type updating based on geographic level
  observe({
    if (input$dash == "la comparisons") {
      choicesSchools <- (school_type_lookup %>%
        dplyr::filter(geographic_level == "Local authority"))$school_type %>%
        unique()
    } else {
      choicesSchools <- (school_type_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice))$school_type %>%
        unique()
    }
    updateSelectInput(session, "school_choice",
      choices = choicesSchools,
      selected = input$school_choice
    )
  })

  # Reactive dates for dropdown
  reactive_latestweeks_string <- reactive({
    latestweek_line <- most_recent_week_lookup %>%
      filter(geographic_level == input$geography_choice)
    if (input$geography_choice == "Regional") {
      latestweek_line <- latestweek_line %>% filter(region_name == input$region_choice)
    } else if (input$geography_choice == "Local authority") {
      latestweek_line <- latestweek_line %>% filter(la_name == input$la_choice)
    }
    paste0("Latest week - ", latestweek_line %>% pull(week_start), " to ", latestweek_line %>% pull(week_end))
  })

  reactive_yeartodate_string <- reactive({
    year_line <- year_lookup %>%
      filter(geographic_level == input$geography_choice)
    if (input$geography_choice == "Regional") {
      year_line <- year_line %>% filter(region_name == input$region_choice)
    } else if (input$geography_choice == "Local authority") {
      year_line <- year_line %>% filter(la_name == input$la_choice)
    }
    paste0("Year to date - ", year_line %>% pull(year_start), " to ", year_line %>% pull(year_end))
  })

  reactive_period_selected <- reactive({
    if (input$ts_choice == "latestweeks") {
      period <- reactive_latestweeks_string()
    } else {
      period <- reactive_yeartodate_string()
    }
    period
  })

  observe({
    newchoices <- c(latest_weeks = "latestweeks", ytd_dates = "yeartodate")
    names(newchoices) <- c(reactive_latestweeks_string(), reactive_yeartodate_string())
    updateSelectInput(session, "ts_choice", choices = newchoices, selected = input$ts_choice)
  })

  # Dropdown expandable label ------------------------------------------------------------
  observeEvent(input$go, {
    toggle(id = "div_a", anim = T)
  })

  output$dropdown_label <- renderText({
    if (input$dash == "la comparisons") {
      paste0("Current selections: ", most_recent_week_dates, ", ", input$school_choice, ", National")
    } else if (input$geography_choice == "National") {
      paste0("Current selections: ", reactive_period_selected(), ", ", input$school_choice, ", ", input$geography_choice)
    } else if (input$geography_choice == "Regional") {
      paste0("Current selections: ", reactive_period_selected(), ", ", input$school_choice, ", ", input$geography_choice, ", ", input$region_choice)
    } else if (input$geography_choice == "Local authority") {
      paste0("Current selections: ", reactive_period_selected(), ", ", input$school_choice, ", ", input$geography_choice, ", ", input$region_choice, ", ", input$la_choice)
    }
  })


  # Defining reactive data ------------------------------------------------------------
  # Creates data all measures are derived from

  # Weekly data
  live_attendance_data_weekly <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == max(time_identifier))
    } else {
      NA
    }
  })

  live_attendance_data_weekly_pre_ht <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == "6")
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == "6")
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>% filter(time_identifier == "6")
    } else {
      NA
    }
  })

  # Weekly data for reasons tables
  live_attendance_data_weekly_reasons_tables <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier)) %>%
        mutate(
          illness_perc = illness_perc / 100,
          appointments_perc = appointments_perc / 100,
          auth_religious_perc = auth_religious_perc / 100,
          auth_study_perc = auth_study_perc / 100,
          auth_mob_perc = auth_mob_perc / 100,
          # auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
          auth_part_time_perc = auth_part_time_perc / 100,
          auth_other_perc = auth_other_perc / 100,
          unauth_hol_perc = unauth_hol_perc / 100,
          unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
          unauth_oth_perc = unauth_oth_perc / 100,
          unauth_not_yet_perc = unauth_not_yet_perc / 100
        )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier)) %>%
        mutate(
          illness_perc = illness_perc / 100,
          appointments_perc = appointments_perc / 100,
          auth_religious_perc = auth_religious_perc / 100,
          auth_study_perc = auth_study_perc / 100,
          auth_mob_perc = auth_mob_perc / 100,
          # auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
          auth_part_time_perc = auth_part_time_perc / 100,
          auth_other_perc = auth_other_perc / 100,
          unauth_hol_perc = unauth_hol_perc / 100,
          unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
          unauth_oth_perc = unauth_oth_perc / 100,
          unauth_not_yet_perc = unauth_not_yet_perc / 100
        )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier)) %>%
        mutate(
          illness_perc = illness_perc / 100,
          appointments_perc = appointments_perc / 100,
          auth_religious_perc = auth_religious_perc / 100,
          auth_study_perc = auth_study_perc / 100,
          auth_mob_perc = auth_mob_perc / 100,
          # auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
          auth_part_time_perc = auth_part_time_perc / 100,
          auth_other_perc = auth_other_perc / 100,
          unauth_hol_perc = unauth_hol_perc / 100,
          unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
          unauth_oth_perc = unauth_oth_perc / 100,
          unauth_not_yet_perc = unauth_not_yet_perc / 100
        )
    } else {
      NA
    }
  })

  # YTD data for reasons tables
  live_attendance_data_ytd_reasons_tables <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_mob_perc = auth_mob_perc / 100,
        # auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_part_time_perc = auth_part_time_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # ,pa_perc = pa_perc / 100
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_mob_perc = auth_mob_perc / 100,
        # auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_part_time_perc = auth_part_time_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # ,pa_perc = pa_perc / 100
      )
    } else if (input$geography_choice == "Local authority") {
      x <- dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      ) %>% mutate(
        illness_perc = illness_perc / 100,
        appointments_perc = appointments_perc / 100,
        auth_religious_perc = auth_religious_perc / 100,
        auth_study_perc = auth_study_perc / 100,
        auth_mob_perc = auth_mob_perc / 100,
        # auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_performance_perc = auth_performance_perc / 100,
        auth_part_time_perc = auth_part_time_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # ,pa_perc = pa_perc / 100
      )
      x
    } else {
      NA
    }
  })

  # Weekly data for local authority comparisons table
  live_attendance_data_weekly_las <- reactive({
    filter(
      attendance_data, geographic_level == "Local authority",
      school_type == input$school_choice,
      time_period == max(time_period),
      breakdown == "Weekly"
    ) %>%
      filter(time_identifier == max(time_identifier)) %>%
      # filter(time_identifier == max(time_identifier)-1) %>%
      mutate(
        overall_absence_perc = overall_absence_perc / 100,
        authorised_absence_perc = authorised_absence_perc / 100,
        unauthorised_absence_perc = unauthorised_absence_perc / 100
      )
  })


  # Weekly data for headline bullet reactive comparisons
  live_attendance_data_weekly_natcomp <- reactive({
    filter(
      attendance_data, geographic_level == "National",
      school_type == input$school_choice,
      time_period == max(time_period),
      breakdown == "Weekly"
    ) %>% filter(time_identifier == max(time_identifier))
  })

  live_attendance_data_weekly_regcomp <- reactive({
    filter(
      attendance_data, geographic_level == "Regional",
      region_name == input$region_choice,
      school_type == input$school_choice,
      time_period == max(time_period),
      breakdown == "Weekly"
    ) %>% filter(time_identifier == max(time_identifier))
  })


  # Ytd data for headline bullet reactive comparisons
  live_attendance_data_ytd_natcomp <- reactive({
    filter(
      attendance_data, geographic_level == "National",
      school_type == input$school_choice,
      # time_period == max(time_period),
      breakdown == "YTD"
    )
  })

  live_attendance_data_ytd_regcomp <- reactive({
    filter(
      attendance_data, geographic_level == "Regional",
      region_name == input$region_choice,
      school_type == input$school_choice,
      # time_period == max(time_period),
      breakdown == "YTD"
    )
  })


  # Full weekly timeseries for latest year
  live_attendance_data_ts <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else {
      NA
    }
  })

  # Full timeseries for latest year
  live_attendance_data_ytd <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      )
    } else {
      NA
    }
  })


  # Full timeseries for latest year response rates - non-reactive
  response_rates <- filter(
    attendance_data, geographic_level == "National",
    time_period == max(time_period),
    school_type %in% c("Total"),
    breakdown == "Daily"
  ) %>%
    arrange(attendance_date)


  # Creating reactive charts ------------------------------------------------------------

  # Reporting schools number chart
  output$response_rates <- renderPlotly({
    response_rates_figs <- response_rates %>%
      group_by(attendance_date) %>%
      mutate(overall_school_num = sum(num_schools))

    response_rate_plot <- plot_ly(
      response_rates_figs,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~overall_school_num,
        line = list(color = "black"),
        marker = list(color = "black"),
        name = "Number of schools reporting",
        hovertemplate = "%{y:.1f}",
        mode = "markers"
      ) %>%
      config(displayModeBar = FALSE)

    response_rate_plot <- response_rate_plot %>% layout(
      xaxis = list(title = "Date", zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      yaxis = list(rangemode = "tozero", title = "Number of schools reporting", zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      hovermode = "x unified",
      legend = list(
        font = list(size = 11),
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5
      ),
      title = "Summary of schools responding",
      font = t
    )
  })


  # Headline absence rates - ytd chart
  newtitle_weekly <- renderText({
    if (input$geography_choice == "National") {
      paste0("Weekly summary of absence rates for ", str_to_lower(input$school_choice), "<br>", " state-funded schools at ", str_to_lower(input$geography_choice), " level")
    } else if (input$geography_choice == "Regional") {
      paste0("Weekly summary of absence rates for ", "<br>", str_to_lower(input$school_choice), " state-funded schools ", "(", input$region_choice, ")")
    } else if (input$geography_choice == "Local authority") {
      paste0("Weekly summary of absence rates for ", "<br>", str_to_lower(input$school_choice), " state-funded schools ", "<br>", "(", input$region_choice, ", ", input$la_choice, ")")
    }
  })

  output$absence_rates_timeseries_plot <- renderPlotly({
    validate(need(nrow(live_attendance_data_ts()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ts()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_rates_ytd <- live_attendance_data_ts()

    ts_plot <- plot_ly(
      absence_rates_ytd,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~overall_absence_perc,
        # line = list(color = "black"),
        line = list(color = "#12436D"),
        # marker = list(color = "black"),
        marker = list(color = "#12436D"),
        name = "Overall absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~authorised_absence_perc,
        # line = list(color = "steelblue"),
        line = list(color = "#28A197"),
        # marker = list(color = "steelblue"),
        marker = list(color = "#28A197"),
        name = "Authorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~unauthorised_absence_perc,
        # line = list(color = "orangered"),
        line = list(color = "#F46A25"),
        # marker = list(color = "orangered"),
        marker = list(color = "#F46A25"),
        name = "Unauthorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      config(displayModeBar = FALSE)

    ts_plot <- ts_plot %>% layout(
      xaxis = list(title = "Week commencing", tickvals = ~week_commencing, zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      yaxis = list(rangemode = "tozero", title = "Absence rate (%)", zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
      hovermode = "x unified",
      legend = list(
        font = list(size = 11),
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5
      ),
      margin = list(t = 80),
      title = newtitle_weekly(),
      font = t
    )

    ts_plot <- ts_plot %>% layout(
      xaxis = list(
        tickmode = "linear",
        tick0 = "2022-09-12",
        # dtick = "M1"
        dtick = 86400000 * 14
      )
    )
  })


  # Headline absence rates - latest week chart
  newtitle_daily <- renderText({
    if (input$geography_choice == "National") {
      paste0("Daily summary of absence rates for ", str_to_lower(input$school_choice), "<br>", " state-funded schools at ", str_to_lower(input$geography_choice), " level")
    } else if (input$geography_choice == "Regional") {
      paste0("Daily summary of absence rates for ", "<br>", str_to_lower(input$school_choice), " state-funded schools ", "(", input$region_choice, ")")
    } else if (input$geography_choice == "Local authority") {
      paste0("Daily summary of absence rates for ", "<br>", str_to_lower(input$school_choice), " state-funded schools ", "<br>", "(", input$region_choice, ", ", input$la_choice, ")")
    }
  })


  output$headline_absence_chart <- ggiraph::renderGirafe({
    ggiraph::girafe(
      ggobj = headline_absence_ggplot(
        reasons_data() |>
          filter(geographic_level == input$geography_choice),
        input$ts_choice
      ),
      fonts = list(sans = plotting_font_family)
    )
  })

  output$absence_reasons_timeseries <- ggiraph::renderGirafe({
    ggiraph::girafe(
      ggobj = reasons_ggplot(
        reasons_data() |>
          filter(geographic_level == input$geography_choice),
        input$ts_choice
      ),
      fonts = list(sans = plotting_font_family),
      height_svg = 6,
      width_svg = 9
    )
  })

  # Reasons for absence - ytd chart

  newtitle_reasonsweekly <- renderText({
    if (input$geography_choice == "National") {
      paste0("Weekly summary of absence reasons for ", "<br>", str_to_lower(input$school_choice), " state-funded schools", "<br>", "at ", str_to_lower(input$geography_choice), " level")
    } else if (input$geography_choice == "Regional") {
      paste0("Weekly summary of absence reasons for ", "<br>", str_to_lower(input$school_choice), " state-funded schools", " at ", str_to_lower(input$geography_choice), " level", "<br>", "(", input$region_choice, ")")
    } else if (input$geography_choice == "Local authority") {
      paste0("Weekly summary of absence reasons for ", "<br>", str_to_lower(input$school_choice), " state-funded schools", " at ", str_to_lower(input$geography_choice), " level", "<br>", "(", input$region_choice, ", ", input$la_choice, ")")
    }
  })

  # Reasons for absence - latest week chart
  newtitle_reasonsdaily <- renderText({
    if (input$geography_choice == "National") {
      paste0("Daily absence reasons for ", str_to_lower(input$school_choice), " state-funded schools", "<br>", "at ", str_to_lower(input$geography_choice), " level")
    } else if (input$geography_choice == "Regional") {
      paste0("Daily absence reasons for ", str_to_lower(input$school_choice), " state-funded schools", "<br>", "(", input$region_choice, ")")
    } else if (input$geography_choice == "Local authority") {
      paste0("Daily absence reasons for ", str_to_lower(input$school_choice), " state-funded schools", "<br>", "(", input$region_choice, ", ", input$la_choice, ")")
    }
  })

  # Creating reactive titles ------------------------------------------------------------

  # timeseries chart reactive title
  output$headline_ts_chart_title <- renderUI({
    tagList(
      tags$h4(
        "Overall, authorised and unauthorised absence rates across the ",
        time_frame_string()
      ),
      tags$p(
        paste0(
          "Absence rates presented here are calculated on a ",
          ifelse(input$ts_choice == "latestweeks", "daily", "weekly"),
          " basis. Each point on the chart shows an absence rate calculated across all sessions in the given ",
          ifelse(input$ts_choice == "latestweeks", "day", "week"),
          "."
        )
      )
    )
  })

  # headline bullet reactive titles
  output$headline_title <- renderUI({
    tagList(
      shiny::tags$h2(
        "Headline figures for the ",
        str_to_lower(reactive_period_selected())
      ),
      shiny::tags$h3(
        input$school_choice,
        " state-funded school attendance at ",
        str_to_lower(input$geography_choice),
        " level",
        paste0(
          if (input$geography_choice == "Local authority") {
            paste0(" (", input$la_choice)
          },
          if (input$geography_choice != "National") {
            paste0(",", input$region_choice)
          },
          if (input$geography_choice != "National") {
            ")"
          }
        )
      )
    )
  })



  # reasons bullet reactive titles
  output$reasons_chart_title_nat <- renderText({
    paste0(input$school_choice, " state-funded schools: absence at ", str_to_lower(input$geography_choice), " level")
  })

  output$reasons_chart_title_reg <- renderText({
    paste0(input$school_choice, " state-funded schools: absence at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ")")
  })

  output$reasons_chart_title_la <- renderText({
    paste0(input$school_choice, " state-funded schools: absence at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ", ", input$la_choice, ")")
  })

  # la comparison table reactive title
  output$la_comparison_title <- renderText({
    paste0(input$school_choice, " state-funded schools: overall, authorised and unauthorised absence rates by local authority")
  })


  # Creating reactive embedded stats ------------------------------------------------------------


  schools_count <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      day_number == "5"
    ) %>%
    filter(time_identifier == max(time_identifier)) %>%
    pull(num_schools) %>%
    sum()

  schools_count_date <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      day_number == "5"
    ) %>%
    filter(time_identifier == max(time_identifier)) %>%
    pull(attendance_date)

  schools_count_pre_ht <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      day_number == "5"
    ) %>%
    filter(time_identifier == "6") %>%
    pull(num_schools) %>%
    sum()

  schools_count_date_pre_ht <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      day_number == "5"
    ) %>%
    filter(time_identifier == "6") %>%
    pull(attendance_date)

  output$daily_schools_count <- renderText({
    paste0(scales::comma(schools_count), " schools provided information on the latest full day of data, i.e. ", schools_count_date)
    # paste0(scales::comma(schools_count), " schools provided information on the latest full day of data prior to half-term, i.e. ", schools_count_date)
    # paste0(scales::comma(schools_count_pre_ht), " schools provided information on the last full day of data prior to half-term, i.e. ", schools_count_date_pre_ht)
  })


  # Proportion of schools in census figures are generated from - latest week
  output$school_count_proportion_weekly <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- live_attendance_data_weekly() %>%
      # count_prop_week <- live_attendance_data_weekly_pre_ht() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("For this breakdown, in the latest week ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 1), "% of schools submitted data, though this has varied throughout the year-to-date.")
  })

  output$school_count_proportion_weekly2 <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- live_attendance_data_weekly() %>%
      # count_prop_week <- live_attendance_data_weekly_pre_ht() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("For this breakdown, in the latest week ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of schools submitted data, though this has varied throughout the year-to-date.")
  })

  output$school_count_proportion_homepage <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- attendance_data %>%
      filter(
        breakdown == "Weekly",
        geographic_level == "National",
        school_type == "Total",
        time_period == max(time_period)
      ) %>%
      filter(time_identifier == max(time_identifier)) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("This number is approximately ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of the number of schools participating in the School Census. From the start of the 2024/25 academic year, it became mandatory for schools to share attendance data with the DfE. As more schools share their data, the number of schools reporting may change over time.")
  })

  # Proportion of schools in census figures are generated from - year to date
  output$school_count_proportion_ytd <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- live_attendance_data_ytd() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    paste0("For this breakdown, across the year-to-date there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% dfeR::round_five_up(dp = 10), "% of schools opted-in.")
  })

  output$headline_bullet_attendance_rate <- renderUI({
    time_frame_text <- ifelse(
      input$ts_choice == "latestweeks",
      "in the latest week",
      "across the year to date"
    )
    time_frame_data_string <- ifelse(
      input$ts_choice == "latestweeks",
      "Week",
      "Year to date"
    )
    # --------------------------------------------------------------------------
    lines <- reasons_data() |>
      filter(
        time_frame == time_frame_data_string,
        geographic_level == input$geography_choice
      )
    if (input$geography_choice == "Local authority") {
      comparator_level <- "Regional"
    } else if (input$geography_choice == "Regional") {
      comparator_level <- "National"
    } else {
      comparator_level <- NULL
    }

    if (!is.null(comparator_level)) {
      comparators <- reasons_data() |>
        filter(
          time_frame == time_frame_data_string,
          geographic_level == comparator_level
        )
    }
    tagList(
      tags$h4("Attendance and absence", time_frame_text),
      tags$p(
        "Attendance and absence rates presented here are calculated across all sessions",
        time_frame_text
      ),
      shiny::tags$ul(
        shiny::tags$li(
          headline_bullet(
            lines |>
              dplyr::filter(
                attendance_status == "Attendance",
                attendance_type == "Overall attendance"
              ) |>
              dplyr::pull(session_percent),
            comparators |>
              dplyr::filter(
                attendance_status == "Attendance",
                attendance_type == "Overall attendance"
              ) |>
              dplyr::pull(session_percent),
            "attending",
            input$geography_choice,
            input$la_choice,
            input$region_choice
          )
        ),
        shiny::tags$li(
          headline_bullet(
            lines |>
              dplyr::filter(
                attendance_status == "Absence",
                attendance_type == "Overall absence"
              ) |>
              dplyr::pull(session_percent),
            comparators |>
              dplyr::filter(
                attendance_status == "Absence",
                attendance_type == "Overall absence"
              ) |>
              dplyr::pull(session_percent),
            "absence",
            input$geography_choice,
            input$la_choice,
            input$region_choice
          )
        ),
        shiny::tags$li(
          headline_bullet(
            lines |>
              dplyr::filter(
                attendance_reason == "Illness (i)"
              ) |>
              dplyr::pull(session_percent),
            comparators |>
              dplyr::filter(
                attendance_reason == "Illness (i)"
              ) |>
              dplyr::pull(session_percent),
            "illness",
            input$geography_choice,
            input$la_choice,
            input$region_choice
          )
        )
      )
    )
  })

  output$headline_persistent_absence <- renderUI({
    if (input$ts_choice == "latestweeks") {
      tagList(
        tags$p(
          "To view persistent absence figures, select \"year to date\" in the drop-down menu.",
          "Figures are not provided in the weekly or daily data because persistent absence is a",
          "measure over time and not valid for short time periods. Underlying data relating to",
          "the Summer, Spring and Autumn terms and year to date is available at the ",
          dfeshiny::external_link(
            paste0(
              "https://explore-education-statistics.service.gov.uk/find-statistics/",
              ees_pub_slug
            ),
            paste(ees_pub_name, "publication on Explore education statistics")
          ),
          "."
        )
      )
    } else {
      lines <- pa_data() |>
        filter(
          geographic_level == input$geography_choice
        )
      if (input$geography_choice == "Local authority") {
        comparator_level <- "Regional"
      } else if (input$geography_choice == "Regional") {
        comparator_level <- "National"
      } else {
        comparator_level <- NULL
      }

      if (!is.null(comparator_level)) {
        comparators <- pa_data() |>
          filter(
            geographic_level == comparator_level
          )
      }
      tagList(
        tags$p(
          "A pupil enrolment is identified as persistently absent if they have missed",
          "10% or more of their possible sessions in the year to date."
        ),
        shiny::tags$ul(
          shiny::tags$li(
            headline_bullet(
              lines |> magrittr::extract2("persistent_absence_percent"),
              comparators |> magrittr::extract2("persistent_absence_percent"),
              "persistently absent",
              input$geography_choice,
              input$la_choice,
              input$region_choice,
              subject = "pupils"
            )
          )
        )
      )
    }
  })


  # Creating reactive dates for text ------------------------------------------------------------
  output$source_version_release <- renderText({
    paste0(
      "The data in this dashboard was released on ",
      reasons_data_version_info()$release_date,
      " as part of the ",
      reasons_data_version_info()$release_name,
      " release."
    )
  })

  output$la_clarity_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    paste0("Data on this tab relates to the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data on this tab relates to the week commencing 2024-10-21.")
  })

  output$update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17
    # as.Date(attendance_date) + 24
    # as.Date(attendance_date) + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31
    # as.Date(attendance_date) + 38

    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  output$update_dates2 <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17
    # as.Date(attendance_date) + 24
    # as.Date(attendance_date) + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31
    # as.Date(attendance_date) + 38

    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  output$homepage_update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17
    # as.Date(attendance_date) + 24
    # as.Date(attendance_date) + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31
    # as.Date(attendance_date) + 38

    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  # Creating reactive boxes ------------------------------------------------------------

  output$absence_rates_value_boxes <- renderUI({
    if (input$ts_choice == "latestweeks") {
      time_frame_text <- "Latest full week"
      time_frame_filter <- "Week"
    } else {
      time_frame_text <- "Year to date"
      time_frame_filter <- "Year to date"
    }
    tagList(
      p(strong(paste0("Authorised absence rate:"))),
      shinyGovstyle::value_box(
        "headline_auth_rate",
        text = time_frame_text,
        value = reasons_data() |>
          filter(
            geographic_level == input$geography_choice,
            attendance_reason == "All authorised",
            time_frame == time_frame_filter
          ) |>
          pull(session_percent) |>
          as.numeric() |>
          dfeR::round_five_up(dp = 1) |>
          paste("%"),
        colour = "blue"
      ),
      p(strong(paste0("Unauthorised absence rate:"))),
      shinyGovstyle::value_box(
        "headline_auth_rate",
        text = time_frame_text,
        value = reasons_data() |>
          filter(
            geographic_level == input$geography_choice,
            attendance_reason == "All unauthorised",
            time_frame == time_frame_filter
          ) |>
          pull(session_percent) |>
          as.numeric() |>
          dfeR::round_five_up(dp = 1) |>
          paste("%"),
        colour = "blue"
      )
    )
  })

  # Creating reactive reasons and la comparison table ------------------------------------------------------------

  output$absence_auth_table_title <- renderUI(
    shiny::tags$h3("Reasons for absence in the", tolower(time_frame_descriptors()$display_string))
  )

  output$absence_auth_reasons_reactable <- renderReactable({
    dfeshiny::dfe_reactable(
      reasons_data() |>
        filter(
          geographic_level == input$geography_choice,
          time_frame == time_frame_descriptors()$filter_string,
          attendance_reason %in% c(
            "Illness (i)",
            "Medical dental (m)",
            "Religious observance (r)",
            "Study leave (s)",
            "Mobile child (t)",
            "Excluded (e)",
            "Temporary reduced timetable (c2)",
            "Other authorised (c)"
          )
        ) |>
        select(attendance_reason, session_percent) |>
        mutate(
          session_percent = session_percent |>
            as.numeric() |>
            dfeR::round_five_up(dp = 1) |>
            paste0("%")
        ) |>
        tidyr::pivot_wider(
          names_from = attendance_reason,
          values_from = session_percent
        )
    )
  })

  output$absence_unauth_reasons_reactable <- renderReactable({
    unauth_data <- reasons_data() |>
      filter(
        geographic_level == input$geography_choice,
        time_frame == time_frame_descriptors()$filter_string,
        attendance_reason %in% c(
          "Unauthorised holiday (g)",
          "Late after registers closed (u)",
          "Other unauthorised (o)",
          "No reason yet (n)"
        )
      ) |>
      select(attendance_reason, session_percent) |>
      mutate(
        attendance_reason = attendance_reason |>
          stringr::str_replace("Unauthorised ", "") |>
          stringr::str_to_sentence(),
        session_percent = session_percent |>
          as.numeric() |>
          dfeR::round_five_up(dp = 1) |>
          paste0("%")
      ) |>
      tidyr::pivot_wider(
        names_from = attendance_reason,
        values_from = session_percent
      )
    dfeshiny::dfe_reactable(
      unauth_data
    )
  })

  # absence reasons by local authority
  output$absence_reasons_la_table <- renderDT({
    absence_reasons_la <- live_attendance_data_weekly_las() %>%
      dplyr::select(time_period, time_identifier, week_commencing, region_name, la_name, overall_absence_perc, authorised_absence_perc, unauthorised_absence_perc) %>%
      arrange(desc(overall_absence_perc)) %>%
      rename(
        "Year" = time_period,
        "Week number" = time_identifier,
        "Week commencing" = week_commencing,
        "Region name" = region_name,
        "Local authority name" = la_name,
        "Overall absence rate" = overall_absence_perc,
        "Authorised absence rate" = authorised_absence_perc,
        "Unauthorised absence rate" = unauthorised_absence_perc
      )


    absence_reasons_la <- datatable(absence_reasons_la,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      options = list(scrollX = TRUE, columnDefs = list(list(className = "dt-center", targets = 0:7), list(targets = 1, searchable = FALSE, visible = FALSE)))
    ) %>%
      formatPercentage(c(6:8), 1)
  })

  output$absence_reasons_la_reactable <- renderReactable({
    dfe_reactable(
      la_data() |>
        select(-all_of(c("attendance_status", "attendance_type"))) |>
        mutate(session_percent = render_percents(session_percent)) |>
        tidyr::pivot_wider(
          names_from = "attendance_reason",
          values_from = "session_percent"
        ) |>
        select(
          Year = time_period,
          `Week commencing` = reference_date,
          Region = reg_name,
          `Local authority` = la_name,
          `Overall absence rate` = "Overall absence",
          `Authorised absence rate` = "All authorised",
          `Unauthorised absence rate` = "All unauthorised"
        ),
      searchable = TRUE,
      defaultSorted = list(
        `Overall absence rate` = "desc",
        `Authorised absence rate` = "desc",
        `Unauthorised absence rate` = "desc",
        `Local authority` = "asc"
      )
    )
  })

  # Tech guidance tables ----------------------------------------------------

  output$notesTableHeadlines <- function() {
    notesTableHeadlines[is.na(notesTableHeadlines)] <- " "

    kable(notesTableHeadlines, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = T) %>%
      column_spec(1, bold = T, extra_css = "vertical-align: top !important;") %>%
      column_spec(2, width_max = "40em")
  }

  output$notesTableReasons <- renderDT({
    datatable(notesTableReasons, options = list(pageLength = 15))
  })

  # Data download button ---------------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Underlying_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      underlying_data <- EES_daily_data %>% filter(school_type %in% c("Primary", "Secondary", "Special", "Total"))
      write.csv(underlying_data, con, row.names = FALSE)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Underlying_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      underlying_data <- EES_daily_data %>% filter(school_type %in% c("Primary", "Secondary", "Special", "Total"))
      write.csv(underlying_data, con, row.names = FALSE)
    }
  )


  ## Create the map function ###############################################

  mapdata_shaped_type <- reactive({
    dplyr::filter(
      mapdata_shaped,
      school_type == input$school_choice
    )
  })

  # Create map function

  output$rates_map <- renderLeaflet({
    if (input$measure_choice == "Overall") {
      rate_choice <- "Overall absence"
    } else {
      rate_choice <- paste("All", tolower(input$measure_choice))
    }

    data <- map_data() |>
      filter(attendance_reason == rate_choice)
    pallette_scale <- colorQuantile(
      map_gov_colours,
      data |>
        dplyr::pull(session_percent),
      n = 5
    )
    rate_map <- data %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~ pallette_scale(session_percent),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "0",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~label,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px",
            "background-color" = "white"
          ),
          textsize = "15px",
          direction = "auto"
        )
      )
    rate_map <- rate_map %>%
      addLegend(
        colors = c("#FFBF47", "#EC933D", "#D86733", "#C53A28", "#B10E1E", "#808080"),
        opacity = 1,
        title = NULL,
        position = "topright",
        labels = c("Lowest absence rates", "", "", "", "Highest absence rates", "Supressed data")
      ) %>%
      setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
  })

  output$map_title <- renderText({
    paste0(
      input$school_choice, " state-funded schools: ", str_to_lower(input$measure_choice), " absence rates by local authority"
    )
  })
}

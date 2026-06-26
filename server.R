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

  rank_data_multi <- function(df) {
    df |>
      dplyr::mutate(
        rank_overall = dplyr::dense_rank(desc(`Overall absence`)),
        rank_authorised = dplyr::dense_rank(desc(`All authorised`)),
        rank_unauthorised = dplyr::dense_rank(desc(`All unauthorised`))
      )
  }

  latest_time_period <- reactive({
    eesyapi::query_dataset(
      schools_submitting_dataset_id,
      geographies = geography_query(
        input$geography_choice,
        input$region_choice,
        input$la_choice
      ),
      filter_items = list(
        time_frame = schools_submitting_sqids$filters$time_frame$friday,
        school_phase = schools_submitting_sqids$filters$education_phase$allschools
      ),
      ees_environment = ees_api_env
    ) |>
      filter(time_period == max(time_period)) |>
      mutate(
        week = str_replace_all(time_identifier, "Week ", "") |> as.numeric()
      ) |>
      filter(week == max(week)) |>
      mutate(
        query = paste(
          time_period,
          time_identifier |>
            stringr::str_replace("Week ", "W"),
          sep = "|"
        )
      ) |>
      pull(query)
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$geography_choice,
      input$region_choice,
      input$la_choice
    )

  time_query <- reactive({
    message("Time period choice: ", input$ts_choice)
    if (input$ts_choice == "latestweeks") {
      time_period_query <- latest_time_period()
      message(time_period_query, latest_time_period())
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
        reasons_sqids$filters$time_frame$yeartodate,
        reasons_sqids$filters$time_frame$week
      )
    }
    message(paste(time_period_query, collapse = ", "))
    return(list(
      time_period_query = time_period_query,
      time_frame_query = time_frame_query
    ))
  })

  # Retrieving data from the API
  latest_schools_count <- reactive({
    time_period_query <- time_query()$time_period_query
    result <- eesyapi::query_dataset(
      schools_submitting_dataset_id,
      time_periods = time_period_query,
      geographies = "National",
      filter_items = list(
        time_frame = schools_submitting_sqids$filters$time_frame$friday,
        school_phase = schools_submitting_sqids$filters$education_phase$allschools
      ),
      ees_environment = ees_api_env
    )
    print(result)
    return(result)
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version
    )

  school_numbers_data <- reactive({
    time_period_query <- time_query()$time_period_query
    if (input$geography_choice == "National") {
      query <- "National"
    } else if (input$geography_choice == "Regional") {
      reg_code <- dfeR::fetch_regions() |>
        dplyr::filter(region_name == input$region_choice) |>
        dplyr::pull(region_code)
      query <- paste0("REG|code|", reg_code)
    } else if (input$geography_choice == "Local authority") {
      la_code <- dfeR::fetch_las() |>
        dplyr::filter(la_name == input$la_choice) |>
        dplyr::pull(new_la_code)
      query <- paste0("LA|code|", la_code)
    }
    message("This is the query: ", query)
    result <- eesyapi::query_dataset(
      schools_submitting_dataset_id,
      time_periods = time_period_query,
      geographies = query,
      filter_items = list(
        time_frame = schools_submitting_sqids$filters$time_frame$week,
        school_phase = schools_submitting_sqids$filters$education_phase |>
          magrittr::extract2(school_phase_key())
      ),
      ees_environment = ees_api_env
    )
    print(result)
    return(result)
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$ts_choice,
      input$geography_choice,
      input$region_choice,
      input$la_choice,
      input$school_choice
    )


  latest_week_data <- reactive({
    time_period_query <- time_query()$time_period_query

    if (input$geography_choice == "National") {
      query <- "National"
    } else if (input$geography_choice == "Regional") {
      reg_code <- dfeR::fetch_regions() |>
        dplyr::filter(region_name == input$region_choice) |>
        dplyr::pull(region_code)
      query <- paste0("REG|code|", reg_code)
    } else if (input$geography_choice == "Local authority") {
      la_code <- dfeR::fetch_las() |>
        dplyr::filter(la_name == input$la_choice) |>
        dplyr::pull(new_la_code)
      query <- paste0("LA|code|", la_code)
    }

    eesyapi::query_dataset(
      schools_submitting_dataset_id,
      time_periods = time_period_query,
      geographies = query,
      filter_items = list(
        time_frame = schools_submitting_sqids$filters$time_frame$monday,
        school_phase = schools_submitting_sqids$filters$education_phase |>
          magrittr::extract2(school_phase_key())
      ),
      ees_environment = ees_api_env
    )
  })

  # reasons_data_version_info <- eventReactive(input$geography_choice, {
  #   eesyapi::get_dataset_versions(
  #     reasons_dataset_id,
  #     ees_environment = ees_api_env
  #   ) |>
  #     filter(version == max(version)) |>
  #     dplyr::slice(1)
  # })

  reasons_data_version_info <- eventReactive(input$geography_choice, {
    eesyapi::get_dataset_versions(
      reasons_dataset_id,
      ees_environment = ees_api_env
    ) |>
      dplyr::mutate(
        major = as.integer(sub("\\..*$", "", version)),
        minor = as.integer(sub("^.*\\.", "", version))
      ) |>
      dplyr::arrange(desc(major), desc(minor)) |>
      dplyr::slice(1)
  })

  school_phase_key <- reactive({
    key <- tolower(input$school_choice)

    if (key == "total") {
      key <- "allschools"
    }

    key
  })


  reasons_data <- reactive({
    reasons <- eesyapi::query_dataset(
      reasons_dataset_id,
      time_periods = time_query()$time_period_query,
      geographies = geography_query(
        input$geography_choice,
        input$region_choice,
        input$la_choice
      ),
      filter_items = list(
        time_frame = time_query()$time_frame_query,
        school_phase = reasons_sqids$filters$education_phase |>
          magrittr::extract2(school_phase_key()),
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
    print(reasons)
    reasons
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
          magrittr::extract2(school_phase_key()),
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
    time_period_query <- pa_data_version_info()$time_period_end |>
      stringr::str_replace("Week ", "W") |>
      stringr::str_replace(" ", "|")
    eesyapi::query_dataset(
      persistent_absence_dataset_id,
      time_periods = time_period_query,
      geographies = geography_query(
        input$geography_choice,
        input$region_choice,
        input$la_choice
      ),
      filter_items = list(
        school_phase = persistent_absence_sqids$filters$education_phase |>
          magrittr::extract2(school_phase_key())
      ),
      indicators = unlist(
        persistent_absence_sqids$indicators,
        use.names = FALSE
      ),
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

  map_data <- reactive({
    merged <- merge(
      mapshape |> dplyr::rename(la_code = CTYUA23CD),
      la_data(),
      by = "la_code",
      duplicateGeoms = TRUE
    )

    # ✅ convert long → wide (fixes correctness)
    wide <- merged |>
      sf::st_drop_geometry() |>
      dplyr::select(la_code, la_name, attendance_reason, session_percent) |>
      tidyr::pivot_wider(
        names_from = attendance_reason,
        values_from = session_percent
      ) |>
      dplyr::mutate(
        overall_absence_perc = as.numeric(`Overall absence`),
        authorised_absence_perc = as.numeric(`All authorised`),
        unauthorised_absence_perc = as.numeric(`All unauthorised`)
      )

    # ✅ England only + reattach geometry
    final <- mapshape |>
      dplyr::rename(la_code = CTYUA23CD) |>
      dplyr::filter(substr(la_code, 1, 1) == "E") |>
      dplyr::left_join(wide, by = "la_code")

    final
  }) |>
    shiny::bindCache(
      reasons_data_version_info()$version,
      input$school_choice
    )

  # Base time frame string to show the latest week dates or year to date range coverage.
  # This is derived direcrtly from the data as downloaded bia the API.
  time_frame_string <- reactive({
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
        dates |> max() + 4
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
    updateTabsetPanel(
      session,
      "tabs_tech_notes",
      selected = "Absence statistics overview"
    )
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
      updateSelectizeInput(session, "la_choice", selected = selected_la)
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
        dplyr::filter(
          geographic_level == input$geography_choice
        ))$school_type %>%
        unique()
    }
    updateSelectInput(
      session,
      "school_choice",
      choices = choicesSchools,
      selected = input$school_choice
    )
  })

  # Reactive dates for dropdown
  # reactive_latestweeks_string <- reactive({
  #   latestweek_line <- most_recent_week_lookup %>%
  #     filter(geographic_level == input$geography_choice)
  #   if (input$geography_choice == "Regional") {
  #     latestweek_line <- latestweek_line %>%
  #       filter(region_name == input$region_choice)
  #   } else if (input$geography_choice == "Local authority") {
  #     latestweek_line <- latestweek_line %>% filter(la_name == input$la_choice)
  #   }
  #   paste0(
  #     "Latest week - ",
  #     latestweek_line %>% pull(week_start),
  #     " to ",
  #     latestweek_line %>% pull(week_end)
  #   )
  # })
  reactive_latestweeks_string <- reactive({
    ref_date <- latest_week_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1) |>
      dplyr::pull(reference_date)

    ref_date <- as.Date(ref_date)

    # reference_date is MONDAY → show Monday to Friday
    paste0(
      "Latest week - ",
      ref_date,
      " to ",
      ref_date + 4
    )
  })

  # reactive_yeartodate_string <- reactive({
  #   year_line <- year_lookup %>%
  #     filter(geographic_level == input$geography_choice)
  #   if (input$geography_choice == "Regional") {
  #     year_line <- year_line %>% filter(region_name == input$region_choice)
  #   } else if (input$geography_choice == "Local authority") {
  #     year_line <- year_line %>% filter(la_name == input$la_choice)
  #   }
  #   paste0(
  #     "Year to date - ",
  #     year_line %>% pull(year_start),
  #     " to ",
  #     year_line %>% pull(year_end)
  #   )
  # })

  reactive_yeartodate_string <- reactive({
    year_line <- latest_week_data()

    # ✅ ensure only ONE row
    year_line <- year_line |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1)

    dates <- as.Date(year_line$reference_date)

    if (length(dates) == 0) {
      return("Year to date - no data available")
    }

    start_date <- as.Date("2025-09-08")
    end_date <- dates + 4

    paste0(
      "Year to date - ",
      start_date,
      " to ",
      end_date
    )
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
    names(newchoices) <- c(
      reactive_latestweeks_string(),
      reactive_yeartodate_string()
    )
    updateSelectInput(
      session,
      "ts_choice",
      choices = newchoices,
      selected = input$ts_choice
    )
  })

  # Dropdown expandable label ------------------------------------------------------------
  observeEvent(input$go, {
    toggle(id = "div_a", anim = T)
  })

  output$dropdown_label <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    most_recent_fullweek_date <- latest_week_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1) |>
      dplyr::pull(reference_date)

    # FIX: reference_date is MONDAY, so show Monday → Friday
    most_recent_week_dates <- paste0(
      as.Date(most_recent_fullweek_date),
      " - ",
      as.Date(most_recent_fullweek_date) + 4
    )

    if (input$dash == "la comparisons") {
      paste0(
        "Current selections: ",
        most_recent_week_dates,
        ", ",
        input$school_choice,
        ", National"
      )
    } else if (input$geography_choice == "National") {
      paste0(
        "Current selections: ",
        reactive_period_selected(),
        ", ",
        input$school_choice,
        ", ",
        input$geography_choice
      )
    } else if (input$geography_choice == "Regional") {
      paste0(
        "Current selections: ",
        reactive_period_selected(),
        ", ",
        input$school_choice,
        ", ",
        input$geography_choice,
        ", ",
        input$region_choice
      )
    } else if (input$geography_choice == "Local authority") {
      paste0(
        "Current selections: ",
        reactive_period_selected(),
        ", ",
        input$school_choice,
        ", ",
        input$geography_choice,
        ", ",
        input$region_choice,
        ", ",
        input$la_choice
      )
    }
  })

  # Defining reactive data ------------------------------------------------------------
  # Creates data all measures are derived from

  # Weekly data
  live_attendance_data_weekly <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data,
        geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data,
        geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data,
        geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      ) %>%
        filter(time_identifier == max(time_identifier))
    } else {
      NA
    }
  })

  # Full timeseries for latest year
  live_attendance_data_ytd <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data,
        geographic_level == "National",
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data,
        geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        # time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data,
        geographic_level == "Local authority",
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

  # Creating reactive charts ------------------------------------------------------------

  # Headline absence rates - ytd chart
  newtitle_weekly <- renderText({
    if (input$geography_choice == "National") {
      paste0(
        "Weekly summary of absence rates for ",
        str_to_lower(input$school_choice),
        "<br>",
        " state-funded schools at ",
        str_to_lower(input$geography_choice),
        " level"
      )
    } else if (input$geography_choice == "Regional") {
      paste0(
        "Weekly summary of absence rates for ",
        "<br>",
        str_to_lower(input$school_choice),
        " state-funded schools ",
        "(",
        input$region_choice,
        ")"
      )
    } else if (input$geography_choice == "Local authority") {
      paste0(
        "Weekly summary of absence rates for ",
        "<br>",
        str_to_lower(input$school_choice),
        " state-funded schools ",
        "<br>",
        "(",
        input$region_choice,
        ", ",
        input$la_choice,
        ")"
      )
    }
  })


  # Headline absence rates - titles (FIXED)

  newtitle_weekly <- reactive({
    paste0(
      "Weekly summary of absence rates for ",
      str_to_lower(input$school_choice),
      "<br>state-funded schools at ",
      str_to_lower(input$geography_choice),
      " level"
    )
  })

  newtitle_daily <- reactive({
    paste0(
      "Daily summary of absence rates for ",
      str_to_lower(input$school_choice),
      "<br>state-funded schools at ",
      str_to_lower(input$geography_choice),
      " level"
    )
  })

  # output$headline_absence_chart <- ggiraph::renderGirafe({
  #   ggiraph::girafe(
  #     ggobj = headline_absence_ggplot(
  #       reasons_data() |>
  #         filter(geographic_level == input$geography_choice),
  #       input$ts_choice
  #     ),
  #     width_svg = 10,
  #     height_svg = 6,
  #     options = list(
  #       ggiraph::opts_sizing(rescale = TRUE),
  #       ggiraph::opts_tooltip(
  #         css = "
  #     background-color:white;
  #     color:#000;
  #     padding:12px;
  #     border-radius:6px;
  #     border:1px solid #d0d0d0;
  #     box-shadow:0 3px 10px rgba(0,0,0,0.15);
  #     font-size:13px;
  #     line-height:1.5;
  #   "
  #       ),
  #       ggiraph::opts_hover(
  #         css = "
  #     stroke-width:3;
  #     opacity:1;
  #   "
  #       )
  #     )
  #   )
  # })


  output$headline_absence_chart <- plotly::renderPlotly({
    title_text <- if (input$ts_choice == "latestweeks") {
      newtitle_daily()
    } else {
      newtitle_weekly()
    }

    headline_absence_plotly(
      reasons_data() |>
        dplyr::filter(geographic_level == input$geography_choice),
      input$ts_choice,
      title_text
    )
  })


  output$absence_reasons_timeseries <- plotly::renderPlotly({
    reasons_plotly(
      reasons_data() |>
        dplyr::filter(geographic_level == input$geography_choice),
      input$ts_choice
    )
  })

  # Reasons for absence - ytd chart

  newtitle_reasonsweekly <- renderText({
    if (input$geography_choice == "National") {
      paste0(
        "Weekly summary of absence reasons for ",
        "<br>",
        str_to_lower(input$school_choice),
        " state-funded schools",
        "<br>",
        "at ",
        str_to_lower(input$geography_choice),
        " level"
      )
    } else if (input$geography_choice == "Regional") {
      paste0(
        "Weekly summary of absence reasons for ",
        "<br>",
        str_to_lower(input$school_choice),
        " state-funded schools",
        " at ",
        str_to_lower(input$geography_choice),
        " level",
        "<br>",
        "(",
        input$region_choice,
        ")"
      )
    } else if (input$geography_choice == "Local authority") {
      paste0(
        "Weekly summary of absence reasons for ",
        "<br>",
        str_to_lower(input$school_choice),
        " state-funded schools",
        " at ",
        str_to_lower(input$geography_choice),
        " level",
        "<br>",
        "(",
        input$region_choice,
        ", ",
        input$la_choice,
        ")"
      )
    }
  })

  # Reasons for absence - latest week chart
  newtitle_reasonsdaily <- renderText({
    if (input$geography_choice == "National") {
      paste0(
        "Daily absence reasons for ",
        str_to_lower(input$school_choice),
        " state-funded schools",
        "<br>",
        "at ",
        str_to_lower(input$geography_choice),
        " level"
      )
    } else if (input$geography_choice == "Regional") {
      paste0(
        "Daily absence reasons for ",
        str_to_lower(input$school_choice),
        " state-funded schools",
        "<br>",
        "(",
        input$region_choice,
        ")"
      )
    } else if (input$geography_choice == "Local authority") {
      paste0(
        "Daily absence reasons for ",
        str_to_lower(input$school_choice),
        " state-funded schools",
        "<br>",
        "(",
        input$region_choice,
        ", ",
        input$la_choice,
        ")"
      )
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
      # shiny::tags$h2(
      #   "Headline figures",
      #   # time_frame_string() # Commented out because of repetition of dates
      #   # str_to_lower(reactive_period_selected())
      # ),
      shiny::tags$h3(
        input$school_choice,
        " state-funded school attendance at ",
        str_to_lower(input$geography_choice),
        " level:",
        paste0(
          if (input$geography_choice == "Local authority") {
            paste0("", input$la_choice)
          },
          if (input$geography_choice != "National") {
            paste0(", ", input$region_choice)
          },
          if (input$geography_choice != "National") {
            " "
          }
        )
      )
    )
  })

  # reasons bullet reactive titles
  output$reasons_chart_title <- renderText({
    paste0(
      input$school_choice,
      " state-funded school absence at ",
      str_to_lower(input$geography_choice),
      " level:",
      paste0(
        if (input$geography_choice == "Local authority") {
          paste0(" ", input$la_choice, ",")
        },
        if (input$geography_choice != "National") {
          paste0(" ", input$region_choice)
        },
        if (input$geography_choice != "National") {
          " "
        }
      )
    )
  })
  # if (input$geography_choice %in% c("Regional", "Local authority")) {
  #   " ("
  # },
  # if (input$geography_choice %in% c("Local authority")) {
  #   paste(input$la_choice, ", ")
  # },
  # if (input$geography_choice %in% c("Regional", "Local authority")) {
  #   paste0(input$region_choice, ")")
  # }


  # la comparison table reactive title
  output$la_comparison_title <- renderText({
    paste0(
      input$school_choice,
      " state-funded schools: overall, authorised and unauthorised absence rates by local authority"
    )
  })

  # Creating reactive embedded stats ------------------------------------------------------------
  output$daily_schools_count <- renderText({
    paste0(
      "On the latest full day of data (i.e. ",
      latest_schools_count()$reference_date,
      "), ",
      scales::comma(
        latest_schools_count()$school_submitted_count |> as.numeric()
      ),
      " schools provided information."
    )
    # paste0(scales::comma(schools_count), " schools provided information on the latest full day of data prior to half-term, i.e. ", schools_count_date)
    # paste0(scales::comma(schools_count_pre_ht), " schools provided information on the last full day of data prior to half-term, i.e. ", schools_count_date_pre_ht)
  })

  output$school_count_proportion_homepage <- renderText({
    validate(need(
      nrow(latest_schools_count()) > 0,
      "There is no data available for this breakdown at present"
    ))
    validate(need(
      latest_schools_count()$school_submitted_count > 1,
      "This data has been suppressed due to a low number of schools at this breakdown"
    ))

    percent_submitted <- as.numeric(
      latest_schools_count()$school_submitted_count
    ) /
      as.numeric(latest_schools_count()$school_all_count) *
      100.
    # paste0(
    #   "This number is approximately ",
    #   percent_submitted |> dfeR::round_five_up(dp = 2),
    #   "% of the number of schools participating in the School Census. From the start of the ",
    #   "2024/25 academic year, it became mandatory for schools to share attendance data with the ",
    #   "DfE. As more schools share their data, the number of schools reporting may change over ",
    #   "time."
    # )
    paste0(
      "This number is approximately ",
      render_percents(percent_submitted),
      " of the number of schools participating in the School Census. From the start of the ",
      "2024/25 academic year, it became mandatory for schools to share attendance data with the ",
      "DfE. As more schools share their data, the number of schools reporting may change over ",
      "time."
    )
  })

  # Proportion of schools in census figures are generated from - latest week
  output$school_count_proportion <- renderText({
    validate(
      need(
        school_numbers_data()$school_submitted_count > 1,
        "This data has been suppressed due to a low number of schools at this breakdown"
      )
    )
    school_numbers_data <- school_numbers_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1)

    school_submitted_percent <- as.numeric(
      school_numbers_data$school_submitted_count
    ) /
      as.numeric(
        school_numbers_data$school_all_count
      ) * 100

    paste0(
      "For this breakdown, in the latest week ",
      render_percents(school_submitted_percent),
      " of schools submitted data, though this has varied throughout the year-to-date."
    )
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
        paste0(
          "Attendance and absence rates presented here are calculated across all sessions ",
          time_frame_text,
          "."
        )
      ),
      shiny::tags$ul(
        shiny::tags$li(
          paste0(
            render_percents(
              lines |>
                filter(
                  attendance_status == "Attendance",
                  attendance_type == "Overall attendance"
                ) |>
                pull(session_percent)
            ),
            " of sessions were recorded as attending"
          )
        ),
        shiny::tags$li(
          paste0(
            render_percents(
              lines |>
                filter(
                  attendance_status == "Absence",
                  attendance_type == "Overall absence"
                ) |>
                pull(session_percent)
            ),
            " of sessions were recorded as absence"
          )
        ),
        shiny::tags$li(
          paste0(
            render_percents(
              lines |>
                filter(attendance_reason == "Illness (i)") |>
                pull(session_percent)
            ),
            " of sessions were recorded as illness"
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
          "the summer, spring and autumn terms and year to date is available at the ",
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

    # most_recent_fullweek_date <- live_attendance_data_weekly() %>%
    #   pull(week_commencing)

    most_recent_fullweek_date <- latest_week_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1) |>
      dplyr::pull(reference_date)

    paste0(
      "Data on this tab relates to the week commencing ",
      most_recent_fullweek_date,
      "."
    )
    # paste0("Data on this tab relates to the week commencing 2024-10-21.")
  })

  output$update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    # most_recent_fullweek_date <- live_attendance_data_weekly() %>%
    #   pull(week_commencing)

    # last_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   17
    # # as.Date(attendance_date) + 24
    # # as.Date(attendance_date) + 31

    last_update_date <- reasons_data_version_info()$release_date

    # next_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   31
    # # as.Date(attendance_date) + 38

    next_update_date <- as.Date(last_update_date) + 14

    # API data date
    most_recent_fullweek_date <- latest_week_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1) |>
      dplyr::pull(reference_date)

    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0(
      "Data was last updated on ",
      last_update_date,
      " and is next expected to be updated on ",
      next_update_date,
      ". The latest full week of data was the week commencing ",
      most_recent_fullweek_date,
      "."
    )
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  output$update_dates2 <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    # most_recent_fullweek_date <- live_attendance_data_weekly() %>%
    #   pull(week_commencing)

    # last_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   17
    # # as.Date(attendance_date) + 24
    # # as.Date(attendance_date) + 31

    last_update_date <- reasons_data_version_info()$release_date

    # next_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   31
    # # as.Date(attendance_date) + 38

    next_update_date <- as.Date(last_update_date) + 14

    # API data date
    most_recent_fullweek_date <- latest_week_data() |>
      dplyr::arrange(desc(reference_date)) |>
      dplyr::slice(1) |>
      dplyr::pull(reference_date)


    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0(
      "Data was last updated on ",
      last_update_date,
      " and is next expected to be updated on ",
      next_update_date,
      ". The latest full week of data was the week commencing ",
      most_recent_fullweek_date,
      "."
    )
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  output$homepage_update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    # last_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   17
    # # as.Date(attendance_date) + 24
    # # as.Date(attendance_date) + 31

    last_update_date <- reasons_data_version_info()$release_date

    # next_update_date <- live_attendance_data_weekly() %>%
    #   pull(attendance_date) %>%
    #   as.Date(attendance_date) +
    #   31
    # # as.Date(attendance_date) + 38

    next_update_date <- as.Date(last_update_date) + 14

    # API data date
    most_recent_fullweek_date <- latest_week_data()$reference_date

    # paste0("Data was last updated on 2025-01-09 and is next expected to be updated on 2025-01-23. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0(
      "Data was last updated on ",
      last_update_date,
      " and is next expected to be updated on ",
      next_update_date,
      ". The latest full week of data was the week commencing ",
      most_recent_fullweek_date,
      "."
    )
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on 2024-08-08. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on 2025-01-09. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  # Creating reactive boxes ------------------------------------------------------------

  output$absence_rates_value_boxes <- renderUI({
    if (input$ts_choice == "latestweeks") {
      time_frame_text <- "latest full week"
      time_frame_filter <- "Week"
    } else {
      time_frame_text <- "year to date"
      time_frame_filter <- "Year to date"
    }

    tagList(
      # ✅ AUTHORISED
      tags$h5(tags$b("Authorised absence rate:"), style = "margin-bottom:8px;"),
      div(
        style = "width:70%; margin-bottom:15px;", # ✅ controls box size

        bslib::value_box(
          title = "",
          value = tags$div(
            # ✅ BIG NUMBER
            tags$div(
              style = "font-weight:700; font-size:42px; line-height:1;",
              reasons_data() |>
                filter(
                  geographic_level == input$geography_choice,
                  attendance_reason == "All authorised",
                  time_frame == time_frame_filter
                ) |>
                pull(session_percent) |>
                render_percents()
            ),

            # ✅ TEXT UNDER NUMBER
            tags$div(
              style = "font-size:15px; margin-top:4px;",
              time_frame_text
            )
          ),
          theme = value_box_theme(bg = "#1d70b8")
        )
      ),

      # ✅ UNAUTHORISED
      tags$h5(tags$b("Unauthorised absence rate:"), style = "margin-top:20px; margin-bottom:8px;"),
      div(
        style = "width:70%; margin-bottom:10px;", # ✅ same width

        bslib::value_box(
          title = "",
          value = tags$div(
            # ✅ BIG NUMBER
            tags$div(
              style = "font-weight:700; font-size:42px; line-height:1;",
              reasons_data() |>
                filter(
                  geographic_level == input$geography_choice,
                  attendance_reason == "All unauthorised",
                  time_frame == time_frame_filter
                ) |>
                pull(session_percent) |>
                render_percents()
            ),

            # ✅ TEXT UNDER NUMBER
            tags$div(
              style = "font-size:15px; margin-top:4px;",
              time_frame_text
            )
          ),
          theme = value_box_theme(bg = "#1d70b8")
        )
      )
    )
  })


  # Creating reactive reasons and la comparison table ------------------------------------------------------------

  output$absence_auth_table_title <- renderUI({
    tags$h4(
      tags$b(
        paste0(
          "Reasons for absence in the ",
          tolower(time_frame_descriptors()$display_string)
        )
      )
    )
  })

  output$absence_auth_reasons_reactable <- renderGovReactable({
    govReactable(
      reasons_data() |>
        filter(
          geographic_level == input$geography_choice,
          time_frame == time_frame_descriptors()$filter_string,
          attendance_reason %in%
            c(
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
        mutate(
          attendance_reason = dplyr::case_when(
            attendance_reason == "Illness (i)" ~ "Illness",
            attendance_reason == "Medical dental (m)" ~ "Medical appointments",
            attendance_reason == "Religious observance (r)" ~ "Religious observance",
            attendance_reason == "Study leave (s)" ~ "Study leave",
            attendance_reason == "Mobile child (t)" ~ "Mobile child",
            attendance_reason == "Excluded (e)" ~ "Excluded",
            attendance_reason == "Temporary reduced timetable (c2)" ~ "Temporary reduced timetable",
            attendance_reason == "Other authorised (c)" ~ "Other authorised"
          ),
          session_percent = render_percents(session_percent)
        ) |>
        select(attendance_reason, session_percent) |>
        tidyr::pivot_wider(
          names_from = attendance_reason,
          values_from = session_percent
        )
    )
  })


  output$absence_unauth_reasons_reactable <- renderGovReactable({
    unauth_data <- reasons_data() |>
      filter(
        geographic_level == input$geography_choice,
        time_frame == time_frame_descriptors()$filter_string,
        attendance_reason %in%
          c(
            "Unauthorised holiday (g)",
            "Late after registers closed (u)",
            "Other unauthorised (o)",
            "No reason yet (n)"
          )
      ) |>
      mutate(
        attendance_reason = dplyr::case_when(
          attendance_reason == "Unauthorised holiday (g)" ~ "Holiday",
          attendance_reason == "Late after registers closed (u)" ~ "Late after registers closed",
          attendance_reason == "Other unauthorised (o)" ~ "Unauthorised other",
          attendance_reason == "No reason yet (n)" ~ "No reason yet"
        ),
        session_percent = render_percents(session_percent)
      ) |>
      select(attendance_reason, session_percent) |>
      tidyr::pivot_wider(
        names_from = attendance_reason,
        values_from = session_percent
      )

    govReactable(unauth_data)
  })

  output$absence_reasons_la_reactable <- renderGovReactable({
    data <- la_data() |>
      dplyr::select(-attendance_status, -attendance_type) |>
      tidyr::pivot_wider(
        names_from = attendance_reason,
        values_from = session_percent
      )

    data <- data |>
      mutate(
        `Overall absence` = suppressWarnings(as.numeric(`Overall absence`)),
        `All authorised` = suppressWarnings(as.numeric(`All authorised`)),
        `All unauthorised` = suppressWarnings(as.numeric(`All unauthorised`))
      )

    # ✅ ONLY THIS PART CHANGED
    fmt <- function(x) {
      ifelse(is.na(x), "—", paste0(round(x, 2), "%"))
    }

    data <- data |>
      mutate(
        `Overall absence rate` = fmt(`Overall absence`),
        `Authorised absence rate` = fmt(`All authorised`),
        `Unauthorised absence rate` = fmt(`All unauthorised`)
      )

    govReactable(
      data |>
        dplyr::select(
          Year = time_period,
          `Week commencing` = reference_date,
          `Region name` = reg_name,
          `Local authority name` = la_name,
          `Overall absence rate`,
          `Authorised absence rate`,
          `Unauthorised absence rate`
        ),
      searchable = TRUE
    )
  })


  # Tech guidance tables ----------------------------------------------------

  output$notesTableHeadlines <- function() {
    notesTableHeadlines[is.na(notesTableHeadlines)] <- " "

    kable(notesTableHeadlines, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = T) %>%
      column_spec(
        1,
        bold = T,
        extra_css = "vertical-align: top !important;"
      ) %>%
      column_spec(2, width_max = "40em")
  }

  output$notesTableReasons <- renderGovReactable({
    govReactable(notesTableReasons, page_size = 15)
  })

  # Data download button ---------------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Underlying_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      underlying_data <- EES_daily_data %>%
        filter(school_type %in% c("Primary", "Secondary", "Special", "Total"))
      write.csv(underlying_data, con, row.names = FALSE)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("Underlying_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      underlying_data <- EES_daily_data %>%
        filter(school_type %in% c("Primary", "Secondary", "Special", "Total"))
      write.csv(underlying_data, con, row.names = FALSE)
    }
  )


  # Create map function

  output$rates_map <- renderLeaflet({
    dat <- map_data()
    req(nrow(dat) > 0)

    # ✅ palettes (consistent with better project)
    overall_abs_pal <- colorBin(
      palette = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026", "#800026"),
      domain = dat$overall_absence_perc,
      bins = quantile(dat$overall_absence_perc, probs = seq(0, 1, 0.2), na.rm = TRUE),
      na.color = "#808080"
    )

    auth_abs_pal <- colorBin(
      palette = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026", "#800026"),
      domain = dat$authorised_absence_perc,
      bins = quantile(dat$authorised_absence_perc, probs = seq(0, 1, 0.2), na.rm = TRUE),
      na.color = "#808080"
    )

    unauth_abs_pal <- colorBin(
      palette = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026", "#800026"),
      domain = dat$unauthorised_absence_perc,
      bins = quantile(dat$unauthorised_absence_perc, probs = seq(0, 1, 0.2), na.rm = TRUE),
      na.color = "#808080"
    )

    # ✅ format %
    fmt_pct <- function(x) {
      ifelse(is.na(x), "—", sprintf("%0.2f%%", x))
    }

    # ✅ choose measure (same behaviour as your good project)
    if (input$measure_choice == "Overall") {
      values <- dat$overall_absence_perc
      pal <- overall_abs_pal
      label_name <- "Overall absence"
    } else if (input$measure_choice == "Authorised") {
      values <- dat$authorised_absence_perc
      pal <- auth_abs_pal
      label_name <- "Authorised absence"
    } else {
      values <- dat$unauthorised_absence_perc
      pal <- unauth_abs_pal
      label_name <- "Unauthorised absence"
    }

    # ✅ ranking helper
    make_rank <- function(v) {
      valid <- !is.na(v)
      r <- rep(NA_integer_, length(v))
      r[valid] <- rank(v[valid], ties.method = "min")
      list(rank = r, total = sum(valid))
    }

    ranks <- make_rank(values)

    label_html <- lapply(seq_len(nrow(dat)), function(i) {
      chip_col <- pal(values[i])

      rk_txt <- if (is.na(values[i])) {
        "Suppressed / missing"
      } else {
        paste0(ranks$rank[i], " / ", ranks$total)
      }

      htmltools::HTML(paste0(
        "<div style='font-family:Arial;'>",
        "<strong>", dat$la_name[i], "</strong><br/>",
        "<span style='display:inline-block;width:10px;height:10px;background:",
        chip_col, ";margin-right:6px;border:1px solid #ccc;'></span>",
        label_name, ": <strong>", fmt_pct(values[i]), "</strong><br/>",
        "<span style='font-size:12px;color:#666;'>Rank ", rk_txt, "</span>",
        "</div>"
      ))
    })


    leaflet(dat) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~ pal(values),
        weight = 0.8,
        opacity = 1,
        color = "#4d4d4d",
        fillOpacity = 0.78,
        highlight = highlightOptions(
          weight = 2,
          color = "#1f1f1f",
          fillOpacity = 0.88,
          bringToFront = TRUE
        ),
        label = label_html,
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px"
        )
      ) |>
      addLegend(
        colors = c("#fecc5c", "#fd8d3c", "#f03b20", "#bd0026", "#800026", "#808080"),
        opacity = 1,
        position = "topright",
        labels = c("Lowest absence rates", "", "", "", "Highest absence rates", "Suppressed data")
      ) |>
      setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
  })


  output$map_title <- renderText({
    paste0(
      input$school_choice,
      " state-funded schools: ",
      str_to_lower(input$measure_choice),
      " absence rates by local authority"
    )
  })
}

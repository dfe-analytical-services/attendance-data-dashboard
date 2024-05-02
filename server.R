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

  # Cookie consent scripts
  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyjs::show(id = "cookieMain")
      } else {
        shinyjs::hide(id = "cookieMain")
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    } else {
      shinyjs::hide(id = "cookieMain")
    }
  })

  # Need these set of observeEvent to create a path through the cookie banner
  observeEvent(input$cookieAccept, {
    msg <- list(
      name = "dfe_analytics",
      value = "granted"
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    shinyjs::show(id = "cookieAcceptDiv")
    shinyjs::hide(id = "cookieMain")
  })

  observeEvent(input$cookieReject, {
    msg <- list(
      name = "dfe_analytics",
      value = "denied"
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    shinyjs::show(id = "cookieRejectDiv")
    shinyjs::hide(id = "cookieMain")
  })

  observeEvent(input$hideAccept, {
    shinyjs::toggle(id = "cookieDiv")
  })

  observeEvent(input$hideReject, {
    shinyjs::toggle(id = "cookieDiv")
  })

  observeEvent(input$remove, {
    shinyjs::toggle(id = "cookieMain")
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      "Cookies consent has not been confirmed."
    }
  })

  observeEvent(input$cookieLink, {
    # Need to link here to where further info is located.  You can
    # updateTabsetPanel to have a cookie page for instance
    updateTabsetPanel(session, "navlistPanel", selected = "Support and feedback")
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

  # Local authority geographies

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

  # observe({
  #   if (input$dash == "la comparisons") {
  #     updateSelectInput(session, "geography_choice",
  #                       choices = c("National","Regional","Local authority"),
  #                       selected = "National"
  #     )
  #   }
  # })

  # Reactive dates for dropdown

  regionReactive <- reactive({
    list(input$geography_choice, input$region_choice)
  })

  laReactive <- reactive({
    list(input$geography_choice, input$la_choice)
  })

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

  # Daily data
  live_attendance_data_daily <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Daily"
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Daily"
      ) %>% filter(time_identifier == max(time_identifier))
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Daily"
      ) %>% filter(time_identifier == max(time_identifier))
    } else {
      NA
    }
  })


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
          auth_grt_perc = auth_grt_perc / 100,
          auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
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
          auth_grt_perc = auth_grt_perc / 100,
          auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
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
          auth_grt_perc = auth_grt_perc / 100,
          auth_holiday_perc = auth_holiday_perc / 100,
          auth_excluded_perc = auth_excluded_perc / 100,
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
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # pa_perc = pa_perc / 100
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
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # pa_perc = pa_perc / 100
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
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
        auth_grt_perc = auth_grt_perc / 100,
        auth_holiday_perc = auth_holiday_perc / 100,
        auth_excluded_perc = auth_excluded_perc / 100,
        auth_other_perc = auth_other_perc / 100,
        unauth_hol_perc = unauth_hol_perc / 100,
        unauth_late_registers_closed_perc = unauth_late_registers_closed_perc / 100,
        unauth_oth_perc = unauth_oth_perc / 100,
        unauth_not_yet_perc = unauth_not_yet_perc / 100
        # pa_perc = pa_perc / 100
      )
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
      # filter(time_identifier == max(time_identifier) - 1) %>%
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

  output$absence_rates_daily_plot <- renderPlotly({
    validate(need(nrow(live_attendance_data_daily()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_daily()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_rates_weekly <- live_attendance_data_daily() %>%
      arrange(attendance_date)

    ts_plot <- plot_ly(
      absence_rates_weekly,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
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
        x = ~attendance_date,
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
        x = ~attendance_date,
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
      xaxis = list(title = "Date", tickvals = ~attendance_date, zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
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
      title = newtitle_daily(),
      font = t
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

  output$absence_reasons_timeseries_plot <- renderPlotly({
    validate(need(nrow(live_attendance_data_ts()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ts()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_reasons_ytd <- live_attendance_data_ts()

    reasons_ts_plot <- plot_ly(
      absence_reasons_ytd,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~illness_perc,
        line = list(color = "#12436D"),
        marker = list(color = "#12436D"),
        name = "Illness",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~appointments_perc,
        line = list(color = "#28A197"),
        marker = list(color = "#28A197"),
        name = "Medical appointments",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~unauth_hol_perc,
        line = list(color = "#F46A25"),
        marker = list(color = "#F46A25"),
        name = "Unauthorised holiday",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~week_commencing,
        y = ~unauth_oth_perc,
        line = list(color = "#801650"),
        marker = list(color = "#801650"),
        name = "Unauthorised other",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      config(displayModeBar = FALSE)

    reasons_ts_plot <- reasons_ts_plot %>% layout(
      xaxis = list(
        title = "Week commencing",
        tickvals = ~week_commencing,
        zeroline = T,
        zerolinewidth = 2,
        zerolinecolor = "Grey",
        zerolinecolor = "#ffff",
        zerolinewidth = 2
      ),
      yaxis = list(
        rangemode = "tozero",
        title = "Absence rate (%)",
        zeroline = T, zerolinewidth = 2,
        zerolinecolor = "Grey",
        zerolinecolor = "#ffff",
        zerolinewidth = 2
      ),
      hovermode = "x unified",
      legend = list(
        font = list(size = 11),
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5
      ),
      title = newtitle_reasonsweekly(),
      font = t
    )

    reasons_ts_plot <- reasons_ts_plot %>% layout(
      xaxis = list(
        tickmode = "linear",
        tick0 = "2022-09-12",
        # dtick = "M1"
        dtick = 86400000 * 14
      ),
      margin = list(t = 80)
    )
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

  output$absence_reasons_daily_plot <- renderPlotly({
    validate(need(nrow(live_attendance_data_daily()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_daily()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_rates_weekly <- live_attendance_data_daily() %>%
      arrange(attendance_date)

    ts_plot <- plot_ly(
      absence_rates_weekly,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~illness_perc,
        line = list(color = "#12436D"),
        marker = list(color = "#12436D"),
        name = "Illness",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~appointments_perc,
        line = list(color = "#28A197"),
        marker = list(color = "#28A197"),
        name = "Medical appointments",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauth_hol_perc,
        line = list(color = "#F46A25"),
        marker = list(color = "#F46A25"),
        name = "Unauthorised holiday",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauth_oth_perc,
        line = list(color = "#801650"),
        marker = list(color = "#801650"),
        name = "Unauthorised other",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      config(displayModeBar = FALSE)

    ts_plot <- ts_plot %>% layout(
      xaxis = list(title = "Date", tickvals = ~attendance_date, zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
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
      title = newtitle_reasonsdaily(),
      font = t
    )
  })

  # Creating reactive titles ------------------------------------------------------------

  # timeseries chart reactive title
  output$headline_ts_chart_title <- renderText({
    paste0("Overall, authorised and unauthorised absence rates across the ", str_to_lower(reactive_period_selected()))
  })

  # headline bullet reactive titles
  output$headline_bullet_title_nat <- renderText({
    paste0("Headline figures for the ", str_to_lower(reactive_period_selected()), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level")
  })

  output$headline_bullet_title_reg <- renderText({
    paste0("Headline figures for the ", str_to_lower(reactive_period_selected()), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ")")
  })

  output$headline_bullet_title_la <- renderText({
    paste0("Headline figures for the ", str_to_lower(reactive_period_selected()), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ", ", input$la_choice, ")")
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
    # paste0(scales::comma(schools_count), " schools provided information on the latest full day of data prior to half-term, i.e. ", schools_count_date)
    paste0(scales::comma(schools_count), " schools provided information on the latest full day of data, i.e. ", schools_count_date)
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

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("For this breakdown, in the latest week ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools submitted data, though this has varied throughout the year-to-date.")
  })

  output$school_count_proportion_weekly2 <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- live_attendance_data_weekly() %>%
      # count_prop_week <- live_attendance_data_weekly_pre_ht() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("For this breakdown, in the latest week ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools submitted data, though this has varied throughout the year-to-date.")
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

    # paste0("For this breakdown, in the week prior to half term there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools opted-in, though this has varied throughout the year-to-date. This figure is not shown for the latest week due to half-term impacting upon number of schools reporting.")
    paste0("This number is approximately ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of the number of schools participating in the School Census. As schools opt in to sharing of data, the number of schools reporting may change over time.")
  })

  # Proportion of schools in census figures are generated from - year to date
  output$school_count_proportion_ytd <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    count_prop_week <- live_attendance_data_ytd() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    paste0("For this breakdown, across the year-to-date there were ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 0), "% of schools opted-in.")
  })

  # Headline attendance latest week
  # Read in weekly data at reactively selected level and compare against weekly data at level above (compare reg to nat, la to reg)
  # Bullet for national level
  output$weekly_attendance_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(attendance_perc) %>% round(digits = 1),
      "% of sessions were recorded as attending"
    )
  })

  # Bullet for regional level
  output$weekly_attendance_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    weekly_headline_att <- live_attendance_data_weekly() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(weekly_overall_attendance_perc = (sum(present_sessions) / sum(possible_sessions)) * 100)

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as attending in ", input$region_choice, " (compared to ", live_attendance_data_weekly_natcomp() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$weekly_attendance_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as attending in ", input$la_choice, " (compared to ", live_attendance_data_weekly_regcomp() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })

  # Headline attendance ytd
  # Read in weekly data at reactively selected level and compare against weekly data at level above (compare reg to nat, la to reg)
  # Bullet for national level
  output$ytd_attendance_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(attendance_perc) %>% round(digits = 1),
      "% of sessions were recorded as attending"
    )
  })

  # Bullet for regional level
  output$ytd_attendance_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as attending in ", input$region_choice, " (compared to ", live_attendance_data_ytd_natcomp() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$ytd_attendance_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as attending in ", input$la_choice, " (compared to ", live_attendance_data_ytd_regcomp() %>%
        pull(attendance_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })


  # Headline absence latest week
  # Read in weekly data at reactively selected level and compare against weekly data at level above (compare reg to nat, la to reg)
  # Bullet for national level
  output$weekly_absence_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(overall_absence_perc) %>% round(digits = 1),
      "% of sessions were recorded as absence"
    )
  })

  # Bullet for regional level
  output$weekly_absence_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    weekly_headline_abs <- live_attendance_data_weekly() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(weekly_overall_absence_perc = (sum(overall_absence) / sum(possible_sessions)) * 100)

    weekly_headline_abs_comp_nat <- live_attendance_data_weekly_natcomp() %>%
      group_by(time_period, time_identifier, geographic_level) %>%
      mutate(weekly_overall_absence_perc = (sum(overall_absence) / sum(possible_sessions)) * 100)

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as absence in ", input$region_choice, " (compared to ", live_attendance_data_weekly_natcomp() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$weekly_absence_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as absence in ", input$la_choice, " (compared to ", live_attendance_data_weekly_regcomp() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })


  # Headline absence ytd
  # Bullet for national level
  output$ytd_absence_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(overall_absence_perc) %>% round(digits = 1),
      "% of sessions were recorded as absence"
    )
  })

  # Bullet for regional level
  output$ytd_absence_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as absence in ", input$region_choice, " (compared to ", live_attendance_data_ytd_natcomp() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$ytd_absence_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as absence in ", input$la_choice, " (compared to ", live_attendance_data_ytd_regcomp() %>%
        pull(overall_absence_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })


  # Headline illness absence latest week
  # Bullet for national level
  output$weekly_illness_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(illness_perc) %>% round(digits = 1),
      "% of sessions were recorded as illness"
    )
  })

  # Bullet for regional level
  output$weekly_illness_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as illness in ", input$region_choice, " (compared to ", live_attendance_data_weekly_natcomp() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$weekly_illness_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as illness in ", input$la_choice, " (compared to ", live_attendance_data_weekly_regcomp() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })


  # Headline illness absence ytd
  # Bullet for national level
  output$ytd_illness_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(illness_perc) %>% round(digits = 1),
      "% of sessions were recorded as illness"
    )
  })

  # Bullet for regional level
  output$ytd_illness_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as illness in ", input$region_choice, " (compared to ", live_attendance_data_ytd_natcomp() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions at national level)"
    )
  })

  # Bullet for LA level
  output$ytd_illness_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions were recorded as illness in ", input$la_choice, " (compared to ", live_attendance_data_ytd_regcomp() %>%
        pull(illness_perc) %>%
        round(digits = 1),
      "% of sessions in ", input$region_choice, ")"
    )
  })


  # Headline persistent absence ytd
  # Bullet for national level
  output$ytd_pa_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(pa_perc) %>% round(digits = 1),
      "% of pupils were recorded as persistently absent"
    )
  })

  # Bullet for regional level
  output$ytd_pa_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(pa_perc) %>%
        round(digits = 1),
      "% of pupils were recorded as persistently absent in ", input$region_choice, " (compared to ", live_attendance_data_ytd_natcomp() %>%
        pull(pa_perc) %>%
        round(digits = 1),
      "% of pupils at national level)"
    )
  })

  # Bullet for LA level
  output$ytd_pa_rate_la <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    paste0(
      "• ", live_attendance_data_ytd() %>%
        pull(pa_perc) %>%
        round(digits = 1),
      "% of pupils were recorded as persistently absent in ", input$la_choice, " (compared to ", live_attendance_data_ytd_regcomp() %>%
        pull(pa_perc) %>%
        round(digits = 1),
      "% of pupils in ", input$region_choice, ")"
    )
  })


  # Creating reactive dates for text ------------------------------------------------------------

  # latest full week
  output$headline_update_date <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- as.Date(last_update_date) + 17
    # last_update_date <- as.Date(last_update_date) + 24
    # last_update_date <- as.Date(last_update_date) + 31


    paste0("Data was last updated on ", last_update_date, ".")
    # paste0("Data was last updated on 2024-04-18")
  })

  output$la_clarity_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    # paste0("Data on this tab relates to the week commencing 2023-10-16")
    paste0("Data on this tab relates to the week commencing ", most_recent_fullweek_date, ".")
  })

  output$update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- as.Date(last_update_date) + 17
    # last_update_date <- as.Date(last_update_date) + 24
    # last_update_date <- as.Date(last_update_date) + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    next_update_date <- as.Date(next_update_date) + 31
    # next_update_date <- as.Date(next_update_date) + 38

    # paste0("Data was last updated on 2024-04-18 and is next expected to be updated on 2024-05-02. The latest full week of data for this breakdown was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  output$update_dates2 <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- as.Date(last_update_date) + 17
    # last_update_date <- as.Date(last_update_date) + 24
    # last_update_date <- as.Date(last_update_date) + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    next_update_date <- as.Date(next_update_date) + 31
    # next_update_date <- as.Date(next_update_date) + 38

    # paste0("Data was last updated on 2024-04-18 and is next expected to be updated on 2024-05-02. The latest full week of data for this breakdown was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  output$homepage_update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(week_commencing)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date() + 17
    # as.Date() + 24
    # as.Date() + 31

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date() + 31
    # as.Date() + 38

    # paste0("Data was last updated on 2024-04-18 and is next expected to be updated on 2024-05-02. The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
    # paste0("Data was last updated on ", last_update_date, ". The latest full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  # Creating reactive boxes ------------------------------------------------------------

  # daily, weekly and ytd overall absence rate


  # weekly overall absence rate
  output$headline_absence_rate_weekly <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    overall_absence_rate_weekly_headline <- live_attendance_data_weekly()
    pull(overall_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_weekly_headline, "%"),
      paste0("Latest full week"),
      color = "blue"
    )
  })

  # ytd overall absence rate
  output$headline_absence_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    overall_absence_rate_ytd_headline <- live_attendance_data_ytd() %>%
      pull(overall_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })

  # daily, weekly and ytd auth absence rate


  # weekly auth absence rate
  output$headline_auth_rate_weekly <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    overall_auth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(authorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_weekly_headline, "%"),
      paste0("Latest full week"),
      color = "blue"
    )
  })

  # ytd auth absence rate
  output$headline_auth_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    overall_auth_rate_ytd_headline <- live_attendance_data_ytd() %>%
      pull(authorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })

  # daily, weekly and ytd unauth absence rate


  # weekly unauth absence rate
  output$headline_unauth_rate_weekly <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))
    validate(need(live_attendance_data_weekly()$num_schools > 1, ""))

    overall_unauth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(unauthorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_weekly_headline, "%"),
      paste0("Latest full week"),
      color = "blue"
    )
  })

  # ytd unauth absence rate
  output$headline_unauth_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))
    validate(need(live_attendance_data_ytd()$num_schools > 1, ""))

    overall_unauth_rate_ytd_headline <- live_attendance_data_ytd() %>%
      pull(unauthorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })

  # Creating reactive reasons and la comparison table ------------------------------------------------------------

  # authorised reasons weekly
  output$absence_auth_reasons_table <- renderDT({
    validate(need(nrow(live_attendance_data_weekly_reasons_tables()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly_reasons_tables()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_auth_reasons_dt <- live_attendance_data_weekly_reasons_tables() %>%
      dplyr::select(illness_perc, appointments_perc, auth_religious_perc, auth_study_perc, auth_grt_perc, auth_holiday_perc, auth_excluded_perc, auth_other_perc) %>%
      rename(
        "Illness" = illness_perc,
        "Medical or dental appointments" = appointments_perc,
        "Religious observance" = auth_religious_perc,
        "Study leave" = auth_study_perc,
        "Traveller" = auth_grt_perc,
        "Holiday" = auth_holiday_perc,
        "Excluded" = auth_excluded_perc,
        "Other" = auth_other_perc
      )

    absence_auth_reasons_dt <- datatable(absence_auth_reasons_dt,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(
        scrollX = TRUE,
        ordering = F,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 0:7))
      )
    ) %>%
      formatPercentage(c(0:7), 1)
  })

  # authorised reasons ytd
  output$absence_auth_reasons_table_ytd <- renderDT({
    validate(need(nrow(live_attendance_data_ytd_reasons_tables()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd_reasons_tables()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_auth_reasons_ytd_dt <- live_attendance_data_ytd_reasons_tables() %>%
      dplyr::select(illness_perc, appointments_perc, auth_religious_perc, auth_study_perc, auth_grt_perc, auth_holiday_perc, auth_excluded_perc, auth_other_perc) %>%
      rename(
        "Illness" = illness_perc,
        "Medical or dental appointments" = appointments_perc,
        "Religious observance" = auth_religious_perc,
        "Study leave" = auth_study_perc,
        "Traveller" = auth_grt_perc,
        "Holiday" = auth_holiday_perc,
        "Excluded" = auth_excluded_perc,
        "Other" = auth_other_perc
      )

    absence_auth_reasons_ytd_dt <- datatable(absence_auth_reasons_ytd_dt,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(
        scrollX = TRUE,
        ordering = F,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 0:7))
      )
    ) %>%
      formatPercentage(c(0:7), 1)
  })

  # unauthorised reasons weekly
  output$absence_unauth_reasons_table <- renderDT({
    validate(need(nrow(live_attendance_data_weekly_reasons_tables()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_weekly_reasons_tables()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_unauth_reasons_dt <- live_attendance_data_weekly_reasons_tables() %>%
      dplyr::select(unauth_hol_perc, unauth_late_registers_closed_perc, unauth_oth_perc, unauth_not_yet_perc) %>%
      rename(
        "Holiday" = unauth_hol_perc,
        "Late after registers closed" = unauth_late_registers_closed_perc,
        "Other" = unauth_oth_perc,
        "No reason yet" = unauth_not_yet_perc
      )

    absence_unauth_reasons_dt <- datatable(absence_unauth_reasons_dt,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(
        scrollX = TRUE,
        ordering = F,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 0:3))
      )
    ) %>%
      formatPercentage(c(0:3), 1)
  })

  # unauthorised reasons ytd
  output$absence_unauth_reasons_table_ytd <- renderDT({
    validate(need(nrow(live_attendance_data_ytd_reasons_tables()) > 0, "There is no data available for this breakdown at present"))
    validate(need(live_attendance_data_ytd_reasons_tables()$num_schools > 1, "This data has been suppressed due to a low number of schools at this breakdown"))

    absence_unauth_reasons_ytd_dt <- live_attendance_data_ytd_reasons_tables() %>%
      dplyr::select(unauth_hol_perc, unauth_late_registers_closed_perc, unauth_oth_perc, unauth_not_yet_perc) %>%
      rename(
        "Holiday" = unauth_hol_perc,
        "Late after registers closed" = unauth_late_registers_closed_perc,
        "Other" = unauth_oth_perc,
        "No reason yet" = unauth_not_yet_perc
      )

    absence_unauth_reasons_ytd_dt <- datatable(absence_unauth_reasons_ytd_dt,
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(
        scrollX = TRUE,
        ordering = F,
        searching = FALSE,
        lengthChange = FALSE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = 0:3))
      )
    ) %>%
      formatPercentage(c(0:3), 1)
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
      rate_map <- mapdata_shaped_type() %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~ overall_abs_pal(overall_absence_perc),
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
          label = ~overall_label,
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
    } else if (input$measure_choice == "Authorised") {
      rate_map <- mapdata_shaped_type() %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~ auth_abs_pal(authorised_absence_perc),
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
          label = ~auth_label,
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
    } else if (input$measure_choice == "Unauthorised") {
      rate_map <- mapdata_shaped_type() %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~ unauth_abs_pal(unauthorised_absence_perc),
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
          label = ~unauth_label,
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
    }

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

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}

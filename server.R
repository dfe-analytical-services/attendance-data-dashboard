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

  # Overarching geographic levels
  geog_levels <- reactive({
    geog_lookup %>%
      dplyr::select(geographic_level) %>%
      unique() %>%
      as.data.table()
  })

  output$levels_filtered <- renderUI({
    selectInput(
      inputId = "geography_choice",
      label = "Choose geographic breakdown level:",
      choices = geog_levels(),
      selected = head(geog_levels, 1)
    )
  })


  # Regional geographies
  reg_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice) %>%
      dplyr::select(region_name) %>%
      unique() %>%
      as.data.table()
  })

  observe({
    if (input$geography_choice != "National") {
      reg_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice) %>%
        dplyr::select(region_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "region_choice",
      choices = reg_geog()
    )
  })

  output$reg_filtered <- renderUI({
    selectInput(
      inputId = "region_choice",
      label = "Choose region:",
      choices = reg_geog(),
      selected = head(reg_geog, 1)
    )
  })


  # Local authority geographies
  la_geog <- reactive({
    geog_lookup %>%
      dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
      dplyr::select(la_name) %>%
      unique() %>%
      as.data.table()
  })

  observe({
    if (input$geography_choice == "Local authority") {
      la_geog <- geog_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice, region_name == input$region_choice) %>%
        dplyr::select(la_name) %>%
        unique() %>%
        as.data.table()
    }
    updateSelectInput(session, "la_choice",
      choices = la_geog()
    )
  })

  output$la_filtered <- renderUI({
    selectInput(
      inputId = "la_choice",
      label = "Choose local authority:",
      choices = la_geog(),
      selected = head(la_geog, 1)
    )
  })




  # School types
  schools <- reactive({
    if (input$dash == "la comparisons") {
      (school_type_lookup %>%
        dplyr::filter(geographic_level == "Local authority"))$school_type %>%
        unique()
    } else {
      (school_type_lookup %>%
        dplyr::filter(geographic_level == input$geography_choice))$school_type %>%
        unique()
    }
  })

  observe({
    choicesSchools <- schools()
    updateSelectInput(session, "school_choice",
      choices = schools()
    )
  })

  output$schools_filtered <- renderUI({
    selectInput(
      inputId = "school_choice",
      label = "Choose school type:",
      choices = schools(),
      selected = head(schools, 1)
    )
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
        time_identifier == (max(time_identifier)),
        breakdown == "Daily"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Daily"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Daily"
      )
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
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
      )
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
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
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
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
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
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        time_identifier == (max(time_identifier)),
        breakdown == "Weekly"
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
      time_identifier == (max(time_identifier)),
      breakdown == "Weekly"
    ) %>%
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
      time_identifier == (max(time_identifier)),
      breakdown == "Weekly"
    )
  })

  live_attendance_data_weekly_regcomp <- reactive({
    filter(
      attendance_data, geographic_level == "Regional",
      region_name == input$region_choice,
      school_type == input$school_choice,
      time_period == max(time_period),
      time_identifier == (max(time_identifier)),
      breakdown == "Weekly"
    )
  })


  # Ytd data for headline bullet reactive comparisons
  live_attendance_data_ytd_natcomp <- reactive({
    filter(
      attendance_data, geographic_level == "National",
      school_type == input$school_choice,
      time_period == max(time_period),
      breakdown == "YTD"
    )
  })

  live_attendance_data_ytd_regcomp <- reactive({
    filter(
      attendance_data, geographic_level == "Regional",
      region_name == input$region_choice,
      school_type == input$school_choice,
      time_period == max(time_period),
      breakdown == "YTD"
    )
  })


  # Full weekly timeseries for most recent year
  live_attendance_data_ts <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "Weekly"
      )
    } else {
      NA
    }
  })

  # Full timeseries for most recent year
  live_attendance_data_ytd <- reactive({
    if (input$geography_choice == "National") {
      dplyr::filter(
        attendance_data, geographic_level == "National",
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Regional") {
      dplyr::filter(
        attendance_data, geographic_level == "Regional",
        region_name == input$region_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "YTD"
      )
    } else if (input$geography_choice == "Local authority") {
      dplyr::filter(
        attendance_data, geographic_level == "Local authority",
        region_name == input$region_choice,
        la_name == input$la_choice,
        school_type == input$school_choice,
        time_period == max(time_period),
        breakdown == "YTD"
      )
    } else {
      NA
    }
  })


  # Full timeseries for most recent year response rates - non-reactive
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
      )

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

    absence_rates_ytd <- live_attendance_data_ts()

    ts_plot <- plot_ly(
      absence_rates_ytd,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~overall_absence_perc,
        line = list(color = "black"),
        marker = list(color = "black"),
        name = "Overall absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~authorised_absence_perc,
        line = list(color = "steelblue"),
        marker = list(color = "steelblue"),
        name = "Authorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauthorised_absence_perc,
        line = list(color = "orangered"),
        marker = list(color = "orangered"),
        name = "Unauthorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      )

    ts_plot <- ts_plot %>% layout(
      xaxis = list(title = "Week commencing", tickvals = ~attendance_date, zeroline = T, zerolinewidth = 2, zerolinecolor = "Grey", zerolinecolor = "#ffff", zerolinewidth = 2),
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

    # ts_plot <- ts_plot %>% layout(
    #  xaxis = list(tickmode = 'linear',
    #               tick0 = "2021-08-01",
    #               dtick = "M1")
    # )
  })


  # Headline absence rates - most recent week chart
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

    absence_rates_weekly <- live_attendance_data_daily() %>%
      arrange(attendance_date)

    ts_plot <- plot_ly(
      absence_rates_weekly,
      type = "scatter", mode = "lines+markers"
    ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~overall_absence_perc,
        line = list(color = "black"),
        marker = list(color = "black"),
        name = "Overall absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~authorised_absence_perc,
        line = list(color = "steelblue"),
        marker = list(color = "steelblue"),
        name = "Authorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauthorised_absence_perc,
        line = list(color = "orangered"),
        marker = list(color = "orangered"),
        name = "Unauthorised absence rate",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      )

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

    absence_reasons_ytd <- live_attendance_data_ts()

    reasons_ts_plot <- plot_ly(
      absence_reasons_ytd,
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
        line = list(color = "	#F46A25"),
        marker = list(color = "	#F46A25"),
        name = "Unauthorised holiday",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauth_oth_perc,
        line = list(color = "	#3D3D3D"),
        marker = list(color = "	#3D3D3D"),
        name = "Unauthorised other",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      )

    reasons_ts_plot <- reasons_ts_plot %>% layout(
      xaxis = list(
        title = "Week commencing",
        tickvals = ~attendance_date,
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
        tick0 = "2021-08-01",
        dtick = "M1"
      ),
      margin = list(t = 80)
    )
  })


  # Reasons for absence - most recent week chart
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
        line = list(color = "	#F46A25"),
        marker = list(color = "	#F46A25"),
        name = "Unauthorised holiday",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      ) %>%
      add_trace(
        x = ~attendance_date,
        y = ~unauth_oth_perc,
        line = list(color = "	#3D3D3D"),
        marker = list(color = "	#3D3D3D"),
        name = "Unauthorised other",
        hovertemplate = "%{y:.1f}%",
        mode = "markers"
      )

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
    paste0("Overall, authorised and unauthorised absence rates across the ", str_to_lower(input$ts_choice))
  })

  # headline bullet reactive titles
  output$headline_bullet_title_nat <- renderText({
    paste0("Headline figures for the ", str_to_lower(input$ts_choice), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level")
  })

  output$headline_bullet_title_reg <- renderText({
    paste0("Headline figures for the ", str_to_lower(input$ts_choice), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ")")
  })

  output$headline_bullet_title_la <- renderText({
    paste0("Headline figures for the ", str_to_lower(input$ts_choice), ": ", str_to_lower(input$school_choice), " state-funded school attendance at ", str_to_lower(input$geography_choice), " level (", input$region_choice, ", ", input$la_choice, ")")
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

  # No. schools reporting on most recent day
  schools_count <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      time_identifier == max(time_identifier),
      day_number == "4"
    ) %>%
    pull(num_schools) %>%
    sum()

  schools_count_date <- attendance_data %>%
    filter(
      time_period == max(time_period),
      geographic_level == "National",
      school_type == "Total",
      time_identifier == max(time_identifier),
      day_number == "5"
    ) %>%
    pull(attendance_date)

  output$daily_schools_count <- renderText({
    paste0(scales::comma(schools_count), " schools provided information on the most recent full day of data, i.e. ", schools_count_date)
  })


  # Proportion of schools in census figures are generated from - most recent week
  output$school_count_proportion_weekly <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))

    count_prop_week <- live_attendance_data_weekly() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    paste0("For this breakdown, measures for the most recent week are produced based on ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 1), "% of schools")
  })

  # Proportion of schools in census figures are generated from - year to date
  output$school_count_proportion_ytd <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))

    count_prop_week <- live_attendance_data_ytd() %>%
      group_by(time_period, time_identifier, geographic_level, region_name, la_name) %>%
      mutate(proportion_schools_count = (num_schools / total_num_schools) * 100)

    paste0("For this breakdown, measures for the year to date are produced based on ", count_prop_week %>% pull(proportion_schools_count) %>% mean(na.rm = TRUE) %>% round(digits = 1), "% of schools")
  })

  # Headline attendance most recent week
  # Read in weekly data at reactively selected level and compare against weekly data at level above (compare reg to nat, la to reg)
  # Bullet for national level
  output$weekly_attendance_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(attendance_perc) %>% round(digits = 1),
      "% of sessions were recorded as attending"
    )
  })

  # Bullet for regional level
  output$weekly_attendance_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, "There is no data available for this breakdown at present"))

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

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(attendance_perc) %>% round(digits = 1),
      "% of sessions were recorded as attending"
    )
  })

  # Bullet for regional level
  output$ytd_attendance_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, "There is no data available for this breakdown at present"))

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


  # Headline absence most recent week
  # Read in weekly data at reactively selected level and compare against weekly data at level above (compare reg to nat, la to reg)
  # Bullet for national level
  output$weekly_absence_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(overall_absence_perc) %>% round(digits = 1),
      "% of sessions were recorded as absence"
    )
  })

  # Bullet for regional level
  output$weekly_absence_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))

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

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(overall_absence_perc) %>% round(digits = 1),
      "% of sessions were recorded as absence"
    )
  })

  # Bullet for regional level
  output$ytd_absence_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))

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


  # Headline illness absence most recent week
  # Bullet for national level
  output$weekly_illness_rate_nat <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))

    paste0(
      "• ", live_attendance_data_weekly() %>% pull(illness_perc) %>% round(digits = 1),
      "% of sessions were recorded as illness"
    )
  })

  # Bullet for regional level
  output$weekly_illness_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))

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

    paste0(
      "• ", live_attendance_data_ytd() %>% pull(illness_perc) %>% round(digits = 1),
      "% of sessions were recorded as illness"
    )
  })

  # Bullet for regional level
  output$ytd_illness_rate_reg <- renderText({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))

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





  # Creating reactive dates for text ------------------------------------------------------------

  # Most recent full week
  output$update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31

    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The most recent full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })

  output$update_dates2 <- renderText({
    validate(need(input$geography_choice != "", ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31

    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The most recent full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  output$homepage_update_dates <- renderText({
    validate(need(input$geography_choice != "", ""))

    most_recent_fullweek_date <- live_attendance_data_weekly() %>%
      pull(attendance_date)

    last_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 17

    next_update_date <- live_attendance_data_weekly() %>%
      pull(attendance_date) %>%
      as.Date(attendance_date) + 31

    paste0("Data was last updated on ", last_update_date, " and is next expected to be updated on ", next_update_date, ". The most recent full week of data was the week commencing ", most_recent_fullweek_date, ".")
  })


  # Creating reactive boxes ------------------------------------------------------------

  # daily, weekly and ytd overall absence rate


  # weekly overall absence rate
  output$headline_absence_rate_weekly <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_weekly()) > 0, ""))

    overall_absence_rate_weekly_headline <- live_attendance_data_weekly()
    pull(overall_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })

  # ytd overall absence rate
  output$headline_absence_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))

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

    overall_auth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(authorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })

  # ytd auth absence rate
  output$headline_auth_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))

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

    overall_unauth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(unauthorised_absence_perc) %>%
      round(digits = 1)

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })

  # ytd unauth absence rate
  output$headline_unauth_rate_ytd <- shinydashboard::renderValueBox({
    validate(need(nrow(live_attendance_data_ytd()) > 0, ""))

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

  # authorised reasons
  output$absence_auth_reasons_table <- renderDT({
    validate(need(nrow(live_attendance_data_weekly_reasons_tables()) > 0, "There is no data available for this breakdown at present"))

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

  # unauthorised reasons
  output$absence_unauth_reasons_table <- renderDT({
    validate(need(nrow(live_attendance_data_weekly_reasons_tables()) > 0, "There is no data available for this breakdown at present"))

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


  # absence reasons by local authority
  output$absence_reasons_la_table <- renderDT({
    absence_reasons_la <- live_attendance_data_weekly_las() %>%
      dplyr::select(time_period, time_identifier, attendance_date, region_name, la_name, overall_absence_perc, authorised_absence_perc, unauthorised_absence_perc) %>%
      arrange(desc(overall_absence_perc)) %>%
      rename(
        "Year" = time_period,
        "Week number" = time_identifier,
        "Week commencing" = attendance_date,
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

  output$notesTableReasons <- function() {
    notesTableReasons[is.na(notesTableReasons)] <- " "

    kable(notesTableReasons, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = T) %>%
      column_spec(1, bold = T, extra_css = "vertical-align: top !important;") %>%
      column_spec(2, width_max = "40em")
  }

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

  ## Reading in data ##########################################################

  # Read in shapefile and transform coordinates (because map reasons...)
  mapshape <- st_read("data/CTYUA_DEC_2021_UK_BUC.shp") %>% st_transform(crs = 4326)

  # Process the joined files to refine our 'mapdata', not pretty yet and mostly done just cos it's how its done in global...

  mapdata0 <- attendance_data %>%
    mutate(time_identifier = as.numeric(str_remove_all(time_identifier, "Week "))) %>%
    filter(time_identifier == max(time_identifier)) %>%
    filter(geographic_level == "Local authority") %>%
    filter(breakdown == "Weekly")


  mapdata <- mapdata0 %>%
    mutate(CTYUA21CD = new_la_code) %>% # renaming to match to shapefile later
    filter(!is.na(region_name), !is.na(la_name))

  mapdata <- mapdata %>%
    group_by(time_period, time_identifier, geographic_level, region_name, la_name, CTYUA21CD, school_type) %>%
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
  mapdata_shaped <- merge(mapshape, mapdata, by = "CTYUA21CD", duplicateGeoms = TRUE)

  # Create colour bins and palette labels --------------------------------------

  # Pull in the colours from another script
  source("R/gov_colours.R")

  # Create bins
  overall_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$overall_abs_perc, n = 5)

  auth_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$auth_abs_perc, n = 5)

  unauth_abs_pal <- colorQuantile(map_gov_colours, mapdata_shaped$unauth_abs_perc, n = 5)

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

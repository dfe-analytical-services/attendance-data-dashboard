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
    updateTabsetPanel(session, "navbar", selected = "headlines")
  })
  
  
  # Reactive geography levels 
  
  #levels
  geog_levels <- reactive({geog_lookup %>% select(geographic_level) %>% unique() %>% as.data.table()
  })
  
  output$levels_filtered <- renderUI({
    selectInput(inputId = "geography_choice",
                label = "Choose geographic breakdown level:",
                choices = geog_levels(),
                selected = head(geog_levels,1))
    
  })
  
  
  #regional
  reg_geog <- reactive({geog_lookup %>% dplyr::filter(geographic_level == input$geography_choice) %>% select(region_name) %>% unique() %>% as.data.table()
  })
  
  output$reg_filtered <- renderUI({
    selectInput(inputId = "region_choice",
                label = "Choose region:",
                choices = reg_geog(),
                selected = head(reg_geog,1))
    
  })
  
  
  #local authority
  la_geog <- reactive({geog_lookup %>% dplyr::filter(region_name == input$region_choice) %>% select(la_name) %>% unique() %>% as.data.table()
  })
  
  output$la_filtered <- renderUI({
    selectInput(inputId = "la_choice",
                label = "Choose local authority:",
                choices = la_geog(),
                selected = head(la_geog,1))
                
  })
  

  
  # Simple server stuff goes here ------------------------------------------------------------
  
  #Weekly data
  live_attendance_data_weekly <- reactive({

    if(input$geography_choice == "National") {
      dplyr::filter(attendance_data, geographic_level == "National",
                                        school_type == input$school_choice,
                                        time_period == "2021",
                                        time_identifier == "Week 3")}

    else if(input$geography_choice == "Regional") {
      dplyr::filter(attendance_data, geographic_level == "Regional",
                                        region_name == input$region_choice,
                                        school_type == input$school_choice,
                                        time_period == "2021",
                                        time_identifier == "Week 3")}

    else if(input$geography_choice == "Local authority") {
      dplyr::filter(attendance_data, geographic_level == "Local authority",
                                        region_name == input$region_choice,
                                        la_name == input$la_choice,
                                        school_type == input$school_choice,
                                        time_period == "2021",
                                        time_identifier == "Week 3")}

    else {NA}

  })
  
  #Daily data
  live_attendance_data_daily <- reactive({
    
    if(input$geography_choice == "National") {
      dplyr::filter(attendance_data, geographic_level == "National",
                    school_type == input$school_choice,
                    time_period == "2021",
                    date == max(date))}
    
    else if(input$geography_choice == "Regional") {
      dplyr::filter(attendance_data, geographic_level == "Regional",
                    region_name == input$region_choice,
                    school_type == input$school_choice,
                    time_period == "2021",
                    date == max(date))}
    
    else if(input$geography_choice == "Local authority") {
      dplyr::filter(attendance_data, geographic_level == "Local authority",
                    region_name == input$region_choice,
                    la_name == input$la_choice,
                    school_type == input$school_choice,
                    time_period == "2021",
                    date == max(date))}
    
    else {NA}
    
  })
  
  
  #Full timeseries for most recent year
  live_attendance_data_ts <- reactive({
    
    if(input$geography_choice == "National") {
      dplyr::filter(attendance_data, geographic_level == "National",
                    school_type == input$school_choice,
                    time_period == "2021")}
    
    else if(input$geography_choice == "Regional") {
      dplyr::filter(attendance_data, geographic_level == "Regional",
                    region_name == input$region_choice,
                    school_type == input$school_choice,
                    time_period == "2021")}
    
    else if(input$geography_choice == "Local authority") {
      dplyr::filter(attendance_data, geographic_level == "Local authority",
                    region_name == input$region_choice,
                    la_name == input$la_choice,
                    school_type == input$school_choice,
                    time_period == "2021")}
    
    else {NA}
    
  }) %>% bindCache(input$geography_choice, input$school_choice, input$region_choice, input$la_choice)
  
  
  # Define server logic required to draw a line graph
  output$absence_rates_timeseries_plot <- renderPlotly({
    ts_plot <- plot_ly(
      live_attendance_data_ts(), type = "scatter", mode = "lines"
    ) %>%
      add_trace(
        x = ~time_identifier,
        y = ~sess_overall_percent,
        line = list(color = "black"),
        name = "Overall absence rate"
      ) %>%
      add_trace(
        x = ~time_identifier,
        y = ~sess_authorised_percent,
        line = list(color = "steelblue"),
        name = "Authorised absence rate"
      ) %>%
      add_trace(
        x = ~time_identifier,
        y = ~sess_unauthorised_percent,
        line = list(color = "orangered"),
        name = "Unauthorised absence rate"
      )
    
    ts_plot <- ts_plot %>%  layout(
      xaxis = list(title = 'Week number'), 
      yaxis = list(title = 'Absence rate (%)'), 
      hovermode = "x unified",
      legend = list(orientation = 'h',
                    yanchor="bottom",
                    y=-0.5,
                    xanchor="left",
                    x=0)
    )
  })
  
  
  output$pa_timeseries_plot <- renderPlotly({
    plot_ly(live_attendance_data_ts(), x = ~time_identifier, y = ~enrolments_pa_10_exact_percent) %>%
      add_lines() %>%
      layout(xaxis = list(title = 'Week number'), 
             yaxis = list(title = 'Persistent absence rate (%)'), hovermode = "x unified")
  })
  
  # create headline absence rate text
  
  output$Table <- renderTable({
    live_attendance_data_weekly()
  })
  
  output$weekly_absence_rate <- renderText({
    paste0("• ",live_attendance_data_weekly() %>% pull(sess_overall_percent) %>% mean() %>% round(digits = 1), 
           "% of pupils were absent in the most recent full week.")
  })
  
  output$weekly_illness_rate <- renderText({
    paste0("• ",live_attendance_data_weekly() %>% pull(sess_auth_illness_rate) %>% mean() %>% round(digits = 1), 
           "% of pupils were absent in the most recent full week due to illness.")
  })
  
  output$ytd_pa_rate <- renderText({
    paste0("• ",live_attendance_data_ts() %>% pull(enrolments_pa_10_exact_percent) %>% mean() %>% round(digits = 1), 
           "% of pupils have missed at least 10% of sessions in the year to date.")
  })
  
  
  # make reactive dates to read in
  
  output$daily_dates <- renderText({
    paste0("The most recent full day of data was ",live_attendance_data_daily() %>% pull(date))
  })
  
  output$weekly_dates <- renderText({
    paste0("THIS NEEDS FIXING - The most recent full week of data was the week commencing  ",live_attendance_data_daily() %>% pull(date - 5))
  })
  
  
  # create headline absence boxes
  
  # daily, weekly and ytd overall absence rate
  output$headline_absence_rate_daily <- renderValueBox({
    
    overall_absence_rate_daily_headline <- live_attendance_data_daily() %>%
      pull(sess_overall_percent) %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_daily_headline, "%"),
      paste0("Most recent full day"),
      color = "blue"
    )
  })
  
  output$headline_absence_rate_weekly <- renderValueBox({
    
    overall_absence_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(sess_overall_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })
  
  output$headline_absence_rate_ytd <- renderValueBox({
    
    overall_absence_rate_ytd_headline <- live_attendance_data_ts() %>%
      pull(sess_overall_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })  
  
  # daily, weekly and ytd auth absence rate
  output$headline_auth_rate_daily <- renderValueBox({
    
    overall_auth_rate_daily_headline <- live_attendance_data_daily() %>%
      pull(sess_authorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_daily_headline, "%"),
      paste0("Most recent full day"),
      color = "blue"
    )
  })  
  
  output$headline_auth_rate_weekly <- renderValueBox({
    
    overall_auth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(sess_authorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })
  
  output$headline_auth_rate_ytd <- renderValueBox({
    
    overall_auth_rate_ytd_headline <- live_attendance_data_ts() %>%
      pull(sess_authorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })
  
  # daily, weekly and ytd unauth absence rate
  output$headline_unauth_rate_daily <- renderValueBox({
    
    overall_unauth_rate_daily_headline <- live_attendance_data_daily() %>%
      pull(sess_unauthorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_daily_headline, "%"),
      paste0("Most recent full day"),
      color = "blue"
    )
  })
  
  
  output$headline_unauth_rate_weekly <- renderValueBox({
    
    overall_unauth_rate_weekly_headline <- live_attendance_data_weekly() %>%
      pull(sess_unauthorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_weekly_headline, "%"),
      paste0("Most recent full week"),
      color = "blue"
    )
  })
  
  output$headline_unauth_rate_ytd <- renderValueBox({
    
    overall_unauth_rate_ytd_headline <- live_attendance_data_ts() %>%
      pull(sess_unauthorised_percent) %>%
      mean() %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_ytd_headline, "%"),
      paste0("Year to date"),
      color = "blue"
    )
  })  
  
  # Tech guidance tables ----------------------------------------------------
  
  output$notesTableHeadlines <- function() {
    notesTableHeadlines[is.na(notesTableHeadlines)] <- " "
    
    kable(notesTableHeadlines, "html", align = "l", escape = FALSE) %>%
      kable_styling(full_width = T) %>%
      column_spec(1, bold = T, extra_css = "vertical-align: top !important;") %>%
      column_spec(2, width_max = "40em") %>%
      column_spec(3, width_max = "40em")
  }
  
  # Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
}


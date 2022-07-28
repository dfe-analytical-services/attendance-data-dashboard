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
                choices = geog_levels())
    
  })
  
  
  #regional
  reg_geog <- reactive({geog_lookup %>% dplyr::filter(geographic_level == input$geography_choice) %>% select(region_name) %>% unique() %>% as.data.table()
  })
  
  output$reg_filtered <- renderUI({
    selectInput(inputId = "region_choice",
                label = "Choose region:",
                choices = reg_geog())
    
  })
  
  
  #local authority
  la_geog <- reactive({geog_lookup %>% dplyr::filter(region_name == input$region_choice) %>% select(la_name) %>% unique() %>% as.data.table()
  })
  
  output$la_filtered <- renderUI({
    selectInput(inputId = "la_choice",
                label = "Choose local authority:",
                choices = la_geog())
                
  })
  

  
  # Simple server stuff goes here ------------------------------------------------------------
  
  #National
  live_attendance_data <- reactive({

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
    
    # attendance_data %>% dplyr::filter(
    #   geographic_level == input$geography_choice,
    #   region_name == input$region_choice,
    #   la_name == input$la_choice,
    #   school_type == input$school_choice,
    #   time_period == "2021"
    
  })

  
  # Define server logic required to draw a line graph
  output$timeseries_plot <- renderPlotly({
    plot_ly(live_attendance_data_ts(), x = ~time_identifier, y = ~sess_overall_percent) %>%
      add_lines() %>%
      layout(xaxis = list(title = 'Week number'), 
             yaxis = list(title = 'Overall absence rate (%)'), hovermode = "x unified")
  })
  
  
  # create headline absence rate text
  
  output$Table <- renderTable({
    live_attendance_data()
  })
  
  output$absence_rate <- renderText({
    paste0("• ",live_attendance_data() %>% pull(sess_overall_percent) %>% round(digits = 1), 
           "% of pupils were absent in the most recent week.")
  })
  
  output$illness_rate <- renderText({
    paste0("• ",live_attendance_data() %>% pull(sess_auth_illness_rate) %>% round(digits = 1), 
           "% of pupils were absent in the most recent week due to illness.")
  })
  
  # create headline absence boxes
  
  output$headline_absence_rate <- renderValueBox({
    
    overall_absence_rate_headline <- live_attendance_data() %>%
      pull(sess_overall_percent) %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate_headline, "%"),
      paste0("Overall absence rate"),
      color = "blue"
    )
  })
  
  output$headline_auth_rate <- renderValueBox({
    
    overall_auth_rate_headline <- live_attendance_data() %>%
      pull(sess_authorised_percent) %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_auth_rate_headline, "%"),
      paste0("Authorised absence rate"),
      color = "blue"
    )
  })
  
  output$headline_unauth_rate <- renderValueBox({
    
    overall_unauth_rate_headline <- live_attendance_data() %>%
      pull(sess_unauthorised_percent) %>%
      round(digits = 1)
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_unauth_rate_headline, "%"),
      paste0("Unauthorised absence rate"),
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


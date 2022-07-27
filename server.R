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
  
  # Simple server stuff goes here ------------------------------------------------------------
  
  live_attendance_data <- reactive({
    attendance_data %>% filter(
      geographic_level == input$geography_choice,
      la_name == input$la_choice,
      region_name == input$region_choice,
      school_type == input$school_choice,
      time_period == "2021",
      time_identifier == "Week 3"
    )
  })
  

  live_attendance_data_ts <- reactive({
    attendance_data %>% filter(
      geographic_level == input$geography_choice,
      la_name == input$la_choice,
      region_name == input$region_choice,
      school_type == input$school_choice,
      time_period == "2021"
    )
  })
  
  # trying to define selective outputs
 
  
  # end
  
  # Define server logic required to draw a line graph
  output$timeseries_plot <- renderPlotly({
    plot_ly(live_attendance_data_ts(), x = ~time_identifier, y = ~sess_overall_percent) %>%
      add_lines() %>%
      layout(xaxis = list(title = 'Week number'), 
             yaxis = list(title = 'Overall absence rate (%)'), hovermode = "x unified")
  })
  
  
  # create headline absence rate text
  
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
  
  # Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
}


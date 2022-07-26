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
  

  
  # Define server logic required to draw a line graph
  output$testPlot <- renderPlot({
    ggplot(attendance_data %>% filter(la_name == input$la_name), 
           aes(time_identifier, sess_overall_percent)) + geom_line()
  })
  
  
  # create box for absence data
  output$absence_rate<- renderValueBox({
    
    overall_absence_rate <- live_attendance_data() %>%
      pull(sess_overall_percent)
      
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_absence_rate),
      paste0("of pupils were absent in the most recent week"),
      color = "blue"
    )
  })
  
  
  # Stop app ---------------------------------------------------------------------------------
  
  session$onSessionEnded(function() {
    stopApp()
  })
}


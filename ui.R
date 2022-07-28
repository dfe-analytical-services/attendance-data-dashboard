# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

ui <- function(input, output, session) {
  fluidPage(
    useShinyjs(),
    tags$head(includeHTML(("google-analytics.html"))),
    includeCSS("www/dfe_shiny_gov_style.css"),
    useShinydashboard(),
    title = "Attendance Data",
    # use_tota11y(), # accessibility layer for local testing
    
    # Set metadata for browser ==================================================
    
    tags$html(lang = "en"),
    # meta_general(
    #   application_name = "Pupil Attendance Data Dashboard",
    #   description = "Pupil absence in schools in England",
    #   robots = "index,follow",
    #   generator = "R-Shiny",
    #   subject = "Pupil Absence",
    #   rating = "General",
    #   referrer = "no-referrer"
    # ),
    
    # Set title for search engines
    HTML("<title>Pupil absence in schools in England data dashboard</title>"),
    
    # Navbar ====================================================================
    
    # This CSS sets the 4th item on the navbar to the right
    tagList(
      tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           }
                           ")))
    ),
    navbarPage("",
               id = "navbar",
               
               # Homepage tab ============================================================
               
               tabPanel(
                 "Homepage",
                 fluidPage(
                   fluidRow(
                     column(
                       12,
                       h1("Pupil absence in schools in England data dashboard"),
                       br("The Department for Education has been working alongside schools to collect attendance data directly from school management information systems. This has made it possible to view absence on a weekly basis in addition to published", 
                        a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms", "termly National Statistics"), "based on the school census."),
                       "This dashboard will be updated fortnightly with further measures and breakdowns being added when possible. The next iteration of this dashboard is likely to include persistent absence.",
                       br(),
                       br()
                     ),
                     
                     ## Left panel -------------------------------------------------------
                     
                     column(
                       6,
                       div(
                         div(
                           class = "panel panel-info",
                           div(
                             class = "panel-heading",
                             style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                             h2("Contents")
                           ),
                           div(
                             class = "panel-body",
                             tags$div(
                               title = "Headline information including overall absence, authorised absence and unauthorised absence",
                               h3(actionLink("link_to_headlines_tab", "Headlines")),
                               br("The headlines page includes information on overall absence, authorised absence and unauthorised absence, in addition to brief information on absence due to illness. Figures are based on information collected by schools and shared with the department."),
                               br(),
                               br("The chart shows the overall absence rate across all data collected in the year to date."),
                               br(),
                               br("Headline boxes show:"),
                               br("• Overall absence rate"),
                               "• Authorised absence rate",
                               br("• Unauthorised absence rate"),
                               br(),
                               br("It is possible to view breakdowns of these headline figures at National, Regional and Local Authority geographic levels, in addition to school type.")
                             ),
                             br("To view breakdowns of the headline figures other than the total, use the drop-down menu options on the left side of the page. You will need to select the geographic level prior to selecting other options below.")
                           )
                         )
                       ),
                     ),
                     
                     ## Right panel ------------------------------------------------------
                     
                     column(
                       6,
                       div(
                         div(
                           class = "panel panel-info",
                           div(
                             class = "panel-heading",
                             style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                             h2("Background Info")
                           ),
                           div(
                             class = "panel-body",
                             tags$div(
                               br("What do we want in here? Links to where to go for main publications? Do we want anything on key data notes e.g. how much data is lagged by or would that be better placed in a technical notes section etc?")
                             ),
                           )
                         )
                       )
                     )
                   )
                 )
               ),
               
               
               
               
               tabPanel(
                 value = "headlines",
                 "Headlines",
                 
                 # Define UI for application
                 
                 # Sidebar with la_name choice
                 sidebarLayout(
                   sidebarPanel(
                     width = 2,
                     # selectInput(inputId = "geography_choice",
                     #             label = "Choose geographic breakdown level:",
                     #             choices = attendance_data$geographic_level[!duplicated(attendance_data$geographic_level)]
                     # ),
                     uiOutput("levels_filtered"),
                     conditionalPanel(condition = "input.geography_choice == 'Regional'|| input.geography_choice == 'Local authority'", 
                                      uiOutput("reg_filtered")
                                      # selectInput(inputId = "region_choice",
                                      #             label = "Choose region:",
                                      #             choices = geog_lookup$region_name %>% unique()
                                      # )
                     ),
                     conditionalPanel(condition = "input.geography_choice == 'Local authority'",
                                      uiOutput("la_filtered")
                                      
                                      # selectInput(inputId = "la_choice",
                                      #             label = "Choose local authority:",
                                      #             choices = list(la_geog())
                                      
                     ),
                     
                     #tableOutput("Table"),
                     selectInput(inputId = "school_choice",
                                 label = "Choose school type:",
                                 choices = attendance_data$school_type[!duplicated(attendance_data$school_type)]
                     )
                   ),
                   
                   # Show a box with most recent absence data
                   mainPanel(
                     width = 10,
                     p(strong(paste0("Most recent absence data"))),
                     textOutput("absence_rate"),
                     textOutput("illness_rate"),
                     br(),
                     
                     fluidPage(
                       fluidRow(
                         column(width = 12, br()),
                         column(
                           6,
                           p(strong("Overall absence rates across the year to date")),
                           plotlyOutput("timeseries_plot")
                         ),
                         column(
                           6,
                           fluidRow(
                             column(
                               12,
                               p(strong(paste0("Summary of absence rates in the most recent week"))),
                               p("additional information"),
                               valueBoxOutput("headline_absence_rate", width = 6),
                               valueBoxOutput("headline_auth_rate", width = 6)
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               valueBoxOutput("headline_unauth_rate", width = 6),
                             )
                           ),
                         )
                       ),
                     )
                   ),
                 )
               ),
               
               
               # Create the tech notes-----------------
               tabPanel(
                 value = "technical_notes",
                 title = "Technical notes",
                 meta_general(
                   application_name = "School absence headlines",
                   description = "Headlines on pupil absence by school type and geographical breakdown in England",
                   robots = "index,follow",
                   generator = "R-Shiny",
                   subject = "Pupil absence in England",
                   rating = "General",
                   referrer = "no-referrer"
                 ),
                 h2("Technical notes"),
                 br("Use this dashboard to view pupil absence by school type and geographical breakdown in England"),
                 br("Week numbers refer to the year starting at January 1st and ending December 31st rather than following the academic year."),
                 br("Data is lagged by x weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late."),
                 br(),
                 tabBox(
                   title = "",
                   id = "tabs_tech_notes", width = "12",
                   tabPanel(
                     "Headlines",
                     tableOutput("notesTableHeadlines") # made in global.R file
                   ), # end of tabPanel
                 ) # end of tabBox
               ),
               
               
               # Create the accessibility statement-----------------
               tabPanel(
                 "Accessibility",
                 h2("Accessibility statement"),
                 br("This accessibility statement applies to the **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
                 h3("WCAG 2.1 compliance"),
                 br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
                 tags$div(tags$ul(
                   tags$li("uses colours that have sufficient contrast"),
                   tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
                   tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
                 )),
                 h3("Limitations"),
                 br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
                 tags$div(tags$ul(
                   tags$li("List"),
                   tags$li("known"),
                   tags$li("limitations, e.g."),
                   tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
                 )),
                 h3("Feedback"),
                 br(
                   "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
                   a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
                 )
               ), # End of accessibility tab
               # Support links ===========================================================
               
               tabPanel(
                 "Support and feedback",
                 support_links() # defined in R/supporting_links.R
               ),
               # Footer ====================================================================
               
               shinyGovstyle::footer(TRUE)
    ) # End of navBarPage
  )
}
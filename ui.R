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
                             p("It is possible to view breakdowns of both the headline figures and reasons for absence at National, Regional and Local Authority geographic levels, in addition to school type. To view breakdowns of these figures other than the total, use the drop-down menu options on the left side of the page. You will need to select the geographic level prior to selecting other options below."),
                             br(),
                             tags$div(
                               title = "Headline information on overall and persistent absence",
                               h3(actionLink("link_to_headlines_tab", "Headlines")),
                               p("The headlines tab includes information on overall absence, authorised, unauthorised and persistent absence in the most recent day, week and across the year to date."),
                               p("• Charts on this tab display overall, authorised, unauthorised and persistent absence rates across all data collected in the year to date."),
                               p("• Headline boxes show the overall absence rate in the most recent full day and week of data, as well as across the year to date."),
                             ),
                             br(),
                             tags$div(
                               title = "Reasons for absence including authorised and unauthorised, in addition to breakdowns of the above",
                               h3(actionLink("link_to_reasons_tab", "Reasons")),
                               p("The reasons tab includes information on authorised and unauthorised absence rates, and breakdowns into individual reasons for absence."),
                               p("• The chart on this tab displays absence rates associated with some of the most common absence reasons, including illness, holidays and appointments."),
                               p("• Headline boxes show the authorised and unauthorised absence rates in the most recent full day and week of data, as well as across the full year to date.")
                             ),
                             
                             
                             
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
                               p("The statistics presented in this dashboard have been developed using management information from schools who have opted in to the data collection process."),
                               textOutput("daily_schools_count"),
                               br(),
                               h3("Useful links"),
                               p("This dashboard has been developed as an accompaniment to National statistics produced annually by the department on pupil absence across the full year and during spring and autumn terms. You can access these publications through the links below:"), 
                              a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England"),
                              br(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms", "Pupil absence in schools in England: autumn and spring terms")),
                              br(),
                              h3("Overview of absence statistics"),
                              p("All maintained schools are required to provide 2 possible sessions per day, morning and afternoon, to all pupils. The length of each session, break and the school day is determined by the school’s governing body. Schools must meet for at least 380 sessions or 190 days during any school year to educate their pupils."),
                              br("If a school is prevented from meeting for 1 or more sessions because of an unavoidable event, it should find a practical way of holding extra sessions. However, if it cannot find a practical way of doing this then it’s not required to make up the lost sessions. Academy and free school funding agreements state that the duration of the school day and sessions are the responsibility of the academy trust."),
                              br("Schools are required to take attendance registers twice a day - once at the start of the first morning session and once during the second afternoon session. In their register, schools are required to record whether pupils are:"),
                              p("• absent"),
                              p("• attending an approved educational activity"),
                              p("• present"),
                              p("• unable to attend due to exceptional circumstances"),
                              br("Where a pupil of compulsory school age is absent, schools have a responsibility to:"),
                              p("• ascertain the reason"),
                              p("• ensure the proper safeguarding action is taken"),
                              p("• indicate in their register whether the absence is authorised by the school or unauthorised"),
                              p("• identify the correct code to use before entering it on to the school’s electronic register, or management information system which is then used to download data to the school census"),
                              br("The parent of every child of compulsory school age is required to ensure their child receives a suitable full-time education for their ability, age, aptitude and any special education needs they may have either by regular attendance at school or otherwise. Failure of a parent to secure regular attendance of their school registered child of compulsory school age can lead to a penalty notice or prosecution. Local authorities (LAs) and schools have legal responsibilities regarding accurate recording of a pupil’s attendance."),
                              br("For further information please see ", 
                                 a(href = "https://www.gov.uk/government/publications/school-attendance", "School attendance: guidance for schools"), ".")
                             ),
                           )
                         )
                       )
                     )
                   )
                 )
               ),
               
               
               
               
               tabPanel(
                 value = "headlines and reasons",
                 "Headlines and reasons",
                 
                 # Define UI for application
                 
                 # Sidebar with la_name choice
                 sidebarLayout(
                   sidebarPanel(
                     width = 2,
                     uiOutput("levels_filtered"),
                     conditionalPanel(condition = "input.geography_choice == 'Regional'|| input.geography_choice == 'Local authority'", 
                                      uiOutput("reg_filtered")
                     ),
                     conditionalPanel(condition = "input.geography_choice == 'Local authority'",
                                      uiOutput("la_filtered")
                     ),
                     selectInput(inputId = "school_choice",
                                 label = "Choose school type:",
                                 choices = attendance_data$school_type[!duplicated(attendance_data$school_type)]
                     )
                   ),
                   
                   # Show lines with most recent absence data
                   mainPanel(
                     width = 10,
                     h2("Absence headlines and reasons"),
                     tabsetPanel(
                       id = "tabs",
                       tabPanel(
                         value = "headlines",
                         title = "Headlines",
                         fluidPage(
                           fluidRow(
                             br(),
                             p(strong(paste0("Most recent absence data"))),
                             textOutput("weekly_absence_rate"),
                             textOutput("weekly_illness_rate"),
                             textOutput("ytd_pa_rate"),
                             column(width = 12, br()),
                             column(
                               6,
                               p(strong("Overall absence rates across the year to date")),
                               plotlyOutput("absence_rates_timeseries_plot")
                             ),
                             column(
                               6,
                               fluidRow(
                                 column(
                                   12,
                                   p(strong("Overall persistent absence rate (at least 10% sessions missed) across the year to date")),
                                   plotlyOutput("pa_timeseries_plot")
                                 )
                               ),
                             )
                           ),
                           p(strong(paste0("Summary of overall, authorised and unathorised absence"))),
                           textOutput("daily_dates"),
                           textOutput("weekly_dates"),
                           br(),
                           p(strong(paste0("Overall absence rate:"))),
                           valueBoxOutput("headline_absence_rate_daily", width = 6),
                           valueBoxOutput("headline_absence_rate_weekly", width = 6),
                           valueBoxOutput("headline_absence_rate_ytd", width = 6)
                         )
                       ),
                       tabPanel(
                         value = "reasons",
                         title = "Reasons",
                         fluidPage(
                           fluidRow(
                             column(
                               6,
                               br(),
                               p(strong("Reasons for absence across the year to date")),
                               plotlyOutput("absence_reasons_timeseries_plot")
                             ),
                             column(
                               6,
                               fluidRow(
                                 column(
                                   12,
                                   br(),
                                   p(strong(paste0("Authorised absence rate:"))),
                                   valueBoxOutput("headline_auth_rate_daily", width = 6),
                                   valueBoxOutput("headline_auth_rate_weekly", width = 6),
                                   valueBoxOutput("headline_auth_rate_ytd", width = 6),
                                   p(strong(paste0("Unauthorised absence rate:"))),
                                   valueBoxOutput("headline_unauth_rate_daily", width = 6),
                                   valueBoxOutput("headline_unauth_rate_weekly", width = 6),
                                   valueBoxOutput("headline_unauth_rate_ytd", width = 6)
                                 )
                               ),
                             ),
                           )
                         )
                       )
                     ),
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
                   tabPanel(
                     "Reasons",
                     tableOutput("notesTableReasons") # made in global.R file
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
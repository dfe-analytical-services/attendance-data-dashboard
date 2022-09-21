# homepage panel

homepage_panel <- function() {
  tabPanel(
    "Homepage",
    fluidPage(
      fluidRow(
        column(
          12,
          h2("Please note, the link to this dashboard should not be shared more widely.", style = "color:red"),
          br(),
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
                p("The dashboard provides breakdowns of attendance and absence headline figures, and reasons for absence at National, Regional and Local Authority geographic levels. Data is available across primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns. Users will need to select a geographic level prior to selecting further options at Region or Local Authority level."),
                p("Please note, attendance and absence rates will not add to 100%. This is due to some pupils participating in approved educational activities which mean they are not in attendance on site or absent. For further information on the measures presented, view the ", actionLink("link_to_technotes", "technical notes"), "."),
                br(),
                tags$div(
                  title = "Headline information on overall and persistent absence",
                  h3(actionLink("link_to_headlines_tab", "Headlines")),
                  p("The headlines tab includes information on attendance, overall absence (including  authorised and unauthorised absence) in the most recent week and across the year to date, depending on dropdown selected."),
                  p("• Charts on this tab display overall, authorised and unauthorised absence rates"),
                  p("• Headline bullets show the overall attendance and absence rates, in addition to illness absence rate"),
                  p("• Persistent absence will be added to the dashboard later in the autumn term"),
                ),
                br(),
                tags$div(
                  title = "Reasons for absence including authorised and unauthorised, in addition to breakdowns of the above",
                  h3(actionLink("link_to_reasons_tab", "Reasons")),
                  p("The reasons tab includes information on authorised and unauthorised absence rates, alongside the individual reasons for absence that comprise authorised and unauthorised absence."),
                  p("• The chart on this tab displays absence rates across the year to date associated with some of the most common absence reasons, including illness, holidays and appointments"),
                  p("• Headline boxes show the authorised and unauthorised absence rates in the most recent full week of data, as well as across the full year to date"),
                  p("• Tables show absence rates associated with each of the individual reasons for absence"),
                ),
                br(),
                tags$div(
                  title = "Breakdown of overall, authorised and unauthorised absence by local authority",
                  h3(actionLink("link_to_la_tab", "Local authority data")),
                  p("The local authority data tab includes information on the overall, authorised and unauthorised absence rates for each local authority in the most recent week.")
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
                  p("The Department for Education is working alongside schools to collect more timely attendance data. This dashboard presents statistics developed using management information from schools who have opted in to the data collection process. This is produced alongside the established", 
                    a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "National Statistics"), "based on the School Census. This dashboard will be updated fortnightly."),
                  br(),
                  h3("Coverage"),
                  h5(textOutput("daily_schools_count")),
                  p("This number is approximately 65% of the number of schools participating in the School Census. As schools opt in to sharing of data, the number of schools reporting may change over time."),
                  p("For further information on the number of schools reporting, view the technical notes on ", actionLink("link_to_coverage", "coverage")),
                  br(),
                  h3("National statistics"),
                  p("This dashboard has been developed as an accompaniment to the termly National statistics releases produced by the department, covering pupil absence. You can access these publications through the links below:"), 
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England"),
                  br(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms", "Pupil absence in schools in England: autumn and spring terms")),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-term", "Pupil absence in schools in England: autumn term"),
                  br(),
                  br(),
                  p("Statistics presented in this dashboard are based on a smaller, non-random sample of schools in comparison to National statistics. Absence statistics are presented at a termly level in current National statistics, while this dashboard enables more timely investigation of data with breakdowns at daily and weekly level. Attendance rates are also presented for the first time on this dashboard."),
                  p("Data is lagged by 2 weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late. As a result, data presented may change between dashboard updates."),
                  br(),
                ),
              )
            )
          )
        )
      )
    )
  )
}


# dashboard panel
dashboard_panel <- function() {
  #fluidPage(
  
  tabPanel(
    value = "dashboard",
    "Dashboard",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    
    
    
    h1("Attendance and absence headlines and reasons"),
    
    
    
    div(
      class = "well",
      style = "min-height: 100%; height: 100%; overflow-y: visible",
      fluidRow(
        column(
          width=3,
          selectInput(inputId = "school_choice",
                      label = "Choose school type:",
                      choices = c("Total", "Primary", "Secondary", "Special"),
                      selected = "Total"
          )),
        column(
          width=3,
          conditionalPanel(condition = "input.dash == 'headlines'|| input.dash == 'reasons'",
                           selectInput(inputId = "geography_choice",
                                       label = "Choose geographic level:",
                                       choices = c("National","Regional","Local authority"),
                                       selected = head(geog_levels,1)))
        ),
        column(
          width=3,
          conditionalPanel(condition = "input.geography_choice == 'Regional' && input.dash != 'la comparisons' || input.geography_choice == 'Local authority' && input.dash != 'la comparisons'", 
                           selectInput(inputId = "region_choice",
                                       label = "Choose region:",
                                       choices = geog_lookup %>% dplyr::filter(geographic_level == 'National') %>% dplyr::select(region_name) %>% unique() %>% as.data.table(),
                                       #selected = head(reg_geog,1)
                           ))
        ),
        column(
          width=3,
          conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.dash != 'la comparisons'", 
                           selectInput(inputId = "la_choice",
                                       label = "Choose local authority:",
                                       choices = geog_lookup %>% dplyr::filter(region_name == "East Midlands") %>% dplyr::select(la_name) %>% unique() %>% as.data.table(),
                                       #selected = la_geog()[2,1]
                           ))
        ),
      ),
      
      conditionalPanel(condition = "input.dash == 'headlines'",
                       fluidRow(
                         column(
                           width=3,
                           conditionalPanel(condition = "input.dash == 'headlines'",
                                            selectInput(inputId = "ts_choice",
                                                        label = "Choose time period:",
                                                        choices = c("Most recent week", "Year to date")
                                            ))
                         ),
                         column(
                           width=3,
                           conditionalPanel(condition = "input.dash == 'headlines'",
                                            p(strong("Download underlying data")),
                                            downloadButton("downloadData", label = "Download data", style = "width:100%;white-space:normal;")
                           )),
                         column(
                           width=3,
                           conditionalPanel(condition = "input.dash == 'headlines'",
                                            p(strong("For more tables and metadata")),
                                            actionButton(inputId='ees', label="Visit Explore Education Statistics", 
                                                         icon = icon("th"), 
                                                         onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools', '_blank')",
                                                         style = "width:100%;white-space:normal;")
                           )),
                       )
      ),
      
      conditionalPanel(condition = "input.dash != 'headlines'",
                       fluidRow(
                         column(
                           width=3,
                           conditionalPanel(condition = "input.dash != 'headlines'",
                                            p(strong("Download underlying data")),
                                            downloadButton("downloadData2", label = "Download data", style = "width:100%;white-space:normal;")
                           )),
                         column(
                           width=3,
                           conditionalPanel(condition = "input.dash != 'headlines'",
                                            p(strong("For more tables and metadata")),
                                            actionButton(inputId='ees', label="Visit Explore Education Statistics", 
                                                         icon = icon("th"), 
                                                         onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england', '_blank')",
                                                         style = "width:100%;white-space:normal;")
                           )),
                       )
      ),
      
    ),
    tabsetPanel(
      id = 'dash',
      tabPanel(
        value = "headlines",
        title = "Headlines",
        fluidPage(
          fluidRow(
            br(),
            conditionalPanel(condition = "input.geography_choice == 'National'", 
                             h4(textOutput("headline_bullet_title_nat"
                             ))),
            conditionalPanel(condition = "input.geography_choice == 'Regional'", 
                             h4(textOutput("headline_bullet_title_reg"
                             ))),
            conditionalPanel(condition = "input.geography_choice == 'Local authority'", 
                             h4(textOutput("headline_bullet_title_la"
                             ))),
            conditionalPanel(condition = "input.ts_choice == 'Most recent week'",
                             textOutput("school_count_proportion_weekly"),
                             textOutput("weekly_dates")
            ),
            conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                             textOutput("school_count_proportion_ytd")),
            br(),
            conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                             p(strong(paste0("Attendance and absence across year to date")))),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_attendance_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_attendance_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_attendance_rate_la"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_absence_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_absence_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_absence_rate_la"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_illness_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_illness_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Year to date'", 
                             textOutput("ytd_illness_rate_la"
                             )),
            conditionalPanel(condition = "input.ts_choice == 'Most recent week'",
                             p(strong(paste0("Attendance and absence in the most recent week")))),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_attendance_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_attendance_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_attendance_rate_la"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_absence_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_absence_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_absence_rate_la"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_illness_rate_nat"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_illness_rate_reg"
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Most recent week'", 
                             textOutput("weekly_illness_rate_la"
                             )),
            br(),
            h5(textOutput("headline_ts_chart_title")),
            conditionalPanel(condition = "input.ts_choice == 'Year to date'", 
                             plotlyOutput("absence_rates_timeseries_plot")),
            conditionalPanel(condition = "input.ts_choice == 'Most recent week'", 
                             plotlyOutput("absence_rates_daily_plot")),
            
          ),
        )
      ),
      tabPanel(
        value = "reasons",
        title = "Reasons",
        fluidRow(
          br(),
          br(),
          conditionalPanel(condition = "input.geography_choice == 'National'", 
                           h4(textOutput("reasons_chart_title_nat"
                           ))),
          conditionalPanel(condition = "input.geography_choice == 'Regional'", 
                           h4(textOutput("reasons_chart_title_reg"
                           ))),
          conditionalPanel(condition = "input.geography_choice == 'Local authority'", 
                           h4(textOutput("reasons_chart_title_la"
                           )))
        ),
        fluidRow(
          column(6,
                 br(),
                 plotlyOutput("absence_reasons_timeseries_plot")),
          column(6,
                 fluidRow(
                   
                   br(),
                   p(strong(paste0("Authorised absence rate:"))),
                   shinydashboard::valueBoxOutput("headline_auth_rate_weekly", width = 6),
                   shinydashboard::valueBoxOutput("headline_auth_rate_ytd", width = 6)
                 ),
                 fluidRow(
                   br(),
                   p(strong(paste0("Unauthorised absence rate:"))),
                   shinydashboard::valueBoxOutput("headline_unauth_rate_weekly", width = 6),
                   shinydashboard::valueBoxOutput("headline_unauth_rate_ytd", width = 6)
                 )
          )
        ),
        br(),
        p(strong("Reasons for absence in the most recent week")),
        p("Authorised absence"),
        DTOutput("absence_auth_reasons_table"),
        br(),
        br(),
        p("Unauthorised absence"),
        DTOutput("absence_unauth_reasons_table")
      ),
      tabPanel(
        value = "la comparisons",
        title = "Local Authority Data",
        fluidPage(
          fluidRow(
            br(),
            h4(textOutput("map_title")),
            fluidRow(
              column(
                3,
                br(),
                p(strong("Select absence rate of interest from drop down menu to view on the map:")),
              ),
              column(
                3,
                selectInput(inputId = "measure_choice",
                            label = "Choose measure of interest:",
                            choices = c("Overall", "Authorised","Unauthorised")
                )
              ),
            ),
            leafletOutput("rates_map"),
            br(),
            h4(textOutput("la_comparison_title")),
            DTOutput("absence_reasons_la_table")
          )
        )
      )
    )
    # add box to show user input
  )
}

# technical notes panel
notes_panel <- function(){
  tabPanel(
    value = "technical notes",
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
    br("Use this dashboard to view pupil attendance by school type and geographical breakdown in England"),
    br("Data is lagged by 2 weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late."),
    br(),
    tabBox(
      title = "",
      id = "tabs_tech_notes", width = "12",
      tabPanel(
        "Absence statistics overview",
        h4("Absence statistics overview"),
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
           a(href = "https://www.gov.uk/government/publications/school-attendance", "School attendance: guidance for schools"), "."),
      ),
      tabPanel(
        "Coverage",
        column(
          6,
          br(),
          h4("Coverage and number of schools reporting"),
          p("Statistics presented in this dashboard are based on a smaller, non-random sample of schools in comparison to National statistics."),
          p("As schools opt in to sharing of data, the number of schools reporting may change over time."),
        ),
        column(
          6,
          fluidRow(
            column(
              12,
              plotlyOutput("response_rates"),
            ),
          ),
        ),
      ),
      tabPanel(
        "Headlines and local authority data",
        h4("Indicators for attendance and absence headlines and local authority data"),
        tableOutput("notesTableHeadlines") # made in global.R file
      ), # end of tabPanel
      tabPanel(
        "Reasons",
        h4("Indicators for attendance and absence reasons"),
        tableOutput("notesTableReasons") # made in global.R file
      ), # end of tabPanel
    ) # end of tabBox
  )
}

# accessibility panel
accessibility_panel <- function(){
  tabPanel(
    "Accessibility",
    h2("Accessibility statement"),
    br("This accessibility statement applies to the Pupil attendance in schools in England data dashboard.
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
      a(href = "mailto:schools.statistics@education.gov.uk", "schools.statistics@education.gov.uk")
    )
  )
}

# support panel
support_panel <- function(){
  tabPanel(
    "Support and feedback",
    support_links() # defined in R/supporting_links.R
  )
}
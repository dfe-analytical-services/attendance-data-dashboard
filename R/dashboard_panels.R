# homepage panel

homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      fluidRow(
        column(
          12,
          h1("Pupil attendance and absence in schools in England"), 
          textOutput("headline_update_date"),
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
                p("The dashboard provides data on attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns. Users will need to select a geographic level prior to selecting further options at Region or Local Authority level."),
                br(),
                tags$div(
                  title = "Headline information on overall and persistent absence",
                  h3(actionLink("link_to_headlines_tab", "Headlines")),
                  p("The headlines tab includes information on attendance, overall absence (including  authorised and unauthorised absence) in the most recent week and across the year to date, depending on dropdown selected."),
                  p("• Charts on this tab display overall, authorised and unauthorised absence rates"),
                  p("• Headline bullets show the overall attendance and absence rates, in addition to illness absence rate"),
                  p("• This tab now includes data relating to persistent absence (pupils missing 10% or more sessions). To view these, select “year to date” in the drop-down menu. Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. Underlying data relating to the Autumn term and year to date is available at the link below:"),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
                  br(),
                  br(),
                  p("No figures for the day of teacher strikes have been provided in the dashboard and underlying data. Further information on attendance during these days available at the link below:"),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
                ),
                br(),
                tags$div(
                  title = "Reasons for absence including authorised and unauthorised, in addition to breakdowns of the above",
                  h3(actionLink("link_to_reasons_tab", "Reasons")),
                  p("The reasons tab includes information on authorised and unauthorised absence rates, alongside the individual reasons for absence."),
                  p("• The chart on this tab displays absence rates across the year to date associated with some of the most common absence reasons, including illness, holidays and medical appointments"),
                  p("• Headline boxes show the latest authorised and unauthorised absence rates"),
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
                  p("These figures are derived from regular data automatically submitted to the Department for Education (DfE) by participating schools. These are intended to continue the series that was previously sourced from the daily Educational Settings Survey (EdSet). Due to the timeliness of the data and that they are based on a subset of schools, the figures are estimates that we expect to change as registers are adjusted. They should be viewed as an early indicator for the more detailed but less regular", 
                    a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "National Statistics"), "on pupil absence (which will include school level breakdowns)."),
                  br(),
                  h3("Coverage"),
                  h4(textOutput("daily_schools_count")),
                  p("This number is approximately 78% of the number of schools participating in the School Census. As schools opt in to sharing of data, the number of schools reporting may change over time."),
                  p("Absence rates are provided broken down by state-funded primary, secondary and special schools. At national and regional level, absence figures are also provided across all schools. In recognition that response rates are not equal across school types and, therefore, not representative of the total school population, the total absence figure for all schools has been weighted based on the Spring 2022 school census. Weighted total figures are not included at local authority level due to the low number of schools involved."),
                  br(),
                  h3("National statistics"),
                  p("This dashboard has been developed as an accompaniment to DFE's termly National statistics on pupil absence. You can access these publications through the links below:"), 
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "Pupil absence in schools in England"),
                  br(a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms", "Pupil absence in schools in England: autumn and spring terms")),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-term", "Pupil absence in schools in England: autumn term"),
                  br(),
                  br(),
                  p("From the 2022/23 academic year onwards, all data relating to pupil attendance will be published at the link below:"),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
                  br(),
                  br(),
                  p("Statistics presented in this dashboard are based on a smaller, non-random sample of schools in comparison to National statistics. Absence statistics are available on a termly basis in the National statistics, while this dashboard enables more timely daily and weekly data. They should, therefore, be viewed as an early indicator for the more detailed but less regular National Statistics (which will include school level breakdowns)."),
                  p("Data is lagged by 2 weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late. As a result, data presented may change between dashboard updates."),
                  textOutput("homepage_update_dates"),
                  p("Data for earlier weeks has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level estimates covering the week commencing 5 September is available at the link below: "),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
                  br(),
                  br(),
                  h4("If you are a school that has not yet signed up to share your data, please visit ", 
                     a(href = "https://www.gov.uk/guidance/share-your-daily-school-attendance-data", "Share your daily school attendance data"), "for more information. This will also give you, your local authority and your multi-academy trust (if applicable)",
                     a(href = "https://esfahelp.education.gov.uk/hc/en-gb/articles/6176380401810-School-Daily-Attendance-Trial?utm_source=5%20September%202022%20C19&utm_medium=Daily%20Email%20C19&utm_campaign=DfE%20C19", "access to daily attendance reports"), "to help identify pupils needing attendance support earlier."),
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
    
    gov_main_layout(
      
      h1("Attendance and absence headlines and reasons"),
      
      
      
      div(
        class = "well",
        style = "min-height: 100%; height: 100%; overflow-y: visible",
        fluidRow(
          column(
            width=6,
            fluidRow(
              column(
                width=6,
                selectInput(inputId = "school_choice",
                            label = "Choose school type:",
                            choices = school_type_lookup %>% dplyr::filter(geographic_level == "National") %>% dplyr::select(school_type) %>% unique() %>% as.data.table(),
                            selected = "Primary"
                )
              ),
              column(width=6,
                     conditionalPanel(condition = "input.dash == 'headlines'|| input.dash == 'reasons'",
                                      selectInput(inputId = "ts_choice",
                                                  label = "Choose time period:",
                                                  choices = c("Most recent week", "Year to date")
                                      )
                     )
              )
            ),
            fluidRow(
              column(
                width=6,
                p(strong("Download underlying data")),
                downloadButton("downloadData2", label = "Download data", style = "width:100%;white-space:normal;")
              ),
              column(
                width=6,
                p(strong("For more tables and metadata")),
                actionButton(inputId='ees', 
                             label="Visit Explore Education Statistics", 
                             icon = icon("th"), 
                             onclick ="window.open('https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools', '_blank')",
                             style = "width:100%;white-space:normal;")
                
              )
            )
          ),
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
                                         choices = regions,
                                         selected = regions[1]
                             )),
            conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.dash != 'la comparisons'", 
                             selectInput(inputId = "la_choice",
                                         label = "Choose local authority:",
                                         choices = las,
                                         selected = las[1]
                             ))
          )
        )
        
        
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
                               textOutput("update_dates"),
                               p("Data for earlier weeks has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level data covering the week commencing 5th September is available on", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Explore Education Statistics"), ". Data at National and Regional level for the week commencing 19th December has not been included as very few Local Authorities have schools open during this week. Where Local Authorities were open during the week commending 19th December, this data has been shown at Local Authority level."),
                               p(strong(paste0("No figures for the day of teacher strikes have been provided in the dashboard and underlying data. Further information on attendance during these days available at the link below:"))),
                               a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
              ),
              conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                               textOutput("school_count_proportion_weekly2"),
                               textOutput("update_dates2"),
                               p("Data for earlier weeks has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level data covering the week commencing 5th September is available on", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Explore Education Statistics"), ". Data at National and Regional level for the week commencing 19th December has not been included as very few Local Authorities have schools open during this week. Where Local Authorities were open during the week commending 19th December, this data has been shown at Local Authority level."),
                               p(strong(paste0("No figures for the day of teacher strikes have been provided in the dashboard and underlying data. Further information on attendance during these days available at the link below:"))),
                               a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
              ),
              
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
              conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                               br(),
                               p(strong(paste0("Persistent absence across year to date"))),
                               p("A pupil enrolment is identified as persistently absent if they have missed 10% or more of their possible sessions in the year to date.")),
              conditionalPanel(condition = "input.geography_choice == 'National' && input.ts_choice == 'Year to date'", 
                               textOutput("ytd_pa_rate_nat"
                               )),
              conditionalPanel(condition = "input.geography_choice == 'Regional' && input.ts_choice == 'Year to date'", 
                               textOutput("ytd_pa_rate_reg"
                               )),
              conditionalPanel(condition = "input.geography_choice == 'Local authority' && input.ts_choice == 'Year to date'", 
                               textOutput("ytd_pa_rate_la"
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
              conditionalPanel(condition = "input.ts_choice == 'Most recent week'",
                               br(),
                               p(strong(paste0("To view persistent absence figures, select “year to date” in the drop-down menu. Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. Underlying data relating to the Autumn term and year to date is available at the link below:"))),
                               a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-term", "Pupil absence in schools in England: autumn term")),
              
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
            conditionalPanel(condition = "input.ts_choice == 'Most recent week'",
                             column(9,
                                    br(),
                                    plotlyOutput("absence_reasons_daily_plot")),
                             column(3,
                                    fluidRow(
                                      
                                      br(),
                                      p(strong(paste0("Authorised absence rate:"))),
                                      shinydashboard::valueBoxOutput("headline_auth_rate_weekly", width = 12)
                                    ),
                                    fluidRow(
                                      br(),
                                      p(strong(paste0("Unauthorised absence rate:"))),
                                      shinydashboard::valueBoxOutput("headline_unauth_rate_weekly", width = 12)
                                    )
                             )
            ),
            conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                             column(9,
                                    br(),
                                    plotlyOutput("absence_reasons_timeseries_plot")),
                             column(3,
                                    fluidRow(
                                      
                                      br(),
                                      p(strong(paste0("Authorised absence rate:"))),
                                      shinydashboard::valueBoxOutput("headline_auth_rate_ytd", width = 12)
                                    ),
                                    fluidRow(
                                      br(),
                                      p(strong(paste0("Unauthorised absence rate:"))),
                                      shinydashboard::valueBoxOutput("headline_unauth_rate_ytd", width = 12)
                                    )
                             )
            )
          ),
          br(),
          conditionalPanel(condition = "input.ts_choice == 'Most recent week'",
                           p(strong("Reasons for absence in the most recent week")),
                           p("Authorised absence"),
                           DTOutput("absence_auth_reasons_table"),
                           br(),
                           br(),
                           p("Unauthorised absence"),
                           DTOutput("absence_unauth_reasons_table")),
          conditionalPanel(condition = "input.ts_choice == 'Year to date'",
                           p(strong("Reasons for absence in the year to date")),
                           p("Authorised absence"),
                           DTOutput("absence_auth_reasons_table_ytd"),
                           br(),
                           br(),
                           p("Unauthorised absence"),
                           DTOutput("absence_unauth_reasons_table_ytd")),
        ),
        tabPanel(
          value = "la comparisons",
          title = "Local Authority Data",
          fluidPage(
            fluidRow(
              br(),
              h4(textOutput("map_title")),
              textOutput("la_clarity_dates"),
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
    gov_main_layout(
      h2("Technical notes"),
      br("The dashboard provides data on attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns."),
      br(),
      p("Users should be aware"),
      p("• Estimates for non-response - In recognition that response rates are not equal across school types and, therefore, not representative of the total school population, the total rates for all schools has been weighted based on the Spring 2022 school census. Weighted total figures are not included at local authority level due to the low number of schools involved."),
      p("• Reporting lag - Schools update their registers continually and attendance codes change, resulting in absence rates for a particular day to decrease over time. Analysis of data from the Summer 2022 term suggests that this could be a decrease in the absence rate of around 1 percentage point before settling down. Historical figures will be recalculated in each publication."),
      p("• Data prior to 12 September 2022 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level estimates covering the week commencing 5 September is available at the link below: "),
      a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
      br(),
      br(),
      p("Full information on methodologies and further technical notes are available through", a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-attendance-in-schools", "Explore Education Statistics")),
      br()
    )
  )
}

# accessibility panel
accessibility_panel <- function(){
  tabPanel(
    "Accessibility",
    gov_main_layout(
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
             to review updates to technology available to us to keep improving accessibility for all of our users."),
      h3("Feedback"),
      br(
        "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
        a(href = "mailto:schools.statistics@education.gov.uk", "schools.statistics@education.gov.uk")
      )
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
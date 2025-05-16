# homepage panel

homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
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
                p(
                  "The dashboard provides data on attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns. Users will need to select a geographic level prior to selecting further options at Region or Local Authority level."
                ),
                p(
                  "You can navigate directly to tabs of the dashboard using the 'Headlines', 'Reasons' and 'Local authority data' links below."
                ),
                br(),
                tags$div(
                  title = "Headline information on overall and persistent absence",
                  h3(actionLink("link_to_headlines_tab", "Headlines")),
                  p(
                    "The headlines tab includes information on attendance, overall absence (including  authorised and unauthorised absence) in the latest week and across the year to date, depending on the dropdown selected."
                  ),
                  p(
                    "- Charts on this tab display overall, authorised and unauthorised absence rates."
                  ),
                  p(
                    "- Headline bullets show the overall attendance and absence rates, in addition to illness absence rate."
                  ),
                  p(
                    "- This tab includes data relating to persistent absence (pupils missing 10% or more sessions). To view these, select “year to date” in the drop-down menu. Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. Underlying data relating to the Autumn term and year to date is available at the link below:"
                  ),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools",
                    "Pupil attendance in schools"
                  ),
                  br(),
                  br(),
                  # p("No figures for the day of teacher strikes have been provided in the dashboard and underlying data. Further information on attendance during these days available at the link below:"),
                  # a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
                ),
                br(),
                tags$div(
                  title = "Reasons for absence including authorised and unauthorised, in addition to breakdowns of the above",
                  h3(actionLink("link_to_reasons_tab", "Reasons")),
                  p(
                    "The reasons tab includes information on authorised and unauthorised absence rates, alongside the individual reasons for absence, in the latest week and across the year to date, depending on the dropdown selected."
                  ),
                  p(
                    "- The chart on this tab displays absence rates associated with some of the most common absence reasons, including illness, holidays and medical appointments."
                  ),
                  p(
                    "- Headline boxes show the latest authorised and unauthorised absence rates."
                  ),
                  p(
                    "- Tables show absence rates associated with each of the individual reasons for absence."
                  ),
                ),
                br(),
                tags$div(
                  title = "Breakdown of overall, authorised and unauthorised absence by local authority",
                  h3(actionLink("link_to_la_tab", "Local authority data")),
                  p(
                    "The local authority data tab includes information on the overall, authorised and unauthorised absence rates for each local authority in the latest week."
                  )
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
                  p(
                    "These figures are derived from regular data automatically submitted to the Department for Education (DfE) by participating schools. Due to the timeliness of the data and that they are based on a subset of schools, the figures are estimates that we expect to change as registers are adjusted. They should be viewed as an early indicator for the more detailed but less regular",
                    a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england",
                      "National Statistics"
                    ),
                    "on pupil absence (which will include school level breakdowns)."
                  ),
                  br(),
                  h3("Coverage"),
                  p(textOutput("daily_schools_count")),
                  p(textOutput("school_count_proportion_homepage")),
                  p(
                    "Absence rates are provided broken down by state-funded primary, secondary and special schools. At national and regional level, absence figures are also provided across all schools. In recognition that response rates are not equal across school types and, therefore, not representative of the total school population, the total absence figure for all schools has been weighted based on the Spring 2024 school census. Weighted total figures are not included at local authority level due to the low number of schools involved."
                  ),
                  br(),
                  h3("National statistics"),
                  p(
                    "Data relating to pupil attendance, including pupil characteristics, is published at the link below:"
                  ),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools",
                    "Pupil attendance in schools - 2024/25 academic year"
                  ),
                  br(),
                  br(),
                  p(
                    "For 2023/24 full academic year and termly pupil attendance data, including by characteristics, please see the historical publication at the link below:"
                  ),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-29",
                    "Pupil attendance in schools - 2023/24 academic year"
                  ),
                  br(),
                  br(),
                  p(
                    "This dashboard has been developed as an accompaniment to DFE's termly National statistics on pupil absence. You can access this publication through the link below:"
                  ),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england",
                    "Pupil absence in schools in England"
                  ),
                  br(),
                  br(),
                  p(
                    "Statistics presented in this dashboard are based on a smaller, non-random sample of schools in comparison to National statistics. Absence statistics are available on a termly basis in the National statistics, while this dashboard enables more timely daily and weekly data. They should, therefore, be viewed as an early indicator for the more detailed but less regular National Statistics (which will include school level breakdowns)."
                  ),
                  p(
                    "Data is lagged by 2 weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late. As a result, data presented may change between dashboard updates."
                  ),
                  textOutput("homepage_update_dates"),
                  br(),
                  p(
                    "Data prior to 09 September 2024 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level estimates covering the week commencing 02 September 2024 is available in the underlying data of the publication linked below: "
                  ),
                  a(
                    href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-37",
                    "Pupil attendance in schools - First publication of 2024/25 academic year"
                  ),
                  br(),
                  br(),
                  p(
                    strong(
                      "From the start of the 2024/25 academic year, it became mandatory for schools to ",
                      a(
                        href = "https://www.gov.uk/guidance/share-your-daily-school-attendance-data",
                        "share attendance data"
                      ),
                      " with the DfE. If you are a school that is not already sharing your daily attendance data, you need to approve this in your Wonde portal. This will also give you, your local authority and your multi-academy trust (if applicable)",
                      a(
                        href = "https://www.gov.uk/guidance/access-your-school-attendance-data",
                        "access to daily attendance reports"
                      ),
                      "to help identify pupils needing attendance support earlier."
                    )
                  ),
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
  # fluidPage(

  tabPanel(
    value = "dashboard",
    "Dashboard",

    # Define UI for application that draws a histogram

    # Sidebar with a slider input for number of bins

    gov_main_layout(
      gov_row(
        h1("Attendance and absence headlines and reasons"),
        expandable(
          inputId = "details",
          label = textOutput("dropdown_label"),
          contents = div(
            id = "div_a",
            # class = "well",
            # style = "min-height: 100%; height: 100%; overflow-y: visible",
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = "school_choice",
                  label = "Choose school type:",
                  choices = school_type_lookup %>%
                    dplyr::filter(geographic_level == "National") %>%
                    dplyr::select(school_type) %>%
                    unique() %>%
                    as.data.table(),
                  selected = "Primary",
                  selectize = TRUE,
                  width = "100%"
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  condition = "input.dash == 'headlines'|| input.dash == 'reasons'",
                  selectInput(
                    inputId = "ts_choice",
                    label = "Choose time period:",
                    choices = c(
                      most_recent_week_dates = "latestweeks",
                      ytd_dates = "yeartodate"
                    ),
                    selectize = TRUE,
                    width = "100%"
                  )
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  condition = "input.dash == 'headlines'|| input.dash == 'reasons'",
                  selectInput(
                    inputId = "geography_choice",
                    label = "Choose geographic level:",
                    choices = c("National", "Regional", "Local authority"),
                    selected = head(geog_levels, 1),
                    selectize = TRUE,
                    width = "100%"
                  )
                )
              ),
              column(
                width = 3,
                conditionalPanel(
                  condition = "input.geography_choice == 'Regional' && input.dash != 'la comparisons'",
                  # conditionalPanel(condition = "input.geography_choice == 'Regional' && input.dash != 'la comparisons' || input.geography_choice == 'Local authority' && input.dash != 'la comparisons'",
                  selectInput(
                    inputId = "region_choice",
                    label = "Choose region:",
                    choices = regions,
                    selected = regions[1],
                    selectize = TRUE,
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "input.geography_choice == 'Local authority' && input.dash != 'la comparisons'",
                  selectInput(
                    inputId = "la_choice",
                    label = "Choose local authority:",
                    choices = la_list,
                    selected = las[1],
                    selectize = TRUE,
                    width = "100%"
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                p(strong("Download underlying data")),
                downloadButton(
                  class = "btn",
                  "downloadData2",
                  label = "Download data",
                  style = "width:100%;white-space:normal;"
                )
              ),
              column(
                width = 3,
                p(strong("For more tables and metadata")),
                actionButton(
                  class = "btn",
                  inputId = "ees",
                  label = "Visit Explore Education Statistics",
                  icon = icon("th"),
                  onclick = "window.open('https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools', '_blank')",
                  style = "width:100%;white-space:normal;"
                )
              )
            )
          )
        ),
        tabsetPanel(
          id = "dash",
          tabPanel(
            value = "headlines",
            title = "Headlines",
            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  conditionalPanel(
                    condition = "input.geography_choice == 'National'",
                    h4(textOutput("headline_bullet_title_nat"))
                  ),
                  conditionalPanel(
                    condition = "input.geography_choice == 'Regional'",
                    h4(textOutput("headline_bullet_title_reg"))
                  ),
                  conditionalPanel(
                    condition = "input.geography_choice == 'Local authority'",
                    h4(textOutput("headline_bullet_title_la"))
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    textOutput("school_count_proportion_weekly"),
                    textOutput("update_dates"),
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    textOutput("school_count_proportion_weekly2"),
                    textOutput("update_dates2"),
                  ),
                  br(),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    p(strong(paste0(
                      "Attendance and absence across year to date"
                    ))),
                    p(
                      "Attendance and absence rates presented here are calculated across all sessions in the year to date."
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'National'"
                    ),
                    textOutput("ytd_attendance_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("ytd_attendance_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("ytd_attendance_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'National'"
                    ),
                    textOutput("ytd_absence_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("ytd_absence_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("ytd_absence_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'National'"
                    ),
                    textOutput("ytd_illness_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("ytd_illness_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("ytd_illness_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    br(),
                    p(strong(paste0("Persistent absence across year to date"))),
                    p(
                      "A pupil enrolment is identified as persistently absent if they have missed 10% or more of their possible sessions in the year to date."
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'National'"
                    ),
                    textOutput("ytd_pa_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("ytd_pa_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'yeartodate' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("ytd_pa_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    p(strong(paste0(
                      "Attendance and absence in the latest week"
                    ))),
                    p(
                      "Attendance and absence rates presented here are calculated across all sessions in the latest week."
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'National'"
                    ),
                    textOutput("weekly_attendance_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("weekly_attendance_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("weekly_attendance_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'National'"
                    ),
                    textOutput("weekly_absence_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("weekly_absence_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("weekly_absence_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'National'"
                    ),
                    textOutput("weekly_illness_rate_nat")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Regional'"
                    ),
                    textOutput("weekly_illness_rate_reg")
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input.ts_choice == 'latestweeks' && input.geography_choice == 'Local authority'"
                    ),
                    textOutput("weekly_illness_rate_la")
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    br(),
                    p(strong(paste0(
                      "To view persistent absence figures, select “year to date” in the drop-down menu. Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. Underlying data relating to the Summer, Spring and Autumn terms and year to date is available at the link below:"
                    ))),
                    a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools",
                      "Pupil attendance in schools"
                    )
                  ),
                  br(),
                  h5(textOutput("headline_ts_chart_title")),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    p(
                      "Absence rates presented here are calculated on a weekly basis. Each point on the chart shows an absence rate calculated across all sessions in the given week."
                    ),
                    plotlyOutput("absence_rates_timeseries_plot")
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    p(
                      "Absence rates presented here are calculated on a daily basis. Each point on the chart shows an absence rate calculated across all sessions in the given day."
                    ),
                    plotlyOutput("absence_rates_daily_plot")
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "reasons",
            title = "Reasons",
            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  conditionalPanel(
                    condition = "input.geography_choice == 'National'",
                    h4(textOutput("reasons_chart_title_nat"))
                  ),
                  conditionalPanel(
                    condition = "input.geography_choice == 'Regional'",
                    h4(textOutput("reasons_chart_title_reg"))
                  ),
                  conditionalPanel(
                    condition = "input.geography_choice == 'Local authority'",
                    h4(textOutput("reasons_chart_title_la"))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    p(
                      "Absence rates presented on the chart below are calculated on a daily basis. Each point on the chart shows an absence rate calculated across all sessions in the given day."
                    ),
                    p(
                      "Absence rates presented in the blue boxes and tables below are calculated across all sessions in the latest week."
                    ),
                    column(
                      9,
                      br(),
                      plotlyOutput("absence_reasons_daily_plot")
                    ),
                    column(
                      3,
                      fluidRow(
                        br(),
                        p(strong(paste0("Authorised absence rate:"))),
                        shinydashboard::valueBoxOutput(
                          "headline_auth_rate_weekly",
                          width = 12
                        )
                      ),
                      fluidRow(
                        br(),
                        p(strong(paste0("Unauthorised absence rate:"))),
                        shinydashboard::valueBoxOutput(
                          "headline_unauth_rate_weekly",
                          width = 12
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    p(
                      "Absence rates presented on the chart below are calculated on a weekly basis. Each point on the chart shows an absence rate calculated across all sessions in the given week."
                    ),
                    p(
                      "Absence rates presented in the blue boxes and tables below are calculated across all sessions in the year to date."
                    ),
                    column(
                      9,
                      br(),
                      plotlyOutput("absence_reasons_timeseries_plot")
                    ),
                    column(
                      3,
                      fluidRow(
                        br(),
                        p(strong(paste0("Authorised absence rate:"))),
                        shinydashboard::valueBoxOutput(
                          "headline_auth_rate_ytd",
                          width = 12
                        )
                      ),
                      fluidRow(
                        br(),
                        p(strong(paste0("Unauthorised absence rate:"))),
                        shinydashboard::valueBoxOutput(
                          "headline_unauth_rate_ytd",
                          width = 12
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    column(
                      12,
                      fluidRow(
                        p(strong("Reasons for absence in the latest week")),
                        p("Authorised absence"),
                        DTOutput("absence_auth_reasons_table"),
                        br(),
                        br(),
                        p("Unauthorised absence"),
                        DTOutput("absence_unauth_reasons_table")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    column(
                      12,
                      fluidRow(
                        p(strong("Reasons for absence in the year to date")),
                        p("Authorised absence"),
                        DTOutput("absence_auth_reasons_table_ytd"),
                        br(),
                        br(),
                        p("Unauthorised absence"),
                        DTOutput("absence_unauth_reasons_table_ytd")
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            value = "la comparisons",
            title = "Local Authority Data",
            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  h4(textOutput("map_title")),
                  textOutput("la_clarity_dates"),
                  fluidRow(
                    column(
                      3,
                      br(),
                      p(strong(
                        "Select absence rate of interest from drop down menu to view on the map:"
                      )),
                    ),
                    column(
                      3,
                      selectInput(
                        inputId = "measure_choice",
                        label = "Choose measure of interest:",
                        choices = c("Overall", "Authorised", "Unauthorised")
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
        )
        # add box to show user input
      )
    )
  )
}

# technical notes panel
notes_panel <- function() {
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
      gov_row(
        column(
          width = 12,
          h2("Technical notes"),
          br(
            "The dashboard provides data on attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns."
          ),
          br(),
          p("Users should be aware"),
          p(
            "- Estimates for non-response - In recognition that response rates are not equal across school types and, therefore, not representative of the total school population, the total rates for all schools has been weighted based on the Spring 2024 school census. Weighted total figures are not included at local authority level due to the low number of schools involved."
          ),
          p(
            "- Reporting lag - Schools update their registers continually and attendance codes change, resulting in absence rates for a particular day to decrease over time. Analysis of data from the Summer 2022 term suggests that this could be a decrease in the absence rate of around 1 percentage point before settling down. Historical figures will be recalculated in each publication."
          ),
          # p("- Data prior to 12 September 2022 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level data covering the week commencing 5th September is available on", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Explore Education Statistics"), ". Data at National and Regional level for the week commencing 19th December has not been included as very few Local Authorities have schools open during this week. Where Local Authorities were open during the week commending 19th December, this data has been shown at Local Authority level."),
          p(
            "- Data prior to 09 September 2024 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level data covering the week commencing 02 September 2024 is available in the underlying data of ",
            a(
              href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-37",
              "this publication",
              .noWS = c("after")
            ),
            "."
          ),
          # p("- No figures for the day of national teacher strikes have been provided in the dashboard and underlying data. No figures for the day of regional teacher strikes have been provided in the dashboard and underlying data for regions affected or at a national level, however figures are still available for regions not expected to be affected. Further information on attendance during these days is available at the link below:"),
          # a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools"),
          br(),
          br(),
          p(
            "Full information on methodologies and further technical notes are available through",
            a(
              href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-attendance-in-schools",
              "Explore Education Statistics",
              .noWS = c("after")
            ),
            "."
          ),
          p(strong("Definitions for common attendance terms")),
          DTOutput("notesTableReasons"),
        )
      )
    )
  )
}

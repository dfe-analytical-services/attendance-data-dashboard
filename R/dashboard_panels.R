# homepage panel

homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Pupil attendance and absence in schools in England"),
          textOutput("source_version_release")
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
                p("You can navigate directly to tabs of the dashboard using the 'Headlines', 'Reasons' and 'Local authority data' links below."),
                tags$div(
                  title = "Headline information on overall and persistent absence",
                  h3(actionLink("link_to_headlines_tab", "Headlines")),
                  p("The headlines tab includes information on attendance, overall absence (including  authorised and unauthorised absence) in the latest week and across the year to date, depending on the dropdown selected."),
                  tags$ul(
                    tags$li("Charts on this tab display overall, authorised and unauthorised absence rates."),
                    tags$li("Headline bullets show the overall attendance and absence rates, in addition to illness absence rate."),
                    tags$li(
                      "This tab includes data relating to persistent absence (pupils missing 10% or more sessions). ",
                      "To view these, select \"year to date\" in the drop-down menu. ",
                      "Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. ",
                      "Underlying data relating to the Autumn term and year to date is available in our Explore educations statistics publication: ",
                      dfeshiny::external_link("https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools")
                    )
                  ),
                ),
                tags$div(
                  title = "Reasons for absence including authorised and unauthorised, in addition to breakdowns of the above",
                  h3(actionLink("link_to_reasons_tab", "Reasons")),
                  p("The reasons tab includes information on authorised and unauthorised absence rates, alongside the individual reasons for absence, in the latest week and across the year to date, depending on the dropdown selected."),
                  tags$ul(
                    tags$li("The chart on this tab displays absence rates associated with some of the most common absence reasons, including illness, holidays and medical appointments."),
                    tags$li("Headline boxes show the latest authorised and unauthorised absence rates."),
                    tags$li("Tables show absence rates associated with each of the individual reasons for absence.")
                  ),
                ),
                tags$div(
                  title = "Breakdown of overall, authorised and unauthorised absence by local authority",
                  h3(actionLink("link_to_la_tab", "Local authority data")),
                  p("The local authority data tab includes information on the overall, authorised and unauthorised absence rates for each local authority in the latest week.")
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
                    "These figures are derived from regular data automatically submitted to the Department for Education (DfE) by participating schools.",
                    "Due to the timeliness of the data and that they are based on a subset of schools, the figures are estimates that we expect to change as registers are adjusted.",
                    "They should be viewed as an early indicator for the more detailed but less regular ",
                    external_link("https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "National Statistics"),
                    "on pupil absence (which will include school level breakdowns)."
                  ),
                  br(),
                  h3("Coverage"),
                  p(textOutput("daily_schools_count")),
                  p(textOutput("school_count_proportion_homepage")),
                  p(
                    "Absence rates are provided broken down by state-funded primary, secondary and special schools. ",
                    "At national and regional level, absence figures are also provided across all schools. ",
                    "In recognition that response rates are not equal across school types and, therefore, ",
                    "not representative of the total school population, the total absence figure for all schools ",
                    "has been weighted based on the Spring 2024 school census. ",
                    "Weighted total figures are not included at local authority level due to the low number of schools involved."
                  ),
                  br(),
                  h3("National statistics"),
                  p("Data relating to pupil attendance, including pupil characteristics, is published at the link below:"),
                  external_link(
                    "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools",
                    "Pupil attendance in schools - 2024/25 academic year"
                  ),
                  br(),
                  br(),
                  p("For 2023/24 full academic year and termly pupil attendance data, including by characteristics, please see the historical publication at the link below:"),
                  external_link(
                    "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-29",
                    "Pupil attendance in schools - 2023/24 academic year"
                  ),
                  br(),
                  br(),
                  p("This dashboard has been developed as an accompaniment to DFE's termly National statistics on pupil absence. You can access this publication through the link below:"),
                  external_link(
                    "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england",
                    "Pupil absence in schools in England"
                  ),
                  br(),
                  br(),
                  p("Statistics presented in this dashboard are based on a smaller, non-random sample of schools in comparison to National statistics. Absence statistics are available on a termly basis in the National statistics, while this dashboard enables more timely daily and weekly data. They should, therefore, be viewed as an early indicator for the more detailed but less regular National Statistics (which will include school level breakdowns)."),
                  p("Data is lagged by 2 weeks in order to allow for any retrospective changes to the data in schools, for example changing an unauthorised absence to late. As a result, data presented may change between dashboard updates."),
                  textOutput("homepage_update_dates"),
                  br(),
                  p("Data prior to 09 September 2024 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level estimates covering the week commencing 02 September 2024 is available in the underlying data of the publication linked below: "),
                  external_link("https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-37", "Pupil attendance in schools - First publication of 2024/25 academic year"),
                  br(),
                  br(),
                  p(
                    strong(
                      "From the start of the 2024/25 academic year, it became mandatory for schools to ",
                      external_link("https://www.gov.uk/guidance/share-your-daily-school-attendance-data", "share attendance data"),
                      " with the DfE. If you are a school that is not already sharing your daily attendance data, you need to approve this in your Wonde portal. This will also give you, your local authority and your multi-academy trust (if applicable)",
                      external_link("https://www.gov.uk/guidance/access-your-school-attendance-data", "access to daily attendance reports"),
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
          inputId = "details", label = textOutput("dropdown_label"), contents =
            div(
              id = "div_a",
              # class = "well",
              # style = "min-height: 100%; height: 100%; overflow-y: visible",
              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "school_choice",
                    label = "Choose school type:",
                    choices = school_type_lookup %>% dplyr::filter(geographic_level == "National") %>% dplyr::select(school_type) %>% unique() %>% as.data.table(),
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
                      choices = c(most_recent_week_dates = "latestweeks", ytd_dates = "yeartodate"),
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
                  downloadButton(class = "btn", "downloadData2", label = "Download data", style = "width:100%;white-space:normal;")
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
            headline_panel()
          ),
          tabPanel(
            value = "reasons",
            title = "Reasons",
            fluidPage(
              fluidRow(
                column(
                  width = 12,
                  h3(textOutput("reasons_chart_title")),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'latestweeks'"),
                    p("Absence rates presented on the chart below are calculated on a daily basis. Each point on the chart shows an absence rate calculated across all sessions in the given day."),
                    p("Absence rates presented in the blue boxes and tables below are calculated across all sessions in the latest week."),
                  ),
                  conditionalPanel(
                    condition = paste0("input.ts_choice == 'yeartodate'"),
                    p("Absence rates presented on the chart below are calculated on a weekly basis. Each point on the chart shows an absence rate calculated across all sessions in the given week."),
                    p("Absence rates presented in the blue boxes and tables below are calculated across all sessions in the year to date.")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  girafeOutput("absence_reasons_timeseries")
                ),
                column(
                  width = 3,
                  uiOutput("absence_rates_value_boxes")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("absence_auth_table_title"),
                  tags$h5("Authorised"),
                  reactableOutput("absence_auth_reasons_reactable"),
                  tags$h5("Unauthorised"),
                  reactableOutput("absence_unauth_reasons_reactable"),
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
                      p(strong("Select absence rate of interest from drop down menu to view on the map:")),
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
                  reactableOutput("absence_reasons_la_reactable")
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
          br("The dashboard provides data on attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type. Drop-down menus at the top of the page allow customisation of breakdowns."),
          br(),
          p("Users should be aware"),
          tags$ul(
            tags$li("Estimates for non-response - In recognition that response rates are not equal across school types and, therefore, not representative of the total school population, the total rates for all schools has been weighted based on the Spring 2024 school census. Weighted total figures are not included at local authority level due to the low number of schools involved."),
            tags$li("Reporting lag - Schools update their registers continually and attendance codes change, resulting in absence rates for a particular day to decrease over time. Analysis of data from the Summer 2022 term suggests that this could be a decrease in the absence rate of around 1 percentage point before settling down. Historical figures will be recalculated in each publication."),
            tags$li("Data prior to 09 September 2024 has not been included in the dashboard due to the impact of different start dates, inset days and phased returns. National level data covering the week commencing 02 September 2024 is available in the underlying data of ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/2024-week-37", "this publication", .noWS = c("after")), "."),
          ),
          br(),
          br(),
          p("Full information on methodologies and further technical notes are available through", a(href = "https://explore-education-statistics.service.gov.uk/methodology/pupil-attendance-in-schools", "Explore Education Statistics", .noWS = c("after")), "."),
          p(strong("Definitions for common attendance terms")),
          DTOutput("notesTableReasons"),
        )
      )
    )
  )
}

# accessibility panel
accessibility_panel <- function() {
  tabPanel(
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Accessibility statement"),
          br("This accessibility statement applies to the Pupil attendance and absence in schools in England data dashboard.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application
            and have actively developed this application with accessibilty in mind."),
          h3("WCAG 2.1 compliance"),
          br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements", .noWS = c("after"), onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), ". This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool", .noWS = c("after")), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool", .noWS = c("after")), ". This means that this application:"),
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
            a(href = "mailto:schools.statistics@education.gov.uk", "schools.statistics@education.gov.uk", .noWS = c("after")), "."
          )
        )
      )
    )
  )
}

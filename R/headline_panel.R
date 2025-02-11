headline_panel <- function() {
  fluidPage(
    fluidRow(
      column(
        width = 12,
        uiOutput("headline_title"),
        bslib::layout_columns(
          col_widths = c(12, 12),
          bslib::card(
            bslib::card_body(
              tags$h4("School return rate"),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'latestweeks'"),
                textOutput("school_count_proportion_weekly"),
                textOutput("update_dates")
              ),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'yeartodate'"),
                textOutput("school_count_proportion_weekly2"),
                textOutput("update_dates2")
              ),
              uiOutput("headline_bullet_attendance_rate"),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'yeartodate'"),
                br(),
                p(strong(paste0("Persistent absence across year to date"))),
                p("A pupil enrolment is identified as persistently absent if they have missed 10% or more of their possible sessions in the year to date.")
              ),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'yeartodate' && input.geography_choice == 'National'"),
                textOutput("ytd_pa_rate_nat")
              ),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'yeartodate' && input.geography_choice == 'Regional'"),
                textOutput("ytd_pa_rate_reg")
              ),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'yeartodate' && input.geography_choice == 'Local authority'"),
                textOutput("ytd_pa_rate_la")
              ),
              conditionalPanel(
                condition = paste0("input.ts_choice == 'latestweeks'"),
                br(),
                p(strong(paste0("To view persistent absence figures, select “year to date” in the drop-down menu. Figures are not provided in the weekly or daily data because persistent absence is a measure over time and not valid for short time periods. Underlying data relating to the Summer, Spring and Autumn terms and year to date is available at the link below:"))),
                a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Pupil attendance in schools")
              )
            )
          ),
          bslib::card(
            bslib::card_body(
              uiOutput("headline_ts_chart_title"),
              ggiraph::girafeOutput(
                "headline_absence_chart",
                width = "100%", height = "100%"
              )
            )
          )
        )
      )
    )
  )
}

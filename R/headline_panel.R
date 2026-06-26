headline_panel <- function() {
  fluidPage(
    fluidRow(
      column(
        width = 12,
        uiOutput("headline_title"),
        bslib::card(
          bslib::card_body(
            tags$h4("School return rate"),
            textOutput("school_count_proportion"),
            conditionalPanel(
              condition = paste0("input.ts_choice == 'latestweeks'"),
              textOutput("update_dates")
            ),
            conditionalPanel(
              condition = paste0("input.ts_choice == 'yeartodate'"),
              textOutput("update_dates2")
            ),
            uiOutput("headline_bullet_attendance_rate"),
            tags$h4("Persistent absence across year to date"),
            uiOutput("headline_persistent_absence")
          )
        ),
        br(),
        bslib::card(
          full_screen = TRUE,
          bslib::card_body(
            style = "padding: 0;",
            uiOutput("headline_ts_chart_title"),
            plotly::plotlyOutput(
              "headline_absence_chart",
              height = "600px"
            ),
            tags$br(),
            tags$h5("Underlying data for the chart"),
            reactableOutput("headline_chart_table")
          )
        )
      )
    )
  )
}

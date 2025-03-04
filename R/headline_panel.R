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
              tags$h4("Persistent absence across year to date"),
              uiOutput("headline_persistent_absence"),
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

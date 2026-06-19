headline_panel <- function() {
  fluidPage(
    fluidRow(
      column(
        width = 12,
        uiOutput("headline_title"),

        # ✅ TEXT CARD (TOP)
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

        # ✅ CHART CARD (UNDERNEATH)
        bslib::card(
          full_screen = TRUE,
          bslib::card_body(
            style = "padding: 0;",
            uiOutput("headline_ts_chart_title"),
            ggiraph::girafeOutput(
              "headline_absence_chart",
              width = "100%",
              height = "600px"
            )
          )
        )
      )
    )
  )
}

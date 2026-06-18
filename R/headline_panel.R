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
          bslib::card_body(
            uiOutput("headline_ts_chart_title"),
            br(),
            div(
              style = "display:flex; justify-content:center;",
              ggiraph::girafeOutput(
                "headline_absence_chart",
                width = "100%",
                height = "450px"
              )
            )
          )
        )
      )
    )
  )
}

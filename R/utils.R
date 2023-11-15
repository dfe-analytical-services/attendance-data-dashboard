# disconnect duck ---------------------------------------------------------
customDisconnectMessage <- function(refresh = "Refresh page",
                                    background = "#000000",
                                    colour = "#FFFFFF",
                                    refreshColour = "#337AB7",
                                    overlayColour = "#000000",
                                    overlayOpacity = 0.6,
                                    width = "full",
                                    top = "center",
                                    size = 24) {
  checkmate::assert_string(refresh)
  checkmate::assert_numeric(size, lower = 0)
  checkmate::assert_string(background)
  checkmate::assert_string(colour)
  checkmate::assert_string(overlayColour)
  checkmate::assert_number(overlayOpacity, lower = 0, upper = 1)
  checkmate::assert_string(refreshColour)

  if (width == "full") {
    width <- "100%"
  } else if (is.numeric(width) && width >= 0) {
    width <- paste0(width, "px")
  } else {
    stop("disconnectMessage: 'width' must be either an integer, or the string \"full\".", call. = FALSE)
  }

  if (top == "center") {
    top <- "50%"
    ytransform <- "-50%"
  } else if (is.numeric(top) && top >= 0) {
    top <- paste0(top, "px")
    ytransform <- "0"
  } else {
    stop("disconnectMessage: 'top' must be either an integer, or the string \"center\".", call. = FALSE)
  }

  htmltools::tagList(
    htmltools::tags$script(
      paste0(
        "$(function() {",
        "  $(document).on('shiny:disconnected', function(event) {",
        "    $('#custom-disconnect-dialog').show();",
        "    $('#ss-overlay').show();",
        "  })",
        "});"
      )
    ),
    htmltools::tags$div(
      id = "custom-disconnect-dialog",
      style = "display: none !important;",
      htmltools::tags$div(
        id = "ss-connect-refresh",
        tags$p("You've lost connection to the dashboard server - please try refreshing the page:"),
        tags$p(tags$a(
          id = "ss-reload-link",
          href = "#", "Refresh page",
          onclick = "window.location.reload(true);"
        )),
        tags$p("If this persists, you can also view the dashboard at one of our mirror sites:"),
        tags$p(
          tags$a(href = "https://department-for-education.shinyapps.io/pupil-attendance-in-schools-overflow", "Mirror a"),
          " - ",
          tags$a(href = "https://department-for-education.shinyapps.io/pupil-attendance-in-schools", "Mirror b")
        ),
        tags$p(
          "All the data used in this dashboard can also be viewed or downloaded via the ",
          tags$a(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools",
            "Pupil Attendance in Schools"
          ),
          "on Explore Education Statistics."
        ),
        tags$p(
          "Please contact",
          tags$a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk"),
          "with details of any problems with this resource."
        )
        #  ),
        # htmltools::tags$p("If this persists, you can view tables and data via the ",htmltools::tags$a(href ='https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools', "Pupil attendance in schools")," release on Explore Education Statistics and please contact statistics.development@education.gov.uk with details of what you were trying to do.")
      )
    ),
    htmltools::tags$div(id = "ss-overlay", style = "display: none;"),
    htmltools::tags$head(htmltools::tags$style(
      glue::glue(
        .open = "{{", .close = "}}",

        ## This hides the old message
        "#ss-connect-dialog { display: none !important; }", # rsconnect
        "#shiny-disconnected-overlay { display: none !important; }", # local

        "#ss-overlay {
             background-color: {{overlayColour}} !important;
             opacity: {{overlayOpacity}} !important;
             position: fixed !important;
             top: 0 !important;
             left: 0 !important;
             bottom: 0 !important;
             right: 0 !important;
             z-index: 99998 !important;
             overflow: hidden !important;
             cursor: not-allowed !important;
          }",
        "#custom-disconnect-dialog {
             background: {{background}} !important;
             color: {{colour}} !important;
             width: {{width}} !important;
             transform: translateX(-50%) translateY({{ytransform}}) !important;
             font-size: {{size}}px !important;
             top: {{top}} !important;
             position: fixed !important;
             bottom: auto !important;
             left: 50% !important;
             padding: 0.8em 1.5em !important;
             text-align: center !important;
             height: auto !important;
             opacity: 1 !important;
             z-index: 99999 !important;
             border-radius: 3px !important;
             box-shadow: rgba(0, 0, 0, 0.3) 3px 3px 10px !important;
          }",
        "#custom-disconnect-dialog a {
             display: {{ if (refresh == '') 'none' else 'inline' }} !important;
             color: {{refreshColour}} !important;
             font-size: {{size}}px !important;
             font-weight: normal !important;
          }"
        # "#custom-disconnect-dialog a::before {
        #     content: '{{refresh}}';
        #     font-size: {{size}}px;
        #   }"
      )
    ))
  )
}


roundFiveUp <- function(x) {
  ceiling(x / 5) * 5
}

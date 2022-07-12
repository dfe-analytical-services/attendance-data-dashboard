support_links <- function() {
  div(
    h2("Give us feedback"),
    "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
    a(
      href = "https://forms.office.com",
      "feedback form", .noWS = c("after")
    ), ".", br(),
    "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
    a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk", .noWS = c("after")), ".",
    br(),
    h2("Find more information on the data"),
    "The data used to produce the dashboard, along with methodological information can be found on ",
    a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics", .noWS = c("after")),
    ".",
    br(),
    h2("Contact us"),
    "If you have questions about the dashboard or data within it, please contact us at ",
    a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk", .noWS = c("after")), br(),
    h2("See the source code"),
    "The source code for this dashboard is available in our ",
    a(href = "https://github.com/dfe-analytical-services/shiny-template", "GitHub repository", .noWS = c("after")),
    ".",
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}

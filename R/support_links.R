support_links <- function() {
  gov_main_layout(
    gov_row(
      h2("Give us feedback"),
      "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
      a(
        href = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-U4ie_t5E21MlsudeT67Fb5UQ0s1NFoxMUo4RjRYT080SFRMUUxVNUg5Uy4u",
        "feedback form", .noWS = c("after")
      ), ".",
      br(),
      "Your feedback is incredibly valuable and has previously been used to develop easier filtering and a glossary of terms.",
      br(),
      "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
      a(href = "mailto:schools.statistics@education.gov.uk", "schools.statistics@education.gov.uk", .noWS = c("after")), ".",
      br(),
      h2("Find more information on the data"),
      p(
        "The release associated with this dashboard can be found on ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools", "Explore Education Statistics "),
        "alongside ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools/data-guidance", "data guidance"),
        " and ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools#explore-data-and-files", "underlying data.")
      ),
      p("Featured tables associated with this dashboard can also be found through the ", a(href = "https://explore-education-statistics.service.gov.uk/data-tables/pupil-attendance-in-schools", "Explore Education Statistics table tool.")),
      h2("Contact us"),
      "If you have questions about the dashboard or data within it, please contact us at ",
      a(href = "mailto:schools.statistics@education.gov.uk", "schools.statistics@education.gov.uk", .noWS = c("after")), br(),
      h2("See the source code"),
      "The source code for this dashboard is available in our ",
      a(href = "https://github.com/dfe-analytical-services/attendance-data-dashboard", "GitHub repository", .noWS = c("after")),
      ".",
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    )
  )
}

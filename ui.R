# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

ui <- function(input, output, session) {
  fluidPage(
    useShinyjs(),
    tags$head(includeHTML(("google-analytics.html"))),
    includeCSS("www/dfe_shiny_gov_style.css"),
    useShinydashboard(),
    # use_tota11y(), # accessibility layer for local testing

    # Set metadata for browser ==================================================

    tags$html(lang = "en"),
    # meta_general(
    #   application_name = "DfE Analytical Services R-Shiny Template",
    #   description = "R-Shiny template for use by DfE external data dashboards",
    #   robots = "index,follow",
    #   generator = "R-Shiny",
    #   subject = "Education data dashboards",
    #   rating = "General",
    #   referrer = "no-referrer"
    # ),

    # Set title for search engines
    HTML("<title>DfE Analytical Services R-Shiny Template</title>"),

    # Navbar ====================================================================

    # This CSS sets the 4th item on the navbar to the right
    tagList(
      tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           }
                           ")))
    ),
    navbarPage("",
      id = "navbar",

      # Homepage tab ============================================================

      tabPanel(
        "Homepage",
        fluidPage(
          fluidRow(
            column(
              12,
              h1("DfE Analytical Services R-Shiny data dashboard template"),
              p("This app demonstrates the DfE Analytical Services R-Shiny data dashboard template."),
              br(),
              br()
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
                    tags$div(
                      title = "This section is useful if you want to understand how well different industries retain graduates.",
                      h3(actionLink("link_to_app_content_tab", "App Content"))
                    ),
                    br()
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
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        value = "app_content",
        "App content",

        # Define UI for application that draws a histogram

        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
            width = 2,
            sliderInput("bins",
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30
            )
          ),

          # Show a plot of the generated distribution
          mainPanel(
            width = 10,
            valueBoxOutput("box_info", width = 6),
            plotOutput("distPlot"),
            br()
            # add box to show user input
          )
        )
      ),

      # Create the accessibility statement-----------------
      tabPanel(
        "Accessibility",
        h2("Accessibility statement"),
        br("This accessibility statement applies to the **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
        h3("WCAG 2.1 compliance"),
        br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
        tags$div(tags$ul(
          tags$li("uses colours that have sufficient contrast"),
          tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
          tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
        )),
        h3("Limitations"),
        br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
        tags$div(tags$ul(
          tags$li("List"),
          tags$li("known"),
          tags$li("limitations, e.g."),
          tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
        )),
        h3("Feedback"),
        br(
          "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
          a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
        )
      ), # End of accessibility tab
      # Support links ===========================================================

      tabPanel(
        "Support and feedback",
        support_links() # defined in R/supporting_links.R
      ),
      # Footer ====================================================================

      shinyGovstyle::footer(TRUE)
    ) # End of navBarPage
  )
}

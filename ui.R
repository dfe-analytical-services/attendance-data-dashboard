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
    #    shinya11y::use_tota11y(),
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title("Pupil attendance and absence in schools in England")
    ),
    shinyjs::useShinyjs(),

    # Setting custom disconnect message --------------------------------------------------------------------------------

    customDisconnectMessage(),
    useShinydashboard(),
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      name = "DfE pupil attendance and absence in schools in England"
    ),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    dfeshiny::header(
      header = "Pupil attendance and absence in schools in England"
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      paste0(
        "This Dashboard is in beta phase and we are still reviewing performance and reliability. "
      )
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 8),
      well = FALSE,
      homepage_panel(),
      dashboard_panel(),
      notes_panel(),
      shiny::tabPanel(
        value = "cookies_panel_ui",
        "Cookies",
        gov_main_layout(
          cookies_panel_ui(google_analytics_key = google_analytics_key)
        )
      ),
      shiny::tabPanel(
        value = "support_panel",
        "Support and feedback",
        gov_main_layout(
          support_panel(
            team_email = "schools.statistics@education.gov.uk",
            repo_name = "https://github.com/dfe-analytical-services/attendance-data-dashboard",
            publication_name = "Pupil attendance in schools",
            publication_slug = "pupil-attendance-in-schools",
            form_url = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-U4ie_t5E21MlsudeT67Fb5UQ0s1NFoxMUo4RjRYT080SFRMUUxVNUg5Uy4u"
          )
        )
      ),
      shiny::tabPanel(
        value = "a11y_panel",
        "Accessibility",
        a11y_panel(
          dashboard_title = site_title,
          dashboard_url = site_primary,
          date_tested = "26/11/2023",
          date_prepared = "27/11/2023",
          date_reviewed = "27/11/2023",
          issues_contact = team_email,
          publication_name = "Pupil attendance in schools in England",
          publication_slug = "pupil-attendance-in-schools"
        )
      )
    ),
    gov_layout(
      size = "full",
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br()
    ),
    tags$script(
      src = "script.js"
    ),
    tags$script(HTML(
      "
    function plotZoom(el){
        el = $(el);
        var parent = el.parent().parent();
        if(el.attr('data-full_screen') === 'false') {
            $('html').css('visibility', 'hidden');
            parent.addClass('full-screen').trigger('resize').hide().show();
            $('.fullscreen-button').text('Exit full screen');
            el.attr('data-full_screen', 'true');
            setTimeout(function() {
              $('html').css('visibility', 'visible');
            }, 700);
        } else {
            parent.removeClass('full-screen').trigger('resize').hide().show();
            $('.fullscreen-button').text('View full screen');
            el.attr('data-full_screen', 'false');
        }
    }
    $(function(){
       $('.plotly-full-screen  .plotly.html-widget').append(
        `
        <div style='position: relative;'>
            <button onclick=plotZoom(this) class='plot-zoom' data-full_screen='false' title='Full screen'>
                <a href='#' class='govuk-link fullscreen-button'>View full screen</a>
            </button>
        </div>
        `);
    })
    "
    )),
    footer(full = TRUE)
  )
}

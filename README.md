<h1 align="center">
  <br>
DfE Attendance data dashboard  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction

The dashboard provides users with an opportunity to investigate pupil attendance and absence at National, Regional and Local Authority geographic levels. Data is available across state-funded primary, secondary and special schools and can also be broken down by individual school type.

Live version of the dashboard can be accessed at
- https://department-for-education.shinyapps.io/pupil-attendance-in-schools/

The dashboard is split across multiple tabs:

- <b>Headlines</b> includes information on attendance, overall absence (including authorised and unauthorised absence) in the most recent week and across the year to date 
- <b>Reasons</b> includes information on authorised and unauthorised absence including the individual reasons for absence
- <b>Local authority data</b> includes information on the overall, authorised and unauthorised absence rates for each local authority in the most recent week

The dashboard also includes further information on the data itself on the technical notes tab, alongside accessibility and information on where to find further support. 

---

## Requirements

### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)

---

## How to use


### Running the app locally

1. Clone or download the repo.

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions, to https://department-for-education.shinyapps.io/pupil-attendance-in-schools/. The yaml file for this can be found in the .github/workflows folder.

If you have any questions about the shinyapps.io subscription and deployment in DfE please contact the Statistics Development Team at [statistics.development@education.gov.uk](mailto:statistics.development@education.gov.uk).

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.


---

## How to contribute

Our contributing guidelines can be found at [https://github.com/dfe-analytical-services/attendance-data-dashboard/blob/main/CONTRIBUTING.md](https://github.com/dfe-analytical-services/attendance-data-dashboard/blob/main/CONTRIBUTING.md).

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the team can merge pull requests. Add GemmaSelby29 as a requested reviewer, and the team will review before merging.

---

## Contact

If you have any questions about the dashboard please contact [schools.statistics@education.gov.uk](mailto:schools.statistics@education.gov.uk).

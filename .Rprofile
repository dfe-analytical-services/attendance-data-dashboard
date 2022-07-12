# ---------------------------------------------------------
# This is the .Rprofile file
#
# Use it to include any functions you want to run before any other code is run.
# For example, using renv automatically sources its activate script to the .RProfile file
# This ensures that all renv checks on package versions happens before any code is run.
#
#
# ---------------------------------------------------------

source("renv/activate.R")

# Tidy code function
tidy_code <- function() {
  source("global.r")
  tidy_code_function()
}

# Function to run tests
run_tests_locally <- function() {
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  source("global.r")
  # message("================================================================================")
  # message("== testthat ====================================================================")
  # message("")
  # testthat::test_dir("tests/testthat")
  # message("")
  message("================================================================================")
  message("== shinytest ===================================================================")
  message("")
  shinytest::testApp()
  message("")
  message("================================================================================")
}

# ---------------------------------------------------------
# This is the .Rprofile file
#
# Use it to include any functions you want to run before any other code is run.
# For example, using renv automatically sources its activate script to the .RProfile file
# This ensures that all renv checks on package versions happens before any code is run.
#
#
# ---------------------------------------------------------

cat("Sourcing .Rprofile.", fill = TRUE)

source("renv/activate.R")
renv::status()

# Run UI tests ------------------------------------------------------------

chromote::local_chrome_version(binary = "chrome-headless-shell", quiet = FALSE)
run_tests_locally <- function() {
  shinytest2::test_app()
}


# Install commit-hooks locally
statusWriteCommit <- file.copy(".hooks/pre-commit.R", ".git/hooks/pre-commit", overwrite = TRUE)

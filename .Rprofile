# ---------------------------------------------------------
# This is the .Rprofile file
#
# Use it to include any functions you want to run before any other code is run.
# For example, using renv automatically sources its activate script to the .RProfile file
# This ensures that all renv checks on package versions happens before any code is run.
#
#
# ---------------------------------------------------------

# Auto-detect Chrome path for chromote
detect_chrome_path <- function() {
  possible_paths <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google/Chrome/Application/chrome.exe")
  )

  for (path in possible_paths) {
    if (file.exists(path)) {
      options(chromote.chrome = path)
      message("✅ Chrome path set to: ", path)
      return(invisible(path))
    }
  }

  stop("❌ Chrome not found in common locations.")
}

detect_chrome_path()

cat("Sourcing .Rprofile.", fill = TRUE)

# Activate renv
source("renv/activate.R")

# Run UI tests ------------------------------------------------------------

run_tests_locally <- function() {
  cs <- chromote::ChromoteSession$new()
  shinytest2::test_app(chromote_session = cs)
}

# Install commit-hooks locally
if (dir.exists(".hooks")) {
  statusWriteCommit <- file.copy(".hooks/pre-commit.R", ".git/hooks/pre-commit", overwrite = TRUE)
}

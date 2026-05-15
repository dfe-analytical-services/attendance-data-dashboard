render_percents <- function(values) {
  sprintf(
    "%.2f%%",
    dfeR::round_five_up(as.numeric(values), dp = 2)
  ) |>
    stringr::str_replace("NA%", "x")
}

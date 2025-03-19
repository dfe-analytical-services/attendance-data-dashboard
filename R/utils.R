render_percents <- function(values) {
  dfeR::round_five_up(
    as.numeric(
      values
    ),
    dp = 1
  ) |>
    paste0(
      "%"
    ) |>
    stringr::str_replace("NA%", "x")
}

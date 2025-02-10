headline_bullet <- function(
    value,
    comparator_value,
    statistic_name,
    geographic_level,
    la_name,
    region_name) {
  if (geographic_level == "Local authority") {
    area_string <- paste("in", area_name)
    comparator_level <- "REG"
    comparator_name <- paste("the", region_name) |>
      stringr::str_replace("the London", "London")
  } else if (geographic_level == "Regional") {
    area_string <- paste("in the", region_name) |>
      stringr::str_replace("the London", "London")
    comparator_level <- "NAT"
    comparator_name <- "England"
  } else {
    area_string <- ""
    comparator_level <- NULL
  }
  paste(
    paste0(
      value |>
        as.numeric() |>
        dfeR::round_five_up(dp = 1), "%"
    ),
    "of sessions were recorded as",
    statistic_name,
    area_string,
    ifelse(
      !is.null(comparator_level),
      paste0(
        "(compared to ",
        comparator_value |>
          as.numeric() |>
          dfeR::round_five_up(dp = 1),
        "% of sessions in ", comparator_name, ")"
      ),
      ""
    )
  )
}

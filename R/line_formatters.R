headline_bullet <- function(
    value,
    comparator_value,
    statistic_name,
    geographic_level,
    la_name_in,
    region_name_in,
    subject = "sessions") {
  if (geographic_level == "Local authority") {
    area_string <- paste("in", la_name_in)
    comparator_level <- "REG"
    comparator_name <- paste(
      "the",
      region_la_lookup |>
        filter(la_name == la_name_in) |>
        pull(region_name)
    ) |>
      stringr::str_replace("the London", "London")
  } else if (geographic_level == "Regional") {
    area_string <- paste("in the", region_name_in) |>
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
    "of",
    subject,
    "were recorded as",
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

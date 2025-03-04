headline_absence_ggplot <- function(reasons, scope) {
  if (scope == "latestweeks") {
    plot_data <- reasons |>
      filter(
        time_frame != "Week",
        time_frame != "Year to date"
      )
    date_breaks <- "1 day"
  } else {
    plot_data <- reasons |>
      filter(
        time_frame == "Week"
      )
    date_breaks <- "1 month"
  }
  plot_data <- plot_data |>
    filter(
      attendance_reason %in% c("All authorised absence", "All unauthorised absence", "Overall absence")
    ) |>
    arrange(attendance_type, reference_date) |>
    mutate(
      session_percent = as.numeric(session_percent),
      attendance_reason = case_when(
        attendance_reason == "Overall absence" ~ "Overall absence rate",
        attendance_reason == "All authorised absence" ~ "Authorised absence rate",
        attendance_reason == "All unauthorised absence" ~ "Unauthorised absence rate"
      )
    )
  dates <- plot_data |>
    pull(reference_date)
  ggplot(
    plot_data,
    aes(
      x = reference_date,
      y = session_percent,
      colour = attendance_type
    )
  ) +
    geom_point_interactive(
      aes(
        tooltip = paste0(
          date_stamp(lubridate::ymd(reference_date)), "\n",
          attendance_type, ": ",
          session_percent, "%"
        )
      )
    ) +
    geom_line_interactive() +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.2)), # This adds 20% of scale as white space
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    afcharts::theme_af() +
    scale_colour_manual(values = afcharts::af_colour_palettes[["main6"]] |> unname()) +
    labs(
      x = year(dates) |>
        unique() |>
        sort() |>
        paste(collapse = "/"),
      y = "%",
      colour = NULL
    ) +
    theme(legend.position = "bottom", text = element_text(family = plotting_font_family)) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
}

reasons_ggplot <- function(reasons, scope) {
  if (scope == "latestweeks") {
    plot_data <- reasons |>
      filter(
        time_frame != "Week",
        time_frame != "Year to date"
      )
    date_breaks <- "1 day"
  } else {
    plot_data <- reasons |>
      filter(
        time_frame == "Week"
      )
    date_breaks <- "1 month"
  }
  plot_data <- plot_data |>
    filter(
      attendance_reason %in% c(
        "Illness (i)",
        "Medical dental (m)",
        "Temporary reduced timetable (c2)",
        "Unauthorised holiday (g)",
        "Other unauthorised (o)"
      )
    ) |>
    arrange(attendance_type, reference_date) |>
    mutate(
      session_percent = as.numeric(session_percent)
    )

  dates <- plot_data |>
    pull(reference_date)
  ggplot(
    plot_data,
    aes(
      x = reference_date,
      y = session_percent,
      colour = attendance_reason
    )
  ) +
    geom_point_interactive(
      aes(
        tooltip = paste0(
          date_stamp(lubridate::ymd(reference_date)), "\n",
          attendance_type, ": ",
          session_percent, "%"
        )
      )
    ) +
    geom_line_interactive() +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.2)), # This adds 20% of scale as white space
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    afcharts::theme_af() +
    scale_colour_manual(
      values = afcharts::af_colour_palettes[["main6"]] |>
        unname() |>
        magrittr::extract(c(1, 2, 3, 4, 6))
    ) +
    labs(
      x = year(dates) |>
        unique() |>
        sort() |>
        paste(collapse = "/"),
      y = "%",
      colour = NULL
    ) +
    theme(legend.position = "bottom", text = element_text(family = plotting_font_family)) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
}

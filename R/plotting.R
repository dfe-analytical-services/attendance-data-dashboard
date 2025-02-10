headline_absence_ggplot <- function(reasons, scope) {
  if (scope == "latestweeks") {
    plot_data <- reasons |>
      filter(
        time_frame != "Week",
        time_frame != "Year to date"
      )
  } else {
    plot_data <- reasons |>
      filter(
        time_frame == "Week"
      )
  }
  plot_data <- plot_data |>
    filter(
      attendance_type %in% c("Authorised", "Unauthorised", "All absence"),
      attendance_reason %in% c("All authorised", "All unauthorised", "All absence")
    ) |>
    arrange(attendance_type, reference_date) |>
    mutate(session_percent = as.numeric(session_percent))
  print(plot_data)
  ggplot(
    plot_data,
    aes(
      x = reference_date,
      y = session_percent,
      colour = attendance_type
    )
  ) +
    geom_point_interactive() +
    geom_line_interactive() +
    afcharts::theme_af() +
    labs(x = "Date", y = "") +
    theme(legend.position = "bottom", text = element_text(size = 12))
}

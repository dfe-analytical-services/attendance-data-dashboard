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
      filter(time_frame == "Week")
    date_breaks <- "1 month"
  }

  plot_data <- plot_data |>
    filter(
      attendance_reason %in%
        c("All authorised", "All unauthorised", "Overall absence")
    ) |>
    arrange(attendance_reason, reference_date) |>
    mutate(
      session_percent = suppressWarnings(as.numeric(session_percent)),
      attendance_reason = case_when(
        attendance_reason == "Overall absence" ~ "Overall absence rate",
        attendance_reason == "All authorised" ~ "Authorised absence rate",
        attendance_reason == "All unauthorised" ~ "Unauthorised absence rate"
      ),
      attendance_reason = factor(
        attendance_reason,
        levels = c(
          "Overall absence rate",
          "Authorised absence rate",
          "Unauthorised absence rate"
        )
      )
    ) |>
    group_by(reference_date) |>
    mutate(
      tooltip_all = paste0(
        "<b>",
        ifelse(
          scope == "latestweeks",
          date_stamp(reference_date),
          paste0("Week commencing ", date_stamp(reference_date))
        ),
        "</b><br><br>",
        paste0(
          "<span style='color:",
          case_when(
            attendance_reason == "Overall absence rate" ~ "#12436D",
            attendance_reason == "Authorised absence rate" ~ "#28A197",
            attendance_reason == "Unauthorised absence rate" ~ "#F46A25"
          ),
          ";'>●</span> ",
          attendance_reason,
          ": ",
          sprintf("%.2f%%", session_percent),
          collapse = "<br>"
        )
      )
    ) |>
    ungroup()

  # ✅ one row per date (for snapping)
  hover_data <- plot_data |>
    distinct(reference_date, tooltip_all)

  # ✅ bounds for hover area
  y_min <- 0
  y_max <- max(plot_data$session_percent, na.rm = TRUE)

  dates <- plot_data |> pull(reference_date)

  ggplot(
    plot_data,
    aes(
      x = reference_date,
      y = session_percent,
      colour = attendance_reason,
      group = attendance_reason
    )
  ) +

    # ✅ FULL HEIGHT HOVER BAND (adaptive width for YTD + daily)
    geom_rect_interactive(
      data = hover_data,
      inherit.aes = FALSE,
      aes(
        xmin = reference_date - ifelse(scope == "latestweeks", 0.5, 3),
        xmax = reference_date + ifelse(scope == "latestweeks", 0.5, 3),
        tooltip = tooltip_all,
        data_id = reference_date
      ),
      ymin = y_min,
      ymax = y_max,
      fill = "transparent",
      alpha = 0
    ) +

    # ✅ Vertical guide line
    geom_vline_interactive(
      data = hover_data,
      inherit.aes = FALSE,
      aes(
        xintercept = reference_date,
        data_id = reference_date
      ),
      colour = "#7A7A7A",
      linewidth = 0.7,
      linetype = "dashed",
      alpha = 0
    ) +

    # ✅ Lines (slightly thicker for YTD)
    geom_line_interactive(
      linewidth = ifelse(scope == "latestweeks", 1, 1.3),
      aes(data_id = reference_date)
    ) +

    # ✅ Points (visual only)
    geom_point(size = 2.5) +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b") +
    afcharts::theme_af() +
    scale_colour_manual(values = c(
      "Overall absence rate" = "#12436D",
      "Authorised absence rate" = "#28A197",
      "Unauthorised absence rate" = "#F46A25"
    )) +
    labs(
      x = year(dates) |> unique() |> sort() |> paste(collapse = "/"),
      y = "%",
      colour = NULL
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(family = dfe_font)
    ) +
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
      attendance_reason %in%
        c(
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
          date_stamp(lubridate::ymd(reference_date)),
          "\n",
          attendance_type,
          ": ",
          session_percent,
          "%"
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
    theme(legend.position = "bottom", text = element_text(family = dfe_font)) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
}

headline_absence_plotly <- function(reasons, scope) {
  # -------------------------
  # Filter data
  # -------------------------
  if (scope == "latestweeks") {
    plot_data <- reasons |>
      dplyr::filter(
        time_frame != "Week",
        time_frame != "Year to date"
      )
  } else {
    plot_data <- reasons |>
      dplyr::filter(time_frame == "Week")
  }

  # -------------------------
  # Data prep
  # -------------------------
  plot_data <- plot_data |>
    dplyr::filter(
      attendance_reason %in%
        c("All authorised", "All unauthorised", "Overall absence")
    ) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      week_commencing = lubridate::floor_date(
        reference_date,
        unit = "week",
        week_start = 1
      ),
      session_percent = as.numeric(session_percent),
      attendance_reason = dplyr::case_when(
        attendance_reason == "Overall absence" ~ "Overall absence rate",
        attendance_reason == "All authorised" ~ "Authorised absence rate",
        attendance_reason == "All unauthorised" ~ "Unauthorised absence rate"
      )
    )

  # -------------------------
  # Colours
  # -------------------------
  colours <- c(
    "Overall absence rate" = "#12436D",
    "Authorised absence rate" = "#28A197",
    "Unauthorised absence rate" = "#F46A25"
  )

  t <- list(
    family = "arial",
    size = 12,
    color = "black"
  )

  # -------------------------
  # Build plot
  # -------------------------
  p <- plotly::plot_ly()

  for (reason in unique(plot_data$attendance_reason)) {
    df <- plot_data |>
      dplyr::filter(attendance_reason == reason)

    x_var <- if (scope == "latestweeks") df$reference_date else df$week_commencing

    p <- p |>
      plotly::add_trace(
        x = x_var,
        y = df$session_percent,
        type = "scatter",
        mode = "lines+markers",
        name = reason,

        # ✅ bold numbers only
        hovertemplate = paste0(
          reason, ": <b>%{y:.2f}%</b><extra></extra>"
        ),
        line = list(
          color = colours[reason],
          width = ifelse(scope == "latestweeks", 2, 3)
        ),
        marker = list(
          size = 6,
          color = colours[reason]
        )
      )
  }

  # -------------------------
  # Layout (MATCH TEMPLATE)
  # -------------------------
  p |>
    plotly::layout(
      hovermode = "x unified",
      xaxis = list(
        title = ifelse(scope == "latestweeks", "", "Week commencing"),
        type = "date",

        # ✅ TEMPLATE MATCH
        tickmode = "linear",
        tick0 = min(plot_data$week_commencing, na.rm = TRUE),
        dtick = ifelse(scope == "latestweeks", 86400000, 14 * 86400000),
        zeroline = FALSE
      ),
      yaxis = list(
        title = "",
        rangemode = "tozero",
        tickformat = ".2f",
        ticksuffix = "%"
      ),
      legend = list(
        orientation = "h",
        yanchor = "top",
        y = -0.5,
        xanchor = "center",
        x = 0.5,
        font = list(size = 12)
      ),
      margin = list(t = 80),
      font = t
    ) |>
    plotly::config(displayModeBar = FALSE)
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

headline_absence_plotly <- function(reasons, scope, title_text = "") {
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
      title = list(
        text = title_text,
        x = 0.5,
        xanchor = "center",
        font = list(size = 18)
      ),
      hovermode = "x unified",
      xaxis = list(
        title = "",
        type = "date",
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
        y = -0.3,
        xanchor = "center",
        x = 0.5
      ),
      margin = list(t = 90),
      font = t
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      displaylogo = FALSE
    )
}

reasons_plotly <- function(reasons, scope, title_text = "") {
  # -------------------------
  # Filter data
  # -------------------------
  if (scope == "latestweeks") {
    plot_data <- reasons |>
      dplyr::filter(
        time_frame != "Week",
        time_frame != "Year to date"
      )
    x_var_name <- "reference_date"
    dtick_val <- 86400000
  } else {
    plot_data <- reasons |>
      dplyr::filter(time_frame == "Week")
    x_var_name <- "week_commencing"
    dtick_val <- 14 * 86400000
  }

  # -------------------------
  # Data prep
  # -------------------------
  plot_data <- plot_data |>
    dplyr::filter(
      attendance_reason %in% c(
        "Illness (i)",
        "Medical dental (m)",
        "Religious observance (r)",
        "Unauthorised holiday (g)",
        "Other unauthorised (o)"
      )
    ) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      week_commencing = lubridate::floor_date(reference_date, "week", week_start = 1),
      session_percent = as.numeric(session_percent),

      # Clean labels
      attendance_reason = dplyr::case_when(
        attendance_reason == "Illness (i)" ~ "Illness",
        attendance_reason == "Medical dental (m)" ~ "Medical appointments",
        attendance_reason == "Religious observance (r)" ~ "Religious observance",
        attendance_reason == "Unauthorised holiday (g)" ~ "Unauthorised holiday",
        attendance_reason == "Other unauthorised (o)" ~ "Unauthorised other"
      )
    ) |>
    dplyr::arrange(attendance_reason, reference_date)

  # Prevent crash if no data
  if (nrow(plot_data) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No data available"))
  }

  # -------------------------
  # Colours
  # -------------------------
  colours <- afcharts::af_colour_palettes[["main6"]] |>
    unname() |>
    magrittr::extract(c(1, 2, 3, 4, 6))

  names(colours) <- unique(plot_data$attendance_reason)

  # -------------------------
  # Build plot
  # -------------------------
  p <- plotly::plot_ly()

  for (reason in unique(plot_data$attendance_reason)) {
    df <- plot_data |>
      dplyr::filter(attendance_reason == reason)

    x_vals <- if (x_var_name == "reference_date") df$reference_date else df$week_commencing

    p <- p |>
      plotly::add_trace(
        data = df,
        x = x_vals,
        y = df$session_percent,
        type = "scatter",
        mode = "lines+markers",
        name = reason,
        hovertemplate = paste0(
          reason, ": <b>%{y:.2f}%</b><extra></extra>"
        ),
        line = list(
          color = colours[reason],
          width = ifelse(scope == "latestweeks", 2, 3)
        ),
        marker = list(
          color = colours[reason],
          size = 6
        )
      )
  }

  # -------------------------
  # Layout (MATCH HEADLINE)
  # -------------------------
  p |>
    plotly::layout(
      title = list(
        text = title_text,
        x = 0.5,
        xanchor = "center",
        font = list(size = 18)
      ),
      hovermode = "x unified",
      xaxis = list(
        title = "",
        type = "date",
        tickmode = "linear",
        tick0 = min(plot_data[[x_var_name]], na.rm = TRUE),
        dtick = dtick_val,
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
        y = -0.3,
        xanchor = "center",
        x = 0.5
      ),
      margin = list(t = 90),
      font = list(family = "arial", size = 12, color = "black")
    ) |>
    plotly::config(displayModeBar = FALSE)
}

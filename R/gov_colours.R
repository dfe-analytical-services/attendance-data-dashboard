# Script to take government approved colour schemes for the app, from: https://github.com/ukgovdatascience/govstyle/blob/master/R/GVA.R

# Create a palette

gov_cols <- c(
  purple      = "#2E358B",
  pink        = "#D53880",
  mellow_red  = "#DF3034",
  yellow      = "#FFBF47",
  turquoise   = "#28A197",
  mauve       = "#6F72AF",
  baby_pink   = "#F499BE",
  orange      = "#F47738",
  green       = "#006435",
  light_blue  = "#2B8CC4",
  fuschia     = "#912B88",
  red         = "#B10E1E",
  brown       = "#B58840",
  grass_green = "#85994B",
  govuk_blue  = "#005EA5"
)

gov_cols_2 <- c(
  "#2E358B",
  "#D53880",
  "#DF3034",
  "#FFBF47",
  "#28A197",
  "#6F72AF",
  "#F499BE",
  "#F47738",
  "#006435",
  "#2B8CC4",
  "#912B88",
  "#B10E1E",
  "#B58840",
  "#85994B",
  "#005EA5"
)

# https://meyerweb.com/eric/tools/color-blend/#FFBF47:B10E1E:3:hex gov red to yellow

map_gov_colours <- c(
  "#FFBF47",
  "#EC933D",
  "#D86733",
  "#C53A28",
  "#B10E1E"
)

# Test the palette

check_pal <- function(
    x = gov_cols) {
  if (is.numeric(x)) {
    if (length(x) > 1) {
      x <- gov_cols[x]
    } else {
      x <- gov_cols[1:x]
    }
  }

  graphics::pie(
    rep(1, length(x)),
    col = x,
    labels = names(x)
  )
}

# Create a theme

theme_gov <- function(
    base_size = 12,
    base_colour = "gray40",
    axes = "x") {
  if (!axes %in% c("n", "x", "y", "xy")) {
    stop("axes must be one of 'n', 'x', 'y', or 'xy'")
  }

  ## Set x and y axis colour

  x_col <- "white"
  y_col <- "white"

  if (axes == "x") {
    x_col <- base_colour
    y_col <- "white"
  }

  if (axes == "y") {
    x_col <- "white"
    y_col <- base_colour
  }

  if (axes == "xy") {
    x_col <- base_colour
    y_col <- base_colour
  }

  theme(
    legend.position = "none",

    ## Adjust tick marks

    axis.ticks = ggplot2::element_blank(),
    # axis.ticks = element_line(colour = "gray40"),
    # axis.ticks.y = element_blank(),
    # axis.ticks.length = grid::unit( -2, "mm"),

    ## Adjust the axis lines

    axis.line = ggplot2::element_line(colour = base_colour),
    axis.line.x = ggplot2::element_line(colour = x_col),
    axis.line.y = ggplot2::element_line(colour = y_col),

    ## Set the overall text attributes

    text = ggplot2::element_text(
      face = "plain", colour = base_colour, size = base_size,
      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
    ),
    axis.text = ggplot2::element_text(colour = base_colour),
    plot.title = ggplot2::element_text(face = "bold", hjust = 1, colour = "black", vjust = -2.5),

    ## Axis title attributes. Adjustments of

    axis.title.y = ggplot2::element_text(hjust = 1, vjust = 1),
    axis.title.x = ggplot2::element_text(hjust = 1, vjust = 0),

    ## Background attributes (currently all blank)

    panel.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),

    ## Adjust the margin around the plot. This is highly sensitive to plot, so
    ## probably needs to be set on a plot by plot basis.

    # plot.margin = grid::unit(c(0,5,5,-30), "mm"),

    ## Strip attributes for facet grid and facet wrap

    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(color = "black", face = "bold", size = base_size + 1),
    strip.text.x = ggplot2::element_text(),
    strip.text.y = ggplot2::element_text(angle = -90),
    complete = FALSE
  )
}

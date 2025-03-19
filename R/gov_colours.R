# Script to take government approved colour schemes for the app, from: https://github.com/ukgovdatascience/govstyle/blob/master/R/GVA.R

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

app <- ShinyDriver$new("../../")
app$snapshotInit("initial_load_test", screenshot = FALSE)

app$snapshot(items = list(
  input = c("bins"),
  output = c("box_info")
))

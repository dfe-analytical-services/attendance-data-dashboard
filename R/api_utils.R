read_api_attendance <- function(
    parse = TRUE,
    time_frame = "Latest week",
    geographic_level = "National",
    establishment_phase = "Primary",
    area_name = "England",
    dataset_id = "d7329101-f275-d277-bbfe-d8cfaa709833") {
  # Get the data-set meta data
  meta_response <- httr::GET(paste0(
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/",
    dataset_id,
    "/meta"
  ))
  filters <- parse_ees_api_meta_filters(meta_response)
  meta <- meta_response %>%
    content("text") %>%
    fromJSON()
  latest_week <- meta$timePeriods %>%
    mutate(year_week = as.numeric(paste0(period, ".", str_pad(gsub("W", "", code), 2, pad = 0)))) %>%
    pull(year_week) %>%
    max() %>%
    format() %>%
    gsub(".*\\.", "", .)
  # Define the query url
  url <- paste0(
    "https://dev.statistics.api.education.gov.uk/api/v1.0/data-sets/",
    dataset_id,
    "/query"
  )

  if (time_frame == "Latest week") {
    time_period_query <- paste0(
      '{
      "timePeriods": {
        "in": [
          {
            "period": "2024",
            "code": "W', latest_week, '"
          }
        ]
      }
    }'
    )
    time_frame_query <- filter_query("time_frame", c("Daily", "Weekly"), filters)
  }

  print(filter_query("establishment_phase", establishment_phase, filters))
  print(time_frame_query)
  # Create the query
  body <- paste0(
    '{
        "criteria": {
           "and": [
             ', geography_query(geographic_level),
    ",", time_period_query,
    ",",
    filter_query("establishment_phase", establishment_phase, filters),
    '
    ]
  },
  "indicators": [
    "session_percent"
  ],
  "debug": true,
  "page": 1,
  "pageSize": 1000
}'
  )
  cat(body, file = "temp.txt")

  response <- httr::POST(
    url,
    body = body,
    encode = "json",
    content_type("application/json")
  )
  print(response)
  output <- content(response)
  if (parse) {
    output <- parse_ees_api_query(output)
  }
  output
}

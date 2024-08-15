# Create a draft meta data frame based on a given data set
meta_template <- function(data) {
  standard_indicators <- c("pupil_count", "pupil_fte", "pupil_percent")
  base_filters <- c(
    "time_period", "time_identifier", "geographic_level",
    "country_code", "country_name",
    "region_code", "region_name",
    "old_la_code", "new_la_code", "la_name",
    "school_laestab",
    "school_urn", "school_name"
  )
  meta <- data.frame(col_name = names(data)) %>%
    filter(
      !(col_name %in% base_filters)
    ) %>%
    mutate(
      col_type = case_when(col_name %in% standard_indicators ~ "Indicator",
        .default = "Filter"
      ),
      label = stringr::str_to_sentence(gsub("_", " ", col_name)),
      indicator_grouping = "",
      indicator_unit = "",
      indicator_dp = "",
      filter_hint = "",
      filter_grouping_column = ""
    )
}


# Parse the meta filters from an EES API meta query
parse_ees_api_meta_filters <- function(api_meta) {
  output <- api_meta %>%
    content("text") %>%
    jsonlite::fromJSON()
  nfilters <- length(output$filters$id)
  filter_table <- data.frame(
    row = 1:nfilters,
    id = output$filters$id,
    label = output$filters$label
  )
  filter_item_lookup <- data.frame(row = NA, item_id = NA, item_label = NA)
  for (i in 1:nfilters) {
    filter_item_lookup <- filter_item_lookup %>%
      rbind(
        output$filters$options[[i]] %>%
          dplyr::select(item_id = id, item_label = label) %>%
          mutate(row = i)
      )
  }
  filter_table %>%
    left_join(filter_item_lookup)
}

# Look up a given filter item from a filter table derived from API gathered
# meta data using parse_ees_api_meta_filters()
get_filter_item_id <- function(colname, item, filter_meta) {
  filter_meta %>%
    filter(
      id == colname,
      item_label %in% c(item)
    ) %>%
    pull(item_id)
}

# Generate an API query entry given a specified column name and item and a
# filter table derived from API gathered meta data using
# parse_ees_api_meta_filters()
filter_query <- function(colname, item, filter_meta) {
  if (length(item) == 1) {
    query <- paste0(
      '  {
        "filters": {
          "eq": "',
      get_filter_item_id(colname, item, filter_meta),
      '"
        }
      }'
    )
  } else {
    query <- paste0(
      '  {
        "filters": {
          "in": ["',
      paste0(get_filter_item_id(colname, item, filter_meta), collapse = '","'),
      '"]
        }
      }'
    )
  }
  query
}

# Running this with brute long code for now so I understand what's going on.
# Updating this to jsonlite as done with the meta parsing above should save a
# few lines of code.
parse_ees_api_query <- function(api_output, clear_codes = TRUE) {
  results <- api_output$results
  print(results)
  dfnames <- c(
    "time_period", "time_identifier", "geographic_level",
    names(results[[1]]$locations),
    names(results[[1]]$filters),
    names(results[[1]]$values)
  )
  df <- data.frame(
    matrix(vector(), 0, length(dfnames),
      dimnames = list(c(), dfnames)
    ),
    stringsAsFactors = F
  )
  if (clear_codes) {
    code_string <- ".*: "
  } else {
    code_string <- "code clearing off"
  }
  for (i in 1:length(results)) {
    df[i, "time_period"] <- results[[i]]$timePeriod$period
    df[i, "time_identifier"] <- results[[i]]$timePeriod$code
    df[i, "geographic_level"] <- results[[i]]$geographicLevel
    for (j in 1:length(results[[i]]$locations)) {
      loc_name <- names(results[[i]]$locations[j])
      df[i, loc_name] <- gsub(code_string, "", results[[i]]$locations[[loc_name]])
    }
    for (j in 1:length(results[[i]]$filters)) {
      filter_name <- names(results[[i]]$filters[j])
      df[i, filter_name] <- gsub(code_string, "", results[[i]]$filters[[filter_name]])
    }
    for (j in 1:length(results[[i]]$values)) {
      value_name <- names(results[[i]]$values[j])
      df[i, value_name] <- gsub(code_string, "", results[[i]]$values[[value_name]])
    }
  }
  df
}

# Generate an API query entry for a given geographic level
geography_query <- function(query_geographic_level) {
  api_geography_lookup <- data.frame(
    geographic_level = c("National"),
    api_geographic_level = c("NAT")
  )
  geography_query <- paste0(
    '  {
        "geographicLevels": {
          "eq": "',
    api_geography_lookup %>%
      filter(geographic_level == query_geographic_level) %>%
      pull(api_geographic_level), '"
        }
      }'
  )
}

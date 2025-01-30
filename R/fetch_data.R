fetch_headline_data <- function(
    establishment_phase,
    time_frame,
    geographic_level,
    region,
    local_authority,
    sqid_lookup,
    dataset_id = "") {
  geography <- geography_code(geographic_level, region, local_authority)
  eesyapi::query_data(
    dataset_id = dataset_id,
    geographies = geography,
    filters = c(),
    indicators = sqid_lookup$session_percent
  )
}

geography_code <- function(geographic_level, region, local_authority) {
  if (geographic_level == "National") {
    geography <- "NAT"
  } else if (geographies == "Regional") {
    code <- dfeR::fetch_regions() |>
      filter(region_name == region) |>
      pull(region_code)
    geography <- paste0("REG|code|", code)
  } else if (geographies == "Local authority") {
    code <- dfeR::fetch_las() |>
      filter(region_name == local_authority) |>
      pull(new_la_code)
    geography <- paste0("LA|code|", code)
  }
  return(geography)
}

fetch_sqid_lookup <- function(
    dataset_id,
    version = NULL) {
  meta <- eesyapi::get_meta(reasons_dataset_id)
  sqid_lookup <- list(
    filters = filter_item_sqid_list(
      meta$filter_items |>
        dplyr::left_join(meta$filter_columns)
    ),
    indicators = indicator_sqid_list(meta$indicators)
  )
  return(sqid_lookup)
}

indicator_sqid_list <- function(indicator_meta) {
  indicator_list <- as.list(
    indicator_meta |>
      pull(col_id)
  )
  names(indicator_list) <- indicator_meta |>
    pull(col_name)
  return(indicator_list)
}


filter_item_sqid_sublist <- function(col_name_ref, filter_lookup) {
  filter_list <- as.list(filter_lookup |> filter(col_name == col_name_ref) |> pull(item_id))
  names(filter_list) <- filter_lookup |>
    filter(col_name == col_name_ref) |>
    pull(item_label) |>
    str_replace_all(" ", "") |>
    tolower()
  return(filter_list)
}

filter_item_sqid_list <- function(filter_lookup) {
  col_names <- filter_lookup |>
    pull(col_name) |>
    unique()
  filter_list <- lapply(col_names, filter_item_sqid_sublist, filter_lookup)
  names(filter_list) <- col_names
  return(filter_list)
}

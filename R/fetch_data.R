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
    indicators = sqid_lookup$session_percent,
    ees_environment = api_environment
  )
}

fetch_sqid_lookup <- function(
    dataset_id,
    version = NULL) {
  meta <- eesyapi::get_meta(
    reasons_dataset_id,
    ees_environment = api_environment
  )
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
    str_replace_all("\\(", "_") |>
    str_replace_all("\\)", "") |>
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

geography_query <- function(input_geographic_level, input_region_name, input_la_name) {
  message(input_geographic_level, input_region_name, input_la_name)
  if (input_geographic_level == "National") {
    return(input_geographic_level)
  } else if (input_geographic_level == "Regional") {
    reg_code <- dfeR::fetch_regions() |>
      dplyr::filter(region_name == input_region_name) |>
      dplyr::pull(region_code)
    return(paste0("REG|code|", reg_code))
  } else if (input_geographic_level == "Local authority") {
    la_code <- dfeR::fetch_las() |>
      dplyr::filter(la_name == input_la_name) |>
      dplyr::pull(new_la_code)
    return(paste0("LA|code|", la_code))
  }
}

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
  filter_lookup <- meta$filter_items |>
    dplyr::mutate(
      col_type = "filter"
    ) |>
    dplyr::select(
      sqid = item_id,
      label = item_label,
      col_type
    )
  indicator_lookup <- meta$indicators |>
    dplyr::mutate(
      col_type = "indicator"
    ) |>
    dplyr::select(
      sqid = col_id,
      label,
      col_type
    )
  sqid_lookup <- bind_rows(
    filter_lookup,
    indicator_lookup
  )
}

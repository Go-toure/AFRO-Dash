# scope_analysis_optimized.R

# Local text normalization + district-name disambiguation helpers (kept
# file-local, not relying on any app.R global, matching the codebase's
# convention of always passing shared objects as explicit parameters).
.scope_normalize_key_text <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[‘’‚′]", "'", x)
  x <- gsub("[“”„″]", '"', x)
  # Treat hyphens the same as underscores -- province names are recorded
  # inconsistently across the shapefile and the survey data (e.g. data-side
  # "CUANZA-NORTE" vs shapefile "CUANZA NORTE").
  x <- gsub("[_-]", " ", x)
  # Drop apostrophes entirely (not just curly->straight) -- they're recorded
  # inconsistently too (shapefile "N'DJAMENA" vs data "NDJAMENA").
  x <- gsub("'", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  # Strip Senegal's "RM " (Region Medicale) health-administrative prefix
  # that the survey data prepends to some province names (e.g. data-side
  # "RM SAINT LOUIS" vs shapefile "SAINT LOUIS", "RM MATAM" vs "MATAM").
  # Confirmed as a systematic naming convention -- not a per-district guess.
  x <- sub("^(?i)RM\\s+", "", x, perl = TRUE)
  toupper(x)
}

.scope_build_match_key <- function(country, district, province, ambiguous_pairs) {
  country_n  <- .scope_normalize_key_text(country)
  district_n <- .scope_normalize_key_text(district)
  province_n <- .scope_normalize_key_text(province)
  pair <- paste(country_n, district_n, sep = "||")
  ifelse(pair %in% ambiguous_pairs, paste(pair, province_n, sep = "||"), pair)
}

# Safe singleton fallback for the disambiguation join: some district names
# need a province match to pick the right polygon (see .match_key above),
# but the survey data's province spelling sometimes doesn't match the
# shapefile's at all (typos, old vs new transliterations, abbreviations --
# e.g. data "CUBANGO" vs shapefile "CUANDO CUBANGO"). No normalization rule
# can chase every such variant safely. Instead: after the exact match_key
# join, if EXACTLY ONE shapefile polygon and EXACTLY ONE distinct orphaned
# data spelling remain unmatched for a given district name, they must refer
# to the same real place -- repoint those rows' .match_key so the existing
# join picks them up. If more than one candidate remains on either side,
# leave it alone (stays unmatched, as before) rather than risk mixing two
# real districts' data together.
.scope_apply_singleton_fallback <- function(district_layer, data, ambiguous_pairs) {
  if (length(ambiguous_pairs) == 0 || is.null(data) || nrow(data) == 0) return(data)

  shp_keys <- district_layer
  if (inherits(shp_keys, "sf")) shp_keys <- sf::st_drop_geometry(shp_keys)
  shp_keys <- unique(shp_keys[shp_keys$.pair %in% ambiguous_pairs, c(".pair", ".match_key"), drop = FALSE])
  if (nrow(shp_keys) == 0) return(data)

  unmatched_shapes <- shp_keys[!(shp_keys$.match_key %in% data$.match_key), , drop = FALSE]
  if (nrow(unmatched_shapes) == 0) return(data)

  is_ambiguous_row <- data$.pair %in% ambiguous_pairs
  is_unmatched_row <- !(data$.match_key %in% shp_keys$.match_key)
  orphan_rows <- unique(data[is_ambiguous_row & is_unmatched_row, c(".pair", ".match_key"), drop = FALSE])
  if (nrow(orphan_rows) == 0) return(data)

  shape_counts  <- table(unmatched_shapes$.pair)
  orphan_counts <- table(orphan_rows$.pair)
  safe_pairs <- intersect(names(shape_counts[shape_counts == 1]), names(orphan_counts[orphan_counts == 1]))
  if (length(safe_pairs) == 0) return(data)

  target_key <- setNames(unmatched_shapes$.match_key, unmatched_shapes$.pair)[safe_pairs]
  redirect <- is_ambiguous_row & is_unmatched_row & (data$.pair %in% safe_pairs)
  if (any(redirect)) {
    data$.match_key[redirect] <- unname(target_key[data$.pair[redirect]])
  }
  data
}

generate_scope_plots_app <- function(scope_data,
                                     x_months = NULL,
                                     year_selection = NULL,
                                     block_selection = NULL,
                                     country_selection = NULL,
                                     province_selection = NULL,
                                     district_selection = NULL,
                                     vaccine_selection = "All",
                                     # Pass required objects as parameters
                                     afro_blocks = NULL,
                                     ist_blocks = NULL,  # ADDED: IST blocks parameter
                                     all_countries = NULL,
                                     all_provinces = NULL,
                                     all_districts = NULL,
                                     block_type = "afro",  # ADDED: block_type parameter
                                     ambiguous_district_pairs = NULL) {
  
  # Check required packages
  required_packages <- c("dplyr", "sf", "lubridate", "ggplot2", "patchwork")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }
  
  # Set options
  sf::sf_use_s2(FALSE)
  
  # Create empty result structure
  empty_result <- list(
    plot = NULL, 
    month_plots = list(), 
    filtered_scope = data.frame(),
    data_summary = list(
      summary_by_country = data.frame(),
      summary_by_vaccine = data.frame(),
      summary_by_month = data.frame(),
      overall_summary = data.frame()
    )
  )
  
  # Check for required objects
  required_objects <- list(all_countries = all_countries, 
                           all_provinces = all_provinces, 
                           all_districts = all_districts)
  
  missing_objects <- names(required_objects)[sapply(required_objects, is.null)]
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }
  
  # MODIFIED: Select appropriate block definition based on block_type
  if (block_type == "afro") {
    block_definition <- afro_blocks
  } else {
    block_definition <- ist_blocks
  }
  
  # Check if block_definition is available
  if (is.null(block_definition)) {
    stop("Block definition not available for type: ", block_type)
  }
  
  # ---- 0) Standardize columns ----
  if ("Vaccine_type" %in% names(scope_data) && !"vaccine.type" %in% names(scope_data)) {
    scope_data$vaccine.type <- scope_data$Vaccine_type
  }
  
  # ---- 1) Enhanced Geographic filtering ----
  filtered_data <- scope_data
  
  if (!is.null(block_selection) && block_selection != "All") {
    # MODIFIED: Use selected block definition
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data[filtered_data$country %in% block_countries, , drop = FALSE]
  } else {
    block_countries <- unlist(block_definition, use.names = FALSE)
  }
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$country %in% country_selection, , drop = FALSE]
    block_countries <- country_selection
  }
  
  # Apply province filtering
  if (!is.null(province_selection) && length(province_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$province %in% province_selection, , drop = FALSE]
  }
  
  # Apply district filtering
  if (!is.null(district_selection) && length(district_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$district %in% district_selection, , drop = FALSE]
  }
  
  if (!is.null(vaccine_selection) && vaccine_selection != "All" && "vaccine.type" %in% names(filtered_data)) {
    filtered_data <- filtered_data[filtered_data$vaccine.type == vaccine_selection, , drop = FALSE]
  }
  
  show_africa_bg <- is.null(block_selection) || identical(block_selection, "All")
  
  # ---- 2) Time filtering ----
  if (!"round_start_date" %in% names(filtered_data)) {
    stop("Column 'round_start_date' not found in scope data.")
  }
  
  filtered_data$round_start_date <- as.Date(filtered_data$round_start_date)
  filtered_data$yearmonth <- lubridate::floor_date(filtered_data$round_start_date, "month")
  filtered_data$year <- lubridate::year(filtered_data$round_start_date)
  
  if (!is.null(x_months) && !is.null(year_selection)) {
    max_date <- suppressWarnings(max(filtered_data$yearmonth[filtered_data$year == year_selection], na.rm = TRUE))
    if (!is.finite(max_date)) return(empty_result)
    valid_dates <- seq(max_date, by = "-1 month", length.out = x_months)
    filtered_data <- filtered_data[filtered_data$year == year_selection & filtered_data$yearmonth %in% valid_dates, , drop = FALSE]
  } else if (!is.null(x_months)) {
    max_date <- suppressWarnings(max(filtered_data$yearmonth, na.rm = TRUE))
    if (!is.finite(max_date)) return(empty_result)
    valid_dates <- seq(max_date, by = "-1 month", length.out = x_months)
    filtered_data <- filtered_data[filtered_data$yearmonth %in% valid_dates, , drop = FALSE]
  } else if (!is.null(year_selection)) {
    filtered_data <- filtered_data[filtered_data$year == year_selection, , drop = FALSE]
  }
  
  if (nrow(filtered_data) == 0) {
    warning("No data available for the selected filters.")
    return(empty_result)
  }
  
  if ("vaccine.type" %in% names(filtered_data)) {
    filtered_data$vaccine.type <- ifelse(is.na(filtered_data$vaccine.type), "(missing)", as.character(filtered_data$vaccine.type))
    filtered_data$vaccine.type <- factor(filtered_data$vaccine.type)
  }
  
  # ---- 3) Shapefiles ----
  country_layer  <- all_countries[all_countries$ADM0_NAME %in% block_countries, ]
  province_layer <- all_provinces[all_provinces$ADM0_NAME %in% block_countries, ]
  district_layer <- all_districts[all_districts$ADM0_NAME %in% block_countries, ]

  # Disambiguated join key -- uses the file-local .scope_build_match_key()
  # helper with ambiguous_district_pairs passed in explicitly from app.R.
  # Only affects the minority of district names that recur in more than one
  # province within the same country; every other district joins exactly as
  # before.
  if (is.null(ambiguous_district_pairs)) ambiguous_district_pairs <- character(0)
  district_layer$.pair <- paste(
    .scope_normalize_key_text(district_layer$ADM0_NAME),
    .scope_normalize_key_text(district_layer$ADM2_NAME),
    sep = "||"
  )
  district_layer$.match_key <- .scope_build_match_key(
    district_layer$ADM0_NAME, district_layer$ADM2_NAME,
    if ("ADM1_NAME" %in% names(district_layer)) district_layer$ADM1_NAME else NA_character_,
    ambiguous_district_pairs
  )
  filtered_data$.pair <- paste(
    .scope_normalize_key_text(filtered_data$country),
    .scope_normalize_key_text(filtered_data$district),
    sep = "||"
  )
  filtered_data$.match_key <- .scope_build_match_key(
    filtered_data$country, filtered_data$district,
    if ("province" %in% names(filtered_data)) filtered_data$province else NA_character_,
    ambiguous_district_pairs
  )
  filtered_data <- .scope_apply_singleton_fallback(district_layer, filtered_data, ambiguous_district_pairs)

  Africa <- NULL
  if ("WORLD_CONTINENTS" %in% names(all_countries)) {
    Africa <- all_countries[all_countries$WORLD_CONTINENTS == "AFRICA", ]
  }

  # ---- 4) Monthly maps ----
  unique_dates <- sort(unique(filtered_data$yearmonth))
  plots_list <- list()
  palette_vals <- c("nOPV2" = "cornflowerblue", "bOPV" = "gold", "nOPV2 & bOPV" = "greenyellow", "(missing)" = "grey80")

  # Join geometry to the survey data ONCE across every month, instead of
  # once per month inside the loop below. merge()/merge.sf() re-copies the
  # ENTIRE district_layer's geometry on every call -- looping it per month
  # meant re-duplicating every district's polygon (matched or not, since the
  # old code used all.x=TRUE) once per month, only to immediately discard
  # the unmatched (NA) rows right after. A single merge across the whole
  # filtered_data (all months at once) followed by split()-ing the result by
  # month produces the exact same per-month row set -- an inner join is
  # equivalent here since the old code's all.x=TRUE rows were always
  # filtered out by !is.na(vaccine.type) immediately afterward -- just
  # without the repeated full-geometry duplication.
  joined_all <- merge(district_layer, filtered_data, by = ".match_key")
  if ("vaccine.type" %in% names(joined_all)) {
    joined_all <- joined_all[!is.na(joined_all$vaccine.type), , drop = FALSE]
  }
  joined_by_month <- if (nrow(joined_all) > 0) split(joined_all, joined_all$yearmonth) else list()

  for (i in seq_along(unique_dates)) {
    sel_date <- unique_dates[i]
    month_data <- filtered_data[filtered_data$yearmonth == sel_date, , drop = FALSE]
    if (nrow(month_data) == 0) next

    joined_data <- joined_by_month[[as.character(sel_date)]]
    if (is.null(joined_data) || nrow(joined_data) == 0) next

    # Calculate district counts
    district_counts <- as.data.frame(table(month_data$vaccine.type))
    names(district_counts) <- c("vaccine.type", "count")
    district_counts$label <- paste0(district_counts$vaccine.type, " (", district_counts$count, ")")
    legend_labels <- setNames(district_counts$label, district_counts$vaccine.type)
    
    # Create plot
    p <- ggplot2::ggplot() +
      {if (isTRUE(show_africa_bg) && !is.null(Africa))
        ggplot2::geom_sf(data = Africa, linewidth = 0.3, color = "grey60", fill = "grey95")} +
      {if (isTRUE(show_africa_bg) && !is.null(province_layer))
        ggplot2::geom_sf(data = province_layer, color = "white", fill = NA)} +
      {if ("vaccine.type" %in% names(joined_data))
        ggplot2::geom_sf(data = joined_data, ggplot2::aes(fill = vaccine.type), color = NA)
        else ggplot2::geom_sf(data = joined_data, fill = "grey80", color = NA)} +
      ggplot2::geom_sf(data = country_layer, linewidth = 0.4, color = "black", fill = NA) +
      # Skip graticule/datum computation entirely -- coord_sf() computes
      # lon/lat gridlines via sf::st_graticule() by default even though
      # theme_void() never draws them, and that computation runs once per
      # panel. With one panel per month in the multiplot, this was a large
      # share of total render time for no visual difference at all.
      ggplot2::coord_sf(datum = NA)

    # Add fill scale if vaccine.type exists
    if ("vaccine.type" %in% names(joined_data)) {
      available_colors <- palette_vals[names(palette_vals) %in% levels(joined_data$vaccine.type)]
      p <- p + ggplot2::scale_fill_manual(
        values = available_colors,
        labels = legend_labels,
        na.translate = FALSE,
        drop = FALSE
      )
    } else {
      p <- p + ggplot2::scale_fill_identity(guide = "none")
    }
    
    p <- p +
      ggplot2::theme_void(base_size = 9) +
      ggplot2::labs(
        fill = "Vaccine Type",
        title = paste("SIA Scope –", format(sel_date, "%b %Y"))
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 3)),
        plot.margin = ggplot2::margin(2, 2, 2, 2),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 8, face = "bold", margin = ggplot2::margin(b = 2)),
        legend.text = ggplot2::element_text(size = 7),
        legend.key.height = ggplot2::unit(0.3, "cm"),
        legend.key.width = ggplot2::unit(0.15, "cm"),
        legend.box.margin = ggplot2::margin(0, 0, 0, -3)
      )
    
    plots_list[[length(plots_list) + 1]] <- p
  }
  
  if (length(plots_list) == 0) {
    return(list(
      plot = NULL, 
      month_plots = list(), 
      filtered_scope = filtered_data,
      data_summary = empty_result$data_summary
    ))
  }
  
  # Create multiplot
  num_plots <- length(plots_list)
  ncol_value <- ifelse(num_plots <= 3, num_plots,
                       ifelse(num_plots <= 6, 3,
                              ifelse(num_plots <= 12, 4, ceiling(sqrt(num_plots)))))
  nrow_value <- ceiling(num_plots / ncol_value)
  
  multiplot <- patchwork::wrap_plots(plots_list, ncol = ncol_value, nrow = nrow_value) +
    patchwork::plot_annotation(
      caption = paste("Source: PEP SIA Repository | © WHO AFRO | Production Date:", Sys.Date()),
      theme = ggplot2::theme(
        plot.caption = ggplot2::element_text(size = 9, hjust = 1, face = "italic", margin = ggplot2::margin(t = 8)),
        plot.margin = ggplot2::margin(5, 5, 5, 5)
      )
    )
  
  # ---- 5) Data Summary Generation ----
  if (nrow(filtered_data) == 0) {
    data_summary <- empty_result$data_summary
  } else {
    # Overall summary with concatenated campaign ID
    overall_summary <- data.frame(
      total_countries = length(unique(filtered_data$country)),
      total_districts = length(unique(filtered_data$district)),
      total_campaigns = filtered_data %>%
        dplyr::mutate(campaign_id = paste(country, response, vaccine.type, roundNumber, round_start_date, sep = "|")) %>%
        dplyr::distinct(campaign_id) %>%
        nrow(),
      date_range = paste(
        format(min(filtered_data$round_start_date), "%b %Y"),
        "to",
        format(max(filtered_data$round_start_date), "%b %Y")
      ),
      vaccines_used = ifelse(
        "vaccine.type" %in% names(filtered_data),
        paste(unique(filtered_data$vaccine.type), collapse = ", "),
        "Not specified"
      ),
      stringsAsFactors = FALSE
    )
    
    # Summary by country and vaccine
    summary_by_country <- filtered_data %>%
      dplyr::mutate(campaign_id = paste(country, response, vaccine.type, roundNumber, round_start_date, sep = "|")) %>%
      dplyr::distinct(campaign_id, .keep_all = TRUE) %>%
      dplyr::count(country, vaccine.type, name = "n_campaigns")
    
    summary_by_country$n_districts <- sapply(1:nrow(summary_by_country), function(i) {
      filtered_data %>%
        dplyr::filter(country == summary_by_country$country[i] & 
                        vaccine.type == summary_by_country$vaccine.type[i]) %>%
        dplyr::distinct(district) %>%
        nrow()
    })
    
    # Summary by vaccine type
    summary_by_vaccine <- filtered_data %>%
      dplyr::mutate(campaign_id = paste(country, response, vaccine.type, roundNumber, round_start_date, sep = "|")) %>%
      dplyr::distinct(campaign_id, .keep_all = TRUE) %>%
      dplyr::count(vaccine.type, name = "n_campaigns")
    
    summary_by_vaccine$n_countries <- sapply(summary_by_vaccine$vaccine.type, function(vacc) {
      filtered_data %>%
        dplyr::filter(vaccine.type == vacc) %>%
        dplyr::distinct(country) %>%
        nrow()
    })
    
    summary_by_vaccine$n_districts <- sapply(summary_by_vaccine$vaccine.type, function(vacc) {
      filtered_data %>%
        dplyr::filter(vaccine.type == vacc) %>%
        dplyr::distinct(country, district) %>%
        nrow()
    })
    
    # Summary by month
    summary_by_month <- filtered_data %>%
      dplyr::mutate(campaign_id = paste(country, response, vaccine.type, roundNumber, round_start_date, sep = "|")) %>%
      dplyr::distinct(campaign_id, .keep_all = TRUE) %>%
      dplyr::count(yearmonth, vaccine.type, name = "n_campaigns")
    
    summary_by_month$n_countries <- sapply(1:nrow(summary_by_month), function(i) {
      filtered_data %>%
        dplyr::filter(yearmonth == summary_by_month$yearmonth[i] & 
                        vaccine.type == summary_by_month$vaccine.type[i]) %>%
        dplyr::distinct(country) %>%
        nrow()
    })
    
    summary_by_month$n_districts <- sapply(1:nrow(summary_by_month), function(i) {
      filtered_data %>%
        dplyr::filter(yearmonth == summary_by_month$yearmonth[i] & 
                        vaccine.type == summary_by_month$vaccine.type[i]) %>%
        dplyr::distinct(country, district) %>%
        nrow()
    })
    
    data_summary <- list(
      summary_by_country = summary_by_country,
      summary_by_vaccine = summary_by_vaccine,
      summary_by_month = summary_by_month,
      overall_summary = overall_summary
    )
  }
  
  # Return all components
  list(
    plot = multiplot,
    month_plots = plots_list,
    filtered_scope = filtered_data,
    data_summary = data_summary
  )
}

# ============================================================
# Scope SUMMARY map -- one aggregated map for the whole selected period,
# instead of one panel per round/month. Companion function to
# generate_scope_plots_app() above; shares its geographic/time filtering
# and the same district-name disambiguation machinery.
#
# A district is shown as "in scope" if it appeared in ANY round within the
# selected period (union across rounds, same convention as the LQAS summary
# map's any-high aggregation). Its fill color reflects every distinct
# vaccine type it was covered with across those rounds -- when that varies,
# it gets its own "Mixed (varies by round)" category rather than silently
# picking one round's value, so a viewer never mistakes a multi-round
# aggregate for a single consistent vaccine choice.
#
# Also returns a per-country round-consistency check: for every country
# that had more than one round in the selected period, were the districts
# covered (the "scope") and the vaccine type the SAME across all of that
# country's rounds, or did they change round to round? Surfaced as
# `footnote_text` (ready to render, "**bold**" markers + newlines) and as
# the structured `round_consistency` data frame.
# ============================================================
generate_scope_summary_map <- function(scope_data,
                                        x_months = NULL,
                                        year_selection = NULL,
                                        block_selection = NULL,
                                        country_selection = NULL,
                                        province_selection = NULL,
                                        district_selection = NULL,
                                        vaccine_selection = "All",
                                        afro_blocks = NULL,
                                        ist_blocks = NULL,
                                        all_countries = NULL,
                                        all_provinces = NULL,
                                        all_districts = NULL,
                                        block_type = "afro",
                                        ambiguous_district_pairs = NULL) {

  required_packages <- c("dplyr", "sf", "lubridate", "ggplot2", "patchwork")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }

  sf::sf_use_s2(FALSE)

  empty_result <- list(
    map = NULL,
    map_with_footnote = NULL,
    footnote_text = "No data available for the selected filters.",
    round_consistency = data.frame(),
    filtered_scope = data.frame()
  )

  required_objects <- list(all_countries = all_countries,
                           all_provinces = all_provinces,
                           all_districts = all_districts)
  missing_objects <- names(required_objects)[sapply(required_objects, is.null)]
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }

  block_definition <- if (block_type == "afro") afro_blocks else ist_blocks
  if (is.null(block_definition)) {
    stop("Block definition not available for type: ", block_type)
  }

  # ---- 0) Standardize columns ----
  if ("Vaccine_type" %in% names(scope_data) && !"vaccine.type" %in% names(scope_data)) {
    scope_data$vaccine.type <- scope_data$Vaccine_type
  }

  # ---- 1) Geographic filtering (mirrors generate_scope_plots_app) ----
  filtered_data <- scope_data

  if (!is.null(block_selection) && block_selection != "All") {
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data[filtered_data$country %in% block_countries, , drop = FALSE]
  } else {
    block_countries <- unlist(block_definition, use.names = FALSE)
  }

  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$country %in% country_selection, , drop = FALSE]
    block_countries <- country_selection
  }

  if (!is.null(province_selection) && length(province_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$province %in% province_selection, , drop = FALSE]
  }

  if (!is.null(district_selection) && length(district_selection) > 0) {
    filtered_data <- filtered_data[filtered_data$district %in% district_selection, , drop = FALSE]
  }

  if (!is.null(vaccine_selection) && vaccine_selection != "All" && "vaccine.type" %in% names(filtered_data)) {
    filtered_data <- filtered_data[filtered_data$vaccine.type == vaccine_selection, , drop = FALSE]
  }

  show_africa_bg <- is.null(block_selection) || identical(block_selection, "All")

  # ---- 2) Time filtering (mirrors generate_scope_plots_app) ----
  if (!"round_start_date" %in% names(filtered_data)) {
    stop("Column 'round_start_date' not found in scope data.")
  }

  filtered_data$round_start_date <- as.Date(filtered_data$round_start_date)
  filtered_data$yearmonth <- lubridate::floor_date(filtered_data$round_start_date, "month")
  filtered_data$year <- lubridate::year(filtered_data$round_start_date)

  if (!is.null(x_months) && !is.null(year_selection)) {
    max_date <- suppressWarnings(max(filtered_data$yearmonth[filtered_data$year == year_selection], na.rm = TRUE))
    if (!is.finite(max_date)) return(empty_result)
    valid_dates <- seq(max_date, by = "-1 month", length.out = x_months)
    filtered_data <- filtered_data[filtered_data$year == year_selection & filtered_data$yearmonth %in% valid_dates, , drop = FALSE]
  } else if (!is.null(x_months)) {
    max_date <- suppressWarnings(max(filtered_data$yearmonth, na.rm = TRUE))
    if (!is.finite(max_date)) return(empty_result)
    valid_dates <- seq(max_date, by = "-1 month", length.out = x_months)
    filtered_data <- filtered_data[filtered_data$yearmonth %in% valid_dates, , drop = FALSE]
  } else if (!is.null(year_selection)) {
    filtered_data <- filtered_data[filtered_data$year == year_selection, , drop = FALSE]
  }

  if (nrow(filtered_data) == 0) {
    warning("No data available for the selected filters.")
    return(empty_result)
  }

  if (!"vaccine.type" %in% names(filtered_data)) filtered_data$vaccine.type <- NA_character_
  filtered_data$vaccine.type <- ifelse(is.na(filtered_data$vaccine.type), "(missing)", as.character(filtered_data$vaccine.type))

  # ---- 3) Per-country round-consistency check ----
  # A "round" = one campaign instance for a country, identified by whichever
  # of response/roundNumber/round_start_date are available (vaccine type is
  # deliberately left OUT of the round identity -- it's the thing we're
  # checking for consistency ACROSS rounds, not part of what defines one).
  round_key_extra <- intersect(c("response", "roundNumber"), names(filtered_data))
  round_key_cols <- c("country", round_key_extra, "round_start_date")

  round_signatures <- filtered_data |>
    dplyr::mutate(district = as.character(district)) |>
    dplyr::filter(!is.na(district), nzchar(district)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(round_key_cols))) |>
    dplyr::summarise(
      n_districts = dplyr::n_distinct(district),
      scope_signature = paste(sort(unique(district)), collapse = "|"),
      vaccine_signature = paste(sort(unique(vaccine.type)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(country, round_start_date)

  round_consistency <- round_signatures |>
    dplyr::group_by(country) |>
    dplyr::summarise(
      n_rounds = dplyr::n(),
      scope_consistent = dplyr::n_distinct(scope_signature) <= 1,
      vaccine_consistent = dplyr::n_distinct(vaccine_signature) <= 1,
      .groups = "drop"
    )

  build_round_detail <- function(country_name) {
    rr <- round_signatures[round_signatures$country == country_name, , drop = FALSE]
    round_label <- if ("roundNumber" %in% names(rr)) paste("Round", as.character(rr$roundNumber)) else paste("Round", seq_len(nrow(rr)))
    lines <- sprintf(
      "%s (%s): %d district(s), vaccine: %s",
      round_label,
      format(rr$round_start_date, "%b %Y"),
      rr$n_districts,
      rr$vaccine_signature
    )
    paste(lines, collapse = "; ")
  }

  round_consistency$detail <- vapply(round_consistency$country, build_round_detail, character(1))

  # ---- 4) Footnote text ----
  n_multi_round_countries <- sum(round_consistency$n_rounds > 1)

  if (n_multi_round_countries == 0) {
    footnote_text <- "Each country included in this period had a single round in scope -- no round-to-round consistency check applies."
  } else {
    flagged <- round_consistency[round_consistency$n_rounds > 1 &
                                    !(round_consistency$scope_consistent & round_consistency$vaccine_consistent), , drop = FALSE]
    consistent_multi <- round_consistency[round_consistency$n_rounds > 1 &
                                             round_consistency$scope_consistent & round_consistency$vaccine_consistent, , drop = FALSE]

    summary_line <- sprintf(
      "**Round consistency check** -- %d countr%s had more than one round in this period: %d consistent (same districts and vaccine type across all rounds), %d differ.",
      n_multi_round_countries,
      ifelse(n_multi_round_countries == 1, "y", "ies"),
      nrow(consistent_multi),
      nrow(flagged)
    )

    if (nrow(flagged) == 0) {
      footnote_text <- summary_line
    } else {
      detail_lines <- sprintf(
        "- **%s**: %s%s -- %s",
        flagged$country,
        ifelse(flagged$scope_consistent, "same districts", "DIFFERENT districts"),
        ifelse(flagged$vaccine_consistent, ", same vaccine type", ", DIFFERENT vaccine type"),
        flagged$detail
      )
      footnote_text <- paste(summary_line, paste(detail_lines, collapse = "\n"), sep = "\n\n")
    }
  }

  # ---- 5) Shapefiles + disambiguated join (identical machinery to the per-round map) ----
  country_layer  <- all_countries[all_countries$ADM0_NAME %in% block_countries, ]
  province_layer <- all_provinces[all_provinces$ADM0_NAME %in% block_countries, ]
  district_layer <- all_districts[all_districts$ADM0_NAME %in% block_countries, ]

  if (is.null(ambiguous_district_pairs)) ambiguous_district_pairs <- character(0)
  district_layer$.pair <- paste(
    .scope_normalize_key_text(district_layer$ADM0_NAME),
    .scope_normalize_key_text(district_layer$ADM2_NAME),
    sep = "||"
  )
  district_layer$.match_key <- .scope_build_match_key(
    district_layer$ADM0_NAME, district_layer$ADM2_NAME,
    if ("ADM1_NAME" %in% names(district_layer)) district_layer$ADM1_NAME else NA_character_,
    ambiguous_district_pairs
  )
  filtered_data$.pair <- paste(
    .scope_normalize_key_text(filtered_data$country),
    .scope_normalize_key_text(filtered_data$district),
    sep = "||"
  )
  filtered_data$.match_key <- .scope_build_match_key(
    filtered_data$country, filtered_data$district,
    if ("province" %in% names(filtered_data)) filtered_data$province else NA_character_,
    ambiguous_district_pairs
  )
  filtered_data <- .scope_apply_singleton_fallback(district_layer, filtered_data, ambiguous_district_pairs)

  Africa <- NULL
  if ("WORLD_CONTINENTS" %in% names(all_countries)) {
    Africa <- all_countries[all_countries$WORLD_CONTINENTS == "AFRICA", ]
  }

  # ---- 6) District-level aggregation across the WHOLE period ----
  district_summary <- filtered_data |>
    dplyr::filter(!is.na(.match_key)) |>
    dplyr::group_by(.match_key) |>
    dplyr::summarise(
      n_rounds_covering = dplyr::n_distinct(round_start_date),
      vaccine_types = paste(sort(unique(vaccine.type)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      fill_category = ifelse(grepl(",", vaccine_types, fixed = TRUE), "Mixed (varies by round)", vaccine_types)
    )

  if (nrow(district_summary) == 0) {
    warning("No mappable districts for the selected filters.")
    return(list(map = NULL, map_with_footnote = NULL, footnote_text = footnote_text, round_consistency = round_consistency, filtered_scope = filtered_data))
  }

  joined_data <- merge(district_layer, district_summary, by = ".match_key", all.x = TRUE)
  joined_data <- joined_data[!is.na(joined_data$fill_category), , drop = FALSE]

  palette_vals <- c(
    "nOPV2" = "cornflowerblue",
    "bOPV" = "gold",
    "nOPV2 & bOPV" = "greenyellow",
    "Mixed (varies by round)" = "#8e44ad",
    "(missing)" = "grey80"
  )

  fill_counts <- as.data.frame(table(district_summary$fill_category))
  names(fill_counts) <- c("fill_category", "count")
  fill_counts$label <- paste0(fill_counts$fill_category, " (", fill_counts$count, ")")
  legend_labels <- setNames(fill_counts$label, fill_counts$fill_category)

  period_label <- paste(
    format(min(filtered_data$round_start_date, na.rm = TRUE), "%b %Y"),
    "-",
    format(max(filtered_data$round_start_date, na.rm = TRUE), "%b %Y")
  )
  n_rounds_total <- nrow(round_signatures)
  n_countries_total <- dplyr::n_distinct(filtered_data$country)

  p <- ggplot2::ggplot() +
    {if (isTRUE(show_africa_bg) && !is.null(Africa))
      ggplot2::geom_sf(data = Africa, linewidth = 0.3, color = "grey60", fill = "grey95")} +
    {if (isTRUE(show_africa_bg) && !is.null(province_layer))
      ggplot2::geom_sf(data = province_layer, color = "white", fill = NA)} +
    ggplot2::geom_sf(data = joined_data, ggplot2::aes(fill = fill_category), color = NA) +
    ggplot2::geom_sf(data = country_layer, linewidth = 0.4, color = "black", fill = NA) +
    # Skip graticule/datum computation -- see note in generate_scope_plots_app().
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_manual(
      values = palette_vals[names(palette_vals) %in% unique(joined_data$fill_category)],
      labels = legend_labels,
      na.translate = FALSE,
      drop = FALSE,
      name = "Vaccine Type"
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::labs(
      title = paste("SIA Scope Summary Map –", period_label),
      subtitle = paste0(
        n_rounds_total, " round(s) aggregated across ", n_countries_total,
        " countr", ifelse(n_countries_total == 1, "y", "ies")
      ),
      caption = paste("Source: PEP SIA Repository | © WHO AFRO | Production Date:", Sys.Date())
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, colour = "grey30", margin = ggplot2::margin(b = 6)),
      plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic"),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 10, face = "bold"),
      legend.text = ggplot2::element_text(size = 9)
    )

  # ---- 7) Combined map + footnote CARD, for downloads ----
  # The on-screen tab shows the footnote as a styled amber card below the
  # plot -- a plain ggsave() of `p` alone would leave that out of the
  # downloaded PNG/PDF entirely, and plain unstyled text looks out of place
  # next to the map. Rebuild the same card look (colored panel, left accent
  # stripe, bold heading) as a second ggplot panel stacked via patchwork --
  # same toolkit as the rest of this function -- so it travels with the export.
  footnote_parts <- strsplit(footnote_text, "\n\n", fixed = TRUE)[[1]]
  footnote_header_raw <- footnote_parts[1]
  footnote_body_raw <- if (length(footnote_parts) > 1) paste(footnote_parts[-1], collapse = "\n\n") else ""

  strip_md_bold <- function(x) gsub("\\*\\*([^*]+)\\*\\*", "\\1", x)
  footnote_header <- strip_md_bold(footnote_header_raw)
  footnote_body   <- strip_md_bold(footnote_body_raw)

  header_lines <- strwrap(footnote_header, width = 130)
  body_lines   <- if (nzchar(footnote_body)) strwrap(footnote_body, width = 130) else character(0)

  max_body_lines <- 18
  if (length(body_lines) > max_body_lines) {
    body_lines <- c(
      body_lines[seq_len(max_body_lines)],
      "... (truncated -- see the in-app footnote panel for the full list)"
    )
  }

  card_bg     <- "#fffbeb"
  card_border <- "#d97706"
  card_text   <- "#374151"

  header_label <- paste(header_lines, collapse = "\n")
  body_label   <- if (length(body_lines) > 0) paste(body_lines, collapse = "\n") else NULL

  # Vertical placement inside the card's own 0-1 coordinate space: the
  # heading starts near the top; the body (if any) starts just below it,
  # spaced out according to how many lines the heading itself wraps to.
  header_y <- 0.93
  body_y   <- max(0.06, 0.93 - 0.15 * length(header_lines))

  footnote_card <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = card_bg, colour = NA) +
    ggplot2::annotate("rect", xmin = 0, xmax = 0.006, ymin = 0, ymax = 1, fill = card_border, colour = NA) +
    ggplot2::annotate("text", x = 0.02, y = header_y, label = header_label,
                       hjust = 0, vjust = 1, fontface = "bold", size = 3.3,
                       colour = card_text, family = "sans", lineheight = 1.15) +
    {if (!is.null(body_label))
      ggplot2::annotate("text", x = 0.02, y = body_y, label = body_label,
                         hjust = 0, vjust = 1, size = 2.9,
                         colour = card_text, family = "sans", lineheight = 1.25)} +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off", expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(3, 4, 3, 4))

  total_lines <- length(header_lines) + length(body_lines)
  footnote_height_ratio <- max(0.14, min(0.55, 0.03 * total_lines + 0.09))
  map_with_footnote <- patchwork::wrap_plots(
    p, footnote_card,
    ncol = 1,
    heights = c(1 - footnote_height_ratio, footnote_height_ratio)
  )

  list(
    map = p,
    map_with_footnote = map_with_footnote,
    footnote_text = footnote_text,
    round_consistency = round_consistency,
    filtered_scope = filtered_data
  )
}
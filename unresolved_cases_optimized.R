# unresolved_cases_optimized.R
# Optimized version of generate_unresolved_NC_plots_DS_app for better performance

# Local text normalization + district-name disambiguation helpers (kept
# file-local, not relying on any app.R global, matching the codebase's
# convention of always passing shared objects as explicit parameters).
.unresolved_normalize_key_text <- function(x) {
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

.unresolved_build_match_key <- function(country, district, province, ambiguous_pairs) {
  country_n  <- .unresolved_normalize_key_text(country)
  district_n <- .unresolved_normalize_key_text(district)
  province_n <- .unresolved_normalize_key_text(province)
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
.unresolved_apply_singleton_fallback <- function(district_layer, data, ambiguous_pairs) {
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

generate_unresolved_NC_plots_DS_optimized <- function(data,
                                                      x = NULL, 
                                                      y = NULL,
                                                      block_selection = NULL,
                                                      country_selection = NULL,
                                                      province_selection = NULL,
                                                      district_selection = NULL,
                                                      all_countries = NULL,
                                                      all_provinces = NULL,
                                                      all_districts = NULL,
                                                      afro_blocks = NULL,
                                                      ist_blocks = NULL,
                                                      block_type = "afro",
                                                      ambiguous_district_pairs = NULL,
                                                      selected_reason = NULL) {
  
  # --------------------------
  # Select appropriate block definition
  # --------------------------
  if (block_type == "afro") {
    block_definition <- afro_blocks
  } else {
    block_definition <- ist_blocks
  }
  
  # Early validation to prevent heavy processing
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      plots = list(),
      summary_data = tibble::tibble(message = "No data available for selected filters."),
      processed_data = tibble::tibble(),
      period_info = "No period data"
    ))
  }
  
  # Lightweight package loading
  required_packages <- c("dplyr", "sf", "lubridate", "ggplot2", "patchwork", "scales", "tibble", "purrr")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package ", pkg, " is required but not installed.")
    }
  }
  
  # ---- Country abbreviations lookup (optimized) ----
  country_abbrs_list <- list(
    c("NIGERIA"="NIE", "NIGER"="NIG", "CAMEROON"="CAE", "CHAD"="CHA", "CENTRAL AFRICAN REPUBLIC"="CAR"),
    c("ALGERIA"="ALG", "BURKINA FASO"="BFA", "MAURITANIA"="MAU", "MALI"="MAI", "GUINEA"="GUI", "GHANA"="GHA", "TOGO"="TOG", "BENIN"="BEN", "COTE D IVOIRE"="CIV", "SIERRA LEONE"="SIL", "LIBERIA"="LIB", "GUINEA-BISSAU"="GBU", "GAMBIA"="GAM", "SENEGAL"="SEN"),
    c("ANGOLA" = "ANG", "BOTSWANA" = "BWA", "BURUNDI" = "BUU", "ERITREA" = "ERI", "ESWATINI" = "SWZ", "ETHIOPIA" = "ETH", "KENYA" = "KEN", "LESOTHO" = "LES", "MADAGASCAR" = "MAD", "MALAWI" = "MAL", "MAURITIUS" = "MAS", "MOZAMBIQUE" = "MOZ", "NAMIBIA" = "NAM", "RWANDA" = "RWA", "SEYCHELLES" = "SEY", "SOUTH AFRICA" = "SOA", "SOUTH SUDAN" = "SSD", "UNITED REPUBLIC OF TANZANIA" = "TAN", "UGANDA" = "UGA", "ZAMBIA" = "ZAM", "ZIMBABWE" = "ZIM"),
    c("DEMOCRATIC REPUBLIC OF THE CONGO" = "DRC"),
    c("CONGO"="CNG", "GABON"="GBN", "EQUATORIAL GUINEA"="EQG")
  )
  
  country_abbrs <- unlist(country_abbrs_list)
  
  get_country_abbr <- function(country_name) {
    if (country_name %in% names(country_abbrs)) {
      return(country_abbrs[[country_name]])
    } else {
      return(substr(toupper(country_name), 1, 3))
    }
  }
  
  # ---- Geographic filters (optimized) ----
  block_countries <- if (!is.null(block_selection) && block_selection != "All") {
    block_definition[[block_selection]]
  } else {
    unique(data$country)
  }
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    target_countries <- intersect(block_countries, country_selection)
    if (length(target_countries) == 0) target_countries <- country_selection
    show_africa_bg <- FALSE
  } else {
    target_countries <- block_countries
    show_africa_bg <- is.null(block_selection) || block_selection == "All"
  }
  
  # Early shapefile validation
  if (is.null(all_countries) || is.null(all_provinces) || is.null(all_districts)) {
    stop("Shapefiles not loaded. Cannot generate maps.")
  }
  
  # Lightweight shapefile subsets
  country_layer <- tryCatch({
    all_countries |> dplyr::filter(ADM0_NAME %in% target_countries)
  }, error = function(e) NULL)
  
  if (is.null(country_layer) || nrow(country_layer) == 0) {
    return(list(
      plots = list(),
      summary_data = tibble::tibble(message = "No shapefile data available for selected countries."),
      processed_data = tibble::tibble(),
      period_info = "No period data"
    ))
  }
  
  province_layer <- tryCatch({
    all_provinces |> dplyr::filter(ADM0_NAME %in% target_countries)
  }, error = function(e) NULL)
  
  district_layer <- tryCatch({
    all_districts |> dplyr::filter(ADM0_NAME %in% target_countries)
  }, error = function(e) NULL)

  # Disambiguated join key -- uses the file-local .unresolved_build_match_key()
  # helper with ambiguous_district_pairs passed in explicitly from app.R. Only
  # affects the minority of district names that recur in more than one
  # province within the same country; every other district joins exactly as
  # before.
  if (is.null(ambiguous_district_pairs)) ambiguous_district_pairs <- character(0)
  if (!is.null(district_layer) && nrow(district_layer) > 0) {
    district_layer$.pair <- paste(
      .unresolved_normalize_key_text(district_layer$ADM0_NAME),
      .unresolved_normalize_key_text(district_layer$ADM2_NAME),
      sep = "||"
    )
    district_layer$.match_key <- .unresolved_build_match_key(
      district_layer$ADM0_NAME, district_layer$ADM2_NAME,
      if ("ADM1_NAME" %in% names(district_layer)) district_layer$ADM1_NAME else NA_character_,
      ambiguous_district_pairs
    )
  }
  
  # Africa background for context (lazy loading)
  Africa <- tryCatch({
    all_countries |> dplyr::filter(WORLD_CONTINENTS == "AFRICA")
  }, error = function(e) NULL)
  
  # ---- Data Preparation with Memory Optimization ----
  df <- data |>
    dplyr::filter(country %in% target_countries) |>
    dplyr::mutate(
      round_start_date = suppressWarnings(as.Date(round_start_date)),
      floor_date = suppressWarnings(lubridate::floor_date(round_start_date, "month")),
      year = lubridate::year(round_start_date)
    ) |>
    dplyr::filter(!is.na(floor_date)) |>
    # Select only necessary columns to reduce memory
    dplyr::select(country, province, district, round_start_date, floor_date, year,
                  dplyr::starts_with("r_"), other_r)

  df$.pair <- paste(
    .unresolved_normalize_key_text(df$country),
    .unresolved_normalize_key_text(df$district),
    sep = "||"
  )
  df$.match_key <- .unresolved_build_match_key(df$country, df$district, df$province, ambiguous_district_pairs)
  if (!is.null(district_layer) && nrow(district_layer) > 0) {
    df <- .unresolved_apply_singleton_fallback(district_layer, df, ambiguous_district_pairs)
  }

  # ---- Apply Province and District Filters ----
  if (!is.null(province_selection) && length(province_selection) > 0) {
    df <- df |> dplyr::filter(province %in% province_selection)
  }
  
  if (!is.null(district_selection) && length(district_selection) > 0) {
    df <- df |> dplyr::filter(district %in% district_selection)
  }
  
  if (nrow(df) == 0) {
    return(list(
      plots = list(),
      summary_data = tibble::tibble(message = "No data available for the selected filters."),
      processed_data = tibble::tibble(),
      period_info = "No period data"
    ))
  }
  
  # Handle temporal filtering with robust error handling
  if (!is.null(x) && is.numeric(x) && x > 0) {
    max_date <- max(df$floor_date, na.rm = TRUE)
    
    if (!is.na(max_date) && !is.infinite(max_date)) {
      cutoff_date <- max_date %m-% months(x - 1)
      df <- df |> dplyr::filter(floor_date >= cutoff_date)
      period_text <- paste("Last", x, "months")
    } else {
      period_text <- "All available data"
    }
  } else if (!is.null(y) && is.numeric(y)) {
    df <- df |> dplyr::filter(year == y)
    period_text <- paste("Year", y)
  } else {
    period_text <- "All available data"
  }
  
  if (nrow(df) == 0) {
    return(list(
      plots = list(),
      summary_data = tibble::tibble(message = "No data available for the selected time period."),
      processed_data = tibble::tibble(),
      period_info = period_text
    ))
  }
  
  # ---- Optimized Reasons Definition ----
  reasons <- c(
    "Non compliance"   = "r_non_compliance",
    "Child absent"     = "r_childabsent",
    "House not visited"= "r_house_not_visited",
    "Child was asleep" = "r_child_was_asleep",
    "Child is visitor" = "r_child_is_a_visitor",
    "Vaccinated NFM"   = "r_vaccinated_but_not_FM",
    "Child not born"   = "r_childnotborn",
    "Security"         = "r_security",
    "Other"            = "other_r"
  )
  
  # Only keep existing reason columns
  existing_reasons <- reasons[unname(reasons) %in% names(df)]
  if (length(existing_reasons) == 0) {
    return(list(
      plots = list(),
      summary_data = tibble::tibble(message = "No valid reason columns found."),
      processed_data = df,
      period_info = period_text
    ))
  }
  
  # ---- Compute total reasons per month (optimized) ----
  df <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(total_reason_month = sum(
      dplyr::c_across(dplyr::all_of(unname(existing_reasons))), 
      na.rm = TRUE
    )) |>
    dplyr::ungroup()
  
  unique_months <- sort(unique(df$floor_date))
  plots_list <- list()

  # ---- Restrict map generation to the selected reason only ----
  # total_reason_month (above) and summary_data (below) both still use the
  # FULL existing_reasons set -- they're reason-independent (the app's
  # value boxes / export handlers read summary_data across all reasons, and
  # perc is a share of the total across all reasons). Only the expensive
  # per-reason x per-month map-building loop needs restricting: the app
  # (output$unresolved_map) only ever displays plots[[input$unresolved_reason]],
  # so building the other ~8 reasons' maps on every Analyze click was pure
  # wasted work. When selected_reason is NULL or not found, fall back to
  # building all reasons (preserves old behavior, e.g. for the slide-deck
  # generator which wants the full set).
  reasons_to_plot <- existing_reasons
  if (!is.null(selected_reason) && selected_reason %in% names(existing_reasons)) {
    reasons_to_plot <- existing_reasons[selected_reason]
  }

  # ---- Generate maps per reason (optimized) ----
  for (reason_name in names(reasons_to_plot)) {
    reason_col <- reasons_to_plot[[reason_name]]
    
    # Skip reasons with no non-zero values
    if (sum(df[[reason_col]], na.rm = TRUE) == 0) next
    
    month_plots <- list()
    
    for (selected_month in unique_months) {
      sel_month_date <- tryCatch(as.Date(selected_month), error = function(e) NA)
      if (is.na(sel_month_date)) next
      
      # Optimized data filtering
      sub_data <- df %>%
        dplyr::filter(floor_date == sel_month_date) %>%
        dplyr::mutate(perc = (!!rlang::sym(reason_col) / total_reason_month) * 100) %>%
        dplyr::filter(!is.na(perc) & perc > 0)
      
      # Skip months with no valid data
      if (nrow(sub_data) == 0) next
      
      # Optimized shapefile joining
      joined <- suppressWarnings(
        dplyr::left_join(
          district_layer,
          sub_data,
          by = ".match_key"
        )
      ) %>%
        dplyr::filter(!is.na(perc) & perc > 0)
      
      if (nrow(joined) == 0) next
      
      # Optimized footnote calculation
      note <- sub_data %>%
        dplyr::group_by(country) %>%
        dplyr::summarise(
          total_ds = dplyr::n_distinct(district),
          high_ds = sum(perc >= 5, na.rm = TRUE),
          prop_high = round((high_ds / total_ds) * 100, 1),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          country_abbr = purrr::map_chr(country, get_country_abbr),
          note_text = paste0(country_abbr, ":", prop_high, "%")
        ) %>%
        dplyr::pull(note_text)
      
      # Wrap footnote text to avoid overcrowding
      note_text <- if (length(note) > 0) {
        if (length(note) > 6) {
          wrapped_note <- paste("≥5% high-districts:", 
                                paste(note[1:6], collapse = ", "), 
                                "...")
        } else {
          wrapped_note <- paste("≥5% high-districts:", paste(note, collapse = ", "))
        }
        if (nchar(wrapped_note) > 80) {
          paste(strwrap(wrapped_note, width = 80), collapse = "\n")
        } else {
          wrapped_note
        }
      } else {
        "No high-districts ≥5%"
      }
      
      safe_month <- lubridate::month(sel_month_date)
      safe_year  <- lubridate::year(sel_month_date)
      month_label <- if (!is.na(safe_month) && safe_month %in% 1:12) month.abb[safe_month] else "Unknown"
      
      # Create optimized plot
      p <- tryCatch({
        ggplot2::ggplot() +
          # Africa background (only if showing full Africa)
          {if (isTRUE(show_africa_bg) && !is.null(Africa)) 
            ggplot2::geom_sf(data = Africa, linewidth = 0.3, color = "grey50", fill = "grey90")} +
          
          # Province and district layers
          {if (isTRUE(show_africa_bg) && !is.null(province_layer)) 
            ggplot2::geom_sf(data = province_layer, color = "white", fill = NA, linewidth = 0.2)} +
          
          # Main data
          ggplot2::geom_sf(data = joined, ggplot2::aes(fill = perc), color = NA) +
          
          # Country borders
          ggplot2::geom_sf(data = country_layer, color = "black", linewidth = 0.5, fill = NA) +

          # Skip graticule/datum computation entirely -- coord_sf() computes
          # lon/lat gridlines via sf::st_graticule() by default even though
          # theme_void() never draws them, and that computation runs once per
          # panel. With up to ~12 month-panels per reason this was a large
          # share of total render time for no visual difference at all.
          ggplot2::coord_sf(datum = NA) +

          # Color gradient with common legend
          ggplot2::scale_fill_gradientn(
            colors = c("green3", "yellow", "red"),
            values = scales::rescale(c(0, 5, 10, 100)),
            name = "Percentage\nof Cases\n(%)",
            labels = scales::label_percent(scale = 1),
            limits = c(0, 100),
            breaks = c(0, 25, 50, 75, 100)
          ) +
          
          # Optimized theme for multi-plot layout
          ggplot2::theme_void(base_size = 9) +
          ggplot2::labs(
            title = paste(reason_name, "–", month_label, safe_year),
            caption = note_text
          ) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 10, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 3)),
            plot.margin = ggplot2::margin(2, 2, 2, 2),
            plot.caption = ggplot2::element_text(size = 7, face = "italic", hjust = 0.5, 
                                                 margin = ggplot2::margin(t = 3), lineheight = 0.8),
            legend.position = "right",
            legend.title = ggplot2::element_text(size = 8, face = "bold", margin = ggplot2::margin(b = 2)),
            legend.text = ggplot2::element_text(size = 7, margin = ggplot2::margin(0, 0, 0, 0)),
            legend.key.height = ggplot2::unit(0.4, "cm"),
            legend.key.width = ggplot2::unit(0.2, "cm"),
            legend.margin = ggplot2::margin(0, 0, 0, 0),
            legend.box.margin = ggplot2::margin(0, 0, 0, -3),
            legend.spacing = ggplot2::unit(0.1, "cm")
          )
      }, error = function(e) {
        warning("Plot creation failed for ", reason_name, " - ", month_label, " ", safe_year, ": ", e$message)
        NULL
      })
      
      if (!is.null(p)) {
        month_plots[[length(month_plots) + 1]] <- p
      }
    }
    
    # Only add reason if at least one month produced a valid map
    if (length(month_plots) > 0) {
      # OPTIMAL LAYOUT
      num_plots <- length(month_plots)
      
      ncol_value <- ifelse(
        num_plots <= 3, num_plots,
        ifelse(
          num_plots <= 6, 3,
          ifelse(
            num_plots <= 9, 4,
            ifelse(
              num_plots <= 12, 4,
              ceiling(sqrt(num_plots))
            )
          )
        )
      )
      
      nrow_value <- ceiling(num_plots / ncol_value)
      
      # ADDED: Block type indicator in title
      block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
      
      combined_plot <- tryCatch({
        patchwork::wrap_plots(month_plots, ncol = ncol_value, nrow = nrow_value) +
          patchwork::plot_annotation(
            title = paste("Unresolved", reason_name, "Cases —", block_type_text, "—", period_text),
            caption = paste("Source: PEP SIA Repository | © WHO AFRO | Production Date:", Sys.Date())
          ) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              size = 12,
              hjust = 0.5,
              face = "bold",
              margin = ggplot2::margin(b = 10)
            ),
            plot.caption = ggplot2::element_text(
              size = 9,
              hjust = 1,
              face = "italic",
              margin = ggplot2::margin(t = 8)
            ),
            plot.margin = ggplot2::margin(5, 5, 5, 5)
          )
      }, error = function(e) {
        warning("Plot combination failed for ", reason_name, ": ", e$message)
        NULL
      })
      
      if (!is.null(combined_plot)) {
        plots_list[[reason_name]] <- combined_plot
      }
    }
  }
  
  # ---- Optimized Summary Data ----
  summary_data <- tryCatch({
    df %>%
      dplyr::group_by(country) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(unname(existing_reasons)), 
          ~ sum(.x, na.rm = TRUE), 
          .names = "{.col}_sum"
        ),
        total_records = dplyr::n(),
        total_districts = dplyr::n_distinct(district),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        total_unresolved = rowSums(
          dplyr::across(dplyr::ends_with("_sum")), 
          na.rm = TRUE
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with("_sum"),
          ~ ifelse(total_unresolved > 0, round(.x / total_unresolved * 100, 1), 0),
          .names = "{.col}_perc"
        )
      ) %>%
      dplyr::select(-total_unresolved) %>%
      dplyr::ungroup()
  }, error = function(e) {
    warning("Summary data creation failed: ", e$message)
    tibble::tibble(message = "Summary data generation failed")
  })
  
  # Force garbage collection
  gc(verbose = FALSE)
  
  list(
    plots = plots_list, 
    summary_data = summary_data,
    processed_data = df,
    period_info = period_text
  )
}
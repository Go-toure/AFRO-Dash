# ============================================================
# lqas_summary_map_optimized.R  (MONTH-ALIGNED + BACKWARD COMPATIBLE)
#  KEEP same Performance Table helper/style.
#  FIX: Table and Map now use the SAME "mappable" universe using
#         normalized join keys (country_key, district_key).
#  Province Summary uses raw monthly "performance" counts
#    Numerator   = count(performance == "high")
#    Denominator = total count of performance records
#    within each province for selected period
#  Province LQAS Summary Map
#    built from province-level "high (%)" and binned as:
#      0-25%, 25-50%, 50-80%, 80-100%
#  Optional province labels
#  NEW: Province standardization before mapping
#    NORD KIVU -> NORD-KIVU
#    SUD KIVU  -> SUD-KIVU
# ============================================================

generate_lqas_summary_map_optimized <- function(data,
                                                x = NULL,
                                                y = NULL,
                                                block_selection = NULL,
                                                country_selection = NULL,
                                                province_selection = NULL,
                                                district_selection = NULL,
                                                high_performing_filter = NULL,
                                                low_performing_filter = NULL,
                                                show_province_labels = FALSE,
                                                all_countries = NULL,
                                                all_provinces = NULL,
                                                all_districts = NULL,
                                                afro_blocks = NULL,
                                                ist_blocks = NULL,
                                                block_type = "afro",
                                                x_months = NULL,
                                                year_selection = NULL,
                                                year_filter = NULL,
                                                month_filter = NULL,
                                                last_data_months = NULL,
                                                ambiguous_district_pairs = NULL,
                                                ...) {
  
  # -----------------------------
  # Packages
  # -----------------------------
  required_packages <- c("dplyr", "sf", "lubridate", "ggplot2", "flextable", "tidyr", "tibble", "rlang")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  
  # -----------------------------
  # Backward compatibility mapping
  # -----------------------------
  if (is.null(x_months) && !is.null(x)) x_months <- x
  if (is.null(year_selection) && !is.null(y)) year_selection <- y
  
  # -----------------------------
  # Helpers
  # -----------------------------
  clean_sel <- function(v) {
    if (is.null(v)) return(NULL)
    v <- as.character(v)
    v <- v[!is.na(v)]
    v <- trimws(v)
    v <- v[v != ""]
    v <- v[!tolower(v) %in% c("all")]
    if (length(v) == 0) return(NULL)
    v
  }
  
  norm_text <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("[\u2018\u2019\u201A\u2032]", "'", x)
    x <- gsub("[\u201C\u201D\u201E\u2033]", '"', x)
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

  # Safe singleton fallback for the disambiguation join: some district names
  # need a province match to pick the right polygon (see .match_key below),
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
  apply_singleton_fallback <- function(district_layer, data, ambiguous_pairs) {
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
  
  normalize_perf <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    x <- ifelse(is.na(x), NA_character_, x)
    x <- tolower(trimws(gsub("_", " ", x)))
    x <- gsub("\\s+", " ", x)
    dplyr::case_when(
      x == "high" ~ "high",
      x %in% c("moderate", "mod", "medium") ~ "moderate",
      x == "poor" ~ "poor",
      x %in% c("very poor", "verypoor", "v poor", "v. poor") ~ "very poor",
      TRUE ~ NA_character_
    )
  }
  
  standardize_province_names <- function(x) {
    x <- trimws(as.character(x))
    dplyr::case_when(
      x == "NORD KIVU" ~ "NORD-KIVU",
      x == "SUD KIVU"  ~ "SUD-KIVU",
      TRUE ~ x
    )
  }
  
  choose_month_perf_any_high <- function(perf_vec) {
    pv <- perf_vec[!is.na(perf_vec)]
    if (length(pv) == 0) return(NA_character_)
    if (any(pv == "high")) return("high")
    if (any(pv == "moderate")) return("moderate")
    if (any(pv == "poor")) return("poor")
    if (any(pv == "very poor")) return("very poor")
    NA_character_
  }
  
  parse_mixed_dates <- function(x,
                                orders = c(
                                  "dmy", "dmY", "d/m/Y", "d.m.Y",
                                  "ymd", "Ymd", "Y-m-d", "Y/m/d",
                                  "mdy", "mdY", "m/d/Y", "m.d.Y"
                                )) {
    if (inherits(x, "Date")) return(as.Date(x))
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) {
      if (all(is.na(x) | (x > 10000 & x < 60000))) return(as.Date(x, origin = "1899-12-30"))
      return(as.Date(x, origin = "1970-01-01"))
    }
    x_chr <- as.character(x)
    x_chr[x_chr %in% c("", "NA", "NaN", "N/A")] <- NA_character_
    parsed <- suppressWarnings(lubridate::parse_date_time(x_chr, orders = orders, quiet = TRUE))
    as.Date(parsed)
  }
  
  parse_month_filter <- function(mf) {
    if (is.null(mf)) return(NULL)
    
    if (is.numeric(mf)) {
      out <- as.integer(mf)
      out <- out[!is.na(out)]
      out <- out[out >= 1 & out <= 12]
      return(unique(out))
    }
    
    m_chr <- tolower(trimws(as.character(mf)))
    m_chr[m_chr %in% c("", "na", "n/a", "nan")] <- NA_character_
    m_chr <- m_chr[!is.na(m_chr)]
    if (length(m_chr) == 0) return(NULL)
    
    out <- integer(0)
    is_num <- grepl("^[0-9]+$", m_chr)
    if (any(is_num)) out <- c(out, as.integer(m_chr[is_num]))
    
    m2 <- m_chr[!is_num]
    if (length(m2) > 0) {
      idx_full <- match(m2, tolower(month.name))
      idx_abb  <- match(m2, tolower(month.abb))
      idx <- ifelse(!is.na(idx_full), idx_full, idx_abb)
      out <- c(out, idx[!is.na(idx)])
    }
    
    out <- out[!is.na(out)]
    out <- out[out >= 1 & out <= 12]
    unique(out)
  }
  
  compute_start_date <- function(max_date, x_months) {
    max_month <- lubridate::floor_date(max_date, "month")
    if (x_months <= 1) return(max_month)
    month_seq <- seq(from = max_month, by = "-1 month", length.out = x_months)
    min(month_seq)
  }
  
  filter_last_data_months_per_country <- function(df, x) {
    x <- as.integer(x)
    if (is.na(x) || x < 1) stop("'last_data_months' must be a positive integer (>= 1).")
    
    req_cols <- c("country", "round_start_date", "performance")
    miss <- setdiff(req_cols, names(df))
    if (length(miss) > 0) stop("Missing required columns for last_data_months: ", paste(miss, collapse = ", "))
    
    df2 <- df |>
      dplyr::mutate(
        round_start_date = as.Date(.data$round_start_date),
        yearmonth   = lubridate::floor_date(.data$round_start_date, "month"),
        country_key = norm_text(.data$country),
        perf_norm   = normalize_perf(.data$performance)
      ) |>
      dplyr::filter(!is.na(.data$round_start_date), !is.na(.data$yearmonth), !is.na(.data$country_key))
    
    keep_months <- df2 |>
      dplyr::filter(!is.na(.data$perf_norm)) |>
      dplyr::distinct(.data$country_key, .data$yearmonth) |>
      dplyr::arrange(.data$country_key, dplyr::desc(.data$yearmonth)) |>
      dplyr::group_by(.data$country_key) |>
      dplyr::slice_head(n = x) |>
      dplyr::ungroup()
    
    df2 |>
      dplyr::semi_join(keep_months, by = c("country_key", "yearmonth")) |>
      dplyr::select(-.data$country_key, -.data$perf_norm, -.data$yearmonth)
  }
  
  add_district_labels_if_single_country <- function(p, joined_sf, show_labels, district_sel = NULL) {
    if (!isTRUE(show_labels)) return(p)
    if (is.null(joined_sf) || nrow(joined_sf) == 0) return(p)
    
    lab <- joined_sf |>
      dplyr::group_by(.data$ADM0_NAME, .data$ADM1_NAME, .data$ADM2_NAME) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
    
    if (!is.null(district_sel) && length(district_sel) > 0) {
      lab <- lab |> dplyr::filter(.data$ADM2_NAME %in% district_sel)
    }
    if (nrow(lab) == 0) return(p)
    
    lab <- tryCatch(sf::st_make_valid(lab), error = function(e) lab)
    lab <- suppressWarnings(sf::st_point_on_surface(lab))
    lab <- lab |> dplyr::mutate(label = as.character(.data$ADM2_NAME))
    
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      xy <- sf::st_coordinates(sf::st_geometry(lab))
      lab_df <- sf::st_drop_geometry(lab)
      lab_df$X <- xy[, 1]; lab_df$Y <- xy[, 2]
      p + ggrepel::geom_text_repel(
        data = lab_df,
        ggplot2::aes(x = .data$X, y = .data$Y, label = .data$label),
        size = 2.2,
        max.overlaps = Inf,
        box.padding = 0.2,
        point.padding = 0.1,
        min.segment.length = 0
      )
    } else {
      p + ggplot2::geom_sf_text(
        data = lab,
        ggplot2::aes(label = .data$label),
        size = 2,
        check_overlap = FALSE
      )
    }
  }
  
  add_province_labels_to_map <- function(p, province_sf, show_labels = FALSE) {
    if (!isTRUE(show_labels)) return(p)
    if (is.null(province_sf) || nrow(province_sf) == 0) return(p)
    
    lab <- province_sf
    lab <- tryCatch(sf::st_make_valid(lab), error = function(e) lab)
    lab <- suppressWarnings(sf::st_point_on_surface(lab))
    
    if (!"province" %in% names(lab)) {
      if ("ADM1_NAME" %in% names(lab)) {
        lab$province <- as.character(lab$ADM1_NAME)
      } else {
        return(p)
      }
    }
    
    lab <- lab |>
      dplyr::mutate(label = as.character(.data$province))
    
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      xy <- sf::st_coordinates(sf::st_geometry(lab))
      lab_df <- sf::st_drop_geometry(lab)
      lab_df$X <- xy[, 1]
      lab_df$Y <- xy[, 2]
      
      p + ggrepel::geom_text_repel(
        data = lab_df,
        ggplot2::aes(x = .data$X, y = .data$Y, label = .data$label),
        size = 2.5,
        max.overlaps = Inf,
        box.padding = 0.25,
        point.padding = 0.15,
        min.segment.length = 0
      )
    } else {
      p + ggplot2::geom_sf_text(
        data = lab,
        ggplot2::aes(label = .data$label),
        size = 2.2,
        check_overlap = FALSE
      )
    }
  }
  
  create_afro_block_summary_table_optimized <- function(pd, block_selection = NULL, block_type = "afro") {
    
    pd <- pd |>
      dplyr::ungroup() |>
      dplyr::mutate(
        AFRO_block = dplyr::if_else(
          is.na(.data$AFRO_block) | !nzchar(as.character(.data$AFRO_block)),
          "UNASSIGNED",
          as.character(.data$AFRO_block)
        )
      )
    
    if (!is.null(block_selection) && block_selection != "All") {
      grouping_var <- "country"
      display_var  <- "country"
    } else {
      grouping_var <- "AFRO_block"
      display_var  <- "AFRO_block"
    }
    
    pd_dist <- pd |>
      dplyr::mutate(
        country  = trimws(as.character(.data$country)),
        district = trimws(as.character(.data$district))
      ) |>
      dplyr::filter(!is.na(.data$country), nzchar(.data$country), !is.na(.data$district), nzchar(.data$district)) |>
      dplyr::distinct(.data$country, .data$district, .data$AFRO_block, .data$High.perf.summary)
    
    summary_tbl <- pd_dist |>
      dplyr::group_by(!!rlang::sym(grouping_var), .data$High.perf.summary) |>
      dplyr::summarise(`Total districts` = dplyr::n(), .groups = "drop") |>
      dplyr::group_by(!!rlang::sym(grouping_var)) |>
      dplyr::mutate(`Proportion_%` = round(`Total districts` / sum(`Total districts`) * 100, 1)) |>
      dplyr::ungroup()
    
    wide <- summary_tbl |>
      tidyr::pivot_wider(
        names_from = .data$High.perf.summary,
        values_from = c(`Total districts`, `Proportion_%`),
        names_sep = " "
      )
    
    expected_columns <- c(
      "Total districts 0-25%", "Proportion_% 0-25%",
      "Total districts 25-50%", "Proportion_% 25-50%",
      "Total districts 50-80%", "Proportion_% 50-80%",
      "Total districts 80-100%", "Proportion_% 80-100%"
    )
    for (col in expected_columns) if (!col %in% names(wide)) wide[[col]] <- 0
    
    wide <- wide |>
      dplyr::select(
        !!rlang::sym(display_var),
        `Total districts 0-25%`, `Proportion_% 0-25%`,
        `Total districts 25-50%`, `Proportion_% 25-50%`,
        `Total districts 50-80%`, `Proportion_% 50-80%`,
        `Total districts 80-100%`, `Proportion_% 80-100%`
      )
    
    names(wide)[1] <- if (!is.null(block_selection) && block_selection != "All") "country" else "AFRO_block"
    
    block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
    table_caption <- if (!is.null(block_selection) && block_selection != "All") {
      paste0("LQAS District Performance Summary by Country (", block_selection, ") | ", block_type_text)
    } else {
      paste0("LQAS District Performance Summary by Block | ", block_type_text)
    }
    
    ft <- flextable::flextable(wide) |>
      flextable::theme_vanilla() |>
      flextable::set_caption(table_caption) |>
      flextable::bg(j = c(2,4,6,8), bg = c("red4","tomato","yellow","green4"), part = "header") |>
      flextable::color(j = c(2,4,8), color = "white", part = "header") |>
      flextable::color(j = 6, color = "black", part = "header") |>
      flextable::align(align = "center", part = "all") |>
      flextable::autofit()
    
    list(ft = ft, data = wide)
  }
  
  create_province_summary_table_optimized <- function(province_perf_df) {
    
    if (is.null(province_perf_df) || nrow(province_perf_df) == 0) {
      empty_df <- tibble::tibble(
        country = character(0),
        province = character(0),
        `high (%)` = numeric(0)
      )
      
      ft <- flextable::flextable(empty_df) |>
        flextable::theme_vanilla() |>
        flextable::set_caption("Province Summary | Proportion of 'high' performance records") |>
        flextable::autofit()
      
      return(list(ft = ft, data = empty_df))
    }
    
    prov_df <- province_perf_df |>
      dplyr::mutate(
        country     = trimws(as.character(.data$country)),
        province    = standardize_province_names(.data$province),
        performance = as.character(.data$performance)
      ) |>
      dplyr::filter(
        !is.na(.data$country), nzchar(.data$country),
        !is.na(.data$province), nzchar(.data$province),
        !is.na(.data$performance), nzchar(.data$performance)
      ) |>
      dplyr::group_by(.data$country, .data$province) |>
      dplyr::summarise(
        high_n = sum(.data$performance == "high", na.rm = TRUE),
        total_n = dplyr::n(),
        `high (%)` = round(dplyr::if_else(.data$total_n > 0,
                                          (.data$high_n / .data$total_n) * 100,
                                          0), 1),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$country, dplyr::desc(.data$`high (%)`), .data$province) |>
      dplyr::select(.data$country, .data$province, .data$`high (%)`)
    
    ft <- flextable::flextable(prov_df) |>
      flextable::theme_vanilla() |>
      flextable::set_caption("Province Summary | Proportion of 'high' performance records") |>
      flextable::bg(part = "header", bg = "#1F4E79") |>
      flextable::color(part = "header", color = "white") |>
      flextable::align(align = "center", part = "header") |>
      flextable::align(j = 3, align = "center", part = "body") |>
      flextable::autofit()
    
    list(ft = ft, data = prov_df)
  }
  
  # -----------------------------
  # Early validation
  # -----------------------------
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      map = NULL,
      province_map = NULL,
      table_flex = flextable::flextable(tibble::tibble(Note = "No data available.")),
      table_data = tibble::tibble(),
      province_summary_flex = flextable::flextable(tibble::tibble(Note = "No data available.")),
      province_summary_data = tibble::tibble(),
      filtered_data = tibble::tibble(),
      performance_data = tibble::tibble(),
      map_district_set = tibble::tibble(),
      map_province_set = tibble::tibble(),
      summary_metrics = list(total_districts = 0, always_high = 0, never_high = 0),
      period_info = "No data",
      block_type = block_type,
      unmapped_table_rows = 0
    ))
  }
  
  if (is.null(all_countries) || is.null(all_provinces) || is.null(all_districts)) {
    stop("Shapefiles not loaded. Cannot generate maps.")
  }
  
  if (!"round_start_date" %in% names(data)) stop("Column 'round_start_date' not found in data.")
  if (!"country" %in% names(data)) stop("Column 'country' not found in data.")
  if (!"district" %in% names(data)) stop("Column 'district' not found in data.")
  if (!"performance" %in% names(data)) stop("Column 'performance' not found in data.")

  # ------------------------------------------------------------
  # Standardize the AFRO block field
  # ------------------------------------------------------------
  # Some datasets contain both:
  #   - `AFRO_block` (sometimes empty), and
  #   - `afro_block` (the populated source field).
  #
  # Build one reliable internal `AFRO_block` column by taking the
  # first non-empty value across all supported column-name variants.

  block_candidates <- intersect(
    c("afro_block", "AFRO_block", "Afro_block", "afro_Block"),
    names(data)
  )

  if (length(block_candidates) > 0) {
    clean_block_value <- function(x) {
      x <- trimws(as.character(x))
      x[x == ""] <- NA_character_
      x[toupper(x) %in% c("NA", "N/A", "NULL", "NONE")] <- NA_character_
      x
    }

    block_values <- lapply(block_candidates, function(nm) {
      clean_block_value(data[[nm]])
    })

    # Prefer `afro_block`, then use the other variants only to fill gaps.
    data$AFRO_block <- Reduce(dplyr::coalesce, block_values)
  } else {
    data$AFRO_block <- NA_character_
  }

  # Final cleanup to avoid false UNASSIGNED values caused by whitespace.
  data <- data |>
    dplyr::mutate(
      AFRO_block = trimws(as.character(.data$AFRO_block)),
      AFRO_block = dplyr::na_if(.data$AFRO_block, "")
    )

  if (!"province" %in% names(data)) data$province <- NA_character_
  
  # -----------------------------
  # Block selection
  # -----------------------------
  block_definition <- if (identical(block_type, "afro")) afro_blocks else ist_blocks

  # ------------------------------------------------------------
  # Rebuild the block field from the authoritative block lists
  # ------------------------------------------------------------
  # The server passes `afro_blocks` / `ist_blocks` as named lists:
  #   list("Block name" = c("Country A", "Country B", ...))
  # This lookup is more reliable than the block column in `data`, which
  # may be empty, stale, or created during an earlier join.
  if (!is.null(block_definition) && length(block_definition) > 0) {
    block_lookup <- dplyr::bind_rows(lapply(names(block_definition), function(block_name) {
      countries <- trimws(as.character(block_definition[[block_name]]))
      countries <- countries[!is.na(countries) & nzchar(countries)]

      data.frame(
        country_key_for_block = toupper(countries),
        block_from_definition = rep(block_name, length(countries)),
        stringsAsFactors = FALSE
      )
    })) |>
      dplyr::distinct(.data$country_key_for_block, .keep_all = TRUE)

    data <- data |>
      dplyr::mutate(
        country_key_for_block = toupper(trimws(as.character(.data$country)))
      ) |>
      dplyr::left_join(block_lookup, by = "country_key_for_block") |>
      dplyr::mutate(
        # Prefer the authoritative country-to-block mapping.
        # Fall back to the source column only for countries absent from the list.
        AFRO_block = dplyr::coalesce(
          trimws(as.character(.data$block_from_definition)),
          trimws(as.character(.data$AFRO_block))
        ),
        AFRO_block = dplyr::na_if(.data$AFRO_block, "")
      ) |>
      dplyr::select(-.data$country_key_for_block, -.data$block_from_definition)
  }
  
  country_selection  <- clean_sel(country_selection)
  province_selection <- clean_sel(province_selection)
  district_selection <- clean_sel(district_selection)
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    target_countries <- country_selection
    show_africa_bg <- FALSE
  } else if (!is.null(block_selection) && block_selection != "All") {
    if (is.null(block_definition) || is.null(block_definition[[block_selection]])) {
      stop("Unknown block_selection: ", block_selection)
    }
    target_countries <- block_definition[[block_selection]]
    show_africa_bg <- FALSE
  } else {
    target_countries <- unique(as.character(data$country))
    show_africa_bg <- TRUE
  }
  
  show_district_labels <- isTRUE(length(target_countries) == 1) && !isTRUE(show_africa_bg)
  
  # Shapefile subsets
  country_layer <- tryCatch(all_countries |> dplyr::filter(.data$ADM0_NAME %in% target_countries), error = function(e) NULL)
  if (is.null(country_layer) || nrow(country_layer) == 0) stop("No shapefile data available for selected countries.")
  
  province_layer <- tryCatch(all_provinces |> dplyr::filter(.data$ADM0_NAME %in% target_countries), error = function(e) NULL)
  district_layer <- tryCatch(all_districts |> dplyr::filter(.data$ADM0_NAME %in% target_countries), error = function(e) NULL)
  
  if (!is.null(district_layer) && nrow(district_layer) > 0) {
    district_layer <- tryCatch(sf::st_make_valid(district_layer), error = function(e) district_layer)
  }
  if (!is.null(province_layer) && nrow(province_layer) > 0) {
    province_layer <- tryCatch(sf::st_make_valid(province_layer), error = function(e) province_layer)
  }
  
  AFRO_layer <- if (isTRUE(show_africa_bg)) tryCatch(all_countries |> dplyr::filter(.data$WHO_REGION == "AFRO"), error = function(e) NULL) else NULL
  Africa     <- if (isTRUE(show_africa_bg)) tryCatch(all_countries |> dplyr::filter(.data$WORLD_CONTINENTS == "AFRICA"), error = function(e) NULL) else NULL
  
  # -----------------------------
  # DATA-side geo filters
  # -----------------------------
  filtered_data <- data |>
    dplyr::filter(.data$country %in% target_countries)
  
  if (!is.null(province_selection) && length(province_selection) > 0) {
    province_selection <- standardize_province_names(province_selection)
    filtered_data <- filtered_data |> dplyr::filter(standardize_province_names(.data$province) %in% province_selection)
  }
  if (!is.null(district_selection) && length(district_selection) > 0) {
    filtered_data <- filtered_data |> dplyr::filter(.data$district %in% district_selection)
  }
  
  # -----------------------------
  # Parse + normalize + standardize province + yearmonth
  # -----------------------------
  filtered_data <- filtered_data |>
    dplyr::mutate(
      round_start_date = parse_mixed_dates(.data$round_start_date),
      performance      = normalize_perf(.data$performance),
      yearmonth        = lubridate::floor_date(.data$round_start_date, "month"),
      country          = trimws(as.character(.data$country)),
      province         = standardize_province_names(.data$province),
      district         = trimws(as.character(.data$district))
    ) |>
    dplyr::filter(!is.na(.data$round_start_date), !is.na(.data$performance), !is.na(.data$yearmonth))
  
  if (nrow(filtered_data) == 0) stop("No data left after parsing dates and normalizing performance.")
  
  # -----------------------------
  # Standardize province shapefile names before province mapping
  # -----------------------------
  if (!is.null(province_layer) && nrow(province_layer) > 0 && "ADM1_NAME" %in% names(province_layer)) {
    province_layer <- province_layer |>
      dplyr::mutate(
        ADM1_NAME = standardize_province_names(.data$ADM1_NAME)
      )
  }
  
  # -----------------------------
  # TIME FILTERING
  # -----------------------------
  title_suffix <- "All Data"
  
  if (!is.null(year_filter) || !is.null(month_filter)) {
    
    if (is.null(year_filter) || is.null(month_filter)) {
      stop("For specific Year + Month(s), both 'year_filter' and 'month_filter' must be provided.")
    }
    
    yv <- as.integer(year_filter)
    mm <- parse_month_filter(month_filter)
    if (is.na(yv) || yv < 1900 || yv > 2100) stop("'year_filter' must be a valid year.")
    if (is.null(mm) || length(mm) < 1) stop("'month_filter' must contain 1+ valid months.")
    
    filtered_data <- filtered_data |>
      dplyr::filter(
        lubridate::year(.data$round_start_date) == yv,
        lubridate::month(.data$round_start_date) %in% mm
      )
    
    title_suffix <- paste0("Year ", yv, " | Month(s): ", paste(mm, collapse = ", "))
    
  } else if (!is.null(last_data_months)) {
    
    filtered_data <- filter_last_data_months_per_country(filtered_data, last_data_months)
    title_suffix <- paste0("Last ", as.integer(last_data_months), " data month(s) per country")
    
  } else if (!is.null(x_months) && !is.null(year_selection)) {
    
    ysel <- as.integer(year_selection)
    year_data <- filtered_data |> dplyr::filter(lubridate::year(.data$round_start_date) == ysel)
    if (nrow(year_data) == 0) stop("No data for selected year.")
    
    max_date <- max(year_data$round_start_date, na.rm = TRUE)
    start_date <- compute_start_date(max_date, as.integer(x_months))
    
    filtered_data <- filtered_data |>
      dplyr::filter(
        .data$round_start_date >= start_date &
          .data$round_start_date <= max_date &
          lubridate::year(.data$round_start_date) == ysel
      )
    
    title_suffix <- paste0("Last ", as.integer(x_months), " month(s) in Year ", ysel)
    
  } else if (!is.null(x_months)) {
    
    max_date <- max(filtered_data$round_start_date, na.rm = TRUE)
    start_date <- compute_start_date(max_date, as.integer(x_months))
    filtered_data <- filtered_data |>
      dplyr::filter(.data$round_start_date >= start_date & .data$round_start_date <= max_date)
    
    title_suffix <- paste0("Last ", as.integer(x_months), " month(s)")
    
  } else if (!is.null(year_selection)) {
    
    yrs <- as.integer(year_selection)
    yrs <- yrs[!is.na(yrs)]
    if (length(yrs) > 0) {
      filtered_data <- filtered_data |>
        dplyr::filter(lubridate::year(.data$round_start_date) %in% yrs)
      title_suffix <- paste0("Year(s): ", paste(sort(unique(yrs)), collapse = ", "))
    }
  }
  
  if (nrow(filtered_data) == 0) stop("No data available after time filtering.")
  
  # ============================================================
  # MONTH LEVEL: ONE performance per district-month (ANY-HIGH wins)
  # ============================================================
  month_level <- filtered_data |>
    dplyr::group_by(.data$country, .data$province, .data$district, .data$AFRO_block, .data$yearmonth) |>
    dplyr::summarise(
      round_start_date = min(.data$round_start_date, na.rm = TRUE),
      performance      = choose_month_perf_any_high(.data$performance),
      n_raw_rows       = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$performance), !is.na(.data$yearmonth))
  
  if (nrow(month_level) == 0) stop("No month-level data available after aggregation.")
  
  # -----------------------------
  # KPI metrics
  # -----------------------------
  kpi_by_district <- month_level |>
    dplyr::group_by(.data$country, .data$district) |>
    dplyr::summarise(
      total_months = dplyr::n(),
      ever_high    = any(.data$performance == "high", na.rm = TRUE),
      always_high  = all(.data$performance == "high", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(never_high = !.data$ever_high)
  
  summary_metrics <- list(
    total_districts = dplyr::n_distinct(paste(kpi_by_district$country, kpi_by_district$district, sep = "||")),
    always_high     = sum(kpi_by_district$always_high, na.rm = TRUE),
    never_high      = sum(kpi_by_district$never_high, na.rm = TRUE)
  )
  
  # -----------------------------
  # District summary over MONTHS (High%)
  # -----------------------------
  performance_data_full <- month_level |>
    dplyr::mutate(
      AFRO_block = dplyr::if_else(
        is.na(.data$AFRO_block) | !nzchar(as.character(.data$AFRO_block)),
        "UNASSIGNED",
        as.character(.data$AFRO_block)
      ),
      country_key  = norm_text(.data$country),
      district_key = norm_text(.data$district)
    ) |>
    dplyr::group_by(.data$country, .data$province, .data$district, .data$AFRO_block, .data$country_key, .data$district_key) |>
    dplyr::summarise(
      total_LQAS = dplyr::n(),
      high_performance = sum(.data$performance == "high", na.rm = TRUE),
      high_performance_percentage = dplyr::if_else(
        .data$total_LQAS > 0,
        (.data$high_performance / .data$total_LQAS) * 100,
        0
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      province = standardize_province_names(.data$province),
      province_key = norm_text(.data$province),
      High.perf.summary = dplyr::case_when(
        .data$high_performance_percentage < 25 ~ "0-25%",
        .data$high_performance_percentage < 50 ~ "25-50%",
        .data$high_performance_percentage < 80 ~ "50-80%",
        TRUE ~ "80-100%"
      )
    )
  
  if (nrow(performance_data_full) == 0) stop("No performance data available after processing.")
  
  # -----------------------------
  # Mappable universe using NORMALIZED keys
  # -----------------------------
  shp_keys <- district_layer |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      country_key  = norm_text(.data$ADM0_NAME),
      district_key = norm_text(.data$ADM2_NAME)
    ) |>
    dplyr::distinct()
  
  unmapped_table_rows <- performance_data_full |>
    dplyr::anti_join(shp_keys, by = c("country_key", "district_key")) |>
    nrow()
  
  # -----------------------------
  # Filtered for district map
  # -----------------------------
  performance_data <- performance_data_full
  if (isTRUE(high_performing_filter)) performance_data <- performance_data |> dplyr::filter(.data$High.perf.summary == "80-100%")
  if (isTRUE(low_performing_filter))  performance_data <- performance_data |> dplyr::filter(.data$High.perf.summary == "0-25%")
  if (nrow(performance_data) == 0) stop("No data available after applying performance percentage filters.")

  # Disambiguated join key -- ambiguous_district_pairs is passed in explicitly
  # from app.R (computed once at startup by compute_ambiguous_district_pairs()).
  # Only affects the minority of district names that recur in more than one
  # province within the same country; every other district joins exactly as
  # before (country_key/district_key already normalized above).
  if (is.null(ambiguous_district_pairs)) ambiguous_district_pairs <- character(0)
  performance_data$.pair <- paste(performance_data$country_key, performance_data$district_key, sep = "||")
  performance_data$.match_key <- ifelse(
    performance_data$.pair %in% ambiguous_district_pairs,
    paste(performance_data$.pair, performance_data$province_key, sep = "||"),
    performance_data$.pair
  )

  # -----------------------------
  # District map join
  # -----------------------------
  district_layer_keyed <- district_layer |>
    dplyr::mutate(
      country_key  = norm_text(.data$ADM0_NAME),
      district_key = norm_text(.data$ADM2_NAME),
      province_key = if ("ADM1_NAME" %in% names(district_layer)) norm_text(.data$ADM1_NAME) else NA_character_
    )
  district_layer_keyed$.pair <- paste(district_layer_keyed$country_key, district_layer_keyed$district_key, sep = "||")
  district_layer_keyed$.match_key <- ifelse(
    district_layer_keyed$.pair %in% ambiguous_district_pairs,
    paste(district_layer_keyed$.pair, district_layer_keyed$province_key, sep = "||"),
    district_layer_keyed$.pair
  )

  performance_data <- apply_singleton_fallback(district_layer_keyed, performance_data, ambiguous_district_pairs)

  joined <- suppressWarnings(
    dplyr::left_join(
      district_layer_keyed,
      performance_data,
      by = ".match_key"
    )
  ) |>
    dplyr::filter(!is.na(.data$High.perf.summary))
  
  if (nrow(joined) == 0) stop("No data available for mapping after join.")
  
  # -----------------------------
  # Titles
  # -----------------------------
  block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
  title_base <- if (is.null(block_selection) || block_selection == "All") {
    paste("LQAS District Performance Summary by", block_type_text)
  } else {
    paste0("LQAS District Performance Summary by Country (", block_selection, ")")
  }
  
  map_title <- paste(title_base, "-", title_suffix)
  if (isTRUE(high_performing_filter)) map_title <- paste(map_title, "- High Performing (80-100%)")
  if (isTRUE(low_performing_filter))  map_title <- paste(map_title, "- Low Performing (0-25%)")
  
  province_map_title <- paste("Province LQAS Summary Map -", title_suffix)
  
  # -----------------------------
  # District map
  # -----------------------------
  summary_map <- if (isTRUE(show_africa_bg) && !is.null(AFRO_layer) && !is.null(Africa)) {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
      ggplot2::geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = "white") +
      ggplot2::geom_sf(data = province_layer, color = NA, fill = NA) +
      ggplot2::geom_sf(data = joined, ggplot2::aes(fill = .data$High.perf.summary), color = "grey80") +
      ggplot2::geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      ggplot2::scale_fill_manual(
        values = c("0-25%"="red4","25-50%"="tomato","50-80%"="yellow","80-100%"="green4"),
        name = "High Performance (%)", drop = FALSE
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(title = map_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
        legend.title = ggplot2::element_text(size = 10),
        legend.text  = ggplot2::element_text(size = 9)
      )
  } else {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = province_layer, color = NA, fill = NA) +
      ggplot2::geom_sf(data = joined, ggplot2::aes(fill = .data$High.perf.summary), color = "grey80") +
      ggplot2::geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      ggplot2::scale_fill_manual(
        values = c("0-25%"="red4","25-50%"="tomato","50-80%"="yellow","80-100%"="green4"),
        name = "High Performance (%)", drop = FALSE
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(title = map_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
        legend.title = ggplot2::element_text(size = 10),
        legend.text  = ggplot2::element_text(size = 9)
      )
  }
  
  summary_map <- tryCatch(
    add_district_labels_if_single_country(summary_map, joined, show_district_labels, district_selection),
    error = function(e) summary_map
  )
  
  # -----------------------------
  # Table
  # -----------------------------
  pd_for_table <- performance_data_full |>
    dplyr::semi_join(shp_keys, by = c("country_key", "district_key")) |>
    dplyr::select(-.data$country_key, -.data$district_key)
  
  blk_tbl <- create_afro_block_summary_table_optimized(
    pd = pd_for_table,
    block_selection = block_selection,
    block_type = block_type
  )
  
  # -----------------------------
  # Province summary on same mappable universe
  # -----------------------------
  mappable_month_level <- month_level |>
    dplyr::mutate(
      country_key  = norm_text(.data$country),
      district_key = norm_text(.data$district),
      province     = standardize_province_names(.data$province)
    ) |>
    dplyr::semi_join(shp_keys, by = c("country_key", "district_key")) |>
    dplyr::select(-.data$country_key, -.data$district_key)
  
  province_tbl <- create_province_summary_table_optimized(mappable_month_level)
  
  # -----------------------------
  # Province map data + join
  # -----------------------------
  province_map_data <- province_tbl$data |>
    dplyr::mutate(
      province      = standardize_province_names(.data$province),
      province_key  = norm_text(.data$province),
      country_key   = norm_text(.data$country),
      province_bin  = dplyr::case_when(
        .data$`high (%)` < 25 ~ "0-25%",
        .data$`high (%)` < 50 ~ "25-50%",
        .data$`high (%)` < 80 ~ "50-80%",
        TRUE ~ "80-100%"
      )
    )
  
  if ("ADM1_NAME" %in% names(province_layer)) {
    province_layer_keyed <- province_layer |>
      dplyr::mutate(
        ADM1_NAME    = standardize_province_names(.data$ADM1_NAME),
        province_key = norm_text(.data$ADM1_NAME),
        country_key  = norm_text(.data$ADM0_NAME),
        province     = as.character(.data$ADM1_NAME)
      )
  } else {
    stop("Column 'ADM1_NAME' not found in province shapefile.")
  }
  
  province_joined <- suppressWarnings(
    dplyr::left_join(
      province_layer_keyed,
      province_map_data,
      by = c("country_key", "province_key")
    )
  ) |>
    dplyr::filter(!is.na(.data$province_bin))
  
  province_summary_map <- if (!is.null(province_joined) && nrow(province_joined) > 0) {
    p0 <- if (isTRUE(show_africa_bg) && !is.null(AFRO_layer) && !is.null(Africa)) {
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
        ggplot2::geom_sf(data = AFRO_layer, linewidth = 0.5, color = "black", fill = "white") +
        ggplot2::geom_sf(data = province_joined, ggplot2::aes(fill = .data$province_bin), color = "grey80") +
        ggplot2::geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
        ggplot2::scale_fill_manual(
          values = c("0-25%"="red4","25-50%"="tomato","50-80%"="yellow","80-100%"="green4"),
          name = "High (%)", drop = FALSE
        ) +
        ggplot2::theme_void() +
        ggplot2::labs(title = province_map_title) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
          legend.title = ggplot2::element_text(size = 10),
          legend.text  = ggplot2::element_text(size = 9)
        )
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = province_joined, ggplot2::aes(fill = .data$province_bin), color = "grey80") +
        ggplot2::geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
        ggplot2::scale_fill_manual(
          values = c("0-25%"="red4","25-50%"="tomato","50-80%"="yellow","80-100%"="green4"),
          name = "High (%)", drop = FALSE
        ) +
        ggplot2::theme_void() +
        ggplot2::labs(title = province_map_title) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold"),
          legend.title = ggplot2::element_text(size = 10),
          legend.text  = ggplot2::element_text(size = 9)
        )
    }
    
    tryCatch(
      add_province_labels_to_map(
        p = p0,
        province_sf = province_joined,
        show_labels = isTRUE(show_province_labels)
      ),
      error = function(e) p0
    )
  } else {
    NULL
  }
  
  # -----------------------------
  # Export / map sets
  # -----------------------------
  filtered_export_data <- month_level |>
    dplyr::left_join(
      performance_data_full |>
        dplyr::select(.data$country, .data$province, .data$district, .data$High.perf.summary, .data$high_performance_percentage),
      by = c("country", "province", "district")
    ) |>
    dplyr::select(.data$country, .data$province, .data$district, .data$AFRO_block,
                  .data$yearmonth, .data$round_start_date, .data$performance,
                  .data$High.perf.summary, .data$high_performance_percentage) |>
    dplyr::arrange(.data$country, .data$province, .data$district, .data$yearmonth)
  
  map_district_set <- pd_for_table |>
    dplyr::select(.data$country, .data$province, .data$district, .data$AFRO_block,
                  .data$High.perf.summary, .data$high_performance_percentage) |>
    dplyr::distinct()
  
  map_province_set <- province_map_data |>
    dplyr::select(.data$country, .data$province, .data$`high (%)`, .data$province_bin) |>
    dplyr::arrange(.data$country, dplyr::desc(.data$`high (%)`), .data$province)
  
  list(
    map = summary_map,
    province_map = province_summary_map,
    table_flex = blk_tbl$ft,
    table_data = blk_tbl$data,
    province_summary_flex = province_tbl$ft,
    province_summary_data = province_tbl$data,
    filtered_data = filtered_export_data,
    performance_data = performance_data_full,
    map_district_set = map_district_set,
    map_province_set = map_province_set,
    summary_metrics = summary_metrics,
    period_info = paste(
      "Data period:",
      min(month_level$round_start_date, na.rm = TRUE),
      "to",
      max(month_level$round_start_date, na.rm = TRUE)
    ),
    block_type = block_type,
    unmapped_table_rows = unmapped_table_rows
  )
}
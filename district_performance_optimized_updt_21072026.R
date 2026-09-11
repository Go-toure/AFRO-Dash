# ============================================================
# district_performance_optimized.R
# FULL UPDATED VERSION + FACETED HEATMAP BY YEAR
# ============================================================

district_lqas_performance <- function(data,
                                      block_selection = NULL,
                                      country_selection = NULL,
                                      province_selection = NULL,
                                      district_selection = NULL,
                                      x_months = NULL,
                                      year_selection = NULL,
                                      year_filter = NULL,
                                      month_filter = NULL,
                                      last_data_months = NULL,
                                      never_high_performing = NULL,
                                      always_high_performing = NULL,
                                      heatmap_geo_level = "country",
                                      afro_blocks = NULL,
                                      ist_blocks = NULL,
                                      block_type = "afro",
                                      all_countries = NULL,
                                      all_provinces = NULL,
                                      all_districts = NULL) {
  
  required_packages <- c(
    "dplyr", "sf", "lubridate", "ggplot2",
    "patchwork", "flextable", "tidyr", "scales", "rlang", "ggnewscale"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }
  
  required_objects <- list(
    all_countries = all_countries,
    all_provinces = all_provinces,
    all_districts = all_districts,
    afro_blocks   = afro_blocks,
    ist_blocks    = ist_blocks
  )
  
  missing_objects <- names(required_objects)[sapply(required_objects, is.null)]
  if (length(missing_objects) > 0) {
    stop("Missing required objects: ", paste(missing_objects, collapse = ", "))
  }
  
  block_definition <- if (identical(block_type, "afro")) afro_blocks else ist_blocks
  
  norm_text <- function(x) {
    if (!is.character(x)) x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("[\u2018\u2019\u201A\u2032]", "'", x)
    x <- gsub("[\u201C\u201D\u201E\u2033]", '"', x)
    x <- gsub("_", " ", x)
    x <- gsub("\\s+", " ", x)
    toupper(trimws(x))
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
  
  parse_mixed_dates <- function(x,
                                orders = c(
                                  "dmy", "dmY", "d/m/Y", "d.m.Y",
                                  "ymd", "Ymd", "Y-m-d", "Y/m/d",
                                  "mdy", "mdY", "m/d/Y", "m.d.Y"
                                )) {
    if (inherits(x, "Date")) return(as.Date(x))
    if (inherits(x, "POSIXt")) return(as.Date(x))
    
    if (is.numeric(x)) {
      if (all(is.na(x) | (x > 10000 & x < 60000))) {
        return(as.Date(x, origin = "1899-12-30"))
      } else {
        return(as.Date(x, origin = "1970-01-01"))
      }
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
    if (is.na(x) || x < 1) {
      stop("'last_data_months' must be a positive integer.")
    }
    
    req_cols <- c("country", "round_start_date", "performance")
    miss <- setdiff(req_cols, names(df))
    if (length(miss) > 0) {
      stop("Missing required columns for last_data_months: ", paste(miss, collapse = ", "))
    }
    
    df2 <- df |>
      dplyr::mutate(
        round_start_date = as.Date(round_start_date),
        yearmonth   = lubridate::floor_date(round_start_date, "month"),
        country_key = norm_text(country),
        perf_norm   = normalize_perf(performance)
      ) |>
      dplyr::filter(!is.na(round_start_date), !is.na(yearmonth), !is.na(country_key))
    
    keep_months <- df2 |>
      dplyr::filter(!is.na(perf_norm)) |>
      dplyr::distinct(country_key, yearmonth) |>
      dplyr::arrange(country_key, dplyr::desc(yearmonth)) |>
      dplyr::group_by(country_key) |>
      dplyr::slice_head(n = x) |>
      dplyr::ungroup()
    
    df2 |>
      dplyr::semi_join(keep_months, by = c("country_key", "yearmonth")) |>
      dplyr::arrange(country_key, round_start_date) |>
      dplyr::select(-yearmonth, -perf_norm)
  }
  
  make_high_perf_heatmap <- function(df, geo_level = "country") {
    
    geo_level <- ifelse(geo_level %in% c("country", "province"), geo_level, "country")
    geo_var <- geo_level
    
    if (!geo_var %in% names(df)) {
      stop("Column '", geo_var, "' not found for heatmap.")
    }
    
    # Prepare data
    heatmap_data <- df |>
      dplyr::mutate(
        year = lubridate::year(round_start_date),
        month_num = lubridate::month(round_start_date),
        month_label = factor(month.abb[month_num], levels = month.abb),
        geo_name = .data[[geo_var]]
      ) |>
      dplyr::filter(
        !is.na(geo_name),
        !is.na(year),
        !is.na(month_num),
        !is.na(performance)
      ) |>
      dplyr::group_by(geo_name, year, month_num, month_label) |>
      dplyr::summarise(
        total_districts = dplyr::n_distinct(district_key),
        high_districts  = dplyr::n_distinct(district_key[performance == "high"]),
        prop_high = dplyr::if_else(
          total_districts > 0,
          100 * high_districts / total_districts,
          NA_real_
        ),
        .groups = "drop"
      ) |>
      dplyr::filter(!is.na(prop_high)) |>
      dplyr::mutate(
        # SOP-aligned heatmap bands
        heatmap_band = dplyr::case_when(
          prop_high >= 80 ~ "High\n80–100%",
          prop_high >= 50 & prop_high < 80 ~ "Moderate\n50–80%",
          prop_high >= 25 & prop_high < 50 ~ "Low\n25–50%",
          prop_high >= 0 & prop_high < 25 ~ "Critical\n0–25%",
          TRUE ~ NA_character_
        ),
        heatmap_band = factor(
          heatmap_band,
          levels = c(
            "Critical\n0–25%",
            "Low\n25–50%",
            "Moderate\n50–80%",
            "High\n80–100%"
          )
        ),
        # Labels
        label_value = paste0(round(prop_high, 0), "%"),
        label_sub = paste0("(", high_districts, "/", total_districts, ")"),
        # Dynamic text color for readability
        label_color = dplyr::case_when(
          prop_high >= 50 ~ "black",      # Dark text on yellow/green
          TRUE ~ "white"                   # White text on red/dark red
        )
      )
    
    if (nrow(heatmap_data) == 0) {
      stop("No heatmap data generated.")
    }
    
    # Calculate dynamic sizing parameters
    n_entities <- length(unique(heatmap_data$geo_name))
    n_years <- length(unique(heatmap_data$year))
    n_months <- length(unique(heatmap_data$month_label))
    
    # Dynamic text sizes
    if (n_entities <= 10) {
      main_text_size <- 2.8
      sub_text_size <- 2.8
      y_axis_text_size <- 11
      tile_height <- 0.88
      tile_width <- 0.92
      legend_position <- "bottom"
    } else if (n_entities <= 20) {
      main_text_size <- 2.3
      sub_text_size <- 2.2
      y_axis_text_size <- 9
      tile_height <- 0.85
      tile_width <- 0.90
      legend_position <- "bottom"
    } else if (n_entities <= 30) {
      main_text_size <- 1.9
      sub_text_size <- 1.8
      y_axis_text_size <- 8
      tile_height <- 0.82
      tile_width <- 0.88
      legend_position <- "right"
    } else if (n_entities <= 40) {
      main_text_size <- 1.5
      sub_text_size <- 1.4
      y_axis_text_size <- 7
      tile_height <- 0.80
      tile_width <- 0.85
      legend_position <- "right"
    } else {
      main_text_size <- 1.2
      sub_text_size <- 1.0
      y_axis_text_size <- 6
      tile_height <- 0.78
      tile_width <- 0.82
      legend_position <- "right"
    }
    
    
    
    
    # # Dynamic text sizes ORIGINAL
    # if (n_entities <= 10) {
    #   main_text_size <- 4.5
    #   sub_text_size <- 2.8
    #   y_axis_text_size <- 11
    #   tile_height <- 0.88
    #   tile_width <- 0.92
    #   legend_position <- "bottom"
    # } else if (n_entities <= 20) {
    #   main_text_size <- 3.5
    #   sub_text_size <- 2.2
    #   y_axis_text_size <- 9
    #   tile_height <- 0.85
    #   tile_width <- 0.90
    #   legend_position <- "bottom"
    # } else if (n_entities <= 30) {
    #   main_text_size <- 2.8
    #   sub_text_size <- 1.8
    #   y_axis_text_size <- 8
    #   tile_height <- 0.82
    #   tile_width <- 0.88
    #   legend_position <- "right"
    # } else if (n_entities <= 40) {
    #   main_text_size <- 2.2
    #   sub_text_size <- 1.4
    #   y_axis_text_size <- 7
    #   tile_height <- 0.80
    #   tile_width <- 0.85
    #   legend_position <- "right"
    # } else {
    #   main_text_size <- 1.8
    #   sub_text_size <- 1.0
    #   y_axis_text_size <- 6
    #   tile_height <- 0.78
    #   tile_width <- 0.82
    #   legend_position <- "right"
    # }
    
    # Create clean heatmap
    p <- ggplot2::ggplot(
      heatmap_data,
      ggplot2::aes(
        x = month_label,
        y = reorder(geo_name, -prop_high, FUN = mean, na.rm = TRUE),
        fill = heatmap_band
      )
    ) +
      # Main tiles
      ggplot2::geom_tile(
        color = "white",
        linewidth = ifelse(n_entities > 30, 0.6, 1.2),
        width = tile_width,
        height = tile_height
      ) +
      # Main percentage text
      ggplot2::geom_text(
        ggplot2::aes(label = label_value, color = label_color),
        size = main_text_size,
        fontface = "bold",
        show.legend = FALSE
      ) +
      # Subtitle with district counts - only if space permits
      {
        if (n_entities <= 30) {
          ggplot2::geom_text(
            ggplot2::aes(label = label_sub, color = label_color),
            size = sub_text_size,
            nudge_y = -0.35,
            show.legend = FALSE
          )
        }
      } +
      ggplot2::scale_color_identity() +
      # Facet by year
      ggplot2::facet_grid(
        . ~ year,
        scales = "free_x",
        space = "free_x"
      ) +
      # SOP-aligned color scheme
      ggplot2::scale_fill_manual(
        values = c(
          "Critical\n0–25%" = "brown4",
          "Low\n25–50%" = "red",
          "Moderate\n50–80%" = "yellow",
          "High\n80–100%" = "green4"
        ),
        drop = FALSE,
        name = "Performance Category\n(% High-Performing Districts)"
      ) +
      # Labels with WHO target in caption
      ggplot2::labs(
        title = "LQAS District Performance Monitoring Dashboard",
        subtitle = paste0(
          "Tracking High-Performing Districts Over Time | ",
          toupper(ifelse(geo_level == "province", "Province-Level Analysis", "Country-Level Analysis")),
          "\nProportion of districts achieving HIGH performance classification | ",
          "Total entities monitored: ", n_entities
        ),
        x = NULL,
        y = NULL,
        caption = paste0(
          "Note: Values show % of HIGH-performing districts (high/total districts in parentheses where space permits)\n",
          "▸ WHO Target: ≥80% High-Performing Districts\n",
          "▲▼ Trend indicators: Month-over-month changes >5 percentage points (available in underlying data)\n",
          "Source: PEP SIA Repository | World Health Organization AFRO | Generated: ", 
          format(Sys.Date(), "%d %B %Y")
        )
      ) +
      # Professional theme
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        # Background
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        
        # Title and subtitle
        plot.title = ggplot2::element_text(
          face = "bold",
          size = ifelse(n_entities > 30, 16, 20),
          hjust = 0,
          color = "#004B87",
          margin = ggplot2::margin(b = 5, t = 10)
        ),
        plot.subtitle = ggplot2::element_text(
          size = ifelse(n_entities > 30, 9, 11),
          hjust = 0,
          color = "#4A4A4A",
          margin = ggplot2::margin(b = 15),
          lineheight = 1.3
        ),
        
        # Axis text
        axis.text.x = ggplot2::element_text(
          angle = ifelse(n_months > 6, 90, 45),
          hjust = ifelse(n_months > 6, 0, 1),
          vjust = ifelse(n_months > 6, 0.5, 1),
          face = "bold",
          size = max(6, min(10, y_axis_text_size - 1)),
          color = "#333333"
        ),
        axis.text.y = ggplot2::element_text(
          size = y_axis_text_size,
          face = "bold",
          color = "#333333",
          margin = ggplot2::margin(r = 8)
        ),
        
        # Grid lines
        panel.grid = ggplot2::element_blank(),
        
        # Facet strips
        strip.text = ggplot2::element_text(
          face = "bold",
          size = ifelse(n_entities > 30, 11, 14),
          color = "white",
          margin = ggplot2::margin(t = 6, b = 6)
        ),
        strip.background = ggplot2::element_rect(
          fill = "#004B87",
          color = "#002B54",
          linewidth = 0.8
        ),
        
        # Legend
        legend.position = legend_position,
        legend.title = ggplot2::element_text(
          face = "bold",
          size = ifelse(n_entities > 30, 8, 10),
          color = "#004B87"
        ),
        legend.text = ggplot2::element_text(
          size = ifelse(n_entities > 30, 7, 9),
          lineheight = 1.1
        ),
        legend.key.width = ggplot2::unit(ifelse(n_entities > 30, 0.8, 1.2), "cm"),
        legend.key.height = ggplot2::unit(ifelse(n_entities > 30, 0.4, 0.6), "cm"),
        legend.box = ifelse(n_entities > 30, "horizontal", "vertical"),
        
        # Caption
        plot.caption = ggplot2::element_text(
          size = ifelse(n_entities > 30, 7, 8),
          hjust = 0,
          color = "#666666",
          margin = ggplot2::margin(t = 12, b = 5),
          lineheight = 1.3
        ),
        
        # Margins and spacing
        plot.margin = ggplot2::margin(10, 15, 10, 10),
        panel.spacing.x = ggplot2::unit(ifelse(n_entities > 30, 0.4, 0.8), "cm"),
        panel.spacing.y = ggplot2::unit(0.2, "cm")
      )
    
    # Print sizing info for debugging
    message("Heatmap generated with:")
    message("  - Entities: ", n_entities)
    message("  - Years: ", n_years)
    message("  - Months: ", n_months)
    message("  - Main text size: ", main_text_size)
    message("  - Y-axis text size: ", y_axis_text_size)
    message("  - Legend position: ", legend_position)
    
    return(p)
  }   
  
  filtered_data <- data
  
  if (!is.null(block_selection) && block_selection != "All") {
    if (!block_selection %in% names(block_definition)) {
      stop("Unknown block_selection: ", block_selection)
    }
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data |> dplyr::filter(country %in% block_countries)
  } else {
    block_countries <- unlist(block_definition)
  }
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data |> dplyr::filter(country %in% country_selection)
    block_countries <- country_selection
  }
  
  if (!is.null(province_selection) && length(province_selection) > 0) {
    filtered_data <- filtered_data |> dplyr::filter(province %in% province_selection)
  }
  
  if (!is.null(district_selection) && length(district_selection) > 0) {
    filtered_data <- filtered_data |> dplyr::filter(district %in% district_selection)
  }
  
  show_africa_bg  <- is.null(block_selection) || identical(block_selection, "All")
  label_districts <- !is.null(country_selection) && length(country_selection) == 1
  
  if (!"round_start_date" %in% names(filtered_data)) {
    stop("Column 'round_start_date' not found in data.")
  }
  
  filtered_data <- filtered_data |>
    dplyr::mutate(round_start_date = parse_mixed_dates(round_start_date)) |>
    dplyr::filter(!is.na(round_start_date))
  
  if (nrow(filtered_data) == 0) {
    stop("No data left after parsing 'round_start_date'.")
  }
  
  if (!is.null(year_filter) || !is.null(month_filter)) {
    
    if (is.null(year_filter) || is.null(month_filter)) {
      stop("Both 'year_filter' and 'month_filter' must be provided.")
    }
    
    y <- as.integer(year_filter)
    mm <- parse_month_filter(month_filter)
    
    if (is.na(y) || y < 1900 || y > 2100) {
      stop("'year_filter' must be a valid year.")
    }
    if (is.null(mm) || length(mm) < 1) {
      stop("'month_filter' must contain one or more valid months.")
    }
    
    filtered_data <- filtered_data |>
      dplyr::filter(
        lubridate::year(round_start_date) == y,
        lubridate::month(round_start_date) %in% mm
      )
    
  } else if (!is.null(last_data_months)) {
    
    filtered_data <- filter_last_data_months_per_country(filtered_data, last_data_months)
    
  } else if (!is.null(x_months) && !is.null(year_selection)) {
    
    ysel <- as.integer(year_selection)
    
    year_data <- filtered_data |>
      dplyr::filter(lubridate::year(round_start_date) == ysel)
    
    if (nrow(year_data) == 0) {
      stop("No data available for the selected year.")
    }
    
    max_date   <- max(year_data$round_start_date, na.rm = TRUE)
    start_date <- compute_start_date(max_date, as.integer(x_months))
    
    filtered_data <- filtered_data |>
      dplyr::filter(
        round_start_date >= start_date,
        round_start_date <= max_date,
        lubridate::year(round_start_date) == ysel
      )
    
  } else if (!is.null(x_months)) {
    
    max_date   <- max(filtered_data$round_start_date, na.rm = TRUE)
    start_date <- compute_start_date(max_date, as.integer(x_months))
    
    filtered_data <- filtered_data |>
      dplyr::filter(round_start_date >= start_date, round_start_date <= max_date)
    
  } else if (!is.null(year_selection)) {
    
    ysel <- as.integer(year_selection)
    filtered_data <- filtered_data |>
      dplyr::filter(lubridate::year(round_start_date) == ysel)
  }
  
  if (nrow(filtered_data) == 0) {
    stop("No data available for the selected filters.")
  }
  
  processed_data <- filtered_data |>
    dplyr::mutate(
      round_start_date = as.Date(round_start_date),
      performance = normalize_perf(performance),
      country_key  = norm_text(country),
      province_key = if ("province" %in% names(filtered_data)) norm_text(province) else NA_character_,
      district_key = norm_text(district)
    ) |>
    dplyr::filter(!is.na(performance), !is.na(country_key), !is.na(district_key))
  
  if (nrow(processed_data) == 0) {
    stop("No data available after normalizing fields.")
  }
  
  if (!is.null(never_high_performing) || !is.null(always_high_performing)) {
    
    perf_by_dist_tmp <- processed_data |>
      dplyr::group_by(country_key, district_key) |>
      dplyr::summarise(
        total_rounds = dplyr::n(),
        ever_high    = any(performance == "high", na.rm = TRUE),
        always_high  = all(performance == "high", na.rm = TRUE),
        .groups = "drop"
      )
    
    if (!is.null(never_high_performing) && isTRUE(never_high_performing)) {
      never_high_districts <- perf_by_dist_tmp |>
        dplyr::filter(!ever_high) |>
        dplyr::select(country_key, district_key)
      
      processed_data <- processed_data |>
        dplyr::inner_join(never_high_districts, by = c("country_key", "district_key"))
    }
    
    if (!is.null(always_high_performing) && isTRUE(always_high_performing)) {
      always_high_districts <- perf_by_dist_tmp |>
        dplyr::filter(always_high) |>
        dplyr::select(country_key, district_key)
      
      processed_data <- processed_data |>
        dplyr::inner_join(always_high_districts, by = c("country_key", "district_key"))
    }
  }
  
  if (nrow(processed_data) == 0) {
    stop("No data available after applying performance status filters.")
  }
  
  country_layer  <- all_countries |> dplyr::filter(ADM0_NAME %in% block_countries)
  province_layer <- all_provinces |> dplyr::filter(ADM0_NAME %in% block_countries)
  district_layer <- all_districts |> dplyr::filter(ADM0_NAME %in% block_countries)
  
  AFRO_layer <- tryCatch(all_countries |> dplyr::filter(WHO_REGION == "AFRO"), error = function(e) NULL)
  Africa     <- tryCatch(all_countries |> dplyr::filter(WORLD_CONTINENTS == "AFRICA"), error = function(e) NULL)
  
  processed_data <- processed_data |>
    dplyr::mutate(yearmonth = lubridate::floor_date(round_start_date, "month"))
  
  unique_months <- sort(unique(processed_data$yearmonth))
  plots_list <- list()
  
  for (sel_date in unique_months) {
    
    sel_date <- as.Date(sel_date)
    month_data <- processed_data |> dplyr::filter(yearmonth == sel_date)
    if (nrow(month_data) == 0) next
    
    plot_title <- paste("LQAS Performance –", format(sel_date, "%b %Y"))
    
    lqas_performance <- suppressWarnings(
      dplyr::left_join(
        district_layer,
        month_data,
        by = c("ADM0_NAME" = "country", "ADM2_NAME" = "district"),
        relationship = "many-to-many"
      )
    ) |>
      dplyr::filter(!is.na(performance))
    
    if (nrow(lqas_performance) == 0) next
    
    label_data <- NULL
    
    if (isTRUE(label_districts)) {
      label_data <- lqas_performance |>
        sf::st_as_sf() |>
        dplyr::group_by(ADM0_NAME, ADM2_NAME) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
      
      if (!is.null(district_selection) && length(district_selection) > 0) {
        label_data <- label_data |> dplyr::filter(ADM2_NAME %in% district_selection)
      }
      
      if (nrow(label_data) > 0) {
        label_data <- suppressWarnings(sf::st_point_on_surface(label_data))
      } else {
        label_data <- NULL
      }
    }
    
    district_counts <- lqas_performance |>
      sf::st_drop_geometry() |>
      dplyr::distinct(ADM0_NAME, ADM2_NAME, performance) |>
      dplyr::count(performance, name = "count")
    
    labels_with_counts <- district_counts |>
      dplyr::mutate(label = paste0(performance, " (", count, ")")) |>
      dplyr::pull(label, name = performance)
    
    base_map <- ggplot2::ggplot()
    
    if (isTRUE(show_africa_bg) && !is.null(AFRO_layer) && !is.null(Africa)) {
      base_map <- base_map +
        ggplot2::geom_sf(data = Africa, linewidth = 0.3, color = "grey50", fill = "grey90") +
        ggplot2::geom_sf(data = AFRO_layer, linewidth = 0.3, color = "grey50", fill = "white")
    }
    
    p <- base_map +
      ggplot2::geom_sf(data = province_layer, color = "white", fill = NA, linewidth = 0.2) +
      ggplot2::geom_sf(data = lqas_performance, ggplot2::aes(fill = performance), color = "grey80") +
      {
        if (!is.null(label_data)) {
          ggplot2::geom_sf_text(
            data = label_data,
            ggplot2::aes(label = ADM2_NAME),
            size = 2,
            check_overlap = TRUE
          )
        } else {
          NULL
        }
      } +
      ggplot2::geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
      ggplot2::scale_fill_manual(
        values = c(
          "high" = "green4",
          "moderate" = "yellow",
          "poor" = "red",
          "very poor" = "brown4"
        ),
        labels = labels_with_counts,
        na.translate = FALSE
      ) +
      ggplot2::theme_void(base_size = 9) +
      ggplot2::labs(fill = "LQAS Range", title = plot_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 10,
          face = "bold",
          hjust = 0.5,
          margin = ggplot2::margin(b = 3)
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 8, face = "bold"),
        legend.text = ggplot2::element_text(size = 7),
        legend.key.height = ggplot2::unit(0.3, "cm"),
        legend.key.width = ggplot2::unit(0.15, "cm")
      )
    
    plots_list[[length(plots_list) + 1]] <- p
  }
  
  if (length(plots_list) == 0) {
    stop("No maps generated for this period.")
  }
  
  perf_by_dist <- processed_data |>
    dplyr::group_by(country_key, district_key) |>
    dplyr::summarise(
      total_rounds = dplyr::n(),
      ever_high    = any(performance == "high", na.rm = TRUE),
      always_high  = all(performance == "high", na.rm = TRUE),
      worst_perf = dplyr::case_when(
        any(performance == "very poor") ~ "VERY POOR",
        any(performance == "poor") ~ "POOR",
        any(performance == "moderate") ~ "MODERATE",
        all(performance == "high") ~ "HIGH",
        TRUE ~ NA_character_
      ),
      .groups = "drop"
    )
  
  total_districts <- perf_by_dist |>
    dplyr::group_by(country_key) |>
    dplyr::summarise(total_districts = dplyr::n_distinct(district_key), .groups = "drop")
  
  high_performing <- perf_by_dist |>
    dplyr::filter(always_high == TRUE) |>
    dplyr::count(country_key, name = "high_performing_districts")
  
  poor_performing <- perf_by_dist |>
    dplyr::filter(ever_high == FALSE) |>
    dplyr::count(country_key, name = "poor_performing_districts")
  
  persistent_poor <- perf_by_dist |>
    dplyr::filter(ever_high == FALSE) |>
    dplyr::count(country_key, worst_perf, name = "district_count") |>
    tidyr::pivot_wider(
      names_from  = worst_perf,
      values_from = district_count,
      values_fill = 0
    )
  
  required_cols <- c("MODERATE", "POOR", "VERY POOR")
  for (col in required_cols) {
    if (!col %in% names(persistent_poor)) {
      persistent_poor[[col]] <- 0L
    }
  }
  
  country_lookup <- processed_data |>
    dplyr::distinct(country_key, country_disp = country)
  
  summary_table <- total_districts |>
    dplyr::left_join(high_performing, by = "country_key") |>
    dplyr::left_join(poor_performing, by = "country_key") |>
    dplyr::left_join(persistent_poor, by = "country_key") |>
    dplyr::left_join(country_lookup, by = "country_key") |>
    dplyr::mutate(
      high_performing_districts = dplyr::coalesce(high_performing_districts, 0L),
      poor_performing_districts = dplyr::coalesce(poor_performing_districts, 0L),
      Moderate = dplyr::coalesce(MODERATE, 0L),
      Poor = dplyr::coalesce(POOR, 0L),
      `Very Poor` = dplyr::coalesce(`VERY POOR`, 0L)
    ) |>
    dplyr::transmute(
      country = country_disp,
      total_districts,
      high_performing_districts,
      poor_performing_districts,
      Moderate,
      Poor,
      `Very Poor`,
      `Total (Persistent Poor)` = Moderate + Poor + `Very Poor`
    )
  
  summary_metrics <- list(
    total_districts = sum(summary_table$total_districts, na.rm = TRUE),
    high_performing_districts = sum(summary_table$high_performing_districts, na.rm = TRUE),
    poor_performing_districts = sum(summary_table$poor_performing_districts, na.rm = TRUE)
  )
  
  block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
  
  summary_flex <- flextable::flextable(summary_table) |>
    flextable::set_header_labels(
      country = "Country",
      total_districts = "Total Districts",
      high_performing_districts = "High Performing Districts",
      poor_performing_districts = "Poor Performing Districts",
      Moderate = "Moderate",
      Poor = "Poor",
      `Very Poor` = "Very Poor",
      `Total (Persistent Poor)` = "Total (Persistent Poor)"
    ) |>
    flextable::add_header_lines(
      values = paste("Districts with Persistent Poor Quality |", block_type_text)
    ) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "all") |>
    flextable::autofit()
  
  num_plots <- length(plots_list)
  
  ncol_value <- ifelse(
    num_plots <= 3,
    num_plots,
    ifelse(
      num_plots <= 6,
      3,
      ifelse(
        num_plots <= 9,
        4,
        ifelse(num_plots <= 12, 4, ceiling(sqrt(num_plots)))
      )
    )
  )
  
  nrow_value <- ceiling(num_plots / ncol_value)
  
  multiplot <- patchwork::wrap_plots(
    plots_list,
    ncol = ncol_value,
    nrow = nrow_value
  ) +
    patchwork::plot_annotation(
      caption = paste(
        "Source: PEP SIA Repository |",
        block_type_text,
        "| © WHO AFRO | Production Date:",
        Sys.Date()
      )
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = 9,
        hjust = 1,
        face = "italic",
        margin = ggplot2::margin(t = 8)
      ),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
  
  heatmap_plot <- make_high_perf_heatmap(
    df = processed_data,
    geo_level = heatmap_geo_level
  )
  
  list(
    multiplot       = multiplot,
    heatmap         = heatmap_plot,
    maps            = plots_list,
    summary_table   = summary_flex,
    summary_data    = summary_table,
    summary_metrics = summary_metrics,
    period_info     = paste(
      "Data period:",
      min(processed_data$round_start_date, na.rm = TRUE),
      "to",
      max(processed_data$round_start_date, na.rm = TRUE)
    ),
    filtered_data = processed_data,
    block_type = block_type,
    heatmap_geo_level = heatmap_geo_level
  )
}
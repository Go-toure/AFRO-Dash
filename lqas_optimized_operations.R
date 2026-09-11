# lqas_optimized_operations.R
# Pre-computed heavy operations for LQAS Summary Maps

library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(flextable)
library(tidyr)
library(purrr)

# ============================================================
# PRE-COMPUTED DATA PREPARATION
# ============================================================

# Pre-process and cache LQAS data
precompute_lqas_data <- function(raw_data) {
  cat("Pre-computing LQAS data...\n")
  
  # Basic data validation
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    stop("Raw data is empty or null")
  }
  
  required_cols <- c("country", "district", "province", "AFRO_block", "round_start_date", "performance")
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  lqas_data <- raw_data %>%
    mutate(
      round_start_date = as.Date(round_start_date),
      floor_date = lubridate::floor_date(round_start_date, "month"),
      year = lubridate::year(round_start_date),
      performance_category = case_when(
        performance == "high" ~ "High",
        performance == "medium" ~ "Medium", 
        performance == "low" ~ "Low",
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(!is.na(AFRO_block)) %>%
    arrange(country, district, round_start_date)
  
  # Cache unique values for filters
  unique_countries <- sort(unique(lqas_data$country))
  unique_afro_blocks <- sort(unique(lqas_data$AFRO_block))
  available_years <- sort(unique(lqas_data$year))
  
  # Pre-aggregate performance summaries
  performance_summary <- lqas_data %>%
    group_by(country, province, district, AFRO_block) %>%
    summarise(
      total_LQAS = n(),
      high_performance = sum(performance == "high", na.rm = TRUE),
      high_performance_percentage = ifelse(total_LQAS > 0, (high_performance / total_LQAS) * 100, 0),
      .groups = "drop"
    ) %>%
    mutate(
      performance_category = case_when(
        high_performance_percentage < 25 ~ "0-25%",
        high_performance_percentage < 50 ~ "25-50%",
        high_performance_percentage < 80 ~ "50-80%",
        TRUE ~ "80-100%"
      )
    )
  
  cat("Pre-computation completed:\n")
  cat("  - Countries:", length(unique_countries), "\n")
  cat("  - Districts:", nrow(performance_summary), "\n")
  cat("  - Years:", length(available_years), "\n")
  
  list(
    lqas_data = lqas_data,
    performance_summary = performance_summary,
    unique_countries = unique_countries,
    unique_afro_blocks = unique_afro_blocks,
    available_years = available_years
  )
}

# ============================================================
# OPTIMIZED FILTERING FUNCTIONS
# ============================================================

# Fast filtering based on user inputs
filter_lqas_data <- function(precomputed_data, 
                             time_type = "months", 
                             months = 6, 
                             years = NULL,
                             block_selection = "All",
                             country_selection = NULL,
                             performance_filters = character(0),
                             afro_blocks = NULL) {
  
  cat("Starting data filtering...\n")
  cat("  Time type:", time_type, "\n")
  cat("  Months:", months, "\n")
  cat("  Years:", if(!is.null(years)) paste(years, collapse=", ") else "NULL", "\n")
  cat("  Block:", block_selection, "\n")
  cat("  Countries:", if(!is.null(country_selection)) paste(country_selection, collapse=", ") else "NULL", "\n")
  cat("  Performance filters:", paste(performance_filters, collapse=", "), "\n")
  
  data <- precomputed_data$lqas_data
  perf_summary <- precomputed_data$performance_summary
  
  # Time-based filtering
  if (time_type == "months" && !is.null(months) && months > 0) {
    max_date <- max(data$floor_date, na.rm = TRUE)
    if (!is.na(max_date)) {
      valid_dates <- seq(max_date, by = "-1 month", length.out = months)
      data <- data %>% filter(floor_date %in% valid_dates)
      perf_summary <- perf_summary %>% 
        filter(country %in% unique(data$country) & district %in% unique(data$district))
    }
  } else if (time_type == "years" && !is.null(years)) {
    data <- data %>% filter(year %in% years)
    perf_summary <- perf_summary %>% 
      filter(country %in% unique(data$country) & district %in% unique(data$district))
  }
  
  # Geographic filtering
  if (block_selection != "All") {
    if (!is.null(afro_blocks) && block_selection %in% names(afro_blocks)) {
      target_countries <- afro_blocks[[block_selection]]
      data <- data %>% filter(country %in% target_countries)
      perf_summary <- perf_summary %>% filter(country %in% target_countries)
    }
  }
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    data <- data %>% filter(country %in% country_selection)
    perf_summary <- perf_summary %>% filter(country %in% country_selection)
  }
  
  # Performance filtering
  if (length(performance_filters) > 0) {
    perf_categories <- c()
    if ("high_performing" %in% performance_filters) {
      perf_categories <- c(perf_categories, "80-100%")
    }
    if ("low_performing" %in% performance_filters) {
      perf_categories <- c(perf_categories, "0-25%")
    }
    
    if (length(perf_categories) > 0) {
      perf_summary <- perf_summary %>% 
        filter(performance_category %in% perf_categories)
      
      # Filter raw data to only include districts in the performance summary
      valid_districts <- perf_summary %>% 
        distinct(country, district)
      data <- data %>% 
        inner_join(valid_districts, by = c("country", "district"))
    }
  }
  
  cat("Filtering completed:\n")
  cat("  - Data rows:", nrow(data), "\n")
  cat("  - Performance rows:", nrow(perf_summary), "\n")
  
  list(
    filtered_data = data,
    filtered_performance = perf_summary
  )
}

# ============================================================
# OPTIMIZED MAP GENERATION
# ============================================================

generate_optimized_lqas_map <- function(filtered_performance, 
                                        shapefiles,
                                        block_selection = "All",
                                        performance_filters = character(0),
                                        title_suffix = "") {
  
  cat("Generating map...\n")
  
  # Extract shapefiles
  all_countries <- shapefiles$countries
  all_provinces <- shapefiles$provinces  
  all_districts <- shapefiles$districts
  
  # Determine target countries
  target_countries <- unique(filtered_performance$country)
  
  cat("  Target countries:", length(target_countries), "\n")
  
  if (length(target_countries) == 0) {
    cat("  No target countries found\n")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available for selected filters") +
             theme_void())
  }
  
  # Prepare shapefile subsets
  country_layer <- all_countries %>% filter(ADM0_NAME %in% target_countries)
  province_layer <- all_provinces %>% filter(ADM0_NAME %in% target_countries)
  district_layer <- all_districts %>% filter(ADM0_NAME %in% target_countries)
  
  cat("  Shapefile subsets prepared\n")
  
  # Join performance data with districts
  joined_data <- tryCatch({
    district_layer %>%
      left_join(
        filtered_performance %>% 
          select(country, district, performance_category),
        by = c("ADM0_NAME" = "country", "ADM2_NAME" = "district"),
        relationship = "many-to-many"
      ) %>%
      filter(!is.na(performance_category))
  }, error = function(e) {
    cat("  Error in join:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(joined_data) || nrow(joined_data) == 0) {
    cat("  No joined data available\n")
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No matching districts found") +
             theme_void())
  }
  
  cat("  Joined data rows:", nrow(joined_data), "\n")
  
  # Dynamic title
  base_title <- if (block_selection == "All") {
    "LQAS District Performance Summary by AFRO Block (AFRO)"
  } else {
    paste("LQAS District Performance Summary by Country (", block_selection, ")")
  }
  
  # Add performance filter info to title
  full_title <- base_title
  if ("high_performing" %in% performance_filters) {
    full_title <- paste(full_title, "- High Performing (80-100%)")
  }
  if ("low_performing" %in% performance_filters) {
    full_title <- paste(full_title, "- Low Performing (0-25%)")
  }
  full_title <- paste(full_title, title_suffix)
  
  # Generate map
  cat("  Creating ggplot...\n")
  map_plot <- ggplot() +
    geom_sf(data = province_layer, color = NA, fill = NA) +
    geom_sf(data = joined_data, aes(fill = performance_category), color = "grey80") +
    geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
    scale_fill_manual(
      values = c("0-25%" = "red4", "25-50%" = "tomato", 
                 "50-80%" = "yellow", "80-100%" = "green4"),
      name = "High Performance (%)", 
      drop = FALSE
    ) +
    theme_void() +
    labs(title = full_title) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  cat("  Map generation completed\n")
  return(map_plot)
}

# ============================================================
# OPTIMIZED TABLE GENERATION
# ============================================================

generate_optimized_lqas_table <- function(filtered_performance, block_selection = "All") {
  
  cat("Generating table...\n")
  
  if (nrow(filtered_performance) == 0) {
    cat("  No performance data for table\n")
    return(list(
      ft = flextable(data_frame(Note = "No data available")),
      data = data_frame()
    ))
  }
  
  # Dynamic grouping
  grouping_var <- if (block_selection != "All") "country" else "AFRO_block"
  display_var <- grouping_var
  
  summary_tbl <- filtered_performance %>%
    group_by(!!sym(grouping_var), performance_category) %>%
    summarise(`Total districts` = n(), .groups = "drop") %>%
    group_by(!!sym(grouping_var)) %>%
    mutate(`Proportion_%` = round(`Total districts` / sum(`Total districts`) * 100, 1)) %>%
    ungroup()
  
  # Pivot to wide format
  wide <- summary_tbl %>%
    pivot_wider(
      names_from = performance_category,
      values_from = c(`Total districts`, `Proportion_%`),
      names_sep = " "
    )
  
  # Ensure all expected columns exist
  expected_columns <- c(
    "Total districts 0-25%", "Proportion_% 0-25%",
    "Total districts 25-50%", "Proportion_% 25-50%", 
    "Total districts 50-80%", "Proportion_% 50-80%",
    "Total districts 80-100%", "Proportion_% 80-100%"
  )
  
  for (col in expected_columns) {
    if (!col %in% names(wide)) wide[[col]] <- 0
  }
  
  # Select and arrange columns
  wide <- wide %>%
    select(
      !!sym(display_var),
      `Total districts 0-25%`, `Proportion_% 0-25%`,
      `Total districts 25-50%`, `Proportion_% 25-50%`,
      `Total districts 50-80%`, `Proportion_% 50-80%`,
      `Total districts 80-100%`, `Proportion_% 80-100%`
    )
  
  # Create flextable
  table_caption <- if (block_selection != "All") {
    paste0("LQAS District Performance Summary by Country (", block_selection, ")")
  } else {
    "LQAS District Performance Summary by AFRO Block (AFRO)"
  }
  
  ft <- flextable(wide) %>%
    theme_vanilla() %>%
    set_caption(table_caption) %>%
    bg(j = c(2,4,6,8), bg = c("red4","tomato","yellow","green4"), part = "header") %>%
    color(j = c(2,4,6,8), color = "white", part = "header") %>%
    align(align = "center", part = "all") %>%
    autofit()
  
  # Rename first column
  names(wide)[1] <- if (block_selection != "All") "country" else "AFRO_block"
  
  cat("  Table generation completed\n")
  list(ft = ft, data = wide)
}

# ============================================================
# MAIN OPTIMIZED FUNCTION
# ============================================================

generate_optimized_lqas_analysis <- function(precomputed_data, 
                                             shapefiles,
                                             time_type = "months",
                                             months = 6,
                                             years = NULL,
                                             block_selection = "All",
                                             country_selection = NULL,
                                             performance_filters = character(0),
                                             afro_blocks = NULL) {
  
  cat("=== STARTING LQAS ANALYSIS ===\n")
  
  # Validate inputs
  if (is.null(precomputed_data)) {
    stop("Precomputed data is null")
  }
  
  if (is.null(shapefiles$countries) || is.null(shapefiles$provinces) || is.null(shapefiles$districts)) {
    stop("Shapefiles are missing")
  }
  
  # Fast filtering
  filtered <- filter_lqas_data(
    precomputed_data,
    time_type = time_type,
    months = months,
    years = years,
    block_selection = block_selection,
    country_selection = country_selection,
    performance_filters = performance_filters,
    afro_blocks = afro_blocks
  )
  
  if (nrow(filtered$filtered_data) == 0 || nrow(filtered$filtered_performance) == 0) {
    cat("No data after filtering\n")
    return(list(
      map = NULL,
      table_flex = flextable(data_frame(Note = "No data available for selected filters")),
      table_data = data_frame(),
      filtered_data = data_frame(),
      performance_data = data_frame(),
      period_info = "No data available"
    ))
  }
  
  # Generate title suffix
  title_suffix <- if (time_type == "months") {
    paste("Last", months, "months")
  } else if (time_type == "years" && !is.null(years)) {
    paste("Year(s):", paste(years, collapse = ", "))
  } else {
    "All Data"
  }
  
  # Generate map
  lqas_map <- generate_optimized_lqas_map(
    filtered_performance = filtered$filtered_performance,
    shapefiles = shapefiles,
    block_selection = block_selection,
    performance_filters = performance_filters,
    title_suffix = title_suffix
  )
  
  # Generate table
  lqas_table <- generate_optimized_lqas_table(
    filtered_performance = filtered$filtered_performance,
    block_selection = block_selection
  )
  
  # Prepare export data
  export_data <- tryCatch({
    filtered$filtered_data %>%
      left_join(
        filtered$filtered_performance %>% 
          select(country, district, performance_category, high_performance_percentage),
        by = c("country", "district"),
        relationship = "many-to-many"
      ) %>%
      select(
        country, province, district, AFRO_block, round_start_date, 
        performance, performance_category, high_performance_percentage
      ) %>%
      arrange(country, district, round_start_date)
  }, error = function(e) {
    cat("Error creating export data:", e$message, "\n")
    return(data_frame())
  })
  
  # Period info
  period_info <- paste(
    "Data period:",
    min(filtered$filtered_data$round_start_date, na.rm = TRUE),
    "to",
    max(filtered$filtered_data$round_start_date, na.rm = TRUE)
  )
  
  cat("=== LQAS ANALYSIS COMPLETED ===\n")
  cat("  Map created:", !is.null(lqas_map), "\n")
  cat("  Table rows:", nrow(lqas_table$data), "\n")
  cat("  Export data rows:", nrow(export_data), "\n")
  
  list(
    map = lqas_map,
    table_flex = lqas_table$ft,
    table_data = lqas_table$data,
    filtered_data = export_data,
    performance_data = filtered$filtered_performance,
    period_info = period_info
  )
}
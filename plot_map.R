# Retry Function for Overpass Queries
retry_query <- function(query, max_attempts = 3) {
  attempt <- 1
  while (attempt <= max_attempts) {
    tryCatch({
      result <- osmdata_sf(query)
      message("OSM query succeeded on attempt ", attempt)
      return(result)
    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      if (attempt == max_attempts) {
        stop("Failed after ", max_attempts, " attempts: ", e$message)
      }
      Sys.sleep(5)
      attempt <<- attempt + 1
    })
  }
}

create_settlement_map <- function(
    x = 300,
    ctry,
    province,
    district,
    facility,
    Response,
    roundNumber,
    settlement,
    bbox_m = x,
    file_id
) {
  required_packages <- c("osmdata", "sf", "dplyr", "ggplot2", "stringr", 
                         "cowplot", "patchwork", "ggspatial", "ggforce", "tidyr", "readr")
  invisible(lapply(required_packages, require, character.only = TRUE))
  
  tryCatch({
    # FIXED: Simple RDS loading without CSV parameters
    im_data <- readRDS(paste0("input/", file_id, ".rds"))
    names(im_data) <- toupper(names(im_data))
    
    clean_coords <- function(x) {
      x <- gsub("[^0-9.-]", "", x)
      ifelse(x == "" | tolower(x) == "na", NA_real_, as.numeric(x))
    }
    
    im_data <- im_data %>%
      mutate(across(c(LON, LAT, LON_END, LAT_END), clean_coords),
             across(c(COUNTRY, PROVINCE, DISTRICT, FACILITY, SETTLEMENT, RESPONSE, ROUNDNUMBER),
                    ~toupper(trimws(.)))) %>%
      filter(complete.cases(LON, LAT, LON_END, LAT_END))
    
    settlement_data <- im_data %>%
      filter(
        COUNTRY == toupper(ctry),
        PROVINCE == toupper(province),
        DISTRICT == toupper(district),
        FACILITY == toupper(facility),
        SETTLEMENT == toupper(settlement),
        RESPONSE == toupper(Response),
        ROUNDNUMBER == toupper(roundNumber)
      )
    
    if (nrow(settlement_data) == 0) stop("No matching data found.")
    
    # Extract points
    departure_point <- settlement_data |> slice(1) |> select(lon = LON, lat = LAT)
    arrival_point   <- settlement_data |> slice(1) |> select(lon_end = LON_END, lat_end = LAT_END)
    
    # Format HH and reasons
    hh_columns <- colnames(settlement_data)[grepl("^HOUSEHOLD_", colnames(settlement_data), ignore.case = TRUE)]
    reason_columns <- c("REASON_ABSENT", "REASON_NC", "REASON_NOTVISITED", "REASON_ASLEEP", 
                        "REASON_VACCINATEDROUTINE", "REASON_OTHERS")
    
    hh_values <- sapply(hh_columns, function(col) settlement_data[[col]][1], simplify = TRUE, USE.NAMES = TRUE)
    reason_values <- sapply(reason_columns, function(col) {
      value <- settlement_data[[col]][1]
      if (value == 0) "NA" else value
    }, simplify = TRUE, USE.NAMES = TRUE)
    
    hh_formatted <- if (length(hh_columns) > 0) {
      paste0(paste("      ", hh_columns, "(", hh_values, ")", collapse = ",\n"))
    } else {
      "No Household Data"
    }
    
    reasons_formatted <- if (length(reason_columns) > 0) {
      paste0(paste("      ", reason_columns, "(", reason_values, ")", collapse = ",\n"))
    } else {
      "No Reasons Data"
    }
    
    missed_child_details <- paste0(
      "Country: ", settlement_data$COUNTRY[1], "\n",
      "Province: ", settlement_data$PROVINCE[1], "\n",
      "District: ", settlement_data$DISTRICT[1], "\n",
      "Facility: ", settlement_data$FACILITY[1], "\n",
      "Settlement: ", settlement_data$SETTLEMENT[1], "\n",
      "Response: ", settlement_data$RESPONSE[1], "\n",
      "RoundNumber: ", settlement_data$ROUNDNUMBER[1], "\n",
      "Total Child Missed: ", settlement_data$TOTAL_CHILD_MISSED[1], "\n",
      "HH where missed child has been found:\n", hh_formatted, "\n",
      "Reason(s) for not being vaccinated:\n", reasons_formatted, "\n",
      "Legend:\n",
      "  - Bounding box(m):", x, "\n",
      "  - Departure Point: \n",
      "  - Arrival Point: "
    )
    
    text_lines <- unlist(str_split(missed_child_details, "\n"))
    max_line_length <- max(nchar(text_lines))
    num_lines <- length(text_lines)
    rect_width <- max_line_length * 0.12
    rect_height <- num_lines * 0.5
    
    # Adjust specific lines for shape coloring
    shape_color_indices <- c(length(text_lines) - 2, length(text_lines) - 1)
    colors <- rep("black", length(text_lines))
    colors[shape_color_indices[1]] <- "chocolate"  # Departure Point ▲
    colors[shape_color_indices[2]] <- "yellow4"    # Arrival Point ▼
    
    missed_child_details_plot <- ggplot() +
      annotate(
        "rect", xmin = -rect_width / 2, xmax = rect_width / 2, 
        ymin = -rect_height / 2, ymax = rect_height / 2,
        fill = "lightgrey", color = NA
      ) +
      geom_text(
        aes(x = -rect_width / 2 + 0.05, y = rect_height / 2 - (0:(length(text_lines) - 1)) * 0.4, 
            label = text_lines),
        size = 4, hjust = 0, vjust = 1, family = "mono", lineheight = 0.8, color = "black"
      ) +
      geom_text(
        aes(x = rect_width / 2 - 0.3, y = rect_height / 2 - (shape_color_indices - 1) * 0.4,
            label = c("▲", "▼"), color = I(c("chocolate", "yellow4"))),
        size = 5, hjust = 0
      ) +
      coord_cartesian(
        xlim = c(-rect_width / 2 - 0.1, rect_width / 2 + 0.1), 
        ylim = c(-rect_height / 2 - 0.1, rect_height / 2 + 0.1), 
        expand = FALSE
      ) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    # Bounding box
    lon <- settlement_data$LON
    lat <- settlement_data$LAT
    lon_end <- settlement_data$LON_END
    lat_end <- settlement_data$LAT_END
    
    lon_margin <- bbox_m / (111320 * cos(mean(lat) * pi / 180))
    lat_margin <- bbox_m / 111320
    
    bbox <- st_bbox(c(
      xmin = min(c(lon, lon_end), na.rm = TRUE) - lon_margin,
      xmax = max(c(lon, lon_end), na.rm = TRUE) + lon_margin,
      ymin = min(c(lat, lat_end), na.rm = TRUE) - lat_margin,
      ymax = max(c(lat, lat_end), na.rm = TRUE) + lat_margin
    ), crs = st_crs(4326))
    
    # OSM queries
    area_major <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary")))
    area_minor <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "highway", value = c("tertiary", "residential")))
    area_blue  <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "waterway"))
    area_buildings <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "building"))
    
    located_missed_child <- ggplot() +
      geom_sf(data = area_major$osm_lines, color = "black", size = 0.4) +
      geom_sf(data = area_minor$osm_lines, color = "gray50", size = 0.3) +
      geom_sf(data = area_blue$osm_lines, color = "blue", size = 0.3) +
      geom_sf(data = area_buildings$osm_polygons, fill = "gray80", color = "gray50", size = 0.2) +
      geom_point(data = departure_point, aes(x = lon, y = lat), shape = 25, color = "chocolate", fill = NA, size = 4, stroke = 2) +
      geom_point(data = arrival_point, aes(x = lon_end, y = lat_end), shape = 24, color = "yellow4", fill = NA, size = 4, stroke = 2) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
      theme_void() +
      labs(title = paste0("Missed Children | ", roundNumber, " | ", settlement, " | ", ctry),
           subtitle = "") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)
      ) +
      annotation_scale(location = "br", width_hint = 0.2) +
      annotation_north_arrow(location = "tl", which_north = "true",
                             style = north_arrow_orienteering(),
                             pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"))
    
    combined_plot <- located_missed_child + missed_child_details_plot + plot_layout(widths = c(4, 1.5))
    return(combined_plot)
    
  }, error = function(e) {
    message("Error in create_settlement_map: ", e$message)
    ggplot() + annotate("text", x = 0, y = 0, label = paste("Error:", e$message), size = 6) + theme_void()
  })
}
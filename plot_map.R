# plot_map.R -- printable settlement map (OpenStreetMap detail). ASCII-only.
# create_settlement_map(rows, bbox_m) draws ALL monitor routes in `rows`
# (a cleaned subset from imset_data(): one settlement visit, one or more routes).

retry_query <- function(query, max_attempts = 3) {
  attempt <- 1
  while (attempt <= max_attempts) {
    res <- tryCatch(osmdata::osmdata_sf(query), error = function(e) {
      message("OSM attempt ", attempt, " failed: ", conditionMessage(e)); NULL })
    if (!is.null(res)) return(res)
    if (attempt == max_attempts) stop("OpenStreetMap query failed after ", max_attempts, " attempts")
    Sys.sleep(2)
    attempt <- attempt + 1
  }
}

.imset_osm_cache <- new.env(parent = emptyenv())

# one OSM layer for a bounding box; cached for the life of the R process; NULL when unavailable
imset_osm_layer <- function(bb, key, value = NULL) {
  ck <- paste(key, paste(value, collapse = ","), paste(round(as.numeric(bb), 4), collapse = ","))
  if (exists(ck, envir = .imset_osm_cache, inherits = FALSE)) return(get(ck, envir = .imset_osm_cache))
  res <- tryCatch({
    q <- osmdata::opq(bbox = as.numeric(bb), timeout = 40)
    q <- if (is.null(value)) osmdata::add_osm_feature(q, key = key) else osmdata::add_osm_feature(q, key = key, value = value)
    retry_query(q)
  }, error = function(e) { message("OSM layer ", key, ": ", conditionMessage(e)); NULL })
  if (!is.null(res)) assign(ck, res, envir = .imset_osm_cache)
  res
}

create_settlement_map <- function(rows, bbox_m = 300) {
  stopifnot(is.data.frame(rows), nrow(rows) > 0)
  routes <- rows
  n_all <- nrow(routes)
  good <- routes[routes$QC == "OK", , drop = FALSE]
  gps_note <- ""
  if (nrow(good) > 0 && nrow(good) < n_all) {
    gps_note <- paste0(" (", n_all - nrow(good), " suspect route(s) not drawn or counted)")
    routes <- good
  } else if (nrow(good) == 0) {
    gps_note <- " (all records of this selection look suspect)"
  }
  routes <- routes[!is.na(routes$LAT) & !is.na(routes$LON) & !is.na(routes$LAT_END) & !is.na(routes$LON_END), , drop = FALSE]
  if (nrow(routes) == 0) stop("No valid GPS coordinates for this selection.")
  routes$N <- seq_len(nrow(routes))

  # ---- bounding box ----
  lon_margin <- bbox_m / (111320 * cos(mean(routes$LAT) * pi / 180))
  lat_margin <- bbox_m / 111320
  bb <- c(xmin = min(c(routes$LON, routes$LON_END)) - lon_margin,
          ymin = min(c(routes$LAT, routes$LAT_END)) - lat_margin,
          xmax = max(c(routes$LON, routes$LON_END)) + lon_margin,
          ymax = max(c(routes$LAT, routes$LAT_END)) + lat_margin)

  # ---- summary panel (sums over all routes of the selection) ----
  u <- function(x) { v <- unique(stats::na.omit(x)); if (length(v)) paste(v, collapse = " / ") else "-" }
  sm <- function(col) if (col %in% names(routes)) sum(routes[[col]], na.rm = TRUE) else 0
  hh_cols <- grep("^HOUSEHOLD_[0-9]+$", names(rows), value = TRUE)
  hh_txt <- vapply(hh_cols, function(h) paste0("HH", sub("HOUSEHOLD_", "", h), "(", sm(h), ")"), character(1))
  hh_lines <- if (length(hh_txt)) vapply(split(hh_txt, ceiling(seq_along(hh_txt) / 4)), function(z) paste0("   ", paste(z, collapse = " ")), character(1)) else "   No household data"
  rs <- c(REASON_ABSENT = "Absent", REASON_NC = "Non-compliance", REASON_NOTVISITED = "Not visited",
          REASON_ASLEEP = "Asleep", REASON_VACCINATEDROUTINE = "Vacc. routine", REASON_OTHERS = "Other")
  rs_txt <- vapply(names(rs), function(r) paste0("   ", rs[[r]], " (", sm(r), ")"), character(1))
  dts <- stats::na.omit(rows$DATE)
  date_txt <- if (length(dts)) { r <- range(dts); if (r[1] == r[2]) format(r[1], "%d %b %Y") else paste(format(r[1], "%d %b %Y"), "-", format(r[2], "%d %b %Y")) } else "-"
  head_lines <- c(
    paste0("Country: ", u(rows$COUNTRY)), paste0("Province: ", u(rows$PROVINCE)), paste0("District: ", u(rows$DISTRICT)),
    paste0("Facility: ", u(rows$FACILITY)), paste0("Settlement: ", u(rows$SETTLEMENT)),
    paste0("Response: ", u(rows$RESPONSE)), paste0("Round: ", u(rows$ROUND)), paste0("Date(s): ", date_txt),
    paste0("Monitor routes: ", nrow(routes)), paste0("Total children missed: ", sm("CHILDREN")))
  wrap <- function(z) unlist(lapply(z, function(l) strwrap(l, width = 33, exdent = 3)))
  lines <- c(wrap(head_lines), "HH where a missed child", "was found:", hh_lines,
             "Reason(s) for not being", "vaccinated:", rs_txt,
             paste0("Legend (margin ", bbox_m, " m):"), "      Start point", "      End point")
  n_l <- length(lines)
  step <- min(0.03, 0.95 / n_l)
  ys <- 0.985 - (seq_len(n_l) - 1) * step
  panel <- ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "grey92", color = NA) +
    ggplot2::geom_text(ggplot2::aes(x = 0.03, y = ys, label = lines), size = 3.35, hjust = 0, vjust = 1, family = "mono", color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = 0.07, y = ys[n_l - 1] - step * 0.4), shape = 25, color = "chocolate", fill = NA, size = 3.2, stroke = 1.5) +
    ggplot2::geom_point(ggplot2::aes(x = 0.07, y = ys[n_l] - step * 0.4), shape = 24, color = "yellow4", fill = NA, size = 3.2, stroke = 1.5) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  # ---- OSM layers (each optional: a failed layer does not stop the map) ----
  major <- imset_osm_layer(bb, "highway", c("motorway", "primary", "secondary"))
  minor <- imset_osm_layer(bb, "highway", c("tertiary", "residential"))
  water <- imset_osm_layer(bb, "waterway")
  bld   <- imset_osm_layer(bb, "building")
  missing_layers <- c(if (is.null(major)) "major roads", if (is.null(minor)) "minor roads",
                      if (is.null(water)) "waterways", if (is.null(bld)) "buildings")
  has <- function(x, el) !is.null(x) && !is.null(x[[el]]) && nrow(x[[el]]) > 0

  p <- ggplot2::ggplot()
  if (has(bld, "osm_polygons"))   p <- p + ggplot2::geom_sf(data = bld$osm_polygons, fill = "gray80", color = "gray50", linewidth = 0.2)
  if (has(minor, "osm_lines"))    p <- p + ggplot2::geom_sf(data = minor$osm_lines, color = "gray50", linewidth = 0.3)
  if (has(major, "osm_lines"))    p <- p + ggplot2::geom_sf(data = major$osm_lines, color = "black", linewidth = 0.4)
  if (has(water, "osm_lines"))    p <- p + ggplot2::geom_sf(data = water$osm_lines, color = "blue", linewidth = 0.3)
  p <- p +
    ggplot2::geom_segment(data = routes, ggplot2::aes(x = LON, y = LAT, xend = LON_END, yend = LAT_END),
                          color = "grey35", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_point(data = routes, ggplot2::aes(x = LON, y = LAT), shape = 25, color = "chocolate", fill = NA, size = 4, stroke = 2) +
    ggplot2::geom_point(data = routes, ggplot2::aes(x = LON_END, y = LAT_END), shape = 24, color = "yellow4", fill = NA, size = 4, stroke = 2)
  if (nrow(routes) > 1 && nrow(routes) <= 20)
    p <- p + ggplot2::geom_text(data = routes, ggplot2::aes(x = LON, y = LAT, label = N), nudge_y = lat_margin * 0.12, size = 3.2, fontface = "bold")
  sub <- paste0(nrow(routes), " monitor route(s) | ", sm("CHILDREN"), " children missed", gps_note,
                if (length(missing_layers)) paste0(" | OSM layers not available: ", paste(missing_layers, collapse = ", ")) else "")
  p <- p +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]]), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Missed Children | ", u(rows$ROUND), " | ", u(rows$SETTLEMENT), " | ", u(rows$COUNTRY)), subtitle = sub) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "grey30"))
  if (requireNamespace("ggspatial", quietly = TRUE))
    p <- p + ggspatial::annotation_scale(location = "br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", style = ggspatial::north_arrow_orienteering(),
                                        pad_x = ggplot2::unit(0.1, "in"), pad_y = ggplot2::unit(0.2, "in"))
  patchwork::wrap_plots(p, panel, widths = c(4, 1.5))
}

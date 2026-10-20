# plot_map.R -- printable settlement map (OpenStreetMap detail). ASCII-only.
# create_settlement_map(rows, bbox_m) draws ALL monitor routes in `rows`
# (a cleaned subset from imset_data(): one settlement visit, one or more routes).

# ---- OpenStreetMap layers -------------------------------------------------------
# One small Overpass request (roads + waterways + buildings together) with a hard time limit,
# tried on several public servers. The old version sent four separate requests through osmdata,
# which the public server rate-limits ("Waiting 60s for retry backoff"). Nothing here retries
# in a loop: if no server answers in time the map is drawn without the OSM layers.

.imset_osm_cache <- new.env(parent = emptyenv())
IMSET_OVERPASS <- c("https://overpass-api.de/api/interpreter",
                    "https://overpass.kumi.systems/api/interpreter",
                    "https://overpass.private.coffee/api/interpreter")

imset_overpass_json <- function(bb, buildings = TRUE, per_server_s = 15, total_s = 40) {
  q <- sprintf('[out:json][timeout:25][bbox:%.6f,%.6f,%.6f,%.6f];(way["highway"];way["waterway"];%s);out geom tags;',
               bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]], if (buildings) 'way["building"];' else "")
  t0 <- Sys.time()
  for (ep in IMSET_OVERPASS) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > total_s) break
    r <- tryCatch(httr::POST(ep, body = list(data = q), encode = "form", httr::timeout(per_server_s),
                             httr::user_agent("AFRO-SIA-Dashboard (WHO AFRO)")), error = function(e) NULL)
    if (is.null(r) || httr::status_code(r) != 200) { message("Overpass ", ep, ": no usable answer"); next }
    js <- tryCatch(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(js) && !is.null(js$elements)) return(js)
  }
  NULL
}

# parse the Overpass JSON ("out geom") into sf layers
imset_parse_osm <- function(js) {
  els <- js$elements
  roads_major <- list(); roads_minor <- list(); water <- list(); bld <- list()
  major_types <- c("motorway", "trunk", "primary", "secondary", "motorway_link", "trunk_link", "primary_link", "secondary_link")
  for (e in els) {
    if (!identical(e$type, "way") || is.null(e$geometry) || length(e$geometry) < 2) next
    xy <- cbind(vapply(e$geometry, function(g) as.numeric(g$lon), numeric(1)), vapply(e$geometry, function(g) as.numeric(g$lat), numeric(1)))
    tg <- e$tags; if (is.null(tg)) tg <- list()
    if (!is.null(tg$building)) {
      if (nrow(xy) >= 4 && all(xy[1, ] == xy[nrow(xy), ])) bld[[length(bld) + 1]] <- sf::st_polygon(list(xy))
    } else if (!is.null(tg$waterway)) {
      water[[length(water) + 1]] <- sf::st_linestring(xy)
    } else if (!is.null(tg$highway)) {
      if (tg$highway %in% major_types) roads_major[[length(roads_major) + 1]] <- sf::st_linestring(xy)
      else roads_minor[[length(roads_minor) + 1]] <- sf::st_linestring(xy)
    }
  }
  mk <- function(x) if (length(x)) sf::st_sf(geometry = sf::st_sfc(x, crs = 4326)) else NULL
  list(major = mk(roads_major), minor = mk(roads_minor), water = mk(water), buildings = mk(bld))
}

# cached on a ~100 m grid of the bounding box; failures are not cached so a later click can retry
imset_osm_layers <- function(bb) {
  ck <- paste(round(as.numeric(bb), 3), collapse = ",")
  if (exists(ck, envir = .imset_osm_cache, inherits = FALSE)) return(get(ck, envir = .imset_osm_cache))
  big <- (bb[["xmax"]] - bb[["xmin"]]) * (bb[["ymax"]] - bb[["ymin"]]) > 0.0064   # > about 8 km x 8 km: skip buildings
  js <- tryCatch(imset_overpass_json(bb, buildings = !big), error = function(e) { message("Overpass: ", conditionMessage(e)); NULL })
  if (is.null(js)) return(NULL)
  res <- tryCatch(imset_parse_osm(js), error = function(e) { message("OSM parse: ", conditionMessage(e)); NULL })
  if (!is.null(res)) assign(ck, res, envir = .imset_osm_cache)
  res
}

create_settlement_map <- function(rows, bbox_m = 300, osm = TRUE) {
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

  # ---- OSM layers (optional: a missing server never stops the map) ----
  lay <- if (isTRUE(osm)) imset_osm_layers(bb) else NULL
  osm_note <- if (!isTRUE(osm)) " | OpenStreetMap layers switched off" else if (is.null(lay)) " | OpenStreetMap server busy: roads/buildings not drawn, try again later" else ""

  p <- ggplot2::ggplot()
  if (!is.null(lay$buildings)) p <- p + ggplot2::geom_sf(data = lay$buildings, fill = "gray80", color = "gray50", linewidth = 0.2)
  if (!is.null(lay$minor))     p <- p + ggplot2::geom_sf(data = lay$minor, color = "gray50", linewidth = 0.3)
  if (!is.null(lay$major))     p <- p + ggplot2::geom_sf(data = lay$major, color = "black", linewidth = 0.4)
  if (!is.null(lay$water))     p <- p + ggplot2::geom_sf(data = lay$water, color = "blue", linewidth = 0.3)
  p <- p +
    ggplot2::geom_segment(data = routes, ggplot2::aes(x = LON, y = LAT, xend = LON_END, yend = LAT_END),
                          color = "grey35", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_point(data = routes, ggplot2::aes(x = LON, y = LAT), shape = 25, color = "chocolate", fill = NA, size = 4, stroke = 2) +
    ggplot2::geom_point(data = routes, ggplot2::aes(x = LON_END, y = LAT_END), shape = 24, color = "yellow4", fill = NA, size = 4, stroke = 2)
  if (nrow(routes) > 1 && nrow(routes) <= 20)
    p <- p + ggplot2::geom_text(data = routes, ggplot2::aes(x = LON, y = LAT, label = N), nudge_y = lat_margin * 0.12, size = 3.2, fontface = "bold")
  sub <- paste0(nrow(routes), " monitor route(s) | ", sm("CHILDREN"), " children missed", gps_note, osm_note)
  p <- p +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]]), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Missed Children | ", u(rows$ROUND), " | ", u(rows$SETTLEMENT), " | ", u(rows$COUNTRY)), subtitle = sub) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "grey30"))
  # scale bar and north arrow drawn by hand (no extra package needed)
  w_m <- (bb[["xmax"]] - bb[["xmin"]]) * 111320 * cos(mean(c(bb[["ymin"]], bb[["ymax"]])) * pi / 180)
  nice <- c(10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000)
  sb_m <- nice[max(1, sum(nice <= w_m * 0.22))]
  sb_deg <- sb_m / (111320 * cos(mean(c(bb[["ymin"]], bb[["ymax"]])) * pi / 180))
  bb_dx <- bb[["xmax"]] - bb[["xmin"]]; bb_dy <- bb[["ymax"]] - bb[["ymin"]]
  sb_x1 <- bb[["xmax"]] - 0.04 * bb_dx; sb_x0 <- sb_x1 - sb_deg; sb_y <- bb[["ymin"]] + 0.05 * bb_dy
  na_x <- bb[["xmin"]] + 0.05 * bb_dx; na_y0 <- bb[["ymax"]] - 0.18 * bb_dy
  p <- p +
    ggplot2::annotate("segment", x = sb_x0, xend = sb_x1, y = sb_y, yend = sb_y, linewidth = 1.2, color = "black") +
    ggplot2::annotate("segment", x = c(sb_x0, sb_x1), xend = c(sb_x0, sb_x1), y = sb_y - 0.012 * bb_dy, yend = sb_y + 0.012 * bb_dy, linewidth = 0.8, color = "black") +
    ggplot2::annotate("text", x = (sb_x0 + sb_x1) / 2, y = sb_y + 0.03 * bb_dy, label = if (sb_m >= 1000) paste0(sb_m / 1000, " km") else paste0(sb_m, " m"), size = 3.2) +
    ggplot2::annotate("segment", x = na_x, xend = na_x, y = na_y0, yend = na_y0 + 0.11 * bb_dy, linewidth = 0.9, color = "black",
                      arrow = grid::arrow(length = grid::unit(0.14, "in"), type = "closed")) +
    ggplot2::annotate("text", x = na_x, y = na_y0 + 0.145 * bb_dy, label = "N", fontface = "bold", size = 4)
  patchwork::wrap_plots(p, panel, widths = c(4, 1.5))
}

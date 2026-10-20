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

# shared by the server-side request and the browser-side request (the browser can reach Overpass when the server cannot)
imset_osm_key <- function(bb) paste(round(as.numeric(bb), 3), collapse = ",")
imset_osm_cached <- function(bb) exists(imset_osm_key(bb), envir = .imset_osm_cache, inherits = FALSE)
imset_osm_buildings <- function(bb) (bb[["xmax"]] - bb[["xmin"]]) * (bb[["ymax"]] - bb[["ymin"]]) <= 0.0064   # beyond about 8 km x 8 km: no buildings
imset_osm_query <- function(bb, buildings = imset_osm_buildings(bb), pad = 0.002) {
  sprintf('[out:json][timeout:25][bbox:%.6f,%.6f,%.6f,%.6f];(way["highway"];way["waterway"];%s);out geom tags;',
          bb[["ymin"]] - pad, bb[["xmin"]] - pad, bb[["ymax"]] + pad, bb[["xmax"]] + pad, if (buildings) 'way["building"];' else "")
}
# compact layers sent by the browser: list(M = major roads, m = minor roads, w = waterways, b = buildings), each geometry a flat [lon,lat,lon,lat,...] array
imset_parse_compact <- function(x) {
  mkm <- function(g) matrix(as.numeric(unlist(g)), ncol = 2, byrow = TRUE)
  ls <- function(items) lapply(Filter(function(g) length(g) >= 4, items), function(g) sf::st_linestring(mkm(g)))
  pl <- function(items) lapply(Filter(function(g) length(g) >= 8, items), function(g) { xy <- mkm(g); if (all(xy[1, ] == xy[nrow(xy), ])) sf::st_polygon(list(xy)) else NULL })
  mk <- function(z) { z <- Filter(Negate(is.null), z); if (length(z)) sf::st_sf(geometry = sf::st_sfc(z, crs = 4326)) else NULL }
  list(major = mk(ls(x$M)), minor = mk(ls(x$m)), water = mk(ls(x$w)), buildings = mk(pl(x$b)))
}
imset_osm_store <- function(key, json) {
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  res <- imset_parse_compact(x)
  assign(key, res, envir = .imset_osm_cache)
  invisible(res)
}

imset_overpass_json <- function(bb, buildings = TRUE, per_server_s = 25, total_s = 28) {
  q <- imset_osm_query(bb, buildings)
  # all mirrors are asked at the same time (GET); the first valid answer wins and the others are cancelled
  js <- NULL; errs <- character(0); handles <- list()
  pool <- curl::new_pool(total_con = length(IMSET_OVERPASS), host_con = 2)
  for (ep in IMSET_OVERPASS) local({
    host <- sub("^https?://([^/]+).*$", "\\1", ep)
    h <- curl::new_handle(timeout = per_server_s, connecttimeout = 10, useragent = "AFRO-SIA-Dashboard (WHO AFRO)", failonerror = FALSE)
    handles[[length(handles) + 1]] <<- h
    curl::curl_fetch_multi(paste0(ep, "?data=", curl::curl_escape(q)),
      done = function(r) {
        if (!is.null(js)) return(invisible())
        if (r$status_code != 200) { errs <<- c(errs, paste0(host, ": HTTP ", r$status_code)); return(invisible()) }
        x <- tryCatch(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(x) && !is.null(x$elements)) js <<- x else errs <<- c(errs, paste0(host, ": unreadable answer"))
      },
      fail = function(msg) errs <<- c(errs, paste0(host, ": ", substr(as.character(msg), 1, 60))), pool = pool, handle = h)
  })
  t0 <- Sys.time()
  repeat {
    left <- total_s - as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (left <= 0) { errs <- c(errs, "time limit reached"); break }
    st <- tryCatch(curl::multi_run(timeout = left, pool = pool, poll = TRUE), error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
    if (!is.null(js) || is.null(st) || st$pending == 0) break
  }
  for (h in handles) try(curl::multi_cancel(h), silent = TRUE)
  if (!is.null(js)) { assign("last_err", "", envir = .imset_osm_cache); return(js) }
  assign("last_err", paste(unique(errs), collapse = "; "), envir = .imset_osm_cache)
  message("Overpass failed: ", paste(unique(errs), collapse = "; "))
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
imset_osm_layers <- function(bb, server = TRUE) {
  ck <- imset_osm_key(bb)
  if (exists(ck, envir = .imset_osm_cache, inherits = FALSE)) return(get(ck, envir = .imset_osm_cache))
  if (!isTRUE(server)) return(NULL)
  js <- tryCatch(imset_overpass_json(bb, buildings = imset_osm_buildings(bb)), error = function(e) { message("Overpass: ", conditionMessage(e)); NULL })
  if (is.null(js)) return(NULL)
  res <- tryCatch(imset_parse_osm(js), error = function(e) { message("OSM parse: ", conditionMessage(e)); NULL })
  if (!is.null(res)) assign(ck, res, envir = .imset_osm_cache)
  res
}


# ---- Raster tile background (satellite photo or OpenStreetMap map tiles) ---------
# Tiles are fetched in parallel with curl (short timeouts), stitched into one image and drawn under
# the routes. Satellite tiles show the houses themselves. Any failure returns NULL + a reason.
IMSET_TILES <- list(
  sat    = list(url = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/%d/%d/%d", zmax = 18, kind = "jpg", yx = TRUE,   # ArcGIS tile URLs are z/row(y)/col(x)
                label = "Esri World Imagery"),
  osmtile = list(url = "https://tile.openstreetmap.org/%d/%d/%d.png", zmax = 19, kind = "png", label = "OpenStreetMap tiles"))

imset_tile_x <- function(lon, z) floor((lon + 180) / 360 * 2^z)
imset_tile_y <- function(lat, z) floor((1 - asinh(tan(lat * pi / 180)) / pi) / 2 * 2^z)
imset_tile_lon <- function(x, z) x / 2^z * 360 - 180
imset_tile_lat <- function(y, z) atan(sinh(pi * (1 - 2 * y / 2^z))) * 180 / pi

# fetch + stitch the tiles of ONE zoom level; returns list(out = mosaic or NULL, errs, ph = share of "no data" tiles)
imset_fetch_at <- function(bb, cfg, src, z, total_s = 25) {
  x0 <- imset_tile_x(bb[["xmin"]], z); x1 <- imset_tile_x(bb[["xmax"]], z)
  y0 <- imset_tile_y(bb[["ymax"]], z); y1 <- imset_tile_y(bb[["ymin"]], z)
  ck <- paste("tile", src, z, x0, x1, y0, y1)
  if (exists(ck, envir = .imset_osm_cache, inherits = FALSE)) return(get(ck, envir = .imset_osm_cache))
  xs <- x0:x1; ys <- y0:y1
  grid <- expand.grid(x = xs, y = ys)
  res <- vector("list", nrow(grid)); errs <- character(0)
  pool <- curl::new_pool(total_con = 8, host_con = 8)
  for (i in seq_len(nrow(grid))) local({
    ii <- i
    h <- curl::new_handle(timeout = 12, connecttimeout = 8, useragent = "AFRO-SIA-Dashboard (WHO AFRO)", failonerror = FALSE)
    curl::curl_fetch_multi(if (isTRUE(cfg$yx)) sprintf(cfg$url, z, grid$y[ii], grid$x[ii]) else sprintf(cfg$url, z, grid$x[ii], grid$y[ii]),
      done = function(r) { if (r$status_code == 200) res[[ii]] <<- r$content else errs <<- c(errs, paste0("HTTP ", r$status_code)) },
      fail = function(msg) errs <<- c(errs, substr(as.character(msg), 1, 70)), pool = pool, handle = h)
  })
  tryCatch(curl::multi_run(timeout = total_s, pool = pool), error = function(e) errs <<- c(errs, conditionMessage(e)))
  dec <- function(raw) {
    a <- tryCatch(if (cfg$kind == "jpg") jpeg::readJPEG(raw) else png::readPNG(raw), error = function(e) NULL)
    if (is.null(a) || length(dim(a)) != 3) return(NULL)
    a[, , 1:3, drop = FALSE]
  }
  tl <- lapply(res, function(r) if (is.null(r)) NULL else dec(r))
  ok <- !vapply(tl, is.null, logical(1))
  if (!any(ok)) return(list(out = NULL, errs = unique(errs), ph = 1))
  # Esri returns a flat grey "Map data not yet available" tile where it has no imagery at this zoom
  is_ph <- vapply(tl[ok], function(a) mean(abs(a - 0.8) < 0.03) > 0.9, logical(1))
  ts <- dim(tl[[which(ok)[1]]])[1]
  big <- array(0.85, dim = c(length(ys) * ts, length(xs) * ts, 3))
  for (i in which(ok)) {
    r0 <- (match(grid$y[i], ys) - 1) * ts; c0 <- (match(grid$x[i], xs) - 1) * ts
    big[r0 + seq_len(ts), c0 + seq_len(ts), ] <- tl[[i]]
  }
  out <- list(img = big, xmin = imset_tile_lon(min(xs), z), xmax = imset_tile_lon(max(xs) + 1, z),
              ymin = imset_tile_lat(max(ys) + 1, z), ymax = imset_tile_lat(min(ys), z),
              z = z, missing = sum(!ok), label = cfg$label)
  r <- list(out = out, errs = unique(errs), ph = if (src == "sat") mean(is_ph) else 0)
  assign(ck, r, envir = .imset_osm_cache)
  r
}

# starts at the sharpest zoom and steps back while most tiles are "no data" (rural areas often stop at zoom 15-16)
imset_fetch_tiles <- function(bb, src = "sat", max_tiles = 36, total_s = 45) {
  cfg <- IMSET_TILES[[src]]
  z <- cfg$zmax; t0 <- Sys.time(); best <- NULL; last <- NULL
  repeat {
    repeat {   # keep the tile count small
      n <- (imset_tile_x(bb[["xmax"]], z) - imset_tile_x(bb[["xmin"]], z) + 1) * (imset_tile_y(bb[["ymin"]], z) - imset_tile_y(bb[["ymax"]], z) + 1)
      if (n <= max_tiles || z <= 10) break
      z <- z - 1
    }
    last <- imset_fetch_at(bb, cfg, src, z)
    if (is.null(last$out)) break                      # network problem: stepping back will not help
    best <- last$out
    if (last$ph < 0.5 || z <= 12) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > total_s) break
    z <- z - 1
  }
  if (is.null(best)) {
    assign("last_err", paste0(cfg$label, ": ", if (length(last$errs)) paste(last$errs, collapse = ", ") else "no tile received"), envir = .imset_osm_cache)
    return(NULL)
  }
  if (!is.null(last) && !is.null(last$out) && last$ph >= 0.5)
    assign("last_err", paste0(cfg$label, ": no imagery available for this area"), envir = .imset_osm_cache)
  best$ph <- if (is.null(last$ph)) 0 else last$ph
  best
}

# the routes that create_settlement_map() draws, numbered the same way (used for the "Zoom to route" list)
imset_valid_routes <- function(rows) {
  good <- rows[rows$QC == "OK", , drop = FALSE]
  r <- if (nrow(good) > 0) good else rows
  r <- r[!is.na(r$LAT) & !is.na(r$LON) & !is.na(r$LAT_END) & !is.na(r$LON_END), , drop = FALSE]
  if (nrow(r)) r$N <- seq_len(nrow(r))
  r
}

# frame of the map: the routes (all or the selected one) plus a margin in metres
imset_map_bbox <- function(routes, bbox_m) {
  lon_margin <- bbox_m / (111320 * cos(mean(routes$LAT) * pi / 180))
  lat_margin <- bbox_m / 111320
  c(xmin = min(c(routes$LON, routes$LON_END)) - lon_margin, ymin = min(c(routes$LAT, routes$LAT_END)) - lat_margin,
    xmax = max(c(routes$LON, routes$LON_END)) + lon_margin, ymax = max(c(routes$LAT, routes$LAT_END)) + lat_margin)
}
imset_map_bbox_for <- function(rows, bbox_m, route = NULL) {
  v <- imset_valid_routes(rows)
  if (!is.null(route) && length(route) == 1 && !is.na(suppressWarnings(as.integer(route))) && as.integer(route) %in% v$N) v <- v[v$N == as.integer(route), , drop = FALSE]
  imset_map_bbox(v, bbox_m)
}

create_settlement_map <- function(rows, bbox_m = 300, bg = "sat", route = NULL, vector_server = TRUE) {
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
  routes_all <- routes                      # every route is drawn; the frame and the summary follow the selected one
  one_route <- !is.null(route) && length(route) == 1 && !is.na(suppressWarnings(as.integer(route))) && as.integer(route) %in% routes$N
  if (one_route) routes <- routes[routes$N == as.integer(route), , drop = FALSE]

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
    paste0("Monitor routes: ", if (one_route) paste0("route ", routes$N[1], " of ", nrow(routes_all)) else nrow(routes)), paste0("Total children missed: ", sm("CHILDREN")))
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

  # ---- background: satellite / OSM tiles / OSM vector layers / none (a missing server never stops the map) ----
  if (!bg %in% c("sat", "osmtile", "vector", "none")) bg <- "sat"
  tl <- NULL; lay <- NULL
  if (bg %in% c("sat", "osmtile")) tl <- imset_fetch_tiles(bb, bg)
  vec_failed <- FALSE
  if (bg == "vector") {
    lay <- imset_osm_layers(bb, server = vector_server)
    if (is.null(lay)) { vec_failed <- TRUE; vec_err <- get0("last_err", envir = .imset_osm_cache, ifnotfound = ""); tl <- imset_fetch_tiles(bb, "osmtile") }
  }
  osm_detail <- if (bg != "none" && is.null(tl) && is.null(lay)) paste0(if (vec_failed) paste0("OSM vector: ", vec_err, " | tiles: ") else "", get0("last_err", envir = .imset_osm_cache, ifnotfound = "")) else if (vec_failed) paste0("OSM vector server did not answer (", vec_err, "); showing OpenStreetMap map tiles instead") else ""
  osm_note <- if (bg == "none") "" else if (!is.null(tl) && isTRUE(tl$ph >= 0.5)) " | Satellite imagery is not available for this area" else if (!is.null(tl) && vec_failed) paste0(" | Background: ", tl$label, " (zoom ", tl$z, ") - vector data unavailable") else if (!is.null(tl)) paste0(" | Background: ", tl$label, " (zoom ", tl$z, ")") else if (!is.null(lay)) " | Background: OpenStreetMap vectors" else " | Background not available (see note below)"
  on_img <- !is.null(tl) && bg == "sat"
  p <- ggplot2::ggplot()
  if (!is.null(tl)) p <- p + ggplot2::annotation_raster(tl$img, xmin = tl$xmin, xmax = tl$xmax, ymin = tl$ymin, ymax = tl$ymax, interpolate = TRUE)
  if (!is.null(lay$buildings)) p <- p + ggplot2::geom_sf(data = lay$buildings, fill = "gray80", color = "gray50", linewidth = 0.2)
  if (!is.null(lay$minor))     p <- p + ggplot2::geom_sf(data = lay$minor, color = "gray50", linewidth = 0.3)
  if (!is.null(lay$major))     p <- p + ggplot2::geom_sf(data = lay$major, color = "black", linewidth = 0.4)
  if (!is.null(lay$water))     p <- p + ggplot2::geom_sf(data = lay$water, color = "blue", linewidth = 0.3)
  seg_col <- if (on_img) "white" else "grey35"
  if (on_img)   # white halo so the markers stay readable on photographs
    p <- p +
      ggplot2::geom_segment(data = routes_all, ggplot2::aes(x = LON, y = LAT, xend = LON_END, yend = LAT_END), color = "black", linewidth = 1.6, alpha = 0.5) +
      ggplot2::geom_point(data = routes_all, ggplot2::aes(x = LON, y = LAT), shape = 25, color = "white", fill = NA, size = 5.2, stroke = 3.6) +
      ggplot2::geom_point(data = routes_all, ggplot2::aes(x = LON_END, y = LAT_END), shape = 24, color = "white", fill = NA, size = 5.2, stroke = 3.6)
  p <- p +
    ggplot2::geom_segment(data = routes_all, ggplot2::aes(x = LON, y = LAT, xend = LON_END, yend = LAT_END),
                          color = seg_col, linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(data = routes_all, ggplot2::aes(x = LON, y = LAT), shape = 25, color = "chocolate", fill = NA, size = 4, stroke = 2) +
    ggplot2::geom_point(data = routes_all, ggplot2::aes(x = LON_END, y = LAT_END), shape = 24, color = if (on_img) "yellow" else "yellow4", fill = NA, size = 4, stroke = 2)
  if (nrow(routes) > 1 && nrow(routes) <= 20)
    p <- p + ggplot2::geom_text(data = routes_all, ggplot2::aes(x = LON, y = LAT, label = N), nudge_y = lat_margin * 0.12, size = 3.4, fontface = "bold", color = if (on_img) "white" else "black")
  sub <- paste0(if (one_route) paste0("route ", routes$N[1], " of ", nrow(routes_all)) else paste0(nrow(routes), " monitor route(s)"), " | ", sm("CHILDREN"), " children missed", gps_note, osm_note)
  p <- p +
    ggplot2::coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]]), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::labs(title = paste0("Missed Children | ", u(rows$ROUND), " | ", u(rows$SETTLEMENT), " | ", u(rows$COUNTRY)), subtitle = sub, caption = if (nzchar(osm_detail)) paste(strwrap(paste0("Background request failed - ", osm_detail), 110), collapse = "\n") else NULL) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "grey30"),
                   plot.caption = ggplot2::element_text(hjust = 0, size = 7, color = "grey40"))
  # scale bar and north arrow drawn by hand (no extra package needed)
  w_m <- (bb[["xmax"]] - bb[["xmin"]]) * 111320 * cos(mean(c(bb[["ymin"]], bb[["ymax"]])) * pi / 180)
  nice <- c(10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000)
  sb_m <- nice[max(1, sum(nice <= w_m * 0.22))]
  sb_deg <- sb_m / (111320 * cos(mean(c(bb[["ymin"]], bb[["ymax"]])) * pi / 180))
  bb_dx <- bb[["xmax"]] - bb[["xmin"]]; bb_dy <- bb[["ymax"]] - bb[["ymin"]]
  sb_x1 <- bb[["xmax"]] - 0.04 * bb_dx; sb_x0 <- sb_x1 - sb_deg; sb_y <- bb[["ymin"]] + 0.05 * bb_dy
  na_x <- bb[["xmin"]] + 0.05 * bb_dx; na_y0 <- bb[["ymax"]] - 0.18 * bb_dy
  p <- p +
    ggplot2::annotate("segment", x = sb_x0, xend = sb_x1, y = sb_y, yend = sb_y, linewidth = 1.2, color = if (on_img) "white" else "black") +
    ggplot2::annotate("segment", x = c(sb_x0, sb_x1), xend = c(sb_x0, sb_x1), y = sb_y - 0.012 * bb_dy, yend = sb_y + 0.012 * bb_dy, linewidth = 0.8, color = if (on_img) "white" else "black") +
    ggplot2::annotate("text", x = (sb_x0 + sb_x1) / 2, y = sb_y + 0.03 * bb_dy, label = if (sb_m >= 1000) paste0(sb_m / 1000, " km") else paste0(sb_m, " m"), size = 3.2, color = if (on_img) "white" else "black") +
    ggplot2::annotate("segment", x = na_x, xend = na_x, y = na_y0, yend = na_y0 + 0.11 * bb_dy, linewidth = 0.9, color = if (on_img) "white" else "black",
                      arrow = grid::arrow(length = grid::unit(0.14, "in"), type = "closed")) +
    ggplot2::annotate("text", x = na_x, y = na_y0 + 0.145 * bb_dy, label = "N", fontface = "bold", size = 4, color = if (on_img) "white" else "black")
  patchwork::wrap_plots(p, panel, widths = c(4, 1.5))
}

# ---------------------------------------------------------------------------
# Western Sahara (Sahara occidental) is a disputed area: draw it in dark grey on
# every map that shows it.  Call add_disputed_area(plot) on a finished ggplot map.
#
# - The shape is taken from the map's own layers when it is there, otherwise from
#   the app's country shapes (`shapefiles$countries`).
# - It is only drawn when the map already covers that part of Africa, so maps of
#   a single country do not suddenly zoom out.
# - Any problem simply returns the map unchanged.
# ---------------------------------------------------------------------------

DISPUTED_FILL   <- "grey30"
DISPUTED_BORDER <- "grey35"

add_disputed_area <- function(p, fill = DISPUTED_FILL, border = DISPUTED_BORDER, linewidth = 0.2) {
  tryCatch({
    if (!inherits(p, "ggplot")) return(p)

    is_ws <- function(d) inherits(d, "sf") && "ADM0_NAME" %in% names(d) && any(toupper(as.character(d$ADM0_NAME)) == "WESTERN SAHARA")

    layer_data <- lapply(p$layers, function(l) l$data)
    layer_sf   <- Filter(function(d) inherits(d, "sf"), layer_data)
    if (!length(layer_sf)) return(p)

    # 1. where does Western Sahara come from?
    ws <- NULL
    for (d in layer_sf) if (is_ws(d)) { ws <- d; break }
    if (is.null(ws)) {
      shp <- get0("shapefiles", envir = globalenv(), inherits = FALSE)
      if (is.list(shp) && is_ws(shp$countries)) ws <- shp$countries
    }
    if (is.null(ws)) return(p)
    ws <- ws[toupper(as.character(ws$ADM0_NAME)) == "WESTERN SAHARA", , drop = FALSE]
    ws <- sf::st_make_valid(ws[, intersect(names(ws), "ADM0_NAME"), drop = FALSE])

    # 2. only when the map already covers that area
    crs <- sf::st_crs(layer_sf[[1]])
    if (!is.na(crs) && !is.na(sf::st_crs(ws)) && crs != sf::st_crs(ws)) ws <- sf::st_transform(ws, crs)
    wb <- sf::st_bbox(ws)
    bbs <- lapply(layer_sf, function(d) {
      b <- sf::st_as_sfc(sf::st_bbox(d))
      if (!is.na(crs) && !is.na(sf::st_crs(b)) && crs != sf::st_crs(b)) b <- sf::st_transform(b, crs)
      sf::st_bbox(b)
    })
    ub <- c(xmin = min(sapply(bbs, `[[`, "xmin")), ymin = min(sapply(bbs, `[[`, "ymin")),
            xmax = max(sapply(bbs, `[[`, "xmax")), ymax = max(sapply(bbs, `[[`, "ymax")))
    tol <- 0.02 * max(ub["xmax"] - ub["xmin"], ub["ymax"] - ub["ymin"])
    covers <- wb["xmin"] >= ub["xmin"] - tol && wb["xmax"] <= ub["xmax"] + tol &&
              wb["ymin"] >= ub["ymin"] - tol && wb["ymax"] <= ub["ymax"] + tol
    if (!isTRUE(unname(covers))) return(p)

    # 3. draw it (the layer only, so the map's own coord / limits are kept)
    p + ggplot2::geom_sf(data = ws, fill = fill, colour = border, linewidth = linewidth,
                         inherit.aes = FALSE, show.legend = FALSE)[[1]]
  }, error = function(e) p)
}

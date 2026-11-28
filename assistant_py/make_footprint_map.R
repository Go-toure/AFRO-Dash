# Footprint map: monitor routes (start to end) that found missed children, on a satellite photo (or the district boundary), with an information card.   Author: Gorgui Ba Toure
# Rscript make_footprint_map.R <boundaries_dir> <routes.csv> <out.png> <country> <title> <subtitle> <xmin> <xmax> <ymin> <ymax> <bg sat|osmtile|none> <width_m>
#   routes.csv: lat, lon, lat_end, lon_end, children, mine (1 = the routes asked for, 0 = other routes shown thin for context)
#   <routes.csv>.meta.json (optional, UTF-8): country, title, subtitle, info (list of [label, value]), totals {routes, children, households},
#                                             reasons (named numbers), round_lines (character), note (character)
#   bg sat / osmtile: the same tile functions as the dashboard's settlement map (AFRO-Dash/plot_map.R: Esri World Imagery, OpenStreetMap tiles);
#                     if no tile can be fetched the map falls back to the district boundary and says so.
suppressMessages({library(sf); library(ggplot2); library(dplyr); library(patchwork)}); suppressMessages(sf_use_s2(FALSE))
a <- commandArgs(trailingOnly = TRUE)
dir <- a[1]; csv <- a[2]; out <- a[3]; ctry <- a[4]; ttl <- a[5]; sub <- a[6]
xl <- as.numeric(a[7:8]); yl <- as.numeric(a[9:10]); bgmode <- a[11]; width_m <- as.numeric(a[12])
if (bgmode %in% c("0", "1")) bgmode <- if (bgmode == "1") "osmtile" else "none"
meta <- list(); mf <- paste0(csv, ".meta.json")
if (file.exists(mf)) meta <- jsonlite::fromJSON(paste(readLines(mf, encoding = "UTF-8", warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
if (!is.null(meta$country)) ctry <- meta$country; if (!is.null(meta$title)) ttl <- meta$title; if (!is.null(meta$subtitle)) sub <- meta$subtitle
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(iconv(as.character(x), to = "ASCII//TRANSLIT")))
p <- read.csv(csv, stringsAsFactors = FALSE)
p$children[!is.finite(p$children)] <- 0
inbox <- function(lon, lat) is.finite(lon) & is.finite(lat) & lon >= xl[1] & lon <= xl[2] & lat >= yl[1] & lat <= yl[2]
p <- p[inbox(p$lon, p$lat) | inbox(p$lon_end, p$lat_end), ]
if (!nrow(p)) stop("no route inside the map window")
mine <- p[p$mine == 1, ]; oth <- p[p$mine == 0, ]
if (nrow(oth) > 6000) oth <- oth[sample.int(nrow(oth), 6000), ]
mlat <- mean(yl); m_per_deg <- 111320 * cos(mlat * pi / 180); span_m <- diff(xl) * m_per_deg
NAVY <- "#0f172a"; INK <- "#e5e7eb"; MUTED <- "#94a3b8"; ACC <- "#38bdf8"

# ---- background: satellite / OSM tiles (dashboard functions) or the district boundary ----
tl <- NULL; bg_note <- ""; bg_label <- "district boundary"
if (bgmode %in% c("sat", "osmtile")) {
  if (nzchar(Sys.getenv("SIA_FP_FAKE_TILES"))) {   # test only: a synthetic photo, so the layout can be checked without network
    set.seed(1); n <- 256; img <- array(0, c(n, n, 3)); img[, , 1] <- 0.35 + 0.12 * matrix(runif(n * n), n); img[, , 2] <- 0.42 + 0.12 * matrix(runif(n * n), n); img[, , 3] <- 0.3 + 0.1 * matrix(runif(n * n), n)
    tl <- list(img = img, xmin = xl[1] - 0.01, xmax = xl[2] + 0.01, ymin = yl[1] - 0.01, ymax = yl[2] + 0.01, z = 18, label = "Esri World Imagery", ph = 0)
  } else {
    pm <- file.path(dir, "..", "plot_map.R")
    ok <- file.exists(pm) && requireNamespace("curl", quietly = TRUE) && requireNamespace("jpeg", quietly = TRUE) && requireNamespace("png", quietly = TRUE)
    if (ok) {
      env <- new.env(); sys.source(pm, envir = env)
      tl <- tryCatch(env$imset_fetch_tiles(c(xmin = xl[1], ymin = yl[1], xmax = xl[2], ymax = yl[2]), bgmode), error = function(e) NULL)
      if (is.null(tl)) bg_note <- paste0("Background not loaded: ", get0("last_err", envir = env$.imset_osm_cache, ifnotfound = "no tile received"))
      else if (isTRUE(tl$ph >= 0.5)) { bg_note <- "Satellite imagery is not available for this area"; tl <- NULL }
    } else bg_note <- "Background not loaded: AFRO-Dash/plot_map.R or the R packages curl, jpeg, png are missing"
  }
  if (!is.null(tl)) bg_label <- paste0(tl$label, " (zoom ", tl$z, ")")
}
on_img <- !is.null(tl)

g <- ggplot()
if (on_img) g <- g + annotation_raster(tl$img, xmin = tl$xmin, xmax = tl$xmax, ymin = tl$ymin, ymax = tl$ymax, interpolate = TRUE) else {
  d <- readRDS(file.path(dir, "africa_districts_simplified.rds")); d <- d[norm(d$ADM0_NAME) == norm(ctry), ]
  g <- g + geom_sf(data = d, fill = "#eef1f5", colour = "#aab4c0", linewidth = 0.3)
}
if (nrow(oth)) g <- g + geom_segment(data = oth, aes(x = lon, y = lat, xend = lon_end, yend = lat_end), colour = if (on_img) "white" else "#9aa5b1", linewidth = 0.4, alpha = 0.55, linetype = "dashed") +
  geom_point(data = oth, aes(lon, lat), colour = if (on_img) "white" else "#9aa5b1", size = 1, alpha = 0.6)
small <- width_m < 12000
psz <- if (width_m < 3000) 3.2 else if (width_m < 12000) 2.6 else if (width_m < 60000) 1.8 else if (width_m < 400000) 1.3 else 1
lw <- if (small) 1.5 else if (width_m < 60000) 1.0 else 0.8
LOW <- "#ffe14d"; HIGH <- "#ff2e63"
if (!on_img) { LOW <- "#f4a582"; HIGH <- "#67001f" }
if (nrow(mine)) {
  if (on_img) g <- g + geom_segment(data = mine, aes(x = lon, y = lat, xend = lon_end, yend = lat_end), colour = "black", linewidth = lw * 2.1, alpha = 0.55, lineend = "round")
  g <- g + geom_segment(data = mine, aes(x = lon, y = lat, xend = lon_end, yend = lat_end, colour = pmax(children, 1)), linewidth = lw,
                        lineend = "round", arrow = if (small) arrow(length = unit(0.13, "in"), type = "closed", angle = 22) else NULL) +
    geom_point(data = mine, aes(lon, lat), shape = 21, fill = "white", colour = if (on_img) "black" else "#1d2530", size = psz, stroke = 0.9) +
    scale_colour_gradient(low = LOW, high = HIGH, trans = "sqrt", name = "Children missed\non the route", limits = c(1, max(2, max(mine$children, 1))),
                          breaks = function(l) { b <- unique(round(pretty(l, 4))); b[b >= 1] })
  if (small && nrow(mine) <= 20) {
    lab <- mine; lab$N <- seq_len(nrow(lab))
    g <- g + geom_label(data = lab, aes(lon, lat, label = N), nudge_y = diff(yl) * 0.035, size = 3.1, fontface = "bold", colour = "white", fill = "#111827", alpha = 0.85, label.size = 0, label.padding = unit(0.12, "lines"))
  }
}
# scale bar and north arrow
nice <- c(10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000, 10000, 20000, 25000, 50000, 100000, 200000)
bar_m <- nice[which.min(abs(nice - span_m / 5))]
dx <- diff(xl); dy <- diff(yl); bw <- bar_m / m_per_deg
bx <- xl[2] - 0.04 * dx - bw; by <- yl[1] + 0.06 * dy
lab_bar <- if (bar_m >= 1000) paste0(bar_m / 1000, " km") else paste0(bar_m, " m")
fg <- if (on_img) "white" else "#1d2530"; shade <- if (on_img) "#000000" else "#ffffff"
nax <- xl[1] + 0.06 * dx; nay <- yl[2] - 0.20 * dy
g <- g +
  annotate("rect", xmin = bx - 0.02 * dx, xmax = bx + bw + 0.02 * dx, ymin = by - 0.03 * dy, ymax = by + 0.065 * dy, fill = shade, alpha = if (on_img) 0.45 else 0.75, colour = NA) +
  annotate("segment", x = bx, xend = bx + bw, y = by, yend = by, linewidth = 1.2, colour = fg) +
  annotate("segment", x = c(bx, bx + bw), xend = c(bx, bx + bw), y = by - 0.012 * dy, yend = by + 0.012 * dy, linewidth = 0.8, colour = fg) +
  annotate("text", x = bx + bw / 2, y = by + 0.04 * dy, label = lab_bar, size = 3.3, colour = fg, fontface = "bold") +
  annotate("rect", xmin = nax - 0.035 * dx, xmax = nax + 0.035 * dx, ymin = nay - 0.03 * dy, ymax = nay + 0.19 * dy, fill = shade, alpha = if (on_img) 0.45 else 0.75, colour = NA) +
  annotate("segment", x = nax, xend = nax, y = nay, yend = nay + 0.11 * dy, linewidth = 1, colour = fg, arrow = arrow(length = unit(0.13, "in"), type = "closed")) +
  annotate("text", x = nax, y = nay + 0.155 * dy, label = "N", fontface = "bold", size = 4, colour = fg)
if (on_img) g <- g + annotate("text", x = xl[1] + 0.015 * dx, y = yl[1] + 0.02 * dy, label = paste0("Imagery: ", if (grepl("Esri", tl$label)) "Esri, Maxar, Earthstar Geographics" else "OpenStreetMap contributors"), hjust = 0, size = 2.4, colour = "white", fontface = "italic")
g <- g + coord_sf(xlim = xl, ylim = yl, expand = FALSE, datum = NA) + theme_void(base_size = 11) +
  theme(legend.position = if (utils::packageVersion("ggplot2") >= "3.5.0") "inside" else c(0.985, 0.985), legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.88), colour = NA), legend.margin = margin(6, 8, 6, 8),
        legend.title = element_text(size = 8, colour = "#334155", face = "bold"), legend.text = element_text(size = 8, colour = "#334155"),
        legend.key.height = unit(0.7, "cm"), legend.key.width = unit(0.3, "cm"), plot.margin = margin(0, 0, 0, 0),
        panel.border = element_rect(colour = NAVY, fill = NA, linewidth = 1.2))
if (utils::packageVersion("ggplot2") >= "3.5.0") g <- g + theme(legend.position.inside = c(0.985, 0.985))
if (nrow(mine) && length(unique(mine$children)) < 2) g <- g + guides(colour = "none")

# ---- the information card (1 unit = 0.1 inch high; text sizes in mm) ----
info <- meta$info; if (is.null(info)) info <- list()
tot <- meta$totals; if (is.null(tot)) tot <- list(routes = nrow(mine), children = sum(mine$children), households = NA)
rs <- unlist(meta$reasons); rs <- rs[is.finite(rs) & rs > 0]; rs <- sort(rs, decreasing = TRUE)
rl <- unlist(meta$round_lines)
asp <- dy / (dx * cos(mlat * pi / 180))
h <- min(8.4, max(7.4, 6.4 * asp)); mapw <- min(9.5, max(4.8, h / asp)); cw <- 3.5
cardH <- h * 10; y <- cardH - 3
card <- ggplot() + annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = cardH, fill = NAVY)
nl <- function(x) length(strsplit(x, "\n")[[1]])
put <- function(label, size = 3.3, colour = INK, face = "plain", x = 6, pad = 0.9) {
  card <<- card + annotate("text", x = x, y = y, label = label, size = size, colour = colour, fontface = face, hjust = 0, vjust = 1, lineheight = 0.95)
  y <<- y - (0.54 * size * nl(label) + pad)
}
put("MISSED CHILDREN  |  IM FOOTPRINT", 2.5, ACC, "bold", pad = 1.6)
put(paste(strwrap(ttl, 22), collapse = "\n"), 5, "white", "bold", pad = 2.2)
for (it in info) {
  v <- paste(strwrap(as.character(it[[2]]), 26), collapse = "\n")
  card <- card + annotate("text", x = 6, y = y - 0.25, label = toupper(it[[1]]), size = 2.1, colour = MUTED, fontface = "bold", hjust = 0, vjust = 1) +
    annotate("text", x = 36, y = y, label = v, size = 3, colour = INK, hjust = 0, vjust = 1, lineheight = 0.95)
  y <- y - (0.54 * 3 * nl(v) + 0.7)
}
y <- y - 1.2
fmt <- function(v) if (is.null(v) || !is.finite(suppressWarnings(as.numeric(v)))) "-" else format(round(as.numeric(v)), big.mark = ",", trim = TRUE)
tile <- function(x0, val, lab) { card <<- card + annotate("rect", xmin = x0, xmax = x0 + 28, ymin = y - 8.2, ymax = y, fill = "#1e293b") +
  annotate("text", x = x0 + 14, y = y - 2.9, label = val, size = 6, colour = "white", fontface = "bold") + annotate("text", x = x0 + 14, y = y - 6.8, label = lab, size = 1.9, colour = MUTED, fontface = "bold") }
tile(4, fmt(tot$routes), "ROUTES"); tile(36, fmt(tot$children), "CHILDREN MISSED"); tile(68, fmt(tot$households), "HOUSEHOLDS")
y <- y - 10.8
for (l in rl) put(paste(strwrap(l, 40), collapse = "\n"), 2.7, INK, pad = 0.8)
if (length(rs)) {
  y <- y - 1; put("REASONS CHILDREN WERE MISSED", 2.2, ACC, "bold", pad = 1.4)
  mx <- max(rs)
  for (i in seq_along(rs)) {
    card <- card + annotate("text", x = 6, y = y, label = names(rs)[i], size = 2.6, colour = INK, hjust = 0, vjust = 1) +
      annotate("rect", xmin = 6, xmax = 6 + 72 * rs[i] / mx, ymin = y - 3.5, ymax = y - 2.3, fill = ACC) +
      annotate("text", x = 94, y = y, label = format(rs[i], big.mark = ","), size = 2.6, colour = "white", hjust = 1, vjust = 1, fontface = "bold")
    y <- y - 5.3
  }
}
note <- if (!is.null(meta$note)) paste(strwrap(meta$note, 52), collapse = "\n") else ""
if (nzchar(bg_note)) note <- paste(note, paste(strwrap(bg_note, 52), collapse = "\n"), sep = "\n")
card <- card + annotate("text", x = 6, y = 1.2, label = trimws(note), size = 1.8, colour = MUTED, hjust = 0, vjust = 0, lineheight = 0.95) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, cardH), expand = FALSE) + theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

# ---- assemble: title above, map | card ----
fig <- (g | card) + plot_layout(widths = c(mapw, cw)) +
  plot_annotation(title = ttl, subtitle = sub, theme = theme(plot.title = element_text(face = "bold", size = 17, colour = "#0f172a"), plot.subtitle = element_text(size = 10.5, colour = "#475569", margin = margin(b = 8)),
                  plot.margin = margin(10, 10, 8, 10), plot.background = element_rect(fill = "white", colour = NA)))
W <- mapw + cw + 0.3; H <- h + 0.95
ok <- requireNamespace("ragg", quietly = TRUE)
if (ok) ggsave(out, fig, device = ragg::agg_png, width = W, height = H, dpi = 170, bg = "white") else ggsave(out, fig, width = W, height = H, dpi = 170, bg = "white")
writeLines(jsonlite::toJSON(list(routes_drawn = nrow(mine), other_routes = nrow(oth), scale_bar_m = bar_m, window_width_m = round(span_m), background = bg_label, satellite = on_img && bgmode == "sat", background_note = bg_note), auto_unbox = TRUE), paste0(out, ".json"))

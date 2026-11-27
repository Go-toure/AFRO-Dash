# Footprint map: monitor routes (start to end) that found missed children, zoomed to a place, a district or a whole selection, with a scale bar.   Author: Gorgui Ba Toure
# Rscript make_footprint_map.R <boundaries_dir> <routes.csv> <out.png> <country> <title> <subtitle> <xmin> <xmax> <ymin> <ymax> <basemap 0|1> <width_m>
#   routes.csv: lat, lon, lat_end, lon_end, children, mine (1 = the place asked for, 0 = other routes shown in grey for context)
#   basemap 1: street map tiles under the routes when the R packages ggspatial and rosm are installed (needs internet); otherwise a plain background.
suppressMessages({library(sf); library(ggplot2); library(dplyr)}); suppressMessages(sf_use_s2(FALSE))
a <- commandArgs(trailingOnly = TRUE)
dir <- a[1]; csv <- a[2]; out <- a[3]; ctry <- a[4]; ttl <- a[5]; sub <- a[6]
xl <- as.numeric(a[7:8]); yl <- as.numeric(a[9:10]); basemap <- a[11] == "1"; width_m <- as.numeric(a[12])
tf <- paste0(csv, ".txt")   # country, title, subtitle in UTF-8 (accents survive any command-line encoding)
if (file.exists(tf)) { z <- readLines(tf, encoding = "UTF-8", warn = FALSE); ctry <- z[1]; ttl <- z[2]; sub <- if (length(z) >= 3) z[3] else "" }
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(iconv(as.character(x), to = "ASCII//TRANSLIT")))
p <- read.csv(csv, stringsAsFactors = FALSE)
p$children[!is.finite(p$children)] <- 0
inbox <- function(lon, lat) is.finite(lon) & is.finite(lat) & lon >= xl[1] & lon <= xl[2] & lat >= yl[1] & lat <= yl[2]
p <- p[inbox(p$lon, p$lat) | inbox(p$lon_end, p$lat_end), ]
if (!nrow(p)) stop("no route inside the map window")
mine <- p[p$mine == 1, ]; oth <- p[p$mine == 0, ]
if (nrow(oth) > 6000) oth <- oth[sample.int(nrow(oth), 6000), ]
ink <- "#1d2530"; muted <- "#5b6675"
g <- ggplot()
tiles <- FALSE
if (basemap && requireNamespace("ggspatial", quietly = TRUE) && requireNamespace("rosm", quietly = TRUE)) {
  g <- g + tryCatch({ tiles <- TRUE; ggspatial::annotation_map_tile(type = "osm", zoomin = 0, progress = "none", quiet = TRUE) }, error = function(e) { tiles <<- FALSE; NULL })
}
if (!tiles) {
  d <- readRDS(file.path(dir, "africa_districts_simplified.rds")); d <- d[norm(d$ADM0_NAME) == norm(ctry), ]
  g <- g + geom_sf(data = d, fill = "#f3f5f7", colour = "#b9c2cc", linewidth = 0.3)
}
if (nrow(oth)) g <- g + geom_segment(data = oth, aes(x = lon, y = lat, xend = lon_end, yend = lat_end), colour = "#9aa5b1", linewidth = 0.35, alpha = 0.7) +
  geom_point(data = oth, aes(lon, lat), colour = "#9aa5b1", size = 0.7, alpha = 0.7)
psz <- if (width_m < 3000) 2.4 else if (width_m < 60000) 1.8 else if (width_m < 400000) 1.3 else 1.0
if (nrow(mine)) g <- g +
  geom_segment(data = mine, aes(x = lon, y = lat, xend = lon_end, yend = lat_end, colour = pmax(children, 1)), linewidth = 1.1, alpha = 0.95, lineend = "round") +
  geom_point(data = mine, aes(lon, lat, colour = pmax(children, 1)), size = psz, shape = 16) +
  { if (width_m < 60000) geom_point(data = mine, aes(lon_end, lat_end), colour = "white", fill = "#1d2530", size = 1.6, shape = 21, stroke = 0.4) } +
  scale_colour_gradient(low = "#f4a582", high = "#67001f", trans = "sqrt", name = "Children missed\non the route", breaks = function(l) unique(round(pretty(l, 4)))[unique(round(pretty(l, 4))) >= 1])
# scale bar: a round length close to a fifth of the window width
mlat <- mean(yl); m_per_deg <- 111320 * cos(mlat * pi / 180)
span_m <- diff(xl) * m_per_deg
nice <- c(10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000, 10000, 20000, 25000, 50000, 100000, 200000)
bar_m <- nice[which.min(abs(nice - span_m / 5))]
bx <- xl[1] + 0.04 * diff(xl); by <- yl[1] + 0.05 * diff(yl); bw <- bar_m / m_per_deg
lab <- if (bar_m >= 1000) paste0(bar_m / 1000, " km") else paste0(bar_m, " m")
g <- g +
  annotate("rect", xmin = bx - 0.012 * diff(xl), xmax = bx + bw + 0.012 * diff(xl), ymin = by - 0.035 * diff(yl), ymax = by + 0.06 * diff(yl), fill = "white", alpha = 0.8, colour = NA) +
  annotate("segment", x = bx, xend = bx + bw, y = by, yend = by, linewidth = 1.3, colour = ink) +
  annotate("text", x = bx + bw / 2, y = by + 0.028 * diff(yl), label = lab, size = 3.4, colour = ink) +
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  labs(title = ttl, subtitle = sub,
       caption = paste(strwrap(sprintf("%d route(s) with missed children%s%s. Routes that found nobody are not in the data.%s", nrow(mine), if (width_m < 60000) " (start = dot, end = white dot)" else " (dot = start of the route)",
                                       if (nrow(oth)) sprintf("; %d other routes in grey", nrow(oth)) else "", if (basemap && !tiles) " Street map not available (install the R packages ggspatial and rosm): plain background." else ""), 100), collapse = "\n")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = ink, size = 14), plot.subtitle = element_text(colour = muted, size = 9.5),
        plot.caption = element_text(colour = muted, size = 8, hjust = 0), axis.title = element_blank(), axis.text = element_text(colour = muted, size = 7.5),
        panel.grid = element_line(colour = "#edf0f3", linewidth = 0.3), legend.position = "right", plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(12, 14, 10, 14))
asp <- (diff(yl)) / (diff(xl) * cos(mlat * pi / 180)); w <- 9; h <- min(9.5, max(5, 7 * asp + 2.2))
ok <- requireNamespace("ragg", quietly = TRUE)
if (ok) ggsave(out, g, device = ragg::agg_png, width = w, height = h, dpi = 150, bg = "white") else ggsave(out, g, width = w, height = h, dpi = 150, bg = "white")
writeLines(jsonlite::toJSON(list(routes_drawn = nrow(mine), other_routes = nrow(oth), scale_bar_m = bar_m, window_width_m = round(span_m), street_map = tiles), auto_unbox = TRUE), paste0(out, ".json"))

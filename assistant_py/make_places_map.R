# Map of ONE place: the start of every monitor route that found missed children, on the district boundary.   Author: Gorgui Ba Toure
# Rscript make_places_map.R <boundaries_dir> <points.csv> <out.png> <country> <district|-> <title> <subtitle>
#   points.csv: lat, lon, children, mine (1 = the place asked for; 0 = the other places of the same district, drawn in grey for context)
suppressMessages({library(sf); library(ggplot2); library(dplyr)}); suppressMessages(sf_use_s2(FALSE))   # simplified boundaries are not always valid on the sphere
a <- commandArgs(trailingOnly = TRUE)
dir <- a[1]; csv <- a[2]; out <- a[3]; ctry <- a[4]; dist <- a[5]; ttl <- a[6]; sub <- a[7]
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(iconv(as.character(x), to = "ASCII//TRANSLIT")))
p <- read.csv(csv, stringsAsFactors = FALSE)
p <- p[is.finite(p$lat) & is.finite(p$lon), ]
if (!nrow(p)) stop("no GPS points to draw")
d <- readRDS(file.path(dir, "africa_districts_simplified.rds"))
d <- d[norm(d$ADM0_NAME) == norm(ctry), ]
sel <- if (dist != "-") d[norm(d$ADM2_NAME) == norm(dist), ] else d[0, ]
mine <- p[p$mine == 1, ]; oth <- p[p$mine == 0, ]
# the district of the place: by name when the form records it, else the polygon that holds the median GPS position of the place
if (!nrow(sel) && nrow(mine)) {
  pt <- st_sfc(st_point(c(median(mine$lon), median(mine$lat))), crs = st_crs(d))
  hit <- suppressWarnings(tryCatch(st_intersects(pt, d)[[1]], error = function(e) integer(0)))
  if (!length(hit)) hit <- suppressWarnings(tryCatch(st_nearest_feature(pt, d), error = function(e) integer(0)))
  if (length(hit)) { sel <- d[hit[1], ]; sub <- paste0(sub, "  |  district by GPS: ", tools::toTitleCase(tolower(sel$ADM2_NAME[1]))) }
}
ref <- if (nrow(mine)) mine else p
pad <- max(0.06, 0.35 * max(diff(range(ref$lon)), diff(range(ref$lat)), 0.03))
xl <- range(ref$lon) + c(-pad, pad); yl <- range(ref$lat) + c(-pad, pad)
if (nrow(sel)) { bb <- st_bbox(sel); xl <- range(c(xl, bb["xmin"], bb["xmax"])); yl <- range(c(yl, bb["ymin"], bb["ymax"])) }
if (diff(xl) > 4 || diff(yl) > 4) { xl <- range(ref$lon) + c(-pad, pad); yl <- range(ref$lat) + c(-pad, pad) }   # a district that spans a huge area: stay on the place
oth <- oth[oth$lon >= xl[1] & oth$lon <= xl[2] & oth$lat >= yl[1] & oth$lat <= yl[2], ]
if (nrow(oth) > 3000) oth <- oth[sample.int(nrow(oth), 3000), ]
ink <- "#1d2530"; muted <- "#5b6675"
g <- ggplot() +
  geom_sf(data = d, fill = "#f3f5f7", colour = "#c9d0d8", linewidth = 0.25) +
  { if (nrow(sel)) geom_sf(data = sel, fill = "#e6f0fa", colour = "#0b3f73", linewidth = 0.7) } +
  { if (nrow(oth)) geom_point(data = oth, aes(lon, lat), colour = "#9aa5b1", size = 0.9, alpha = 0.55) } +
  geom_point(data = mine, aes(lon, lat, size = pmax(children, 1)), shape = 21, fill = "#b83b5e", colour = "white", stroke = 0.6, alpha = 0.9) +
  scale_size_area(max_size = 9, breaks = c(1, 5, 10, 25), name = "Children missed\non the route") +
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  labs(title = ttl, subtitle = sub,
       caption = paste(strwrap(sprintf("%d route(s) of this place with missed children (red); %d other routes nearby in grey.%s Routes that found nobody are not in the data.", nrow(mine), nrow(oth), if (nrow(sel)) " Blue outline = district." else ""), 95), collapse = "\n")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = ink, size = 14), plot.subtitle = element_text(colour = muted, size = 9.5),
        plot.caption = element_text(colour = muted, size = 8, hjust = 0), axis.title = element_blank(), axis.text = element_text(colour = muted, size = 8),
        panel.grid = element_line(colour = "#edf0f3", linewidth = 0.3), legend.position = "right", plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(12, 14, 10, 14))
ok <- requireNamespace("ragg", quietly = TRUE)
if (ok) ggsave(out, g, device = ragg::agg_png, width = 8, height = 7, dpi = 150, bg = "white") else ggsave(out, g, width = 8, height = 7, dpi = 150, bg = "white")
info <- list(district_by_gps = if (nrow(sel)) tools::toTitleCase(tolower(sel$ADM2_NAME[1])) else NA, province_by_gps = if (nrow(sel) && "ADM1_NAME" %in% names(sel)) tools::toTitleCase(tolower(sel$ADM1_NAME[1])) else NA,
             routes_of_place = nrow(mine), other_routes_nearby = nrow(oth))
writeLines(jsonlite::toJSON(info, auto_unbox = TRUE, na = "null"), paste0(out, ".json"))

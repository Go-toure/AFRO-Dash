# Draws a choropleth of one engine result on the AFRO boundaries.   Author: Gorgui Ba Toure
# Rscript make_map.R <boundaries_dir> <values.csv> <out.png> <level> <title> <subtitle> <country|-> <unit>
#   boundaries_dir  folder with africa_districts_simplified.rds, africa_provinces_simplified.rds, africa_countries_simplified.rds (AFRO-Dash/input)
#   values.csv      columns: key, value, n   (key = GEO_CODE, or a place name, or a country name, as `level` says)
#   level           geo_code | district | province | country   (district and province join by name, inside the country filter)
# Writes <out.png> and <out.png>.json (rows matched, rows not matched, the unmatched keys).
suppressMessages({library(sf); library(ggplot2); library(dplyr)})
a <- commandArgs(trailingOnly = TRUE)
dir <- a[1]; csv <- a[2]; out <- a[3]; level <- a[4]; ttl <- a[5]; sub <- a[6]; ctry <- a[7]; unit <- a[8]
norm <- function(x) gsub("[^A-Z0-9]", "", toupper(iconv(as.character(x), to = "ASCII//TRANSLIT")))
v <- read.csv(csv, stringsAsFactors = FALSE, encoding = "UTF-8")
v$k <- if (level == "geo_code") toupper(trimws(v$key)) else norm(v$key)
ctn <- if (ctry != "-") norm(ctry) else NA

countries <- readRDS(file.path(dir, "africa_countries_simplified.rds"))
cn <- intersect(c("ADM0_NAME", "country", "COUNTRY"), names(countries))[1]
countries$.c <- norm(countries[[cn]])
file <- c(geo_code = "africa_districts_simplified.rds", district = "africa_districts_simplified.rds", province = "africa_provinces_simplified.rds", country = "africa_countries_simplified.rds")[[level]]
b <- readRDS(file.path(dir, file))
b$.c <- norm(b[["ADM0_NAME"]])
b$.k <- switch(level, geo_code = toupper(b$GEO_CODE), district = norm(b$ADM2_NAME), province = norm(b$ADM1_NAME), country = b$.c)
if (!is.na(ctn)) { b <- b[b$.c == ctn, ]; countries <- countries[countries$.c == ctn, ]; if (nrow(b) == 0) stop("country not found in the boundaries: ", ctry) }
vv <- v %>% filter(!is.na(value)) %>% group_by(k) %>% summarise(value = first(value), n = sum(n, na.rm = TRUE), .groups = "drop")
m <- b %>% left_join(vv, by = c(".k" = "k"))
matched <- sum(vv$k %in% b$.k); unmatched <- v$key[!(v$k %in% b$.k)]
if (matched == 0) stop("no place of the result matches the boundaries (level ", level, ")")

cat_vals <- !is.numeric(m$value)   # a class per place (for example the LQAS performance class): one colour per class
pct <- !cat_vals && grepl("percent", unit, ignore.case = TRUE)
lims <- if (cat_vals) NULL else if (pct) c(0, 100) else range(m$value, na.rm = TRUE)
if (!cat_vals && !pct && diff(lims) == 0) lims <- lims + c(-1, 1)
CLS <- c("high" = "#1a9850", "moderate" = "#fee08b", "poor" = "#fc8d59", "very poor" = "#b2182b", "pass" = "#1a9850", "fail" = "#b2182b")
if (cat_vals) { lv <- unique(na.omit(m$value)); lv <- c(intersect(names(CLS), lv), setdiff(lv, names(CLS))); m$value <- factor(m$value, levels = lv)
  cls_col <- ifelse(lv %in% names(CLS), CLS[lv], grDevices::hcl.colors(length(lv), "Dark 3")); names(cls_col) <- lv }
# zoom on the places that have a value (a block, a country, a few districts), with a small margin
has <- if (cat_vals) !is.na(m$value) else !is.na(m$value)
bb <- sf::st_bbox(m[has, ]); padx <- 0.04 * (bb["xmax"] - bb["xmin"]) + 0.05; pady <- 0.04 * (bb["ymax"] - bb["ymin"]) + 0.05
xl <- c(bb["xmin"] - padx, bb["xmax"] + padx); yl <- c(bb["ymin"] - pady, bb["ymax"] + pady)
ink <- "#1d2530"; muted <- "#5b6675"
p <- ggplot(m) +
  geom_sf(aes(fill = value), colour = if (level == "country") "white" else "#ffffff", linewidth = if (level == "district" || level == "geo_code") 0.08 else 0.2) +
  geom_sf(data = countries, fill = NA, colour = "#8a94a3", linewidth = 0.25) +
  { if (cat_vals) scale_fill_manual(values = cls_col, na.value = "#e9ecf0", name = unit, drop = FALSE)
    else scale_fill_gradient(low = "#e6f0fa", high = "#0b3f73", limits = lims, na.value = "#e9ecf0", name = unit,
                      labels = function(x) if (pct) paste0(x, "%") else format(x, big.mark = ",", trim = TRUE)) } +
  coord_sf(xlim = xl, ylim = yl, expand = FALSE) +
  labs(title = ttl, subtitle = sub, caption = sprintf("Grey = no data. %d of %d places of the result placed on the map.", matched, nrow(vv) + length(unmatched))) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = ink, size = 14, margin = margin(b = 3)),
        plot.subtitle = element_text(colour = muted, size = 9.5, margin = margin(b = 8)),
        plot.caption = element_text(colour = muted, size = 8, hjust = 0, margin = margin(t = 6)),
        legend.position = "bottom", legend.key.width = unit(2.2, "cm"), legend.key.height = unit(0.28, "cm"),
        legend.title = element_text(colour = muted, size = 8.5, vjust = 1), legend.text = element_text(colour = muted, size = 8),
        plot.margin = margin(12, 14, 10, 14), plot.background = element_rect(fill = "white", colour = NA))
ok <- requireNamespace("ragg", quietly = TRUE)
asp <- as.numeric((yl[2] - yl[1]) / (xl[2] - xl[1])); w <- 9; h <- min(9.5, max(5.2, w * asp + 1.7))
if (ok) ggsave(out, p, device = ragg::agg_png, width = w, height = h, dpi = 150, bg = "white") else ggsave(out, p, width = w, height = h, dpi = 150, bg = "white")
writeLines(jsonlite::toJSON(list(matched = matched, total = nrow(vv) + length(unmatched), unmatched = head(unique(unmatched), 15)), auto_unbox = TRUE), paste0(out, ".json"))

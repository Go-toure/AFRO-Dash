# ============================================================
# lqas_summary_modern.R
# Executive page for the LQAS Summary Maps: one message, plain words.
#   - headline: how many districts were rated "high" in most of their rounds
#   - the district map (same data as "District Summary Map") with a plain legend
#   - countries ranked by the percentage of their districts in each band
#   - cards: districts analysed, mostly high, rarely high, never high
# Needs the helpers of reasons_refusal_modern.R (modern_header_gg, modern_cards_gg,
# modern_banner_gg, REFUSAL_COL). Input = the list returned by generate_lqas_summary_map_optimized().
# ============================================================

LQ_BANDS    <- c("80-100%", "50-80%", "25-50%", "0-25%")      # best first
# the standard LQAS colours (same as the District Summary Map): green4, yellow, tomato, red4
LQ_BAND_COL <- c("80-100%" = "green4", "50-80%" = "yellow", "25-50%" = "tomato", "0-25%" = "red4")
LQ_THRESHOLD <- 80   # a country reaches the threshold when at least 80% of its districts are mostly high
LQ_BAND_LAB <- c("80-100%" = "80% or more", "50-80%" = "50 to 80%", "25-50%" = "25 to 50%", "0-25%" = "Under 25%")

lq_period_text <- function(info) {
  d <- regmatches(info, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", info))[[1]]
  if (length(d) < 2) return("Selected period")
  f <- function(x) format(as.Date(x), "%b %Y")
  a <- f(d[1]); b <- f(d[2])
  if (identical(a, b)) a else paste(a, "to", b)
}

lq_summary <- function(res) {
  # Every district with LQAS results (not only the ones drawn on the map). res$exec_districts holds one
  # row per district, counted exactly like the cards above the maps and the District Performance tab.
  n_map <- if (!is.null(res$map_district_set)) nrow(res$map_district_set) else NA_integer_
  d <- res$exec_districts
  if (is.null(d) || !nrow(d)) {
    d <- res$map_district_set
    if (is.null(d) || !nrow(d)) return(NULL)
    d$always_high <- d$high_performance_percentage >= 100
    d$ever_high   <- d$high_performance_percentage > 0
  }
  d <- as.data.frame(d)
  d$band <- factor(as.character(d$High.perf.summary), levels = LQ_BANDS)
  d <- d[!is.na(d$band), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  n <- nrow(d)
  cnt <- table(d$band)
  by <- as.data.frame.matrix(table(d$country, d$band))
  by$country <- rownames(by); by$n <- rowSums(by[, LQ_BANDS, drop = FALSE])
  for (b in LQ_BANDS) by[[paste0("p_", b)]] <- 100 * by[[b]] / by$n
  list(d = d, n = n, n_map = n_map, k = length(unique(d$country)), cnt = cnt, by = by,
       always = sum(d$always_high, na.rm = TRUE),
       never  = sum(!d$ever_high, na.rm = TRUE))
}

lq_nice <- function(x) {
  x <- stringr::str_to_title(tolower(x))
  x <- gsub("\\bOf\\b", "of", x); x <- gsub("\\bThe\\b", "the", x); x <- gsub("\\bAnd\\b", "and", x)
  x
}

# header, cards and banner of this page: same look as the other modern pages, with larger text
lq_header_gg <- function(title, subtitle) {
  C <- REFUSAL_COL
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.02, y = 0.70, label = title, hjust = 0, size = 5.3, fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.18, label = subtitle, hjust = 0, size = 3.2, colour = C$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 14, 0, 10))
}

lq_cards_gg <- function(label, value, sub, col) {
  C <- REFUSAL_COL
  n <- length(label)
  cd <- data.frame(y1 = n:1, y0 = (n:1) - 1, label = label, value = value, sub = sub, col = col, stringsAsFactors = FALSE)
  # compact cards, light look: white card, thin soft-coloured stripe, one dark colour for all the numbers
  ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.98, ymin = y0 + 0.05, ymax = y1 - 0.05), fill = "white", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.035, ymin = y0 + 0.05, ymax = y1 - 0.05, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(0.08, y1 - 0.22, label = toupper(label)), hjust = 0, size = 2.8, fontface = "bold", colour = C$muted) +
    ggplot2::geom_text(ggplot2::aes(0.08, (y0 + y1) / 2 + 0.0, label = value), hjust = 0, size = 6.5, fontface = "bold", colour = C$navy) +
    ggplot2::geom_text(ggplot2::aes(0.08, y0 + 0.21, label = sub), hjust = 0, size = 2.6, colour = C$muted, lineheight = 0.95) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.3 * n, n), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 6, 6, 4))
}

lq_banner_gg <- function(text, col) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = text, colour = "white", fontface = "bold", size = 3.8) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(2, 14, 4, 10))
}

lq_pct <- function(x, n) if (n > 0) sprintf("%.0f%%", 100 * x / n) else "0%"

# lighter country borders, and Western Sahara (disputed area) in dark grey
lq_soften_map <- function(p) {
  ws <- NULL
  for (i in seq_along(p$layers)) {
    d <- p$layers[[i]]$data
    if (is.null(ws) && inherits(d, "sf") && "ADM0_NAME" %in% names(d) && any(toupper(d$ADM0_NAME) == "WESTERN SAHARA"))
      ws <- d[toupper(d$ADM0_NAME) == "WESTERN SAHARA", , drop = FALSE]
    ap <- p$layers[[i]]$aes_params
    if (identical(ap$colour, "black")) {
      p$layers[[i]] <- ggplot2::geom_sf(data = d, fill = ap$fill, colour = "grey35", linewidth = 0.2,
                                        inherit.aes = FALSE, show.legend = FALSE)[[1]]
    }
  }
  if (!is.null(ws) && nrow(ws) > 0)
    p <- p + ggplot2::geom_sf(data = ws, fill = "grey30", colour = "grey35", linewidth = 0.2,
                              inherit.aes = FALSE, show.legend = FALSE)
  p
}

lq_map_panel <- function(res) {
  C <- REFUSAL_COL
  if (is.null(res$map)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No map for this selection", colour = C$muted) +
             ggplot2::theme_void())
  }
  suppressMessages(
    lq_soften_map(res$map) +
      ggplot2::scale_fill_manual(values = LQ_BAND_COL, breaks = LQ_BANDS, labels = unname(LQ_BAND_LAB[LQ_BANDS]),
                                 name = "Rounds rated high", drop = FALSE,
                                 guide = ggplot2::guide_legend(ncol = 1, title.position = "top")) +
      ggplot2::labs(title = "Each district, by how often it was rated high") +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = ggplot2::element_text(size = 10.2, face = "bold", colour = C$text, hjust = 0, margin = ggplot2::margin(0, 0, 4, 0)),
        plot.title.position = "plot", legend.position = "left", legend.justification = "center", legend.title = ggplot2::element_text(size = 8, colour = C$text),
        legend.text = ggplot2::element_text(size = 7.8, colour = C$text), legend.key.size = grid::unit(0.38, "cm"),
        plot.margin = ggplot2::margin(6, 6, 2, 8))
  )
}

lq_country_panel <- function(s, max_rows = 26) {
  C <- REFUSAL_COL
  by <- s$by
  by <- by[order(-by[["p_80-100%"]], by[["p_0-25%"]], -by$n), , drop = FALSE]
  if (nrow(by) > max_rows) by <- by[order(-by$n), , drop = FALSE][seq_len(max_rows), , drop = FALSE][order(-by[order(-by$n), , drop = FALSE][seq_len(max_rows), "p_80-100%"]), , drop = FALSE]
  by$row <- rev(seq_len(nrow(by)))
  rect <- do.call(rbind, lapply(seq_len(nrow(by)), function(i) {
    p <- unlist(by[i, paste0("p_", LQ_BANDS)]); x1 <- cumsum(p); x0 <- x1 - p
    data.frame(row = by$row[i], band = LQ_BANDS, x0 = x0, x1 = x1, p = p, stringsAsFactors = FALSE)
  }))
  rect$col <- LQ_BAND_COL[rect$band]
  rect$lab <- ifelse(rect$p >= 9, sprintf("%.0f", rect$p), "")
  rect$txt <- ifelse(rect$band %in% c("50-80%", "25-50%"), C$text, "white")
  nm <- lq_nice(by$country)
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = rect, ggplot2::aes(xmin = x0, xmax = x1, ymin = row - 0.38, ymax = row + 0.38, fill = col), colour = NA) +
    ggplot2::geom_segment(ggplot2::aes(x = LQ_THRESHOLD, xend = LQ_THRESHOLD, y = min(by$row) - 0.6, yend = max(by$row) + 1.15),
                          colour = C$navy, linetype = "dashed", linewidth = 0.55) +
    ggplot2::geom_text(data = rect, ggplot2::aes(x = (x0 + x1) / 2, y = row, label = lab, colour = txt), size = 2.8, fontface = "bold") +
    ggplot2::geom_text(data = by, ggplot2::aes(x = 101.5, y = row, label = format(n, big.mark = ",")), hjust = 0, size = 2.7, colour = C$text) +
    ggplot2::annotate("text", x = 101.5, y = max(by$row) + 0.75, label = "districts", hjust = 0, size = 2.6, colour = C$text) +
    ggplot2::annotate("text", x = LQ_THRESHOLD, y = max(by$row) + 1.55, label = paste0(LQ_THRESHOLD, "% threshold"), size = 2.8,
                      colour = C$navy, fontface = "bold") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_y_continuous(breaks = by$row, labels = nm, expand = ggplot2::expansion(add = c(0.6, 1.9))) +
    ggplot2::scale_x_continuous(limits = c(0, 124), breaks = NULL, expand = c(0, 0)) +
    ggplot2::labs(title = "Districts per country (%)", x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.title = ggplot2::element_text(size = 10.2, face = "bold", colour = C$text, hjust = 0, margin = ggplot2::margin(0, 0, 4, 0)),
                   axis.text.y = ggplot2::element_text(size = 8.2, colour = C$text, hjust = 1, margin = ggplot2::margin(r = 4)),
                   plot.margin = ggplot2::margin(6, 4, 2, 4))
}

lq_headline <- function(s) {
  n80 <- as.integer(s$cnt[["80-100%"]])
  paste0(round(100 * n80 / s$n), "% of districts (", format(n80, big.mark = ","), " of ", format(s$n, big.mark = ","),
         ") were rated high in at least 4 out of 5 rounds")
}

lq_banner_text <- function(s) {
  by <- s$by[s$by$n >= 5, , drop = FALSE]
  if (!nrow(by)) by <- s$by
  nice <- lq_nice
  best <- by[order(-by[["p_80-100%"]], -by$n), , drop = FALSE]; best <- head(best[best[["p_80-100%"]] > 0, , drop = FALSE], 2)
  worst <- by[order(-by[["p_0-25%"]], -by$n), , drop = FALSE]; worst <- head(worst[worst[["p_0-25%"]] > 0, , drop = FALSE], 2)
  reach <- s$by[s$by[["p_80-100%"]] >= LQ_THRESHOLD, , drop = FALSE]
  reach <- reach[order(-reach[["p_80-100%"]]), , drop = FALSE]
  shown <- head(nice(reach$country), 4)
  a <- paste0(nrow(reach), " of ", nrow(s$by), " countries reach the ", LQ_THRESHOLD, "% threshold",
              if (nrow(reach)) paste0(" (", paste(shown, collapse = ", "), if (nrow(reach) > 4) paste0(" +", nrow(reach) - 4, " more"), ")") else "")
  b <- if (nrow(worst)) paste0("Needs attention: ", paste0(nice(worst$country), " ", sprintf("%.0f%%", worst[["p_0-25%"]]), collapse = ", "), " rarely high")
  paste(c(a, b), collapse = "   |   ")
}

# cards side by side in one row (same light look as the stacked version)
lq_cards_row_gg <- function(label, value, sub, col) {
  C <- REFUSAL_COL
  n <- length(label)
  cd <- data.frame(x0 = seq_len(n) - 1, x1 = seq_len(n), label = label, value = value, sub = sub, col = col, stringsAsFactors = FALSE)
  ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x0 + 0.02, xmax = x1 - 0.02, ymin = 0.06, ymax = 0.94), fill = "white", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = x0 + 0.02, xmax = x0 + 0.032, ymin = 0.06, ymax = 0.94, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(x0 + 0.06, 0.74, label = toupper(label)), hjust = 0, size = 2.8, fontface = "bold", colour = C$muted) +
    ggplot2::geom_text(ggplot2::aes(x0 + 0.06, 0.36, label = value), hjust = 0, size = 7, fontface = "bold", colour = C$navy) +
    ggplot2::geom_text(ggplot2::aes(x0 + 0.40, 0.36, label = sub), hjust = 0, size = 2.7, colour = C$muted, lineheight = 0.95) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, n), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(2, 14, 2, 8))
}

lqas_exec_height <- function(res = NULL) 7.4

lqas_exec_gg <- function(res) {
  s <- lq_summary(res)
  if (is.null(s)) return(NULL)
  C <- REFUSAL_COL
  hd <- lq_header_gg("LQAS performance over time: how consistently districts are rated high",
                         paste0(lq_period_text(res$period_info), "  |  each district is judged by the percentage of its LQAS rounds rated \"high\""))
  cnt <- s$cnt
  cd <- lq_cards_row_gg(
    c("Districts analysed", "Mostly high", "Rarely high", "Never rated high"),
    c(format(s$n, big.mark = ","), format(as.integer(cnt[["80-100%"]]), big.mark = ","), format(as.integer(cnt[["0-25%"]]), big.mark = ","), format(s$never, big.mark = ",")),
    c(paste0("in ", s$k, if (s$k == 1) " country" else " countries"),
      paste0(lq_pct(cnt[["80-100%"]], s$n), " of districts\nhigh in 80% or more of rounds"),
      paste0(lq_pct(cnt[["0-25%"]], s$n), " of districts\nhigh in under 25% of rounds"),
      paste0("always rated high: ", format(s$always, big.mark = ","))),
    c("#A8C0C6", "#8CC9A0", "#E3A2A2", "#E3A2A2"))
  mid <- cowplot::plot_grid(lq_map_panel(res), lq_country_panel(s), nrow = 1, rel_widths = c(1.2, 1.1))
  bn <- lq_banner_gg(lq_banner_text(s), C$navy)
  cowplot::plot_grid(hd, cd, mid, bn, ncol = 1, rel_heights = c(0.85, 0.95, 5.1, 0.5))
}

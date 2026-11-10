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
  d <- res$map_district_set
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- as.data.frame(d)
  d$band <- factor(as.character(d$High.perf.summary), levels = LQ_BANDS)
  d <- d[!is.na(d$band), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  n <- nrow(d)
  cnt <- table(d$band)
  by <- as.data.frame.matrix(table(d$country, d$band))
  by$country <- rownames(by); by$n <- rowSums(by[, LQ_BANDS, drop = FALSE])
  for (b in LQ_BANDS) by[[paste0("p_", b)]] <- 100 * by[[b]] / by$n
  list(d = d, n = n, k = length(unique(d$country)), cnt = cnt, by = by,
       always = sum(d$high_performance_percentage >= 100, na.rm = TRUE),
       never  = sum(d$high_performance_percentage <= 0, na.rm = TRUE))
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
    ggplot2::annotate("text", x = 0.02, y = 0.70, label = title, hjust = 0, size = 5.9, fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.18, label = subtitle, hjust = 0, size = 3.6, colour = C$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 14, 0, 10))
}

lq_cards_gg <- function(label, value, sub, col) {
  C <- REFUSAL_COL
  n <- length(label)
  cd <- data.frame(y1 = n:1, y0 = (n:1) - 1, label = label, value = value, sub = sub, col = col, stringsAsFactors = FALSE)
  ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.98, ymin = y0 + 0.04, ymax = y1 - 0.04), fill = "#F7F9F6", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.045, ymin = y0 + 0.04, ymax = y1 - 0.04, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(0.09, y1 - 0.17, label = toupper(label)), hjust = 0, size = 3.4, fontface = "bold", colour = C$text) +
    ggplot2::geom_text(ggplot2::aes(0.09, (y0 + y1) / 2 + 0.02, label = value, colour = col), hjust = 0, size = 9, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(0.09, y0 + 0.2, label = sub), hjust = 0, size = 3.1, colour = C$text, lineheight = 0.95) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, n), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 6, 6, 4))
}

lq_banner_gg <- function(text, col) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = text, colour = "white", fontface = "bold", size = 4.2) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(2, 14, 4, 10))
}

lq_pct <- function(x, n) if (n > 0) sprintf("%.0f%%", 100 * x / n) else "0%"

lq_map_panel <- function(res) {
  C <- REFUSAL_COL
  if (is.null(res$map)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No map for this selection", colour = C$muted) +
             ggplot2::theme_void())
  }
  suppressMessages(
    res$map +
      ggplot2::scale_fill_manual(values = LQ_BAND_COL, breaks = LQ_BANDS, labels = unname(LQ_BAND_LAB[LQ_BANDS]),
                                 name = "Rounds rated high", drop = FALSE,
                                 guide = ggplot2::guide_legend(nrow = 1, title.position = "left")) +
      ggplot2::labs(title = "Each district, by how often it was rated high") +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = ggplot2::element_text(size = 11.5, face = "bold", colour = C$text, hjust = 0, margin = ggplot2::margin(0, 0, 4, 0)),
        legend.position = "bottom", legend.title = ggplot2::element_text(size = 9, colour = C$text),
        legend.text = ggplot2::element_text(size = 8.8, colour = C$text), legend.key.size = grid::unit(0.42, "cm"),
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
    ggplot2::geom_text(data = rect, ggplot2::aes(x = (x0 + x1) / 2, y = row, label = lab, colour = txt), size = 3.1, fontface = "bold") +
    ggplot2::geom_text(data = by, ggplot2::aes(x = 101.5, y = row, label = format(n, big.mark = ",")), hjust = 0, size = 3.0, colour = C$text) +
    ggplot2::annotate("text", x = 101.5, y = max(by$row) + 0.75, label = "districts", hjust = 0, size = 2.9, colour = C$text) +
    ggplot2::annotate("text", x = LQ_THRESHOLD, y = max(by$row) + 1.55, label = paste0(LQ_THRESHOLD, "% threshold"), size = 3.1,
                      colour = C$navy, fontface = "bold") +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_y_continuous(breaks = by$row, labels = nm, expand = ggplot2::expansion(add = c(0.6, 1.9))) +
    ggplot2::scale_x_continuous(limits = c(0, 124), breaks = NULL, expand = c(0, 0)) +
    ggplot2::labs(title = "Districts per country (%)", x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.title = ggplot2::element_text(size = 11.5, face = "bold", colour = C$text, hjust = 0, margin = ggplot2::margin(0, 0, 4, 0)),
                   axis.text.y = ggplot2::element_text(size = 9.2, colour = C$text, hjust = 1, margin = ggplot2::margin(r = 4)),
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

lqas_exec_height <- function(res = NULL) 7.0

lqas_exec_gg <- function(res) {
  s <- lq_summary(res)
  if (is.null(s)) return(NULL)
  C <- REFUSAL_COL
  hd <- lq_header_gg(lq_headline(s),
                         paste0(lq_period_text(res$period_info), "  |  each district is judged by the percentage of its LQAS rounds rated \"high\""))
  cnt <- s$cnt
  cd <- lq_cards_gg(
    c("Districts analysed", "Mostly high", "Rarely high", "Never rated high"),
    c(format(s$n, big.mark = ","), format(as.integer(cnt[["80-100%"]]), big.mark = ","), format(as.integer(cnt[["0-25%"]]), big.mark = ","), format(s$never, big.mark = ",")),
    c(paste0("in ", s$k, if (s$k == 1) " country" else " countries",
             if (!is.null(res$summary_metrics$total_districts) && res$summary_metrics$total_districts > s$n)
               paste0("\nmatched to the map (of ", format(res$summary_metrics$total_districts, big.mark = ","), ")") else ""),
      paste0(lq_pct(cnt[["80-100%"]], s$n), " of districts\nhigh in 80% or more of rounds"),
      paste0(lq_pct(cnt[["0-25%"]], s$n), " of districts\nhigh in under 25% of rounds"),
      paste0("always rated high: ", format(s$always, big.mark = ","))),
    c(C$navy, unname(LQ_BAND_COL[["80-100%"]]), unname(LQ_BAND_COL[["0-25%"]]), unname(LQ_BAND_COL[["0-25%"]])))
  mid <- cowplot::plot_grid(lq_map_panel(res), lq_country_panel(s), cd, nrow = 1, rel_widths = c(1.3, 1.45, 0.8))
  bn <- lq_banner_gg(lq_banner_text(s), C$navy)
  cowplot::plot_grid(hd, mid, bn, ncol = 1, rel_heights = c(0.85, 5.65, 0.5))
}

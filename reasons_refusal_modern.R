# ============================================================
# Reasons Analysis -- "Refusals vs standard" view (modern)
# Share of sampled children missed through refusal (non-compliance), latest campaign with LQAS
# per country (or province / district), against the standard (at most 2%).
# Needs ggplot2 and cowplot.
# Input: the LQAS data (`dat`): country, province, district, response, roundNumber,
#        round_start_date, total_sampled, r_non_compliance
# ============================================================

REFUSAL_STANDARD <- 2
REFUSAL_COL <- list(green = "#3D8B6E", red = "#B83B5E", amber = "#E3B23C", navy = "#14454F",
                    text = "#22313A", muted = "#66737A")

# Same filters as the other Reasons Analysis views (block, country, province, district, period)
refusal_filter_lqas <- function(data, x_months = NULL, specific_month = NULL, specific_year = NULL,
                                block_selection = NULL, country_selection = NULL,
                                province_selection = NULL, district_selection = NULL,
                                block_definition = NULL) {
  d <- data
  if (!is.null(block_definition) && !is.null(block_selection) && block_selection != "All")
    d <- d[d$country %in% block_definition[[block_selection]], , drop = FALSE]
  if (!is.null(country_selection) && length(country_selection) > 0)   d <- d[d$country  %in% country_selection,  , drop = FALSE]
  if (!is.null(province_selection) && length(province_selection) > 0) d <- d[d$province %in% province_selection, , drop = FALSE]
  if (!is.null(district_selection) && length(district_selection) > 0) d <- d[d$district %in% district_selection, , drop = FALSE]
  d$date <- suppressWarnings(as.Date(d$round_start_date))
  d <- d[!is.na(d$date), , drop = FALSE]
  period_label <- "All time"
  if (nrow(d) && !is.null(x_months)) {
    cutoff <- seq(max(d$date), by = paste0("-", x_months, " months"), length.out = 2)[2]
    d <- d[d$date >= cutoff, , drop = FALSE]
    period_label <- paste0("Last ", x_months, " months")
  } else if (nrow(d) && !is.null(specific_month) && !is.null(specific_year)) {
    start <- as.Date(sprintf("%04d-%02d-01", as.integer(specific_year), as.integer(specific_month)))
    end <- seq(start, by = "1 month", length.out = 2)[2] - 1
    d <- d[d$date >= start & d$date <= end, , drop = FALSE]
    period_label <- paste(month.name[as.integer(specific_month)], specific_year)
  }
  attr(d, "period_label") <- period_label
  d
}

refusal_nice <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (toupper(trimws(s)) == "CAR") return("Central African Republic")
    s2 <- gsub("\\b([a-z])", "\\U\\1", tolower(s), perl = TRUE)
    gsub("(?<= )(Of|The|And|De|Du|Des)(?= )", "\\L\\1", s2, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

# One row per country (or province / district): latest campaign with LQAS and its refusal rate
refusal_summary <- function(filtered, geo_level = "country", period_label = "All time", std = REFUSAL_STANDARD, max_rows = 30) {
  geo_col <- switch(geo_level, province = "province", district = "district", "country")
  need <- c(geo_col, "response", "total_sampled", "r_non_compliance", "date")
  if (!nrow(filtered) || !all(need %in% names(filtered))) return(NULL)
  rn <- if ("roundNumber" %in% names(filtered)) as.character(filtered$roundNumber) else rep(NA_character_, nrow(filtered))
  d <- data.frame(geo = filtered[[geo_col]], country = filtered$country, response = as.character(filtered$response), rnd = rn,
                  date = filtered$date, sampled = suppressWarnings(as.numeric(filtered$total_sampled)),
                  nc = suppressWarnings(as.numeric(filtered$r_non_compliance)), stringsAsFactors = FALSE)
  d <- d[!is.na(d$geo) & !is.na(d$nc) & !is.na(d$sampled) & d$sampled > 0, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  key <- paste(d$geo, d$response, d$rnd, sep = "\r")
  camp <- do.call(rbind, lapply(split(seq_len(nrow(d)), key), function(i)
    data.frame(geo = d$geo[i[1]], country = d$country[i[1]], response = d$response[i[1]], rnd = d$rnd[i[1]],
               date = max(d$date[i]), sampled = sum(d$sampled[i]), nc = sum(d$nc[i]), stringsAsFactors = FALSE)))
  camp <- camp[order(camp$geo, camp$date), , drop = FALSE]
  latest <- camp[!duplicated(camp$geo, fromLast = TRUE), , drop = FALSE]
  latest$rate <- 100 * latest$nc / latest$sampled
  latest$name <- if (geo_col == "country") refusal_nice(latest$geo) else as.character(latest$geo)
  rl <- toupper(gsub("[^A-Za-z0-9]", "", latest$rnd))
  latest$round_txt <- ifelse(is.na(latest$rnd) | rl == "", "", paste0("Rnd", sub("^RND?", "", rl)))
  latest <- latest[order(-latest$rate), , drop = FALSE]
  n_all <- nrow(latest)
  shown <- if (n_all > max_rows) latest[seq_len(max_rows), , drop = FALSE] else latest
  list(data = shown, n_all = n_all, n_ok = sum(latest$rate <= std), n_shown = nrow(shown),
       highest = latest[1, , drop = FALSE], std = std, geo_level = geo_level, period_label = period_label)
}

refusal_unit_word <- function(res) switch(res$geo_level, province = "provinces", district = "districts", "countries")

refusal_title <- function() "Refusals under control"
refusal_subtitle <- function(res)
  paste0("Share of sampled children missed through refusal, latest campaign with LQAS (standard: at most ",
         res$std, "%)  |  ", res$period_label)
refusal_banner_text <- function(res) {
  if (res$n_ok == res$n_all) "Refusals are not a driver of missed children in these campaigns."
  else paste0("Some ", refusal_unit_word(res), " are above the line: they need a refusal-resolution plan.")
}

refusal_chart_gg <- function(res) {
  C <- REFUSAL_COL
  d <- res$data
  d$name <- factor(d$name, levels = rev(unique(d$name)))
  d$ok <- d$rate <= res$std
  xmax <- max(res$std * 1.6, max(d$rate) * 1.25)
  ggplot2::ggplot(d, ggplot2::aes(rate, name, fill = ok)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", rate)), hjust = -0.15, fontface = "bold", colour = C$text, size = 3.4) +
    ggplot2::geom_vline(xintercept = res$std, colour = C$red, linetype = "dashed", linewidth = 0.7) +
    ggplot2::annotate("text", x = res$std, y = -Inf, label = paste0("standard: ", res$std, "%"), hjust = -0.1, vjust = -0.8,
                      colour = C$red, size = 3.3) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = C$green, `FALSE` = C$red), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, xmax), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#E9EEEA"),
                   axis.text.y = ggplot2::element_text(colour = C$text, size = 10),
                   axis.text.x = ggplot2::element_text(colour = C$muted),
                   plot.margin = ggplot2::margin(6, 14, 6, 6),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# the two summary cards, drawn as a small ggplot so they can sit next to the chart in one image
refusal_cards_gg <- function(res) {
  C <- REFUSAL_COL
  hi <- res$highest
  all_ok <- res$n_ok == res$n_all
  cd <- data.frame(
    y0 = c(1.04, 0.0), y1 = c(2.0, 0.96),
    label = c("Within the standard", "Highest refusal rate"),
    value = c(paste0(res$n_ok, " / ", res$n_all), sprintf("%.1f%%", hi$rate)),
    sub = c(paste0("refusals at most ", res$std, "% of sampled children"),
            paste0(hi$name, if (nzchar(hi$round_txt)) paste0(", ", hi$round_txt) else "")),
    col = c(if (all_ok) C$green else C$amber, if (hi$rate <= res$std) C$navy else C$red),
    stringsAsFactors = FALSE)
  ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.98, ymin = y0 + 0.03, ymax = y1 - 0.03), fill = "#F7F9F6", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.04, ymin = y0 + 0.03, ymax = y1 - 0.03, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(0.09, y1 - 0.26, label = toupper(label)), hjust = 0, size = 3.2, fontface = "bold", colour = C$text) +
    ggplot2::geom_text(ggplot2::aes(0.09, (y0 + y1) / 2 - 0.02, label = value, colour = col), hjust = 0, size = 9, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(0.09, y0 + 0.2, label = sub), hjust = 0, size = 3, colour = C$muted) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(6, 6, 6, 4))
}

refusal_header_gg <- function(res) {
  C <- REFUSAL_COL
  lg <- data.frame(x = c(7.6, 10.75), label = c(paste0("Within the standard (", res$std, "% or less)"), "Above the standard"),
                   col = c(C$green, C$red), stringsAsFactors = FALSE)
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.02, y = 0.72, label = refusal_title(), hjust = 0, size = 5.3,
                      fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.2, label = refusal_subtitle(res), hjust = 0, size = 3.2, colour = C$muted) +
    ggplot2::geom_tile(data = lg, ggplot2::aes(x, 0.72, fill = col), width = 0.22, height = 0.3) +
    ggplot2::geom_text(data = lg, ggplot2::aes(x + 0.2, 0.72, label = label), hjust = 0, size = 3.2, colour = C$text) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(6, 14, 0, 10))
}

refusal_banner_gg <- function(res) {
  C <- REFUSAL_COL
  col <- if (res$n_ok == res$n_all) C$green else C$amber
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = refusal_banner_text(res), colour = "white", fontface = "bold", size = 4.4) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(2, 14, 4, 10))
}

refusal_foot_gg <- function(res) {
  note <- paste0("Refusal = children not vaccinated because of non-compliance in the LQAS sample, as a share of children sampled. ",
                 "Only the latest campaign with LQAS results is shown per ", sub("ies$", "y", sub("s$", "", refusal_unit_word(res))), ".",
                 if (res$n_all > res$n_shown) paste0(" The ", res$n_shown, " highest of ", res$n_all, " are shown.") else "",
                 "  Source: LQAS repository © WHO AFRO GIS CENTRE, generated on ", format(Sys.Date(), "%Y-%m-%d"))
  note <- paste(strwrap(note, 175), collapse = "\n")
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.005, y = 0.5, label = note, hjust = 0, size = 2.7, colour = REFUSAL_COL$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(0, 14, 4, 10))
}

# height in inches for the whole image
refusal_height <- function(res) max(4.8, 0.27 * res$n_shown + 3.3)

refusal_full_gg <- function(res) {
  hd <- refusal_header_gg(res); ch <- refusal_chart_gg(res); cd <- refusal_cards_gg(res)
  bn <- refusal_banner_gg(res); ft <- refusal_foot_gg(res)
  body_h <- max(2.2, 0.27 * res$n_shown + 0.6)
  # cowplot keeps every block flush with the image edges (patchwork would align them to the chart panel)
  mid <- cowplot::plot_grid(ch, cd, nrow = 1, rel_widths = c(2.15, 1))
  cowplot::plot_grid(hd, mid, bn, ft, ncol = 1, rel_heights = c(0.85, body_h, 0.5, 0.45))
}

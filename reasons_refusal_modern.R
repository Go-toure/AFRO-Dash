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
                    blue = "#2F7F9E", text = "#22313A", muted = "#66737A")

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
                 "  Source: LQAS repository ", intToUtf8(169), " WHO AFRO GIS CENTRE, generated on ", format(Sys.Date(), "%Y-%m-%d"))
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

# ============================================================
# Same modern look for the other Reasons Analysis tabs
# (Combined Overview, Absence Reasons, Traditional Reasons)
# These have no standard, so they show who/what drives the reasons
# ============================================================
REASON_PAL <- c("#1F7A8C", "#E08A3C", "#7A5C99", "#B83B5E", "#3D8B6E", "#E3B23C")
REASON_OTHER_COL <- "#B7C0C4"
REASON_KIND_COL <- c(traditional = "#1F7A8C", nc = "#7A5C99", absence = "#E08A3C")
REASON_KIND_NAME <- c(traditional = "Traditional reasons", nc = "Non-compliance reasons", absence = "Absence reasons")

.reason_trad_labels <- c(r_non_compliance = "Non compliance", r_childabsent = "Child absent",
  r_child_is_a_visitor = "Child is a visitor", r_child_was_asleep = "Child was asleep",
  r_house_not_visited = "House not visited", r_vaccinated_but_not_fm = "Vaccinated but not FM",
  r_childnotborn = "Child not born", r_security = "Security", other_r = "Other")

reason_pretty <- function(x) {
  special <- c(others = "Other", other = "Other", covid_19 = "COVID-19", nopvconcern = "No PV concern",
               poliofree = "Polio free", no_felt_need = "No felt need", vaccines_safety = "Vaccine safety",
               no_care_giver_consent = "No caregiver consent", too_many_rnd = "Too many rounds",
               religious_cultural = "Religious / cultural", in_playground = "In playground")
  vapply(x, function(s) {
    if (s %in% names(special)) return(unname(special[s]))
    s <- gsub("_", " ", s)
    paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  }, character(1), USE.NAMES = FALSE)
}

reason_columns_for <- function(filtered, kind) {
  nm <- names(filtered); lo <- tolower(nm)
  if (kind == "traditional") {
    idx <- match(names(.reason_trad_labels), lo); ok <- !is.na(idx)
    return(list(cols = nm[idx[ok]], labels = unname(.reason_trad_labels)[ok]))
  }
  pre <- if (kind == "absence") "^abs_reason_" else "^nc_reason_"
  cols <- nm[grepl(pre, lo)]
  list(cols = cols, labels = reason_pretty(sub(pre, "", tolower(cols))))
}

reason_matrix <- function(filtered, rc) {
  m <- as.data.frame(lapply(rc$cols, function(c) suppressWarnings(as.numeric(filtered[[c]]))))
  m[is.na(m)] <- 0
  names(m) <- make.unique(rc$labels)
  as.matrix(m)
}

modern_cards_gg <- function(label, value, sub, col) {
  C <- REFUSAL_COL
  n <- length(label)
  cd <- data.frame(y1 = n:1, y0 = (n:1) - 1, label = label, value = value, sub = sub, col = col, stringsAsFactors = FALSE)
  ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.98, ymin = y0 + 0.04, ymax = y1 - 0.04), fill = "#F7F9F6", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0.02, xmax = 0.04, ymin = y0 + 0.04, ymax = y1 - 0.04, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(0.09, y1 - 0.2, label = toupper(label)), hjust = 0, size = 3, fontface = "bold", colour = C$text) +
    ggplot2::geom_text(ggplot2::aes(0.09, (y0 + y1) / 2 - 0.02, label = value, colour = col), hjust = 0, size = 8, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(0.09, y0 + 0.19, label = sub), hjust = 0, size = 2.9, colour = C$muted, lineheight = 0.95) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, n), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 6, 6, 4))
}

modern_header_gg <- function(title, subtitle) {
  C <- REFUSAL_COL
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.02, y = 0.72, label = title, hjust = 0, size = 5.3, fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.2, label = subtitle, hjust = 0, size = 3.2, colour = C$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(6, 14, 0, 10))
}

modern_banner_gg <- function(text, col) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col) +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = text, colour = "white", fontface = "bold", size = 4.2) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(2, 14, 4, 10))
}

modern_foot_gg <- function(note) {
  note <- paste(strwrap(paste0(note, "  Source: LQAS repository ", intToUtf8(169), " WHO AFRO GIS CENTRE, generated on ", format(Sys.Date(), "%Y-%m-%d")), 175), collapse = "\n")
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.005, y = 0.5, label = note, hjust = 0, size = 2.7, colour = REFUSAL_COL$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA), plot.margin = ggplot2::margin(0, 14, 4, 10))
}


# ---------- executive view: WHY (reasons) and WHERE (countries), one message per page ----------
reason_unit_word <- function(geo_level) switch(geo_level, province = "provinces", district = "districts", "countries")

reason_colours <- function(labels) {
  named <- labels[tolower(labels) != "other"]
  cols <- stats::setNames(rep(REASON_OTHER_COL, length(labels)), labels)
  cols[named] <- REASON_PAL[(seq_along(named) - 1) %% length(REASON_PAL) + 1]
  cols
}

reason_lower <- function(x) gsub("\\bfm\\b", "FM", ifelse(grepl("^[A-Z]{2,}", x), x, tolower(x)))
reason_join <- function(a, b) paste0(a, " and ", reason_lower(b))

# kind = "absence" or "traditional"
reason_profile <- function(filtered, kind, geo_level = "country", period_label = "All time", n_reasons = 6, n_geo = 8) {
  geo_col <- switch(geo_level, province = "province", district = "district", "country")
  rc <- reason_columns_for(filtered, kind)
  if (!nrow(filtered) || !length(rc$cols) || !geo_col %in% names(filtered)) return(NULL)
  m <- reason_matrix(filtered, rc)
  g <- as.character(filtered[[geo_col]]); keep <- !is.na(g)
  m <- m[keep, , drop = FALSE]; g <- g[keep]
  if (!nrow(m)) return(NULL)
  agg <- rowsum(m, g); tot <- rowSums(agg)
  agg <- agg[tot > 0, , drop = FALSE]; tot <- tot[tot > 0]
  if (!nrow(agg)) return(NULL)
  ov <- colSums(agg); ov <- sort(ov[ov > 0], decreasing = TRUE)
  total <- sum(ov)
  why <- data.frame(label = names(ov), n = as.numeric(ov), pct = 100 * as.numeric(ov) / total, stringsAsFactors = FALSE)
  named <- why[tolower(why$label) != "other", , drop = FALSE]
  if (!nrow(named)) named <- why
  why <- why[seq_len(min(n_reasons, nrow(why))), , drop = FALSE]
  ord <- names(sort(tot, decreasing = TRUE)); n_all <- length(ord)
  show <- ord[seq_len(min(n_geo, n_all))]
  lead <- vapply(show, function(k) { v <- agg[k, ]; v2 <- v[tolower(names(v)) != "other"]; if (!length(v2) || max(v2) <= 0) v2 <- v; names(v2)[which.max(v2)] }, character(1))
  lead_pct <- vapply(seq_along(show), function(i) 100 * agg[show[i], lead[i]] / tot[[show[i]]], numeric(1))
  nice <- if (geo_col == "country") refusal_nice(show) else show
  where <- data.frame(name = nice, n = as.numeric(tot[show]), share = 100 * as.numeric(tot[show]) / total,
                      lead = lead, lead_pct = lead_pct, stringsAsFactors = FALSE)
  top3 <- where[seq_len(min(3, nrow(where))), , drop = FALSE]
  list(why = why, where = where, kind = kind, total = total, n_all = n_all, geo_level = geo_level, period_label = period_label,
       A = named$label[1], A_pct = named$pct[1], B = if (nrow(named) > 1) named$label[2] else NA_character_,
       AB_pct = sum(named$pct[seq_len(min(2, nrow(named)))]), top3 = top3, top3_share = sum(top3$share),
       unit = reason_unit_word(geo_level), colours = reason_colours(unique(c(why$label, where$lead))))
}

reason_height <- function(p = NULL) 6.4

reason_why_gg <- function(p, title_txt) {
  C <- REFUSAL_COL
  d <- p$why; d$label <- factor(d$label, levels = rev(d$label)); d$col <- p$colours[as.character(d$label)]
  xmax <- max(d$pct) * 1.5
  ggplot2::ggplot(d, ggplot2::aes(pct, label, fill = col)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, fontface = "bold", colour = C$text, size = 4) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, xmax), expand = c(0, 0)) +
    ggplot2::labs(title = title_txt, subtitle = paste0(format(p$total, big.mark = ",", trim = TRUE), " reasons recorded"), x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = C$text, size = 10.5),
                   plot.title = ggplot2::element_text(face = "bold", colour = C$navy, size = 12.5),
                   plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9),
                   plot.title.position = "plot", plot.margin = ggplot2::margin(6, 10, 6, 6),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

reason_where_gg <- function(p, title_txt) {
  C <- REFUSAL_COL
  d <- p$where; d$name <- factor(d$name, levels = rev(d$name)); d$col <- p$colours[d$lead]
  xmax <- max(d$n) * 3.5
  d$lab <- paste0(format(d$n, big.mark = ",", trim = TRUE), " (", sprintf("%.0f%%", d$share), ")")
  ggplot2::ggplot(d, ggplot2::aes(n, name, fill = col)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = -0.1, fontface = "bold", colour = C$text, size = 3.6) +
    ggplot2::geom_text(ggplot2::aes(x = max(n) * 1.95, label = paste0(lead, " ", sprintf("%.0f%%", lead_pct))), hjust = 0, colour = C$muted, size = 3.4) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, xmax), expand = c(0, 0)) +
    ggplot2::labs(title = title_txt, subtitle = "Reasons recorded (share of all)  |  leading reason", x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = C$text, size = 10.5),
                   plot.title = ggplot2::element_text(face = "bold", colour = C$navy, size = 12.5),
                   plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9),
                   plot.title.position = "plot", plot.margin = ggplot2::margin(6, 10, 6, 6),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

reason_full_gg <- function(p) {
  unit1 <- sub("ies$", "y", sub("s$", "", p$unit))
  noun <- if (p$kind == "absence") "absences" else "reasons for missed children"
  head_txt <- if (is.na(p$B)) paste0(p$A, " is the leading reason: ", sprintf("%.0f%%", p$A_pct), " of recorded ", noun)
              else paste0(reason_join(p$A, p$B), " explain ", sprintf("%.0f%%", p$AB_pct), " of recorded ", noun)
  sub_txt <- paste0(if (p$kind == "absence") "Why children were absent during LQAS" else "Why children were missed in LQAS",
                    "  |  ", p$period_label, "  |  ", p$n_all, " ", if (p$n_all == 1) unit1 else p$unit)
  hd <- modern_header_gg(head_txt, sub_txt)
  why <- reason_why_gg(p, "WHY: reasons recorded")
  wh <- reason_where_gg(p, paste0("WHERE: top ", nrow(p$where), " ", p$unit))
  t3 <- paste(p$top3$name, collapse = ", ")
  cd <- modern_cards_gg(
    c("Reasons recorded", "Leading named reason", paste0("Concentration, top ", nrow(p$top3))),
    c(format(p$total, big.mark = ",", trim = TRUE), sprintf("%.0f%%", p$A_pct), sprintf("%.0f%%", p$top3_share)),
    c(paste0("across ", p$n_all, " ", if (p$n_all == 1) unit1 else p$unit), tolower(p$A), paste(strwrap(t3, 28), collapse = "\n")),
    c(REFUSAL_COL$navy, unname(p$colours[p$A]), REFUSAL_COL$blue))
  act <- paste0("Priority: ", if (is.na(p$B)) reason_lower(p$A) else reason_join(reason_lower(p$A), p$B), ", starting with ", t3, ".")
  bn <- modern_banner_gg(act, REFUSAL_COL$navy)
  ft <- modern_foot_gg(paste0("Reasons are counted over all LQAS campaigns in the period. Colours show the leading reason. \"Other\" is shown but never named as the leading reason."))
  mid <- cowplot::plot_grid(why, wh, cd, nrow = 1, rel_widths = c(1.0, 1.9, 0.8))
  cowplot::plot_grid(hd, mid, bn, ft, ncol = 1, rel_heights = c(0.85, 4.4, 0.5, 0.45))
}

# Combined tab: one page, three reason types, the leader of each highlighted
reason_pooled <- function(filtered, kind, top_k = 5) {
  rc <- reason_columns_for(filtered, kind)
  if (!nrow(filtered) || !length(rc$cols)) return(NULL)
  v <- colSums(reason_matrix(filtered, rc)); v <- sort(v[v > 0], decreasing = TRUE)
  if (!length(v)) return(NULL)
  d <- data.frame(label = names(v), n = as.numeric(v), pct = 100 * as.numeric(v) / sum(v), stringsAsFactors = FALSE)
  nm <- d[tolower(d$label) != "other", , drop = FALSE]
  list(data = d[seq_len(min(top_k, nrow(d))), , drop = FALSE], total = sum(v), lead = if (nrow(nm)) nm[1, ] else d[1, ])
}

reason_panel_gg <- function(pl, kind) {
  C <- REFUSAL_COL; kc <- REASON_KIND_COL[[kind]]
  d <- pl$data; d$is_lead <- d$label == pl$lead$label
  d$label <- factor(d$label, levels = rev(d$label))
  d$col <- ifelse(d$is_lead, kc, scales::alpha(kc, 0.4))
  xmax <- max(d$pct) * 1.5
  ggplot2::ggplot(d, ggplot2::aes(pct, label, fill = col)) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", pct)), hjust = -0.15, fontface = "bold", colour = C$text, size = 3.8) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, xmax), expand = c(0, 0)) +
    ggplot2::labs(title = REASON_KIND_NAME[[kind]], subtitle = paste0(format(pl$total, big.mark = ",", trim = TRUE), " reasons recorded"), x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = C$text, size = 10),
                   plot.title = ggplot2::element_text(face = "bold", colour = kc, size = 12.5),
                   plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9),
                   plot.title.position = "plot", plot.margin = ggplot2::margin(6, 10, 6, 6),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

reason_combined_height <- function(types = NULL) 6.4

reason_combined_gg <- function(filtered, types, geo_level, period_label, refusal = NULL) {
  pools <- stats::setNames(lapply(types, function(k) reason_pooled(filtered, k)), types)
  pools <- pools[!vapply(pools, is.null, logical(1))]
  if (!length(pools)) return(NULL)
  panels <- lapply(names(pools), function(k) reason_panel_gg(pools[[k]], k))
  grid <- cowplot::plot_grid(plotlist = panels, nrow = 1)
  ok <- is.null(refusal) || refusal$n_ok == refusal$n_all
  unit <- if (is.null(refusal)) reason_unit_word(geo_level) else refusal_unit_word(refusal)
  ld <- function(k) if (is.null(pools[[k]])) NULL else pools[[k]]$lead
  tr <- ld("traditional"); ab <- ld("absence"); nc <- ld("nc")
  cards_l <- c("Refusals within standard"); cards_v <- if (is.null(refusal)) "n/a" else paste0(refusal$n_ok, " / ", refusal$n_all)
  cards_s <- if (is.null(refusal)) "no LQAS refusal data" else paste0(unit, " at most ", REFUSAL_STANDARD, "%")
  cards_c <- if (ok) REFUSAL_COL$green else REFUSAL_COL$amber
  main <- if (!is.null(tr)) "traditional" else names(pools)[1]
  cards_l <- c(cards_l, "Reasons recorded"); cards_v <- c(cards_v, format(pools[[main]]$total, big.mark = ",", trim = TRUE))
  cards_s <- c(cards_s, "missed children, all reasons"); cards_c <- c(cards_c, REFUSAL_COL$navy)
  lead_main <- pools[[main]]$lead
  cards_l <- c(cards_l, "Leading named reason"); cards_v <- c(cards_v, sprintf("%.0f%%", lead_main$pct))
  cards_s <- c(cards_s, tolower(lead_main$label)); cards_c <- c(cards_c, REASON_KIND_COL[[main]])
  cd <- modern_cards_gg(cards_l, cards_v, cards_s, cards_c)
  head_txt <- paste0(if (is.null(refusal)) "" else if (ok) "Refusals are under control; " else "Refusals need attention; ",
                     if (!is.null(ab)) paste0(reason_lower(ab$label), " is the main reason children are absent") else paste0(reason_lower(lead_main$label), " leads the reasons for missed children"))
  hd <- modern_header_gg(head_txt, paste0("Why children were missed: all reason types, LQAS  |  ", period_label))
  parts <- c(if (!is.null(tr)) paste0("overall ", reason_lower(tr$label), " (", sprintf("%.0f%%", tr$pct), ")"),
             if (!is.null(ab)) paste0("absence ", reason_lower(ab$label), " (", sprintf("%.0f%%", ab$pct), ")"),
             if (!is.null(nc)) paste0("refusal ", reason_lower(nc$label), " (", sprintf("%.0f%%", nc$pct), ")"))
  bn <- modern_banner_gg(paste0("Leading reasons: ", paste(parts, collapse = "; "), "."), if (ok) REFUSAL_COL$green else REFUSAL_COL$amber)
  ft <- modern_foot_gg("Top five reasons of each reason type, as a share of the reasons recorded in that type, over all LQAS campaigns in the period. The leading named reason is highlighted.")
  mid <- cowplot::plot_grid(grid, cd, nrow = 1, rel_widths = c(3.3, 1))
  cowplot::plot_grid(hd, mid, bn, ft, ncol = 1, rel_heights = c(0.85, 4.4, 0.5, 0.45))
}

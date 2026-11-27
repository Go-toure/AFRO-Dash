# im_slides_modern.R -- static IM figures with the IM Overview definitions (cards, districts at 95% by country and month, operational failure, social mobilisation).
# Used by the weekly deck job (SIA_data_operations_and_analytics/tools/weekly_deck.R) and by the Generate Slides tab. Needs im_overview_module.R and im_slides.R loaded first.
# Needs the dashboard definitions loaded (im_overview_module.R helpers, im_slides.R) and a prepared IM data frame (lower-case names).
IMW <- list(green = "#3D8B6E", gold = "#E3B23C", red = "#B83B5E", blue = "#2F7F9E", navy = "#005C97", ink = "#1A2637", muted = "#66737A", purple = "#7A5C99", teal = "#1F7A8C", orange = "#E08A3C")
imw_num <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
imw_card <- function(value, label, sub = "", col = IMW$navy, vsize = 8.5) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col, alpha = 0.08, colour = NA) +
    ggplot2::annotate("rect", xmin = 0, xmax = 0.022, ymin = 0, ymax = 1, fill = col, colour = NA) +
    ggplot2::annotate("text", x = 0.06, y = 0.66, label = value, hjust = 0, fontface = "bold", size = vsize, colour = col) +
    ggplot2::annotate("text", x = 0.06, y = 0.37, label = label, hjust = 0, fontface = "bold", size = 3.3, colour = IMW$ink) +
    ggplot2::annotate("text", x = 0.06, y = 0.15, label = sub, hjust = 0, size = 2.7, colour = IMW$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(3, 4, 3, 4))
}
imw_head <- function(title, sub) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0.68, label = title, hjust = 0, fontface = "bold", size = 6, colour = IMW$ink) +
    ggplot2::annotate("text", x = 0, y = 0.22, label = sub, hjust = 0, size = 3.2, colour = IMW$muted) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(6, 10, 2, 10))
}
imw_banner <- function(txt, col = IMW$navy) {
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col, colour = NA) +
    ggplot2::annotate("text", x = 0.012, y = 0.5, label = txt, hjust = 0, fontface = "bold", size = 3.6, colour = "white") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(2, 10, 6, 10))
}
imw_row <- function(...) cowplot::plot_grid(plotlist = list(...), nrow = 1)
imw_period <- function(df) {
  dc <- intersect(c("round_start_date", "start_date_im_end", "start_date"), names(df))[1]
  d <- suppressWarnings(as.Date(df[[dc]])); d <- d[!is.na(d)]
  if (length(d)) paste(format(min(d), "%b %Y"), "to", format(max(d), "%b %Y")) else ""
}

# slide 6: the intelligence cards
imw_cards_fig <- function(df, period_txt) {
  dfr <- ims_with_risk(df)
  tp <- sum(df$u5_present, na.rm = TRUE); tf <- sum(df$u5_fm, na.rm = TRUE)
  aw <- if ("awareness_rate" %in% names(dfr)) mean(dfr$awareness_rate, na.rm = TRUE) else NA_real_
  key <- intersect(c("country", "response", "vaccine_type", "roundnumber"), names(df))
  n_round <- nrow(unique(df[, key, drop = FALSE]))
  hh <- sum(as.numeric(df$number_of_hh_visited), na.rm = TRUE)
  top <- imw_row(imw_card(imw_num(hh), "HOUSEHOLDS VISITED", "households checked by the monitors", IMW$navy, 9),
                 imw_card(imw_num(tf), "CHILDREN FOUND VACCINATED", paste0("of ", imw_num(tp), " children checked"), IMW$green, 9),
                 imw_card(imw_num(max(tp - tf, 0)), "MISSED CHILDREN", paste0(sprintf("%.1f%%", 100 * max(tp - tf, 0) / max(tp, 1)), " of children checked"), IMW$red, 9),
                 imw_card(if (is.na(aw)) "-" else sprintf("%.1f%%", 100 * aw), "CAREGIVER AWARENESS", "target 80%", IMW$gold, 9))
  small <- imw_row(imw_card(imw_num(n_round), "Campaign rounds", "", IMW$navy, 6.5), imw_card(imw_num(length(unique(df$country))), "Countries", "", IMW$navy, 6.5),
                   imw_card(imw_num(nrow(unique(df[, intersect(c("country", "province"), names(df)), drop = FALSE]))), "Provinces", "", IMW$navy, 6.5),
                   imw_card(imw_num(nrow(unique(df[, intersect(c("country", "province", "district"), names(df)), drop = FALSE]))), "Districts", "", IMW$navy, 6.5))
  cowplot::plot_grid(imw_head("AFRO Regional Independent Monitoring Intelligence", paste0(period_txt, "  |  ", imw_period(df), "  |  generated ", format(Sys.Date(), "%d %b %Y"))),
                     top, small, ncol = 1, rel_heights = c(0.9, 2.2, 1.1)) -> g
  g
}

# slide 7: share of districts at 95% coverage, by country and month (the box view, same table as the module)
imw_cv95_fig <- function(df, period_txt) {
  dc <- intersect(c("round_start_date", "start_date_im_end", "start_date"), names(df))[1]
  uid <- intersect(c("country", "province", "district"), names(df))
  d2 <- df
  d2$cam_date <- as.Date(d2[[dc]])
  d2 <- d2[!is.na(d2$cam_date) & !is.na(d2$country), , drop = FALSE]
  d2$period_key <- paste0(lubridate::year(d2$cam_date), sprintf("%02d", lubridate::month(d2$cam_date)))
  d2$district_id <- do.call(paste, c(as.list(d2[uid]), list(sep = "|")))
  a <- dplyr::summarise(dplyr::group_by(d2, country, period_key, district_id), dist_cv = sum(as.numeric(u5_fm), na.rm = TRUE) / pmax(sum(as.numeric(u5_present), na.rm = TRUE), 1), .groups = "drop")
  tbl <- dplyr::summarise(dplyr::group_by(a, country, period_key), n_dist = dplyr::n_distinct(district_id), n_cv95 = sum(dist_cv >= 0.95), prop_cv95 = n_cv95 / n_dist, .groups = "drop")
  tbl <- as.data.frame(tbl)
  K <- IM_CV95_COL
  v <- round(100 * tbl$prop_cv95); v <- v[!is.na(v)]; n <- length(v)
  n95 <- sum(v >= 95); nmid <- sum(v >= 80 & v < 95); nlow <- sum(v < 80)
  pct <- function(k) paste0(round(100 * k / max(n, 1)), "% of campaigns")
  cards <- imw_row(imw_card(paste0(n95, " of ", n), "Campaigns at 95%+", paste0("in at least 95% of districts (", pct(n95), ")"), K$green),
                   imw_card(as.character(nmid), "Close to target", paste0("80 to 94% of districts (", pct(nmid), ")"), K$gold),
                   imw_card(as.character(nlow), "Below 80%", paste0("of districts at 95% coverage (", pct(nlow), ")"), K$red))
  hh <- max(4, 0.52 * max(table(im_cv95_stack_data(tbl)$mlab)) + 2.2)
  main <- im_cv95_stack_gg(tbl)
  list(plot = cowplot::plot_grid(imw_head("Campaign coverage by country and month", paste0(period_txt, "  |  share of each country's monitored districts at 95% coverage or more  |  green = 95% or more, amber = 80 to 94%, red = below 80%")), cards, main, ncol = 1, rel_heights = c(0.8, 1.25, hh)),
       height = hh + 2.4)
}

# slide 8: operational failure profile
imw_opfail_fig <- function(df, period_txt) {
  needed <- c("r_non_fm_absent", "r_non_fm_nc", "r_non_fm_hh_notvisited", "r_non_fm_hh_notrevisited")
  if (!all(needed %in% names(df))) stop("missing r_non_fm_* columns")
  grp <- intersect(c("country", "province", "district", "response", "roundnumber"), names(df))
  mc <- if ("missed_child" %in% names(df)) "missed_child" else NA
  op <- dplyr::summarise(dplyr::group_by(df, dplyr::across(dplyr::all_of(grp))),
    missed_child = if (!is.na(mc)) sum(.data[[mc]], na.rm = TRUE) else sum(pmax(as.numeric(u5_present) - as.numeric(u5_fm), 0), na.rm = TRUE),
    absent = sum(r_non_fm_absent, na.rm = TRUE), non_compliance = sum(r_non_fm_nc, na.rm = TRUE),
    house_not_visited = sum(r_non_fm_hh_notvisited, na.rm = TRUE), house_not_revisited = sum(r_non_fm_hh_notrevisited, na.rm = TRUE), .groups = "drop")
  op <- dplyr::mutate(op, type = dplyr::case_when(
    house_not_visited >= house_not_revisited & house_not_visited >= absent & house_not_visited >= non_compliance & house_not_visited > 0 ~ "Team did not visit households",
    house_not_revisited >= house_not_visited & house_not_revisited >= absent & house_not_revisited >= non_compliance & house_not_revisited > 0 ~ "Weak revisit / supervision failure",
    absent >= house_not_visited & absent >= house_not_revisited & absent >= non_compliance & absent > 0 ~ "Population absence / timing issue",
    non_compliance >= house_not_visited & non_compliance >= house_not_revisited & non_compliance >= absent & non_compliance > 0 ~ "Community resistance / refusal",
    missed_child == 0 ~ "No operational failure detected", TRUE ~ "Mixed / other operational issue"))
  tbl <- dplyr::arrange(dplyr::filter(dplyr::summarise(dplyr::group_by(dplyr::filter(op, type != "No operational failure detected"), type), total = sum(missed_child, na.rm = TRUE), .groups = "drop"), total > 0), dplyr::desc(total))
  if (!nrow(tbl)) stop("no operational failure")
  tbl$type <- factor(tbl$type, levels = rev(tbl$type))
  g <- ggplot2::ggplot(tbl, ggplot2::aes(total, type)) +
    ggplot2::geom_col(fill = IMW$navy, width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = imw_num(total)), hjust = -0.15, size = 3.6, colour = IMW$ink) +
    ggplot2::scale_x_continuous(labels = function(x) imw_num(x), expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::labs(x = "Missed children", y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_line(colour = "#E8ECF0"),
                   axis.text.y = ggplot2::element_text(colour = IMW$ink, size = 12), axis.text.x = ggplot2::element_text(colour = IMW$muted), axis.title.x = ggplot2::element_text(colour = IMW$muted),
                   plot.margin = ggplot2::margin(6, 16, 8, 10), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  lead <- as.character(tbl$type[which.max(tbl$total)])
  cowplot::plot_grid(imw_head("Operational failure profile", paste0(period_txt, "  |  missed children by the main operational cause of each district-round (top cause per district-round)")), g,
                     imw_banner(paste0("Largest operational cause of missed children: ", tolower(lead), " (", imw_num(max(tbl$total)), " children).")),
                     ncol = 1, rel_heights = c(0.55, 4, 0.4))
}

# slide 9: social mobilisation (caregiver awareness) against coverage, by district
imw_sm_fig <- function(df, period_txt) {
  dfr <- ims_with_risk(df)
  if (!"awareness_rate" %in% names(dfr) || all(is.na(dfr$awareness_rate))) stop("no awareness data")
  grp <- intersect(c("country", "province", "district"), names(dfr))
  if (!"missed_child" %in% names(dfr)) dfr$missed_child <- pmax(as.numeric(dfr$u5_present) - as.numeric(dfr$u5_fm), 0)
  s <- dplyr::summarise(dplyr::group_by(dfr, dplyr::across(dplyr::all_of(grp))),
    cv_d = sum(as.numeric(u5_fm), na.rm = TRUE) / pmax(sum(as.numeric(u5_present), na.rm = TRUE), 1), awareness_rate = mean(awareness_rate, na.rm = TRUE),
    missed_child = sum(as.numeric(missed_child), na.rm = TRUE), .groups = "drop")
  s <- dplyr::filter(s, is.finite(awareness_rate), is.finite(cv_d))
  s$quadrant <- dplyr::case_when(s$awareness_rate < 0.80 & s$cv_d < 0.90 ~ "Low awareness + low coverage", s$awareness_rate < 0.80 & s$cv_d >= 0.90 ~ "Low awareness but acceptable coverage",
                                 s$awareness_rate >= 0.80 & s$cv_d < 0.90 ~ "High awareness but low coverage", TRUE ~ "High awareness + good coverage")
  cols <- c("Low awareness + low coverage" = IMW$red, "Low awareness but acceptable coverage" = IMW$orange, "High awareness but low coverage" = IMW$purple, "High awareness + good coverage" = IMW$teal)
  s$sz <- pmax(pmin(sqrt(s$missed_child + 1) * 0.9, 15), 4.5)
  n <- nrow(s); q <- function(x) sum(s$quadrant == x)
  pc <- function(x) { v <- 100 * x / n; if (v < 10) sprintf("%.1f%%", v) else sprintf("%.0f%%", v) }
  set.seed(1); sp <- s[sample.int(n, min(n, 2500)), ]
  g <- ggplot2::ggplot(sp, ggplot2::aes(awareness_rate, cv_d, colour = quadrant, size = sz)) +
    ggplot2::geom_vline(xintercept = 0.8, linetype = "dashed", colour = "#9AA5B1") + ggplot2::geom_hline(yintercept = 0.9, linetype = "dashed", colour = "#9AA5B1") +
    ggplot2::geom_point(alpha = 0.7) + ggplot2::scale_colour_manual(values = cols, name = NULL) + ggplot2::scale_size_continuous(range = c(1.2, 5), guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.02, 1.05)) + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(x = "Caregiver awareness rate (%)", y = "Vaccination coverage, IM (%)") +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(), plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                                                         axis.title = ggplot2::element_text(colour = IMW$muted), plot.margin = ggplot2::margin(6, 8, 6, 10)) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 3, alpha = 1)))
  low <- s[s$awareness_rate < 0.80, ]
  topc <- if (nrow(low) && "country" %in% names(low)) head(sort(table(im_nice_name(low$country)), decreasing = TRUE), 4) else NULL
  side <- cowplot::plot_grid(
    imw_card(pc(q("High awareness + good coverage")), "High awareness and good coverage", paste0(imw_num(q("High awareness + good coverage")), " of ", imw_num(n), " districts"), "#3D8B6E", 7),
    imw_card(pc(q("Low awareness + low coverage")), "Low awareness and low coverage", paste0(imw_num(q("Low awareness + low coverage")), " districts: priority for social mobilisation"), IMW$red, 7),
    imw_card(pc(q("High awareness but low coverage")), "High awareness but low coverage", paste0(imw_num(q("High awareness but low coverage")), " districts: check operations and microplans"), IMW$purple, 7),
    ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0.5, hjust = 0, size = 3.1, colour = IMW$ink, label = if (!is.null(topc)) paste(strwrap(paste0("Most low-awareness districts: ", paste0(names(topc), " (", as.integer(topc), ")", collapse = ", ")), 34), collapse = "\n") else "") +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) + ggplot2::theme_void(),
    ncol = 1, rel_heights = c(1, 1, 1, 0.7))
  body <- cowplot::plot_grid(g, side, nrow = 1, rel_widths = c(3, 1.15))
  cowplot::plot_grid(imw_head("Social mobilisation vs coverage", paste0("Caregiver awareness and monitored coverage by district  |  ", imw_period(df), "  (", imw_num(n), " districts)")), body,
                     imw_banner("Advocacy: intensify pre-campaign mobilisation where awareness is below 80%."), ncol = 1, rel_heights = c(0.55, 4, 0.4))
}

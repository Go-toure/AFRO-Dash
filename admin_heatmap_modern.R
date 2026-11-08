# ============================================================
# Admin Data Overview -- "Countries by Month" view (added next to the flextables)
# One column per campaign month, one named box per country, coloured by the share of
# its districts at 95% coverage or more. Needs only ggplot2 / dplyr / stringr / tools
# (plotly optional, used for the hover tooltips when installed).
# Input: `dist_df` from admin_coverage_summary()
#   columns: Country, District, vaccine.type, floor_date, cov (coverage in %)
# ============================================================

ADMIN_HM_COL <- list(
  green = "#3D8B6E", gold = "#E3B23C", red = "#B83B5E",
  navy = "#14454F", text = "#22313A", muted = "#66737A",
  blue = "#2F7F9E", teal = "#2A9D8F", purple = "#7A5C99"
)

admin_coverage_country_month_data <- function(dist_df) {
  dist_df |>
    dplyr::filter(!is.na(cov)) |>
    dplyr::group_by(Country, floor_date) |>
    dplyr::summarise(n = dplyr::n(), n95 = sum(cov >= 95), .groups = "drop") |>
    dplyr::mutate(share95 = round(100 * n95 / n, 0))
}

# Card value font sizes shrink with the text length so long names always fit
admin_card_px <- function(txt, base = 26, min_px = 12, avail_px = 215) {
  n <- max(nchar(as.character(txt)), 1)
  max(min_px, min(base, floor(avail_px / (0.6 * n))))
}
admin_card_mm <- function(txt, base = 8.5, avail = 118) {
  n <- pmax(nchar(as.character(txt)), 1)
  pmin(base, pmax(3.2, avail / n))
}

admin_cm_title <- function() "Vaccination coverage by country and month"
admin_cm_subtitle <- function(block_display = "All Countries", period_text = "") {
  paste0(block_display, if (nzchar(period_text)) paste0("  |  ", period_text) else "",
         "  |  share of each country's districts at 95% coverage or more",
         "  |  green = 95% or more, amber = 80 to 94%, red = below 80%; best at the bottom")
}

admin_coverage_country_month_gg <- function(dist_df, block_display = "All Countries", period_text = "",
                                            show_title = TRUE, show_legend = TRUE) {
  C <- ADMIN_HM_COL
  d <- admin_coverage_country_month_data(dist_df)
  months <- seq(min(d$floor_date), max(d$floor_date), by = "month")
  d$mlab <- factor(format(d$floor_date, "%b %Y"), levels = format(months, "%b %Y"))
  d$band <- factor(dplyr::case_when(d$share95 >= 95 ~ "95% or more", d$share95 >= 80 ~ "80 to 94%", TRUE ~ "Below 80%"),
                   levels = c("95% or more", "80 to 94%", "Below 80%"))
  d <- d |> dplyr::arrange(mlab, dplyr::desc(share95), Country) |>
    dplyr::group_by(mlab) |> dplyr::mutate(k = dplyr::row_number()) |> dplyr::ungroup()
  nm <- ifelse(d$Country == "CAR", "Central African Republic", tools::toTitleCase(tolower(d$Country)))
  d$name <- paste0(stringr::str_wrap(nm, 12), "\n", d$share95, "%")
  d$tip <- paste0(d$Country, " | ", format(d$floor_date, "%B %Y"), "\n", d$share95,
                  "% of districts at 95% coverage or more\n(", d$n95, " of ", d$n, " district results)")
  cnt <- d |> dplyr::count(mlab, name = "nc")

  fills <- c("95% or more" = C$green, "80 to 94%" = C$gold, "Below 80%" = C$red)
  ggplot2::ggplot(d, ggplot2::aes(mlab, k, fill = band, text = tip)) +
    ggplot2::geom_tile(width = 0.94, height = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = name, colour = band == "80 to 94%"), size = 2.35, lineheight = 0.88,
                       fontface = "bold", show.legend = FALSE) +
    ggplot2::geom_text(data = cnt, ggplot2::aes(mlab, nc + 0.9, label = paste0(nc, ifelse(nc == 1, " country", " countries"))),
                       inherit.aes = FALSE, size = 3, colour = C$muted) +
    ggplot2::scale_colour_manual(values = c("TRUE" = C$text, "FALSE" = "white"), guide = "none") +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE, name = NULL) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(add = c(0.3, 0.8))) +
    ggplot2::labs(
      title = if (show_title) admin_cm_title(),
      subtitle = if (show_title) admin_cm_subtitle(block_display, period_text),
      x = NULL, y = NULL,
      caption = paste0("Each box is one country and month (all vaccine types together); the value is its share of districts at 95% coverage or more. Source: PEP SIA repository © WHO AFRO GIS CENTRE, generated on ",
                       format(Sys.Date(), "%Y-%m-%d"))) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = C$navy, size = 15, family = "serif"),
      plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9.5),
      plot.caption = ggplot2::element_text(colour = C$muted, size = 8, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.text.x = ggplot2::element_text(colour = C$text, size = 10, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      legend.position = if (show_legend) "top" else "none", legend.justification = "left",
      plot.margin = ggplot2::margin(10, 14, 8, 10),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

admin_country_month_height <- function(dist_df) {
  d <- admin_coverage_country_month_data(dist_df)
  max(4, 0.52 * max(table(d$floor_date)) + 2.2)
}
admin_country_month_height_cards <- function(dist_df) admin_country_month_height(dist_df) + 1.0

# ------------------------------------------------------------
# Headline numbers shown as light cards above the chart
# ------------------------------------------------------------
admin_cm_summary <- function(dist_df) {
  d <- admin_coverage_country_month_data(dist_df)
  list(
    n_all = nrow(d),
    n_green = sum(d$share95 >= 95),
    n_amber = sum(d$share95 >= 80 & d$share95 < 95),
    n_red = sum(d$share95 < 80),
    n_countries = dplyr::n_distinct(d$Country),
    n_months = dplyr::n_distinct(d$floor_date)
  )
}

# list of cards: value, label, sub-label, accent colour
admin_cm_cards <- function(dist_df) {
  C <- ADMIN_HM_COL; sm <- admin_cm_summary(dist_df)
  pct <- function(x) paste0(round(100 * x / max(sm$n_all, 1)), "% of campaigns")
  list(
    list(value = paste0(sm$n_green, " of ", sm$n_all), label = "Reached 95% coverage",
         sub = paste0("in at least 95% of districts (", pct(sm$n_green), ")"), col = C$green),
    list(value = as.character(sm$n_amber), label = "Close to target",
         sub = paste0("80 to 94% of districts (", pct(sm$n_amber), ")"), col = C$gold),
    list(value = as.character(sm$n_red), label = "Below 80%",
         sub = paste0("of districts at 95% coverage (", pct(sm$n_red), ")"), col = C$red),
    list(value = as.character(sm$n_countries), label = "Countries",
         sub = paste0("with campaigns over ", sm$n_months, " months"), col = C$navy)
  )
}

admin_cm_legend_items <- function() {
  C <- ADMIN_HM_COL
  list(list(label = "95% or more", col = C$green), list(label = "80 to 94%", col = C$gold),
       list(label = "Below 80%", col = C$red))
}

# Shiny / HTML version (light cards)
admin_cm_cards_html <- function(dist_df, block_display = "All Countries", period_text = "") {
  cards <- admin_cm_cards(dist_df)
  legend <- htmltools::div(
    style = "display:flex; gap:14px; align-items:center; white-space:nowrap; font-size:12.5px; color:#22313A;",
    lapply(admin_cm_legend_items(), function(k)
      htmltools::span(htmltools::span(style = paste0("display:inline-block; width:13px; height:13px; border-radius:3px; margin-right:5px; vertical-align:-2px; background:", k$col, ";")), k$label)))
  htmltools::tagList(
    htmltools::div(style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:8px;",
      htmltools::div(style = "font-family:Georgia,serif; font-weight:700; font-size:20px; color:#14454F;",
                     admin_cm_title()),
      legend),
    htmltools::div(style = "font-size:12.5px; color:#66737A; margin-bottom:10px;",
                   admin_cm_subtitle(block_display, period_text)),
  htmltools::div(
    style = "display:flex; gap:12px; flex-wrap:wrap; margin:4px 0 14px 0;",
    lapply(cards, function(k) {
      htmltools::div(
        style = paste0("flex:1 1 200px; background:#F7F9F6; border:1px solid #E3E8E1; ",
                       "border-left:5px solid ", k$col, "; border-radius:8px; padding:10px 14px;"),
        htmltools::div(style = paste0("font-size:", admin_card_px(k$value), "px; font-weight:700; line-height:1.15; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; color:", k$col, ";"),
                       title = k$value, k$value),
        htmltools::div(style = "font-size:13px; font-weight:600; color:#22313A;", k$label),
        htmltools::div(style = "font-size:11.5px; color:#66737A;", k$sub)
      )
    })
  ))
}

# Image version: cards on top of the chart (used for the PNG download)
admin_cm_with_cards_gg <- function(dist_df, block_display = "All Countries", period_text = "") {
  C <- ADMIN_HM_COL
  cards <- admin_cm_cards(dist_df)
  k <- length(cards)
  cd <- do.call(rbind, lapply(seq_along(cards), function(i)
    data.frame(i = i, value = cards[[i]]$value, label = cards[[i]]$label, sub = stringr::str_wrap(cards[[i]]$sub, 44),
               col = cards[[i]]$col, stringsAsFactors = FALSE)))
  cp <- ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = i - 0.47, xmax = i + 0.47, ymin = 0, ymax = 1), fill = "#F7F9F6", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = i - 0.47, xmax = i - 0.455, ymin = 0, ymax = 1, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.74, label = value, colour = col, size = admin_card_mm(value, 8.5)), hjust = 0, fontface = "bold") +
    ggplot2::scale_size_identity() +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.44, label = label), hjust = 0, size = 3.8, fontface = "bold", colour = C$text) +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.2, label = sub), hjust = 0, size = 2.9, colour = C$muted, lineheight = 0.9, vjust = 0.5) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0.5, k + 0.5), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                                           plot.margin = ggplot2::margin(6, 14, 0, 10))
  main <- admin_coverage_country_month_gg(dist_df, block_display, period_text, show_title = FALSE, show_legend = FALSE)
  # header: title on the left, legend on the right of the same row, subtitle underneath
  li <- admin_cm_legend_items()
  lg <- data.frame(x = c(8.9, 10.4, 11.8)[seq_along(li)], label = vapply(li, `[[`, "", "label"),
                   col = vapply(li, `[[`, "", "col"), stringsAsFactors = FALSE)
  hd <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.02, y = 0.72, label = admin_cm_title(), hjust = 0, size = 5.3,
                      fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.18, label = admin_cm_subtitle(block_display, period_text),
                      hjust = 0, size = 3.3, colour = C$muted) +
    ggplot2::geom_tile(data = lg, ggplot2::aes(x, 0.72, fill = col), width = 0.22, height = 0.3) +
    ggplot2::geom_text(data = lg, ggplot2::aes(x + 0.2, 0.72, label = label), hjust = 0, size = 3.5, colour = C$text) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(6, 14, 0, 10))
  patchwork::wrap_plots(hd, cp, main, ncol = 1, heights = c(0.9, 1.9, 10))
}

# ============================================================
# Number vaccinated by country and month (added next to the "Children Vaccinated" flextable)
# Input `doses_df` from admin_vaccinated_summary():
#   Country, vaccine.type, floor_date, doses (in millions)
# ============================================================
ADMIN_VACC_BINS <- list(
  list(label = "Under 1 M",  lo = 0,  hi = 1,   col = "#D6EAE4", txt = "#22313A"),
  list(label = "1 to 5 M",   lo = 1,  hi = 5,   col = "#8CC7B8", txt = "#22313A"),
  list(label = "5 to 10 M",  lo = 5,  hi = 10,  col = "#3D8B6E", txt = "white"),
  list(label = "10 M or more", lo = 10, hi = Inf, col = "#14454F", txt = "white")
)

admin_vacc_data <- function(doses_df) {
  d <- doses_df |>
    dplyr::filter(!is.na(doses)) |>
    dplyr::group_by(Country, floor_date) |>
    dplyr::summarise(m = sum(doses), vacc = paste0(vaccine.type, ": ", format(round(doses, 2), nsmall = 2), " M", collapse = "\n"),
                     .groups = "drop") |>
    dplyr::filter(m > 0)
  b <- ADMIN_VACC_BINS
  d$bin <- factor(vapply(d$m, function(x) { for (k in b) if (x >= k$lo && x < k$hi) return(k$label); b[[length(b)]]$label }, ""),
                  levels = vapply(b, `[[`, "", "label"))
  d
}

admin_vacc_title <- function() "Number vaccinated by country and month (millions)"
admin_vacc_subtitle <- function(block_display = "All Countries", period_text = "") {
  paste0(block_display, if (nzchar(period_text)) paste0("  |  ", period_text) else "",
         "  |  all vaccine types together; darker = more vaccinated; largest campaign at the bottom")
}

admin_vacc_cards <- function(doses_df) {
  C <- ADMIN_HM_COL
  d <- admin_vacc_data(doses_df)
  tot <- sum(d$m)
  bm <- d |> dplyr::group_by(floor_date) |> dplyr::summarise(m = sum(m), .groups = "drop") |> dplyr::slice_max(m, n = 1, with_ties = FALSE)
  bc <- d |> dplyr::group_by(Country) |> dplyr::summarise(m = sum(m), .groups = "drop") |> dplyr::slice_max(m, n = 1, with_ties = FALSE)
  nice <- function(x) tools::toTitleCase(tolower(sub("^DRC$", "Democratic Republic of the Congo", x)))
  list(
    list(value = paste0(format(round(tot, 1), nsmall = 1, big.mark = ","), " M"), label = "Total vaccinated",
         sub = paste0("across ", nrow(d), " country campaigns"), col = C$navy),
    list(value = as.character(dplyr::n_distinct(d$Country)), label = "Countries",
         sub = paste0("with campaigns over ", dplyr::n_distinct(d$floor_date), " months"), col = C$blue),
    list(value = format(bm$floor_date, "%b %Y"), label = "Busiest month",
         sub = paste0(format(round(bm$m, 1), nsmall = 1), " M vaccinated"), col = C$teal),
    list(value = ifelse(bc$Country == "DRC", "Democratic Republic of the Congo",
                 ifelse(bc$Country == "CAR", "Central African Republic", nice(bc$Country))), label = "Largest total",
         sub = paste0(format(round(bc$m, 1), nsmall = 1), " M vaccinated over the period"), col = C$purple)
  )
}

admin_vacc_cards_html <- function(doses_df, block_display = "All Countries", period_text = "") {
  cards <- admin_vacc_cards(doses_df)
  legend <- htmltools::div(
    style = "display:flex; gap:14px; align-items:center; white-space:nowrap; font-size:12.5px; color:#22313A; flex-wrap:wrap;",
    lapply(ADMIN_VACC_BINS, function(k)
      htmltools::span(htmltools::span(style = paste0("display:inline-block; width:13px; height:13px; border-radius:3px; margin-right:5px; vertical-align:-2px; background:", k$col, ";")), k$label)))
  htmltools::tagList(
    htmltools::div(style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:8px;",
      htmltools::div(style = "font-family:Georgia,serif; font-weight:700; font-size:20px; color:#14454F;", admin_vacc_title()),
      legend),
    htmltools::div(style = "font-size:12.5px; color:#66737A; margin-bottom:10px;", admin_vacc_subtitle(block_display, period_text)),
    htmltools::div(
      style = "display:flex; gap:12px; flex-wrap:wrap; margin:4px 0 14px 0;",
      lapply(cards, function(k) {
        htmltools::div(
          style = paste0("flex:1 1 200px; background:#F7F9F6; border:1px solid #E3E8E1; border-left:5px solid ", k$col,
                         "; border-radius:8px; padding:10px 14px;"),
          htmltools::div(style = paste0("font-size:", admin_card_px(k$value), "px; font-weight:700; line-height:1.15; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; color:", k$col, ";"),
                       title = k$value, k$value),
          htmltools::div(style = "font-size:13px; font-weight:600; color:#22313A;", k$label),
          htmltools::div(style = "font-size:11.5px; color:#66737A;", k$sub))
      })))
}

admin_vacc_gg <- function(doses_df, block_display = "All Countries", period_text = "",
                          show_title = TRUE, show_legend = TRUE) {
  C <- ADMIN_HM_COL
  d <- admin_vacc_data(doses_df)
  months <- seq(min(d$floor_date), max(d$floor_date), by = "month")
  d$mlab <- factor(format(d$floor_date, "%b %Y"), levels = format(months, "%b %Y"))
  d <- d |> dplyr::arrange(mlab, dplyr::desc(m), Country) |>
    dplyr::group_by(mlab) |> dplyr::mutate(k = dplyr::row_number()) |> dplyr::ungroup()
  nm <- ifelse(d$Country == "DRC", "Democratic Republic of the Congo",
        ifelse(d$Country == "CAR", "Central African Republic", tools::toTitleCase(tolower(d$Country))))
  d$name <- paste0(stringr::str_wrap(nm, 12), "\n", format(round(d$m, 1), nsmall = 1), " M")
  d$tip <- paste0(nm, " | ", format(d$floor_date, "%B %Y"), "\n", format(round(d$m, 2), nsmall = 2), " M vaccinated\n", d$vacc)
  d$txt <- ifelse(d$bin %in% c("Under 1 M", "1 to 5 M"), "dark", "light")
  cnt <- d |> dplyr::group_by(mlab) |> dplyr::summarise(nc = dplyr::n(), tot = sum(m), .groups = "drop")
  fills <- setNames(vapply(ADMIN_VACC_BINS, `[[`, "", "col"), vapply(ADMIN_VACC_BINS, `[[`, "", "label"))
  ggplot2::ggplot(d, ggplot2::aes(mlab, k, fill = bin, text = tip)) +
    ggplot2::geom_tile(width = 0.94, height = 0.92) +
    ggplot2::geom_text(ggplot2::aes(label = name, colour = txt), size = 2.35, lineheight = 0.88,
                       fontface = "bold", show.legend = FALSE) +
    ggplot2::geom_text(data = cnt, ggplot2::aes(mlab, nc + 0.9, label = paste0(format(round(tot, 1), nsmall = 1), " M")),
                       inherit.aes = FALSE, size = 3.1, fontface = "bold", colour = C$navy) +
    ggplot2::scale_colour_manual(values = c(dark = C$text, light = "white"), guide = "none") +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE, name = NULL) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(add = c(0.3, 0.8))) +
    ggplot2::labs(
      title = if (show_title) admin_vacc_title(),
      subtitle = if (show_title) admin_vacc_subtitle(block_display, period_text),
      x = NULL, y = NULL,
      caption = paste0("Each box is one country and month; the number above a column is the total vaccinated that month (millions). Source: PEP SIA repository © WHO AFRO GIS CENTRE, generated on ",
                       format(Sys.Date(), "%Y-%m-%d"))) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = C$navy, size = 15, family = "serif"),
      plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9.5),
      plot.caption = ggplot2::element_text(colour = C$muted, size = 8, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.text.x = ggplot2::element_text(colour = C$text, size = 10, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      legend.position = if (show_legend) "top" else "none", legend.justification = "left",
      plot.margin = ggplot2::margin(10, 14, 8, 10),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

admin_vacc_height <- function(doses_df) {
  d <- admin_vacc_data(doses_df)
  max(4, 0.52 * max(table(d$floor_date)) + 2.2)
}
admin_vacc_height_cards <- function(doses_df) admin_vacc_height(doses_df) + 1.0

admin_vacc_with_cards_gg <- function(doses_df, block_display = "All Countries", period_text = "") {
  C <- ADMIN_HM_COL
  cards <- admin_vacc_cards(doses_df); k <- length(cards)
  cd <- do.call(rbind, lapply(seq_along(cards), function(i)
    data.frame(i = i, value = cards[[i]]$value, label = cards[[i]]$label, sub = stringr::str_wrap(cards[[i]]$sub, 44),
               col = cards[[i]]$col, stringsAsFactors = FALSE)))
  cp <- ggplot2::ggplot(cd) +
    ggplot2::geom_rect(ggplot2::aes(xmin = i - 0.47, xmax = i + 0.47, ymin = 0, ymax = 1), fill = "#F7F9F6", colour = "#E3E8E1") +
    ggplot2::geom_rect(ggplot2::aes(xmin = i - 0.47, xmax = i - 0.455, ymin = 0, ymax = 1, fill = col)) +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.74, label = value, colour = col, size = admin_card_mm(value, 8)), hjust = 0, fontface = "bold") +
    ggplot2::scale_size_identity() +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.44, label = label), hjust = 0, size = 3.8, fontface = "bold", colour = C$text) +
    ggplot2::geom_text(ggplot2::aes(i - 0.40, 0.2, label = sub), hjust = 0, size = 2.9, colour = C$muted, lineheight = 0.9) +
    ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(xlim = c(0.5, k + 0.5), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                                           plot.margin = ggplot2::margin(6, 14, 0, 10))
  main <- admin_vacc_gg(doses_df, block_display, period_text, show_title = FALSE, show_legend = FALSE)
  li <- ADMIN_VACC_BINS
  lg <- data.frame(x = c(7.0, 8.4, 9.7, 11.0)[seq_along(li)], label = vapply(li, `[[`, "", "label"),
                   col = vapply(li, `[[`, "", "col"), stringsAsFactors = FALSE)
  hd <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.02, y = 0.72, label = admin_vacc_title(), hjust = 0, size = 5.3,
                      fontface = "bold", family = "serif", colour = C$navy) +
    ggplot2::annotate("text", x = 0.02, y = 0.18, label = admin_vacc_subtitle(block_display, period_text),
                      hjust = 0, size = 3.3, colour = C$muted) +
    ggplot2::geom_tile(data = lg, ggplot2::aes(x, 0.72, fill = col), width = 0.22, height = 0.3) +
    ggplot2::geom_text(data = lg, ggplot2::aes(x + 0.2, 0.72, label = label), hjust = 0, size = 3.5, colour = C$text) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = c(0, 13), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(6, 14, 0, 10))
  patchwork::wrap_plots(hd, cp, main, ncol = 1, heights = c(0.9, 1.9, 10))
}

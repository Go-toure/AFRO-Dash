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
  navy = "#14454F", text = "#22313A", muted = "#66737A"
)

admin_coverage_country_month_data <- function(dist_df) {
  dist_df |>
    dplyr::filter(!is.na(cov)) |>
    dplyr::group_by(Country, floor_date) |>
    dplyr::summarise(n = dplyr::n(), n95 = sum(cov >= 95), .groups = "drop") |>
    dplyr::mutate(share95 = round(100 * n95 / n, 0))
}

admin_coverage_country_month_gg <- function(dist_df, block_display = "All Countries", period_text = "") {
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
  n_all <- nrow(d); n_green <- sum(d$band == "95% or more"); n_red <- sum(d$band == "Below 80%")
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
      title = paste0("Vaccination coverage by country and month: ", n_green, " of ", n_all,
                     " campaigns reached 95% coverage in at least 95% of districts"),
      subtitle = paste0(block_display, if (nzchar(period_text)) paste0("  |  ", period_text) else "",
                        "  |  ", n_red, " campaigns stayed below 80% of districts at 95% coverage",
                        "  |  green = 95% or more, amber = 80 to 94%, red = below 80%; best at the bottom"),
      x = NULL, y = NULL,
      caption = paste0("Each box is one country and month (all vaccine types together); the value is its share of districts at 95% coverage or more. Source: PEP SIA repository © WHO AFRO GIS CENTRE, generated on ",
                       format(Sys.Date(), "%Y-%m-%d"))) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = C$navy, size = 14, family = "serif"),
      plot.subtitle = ggplot2::element_text(colour = C$muted, size = 9.5),
      plot.caption = ggplot2::element_text(colour = C$muted, size = 8, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      axis.text.x = ggplot2::element_text(colour = C$text, size = 10, face = "bold"),
      panel.grid = ggplot2::element_blank(),
      legend.position = "top", legend.justification = "left",
      plot.margin = ggplot2::margin(10, 14, 8, 10),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

admin_country_month_height <- function(dist_df) {
  d <- admin_coverage_country_month_data(dist_df)
  max(4, 0.52 * max(table(d$floor_date)) + 2.2)
}

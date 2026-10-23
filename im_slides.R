# ============================================================================
# im_slides.R  --  Independent Monitoring (IM) analyses for the Generate Slides tab
#
# Static (ggplot / flextable) versions of the IM Overview views, built from the
# same cleaned IM repository (input/AFRO_IM_data.rds) and the same definitions:
#   coverage = finger-marked / children present (target 90%), missed children =
#   present - finger-marked, caregiver awareness = informed / households, risk
#   score and class as in the IM Overview module.
#
# Three slide analyses:
#   im_overview  key numbers, coverage by country/province/district, monthly
#                trend, district risk classes
#   im_reasons   root causes of missed children
#   im_priority  priority districts (lowest coverage)
# ============================================================================

IMS_COL <- list(green = "#27AE60", orange = "#E8730A", red = "#C0392B", blue = "#005C97",
                blue_l = "#1A7BBF", grey = "#6C7A8D", dark = "#1A2637", other = "#0093D5",
                priority = "#C00000")

ims_pct <- function(x, d = 1) ifelse(is.na(x), "-", paste0(round(x * 100, d), "%"))
ims_num <- function(x) ifelse(is.na(x), "-", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE))

# ---- filtering (same rules as the other slide analyses) --------------------------------
ims_prepare <- function(df, afro_blocks = NULL, block = NULL, countries = NULL,
                        provinces = NULL, districts = NULL, x_months = NULL, years = NULL) {
  if (is.null(df) || !nrow(df)) return(NULL)
  df <- as.data.frame(df)
  names(df) <- tolower(names(df))
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  for (cn in intersect(c("u5_present", "u5_fm", "number_of_hh_visited", "care_giver_informed_sia"), names(df)))
    df[[cn]] <- num(df[[cn]])

  if (!is.null(block) && nzchar(block) && block != "All") {
    if ("afro_block" %in% names(df)) df <- df[!is.na(df$afro_block) & df$afro_block == block, , drop = FALSE]
    else if (!is.null(afro_blocks[[block]])) df <- df[toupper(df$country) %in% toupper(afro_blocks[[block]]), , drop = FALSE]
  }
  if (length(countries)) df <- df[!is.na(df$country) & toupper(df$country) %in% toupper(countries), , drop = FALSE]
  if (length(provinces) && "province" %in% names(df))
    df <- df[!is.na(df$province) & toupper(df$province) %in% toupper(provinces), , drop = FALSE]
  if (length(districts) && "district" %in% names(df))
    df <- df[!is.na(df$district) & toupper(df$district) %in% toupper(districts), , drop = FALSE]

  dc <- intersect(c("round_start_date", "start_date_im_end", "start_date"), names(df))[1]
  period <- "All available data"
  if (!is.na(dc)) {
    d <- suppressWarnings(as.Date(df[[dc]]))
    df$.date <- d
    if (!is.null(x_months) && is.finite(x_months) && x_months > 0 && any(!is.na(d))) {
      mx <- max(d, na.rm = TRUE)
      cut <- seq(mx, by = paste0("-", as.integer(x_months), " months"), length.out = 2)[2]
      df <- df[!is.na(df$.date) & df$.date >= cut, , drop = FALSE]
      period <- paste0("Last ", x_months, " months (", format(cut, "%b %Y"), " - ", format(mx, "%b %Y"), ")")
    } else if (length(years)) {
      df <- df[!is.na(df$.date) & as.integer(format(df$.date, "%Y")) %in% as.integer(years), , drop = FALSE]
      period <- paste("Year", paste(years, collapse = ", "))
    }
  }
  if (!nrow(df)) return(NULL)
  attr(df, "ims_period") <- period
  df
}

ims_with_risk <- function(df) {
  if (!all(c("u5_present", "u5_fm") %in% names(df))) return(df)
  df$missed_child <- pmax(df$u5_present - df$u5_fm, 0)
  df$cv_d <- ifelse(df$u5_present > 0, df$u5_fm / df$u5_present, NA_real_)
  df$missed_rate <- ifelse(df$u5_present > 0, df$missed_child / df$u5_present, NA_real_)
  has_sm <- all(c("number_of_hh_visited", "care_giver_informed_sia") %in% names(df))
  df$awareness_rate <- if (has_sm)
    ifelse(df$number_of_hh_visited > 0, df$care_giver_informed_sia / df$number_of_hh_visited, NA_real_)
  else NA_real_
  df$risk_score <-
    ifelse(is.na(df$cv_d), 50, ifelse(df$cv_d < 0.80, 60, ifelse(df$cv_d < 0.90, 40, 20))) +
    ifelse(is.na(df$missed_rate), 0, ifelse(df$missed_rate > 0.20, 30, ifelse(df$missed_rate > 0.10, 15, 0))) +
    ifelse(is.na(df$awareness_rate), 0, ifelse(df$awareness_rate < 0.50, 20, ifelse(df$awareness_rate < 0.80, 10, 0)))
  df$risk_class <- ifelse(df$risk_score >= 70, "Critical", ifelse(df$risk_score >= 50, "High",
                          ifelse(df$risk_score >= 30, "Moderate", "Low")))
  df
}

ims_theme <- function() {
  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, colour = IMS_COL$dark),
      plot.subtitle = ggplot2::element_text(size = 11, colour = IMS_COL$grey),
      plot.caption = ggplot2::element_text(size = 9, colour = IMS_COL$grey, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom")
}

ims_caption <- function(df) paste0("Source: WHO AFRO Independent Monitoring repository | ", attr(df, "ims_period"))

# ---- the individual views ---------------------------------------------------------------------
ims_metrics <- function(df) {
  dfr <- ims_with_risk(df)
  tp <- sum(df$u5_present, na.rm = TRUE); tf <- sum(df$u5_fm, na.rm = TRUE)
  cv <- if (tp > 0) tf / tp else NA_real_
  aw <- if ("awareness_rate" %in% names(dfr)) mean(dfr$awareness_rate, na.rm = TRUE) else NA_real_
  crit <- if ("risk_class" %in% names(dfr))
    sum(!duplicated(dfr[dfr$risk_class == "Critical", c("country", "province", "district")])) else NA
  list(
    list(label = "Records", value = ims_num(nrow(df))),
    list(label = "Countries", value = ims_num(length(unique(df$country)))),
    list(label = "Households visited", value = ims_num(sum(df$number_of_hh_visited, na.rm = TRUE))),
    list(label = "Coverage (target 90%)", value = ims_pct(cv)),
    list(label = "Missed children", value = ims_num(max(tp - tf, 0))),
    list(label = "Caregiver awareness", value = ims_pct(aw)),
    list(label = "Critical districts", value = ims_num(crit)),
    list(label = "Children present", value = ims_num(tp)))
}

ims_cv_plot <- function(df, countries = NULL, provinces = NULL, districts = NULL) {
  grp <- if (length(districts)) "province" else if (length(provinces)) "district"
         else if (length(countries) == 1) "province" else "country"
  if (!grp %in% names(df)) grp <- "country"
  s <- stats::aggregate(cbind(u5_fm, u5_present) ~ g, data = data.frame(g = df[[grp]], u5_fm = df$u5_fm, u5_present = df$u5_present),
                        FUN = sum, na.rm = TRUE)
  s$n <- as.vector(table(df[[grp]])[as.character(s$g)])
  s$cv <- pmin(s$u5_fm / pmax(s$u5_present, 1), 1)
  s <- s[order(s$cv), ]
  shown <- nrow(s)
  note <- NULL
  if (nrow(s) > 30) { s <- s[1:30, ]; note <- paste0("30 lowest of ", shown, " shown") }
  s$col <- ifelse(s$cv >= 0.9, "90% or more", ifelse(s$cv >= 0.8, "80 - 89%", "Below 80%"))
  s$col <- factor(s$col, levels = c("90% or more", "80 - 89%", "Below 80%"))
  s$g <- factor(s$g, levels = s$g)
  ggplot2::ggplot(s, ggplot2::aes(x = cv, y = g, fill = col)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(cv * 100, 1), "%")), hjust = -0.15, size = 3.6, colour = IMS_COL$dark) +
    ggplot2::geom_vline(xintercept = c(0.8, 0.9), linetype = "dotted", colour = IMS_COL$grey) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%"), limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
    ggplot2::scale_fill_manual(values = c("90% or more" = IMS_COL$green, "80 - 89%" = IMS_COL$orange, "Below 80%" = IMS_COL$red), name = "Coverage") +
    ggplot2::labs(title = paste0("IM coverage by ", grp), subtitle = note, x = NULL, y = NULL, caption = ims_caption(df)) +
    ims_theme() + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

ims_trend_plot <- function(df) {
  if (!".date" %in% names(df)) return(NULL)
  d <- df[!is.na(df$.date) & df$.date >= as.Date("2020-01-01"), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$period <- as.Date(format(d$.date, "%Y-%m-01"))
  a <- stats::aggregate(cbind(u5_fm, u5_present) ~ period, data = d, FUN = sum, na.rm = TRUE)
  a$records <- as.vector(table(d$period)[as.character(a$period)])
  a$cv <- pmin(a$u5_fm / pmax(a$u5_present, 1), 1)
  if (nrow(a) < 2) return(NULL)
  p1 <- ggplot2::ggplot(a, ggplot2::aes(period, records)) +
    ggplot2::geom_col(fill = IMS_COL$blue, alpha = 0.8, width = 22) +
    ggplot2::scale_x_date(date_labels = "%b %Y") +
    ggplot2::labs(title = "Records and coverage by month", x = NULL, y = "Records") + ims_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
  p2 <- ggplot2::ggplot(a, ggplot2::aes(period, cv)) +
    ggplot2::geom_hline(yintercept = 0.9, linetype = "dotted", colour = IMS_COL$green) +
    ggplot2::geom_line(colour = IMS_COL$green, linewidth = 1) + ggplot2::geom_point(colour = IMS_COL$green, size = 2) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), limits = c(0, 1)) +
    ggplot2::scale_x_date(date_labels = "%b %Y") +
    ggplot2::labs(x = NULL, y = "Coverage", caption = paste0(ims_caption(df), " | dotted line = 90% target")) + ims_theme()
  patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(1, 1.3))
}

ims_risk_plot <- function(df) {
  dfr <- ims_with_risk(df)
  if (!"risk_class" %in% names(dfr)) return(NULL)
  k <- dfr[!duplicated(dfr[, c("country", "province", "district")]), , drop = FALSE]
  t <- as.data.frame(table(factor(k$risk_class, levels = c("Critical", "High", "Moderate", "Low"))), stringsAsFactors = FALSE)
  names(t) <- c("cls", "n")
  t$share <- t$n / max(sum(t$n), 1)
  t$cls <- factor(t$cls, levels = rev(c("Critical", "High", "Moderate", "Low")))
  ggplot2::ggplot(t, ggplot2::aes(n, cls, fill = cls)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(ims_num(n), "  (", round(share * 100, 1), "%)")), hjust = -0.1, size = 4.2, colour = IMS_COL$dark) +
    ggplot2::scale_fill_manual(values = c(Critical = IMS_COL$red, High = IMS_COL$orange, Moderate = IMS_COL$blue_l, Low = IMS_COL$green)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(title = "District risk classification", subtitle = "Number of districts by risk class", x = NULL, y = NULL, caption = ims_caption(df)) +
    ims_theme()
}

IMS_ROOT_LABELS <- c(
  r_non_fm_absent = "Absent", r_non_fm_nc = "Non-compliance", r_non_fm_hh_notvisited = "House not visited",
  r_non_fm_hh_notrevisited = "House not revisited", r_non_fm_sleep = "Sleeping child",
  r_non_fm_vaccinatedroutine = "Vaccinated (routine)", r_non_fm_other = "Other reason",
  r_non_fm_vaccinated_but_not_fm = "Vaccinated but not FM", r_non_fm_child_is_a_visitor = "Visitor child",
  r_non_fm_childnotborn = "Child not born", r_non_fm_security = "Security")

ims_reasons_plot <- function(df) {
  rc <- intersect(names(IMS_ROOT_LABELS), names(df))
  if (!length(rc)) return(NULL)
  tot <- vapply(rc, function(cn) sum(suppressWarnings(as.numeric(df[[cn]])), na.rm = TRUE), numeric(1))
  t <- data.frame(reason = unname(IMS_ROOT_LABELS[rc]), total = unname(tot), stringsAsFactors = FALSE)
  t <- t[t$total > 0, , drop = FALSE]
  if (!nrow(t)) return(NULL)
  t <- t[order(-t$total), ][seq_len(min(12, nrow(t))), ]
  t$share <- t$total / sum(t$total)
  t$cat <- ifelse(t$reason %in% c("Non-compliance", "House not visited", "House not revisited"),
                  "Priority operational / behavioural issue", "Other missed-child driver")
  t$reason <- factor(t$reason, levels = rev(t$reason))
  ggplot2::ggplot(t, ggplot2::aes(total, reason, fill = cat)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(ims_num(total), " (", round(share * 100, 1), "%)")), hjust = -0.08, size = 3.8, colour = IMS_COL$dark) +
    ggplot2::scale_fill_manual(values = c("Priority operational / behavioural issue" = IMS_COL$priority, "Other missed-child driver" = IMS_COL$other), name = NULL) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.3)), labels = function(x) format(x, big.mark = ",")) +
    ggplot2::labs(title = "Root causes of missed children", subtitle = "Children not finger-marked, by reason", x = NULL, y = NULL, caption = ims_caption(df)) +
    ims_theme()
}

ims_priority_table <- function(df, n = 15, min_children = 30) {
  g <- stats::aggregate(cbind(u5_fm, u5_present) ~ country + province + district, data = df, FUN = sum, na.rm = TRUE)
  g <- g[g$u5_present >= min_children, , drop = FALSE]
  if (!nrow(g)) return(NULL)
  g$cv <- pmin(g$u5_fm / g$u5_present, 1)
  g$missed <- pmax(g$u5_present - g$u5_fm, 0)
  g <- g[order(g$cv, -g$missed), ][seq_len(min(n, nrow(g))), ]
  out <- data.frame(Country = g$country, Province = g$province, District = g$district,
                    `Children present` = g$u5_present, Missed = g$missed, Coverage = paste0(round(g$cv * 100, 1), "%"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  ft <- flextable::flextable(out)
  ft <- flextable::theme_zebra(ft)
  ft <- flextable::color(ft, j = "Coverage", color = IMS_COL$red, part = "body")
  ft <- flextable::bold(ft, j = "Coverage", part = "body")
  ft <- flextable::colformat_num(ft, j = c("Children present", "Missed"), big.mark = ",", digits = 0)
  ft <- flextable::add_header_lines(ft, paste0("Priority districts - lowest IM coverage (at least ", min_children, " children present)"))
  flextable::autofit(ft)
}

# ---- entry point used by the Generate Slides tab ------------------------------------------------
# keys: any of "im_overview", "im_reasons", "im_priority". Returns a named list of results,
# each shaped like the other slide analyses (see slb_items()).
ims_run <- function(keys, df, countries = NULL, provinces = NULL, districts = NULL) {
  out <- list()
  if (is.null(df) || !nrow(df)) return(out)
  safe <- function(expr) tryCatch(expr, error = function(e) NULL)
  per <- attr(df, "ims_period")
  if ("im_overview" %in% keys) {
    out$im_overview <- list(
      metrics = safe(ims_metrics(df)),
      cv_plot = safe(ims_cv_plot(df, countries, provinces, districts)),
      trend_plot = safe(ims_trend_plot(df)),
      risk_plot = safe(ims_risk_plot(df)),
      period_info = per)
  }
  if ("im_reasons" %in% keys) out$im_reasons <- list(plot = safe(ims_reasons_plot(df)), period_info = per)
  if ("im_priority" %in% keys) out$im_priority <- list(table = safe(ims_priority_table(df)), period_info = per)
  out
}

# ---- data loader (cached on the file's modification time) ----------------------------------------
.ims_cache <- new.env(parent = emptyenv())
ims_load <- function(path = file.path("input", "AFRO_IM_data.rds")) {
  if (!file.exists(path)) return(NULL)
  mt <- file.mtime(path)
  if (is.null(.ims_cache$data) || !identical(.ims_cache$mtime, mt)) {
    d <- tryCatch(readRDS(path), error = function(e) NULL)
    .ims_cache$data <- if (is.null(d)) NULL else as.data.frame(d, stringsAsFactors = FALSE)
    if (!is.null(.ims_cache$data) && exists("focus_filter", mode = "function")) .ims_cache$data <- focus_filter(.ims_cache$data)
    .ims_cache$mtime <- mt
  }
  .ims_cache$data
}

# ==============================================================================
# snapshot_module.R -- "Snapshot" landing page of the WHO AFRO SIA Dashboard
#
# One page that captures the headline numbers of the independent-monitoring
# programme (LQAS + IM) and of the campaigns themselves (administrative data):
#
#   Independent monitoring : ONA submissions, countries, provinces, districts,
#                            settlements, health facilities, households,
#                            children sampled / vaccinated, campaigns, rounds
#   Campaign delivery      : doses administered, children vaccinated, campaigns,
#                            rounds, % of district-rounds >= 95% admin coverage,
#                            campaigns by vaccine (nOPV only / bOPV only / both)
#
# Data (all read from input/, nothing from the network):
#   AFRO_LQAS_data_c.rds   LQAS repository (district-round)
#   AFRO_IM_data.rds       IM repository   (district-round)
#   AFRO_admin_data.rds    administrative coverage (district-round)
#   AFRO_snapshot_stats_IM.csv / _LQAS.csv
#                          raw ONA counts (submissions / settlements / health
#                          facilities) written by tools/snapshot_stats.py -- the
#                          repositories are aggregated and cannot give these.
#
# Usage in app.R:  source("snapshot_module.R")
#                  UI:     snap_ui("snap")
#                  server: snap_server("snap", blocks = afro_blocks)
# Needs only shiny, shinydashboard-free HTML, dplyr.  No plotly.
# ==============================================================================

SNAP_FILES <- list(
  lqas      = file.path("input", "AFRO_LQAS_data_c.rds"),
  im        = file.path("input", "AFRO_IM_data.rds"),
  admin     = file.path("input", "AFRO_admin_data.rds"),
  stats_im  = file.path("input", "AFRO_snapshot_stats_IM.csv"),
  stats_lqas = file.path("input", "AFRO_snapshot_stats_LQAS.csv")
)

.snap_cache <- new.env(parent = emptyenv())

# ---- small helpers -----------------------------------------------------------
.snap_accents <- list(
  c("\u00c9", "E"), c("\u00c8", "E"), c("\u00ca", "E"), c("\u00cb", "E"), c("\u00e9", "E"), c("\u00e8", "E"),
  c("\u00ea", "E"), c("\u00eb", "E"), c("\u00d4", "O"), c("\u00d6", "O"), c("\u00f4", "O"), c("\u00f6", "O"),
  c("\u00ce", "I"), c("\u00cf", "I"), c("\u00ee", "I"), c("\u00ef", "I"), c("\u00c0", "A"), c("\u00c2", "A"),
  c("\u00e0", "A"), c("\u00e2", "A"), c("\u00c7", "C"), c("\u00e7", "C"), c("\u00dc", "U"), c("\u00db", "U"),
  c("\u00d9", "U"), c("\u00fc", "U"), c("\u00fb", "U"), c("\u00f9", "U"))

# Locale-independent (works in a C locale and a UTF-8 locale): byte-wise fixed
# replacement of the accented letters, then everything that is not A-Z / 0-9 goes.
.snap_accents <- list(
  c("\u00c9", "E"), c("\u00c8", "E"), c("\u00ca", "E"), c("\u00cb", "E"), c("\u00e9", "E"), c("\u00e8", "E"),
  c("\u00ea", "E"), c("\u00eb", "E"), c("\u00d4", "O"), c("\u00d6", "O"), c("\u00f4", "O"), c("\u00f6", "O"),
  c("\u00ce", "I"), c("\u00cf", "I"), c("\u00ee", "I"), c("\u00ef", "I"), c("\u00c0", "A"), c("\u00c2", "A"),
  c("\u00e0", "A"), c("\u00e2", "A"), c("\u00c7", "C"), c("\u00e7", "C"), c("\u00dc", "U"), c("\u00db", "U"),
  c("\u00d9", "U"), c("\u00fc", "U"), c("\u00fb", "U"), c("\u00f9", "U"))

# Locale-independent (works in a C locale and a UTF-8 locale): byte-wise fixed
# replacement of the accented letters, then everything that is not A-Z / 0-9 goes.
snap_norm_key <- function(x) {
  x <- enc2utf8(as.character(x))
  for (m in .snap_accents) x <- gsub(enc2utf8(m[1]), m[2], x, fixed = TRUE, useBytes = TRUE)
  x <- toupper(trimws(x))
  gsub("[^A-Z0-9]", "", x, useBytes = TRUE)
}

snap_canon_country <- function(x, ref) {
  # ref: named character vector, names = normalised key, values = display name
  k <- snap_norm_key(x)
  alias <- c(REPUBLIQUECENTRAFRIQUE = "CENTRAL AFRICAN REPUBLIC",
             REPUBLIQUECENTRAFRICAINE = "CENTRAL AFRICAN REPUBLIC",
             CENTRAFRIQUE = "CENTRAL AFRICAN REPUBLIC", CAMEROUN = "CAMEROON",
             TANZANIA = "UNITED REPUBLIC OF TANZANIA", TCHAD = "CHAD",
             RDC = "DEMOCRATIC REPUBLIC OF THE CONGO", DRC = "DEMOCRATIC REPUBLIC OF THE CONGO")
  out <- unname(ref[k])
  miss <- is.na(out)
  out[miss] <- unname(alias[k[miss]])
  miss <- is.na(out)
  out[miss] <- toupper(trimws(as.character(x[miss])))
  out
}

snap_keyp <- function(..., optional = character(0)) {
  # "|"-joined normalised key; NA when a required part is blank/NA.
  # optional: indices (1-based) of parts that may be blank.
  parts <- lapply(list(...), snap_norm_key)
  ok <- rep(TRUE, length(parts[[1]]))
  for (i in seq_along(parts)) {
    blank <- is.na(parts[[i]]) | !nzchar(parts[[i]])
    if (!(i %in% optional)) ok <- ok & !blank
    parts[[i]][is.na(parts[[i]])] <- ""
  }
  out <- do.call(paste, c(parts, sep = "|"))
  out[!ok] <- NA_character_
  out
}

snap_vfam <- function(v) {
  k <- toupper(gsub("[^A-Za-z0-9]", "", as.character(v)))
  hn <- grepl("NOPV", k); hb <- grepl("BOPV", k)
  ifelse(hn & hb, "both", ifelse(hn, "nopv", ifelse(hb, "bopv",
         ifelse(grepl("MOPV", k), "mopv", ifelse(k == "MR", "mr", "other")))))
}

snap_fmt <- function(n, digits = 2) {
  if (is.null(n) || length(n) == 0 || is.na(n)) return("\u2014")
  a <- abs(n)
  if (a >= 1e9) sprintf(paste0("%.", digits, "fB"), n / 1e9)
  else if (a >= 1e6) sprintf(paste0("%.", digits, "fM"), n / 1e6)
  else if (a >= 1e4) sprintf("%.1fK", n / 1e3)
  else format(round(n), big.mark = ",", scientific = FALSE)
}
snap_full <- function(n) if (is.null(n) || length(n) == 0 || is.na(n)) "" else format(round(n), big.mark = ",", scientific = FALSE)
snap_pct <- function(x, d = 1) if (is.null(x) || length(x) == 0 || is.na(x)) "\u2014" else sprintf(paste0("%.", d, "f%%"), 100 * x)

# ---- data load (cached on file mtimes) --------------------------------------
snap_read_stats <- function(path, src) {
  if (!file.exists(path)) return(NULL)
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  d$year <- as.character(d$year)
  d
}

snap_load <- function(files = SNAP_FILES) {
  mt <- vapply(files, function(f) if (file.exists(f)) as.numeric(file.mtime(f)) else NA_real_, numeric(1))
  if (!is.null(.snap_cache$data) && identical(.snap_cache$mt, mt)) return(.snap_cache$data)

  rd <- function(path, cols) {
    if (!file.exists(path)) return(NULL)
    d <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    nm <- names(d); names(d) <- nm
    have <- intersect(cols, names(d))
    d[, have, drop = FALSE]
  }
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  chr <- function(x) as.character(x)

  lq <- rd(files$lqas, c("country", "province", "district", "response", "roundNumber", "round_start_date",
                         "total_sampled", "total_vaccinated", "number_clusters"))
  im <- rd(files$im, c("country", "province", "district", "response", "roundnumber", "round_start_date",
                       "number_of_hh_visited", "u5_present", "u5_fm"))
  ad <- rd(files$admin, c("Country", "Province", "District", "Response", "Round_Add", "Vaccine_type", "SIA_date",
                          "TotalNbrVaccPolio", "CVPolio"))
  im_pub <- tryCatch(attr(readRDS(files$im), "im_published_at"), error = function(e) NULL)

  # reference country names = the repository spelling
  ref_names <- unique(c(if (!is.null(lq)) toupper(trimws(chr(lq$country))),
                        if (!is.null(im)) toupper(trimws(chr(im$country)))))
  ref_names <- ref_names[!is.na(ref_names) & nzchar(ref_names)]
  ref <- stats::setNames(ref_names, snap_norm_key(ref_names))

  yr <- function(d) { d <- suppressWarnings(as.Date(d)); as.integer(format(d, "%Y")) }

  L <- if (!is.null(lq)) data.frame(
    src = "LQAS", country = snap_canon_country(lq$country, ref), province = chr(lq$province),
    district = chr(lq$district), response = chr(lq$response), round = chr(lq$roundNumber),
    date = suppressWarnings(as.Date(lq$round_start_date)),
    sampled = num(lq$total_sampled), vaccinated = num(lq$total_vaccinated),
    hh = NA_real_, stringsAsFactors = FALSE) else NULL
  I <- if (!is.null(im)) data.frame(
    src = "IM", country = snap_canon_country(im$country, ref), province = chr(im$province),
    district = chr(im$district), response = chr(im$response), round = toupper(chr(im$roundnumber)),
    date = suppressWarnings(as.Date(im$round_start_date)),
    sampled = num(im$u5_present), vaccinated = num(im$u5_fm),
    hh = num(im$number_of_hh_visited), stringsAsFactors = FALSE) else NULL
  if (!is.null(L)) { L$year <- yr(L$date); L$round <- toupper(L$round) }
  if (!is.null(I)) I$year <- yr(I$date)
  M <- do.call(rbind, Filter(Negate(is.null), list(L, I)))
  if (!is.null(M)) {
    M$pkey <- snap_keyp(M$country, M$province)
    M$dkey <- snap_keyp(M$country, M$province, M$district, optional = 2)
    M$ckey <- snap_keyp(M$country, M$response)
    M$rkey <- snap_keyp(M$country, M$response, M$round)
  }

  A <- if (!is.null(ad)) {
    a <- data.frame(
      country = snap_canon_country(ad$Country, ref), province = chr(ad$Province), district = chr(ad$District),
      response = trimws(chr(ad$Response)), round = toupper(chr(ad$Round_Add)),
      fam = snap_vfam(ad$Vaccine_type), date = suppressWarnings(as.Date(ad$SIA_date)),
      doses = num(ad$TotalNbrVaccPolio), cv = num(ad$CVPolio), stringsAsFactors = FALSE)
    a$year <- yr(a$date)
    a$month <- format(a$date, "%Y-%m")
    a$ckey <- snap_keyp(a$country, a$response)
    a$rkey <- snap_keyp(a$country, a$response, a$round)
    a$dkey <- snap_keyp(a$country, a$province, a$district, optional = 2)
    a
  } else NULL

  st <- do.call(rbind, Filter(Negate(is.null), list(snap_read_stats(files$stats_im, "IM"),
                                                   snap_read_stats(files$stats_lqas, "LQAS"))))
  if (!is.null(st)) st$country <- snap_canon_country(st$country, ref)

  out <- list(M = M, A = A, S = st, im_published = im_pub, ref = ref)
  .snap_cache$data <- out
  .snap_cache$mt <- mt
  out
}

# ---- fallback blocks (used only if the app does not pass afro_blocks) -------
.snap_default_blocks <- list(
  LCB = c("NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC"),
  WA  = c("ALGERIA", "BURKINA FASO", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN",
          "COTE D IVOIRE", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "GAMBIA", "SENEGAL"),
  ESA = c("BOTSWANA", "BURUNDI", "ERITREA", "ESWATINI", "ETHIOPIA", "KENYA", "LESOTHO", "MADAGASCAR",
          "MALAWI", "MAURITIUS", "MOZAMBIQUE", "NAMIBIA", "RWANDA", "SEYCHELLES", "SOUTH AFRICA",
          "SOUTH SUDAN", "UNITED REPUBLIC OF TANZANIA", "UGANDA", "ZAMBIA", "ZIMBABWE"),
  DRC = c("ANGOLA", "DEMOCRATIC REPUBLIC OF THE CONGO"),
  ECA = c("CONGO", "GABON", "EQUATORIAL GUINEA"))

# ---- CSS ---------------------------------------------------------------------
SNAP_CSS <- "
.snap-wrap { padding: 4px 6px 30px 6px; color: var(--kpi-text, #111827); }
.snap-hero { display: flex; flex-wrap: wrap; justify-content: space-between; align-items: flex-end;
  gap: 12px; padding: 18px 22px; border-radius: 16px; margin-bottom: 14px;
  background: linear-gradient(120deg, #009ADE 0%, #0072B2 55%, #00507f 100%); color: #fff; }
.snap-hero h2 { margin: 0 0 4px 0; font-size: 26px; font-weight: 700; color: #fff; letter-spacing: .2px; }
.snap-hero p { margin: 0; font-size: 13px; opacity: .92; }
.snap-asof { font-size: 12px; opacity: .85; margin-top: 6px; }
.snap-actions .btn { margin-left: 6px; border-radius: 8px; font-weight: 600; }
.snap-filters { display: flex; flex-wrap: wrap; gap: 14px; align-items: flex-end; padding: 12px 16px 2px 16px;
  margin-bottom: 14px; border-radius: 14px; background: var(--kpi-bg, #fff);
  border: 1px solid var(--kpi-border, rgba(37,99,235,.18)); box-shadow: var(--kpi-shadow, none); }
.snap-filters .form-group { margin-bottom: 10px; min-width: 190px; }
.snap-filters label { font-size: 11px; letter-spacing: .6px; text-transform: uppercase; color: var(--kpi-subtext, #6b7280); }
.snap-sec { display: flex; align-items: center; gap: 10px; margin: 18px 2px 10px 2px; }
.snap-sec h3 { margin: 0; font-size: 15px; font-weight: 700; letter-spacing: .8px; text-transform: uppercase;
  color: var(--kpi-text, #111827); }
.snap-sec .snap-rule { flex: 1; height: 1px; background: var(--kpi-border, rgba(37,99,235,.18)); }
.snap-sec .snap-tag { font-size: 11px; padding: 2px 8px; border-radius: 999px; color: var(--kpi-subtext, #6b7280);
  border: 1px solid var(--kpi-border, rgba(37,99,235,.18)); }
.snap-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); gap: 12px; }
.snap-card { position: relative; border-radius: 14px; padding: 14px 16px 12px 16px; overflow: hidden;
  background: var(--kpi-bg, #fff); border: 1px solid var(--kpi-border, rgba(37,99,235,.18));
  border-top: 4px solid var(--accent, #009ADE); box-shadow: var(--kpi-shadow, none); min-height: 112px; }
.snap-card .snap-ico { position: absolute; right: 12px; top: 12px; font-size: 22px; color: var(--accent, #009ADE); opacity: .35; }
.snap-label { font-size: 11px; font-weight: 700; letter-spacing: .8px; text-transform: uppercase;
  color: var(--kpi-subtext, #6b7280); padding-right: 30px; }
.snap-value { font-size: 32px; line-height: 1.15; font-weight: 800; margin: 4px 0 2px 0; color: var(--kpi-text, #111827);
  font-variant-numeric: tabular-nums; }
.snap-sub { font-size: 12px; color: var(--kpi-subtext, #6b7280); line-height: 1.35; }
.snap-sub b { color: var(--kpi-text, #111827); font-weight: 600; }
.snap-meter { height: 6px; border-radius: 99px; margin-top: 8px; background: rgba(148,163,184,.28); overflow: hidden; }
.snap-meter > span { display: block; height: 100%; border-radius: 99px; background: var(--accent, #009ADE); }
.snap-grid-6 { grid-template-columns: repeat(auto-fit, minmax(178px, 1fr)); }
.snap-split-1 { grid-template-columns: 1fr !important; }
.snap-split { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 12px; margin-top: 12px; }
.snap-panel { border-radius: 14px; padding: 14px 16px; background: var(--kpi-bg, #fff);
  border: 1px solid var(--kpi-border, rgba(37,99,235,.18)); box-shadow: var(--kpi-shadow, none); }
.snap-panel h4 { margin: 0 0 10px 0; font-size: 12px; font-weight: 700; letter-spacing: .8px; text-transform: uppercase;
  color: var(--kpi-subtext, #6b7280); }
.snap-stack { display: flex; height: 22px; border-radius: 8px; overflow: hidden; gap: 2px; margin: 4px 0 12px 0; }
.snap-stack > div { min-width: 2px; }
.snap-legend { display: grid; gap: 6px; max-width: 560px; }
.snap-legend .snap-lrow { display: grid; grid-template-columns: 1fr auto 64px; align-items: center; font-size: 13px; column-gap: 12px; }
.snap-legend.inline { display: flex; gap: 16px; }
.snap-legend.inline .snap-lrow { display: block; }
.snap-legend .dot { width: 10px; height: 10px; border-radius: 3px; display: inline-block; margin-right: 8px; }
.snap-legend .n { font-weight: 700; font-variant-numeric: tabular-nums; }
.snap-legend .p { color: var(--kpi-subtext, #6b7280); text-align: right; display: block; }
.snap-bars { display: flex; align-items: flex-end; gap: 10px; padding-top: 6px; }
.snap-bar { flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: flex-end; min-width: 0; }
.snap-bar-val { font-size: 11px; font-weight: 700; margin-bottom: 3px; white-space: nowrap; font-variant-numeric: tabular-nums; }
.snap-bar-track { width: 100%; max-width: 46px; height: 140px; flex: none; display: flex; flex-direction: column; justify-content: flex-end; }
.snap-bar-fill { width: 100%; border-radius: 5px 5px 0 0; min-height: 2px; }
.snap-bar-fill + .snap-bar-fill { margin-top: 2px; border-radius: 0; }
.snap-bar-lab { font-size: 11px; margin-top: 5px; color: var(--kpi-subtext, #6b7280); }
.snap-note { font-size: 12px; color: var(--kpi-subtext, #6b7280); margin-top: 10px; line-height: 1.5; }
.snap-note details { margin-top: 6px; }
.snap-note summary { cursor: pointer; font-weight: 600; color: var(--kpi-text, #111827); }
.snap-note li { margin-bottom: 3px; }
.snap-pending { opacity: .75; }
@media print {
  .main-header, .main-sidebar, .snap-filters, .snap-actions, .dark-mode-toggle { display: none !important; }
  .content-wrapper, .right-side { margin-left: 0 !important; background: #fff !important; }
  .snap-card, .snap-panel { break-inside: avoid; box-shadow: none !important; }
  html, body, .snap-wrap, .snap-wrap * { -webkit-print-color-adjust: exact !important; print-color-adjust: exact !important; }
  .snap-wrap { --kpi-bg: #ffffff; --kpi-text: #111827; --kpi-subtext: #6b7280; --kpi-border: rgba(37,99,235,.25); color: #111827; }
  body.dark-mode, body.dark-mode .content-wrapper, body.dark-mode .content { background: #fff !important; }
  .snap-panel, .snap-sec { break-inside: avoid; page-break-inside: avoid; }
  /* Bootstrap 3 print CSS sets background:transparent !important on everything; re-assert fills */
  .snap-wrap .snap-bar-fill, .snap-wrap .snap-seg, .snap-wrap .snap-legend .dot { background: var(--c) !important; }
  .snap-wrap .snap-meter > span { background: var(--accent) !important; }
  .snap-wrap .snap-meter { background: #e5e7eb !important; }
  .snap-wrap .snap-card, .snap-wrap .snap-panel { background: #fff !important; }
  .snap-hero { break-inside: avoid; background: #fff !important; color: #111827 !important; border-bottom: 4px solid #0072B2; border-radius: 0; padding: 8px 4px 10px 4px; }
  .snap-hero h2 { color: #0b3c5d !important; }
  .snap-hero p, .snap-asof { color: #374151 !important; opacity: 1 !important; }
  .content { padding: 0 !important; }
}
"

snap_card <- function(label, value, sub = NULL, icon = "chart-bar", accent = "#009ADE", full = NULL, meter = NULL, pending = FALSE) {
  shiny::tags$div(
    class = paste("snap-card", if (pending) "snap-pending"), style = paste0("--accent:", accent, ";"),
    shiny::tags$div(class = "snap-ico", shiny::icon(icon)),
    shiny::tags$div(class = "snap-label", label),
    shiny::tags$div(class = "snap-value", title = full, value),
    if (!is.null(sub)) shiny::tags$div(class = "snap-sub", sub),
    if (!is.null(meter) && !is.na(meter)) shiny::tags$div(class = "snap-meter",
      shiny::tags$span(style = sprintf("width:%.1f%%;", max(0, min(100, 100 * meter)))))
  )
}

snap_section <- function(title, tag = NULL) {
  shiny::tags$div(class = "snap-sec", shiny::tags$h3(title), shiny::tags$div(class = "snap-rule"),
                  if (!is.null(tag)) shiny::tags$span(class = "snap-tag", tag))
}

snap_bars <- function(labels, series, colors, fmt = snap_fmt, totals = TRUE, names = NULL) {
  # series: list of numeric vectors (stacked); colors: vector
  tot <- Reduce(`+`, lapply(series, function(s) ifelse(is.na(s), 0, s)))
  mx <- max(c(tot, 0), na.rm = TRUE); if (!is.finite(mx) || mx <= 0) mx <- 1
  bars <- lapply(seq_along(labels), function(i) {
    segs <- lapply(seq_along(series), function(j) {
      v <- series[[j]][i]; v <- if (is.na(v)) 0 else v
      shiny::tags$div(class = "snap-bar-fill",
        title = paste0(if (!is.null(names)) paste0(names[j], ": ") else "", fmt(v), " (", labels[i], ")"),
        style = sprintf("height:%.1fpx;background:%s;--c:%s;", max(2, 140 * v / mx), colors[j], colors[j]))
    })
    shiny::tags$div(class = "snap-bar",
      shiny::tags$div(class = "snap-bar-val", if (totals) fmt(tot[i])),
      shiny::tags$div(class = "snap-bar-track", rev(segs)),
      shiny::tags$div(class = "snap-bar-lab", labels[i]))
  })
  shiny::tags$div(class = "snap-bars", bars)
}

# ==============================================================================
# UI
# ==============================================================================
snap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(SNAP_CSS)),
      shiny::tags$script(shiny::HTML("window.addEventListener('beforeprint', function(){ document.querySelectorAll('.snap-wrap details').forEach(function(d){ d.setAttribute('open',''); }); });"))),
    shiny::div(class = "snap-wrap",
      shiny::div(class = "snap-hero",
        shiny::div(
          shiny::tags$h2("Polio SIA Monitoring Snapshot"),
          shiny::tags$p("WHO AFRO | Independent monitoring (LQAS & IM) and campaign delivery"),
          shiny::uiOutput(ns("asof"))),
        shiny::div(class = "snap-actions",
          shiny::tags$button(type = "button", class = "btn btn-default", onclick = "window.print()",
                             shiny::icon("print"), " Print / PDF"),
          shiny::downloadButton(ns("dl"), "Snapshot (CSV)", class = "btn-default"))),
      shiny::div(class = "snap-filters",
        shiny::selectizeInput(ns("year"), "Year", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All years")),
        shiny::selectInput(ns("block"), "AFRO block", choices = "All"),
        shiny::selectizeInput(ns("country"), "Country", choices = NULL, multiple = TRUE,
                              options = list(placeholder = "All countries")),
        shiny::actionButton(ns("reset"), "Reset", icon = shiny::icon("undo"), class = "btn-default",
                            style = "margin-bottom:10px;")),
      shiny::uiOutput(ns("sec_im")),
      shiny::uiOutput(ns("sec_admin")),
      shiny::uiOutput(ns("sec_charts")),
      shiny::uiOutput(ns("notes"))
    )
  )
}

# ==============================================================================
# Server
# ==============================================================================
snap_server <- function(id, blocks = NULL, files = SNAP_FILES) {
  shiny::moduleServer(id, function(input, output, session) {
    D <- snap_load(files)
    M <- D$M; A <- D$A; S <- D$S
    blocks <- if (is.null(blocks)) .snap_default_blocks else blocks

    all_countries <- sort(unique(c(M$country, A$country, S$country)))
    all_years <- sort(unique(c(M$year, A$year)), decreasing = TRUE)
    all_years <- all_years[!is.na(all_years) & all_years >= 2020]
    block_choices <- c("All", names(blocks)[vapply(names(blocks), function(b) any(blocks[[b]] %in% all_countries), logical(1))])

    shiny::updateSelectizeInput(session, "year", choices = all_years, server = FALSE)
    shiny::updateSelectInput(session, "block", choices = block_choices, selected = "All")
    shiny::updateSelectizeInput(session, "country", choices = all_countries, server = FALSE)

    shiny::observeEvent(input$block, {
      ch <- if (is.null(input$block) || input$block == "All") all_countries else intersect(all_countries, blocks[[input$block]])
      shiny::updateSelectizeInput(session, "country", choices = ch, selected = intersect(shiny::isolate(input$country), ch))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$reset, {
      shiny::updateSelectizeInput(session, "year", selected = character(0))
      shiny::updateSelectInput(session, "block", selected = "All")
      shiny::updateSelectizeInput(session, "country", choices = all_countries, selected = character(0))
    })

    # ---- selection -----------------------------------------------------------------
    sel_countries <- shiny::reactive({
      if (length(input$country)) return(input$country)
      if (!is.null(input$block) && input$block != "All") return(intersect(all_countries, blocks[[input$block]]))
      NULL
    })
    sel_years <- shiny::reactive({ y <- suppressWarnings(as.integer(input$year)); if (length(y)) y else NULL })

    by_country <- function(d) {
      sc <- sel_countries()
      if (is.null(d) || is.null(sc)) d else d[d$country %in% sc, , drop = FALSE]
    }
    by_year <- function(d, col = "year") {
      sy <- sel_years()
      if (is.null(d) || is.null(sy)) d else d[!is.na(d[[col]]) & d[[col]] %in% sy, , drop = FALSE]
    }
    Mc <- shiny::reactive(by_country(M))           # country filtered (all years) -> charts
    Ac <- shiny::reactive(by_country(A))
    Sc <- shiny::reactive(by_country(S))
    Mf <- shiny::reactive(by_year(Mc()))
    Af <- shiny::reactive(by_year(Ac()))
    Sf <- shiny::reactive({
      s <- Sc(); if (is.null(s)) return(NULL)
      sy <- sel_years()
      if (is.null(sy)) s[s$year == "ALL", , drop = FALSE] else s[s$year %in% as.character(sy), , drop = FALSE]
    })

    # ---- metrics ----------------------------------------------------------------------
    nd <- function(x) length(unique(x[!is.na(x) & nzchar(x)]))

    im_metrics <- shiny::reactive({
      m <- Mf(); s <- Sf()
      r <- list()
      if (!is.null(m) && nrow(m)) {
        l <- m[m$src == "LQAS", , drop = FALSE]; i <- m[m$src == "IM", , drop = FALSE]
        r$countries <- nd(m$country); r$countries_l <- nd(l$country); r$countries_i <- nd(i$country)
        r$provinces <- nd(m$pkey); r$districts <- nd(m$dkey)
        r$districts_l <- nd(l$dkey); r$districts_i <- nd(i$dkey)
        r$hh <- sum(i$hh, na.rm = TRUE)
        r$sampled <- sum(m$sampled, na.rm = TRUE)
        r$sampled_l <- sum(l$sampled, na.rm = TRUE); r$sampled_i <- sum(i$sampled, na.rm = TRUE)
        r$vacc <- sum(m$vaccinated, na.rm = TRUE)
        r$vacc_l <- sum(l$vaccinated, na.rm = TRUE); r$vacc_i <- sum(i$vaccinated, na.rm = TRUE)
        r$campaigns <- nd(m$ckey); r$rounds <- nd(m$rkey)
        r$campaigns_l <- nd(l$ckey); r$campaigns_i <- nd(i$ckey)
        r$rounds_l <- nd(l$rkey); r$rounds_i <- nd(i$rkey)
      }
      if (!is.null(s) && nrow(s)) {
        agg <- function(src, col) sum(s[[col]][s$source == src], na.rm = TRUE)
        r$sub_i <- agg("IM", "submissions"); r$sub_l <- agg("LQAS", "submissions")
        r$sub <- r$sub_i + r$sub_l
        r$sett_i <- agg("IM", "settlements"); r$sett_l <- agg("LQAS", "settlements")
        r$sett <- r$sett_i + r$sett_l
        r$sett_unnamed <- agg("IM", "settlements_unnamed")
        r$fac_i <- agg("IM", "facilities"); r$fac_l <- agg("LQAS", "facilities")
        r$fac <- r$fac_i + r$fac_l
        r$has_stats <- TRUE
      } else r$has_stats <- FALSE
      r
    })

    adm_metrics <- shiny::reactive({
      a <- Af(); r <- list()
      if (is.null(a) || !nrow(a)) return(r)
      r$doses <- sum(a$doses, na.rm = TRUE)
      # same method as the Admin Data Overview: largest single-month total per
      # country, summed across countries (a round-based proxy for unique children)
      mm <- stats::aggregate(doses ~ country + month, data = a[!is.na(a$month), ], FUN = sum)
      r$children <- if (nrow(mm)) sum(tapply(mm$doses, mm$country, max)) else NA_real_
      r$campaigns <- nd(a$ckey); r$rounds <- nd(a$rkey); r$districts <- nd(a$dkey)
      r$countries <- nd(a$country)
      cv <- a$cv[!is.na(a$cv)]
      r$cv_n <- length(cv); r$cv_95 <- sum(cv >= 0.95); r$cv_p <- if (length(cv)) r$cv_95 / length(cv) else NA_real_
      # campaign classification by vaccine family
      cl <- split(a$fam, a$ckey)
      cls <- vapply(cl, function(f) {
        n <- any(f %in% c("nopv", "both")); b <- any(f %in% c("bopv", "both"))
        if (n && b) "both" else if (n) "nopv" else if (b) "bopv" else "other"
      }, character(1))
      r$mix <- c(nopv = sum(cls == "nopv"), bopv = sum(cls == "bopv"), both = sum(cls == "both"), other = sum(cls == "other"))
      r$dmax <- suppressWarnings(max(a$date, na.rm = TRUE))
      r
    })

    yearly <- shiny::reactive({
      a <- Ac(); m <- Mc(); s <- Sc()
      yrs <- sort(unique(c(a$year, m$year)))
      yrs <- yrs[!is.na(yrs) & yrs >= 2020]
      cv <- vapply(yrs, function(y) { x <- a$cv[!is.na(a$year) & a$year == y & !is.na(a$cv)]; if (length(x)) mean(x >= 0.95) else NA_real_ }, numeric(1))
      ds <- vapply(yrs, function(y) sum(a$doses[!is.na(a$year) & a$year == y], na.rm = TRUE), numeric(1))
      sub_i <- sub_l <- rep(NA_real_, length(yrs))
      if (!is.null(s)) {
        sub_i <- vapply(yrs, function(y) sum(s$submissions[s$source == "IM" & s$year == as.character(y)], na.rm = TRUE), numeric(1))
        sub_l <- vapply(yrs, function(y) sum(s$submissions[s$source == "LQAS" & s$year == as.character(y)], na.rm = TRUE), numeric(1))
      }
      list(years = yrs, cv = cv, doses = ds, sub_i = sub_i, sub_l = sub_l)
    })

    # ---- header ----------------------------------------------------------------------
    output$asof <- shiny::renderUI({
      bits <- character(0)
      if (!is.null(A)) bits <- c(bits, paste0("Admin data to ", format(max(A$date, na.rm = TRUE), "%d %b %Y")))
      if (!is.null(M)) {
        lm <- max(M$date[M$src == "LQAS"], na.rm = TRUE); im <- max(M$date[M$src == "IM"], na.rm = TRUE)
        bits <- c(bits, paste0("LQAS rounds to ", format(lm, "%b %Y")), paste0("IM rounds to ", format(im, "%b %Y")))
      }
      if (!is.null(S)) bits <- c(bits, paste0("ONA counts generated ", substr(max(S$generated_at), 1, 10)))
      shiny::tags$div(class = "snap-asof", paste(bits, collapse = "  |  "))
    })

    # ---- section: independent monitoring ---------------------------------------------
    output$sec_im <- shiny::renderUI({
      r <- im_metrics()
      pend <- !isTRUE(r$has_stats)
      pend_sub <- shiny::tags$span("Run tools/snapshot_stats.py to publish raw ONA counts")
      cap <- function(il, ll) shiny::HTML(sprintf("IM <b>%s</b> &middot; LQAS <b>%s</b>", il, ll))
      z <- function(x) if (is.null(x) || is.na(x) || x == 0) NA_real_ else x   # 0 = not captured, show a dash
      nc <- function(x) if (is.na(z(x))) shiny::tags$span("not recorded for this selection") else NULL
      vacc_rate <- if (!is.null(r$sampled) && r$sampled > 0) r$vacc / r$sampled else NA_real_
      shiny::tagList(
        snap_section("Independent monitoring", "LQAS + IM repositories & ONA"),
        shiny::div(class = "snap-grid",
          snap_card("ONA submissions", snap_fmt(r$sub), if (pend) pend_sub else cap(snap_fmt(r$sub_i), snap_fmt(r$sub_l)),
                    "cloud-upload-alt", "#0072B2", snap_full(r$sub), pending = pend),
          snap_card("Countries", snap_fmt(r$countries), cap(snap_fmt(r$countries_i), snap_fmt(r$countries_l)),
                    "globe-africa", "#009E73", snap_full(r$countries)),
          snap_card("Provinces", snap_fmt(r$provinces), "unique, LQAS + IM", "map", "#009E73", snap_full(r$provinces)),
          snap_card("Districts", snap_fmt(r$districts), cap(snap_fmt(r$districts_i), snap_fmt(r$districts_l)),
                    "map-marker-alt", "#009E73", snap_full(r$districts)),
          snap_card("Settlements visited", snap_fmt(r$sett),
                    if (pend) pend_sub else cap(snap_fmt(r$sett_i), snap_fmt(r$sett_l)),
                    "home", "#E69F00", snap_full(r$sett), pending = pend),
          snap_card("Health facilities", snap_fmt(z(r$fac)),
                    if (pend) pend_sub else if (is.na(z(r$fac))) nc(r$fac) else cap(snap_fmt(r$fac_i), snap_fmt(r$fac_l)),
                    "clinic-medical", "#E69F00", snap_full(r$fac), pending = pend),
          snap_card("Households visited", snap_fmt(z(r$hh)), if (is.na(z(r$hh))) nc(r$hh) else "IM households", "users", "#CC79A7", snap_full(r$hh)),
          snap_card("Children sampled", snap_fmt(r$sampled), cap(snap_fmt(r$sampled_i), snap_fmt(r$sampled_l)),
                    "child", "#CC79A7", snap_full(r$sampled)),
          snap_card("Children found vaccinated", snap_fmt(r$vacc),
                    shiny::HTML(sprintf("<b>%s</b> of sampled &middot; IM %s &middot; LQAS %s", snap_pct(vacc_rate),
                                        snap_fmt(r$vacc_i), snap_fmt(r$vacc_l))),
                    "syringe", "#CC79A7", snap_full(r$vacc), meter = vacc_rate)
        )
      )
    })

    # ---- section: campaign delivery -------------------------------------------------
    output$sec_admin <- shiny::renderUI({
      r <- adm_metrics()
      if (!length(r)) return(shiny::tagList(snap_section("Campaign delivery", "Administrative data"),
        shiny::div(class = "alert alert-warning", "No administrative data for this selection.")))
      mix <- r$mix; tot <- sum(mix[c("nopv", "bopv", "both")])
      pc <- function(x) if (tot > 0) x / tot else NA_real_
      cols <- c(nopv = "#0072B2", bopv = "#D55E00", both = "#009E73")
      lab <- c(nopv = "nOPV only", bopv = "bOPV only", both = "nOPV + bOPV (both)")
      stack <- shiny::div(class = "snap-stack", lapply(names(cols), function(k)
        shiny::div(class = "snap-seg", style = sprintf("width:%.2f%%;background:%s;--c:%s;", if (tot > 0) 100 * mix[[k]] / tot else 0, cols[[k]], cols[[k]]),
                   title = paste0(lab[[k]], ": ", mix[[k]]))))
      legend <- shiny::div(class = "snap-legend", lapply(names(cols), function(k)
        shiny::div(class = "snap-lrow",
          shiny::span(shiny::span(class = "dot", style = paste0("background:", cols[[k]], ";--c:", cols[[k]], ";")), lab[[k]]),
          shiny::span(class = "n", mix[[k]]), shiny::span(class = "p", snap_pct(pc(mix[[k]]))))))
      other_txt <- if (mix[["other"]] > 0) paste0(mix[["other"]], " other campaign(s) (e.g. mOPV2) not shown.") else NULL
      shiny::tagList(
        snap_section("Campaign delivery", "Administrative data"),
        shiny::div(class = "snap-grid snap-grid-6",
          snap_card("Doses administered", snap_fmt(r$doses), "all rounds summed", "syringe", "#0072B2", snap_full(r$doses)),
          snap_card("Children vaccinated", snap_fmt(r$children),
                    "largest monthly total per country, summed", "child", "#0072B2", snap_full(r$children)),
          snap_card("Campaigns implemented", snap_fmt(r$campaigns), paste0(snap_full(r$countries), " countries"),
                    "bullhorn", "#009E73", snap_full(r$campaigns)),
          snap_card("Rounds implemented", snap_fmt(r$rounds), "campaign rounds", "redo-alt", "#009E73", snap_full(r$rounds)),
          snap_card("Districts reached", snap_fmt(r$districts), "unique, any round", "map-marker-alt", "#009E73", snap_full(r$districts)),
          snap_card("District-rounds >= 95% coverage", snap_pct(r$cv_p),
                    shiny::HTML(sprintf("<b>%s</b> of %s with admin coverage", snap_full(r$cv_95), snap_full(r$cv_n))),
                    "bullseye", "#E69F00", meter = r$cv_p)),
        shiny::div(class = "snap-split snap-split-1",
          shiny::div(class = "snap-panel", shiny::tags$h4("Campaigns by vaccine (nOPV only / bOPV only / both)"), stack, legend,
                     if (!is.null(other_txt)) shiny::div(class = "snap-note", other_txt))
        )
      )
    })

    # ---- section: trends ------------------------------------------------------------------
    output$sec_charts <- shiny::renderUI({
      y <- yearly()
      if (!length(y$years)) return(NULL)
      lab <- as.character(y$years)
      shiny::tagList(
        snap_section("Trends by year", "country filter applies, year filter does not"),
        shiny::div(class = "snap-split",
          shiny::div(class = "snap-panel", shiny::tags$h4("Doses administered (millions)"),
            snap_bars(lab, list(y$doses / 1e6), "#0072B2", fmt = function(v) sprintf("%.0f", v))),
          shiny::div(class = "snap-panel", shiny::tags$h4("District-rounds >= 95% admin coverage"),
            snap_bars(lab, list(y$cv), "#E69F00", fmt = function(v) if (is.na(v)) "" else sprintf("%.0f%%", 100 * v))),
          if (any(!is.na(y$sub_i) & y$sub_i > 0 | !is.na(y$sub_l) & y$sub_l > 0))
            shiny::div(class = "snap-panel", shiny::tags$h4("ONA submissions per year"),
              snap_bars(lab, list(y$sub_i, y$sub_l), c("#CC79A7", "#56B4E9"), names = c("IM", "LQAS")),
              shiny::div(class = "snap-legend inline", style = "margin-top:8px;",
                shiny::div(class = "snap-lrow", shiny::span(shiny::span(class = "dot", style = "background:#CC79A7;--c:#CC79A7;"), "IM")),
                shiny::div(class = "snap-lrow", shiny::span(shiny::span(class = "dot", style = "background:#56B4E9;--c:#56B4E9;"), "LQAS"))))
        )
      )
    })

    output$notes <- shiny::renderUI({
      shiny::div(class = "snap-note",
        shiny::tags$details(
          shiny::tags$summary("How these numbers are defined"),
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML("<b>ONA submissions</b>: raw ONA submissions of all IM and LQAS forms, de-duplicated by submission id, 2020 onwards, test records removed.")),
            shiny::tags$li(shiny::HTML("<b>Settlements visited</b>: IM = unique named settlements per district, plus one per submission where the country form does not record a settlement name; LQAS = clusters (a cluster is the settlement/village sampled).")),
            shiny::tags$li(shiny::HTML("<b>Health facilities</b>: unique facility names recorded on the forms (not every country form captures a facility); IM and LQAS counted separately.")),
            shiny::tags$li(shiny::HTML("<b>Households</b>: IM households visited. <b>Children sampled / vaccinated</b>: LQAS children sampled and found vaccinated plus IM children under five present and finger-marked.")),
            shiny::tags$li(shiny::HTML("<b>Countries, provinces, districts</b>: unique values across the LQAS and IM repositories (names matched ignoring accents and punctuation). Settlements and facilities are counted per source, so a facility monitored by both IM and LQAS is counted in each.")),
            shiny::tags$li(shiny::HTML("<b>Doses administered</b>: total vaccinated in the administrative data, all rounds. <b>Children vaccinated</b>: same method as the Admin Data Overview (largest monthly total per country, summed), which avoids counting a child again in every round.")),
            shiny::tags$li(shiny::HTML("<b>95% coverage</b>: share of district-rounds with administrative coverage of at least 95%.")),
            shiny::tags$li(shiny::HTML("<b>Campaign</b>: unique country + response. A campaign is <i>both</i> when it used nOPV and bOPV (in the same or different rounds).")))))
    })

    # ---- csv ----------------------------------------------------------------------------------
    snapshot_table <- shiny::reactive({
      r <- im_metrics(); a <- adm_metrics()
      f <- function(sec, metric, val, note = "") data.frame(Section = sec, Metric = metric, Value = val, Note = note, stringsAsFactors = FALSE)
      sel <- paste0("Year: ", if (length(input$year)) paste(input$year, collapse = ",") else "All",
                    " | Block: ", input$block, " | Countries: ", if (length(input$country)) paste(input$country, collapse = ", ") else "All")
      rows <- list(
        f("Selection", "Filters", NA, sel),
        f("Independent monitoring", "ONA submissions (IM + LQAS)", r$sub),
        f("Independent monitoring", "ONA submissions - IM", r$sub_i), f("Independent monitoring", "ONA submissions - LQAS", r$sub_l),
        f("Independent monitoring", "Countries", r$countries), f("Independent monitoring", "Provinces", r$provinces),
        f("Independent monitoring", "Districts", r$districts),
        f("Independent monitoring", "Settlements visited", r$sett, "IM: unnamed submissions counted once; LQAS: clusters"),
        f("Independent monitoring", "Health facilities", r$fac), f("Independent monitoring", "Households visited (IM)", r$hh),
        f("Independent monitoring", "Children sampled", r$sampled), f("Independent monitoring", "Children vaccinated", r$vacc),
        f("Campaign delivery", "Doses administered", a$doses), f("Campaign delivery", "Children vaccinated", a$children, "Admin Data Overview method"),
        f("Campaign delivery", "Campaigns implemented", a$campaigns), f("Campaign delivery", "Rounds implemented", a$rounds),
        f("Campaign delivery", "Districts reached", a$districts),
        f("Campaign delivery", "District-rounds with coverage >= 95% (n)", a$cv_95),
        f("Campaign delivery", "District-rounds with coverage value (n)", a$cv_n),
        f("Campaign delivery", "Share of district-rounds >= 95%", round(100 * a$cv_p, 1), "percent"),
        f("Campaign delivery", "Campaigns - nOPV only", unname(a$mix["nopv"])),
        f("Campaign delivery", "Campaigns - bOPV only", unname(a$mix["bopv"])),
        f("Campaign delivery", "Campaigns - nOPV + bOPV", unname(a$mix["both"])))
      do.call(rbind, rows)
    })
    output$dl <- shiny::downloadHandler(
      filename = function() paste0("AFRO_SIA_Snapshot_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(snapshot_table(), file, row.names = FALSE, na = ""))
  })
}

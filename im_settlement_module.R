# ==============================================================================
# im_settlement_module.R -- IM Settlement Maps (interactive explorer + printable map)
# ASCII-only on purpose (use \uXXXX escapes) so it parses in any locale.
# Data: input/<formid>.csv.gz (built by tools/build_settlement_inputs.py; .csv and .rds also read; one file per country; one row = one monitor route that
# found missed children: start/end GPS, households, reasons, total children missed).
# ==============================================================================

IMSET_REASONS <- c(REASON_ABSENT = "Absent", REASON_NC = "Non-compliance",
                   REASON_NOTVISITED = "Not visited", REASON_ASLEEP = "Asleep",
                   REASON_VACCINATEDROUTINE = "Vaccinated (routine)", REASON_OTHERS = "Other")

# Generous country bounding boxes (lat_min, lat_max, lon_min, lon_max); others -> Africa box
IMSET_BOX <- list(
  "ALGERIA" = c(18.9, 37.2, -8.8, 12.1),
  "ANGOLA" = c(-18.1, -4.3, 11.6, 24.2),
  "BENIN" = c(6.0, 12.5, 0.7, 3.9),
  "BOTSWANA" = c(-27.0, -17.7, 19.9, 29.5),
  "BURKINA FASO" = c(9.3, 15.2, -5.6, 2.5),
  "BURUNDI" = c(-4.5, -2.3, 28.9, 30.9),
  "CAMEROON" = c(1.6, 13.2, 8.3, 16.3),
  "CENTRAL AFRICAN REPUBLIC" = c(2.1, 11.1, 14.3, 27.6),
  "CHAD" = c(7.3, 23.6, 13.4, 24.1),
  "CONGO" = c(-5.1, 3.8, 11.0, 18.7),
  "COTE D IVOIRE" = c(4.2, 10.8, -8.7, -2.4),
  "DEMOCRATIC REPUBLIC OF THE CONGO" = c(-13.6, 5.5, 12.0, 31.4),
  "ETHIOPIA" = c(3.3, 15.0, 32.9, 48.1),
  "GHANA" = c(4.5, 11.3, -3.4, 1.3),
  "GUINEA" = c(7.1, 12.8, -15.2, -7.6),
  "GUINEA-BISSAU" = c(10.8, 12.8, -16.8, -13.6),
  "KENYA" = c(-4.8, 5.1, 33.8, 42.0),
  "LIBERIA" = c(4.2, 8.6, -11.6, -7.3),
  "MADAGASCAR" = c(-25.7, -11.9, 43.1, 50.6),
  "MALAWI" = c(-17.2, -9.3, 32.6, 36.0),
  "MAURITANIA" = c(14.6, 27.4, -17.2, -4.7),
  "MOZAMBIQUE" = c(-27.0, -10.4, 30.1, 41.0),
  "NAMIBIA" = c(-29.1, -16.9, 11.6, 25.4),
  "NIGER" = c(11.6, 23.6, 0.0, 16.1),
  "NIGERIA" = c(4.2, 14.0, 2.6, 14.8),
  "RWANDA" = c(-3.0, -1.0, 28.8, 31.0),
  "SENEGAL" = c(12.2, 16.8, -17.6, -11.3),
  "SIERRA LEONE" = c(6.8, 10.1, -13.4, -10.2),
  "SOUTH SUDAN" = c(3.4, 12.3, 23.3, 35.9),
  "TOGO" = c(5.9, 11.2, -0.2, 1.9),
  "UGANDA" = c(-1.6, 4.3, 29.5, 35.1),
  "UNITED REPUBLIC OF TANZANIA" = c(-11.8, -0.9, 29.2, 40.5),
  "ZAMBIA" = c(-18.2, -8.1, 21.9, 33.8),
  "ZIMBABWE" = c(-22.5, -15.5, 25.1, 33.1)
)
IMSET_AFRICA <- c(-35.0, 38.0, -20.0, 55.0)
IMSET_MAX_ROUTE_M <- 5000   # start-to-end distance above which a route is treated as suspect
IMSET_MAX_CHILDREN <- 100   # children missed on ONE route above this is treated as a data-entry error
IMSET_MAX_DRAW <- 12000     # max routes drawn in the interactive map

imset_clean_txt <- function(x, underscores = TRUE) {
  x <- toupper(trimws(as.character(x)))
  if (underscores) x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  x
}

imset_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("[^0-9.-]", "", as.character(x))
  suppressWarnings(as.numeric(ifelse(x == "", NA, x)))
}

imset_haversine_m <- function(lat1, lon1, lat2, lon2) {
  r <- 6371000; rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# one file id -> best file on disk (csv.gz > csv > rds)
imset_files <- function(dir = "input") {
  f <- list.files(dir, pattern = "^[0-9]+\\.(csv\\.gz|csv|rds)$", full.names = TRUE)
  if (!length(f)) return(character(0))
  id <- sub("\\.(csv\\.gz|csv|rds)$", "", basename(f))
  rank <- ifelse(grepl("\\.csv\\.gz$", f), 1L, ifelse(grepl("\\.csv$", f), 2L, 3L))
  f <- f[order(id, rank)]
  f[!duplicated(sub("\\.(csv\\.gz|csv|rds)$", "", basename(f)))]
}

# path of the best file for one file id ("" if none); used by the legacy Settlement Data tab
imset_find_file <- function(dir, id) {
  for (ext in c(".csv.gz", ".csv", ".rds")) {
    f <- file.path(dir, paste0(id, ext))
    if (file.exists(f)) return(f)
  }
  ""
}

# raw table with upper-case names (csv/csv.gz read with numeric columns typed up front)
imset_read_raw <- function(path, n_max = Inf) {
  if (grepl("\\.rds$", path)) {
    d <- suppressWarnings(readRDS(path))
    if (is.finite(n_max)) d <- utils::head(d, n_max)
  } else {
    hdr <- names(utils::read.csv(path, nrows = 1, header = TRUE, check.names = FALSE, encoding = "UTF-8"))
    isnum <- grepl("^(lat|lon|lat_end|lon_end|household_[0-9]+|reason_[a-z]+|total_child_missed|invalid_hh_cells)$",
                   hdr, ignore.case = TRUE)
    cc <- ifelse(isnum, "numeric", "character")
    d <- utils::read.csv(path, header = TRUE, colClasses = cc, na.strings = c("", "NA"), nrows = if (is.finite(n_max)) n_max else -1,
                         check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8", comment.char = "", quote = "\"")
  }
  names(d) <- toupper(names(d))
  d
}

imset_read_one <- function(path) {
  d <- imset_read_raw(path)
  need <- c("COUNTRY", "LAT", "LON", "LAT_END", "LON_END")
  if (!all(need %in% names(d))) return(NULL)
  chr <- function(col) if (col %in% names(d)) d[[col]] else NA_character_
  num <- function(col) if (col %in% names(d)) imset_num(d[[col]]) else rep(NA_real_, nrow(d))
  out <- data.frame(
    FILE_ID  = sub("\\.(csv\\.gz|csv|rds)$", "", basename(path)),
    COUNTRY  = imset_clean_txt(chr("COUNTRY")),
    PROVINCE = imset_clean_txt(chr("PROVINCE")),
    DISTRICT = imset_clean_txt(chr("DISTRICT")),
    FACILITY = imset_clean_txt(chr("FACILITY")),
    SETTLEMENT = imset_clean_txt(chr("SETTLEMENT")),
    RESPONSE = imset_clean_txt(chr("RESPONSE"), underscores = FALSE),
    ROUND    = gsub(" ", "", imset_clean_txt(chr("ROUNDNUMBER"), underscores = FALSE), fixed = TRUE),
    stringsAsFactors = FALSE
  )
  # monitoring type: "IN" = in process (default; also files without the column), "END" = end process
  mon_raw <- toupper(gsub("[^A-Za-z]", "", as.character(chr("MONITORING"))))
  out$MONITORING <- ifelse(!is.na(mon_raw) & mon_raw == "ENDPROCESS", "END", "IN")
  out$RESPONSE[is.na(out$RESPONSE)] <- "NOT RECORDED"
  out$ROUND[is.na(out$ROUND)] <- "NOT RECORDED"
  dt <- suppressWarnings(as.Date(substr(as.character(chr("DATE_MONITORED")), 1, 10)))
  dt[!is.na(dt) & dt < as.Date("2018-01-01")] <- NA   # 2010-01-01 placeholder dates
  out$DATE <- dt
  out$YEAR <- as.integer(format(dt, "%Y"))
  out$LAT <- num("LAT"); out$LON <- num("LON"); out$LAT_END <- num("LAT_END"); out$LON_END <- num("LON_END")
  out$CHILDREN <- num("TOTAL_CHILD_MISSED")
  hh <- grep("^HOUSEHOLD_[0-9]+$", names(d), value = TRUE)
  hh <- hh[order(as.integer(sub("HOUSEHOLD_", "", hh)))]
  for (h in hh) out[[h]] <- imset_num(d[[h]])
  for (r in names(IMSET_REASONS)) out[[r]] <- if (r %in% names(d)) imset_num(d[[r]]) else NA_real_
  hcols <- hh
  out$HH_MISSED <- if (length(hcols)) rowSums(out[, hcols, drop = FALSE] > 0, na.rm = TRUE) else NA_real_
  out$ROUTE_M <- imset_haversine_m(out$LAT, out$LON, out$LAT_END, out$LON_END)
  # GPS quality flag
  ctry <- out$COUNTRY[1]
  box <- if (!is.na(ctry) && !is.null(IMSET_BOX[[ctry]])) IMSET_BOX[[ctry]] else IMSET_AFRICA
  inside <- function(la, lo) !is.na(la) & !is.na(lo) & la >= box[1] & la <= box[2] & lo >= box[3] & lo <= box[4]
  ok_pos <- inside(out$LAT, out$LON) & inside(out$LAT_END, out$LON_END)
  out$QC <- ifelse(!ok_pos, "Outside country / missing GPS",
            ifelse(!is.na(out$CHILDREN) & out$CHILDREN > IMSET_MAX_CHILDREN, paste0("Implausible child count (> ", IMSET_MAX_CHILDREN, ")"),
            ifelse(!is.na(out$ROUTE_M) & out$ROUTE_M > IMSET_MAX_ROUTE_M, "Long route (> 5 km)", "OK")))
  out
}

# Country list = first row of each file (cheap); a country's rows are read only when it is picked.
imset_file_index <- local({
  cache <- NULL; sig <- NULL
  function(dir = "input") {
    files <- imset_files(dir)
    s <- paste(basename(files), file.mtime(files), collapse = "|")
    if (is.null(cache) || !identical(s, sig)) {
      rows <- lapply(files, function(f) tryCatch({
        d <- imset_read_raw(f, n_max = 1)
        if (!all(c("COUNTRY", "LAT", "LON") %in% names(d)) || !nrow(d)) NULL
        else data.frame(FILE_ID = sub("\\.(csv\\.gz|csv|rds)$", "", basename(f)),
                        COUNTRY = imset_clean_txt(d$COUNTRY[1]), PATH = f, stringsAsFactors = FALSE)
      }, error = function(e) NULL))
      rows <- rows[!vapply(rows, is.null, logical(1))]
      cache <<- if (length(rows)) do.call(rbind, rows) else NULL
      sig <<- s
    }
    cache
  }
})

# rows of one country, cleaned; the last few countries stay in memory
imset_country_data <- local({
  store <- list()
  function(dir = "input", country) {
    idx <- imset_file_index(dir)
    if (is.null(idx)) return(NULL)
    hit <- idx[!is.na(idx$COUNTRY) & idx$COUNTRY == country, , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    key <- paste(hit$PATH[1], file.mtime(hit$PATH[1]))
    if (is.null(store[[key]])) {
      d <- imset_read_one(hit$PATH[1])
      if (is.null(d)) return(NULL)
      store[[key]] <<- d
      if (length(store) > 3) store <<- store[utils::tail(seq_along(store), 3)]
    }
    store[[key]]
  }
})

imset_country_choices <- function(idx) {
  v <- sort(unique(stats::na.omit(idx$COUNTRY)))
  stats::setNames(v, tools::toTitleCase(tolower(v)))
}

# named file-id choices for the legacy "Settlement Data" tab (country name -> file id)
imset_file_choices <- function(dir = "input") {
  m <- imset_file_index(dir)
  if (is.null(m)) return(character(0))
  m <- m[!is.na(m$COUNTRY), , drop = FALSE]
  m <- m[order(m$COUNTRY), , drop = FALSE]
  stats::setNames(m$FILE_ID, tools::toTitleCase(tolower(m$COUNTRY)))
}

imset_pick <- function(d, col, val) {
  if (is.null(val) || !length(val) || identical(val, "ALL") || identical(val, "")) return(d)
  d[!is.na(d[[col]]) & d[[col]] == val, , drop = FALSE]
}

IMSET_MON_LABEL <- c(IN = "In process", END = "End process")
# monitoring-type choices for one country's rows: the types present, plus "Both" when there are two; In process first
imset_mon_choices <- function(d) {
  t <- intersect(c("IN", "END"), unique(stats::na.omit(d$MONITORING)))
  if (!length(t)) t <- "IN"
  ch <- stats::setNames(t, IMSET_MON_LABEL[t])
  if (length(t) > 1) ch <- c(ch, "Both" = "ALL")
  ch
}

imset_choices <- function(values, all_label = NULL, decreasing_dates = NULL) {
  v <- unique(stats::na.omit(values))
  v <- if (is.null(decreasing_dates)) sort(v) else v
  if (!is.null(all_label)) c(stats::setNames("ALL", all_label), stats::setNames(v, v)) else stats::setNames(v, v)
}

imset_fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)

# ---- styling -------------------------------------------------------------------
IMSET_CSS <- "
.imx-wrap { padding: 2px 4px 24px 4px; color: var(--kpi-text, #111827); }
.imx-filters { display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 10px 14px;
  padding: 12px 16px 6px 16px; border-radius: 14px; margin-bottom: 12px; background: var(--kpi-bg, #fff);
  border: 1px solid var(--kpi-border, rgba(37,99,235,.18)); box-shadow: var(--kpi-shadow, 0 1px 3px rgba(0,0,0,.08)); }
.imx-filters .form-group { margin-bottom: 6px; }
.imx-filters label { font-size: 11px; letter-spacing: .6px; text-transform: uppercase; color: var(--kpi-subtext, #6b7280); font-weight: 700; }
.imx-kpis { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 10px; margin-bottom: 12px; }
.imx-kpi { padding: 10px 14px; border-radius: 12px; background: var(--kpi-bg, #fff); border: 1px solid var(--kpi-border, rgba(37,99,235,.18));
  border-top: 3px solid var(--accent, #0072B2); box-shadow: var(--kpi-shadow, 0 1px 3px rgba(0,0,0,.08)); }
.imx-kpi .l { font-size: 10.5px; letter-spacing: .7px; text-transform: uppercase; color: var(--kpi-subtext, #6b7280); font-weight: 700; }
.imx-kpi .v { font-size: 24px; font-weight: 700; line-height: 1.25; color: var(--kpi-text, #111827); font-variant-numeric: tabular-nums; }
.imx-kpi .v.sm { font-size: 14px; line-height: 1.9; }
.imx-kpi .s { font-size: 11.5px; color: var(--kpi-subtext, #6b7280); }
.imx-panel { padding: 12px 14px; border-radius: 14px; background: var(--kpi-bg, #fff); margin-bottom: 12px;
  border: 1px solid var(--kpi-border, rgba(37,99,235,.18)); box-shadow: var(--kpi-shadow, 0 1px 3px rgba(0,0,0,.08)); }
.imx-panel h4 { margin: 0 0 8px 0; font-size: 12px; letter-spacing: .7px; text-transform: uppercase; color: var(--kpi-subtext, #6b7280); font-weight: 700; }
.imx-note { font-size: 12px; color: var(--kpi-subtext, #6b7280); margin: 6px 2px; line-height: 1.5; }
.imx-actions { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-top: 4px; }
.imx-wrap .checkbox { margin-top: 18px; }
"

# ==============================================================================
# INTERACTIVE EXPLORER
# ==============================================================================
imset_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(IMSET_CSS)),
      shiny::tags$script(shiny::HTML("$(document).on('shown.bs.tab', function(){ setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 150); });"))),
    shiny::div(class = "imx-wrap",
      shiny::div(class = "imx-filters",
        shiny::selectInput(ns("country"), "Country", choices = NULL, width = "100%"),
        shiny::selectInput(ns("mon"), "Monitoring", choices = c("In process" = "IN"), width = "100%"),
        shiny::selectInput(ns("year"), "Year", choices = NULL, width = "100%"),
        shiny::selectInput(ns("response"), "Response", choices = NULL, width = "100%"),
        shiny::selectInput(ns("round"), "Round", choices = NULL, width = "100%"),
        shiny::selectInput(ns("province"), "Province", choices = NULL, width = "100%"),
        shiny::selectInput(ns("district"), "District", choices = NULL, width = "100%"),
        shiny::selectInput(ns("basemap"), "Basemap", width = "100%",
          choices = c("Satellite + labels (Esri)" = "hyb", "Satellite only (Esri)" = "sat", "Streets (OpenStreetMap)" = "osm", "No basemap" = "none"),
          selected = "hyb"),
        shiny::checkboxInput(ns("hide_bad"), "Hide suspect records (GPS or counts)", value = TRUE)),
      shiny::uiOutput(ns("kpis")),
      shiny::div(class = "imx-panel",
        shiny::h4("Where missed children were found: monitor routes (dot = start, line = route to the end point)"),
        plotly::plotlyOutput(ns("map"), height = "620px"),
        shiny::uiOutput(ns("note"))),
      shiny::div(class = "imx-panel",
        shiny::h4("Summary by district"),
        DT::DTOutput(ns("tbl")),
        shiny::div(class = "imx-actions",
          shiny::downloadButton(ns("dl"), "Download routes (CSV)", class = "btn-default"))))
  )
}

imset_explorer_server <- function(id, input_dir = "input") {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      idx <- imset_file_index(input_dir); shiny::req(idx)
      ch <- imset_country_choices(idx)
      keep <- shiny::isolate(input$country)
      shiny::updateSelectInput(session, "country", choices = ch,
                               selected = if (!is.null(keep) && keep %in% ch) keep else unname(ch[1]))
    })
    s_country <- shiny::reactive({
      shiny::req(input$country)
      d <- imset_country_data(input_dir, input$country); shiny::req(d); d
    })

    shiny::observeEvent(s_country(), {
      ch <- imset_mon_choices(s_country())
      keep <- shiny::isolate(input$mon)
      shiny::updateSelectInput(session, "mon", choices = ch, selected = if (!is.null(keep) && keep %in% ch) keep else unname(ch[1]))
    })
    s_mon <- shiny::reactive({ shiny::req(input$mon); imset_pick(s_country(), "MONITORING", input$mon) })
    shiny::observeEvent(s_mon(), {
      d <- s_mon(); yrs <- sort(unique(stats::na.omit(d$YEAR)), decreasing = TRUE)
      shiny::updateSelectInput(session, "year",
        choices = c("All years" = "ALL", stats::setNames(yrs, yrs)), selected = if (length(yrs)) yrs[1] else "ALL")
    })
    s_year <- shiny::reactive({
      shiny::req(input$year); d <- s_mon()
      if (identical(input$year, "ALL")) d else d[!is.na(d$YEAR) & d$YEAR == as.integer(input$year), , drop = FALSE]
    })
    shiny::observeEvent(s_year(), {
      d <- s_year()
      if (nrow(d)) { o <- order(d$DATE, decreasing = TRUE, na.last = TRUE); resp <- unique(stats::na.omit(d$RESPONSE[o])) } else resp <- character(0)
      shiny::updateSelectInput(session, "response", choices = c("All responses" = "ALL", stats::setNames(resp, resp)), selected = "ALL")
    })
    s_resp <- shiny::reactive({ shiny::req(input$response); imset_pick(s_year(), "RESPONSE", input$response) })
    shiny::observeEvent(s_resp(), {
      rd <- sort(unique(stats::na.omit(s_resp()$ROUND)))
      shiny::updateSelectInput(session, "round", choices = c("All rounds" = "ALL", stats::setNames(rd, rd)), selected = "ALL")
    })
    s_round <- shiny::reactive({ shiny::req(input$round); imset_pick(s_resp(), "ROUND", input$round) })
    shiny::observeEvent(s_round(), {
      pv <- sort(unique(stats::na.omit(s_round()$PROVINCE)))
      shiny::updateSelectInput(session, "province", choices = c("All provinces" = "ALL", stats::setNames(pv, pv)), selected = "ALL")
    })
    s_prov <- shiny::reactive({ shiny::req(input$province); imset_pick(s_round(), "PROVINCE", input$province) })
    shiny::observeEvent(s_prov(), {
      ds <- sort(unique(stats::na.omit(s_prov()$DISTRICT)))
      shiny::updateSelectInput(session, "district", choices = c("All districts" = "ALL", stats::setNames(ds, ds)), selected = "ALL")
    })
    s_dist <- shiny::reactive({ shiny::req(input$district); imset_pick(s_prov(), "DISTRICT", input$district) })

    shown <- shiny::reactive({
      d <- s_dist()
      if (isTRUE(input$hide_bad)) d <- d[d$QC == "OK", , drop = FALSE]
      d
    })
    n_bad <- shiny::reactive({ d <- s_dist(); sum(d$QC != "OK") })

    output$kpis <- shiny::renderUI({
      d <- shown(); all_n <- nrow(s_dist())
      card <- function(l, v, s = NULL, accent = "#0072B2")
        shiny::div(class = "imx-kpi", style = paste0("--accent:", accent, ";"),
          shiny::div(class = "l", l), shiny::div(class = if (nchar(v) > 12) "v sm" else "v", v), if (!is.null(s)) shiny::div(class = "s", s))
      rng <- if (nrow(d) && any(!is.na(d$DATE))) paste(format(min(d$DATE, na.rm = TRUE), "%d %b %Y"), "-", format(max(d$DATE, na.rm = TRUE), "%d %b %Y")) else "no date"
      shiny::div(class = "imx-kpis",
        card("Monitor routes", imset_fmt(nrow(d)), paste0("of ", imset_fmt(all_n), " in selection"), "#0072B2"),
        card("Settlements", imset_fmt(nrow(unique(d[, c("DISTRICT", "FACILITY", "SETTLEMENT")]))), "distinct names", "#009E73"),
        card("Districts", imset_fmt(length(unique(stats::na.omit(d$DISTRICT)))), NULL, "#E69F00"),
        card("Children missed", imset_fmt(sum(d$CHILDREN, na.rm = TRUE)), "sum over the routes shown", "#D55E00"),
        card("Households with a missed child", imset_fmt(sum(d$HH_MISSED, na.rm = TRUE)), NULL, "#CC79A7"),
        card("Suspect records", imset_fmt(n_bad()), if (isTRUE(input$hide_bad)) "hidden from the map" else "shown on the map", "#56B4E9"),
        card("Dates", rng, NULL, "#999999"))
    })

    cls_levels <- c("1 child", "2 children", "3-4 children", "5+ children")
    cls_cols <- c("#F2B134", "#E6761B", "#C73E1D", "#7A1F5C")
    class_of <- function(n) factor(ifelse(is.na(n) | n <= 1, cls_levels[1], ifelse(n == 2, cls_levels[2], ifelse(n <= 4, cls_levels[3], cls_levels[4]))), levels = cls_levels)

    shown_d <- shiny::debounce(shown, 400)   # avoid redrawing/flying while the filter lists are still updating
    output$map <- plotly::renderPlotly({
      d <- shown_d()
      shiny::validate(shiny::need(nrow(d) > 0, "No monitor routes for this selection. Try another year, response or round."))
      if (nrow(d) > IMSET_MAX_DRAW) d <- d[order(d$DATE, decreasing = TRUE, na.last = TRUE)[seq_len(IMSET_MAX_DRAW)], , drop = FALSE]
      d$CLS <- class_of(d$CHILDREN)
      reasons <- as.matrix(d[, names(IMSET_REASONS), drop = FALSE])
      main <- apply(reasons, 1, function(r) if (all(is.na(r)) || max(r, na.rm = TRUE) <= 0) "-" else IMSET_REASONS[[names(IMSET_REASONS)[which.max(r)]]])
      d$TXT <- paste0("<b>", ifelse(is.na(d$SETTLEMENT), "(no settlement name)", d$SETTLEMENT), "</b><br>",
        ifelse(is.na(d$FACILITY), "", paste0("Facility: ", d$FACILITY, "<br>")),
        d$DISTRICT, ", ", d$PROVINCE, "<br>",
        ifelse(is.na(d$DATE), "", paste0(format(d$DATE, "%d %b %Y"), " | ")), ifelse(is.na(d$RESPONSE), "", d$RESPONSE), " ", ifelse(is.na(d$ROUND), "", d$ROUND), "<br>",
        "Children missed: <b>", d$CHILDREN, "</b> | households: ", d$HH_MISSED, "<br>",
        "Main reason: ", main, "<br>Route length: ", round(d$ROUTE_M), " m",
        ifelse(d$QC == "OK", "", paste0("<br><i>Data check: ", d$QC, "</i>")))
      p <- plotly::plot_ly()
      for (k in seq_along(cls_levels)) {
        s <- d[d$CLS == cls_levels[k], , drop = FALSE]
        if (!nrow(s)) next
        ln_lon <- as.vector(rbind(s$LON, s$LON_END, NA)); ln_lat <- as.vector(rbind(s$LAT, s$LAT_END, NA))
        p <- plotly::add_trace(p, type = "scattermapbox", mode = "lines", lon = ln_lon, lat = ln_lat,
          line = list(color = cls_cols[k], width = 2), hoverinfo = "skip", showlegend = FALSE, legendgroup = cls_levels[k])
        p <- plotly::add_trace(p, type = "scattermapbox", mode = "markers", lon = s$LON, lat = s$LAT, text = s$TXT, hoverinfo = "text",
          marker = list(color = cls_cols[k], size = 8, opacity = 0.9), name = paste0(cls_levels[k], " (", nrow(s), ")"), legendgroup = cls_levels[k])
      }
      lon_r <- range(c(d$LON, d$LON_END), na.rm = TRUE); lat_r <- range(c(d$LAT, d$LAT_END), na.rm = TRUE)
      lon_sp <- max(diff(lon_r) * cos(mean(lat_r) * pi / 180), 0.002) * 1.15
      lat_sp <- max(diff(lat_r), 0.002) * 1.15
      bm <- if (is.null(input$basemap)) "hyb" else input$basemap
      mb <- list(uirevision = "imx")
      # keep the camera where the user (or the fly-in) left it, so a redraw never makes the map jump
      v <- shiny::isolate(input$view)
      if (!is.null(v) && length(v$c) == 2 && is.finite(v$z)) { mb$zoom <- v$z; mb$center <- list(lon = v$c[[1]], lat = v$c[[2]]) }
      esri <- function(path, attr = NULL) list(below = "traces", sourcetype = "raster", opacity = 1,
        source = list(paste0("https://server.arcgisonline.com/ArcGIS/rest/services/", path, "/MapServer/tile/{z}/{y}/{x}")),
        sourceattribution = attr)
      if (bm == "osm") {
        mb$style <- "open-street-map"
      } else if (bm %in% c("sat", "hyb")) {
        mb$style <- "white-bg"
        mb$layers <- list(esri("World_Imagery", "Imagery: Esri, Maxar, Earthstar Geographics"))
        if (bm == "hyb") mb$layers[[2]] <- esri("Reference/World_Boundaries_and_Places")
      } else {
        mb$style <- "white-bg"
      }
      p <- plotly::layout(p, mapbox = mb, uirevision = "imx",
        margin = list(l = 0, r = 0, t = 0, b = 0),
        legend = list(orientation = "h", x = 0.01, y = 0.01, bgcolor = "rgba(255,255,255,.85)"),
        paper_bgcolor = "rgba(0,0,0,0)")
      # fit the view to the data using the real size of the map box (done in the browser)
      htmlwidgets::onRender(p, "function(el, x, b) {
        function getMap() { try { return el._fullLayout.mapbox._subplot.map; } catch (e) { return null; } }
        function targetZoom() {
          var w = el.clientWidth, h = el.clientHeight;
          if (!w || !h) return null;
          var z = Math.log2(Math.min((w - 40) * 360 / (512 * b.lonSp), (h - 70) * 360 / (512 * b.latSp)));
          return isFinite(z) ? Math.max(2, Math.min(18, z)) : null;
        }
        function reveal() { el.style.visibility = 'visible'; }
        var first = !el.__imxPrev, t0 = Date.now(), done = false;
        var m0 = getMap();
        // report the camera to R after every move so redraws keep the current view
        if (m0 && !m0.__imxBound && window.Shiny) {
          m0.__imxBound = true;
          m0.on('moveend', function() { try { var c = m0.getCenter(); Shiny.setInputValue(b.viewId, {c: [c.lng, c.lat], z: m0.getZoom()}, {priority: 'event'}); } catch (e) {} });
        }
        if (first) { el.style.visibility = 'hidden'; setTimeout(reveal, 8000); }
        function go() {
          if (done) return;
          var m = getMap(), z = targetZoom();
          if (!m || z === null) return;
          var ready = !first; try { if (first) ready = m.isStyleLoaded(); } catch (e) {}
          if (!ready && Date.now() - t0 < 4000) return;   // let the map style finish loading before we move it
          done = true;
          try { m.setMaxZoom(19); } catch (e) {}
          var prev = el.__imxPrev;
          var same = prev && Math.abs(prev.z - z) < 0.02 && Math.abs(prev.c[0] - b.lon) < 1e-4 && Math.abs(prev.c[1] - b.lat) < 1e-4;
          el.__imxPrev = {c: [b.lon, b.lat], z: z};
          if (same) { reveal(); return; }
          // first load: start from the whole world and fly in; later changes: fly from the current view
          if (first) { try { m.jumpTo({center: [b.lon, Math.max(-50, Math.min(60, b.lat))], zoom: 1.1}); } catch (e) {} }
          setTimeout(reveal, 80);
          setTimeout(function() {
            try { m.flyTo({center: [b.lon, b.lat], zoom: z, duration: first ? 3800 : 2200, curve: 1.7, essential: true}); }
            catch (e) { Plotly.relayout(el, {'mapbox.zoom': z, 'mapbox.center': {lon: b.lon, lat: b.lat}}); }
          }, first ? 600 : 50);
        }
        if (window.ResizeObserver) { new ResizeObserver(function() { go(); }).observe(el); }
        var iv = setInterval(function() { go(); if (done || Date.now() - t0 > 9000) clearInterval(iv); }, 250);
        go();
      }", data = list(lon = mean(lon_r), lat = mean(lat_r), lonSp = lon_sp, latSp = lat_sp, viewId = session$ns("view")))
    })

    output$note <- shiny::renderUI({
      d <- shown(); msg <- character(0)
      if (nrow(d) > IMSET_MAX_DRAW) msg <- c(msg, paste0("Showing the ", imset_fmt(IMSET_MAX_DRAW), " most recent of ", imset_fmt(nrow(d)), " routes. Choose a year, response or round to see all of them."))
      msg <- c(msg, paste0("Zoom in with the mouse wheel to see houses: use the Satellite basemap for building footprints (up to zoom level 19). Each row of the source data is one monitor route that found missed children; several monitors can visit the same settlement, so one settlement can have several routes. ",
        "Suspect records = start or end GPS outside the country, a start-to-end distance above 5 km, or more than 100 children missed on one route (likely a typing error)."))
      shiny::div(class = "imx-note", paste(msg, collapse = " "))
    })

    by_district <- shiny::reactive({
      d <- shown()
      if (!nrow(d)) return(data.frame())
      g <- split(d, paste(d$PROVINCE, d$DISTRICT, sep = "\r"))
      out <- do.call(rbind, lapply(g, function(x) data.frame(
        Province = x$PROVINCE[1], District = x$DISTRICT[1], Routes = nrow(x),
        Settlements = nrow(unique(x[, c("FACILITY", "SETTLEMENT")])),
        `Children missed` = sum(x$CHILDREN, na.rm = TRUE), `HH with missed child` = sum(x$HH_MISSED, na.rm = TRUE),
        check.names = FALSE, stringsAsFactors = FALSE)))
      out[order(-out$`Children missed`), , drop = FALSE]
    })
    output$tbl <- DT::renderDT({
      DT::datatable(by_district(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"))
    })
    output$dl <- shiny::downloadHandler(
      filename = function() paste0("IM_settlement_routes_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- shown(); d <- d[, base::setdiff(names(d), c("FILE_ID")), drop = FALSE]
        utils::write.csv(d, file, row.names = FALSE, na = "")
      })
  })
}

# ==============================================================================
# PRINTABLE SETTLEMENT MAP (OpenStreetMap detail for one settlement)
# ==============================================================================

OV_JS <- paste0(
"Shiny.addCustomMessageHandler('imset_overpass', function(msg) {",
"  var eps = msg.endpoints, tiers = msg.tiers, tmo = msg.timeout_ms || 30000, deadline = Date.now() + (msg.total_ms || 100000);",
"  var errs = [], done = false, lkey = 'imset_ov:' + msg.key;",
"  var major = ['motorway','trunk','primary','secondary','motorway_link','trunk_link','primary_link','secondary_link'];",
"  function reply(ok, payload, lite) {",
"    if (done) return; done = true;",
"    var o = ok ? {ok:true, key:msg.key, json:payload, lite:!!lite, n:Math.random()} : {ok:false, key:msg.key, err:payload, n:Math.random()};",
"    Shiny.setInputValue(msg.id, o, {priority:'event'});",
"  }",
"  function compact(js) {",
"    var M=[], m=[], w=[], b=[];",
"    (js.elements || []).forEach(function(e) {",
"      if (e.type !== 'way' || !e.geometry || e.geometry.length < 2) return;",
"      var t = e.tags || {}, g = [];",
"      e.geometry.forEach(function(p) { g.push(Math.round(p.lon*1e6)/1e6, Math.round(p.lat*1e6)/1e6); });",
"      if (t.building) b.push(g); else if (t.waterway) w.push(g);",
"      else if (t.highway) { if (major.indexOf(t.highway) >= 0) M.push(g); else m.push(g); }",
"    });",
"    return JSON.stringify({M:M, m:m, w:w, b:b});",
"  }",
"  try {",
"    var hit = JSON.parse(localStorage.getItem(lkey) || 'null');",
"    if (hit && hit.j && hit.t && Date.now() - hit.t < 7*864e5) { reply(true, hit.j, false); return; }",
"  } catch(e) {}",
"  function race(q, ms) {",
"    return new Promise(function(resolve, reject) {",
"      var cs = [], failed = 0, fin = false, es = [];",
"      function stop() { cs.forEach(function(c) { try { c.abort(); } catch(e) {} }); }",
"      var timer = setTimeout(function() { if (fin) return; fin = true; stop();",
"        reject(new Error('no answer within ' + Math.round(ms/1000) + ' s' + (es.length ? ' (' + es.join(', ') + ')' : ''))); }, ms);",
"      eps.forEach(function(ep) {",
"        var c = new AbortController(); cs.push(c); var host = ep.replace(/^https?:\\/\\/([^\\/]+).*$/, '$1');",
"        fetch(ep + '?data=' + encodeURIComponent(q), {signal: c.signal})",
"          .then(function(r) { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); })",
"          .then(function(js) { if (!js || !js.elements) throw new Error('unreadable answer'); if (fin) return; fin = true; clearTimeout(timer); stop(); resolve(js); })",
"          .catch(function(err) { if (fin) return; es.push(host + ' ' + (err && err.message ? err.message : err)); failed++;",
"            if (failed >= eps.length) { fin = true; clearTimeout(timer); reject(new Error(es.join(', '))); } });",
"      });",
"    });",
"  }",
"  var plan = [];",
"  tiers.forEach(function(t, i) { plan.push({q:t, lite:i > 0}); plan.push({q:t, lite:i > 0}); });",
"  var idx = 0;",
"  function step() {",
"    if (idx >= plan.length || Date.now() > deadline) { reply(false, errs.slice(-2).join(' | ')); return; }",
"    var p = plan[idx++];",
"    race(p.q, Math.min(tmo, deadline - Date.now())).then(function(js) {",
"      var c = compact(js);",
"      if (!p.lite) { try { localStorage.setItem(lkey, JSON.stringify({t:Date.now(), j:c})); } catch(e) {} }",
"      reply(true, c, p.lite);",
"    }).catch(function(err) {",
"      errs.push((p.lite ? 'light query: ' : 'full query: ') + err.message);",
"      setTimeout(step, Math.min(2500 * idx, 6000));",
"    });",
"  }",
"  step();",
"});")

imset_static_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(IMSET_CSS)), shiny::tags$script(shiny::HTML(OV_JS))),
    shiny::div(class = "imx-wrap",
      shiny::div(class = "imx-filters",
        shiny::selectInput(ns("country"), "Country", choices = NULL, width = "100%"),
        shiny::selectInput(ns("mon"), "Monitoring", choices = c("In process" = "IN"), width = "100%"),
        shiny::selectInput(ns("year"), "Year", choices = NULL, width = "100%"),
        shiny::selectInput(ns("response"), "Response", choices = NULL, width = "100%"),
        shiny::selectInput(ns("round"), "Round", choices = NULL, width = "100%"),
        shiny::selectInput(ns("province"), "Province", choices = NULL, width = "100%"),
        shiny::selectInput(ns("district"), "District", choices = NULL, width = "100%"),
        shiny::selectInput(ns("facility"), "Facility", choices = NULL, width = "100%"),
        shiny::selectInput(ns("settlement"), "Settlement", choices = NULL, width = "100%"),
        shiny::selectInput(ns("date"), "Visit date", choices = NULL, width = "100%"),
        shiny::selectInput(ns("route"), "Zoom to route", choices = c("All routes" = "all"), width = "100%"),
        shiny::numericInput(ns("bbox_m"), "Map margin (m)", value = 300, min = 50, max = 3000, step = 50, width = "100%"),
        shiny::selectInput(ns("bg"), "Background", width = "100%", selected = "vector",
          choices = c("OSM roads and buildings (map style)" = "vector", "Satellite photo (shows houses)" = "sat",
                      "OpenStreetMap map (standard tiles)" = "osmtile", "None (routes only)" = "none"))),
      shiny::div(class = "imx-actions", style = "margin-bottom:10px;",
        shiny::actionButton(ns("generate"), "Generate map", class = "btn-primary"),
        shiny::downloadButton(ns("download_map"), "Download map (PNG)", class = "btn-default"),
        shiny::uiOutput(ns("sel_info"), inline = TRUE)),
      shiny::div(class = "imx-panel",
        shiny::plotOutput(ns("map"), height = "620px"),
        shiny::div(class = "imx-note", "The background is loaded from public map servers (about 5-10 seconds). The default draws OpenStreetMap roads, water and buildings in plain map style, fetched by your browser; the satellite photo shows the houses themselves. If a source cannot be reached, the map still draws and the reason is printed under it. All monitor routes of the selected settlement visit are drawn; \"Zoom to route\" frames and summarises a single route.")))
  )
}

imset_static_server <- function(id, input_dir = "input") {
  shiny::moduleServer(id, function(input, output, session) {
    chain <- function(col, label, parent, input_id, all_label = NULL, selected_all = TRUE) {
      shiny::observeEvent(parent(), {
        v <- sort(unique(stats::na.omit(parent()[[col]])))
        ch <- if (is.null(all_label)) stats::setNames(v, v) else c(stats::setNames("ALL", all_label), stats::setNames(v, v))
        shiny::updateSelectInput(session, input_id, choices = ch)
      })
    }
    shiny::observe({
      idx <- imset_file_index(input_dir); shiny::req(idx); ch <- imset_country_choices(idx)
      keep <- shiny::isolate(input$country)
      shiny::updateSelectInput(session, "country", choices = ch, selected = if (!is.null(keep) && keep %in% ch) keep else unname(ch[1]))
    })
    s1 <- shiny::reactive({
      shiny::req(input$country)
      d <- imset_country_data(input_dir, input$country); shiny::req(d); d
    })
    # Year (newest first, "All years" available): narrows everything below it
    shiny::observeEvent(s1(), {
      ch <- imset_mon_choices(s1())
      keep <- shiny::isolate(input$mon)
      shiny::updateSelectInput(session, "mon", choices = ch, selected = if (!is.null(keep) && keep %in% ch) keep else unname(ch[1]))
    })
    s1m <- shiny::reactive({ shiny::req(input$mon); imset_pick(s1(), "MONITORING", input$mon) })
    shiny::observeEvent(s1m(), {
      yrs <- sort(unique(stats::na.omit(s1m()$YEAR)), decreasing = TRUE)
      shiny::updateSelectInput(session, "year",
        choices = c("All years" = "ALL", stats::setNames(yrs, yrs)), selected = if (length(yrs)) yrs[1] else "ALL")
    })
    s1y <- shiny::reactive({
      shiny::req(input$year); d <- s1m()
      if (identical(input$year, "ALL")) d else d[!is.na(d$YEAR) & d$YEAR == as.integer(input$year), , drop = FALSE]
    })
    shiny::observeEvent(s1y(), {
      d <- s1y(); o <- order(d$DATE, decreasing = TRUE, na.last = TRUE); v <- unique(stats::na.omit(d$RESPONSE[o]))
      shiny::updateSelectInput(session, "response", choices = stats::setNames(v, v))
    })
    s2 <- shiny::reactive({ shiny::req(input$response); imset_pick(s1y(), "RESPONSE", input$response) })
    chain("ROUND", "Round", s2, "round")
    s3 <- shiny::reactive({ shiny::req(input$round); imset_pick(s2(), "ROUND", input$round) })
    chain("PROVINCE", "Province", s3, "province")
    s4 <- shiny::reactive({ shiny::req(input$province); imset_pick(s3(), "PROVINCE", input$province) })
    chain("DISTRICT", "District", s4, "district")
    s5 <- shiny::reactive({ shiny::req(input$district); imset_pick(s4(), "DISTRICT", input$district) })
    chain("FACILITY", "Facility", s5, "facility")
    s6 <- shiny::reactive({ shiny::req(input$facility); imset_pick(s5(), "FACILITY", input$facility) })
    chain("SETTLEMENT", "Settlement", s6, "settlement")
    s7 <- shiny::reactive({ shiny::req(input$settlement); imset_pick(s6(), "SETTLEMENT", input$settlement) })
    shiny::observeEvent(s7(), {
      dts <- sort(unique(stats::na.omit(s7()$DATE)), decreasing = TRUE)
      ch <- c("All dates" = "ALL", stats::setNames(as.character(dts), format(dts, "%d %b %Y")))
      shiny::updateSelectInput(session, "date", choices = ch, selected = "ALL")
    })
    rows <- shiny::reactive({
      d <- s7(); shiny::req(nrow(d) > 0, input$date)
      if (!identical(input$date, "ALL")) d <- d[!is.na(d$DATE) & as.character(d$DATE) == input$date, , drop = FALSE]
      d
    })
    shiny::observeEvent(rows(), {
      r <- imset_valid_routes(rows())
      ch <- c("All routes" = "all")
      if (nrow(r) > 1) ch <- c(ch, stats::setNames(as.character(r$N), paste0("Route ", r$N, ": ", r$CHILDREN, " child(ren), ", ifelse(is.na(r$ROUTE_M), "?", round(r$ROUTE_M)), " m long")))
      shiny::updateSelectInput(session, "route", choices = ch, selected = "all")
    })
    output$sel_info <- shiny::renderUI({
      d <- tryCatch(rows(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      ok <- d[d$QC == "OK", , drop = FALSE]
      shiny::span(class = "imx-note", paste0(nrow(ok), " monitor route(s), ", imset_fmt(sum(ok$CHILDREN, na.rm = TRUE)), " children missed",
        if (nrow(ok) < nrow(d)) paste0(" (", nrow(d) - nrow(ok), " suspect record(s) excluded)") else ""))
    })

    # The OpenStreetMap vector data (roads, water, buildings) is requested by the user's BROWSER, because the
    # Connect server cannot reach the Overpass servers. The browser sends the compact layers back to R.
    trig <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$generate, {
      if (identical(input$bg, "vector")) {
        d <- tryCatch(rows(), error = function(e) NULL)
        rt <- if (identical(input$route, "all")) NULL else input$route
        bb <- if (is.null(d)) NULL else tryCatch(imset_map_bbox_for(d, input$bbox_m, rt), error = function(e) NULL)
        if (!is.null(bb) && !imset_osm_cached(bb)) {
          shiny::showNotification("Loading roads and buildings from OpenStreetMap through your browser...", id = "imset_ov", duration = NULL, type = "message")
          session$sendCustomMessage("imset_overpass", list(id = session$ns("ovr"), key = imset_osm_key(bb),
                                                           tiers = list(imset_osm_query(bb), imset_osm_query_lite(bb)),
                                                           endpoints = as.list(IMSET_OVERPASS), timeout_ms = 30000, total_ms = 100000))
          return(invisible(NULL))
        }
      }
      trig(shiny::isolate(if (is.null(trig())) 1 else trig() + 1))
    })
    shiny::observeEvent(input$ovr, {
      shiny::removeNotification("imset_ov")
      r <- input$ovr
      if (isTRUE(r$ok)) {
        ok <- tryCatch({ imset_osm_store(r$key, r$json); TRUE }, error = function(e) { assign("last_err", paste("browser answer could not be read:", conditionMessage(e)), envir = .imset_osm_cache); FALSE })
        if (ok) assign("last_err", "", envir = .imset_osm_cache)
        # a "light" answer (no buildings, servers busy) is used for this map but not kept, so the next Generate retries the full query
        if (ok && isTRUE(r$lite)) {
          assign(r$key, TRUE, envir = .imset_osm_lite)
          shiny::showNotification("The OpenStreetMap servers are busy: roads and water are drawn, buildings could not be loaded. Click Generate map again later to retry.", type = "warning", duration = 12)
        } else if (ok && exists(r$key, envir = .imset_osm_lite, inherits = FALSE)) rm(list = r$key, envir = .imset_osm_lite)
      } else assign("last_err", paste0("browser: ", r$err), envir = .imset_osm_cache)
      trig(shiny::isolate(if (is.null(trig())) 1 else trig() + 1))
    })
    plot_r <- shiny::eventReactive(trig(), {
      d <- rows()
      shiny::withProgress(message = "Building the settlement map...", detail = "Loading the background", value = 0.4, {
        tryCatch(create_settlement_map(d, bbox_m = input$bbox_m, bg = input$bg, route = if (identical(input$route, "all")) NULL else input$route, vector_server = FALSE),
                 error = function(e) { cat("Settlement map error:", conditionMessage(e), "\n"); structure(list(msg = conditionMessage(e)), class = "imset_err") })
      })
    })
    # draw the plot; any error (building or drawing) is shown as text instead of the generic Connect message
    imset_draw <- function(p) {
      msg <- if (inherits(p, "imset_err")) p$msg else tryCatch({ print(p); NULL }, error = function(e) conditionMessage(e))
      if (!is.null(msg)) {
        cat("Settlement map error:", msg, "\n")
        grid::grid.newpage()
        grid::grid.text(paste(strwrap(paste("The map could not be drawn:", msg), 90), collapse = "\n"),
                        x = 0.05, y = 0.9, just = c("left", "top"), gp = grid::gpar(col = "firebrick", fontsize = 11))
      }
      invisible(NULL)
    }
    output$map <- shiny::renderPlot({ imset_draw(plot_r()) }, height = 620, res = 96)
    output$download_map <- shiny::downloadHandler(
      filename = function() paste0("settlement_map_", gsub("[^A-Za-z0-9]+", "_", input$settlement), "_", Sys.Date(), ".png"),
      content = function(file) {
        p <- tryCatch(plot_r(), error = function(e) NULL)
        if (is.null(p) || inherits(p, "imset_err")) { shiny::showNotification("Generate the map first.", type = "error"); return(invisible(NULL)) }
        grDevices::png(file, width = 12, height = 7, units = "in", res = 300)
        on.exit(grDevices::dev.off(), add = TRUE)
        imset_draw(p)
      })
  })
}

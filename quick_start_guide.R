# ============================================================================
# quick_start_guide.R  --  Quick Start Guide page + PDF download
#
# One content definition (qsg_content) feeds both the on-screen page (qsg_ui)
# and the downloadable PDF (qsg_pdf), so the two never drift apart.
# The PDF is drawn with base grid graphics: no LaTeX, pandoc or browser needed,
# so it also works on Posit Connect Cloud. Plain ASCII text, no symbols.
# ============================================================================

QSG_TITLE <- "WHO AFRO SIA Dashboard"
QSG_SUBTITLE <- "Quick Start Guide"
QSG_TAGLINE <- paste("Integrated digital platform for monitoring Supplementary Immunization Activities (SIA)",
                     "across the WHO African Region")
QSG_CONTACTS <- c("dimafro@who.int", "tourayk@who.int", "mwanzam@who.int")

# ---- content ----------------------------------------------------------------------
# section: list(title, paras, bullets, steps, table = data.frame(a, b), table_head = c("A", "B"), image = "qr")
qsg_content <- function() {
  list(
    list(title = "About this dashboard",
         paras = paste(
           "The WHO AFRO SIA Dashboard brings together independent monitoring (IM and LQAS), SIA scope,",
           "campaign delivery from administrative data and a Power BI campaign overview, so that campaign",
           "quality and coverage can be reviewed in one place."),
         heading2 = "Key objectives",
         bullets = c(
           "Track and compare SIA performance across countries, regions and rounds.",
           "Support data-driven decisions and field coordination.",
           "Improve accountability and timeliness of data reporting.",
           "Help teams find and follow up missed children and low-performing districts.")),

    list(title = "Getting started",
         steps = c(
           "Open At a Glance (the landing page) for the headline numbers, then use the menu on the left to go to the page you need.",
           "On each page, set the filters first (period, block or country and, where offered, province or district). Click the analysis button where the page has one.",
           "Where a page has sub-tabs (for example Overview, Missed Children and Coverage Analysis), use the tabs at the top of the page to switch views.",
           "Use the download buttons on charts and tables, or Generate Slides to build a PowerPoint.")),

    list(title = "Pages at a glance",
         table_head = c("Page", "What it shows"),
         table = data.frame(
           a = c("At a Glance",
                 "Campaigns Overview (Power BI)",
                 "SIA Scope Analysis",
                 "LQAS Repository Overview",
                 "LQAS District Performance",
                 "IM Overview",
                 "IM Settlement Maps",
                 "Admin Data Overview",
                 "Generate Slides",
                 "Quick Start Guide"),
           b = c("Headline numbers for the monitoring programme (submissions, countries, districts, settlements visited, children sampled and found vaccinated) and for campaign delivery (doses, campaigns, rounds, 95% coverage), with trends by year. Can be printed or downloaded as CSV.",
                 "The campaign overview report, embedded from Power BI.",
                 "Scope maps showing which districts were in scope in each round (Scope Map and Scope Summary Map).",
                 "Overview of the LQAS repository, with Missed Children and Coverage Analysis sub-tabs.",
                 "District performance maps, with LQAS Summary Maps, Reasons Analysis and Unresolved Cases sub-tabs.",
                 "The Independent Monitoring repository (Overview sub-tab) and IM Analytics: coverage, trend, risk classes, reasons for missed children and priority districts.",
                 "Settlement-level maps of the settlements visited by IM teams. Filter by country, year and monitoring type (in process or end process). The Settlement Data tab lists and exports the records.",
                 "Administrative coverage and children vaccinated by country and block, including district coverage of 95% or more.",
                 "Builds a PowerPoint from the analyses you choose (see the next section).",
                 "This page, also available as a PDF."),
           stringsAsFactors = FALSE)),

    list(title = "Generate a PowerPoint",
         steps = c(
           "Open Generate Slides and choose the period and, if you wish, a block, countries, provinces or districts.",
           "Tick the analyses (SIA, LQAS, administrative data and Independent Monitoring) and the output types: maps, charts, summary tables and key numbers.",
           "Click Generate slides, review each slide in the preview, then click Download PowerPoint."),
         heading2 = "Good to know",
         bullets = c(
           "A specific period or country is much faster than All data.",
           "Missed Children Analysis and Coverage Analysis work in rolling months and at country level, so a province or district filter does not apply to them.",
           "If an analysis could not be built, the reason is shown under the Generate button.")),

    list(title = "Key definitions",
         table_head = c("Term", "Meaning"),
         table = data.frame(
           a = c("Campaign", "Round", "IM coverage", "LQAS", "Administrative coverage", "Monitoring type", "Data as of"),
           b = c("A unique country and response, for example one nOPV campaign in one country. A campaign counts as both when it used nOPV and bOPV.",
                 "One implementation of a campaign, counted per country, response and round number.",
                 "Children found with a finger mark divided by the children under five present at the household. The target is 90%.",
                 "Lot Quality Assurance Sampling: a sample survey after the campaign that classifies each district (lot) as pass or fail.",
                 "Children vaccinated as reported by the teams divided by the target population. District rounds with 95% or more are highlighted.",
                 "In process (during the round) or end process (after the round). IM Settlement Maps can show either or both.",
                 "The date of the latest data loaded, shown on At a Glance."),
           stringsAsFactors = FALSE)),

    list(title = "Tips for best performance",
         bullets = c(
           "Start with specific time periods rather than All data.",
           "Use the AFRO Block filter to focus on a region.",
           "Limit country selections to 10 to 15 countries.",
           "Reload the page to clear memory and restart an analysis.")),

    list(title = "Data sources",
         paras = "This dashboard uses data from the PEP SIA repository and the Independent Monitoring workflow, and includes:",
         bullets = c(
           "Campaign and SIA scope data.",
           "LQAS survey results.",
           "Independent Monitoring submissions (ONA) and settlement-level locations.",
           "Administrative coverage data.",
           "Geographic shapefiles (country, province and district) for mapping."),
         paras2 = "The date of the latest data is shown on At a Glance."),

    list(title = "Technical support",
         paras = "For technical assistance or data questions, please contact the WHO AFRO DIM Team:",
         contacts = TRUE)
  )
}

# ---- on-screen page -----------------------------------------------------------------
QSG_CSS <- "
.qsg { max-width: 1100px; margin: 0 auto; color: #1f2937; }
.qsg-banner { background: linear-gradient(120deg, #eef6fc 0%, #f7fbfe 100%); border: 1px solid rgba(148,163,184,.35);
  border-radius: 12px; padding: 18px 24px; margin-bottom: 16px; display: flex; align-items: center;
  justify-content: space-between; flex-wrap: wrap; gap: 14px; }
.qsg-banner h2 { margin: 0; font-size: 24px; font-weight: 600; color: #0b3c5d; }
.qsg-banner p { margin: 4px 0 0 0; color: #4b5563; font-size: 14px; max-width: 640px; }
.qsg-card { background: #fff; border: 1px solid rgba(148,163,184,.35); border-radius: 12px; padding: 16px 20px; margin-bottom: 14px; }
.qsg-card h3 { margin: 0 0 10px 0; font-size: 16px; font-weight: 600; color: #0b3c5d; padding-bottom: 6px; border-bottom: 1px solid rgba(148,163,184,.35); }
.qsg-card h4 { margin: 14px 0 6px 0; font-size: 14px; font-weight: 600; color: #374151; }
.qsg-card p, .qsg-card li { font-size: 14px; line-height: 1.5; color: #1f2937; }
.qsg-card ul, .qsg-card ol { margin: 4px 0 4px 0; padding-left: 20px; }
.qsg-table { width: 100%; border-collapse: collapse; font-size: 14px; }
.qsg-table th { text-align: left; background: #eef6fc; color: #0b3c5d; font-weight: 600; padding: 7px 10px; border-bottom: 1px solid rgba(148,163,184,.5); }
.qsg-table td { padding: 7px 10px; vertical-align: top; border-bottom: 1px solid rgba(148,163,184,.25); line-height: 1.45; }
.qsg-table td:first-child { width: 24%; font-weight: 600; color: #0b3c5d; }
.qsg-access { display: flex; gap: 18px; align-items: center; flex-wrap: wrap; }
.qsg-access img.qr { width: 150px; height: 150px; border: 1px solid rgba(148,163,184,.5); border-radius: 8px; }
.qsg-foot { text-align: center; color: #6b7280; font-size: 13px; margin: 6px 0 14px 0; }
body.dark-mode .qsg { color: #e5e7eb; }
body.dark-mode .qsg-banner, body.dark-mode .qsg-card { background: #0b1220; border-color: rgba(148,163,184,.28); }
body.dark-mode .qsg-banner h2, body.dark-mode .qsg-card h3, body.dark-mode .qsg-table td:first-child { color: #7cc4ff; }
body.dark-mode .qsg-banner p, body.dark-mode .qsg-card p, body.dark-mode .qsg-card li, body.dark-mode .qsg-card h4, body.dark-mode .qsg-table td { color: #e5e7eb; }
body.dark-mode .qsg-table th { background: #111b2e; color: #7cc4ff; }
@media print { .qsg-noprint { display: none !important; } }
"

qsg_section_ui <- function(sec, qr_src = NULL, gis_src = NULL) {
  tg <- shiny::tags
  kids <- list(tg$h3(sec$title))
  if (!is.null(sec$paras)) kids <- c(kids, list(tg$p(sec$paras)))
  if (!is.null(sec$steps)) kids <- c(kids, list(tg$ol(lapply(sec$steps, tg$li))))
  if (!is.null(sec$heading2)) kids <- c(kids, list(tg$h4(sec$heading2)))
  if (!is.null(sec$bullets)) kids <- c(kids, list(tg$ul(lapply(sec$bullets, tg$li))))
  if (!is.null(sec$paras2)) kids <- c(kids, list(tg$p(sec$paras2)))
  if (!is.null(sec$table)) {
    kids <- c(kids, list(tg$table(class = "qsg-table",
      tg$thead(tg$tr(lapply(sec$table_head, tg$th))),
      tg$tbody(lapply(seq_len(nrow(sec$table)), function(i) tg$tr(tg$td(sec$table$a[i]), tg$td(sec$table$b[i])))))))
  }
  if (isTRUE(sec$contacts))
    kids <- c(kids, list(tg$p(lapply(seq_along(QSG_CONTACTS), function(i)
      list(if (i > 1) " | ", tg$a(href = paste0("mailto:", QSG_CONTACTS[i]), QSG_CONTACTS[i]))))))
  tg$div(class = "qsg-card", kids)
}

qsg_ui <- function(version = "", date = "", qr_src = NULL, gis_src = NULL, logo_src = NULL, download_id = "guide_pdf") {
  tg <- shiny::tags
  secs <- qsg_content()
  # the mobile-access block sits right after "About this dashboard"
  access <- tg$div(class = "qsg-card",
    tg$h3("Open the dashboard on a mobile device"),
    tg$div(class = "qsg-access",
      if (!is.null(qr_src)) tg$img(class = "qr", src = qr_src, alt = "QR code to open the WHO AFRO SIA Dashboard"),
      tg$div(tg$p("Scan this QR code with the camera of your phone or tablet to open the live dashboard."),
             if (!is.null(gis_src)) tg$img(src = gis_src, alt = "AFRO GIS Centre", style = "height: 70px; width: auto;"))))
  body <- list()
  for (i in seq_along(secs)) {
    body[[length(body) + 1]] <- qsg_section_ui(secs[[i]])
    if (i == 1) body[[length(body) + 1]] <- access
  }
  shiny::tagList(
    tg$style(shiny::HTML(QSG_CSS)),
    tg$div(class = "qsg",
      tg$div(class = "qsg-banner",
        tg$div(if (!is.null(logo_src)) tg$img(src = logo_src, alt = "WHO", style = "height: 40px; margin-bottom: 8px;"),
               tg$h2(QSG_TITLE), tg$p(QSG_TAGLINE)),
        tg$div(class = "qsg-noprint", shiny::downloadButton(download_id, "Download PDF", class = "btn-primary-custom", icon = NULL))),
      body,
      tg$div(class = "qsg-foot", paste0("Version ", version, " | Last updated ", date))))
}

# ---- PDF ------------------------------------------------------------------------------
qsg_pdf <- function(file, version = "", date = "", qr_path = NULL, gis_path = NULL) {
  W <- 8.27; H <- 11.69; ML <- 0.8; MR <- 0.8; MT <- 0.85; MB <- 0.85
  navy <- "#0B3C5D"; ink <- "#1F2937"; grey <- "#4B5563"; rule <- "#CBD5E1"; zebra <- "#F1F5F9"; head_bg <- "#E3F0FA"
  CW <- W - ML - MR
  # ASCII text; the hyphen is swapped for the Latin-1 hyphen glyph, because the PDF device draws a plain
  # hyphen-minus as a longer minus sign
  ascii <- function(s) {
    s <- iconv(as.character(s), to = "ASCII//TRANSLIT", sub = "")
    enc2utf8(gsub("-", "\u00ad", s, fixed = TRUE))
  }

  grDevices::pdf(file, width = W, height = H, family = "Helvetica", onefile = TRUE, encoding = "ISOLatin1.enc",
                 title = paste(QSG_TITLE, "-", QSG_SUBTITLE))
  on.exit(grDevices::dev.off(), add = TRUE)

  U <- function(x) grid::unit(x, "inches")
  Y <- function(top) grid::unit(H - top, "inches")
  gp <- function(size, face = "plain", col = ink) grid::gpar(fontsize = size, fontface = face, col = col)
  tw <- function(s, size, face = "plain")
    grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = gp(size, face))), "inches", valueOnly = TRUE)
  lh <- function(size) size / 72 * 1.38

  wrap <- function(s, width, size, face = "plain") {
    words <- strsplit(ascii(s), " +")[[1]]
    lines <- character(0); cur <- ""
    for (w in words) {
      cand <- if (nzchar(cur)) paste(cur, w) else w
      if (tw(cand, size, face) <= width || !nzchar(cur)) cur <- cand else { lines <- c(lines, cur); cur <- w }
    }
    c(lines, cur)
  }

  st <- new.env(); st$y <- MT; st$page <- 0
  new_page <- function() {
    grid::grid.newpage(); st$page <- st$page + 1
    if (st$page > 1) {
      grid::grid.text(paste(QSG_TITLE, "-", QSG_SUBTITLE), x = U(ML), y = Y(0.45), just = c("left", "center"), gp = gp(8, "plain", grey))
      grid::grid.lines(x = U(c(ML, W - MR)), y = Y(c(0.58, 0.58)), gp = grid::gpar(col = rule, lwd = 0.8))
    }
    grid::grid.lines(x = U(c(ML, W - MR)), y = Y(c(H - 0.6, H - 0.6)), gp = grid::gpar(col = rule, lwd = 0.8))
    grid::grid.text(paste0("Version ", version, " | Last updated ", date), x = U(ML), y = Y(H - 0.42), just = c("left", "center"), gp = gp(8, "plain", grey))
    grid::grid.text(paste("Page", st$page), x = U(W - MR), y = Y(H - 0.42), just = c("right", "center"), gp = gp(8, "plain", grey))
    st$y <- if (st$page == 1) MT else 0.85
  }
  ensure <- function(h) if (st$y + h > H - MB) new_page()
  put <- function(lines, x, size, face = "plain", col = ink) {
    for (l in lines) {
      grid::grid.text(l, x = U(x), y = Y(st$y), just = c("left", "top"), gp = gp(size, face, col))
      st$y <- st$y + lh(size)
    }
  }

  new_page()
  # banner
  grid::grid.rect(x = U(0), y = Y(0), width = U(W), height = U(1.55), just = c("left", "top"), gp = grid::gpar(fill = navy, col = NA))
  grid::grid.text(QSG_TITLE, x = U(ML), y = Y(0.55), just = c("left", "center"), gp = gp(22, "bold", "white"))
  grid::grid.text(QSG_SUBTITLE, x = U(ML), y = Y(0.95), just = c("left", "center"), gp = gp(13, "plain", "#CFE3F2"))
  tl <- wrap(QSG_TAGLINE, CW, 9)
  st$y <- 1.18
  put(tl, ML, 9, "plain", "#CFE3F2")
  st$y <- 1.85

  heading <- function(s) {
    ensure(0.75); st$y <- st$y + 0.12
    grid::grid.text(ascii(s), x = U(ML), y = Y(st$y), just = c("left", "top"), gp = gp(13.5, "bold", navy))
    st$y <- st$y + lh(13.5) + 0.02
    grid::grid.lines(x = U(c(ML, W - MR)), y = Y(c(st$y, st$y)), gp = grid::gpar(col = rule, lwd = 0.9))
    st$y <- st$y + 0.1
  }
  subheading <- function(s) { ensure(0.5); st$y <- st$y + 0.06; put(ascii(s), ML, 10.5, "bold", grey); st$y <- st$y + 0.02 }
  para <- function(s) { l <- wrap(s, CW, 10); ensure(lh(10) * min(length(l), 3)); put(l, ML, 10); st$y <- st$y + 0.07 }
  bullets <- function(v) {
    for (s in v) {
      l <- wrap(s, CW - 0.3, 10); ensure(lh(10) * min(length(l), 2))
      grid::grid.circle(x = U(ML + 0.1), y = Y(st$y + lh(10) * 0.42), r = U(0.028), gp = grid::gpar(fill = grey, col = NA))
      put(l, ML + 0.28, 10); st$y <- st$y + 0.03
    }
    st$y <- st$y + 0.05
  }
  steps <- function(v) {
    for (i in seq_along(v)) {
      l <- wrap(v[i], CW - 0.35, 10); ensure(lh(10) * min(length(l), 2))
      grid::grid.text(paste0(i, "."), x = U(ML + 0.05), y = Y(st$y), just = c("left", "top"), gp = gp(10, "bold", navy))
      put(l, ML + 0.32, 10); st$y <- st$y + 0.04
    }
    st$y <- st$y + 0.05
  }
  table <- function(head, df) {
    c1 <- 1.75; c2 <- CW - c1; pad <- 0.07
    row <- function(a, b, face1 = "bold", bg = NULL, fcol = ink, face2 = "plain", repeat_head = TRUE) {
      la <- wrap(a, c1 - 2 * pad, 9.5, face1); lb <- wrap(b, c2 - 2 * pad, 9.5, face2)
      h <- max(length(la), length(lb)) * lh(9.5) + 2 * pad
      if (repeat_head && st$y + h > H - MB) {
        new_page()
        row(head[1], head[2], "bold", head_bg, navy, "bold", repeat_head = FALSE)
      }
      ensure(h)
      if (!is.null(bg)) grid::grid.rect(x = U(ML), y = Y(st$y), width = U(CW), height = U(h), just = c("left", "top"), gp = grid::gpar(fill = bg, col = NA))
      top <- st$y
      st$y <- top + pad; put(la, ML + pad, 9.5, face1, if (identical(face1, "bold")) navy else fcol)
      st$y <- top + pad; put(lb, ML + c1 + pad, 9.5, face2, fcol)
      st$y <- top + h
      grid::grid.lines(x = U(c(ML, W - MR)), y = Y(c(st$y, st$y)), gp = grid::gpar(col = rule, lwd = 0.5))
    }
    row(head[1], head[2], "bold", head_bg, navy, "bold", repeat_head = FALSE)
    for (i in seq_len(nrow(df))) row(df$a[i], df$b[i], "bold", if (i %% 2 == 0) zebra else NULL)
    st$y <- st$y + 0.1
  }

  secs <- qsg_content()
  for (i in seq_along(secs)) {
    s <- secs[[i]]
    heading(s$title)
    if (!is.null(s$paras)) para(s$paras)
    if (!is.null(s$steps)) steps(s$steps)
    if (!is.null(s$heading2)) subheading(s$heading2)
    if (!is.null(s$bullets)) bullets(s$bullets)
    if (!is.null(s$paras2)) para(s$paras2)
    if (!is.null(s$table)) table(s$table_head, s$table)
    if (isTRUE(s$contacts)) para(paste(QSG_CONTACTS, collapse = "  |  "))
    if (i == 1) {
      heading("Open the dashboard on a mobile device")
      img <- if (!is.null(qr_path) && file.exists(qr_path) && requireNamespace("png", quietly = TRUE))
        tryCatch(png::readPNG(qr_path), error = function(e) NULL) else NULL
      sz <- 1.45
      ensure(sz + 0.1)
      if (!is.null(img)) grid::grid.raster(img, x = U(ML), y = Y(st$y), width = U(sz), height = U(sz), just = c("left", "top"), interpolate = FALSE)
      top <- st$y
      st$y <- top + 0.1
      put(wrap("Scan this QR code with the camera of your phone or tablet to open the live dashboard.", CW - sz - 0.3, 10), ML + sz + 0.3, 10)
      st$y <- top + sz + 0.12
    }
  }
  invisible(file)
}

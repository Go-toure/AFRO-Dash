# ============================================================================
# slides_builder.R  --  PowerPoint builder for the "Generate Slides" tab
#
# Turns the analysis results produced on the Generate Slides tab (maps, charts,
# flextables, headline metrics) into a 16:9 presentation with the officer
# package. Pure functions, no Shiny: slb_build_pptx() returns the presentation
# object plus a list of items that could not be placed.
#
# Slide layout (all on the template's "Blank" layout, drawn with explicit
# positions so it does not depend on the template placeholders):
#   title slide -> scope slide (what is in this report) -> one or more slides
#   per analysis (maps / charts / tables / headline metrics) -> closing slide
# ============================================================================

SLB_W <- 13.333   # slide width  (inches)
SLB_H <- 7.5      # slide height (inches)
SLB_FONT <- "Calibri"

# ---- palettes (the "PowerPoint Template" dropdown) ---------------------------
slb_palette <- function(template = "default") {
  switch(as.character(template),
    minimal = list(bar = "#FFFFFF", bar_text = "#0B3C5D", sub_text = "#4B5563",
                   accent = "#0072B2", rule = "#0072B2", body_text = "#111827",
                   muted = "#6B7280", card = "#F3F4F6", title_bg = "#FFFFFF",
                   title_text = "#0B3C5D",
                   cards = c("#0072B2", "#009E73", "#E69F00", "#D55E00", "#CC79A7", "#56B4E9")),
    corporate = list(bar = "#2F3B4A", bar_text = "#FFFFFF", sub_text = "#D1D5DB",
                     accent = "#009E73", rule = "#009E73", body_text = "#111827",
                     muted = "#6B7280", card = "#EEF2F5", title_bg = "#2F3B4A",
                     title_text = "#FFFFFF",
                     cards = c("#009E73", "#0072B2", "#E69F00", "#D55E00", "#CC79A7", "#56B4E9")),
    list(bar = "#0B3C5D", bar_text = "#FFFFFF", sub_text = "#CFE3F2",
         accent = "#0072B2", rule = "#009ADE", body_text = "#111827",
         muted = "#6B7280", card = "#EEF6FC", title_bg = "#0B3C5D",
         title_text = "#FFFFFF",
         cards = c("#0072B2", "#009E73", "#E69F00", "#D55E00", "#CC79A7", "#56B4E9"))
  )
}

# ---- small helpers -----------------------------------------------------------
slb_txt <- function(x, size = 12, color = "#111827", bold = FALSE, italic = FALSE) {
  officer::ftext(as.character(x), officer::fp_text(
    font.size = size, color = color, bold = bold, italic = italic, font.family = SLB_FONT))
}

slb_par <- function(..., align = "left", top = 0, left = 0, bottom = 0, right = 0) {
  officer::fpar(..., fp_p = officer::fp_par(
    text.align = align, padding.top = top, padding.left = left,
    padding.bottom = bottom, padding.right = right))
}

slb_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("-")
  format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
}

slb_label_n <- function(x, max_items = 6) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("")
  if (length(x) <= max_items) paste(x, collapse = ", ")
  else paste0(paste(x[seq_len(max_items)], collapse = ", "), " (+", length(x) - max_items, " more)")
}

# a text box: optional filled background, text given as a list of fpar()
slb_box <- function(pres, pars, left, top, width, height, bg = NULL) {
  loc <- if (is.null(bg)) officer::ph_location(left = left, top = top, width = width, height = height)
         else officer::ph_location(left = left, top = top, width = width, height = height, bg = bg)
  officer::ph_with(pres, value = do.call(officer::block_list, pars), location = loc)
}

# ---- frame shared by every content slide --------------------------------------
slb_frame <- function(pres, title, subtitle = NULL, pal, footer = "") {
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme")
  # header bar
  pars <- list(slb_par(slb_txt(title, 26, pal$bar_text, bold = TRUE), top = 12, left = 22))
  if (!is.null(subtitle) && nzchar(subtitle))
    pars[[2]] <- slb_par(slb_txt(subtitle, 12, pal$sub_text), left = 22, top = 1)
  pres <- slb_box(pres, pars, 0, 0, SLB_W, 0.95, bg = pal$bar)
  # accent rule under the header
  pres <- officer::ph_with(pres, value = "", location = officer::ph_location(
    left = 0, top = 0.95, width = SLB_W, height = 0.05, bg = pal$rule))
  # footer
  pres <- slb_box(pres, list(slb_par(slb_txt(footer, 9, pal$muted), left = 22)),
                  0, 7.08, 11.6, 0.35)
  pres <- slb_box(pres, list(slb_par(slb_txt(length(pres) + 0, 9, pal$muted), align = "right", right = 22)),
                  11.7, 7.08, 1.63, 0.35)
  pres
}

# ---- plot / table / metric placement -------------------------------------------
slb_plot_png <- function(plot, w_in, h_in, dpi = 150) {
  f <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(f, width = w_in, height = h_in, units = "in", res = dpi, background = "white")
  } else {
    grDevices::png(f, width = w_in, height = h_in, units = "in", res = dpi, bg = "white")
  }
  ok <- tryCatch({ print(plot); TRUE }, error = function(e) e)
  grDevices::dev.off()
  if (!isTRUE(ok)) stop(conditionMessage(ok))
  f
}

slb_add_plot <- function(pres, plot, title, subtitle, pal, footer) {
  bw <- SLB_W - 1.0; bh <- 5.8
  f <- slb_plot_png(plot, bw, bh)
  pres <- slb_frame(pres, title, subtitle, pal, footer)
  officer::ph_with(pres, value = officer::external_img(f, width = bw, height = bh, unit = "in"),
                   location = officer::ph_location(left = 0.5, top = 1.15, width = bw, height = bh))
}

# width, height in pixels from the PNG header (no extra package needed)
slb_png_dim <- function(f) {
  b <- readBin(f, "raw", 24)
  rd <- function(i) sum(as.integer(b[i:(i + 3)]) * c(16777216, 65536, 256, 1))
  c(rd(17), rd(21))
}

slb_table_dims <- function(ft) {
  d <- tryCatch(flextable::flextable_dim(ft), error = function(e) NULL)
  if (is.null(d)) list(width = NA_real_, height = NA_real_) else d
}

slb_add_table <- function(pres, ft, title, subtitle, pal, footer) {
  maxw <- SLB_W - 0.8; maxh <- 5.75
  fitted <- NULL
  for (fs in c(16, 14, 12, 11, 10, 9, 8, 7)) {
    ft2 <- tryCatch({
      x <- flextable::fontsize(ft, size = fs, part = "all")
      x <- flextable::autofit(x)
      flextable::fit_to_width(x, max_width = maxw)
    }, error = function(e) NULL)
    if (is.null(ft2)) next
    d <- slb_table_dims(ft2)
    fitted <- list(ft = ft2, dims = d)
    if (!is.na(d$height) && d$height <= maxh) break
  }
  if (is.null(fitted)) stop("table could not be sized")
  pres <- slb_frame(pres, title, subtitle, pal, footer)
  d <- fitted$dims
  if (!is.na(d$height) && d$height > maxh) {
    # still too tall: place it as a picture scaled to the slide (complete but small)
    f <- tempfile(fileext = ".png")
    flextable::save_as_image(fitted$ft, path = f, res = 200)
    dims <- slb_png_dim(f); asp <- dims[1] / dims[2]
    w <- min(maxw, maxh * asp); h <- w / asp
    return(officer::ph_with(pres, value = officer::external_img(f, width = w, height = h, unit = "in"),
                            location = officer::ph_location(left = (SLB_W - w) / 2, top = 1.2, width = w, height = h)))
  }
  left <- if (!is.na(d$width)) max(0.4, (SLB_W - d$width) / 2) else 0.4
  officer::ph_with(pres, value = fitted$ft, location = officer::ph_location(left = left, top = 1.25))
}

# metrics: list of list(label=, value=) -> row of cards
slb_add_metrics <- function(pres, metrics, title, subtitle, pal, footer) {
  n <- length(metrics)
  pres <- slb_frame(pres, title, subtitle, pal, footer)
  per_row <- if (n <= 4) max(n, 1) else if (n <= 8) 4 else 5
  gap <- 0.3; cw <- (SLB_W - 1.0 - gap * (per_row - 1)) / per_row; ch <- 1.7
  vsize <- if (per_row >= 4) 30 else 40
  for (i in seq_len(n)) {
    r <- (i - 1) %/% per_row; c <- (i - 1) %% per_row
    col <- pal$cards[(i - 1) %% length(pal$cards) + 1]
    pars <- list(slb_par(slb_txt(metrics[[i]]$label, 13, "#374151", bold = TRUE), left = 14, top = 14),
                 slb_par(slb_txt(metrics[[i]]$value, vsize, col, bold = TRUE), left = 14, top = 6))
    pres <- slb_box(pres, pars, 0.5 + c * (cw + gap), 1.5 + r * (ch + gap), cw, ch, bg = pal$card)
  }
  pres
}

# ---- items per analysis -----------------------------------------------------------
# Each item: list(kind = "maps"|"charts"|"tables"|"metrics", type = "plot"|"table"|"metrics",
#                 title, sub, obj)
slb_item <- function(kind, type, title, obj, sub = NULL) {
  if (is.null(obj)) return(NULL)
  list(kind = kind, type = type, title = title, sub = sub, obj = obj)
}

slb_df_table <- function(df, max_rows = 18) {
  df <- as.data.frame(df)
  if (!nrow(df)) return(NULL)
  df <- utils::head(df, max_rows)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(v) round(v, 1))
  ft <- flextable::flextable(df)
  ft <- flextable::theme_zebra(ft)
  flextable::autofit(ft)
}

slb_items <- function(key, res) {
  per <- function(x) if (!is.null(x$period_info) && length(x$period_info) == 1) as.character(x$period_info) else NULL
  switch(key,
    scope = Filter(Negate(is.null), list(
      slb_item("maps", "plot", "SIA Scope Analysis", res$plot, "Districts in scope by round"))),
    admin = Filter(Negate(is.null), list(
      slb_item("tables", "table", "Admin Data Overview - Coverage", res$coverage$flextable),
      slb_item("tables", "table", "Admin Data Overview - Vaccinated", res$vaccinated$flextable))),
    missed = Filter(Negate(is.null), list(
      slb_item("tables", "table", "Missed Children Analysis", res$flextable, res$period_text))),
    coverage = Filter(Negate(is.null), list(
      slb_item("tables", "table", "Coverage Analysis", res$flextable, res$period_text))),
    district_perf = {
      sm <- res$summary_metrics
      Filter(Negate(is.null), list(
        slb_item("metrics", "metrics", "LQAS District Performance - Key Numbers",
                 if (!is.null(sm)) list(
                   list(label = "Total districts", value = slb_num(sm$total_districts)),
                   list(label = "High performing", value = slb_num(sm$high_performing_districts)),
                   list(label = "Poor performing", value = slb_num(sm$poor_performing_districts))), per(res)),
        slb_item("maps", "plot", "LQAS District Performance - Map", res$multiplot, per(res)),
        slb_item("charts", "plot", "LQAS District Performance - Heatmap", res$heatmap, per(res)),
        slb_item("tables", "table", "LQAS District Performance - Summary", res$summary_table, per(res))))
    },
    lqas_maps = {
      sm <- res$summary_metrics
      Filter(Negate(is.null), list(
        slb_item("metrics", "metrics", "LQAS Summary - Key Numbers",
                 if (!is.null(sm)) list(
                   list(label = "Total districts", value = slb_num(sm$total_districts)),
                   list(label = "Always high performing", value = slb_num(sm$always_high)),
                   list(label = "Never high performing", value = slb_num(sm$never_high))), per(res)),
        slb_item("maps", "plot", "LQAS Summary Map - Districts", res$map, per(res)),
        slb_item("maps", "plot", "LQAS Summary Map - Provinces", res$province_map, per(res)),
        slb_item("tables", "table", "LQAS Summary - By Country", res$table_flex, per(res)),
        slb_item("tables", "table", "LQAS Summary - By Province", res$province_summary_flex, per(res))))
    },
    reasons = Filter(Negate(is.null), list(
      slb_item("charts", "plot", "Reasons for Missed Children", res$combined_plot),
      slb_item("tables", "table", "Reasons - Top Reasons Summary",
               if (!is.null(res$top_reasons_summary)) slb_df_table(res$top_reasons_summary)))),
    unresolved = {
      pl <- res$plots
      if (is.null(pl) || !length(pl)) list() else
        lapply(names(pl), function(nm) slb_item("maps", "plot", paste("Unresolved Cases -", nm), pl[[nm]], per(res)))
    },
    im_overview = Filter(Negate(is.null), list(
      slb_item("metrics", "metrics", "IM Overview - Key Numbers", res$metrics, per(res)),
      slb_item("charts", "plot", "IM Overview - Coverage", res$cv_plot, per(res)),
      slb_item("charts", "plot", "IM Overview - Trend", res$trend_plot, per(res)),
      slb_item("charts", "plot", "IM Overview - District Risk", res$risk_plot, per(res)))),
    im_reasons = Filter(Negate(is.null), list(
      slb_item("charts", "plot", "IM - Reasons for Missed Children", res$plot, per(res)))),
    im_priority = Filter(Negate(is.null), list(
      slb_item("tables", "table", "IM - Priority Districts", res$table, per(res)))),
    list())
}

SLB_ANALYSIS_LABELS <- c(
  scope = "SIA Scope Analysis", admin = "Admin Data Overview",
  missed = "Missed Children Analysis", coverage = "Coverage Analysis",
  district_perf = "LQAS District Performance", lqas_maps = "LQAS Summary Maps",
  reasons = "Reasons Analysis", unresolved = "Unresolved Cases",
  im_overview = "IM Overview", im_reasons = "IM Reasons for Missed Children",
  im_priority = "IM Priority Districts")

# ---- title / scope / closing slides -------------------------------------------------
slb_title_slide <- function(pres, meta, pal) {
  pres <- officer::add_slide(pres, layout = "Blank", master = "Office Theme")
  pars <- list(
    slb_par(slb_txt(meta$title, 40, pal$title_text, bold = TRUE), left = 60, top = 150),
    slb_par(slb_txt(meta$subtitle, 20, pal$sub_text), left = 60, top = 10),
    slb_par(slb_txt(paste0(meta$author, "  |  ", format(meta$date, "%d %B %Y")), 14, pal$sub_text),
            left = 60, top = 26))
  pres <- slb_box(pres, pars, 0, 0, SLB_W, SLB_H, bg = pal$title_bg)
  officer::ph_with(pres, value = "", location = officer::ph_location(
    left = 0.8, top = 4.55, width = 3.0, height = 0.07, bg = pal$rule))
}

slb_scope_slide <- function(pres, meta, included, pal, footer) {
  pres <- slb_frame(pres, "About this report", "Filters used and analyses included", pal, footer)
  rows <- list(
    c("Period", meta$period_text), c("Geography", meta$geo_text),
    c("Output types", meta$outputs_text), c("Analyses", paste(included, collapse = ", ")))
  pars <- list()
  for (r in rows) {
    pars[[length(pars) + 1]] <- slb_par(slb_txt(r[1], 12, pal$muted, bold = TRUE), left = 0, top = 10)
    pars[[length(pars) + 1]] <- slb_par(slb_txt(r[2], 16, pal$body_text), left = 0, top = 0)
  }
  slb_box(pres, pars, 0.8, 1.4, SLB_W - 1.6, 5.3)
}

slb_closing_slide <- function(pres, meta, skipped, pal, footer) {
  pres <- slb_frame(pres, "Notes", "Data and method", pal, footer)
  lines <- c(
    "Source: WHO AFRO independent monitoring (LQAS and IM) repositories, SIA scope and administrative data.",
    paste0("Production date: ", format(meta$date, "%d %B %Y"), "."),
    "Maps and tables reflect the filters listed on the 'About this report' slide.")
  if (length(skipped)) lines <- c(lines, paste0("Not included (no data or an error): ", paste(skipped, collapse = "; "), "."))
  pars <- lapply(lines, function(l) slb_par(slb_txt(l, 16, pal$body_text), top = 10))
  slb_box(pres, pars, 0.8, 1.4, SLB_W - 1.6, 5.3)
}

# ---- main entry point ------------------------------------------------------------------
# results : named list (scope, admin, ...) as stored by the Generate Slides tab
# meta    : list(title, author, subtitle, period_text, geo_text, date)
# outputs : any of "tables", "maps", "charts", "metrics"
slb_build_pptx <- function(results, meta, analyses = names(results),
                           outputs = c("tables", "maps", "charts", "metrics"),
                           template = "default",
                           template_file = file.path("www", "slides_template_16x9.pptx"),
                           progress = function(msg) NULL) {
  pal <- slb_palette(template)
  meta$date <- if (is.null(meta$date)) Sys.Date() else meta$date
  if (is.null(meta$subtitle)) meta$subtitle <- paste0(meta$geo_text, " | ", meta$period_text)
  meta$outputs_text <- paste(c(tables = "Summary tables", maps = "Maps", charts = "Charts",
                               metrics = "Key numbers")[outputs], collapse = ", ")
  footer <- paste0("WHO AFRO | ", meta$geo_text, " | ", meta$period_text,
                   " | Produced ", format(meta$date, "%d %b %Y"))

  pres <- if (file.exists(template_file)) officer::read_pptx(template_file) else officer::read_pptx()
  skipped <- character(0)
  included <- character(0)

  keys <- intersect(names(SLB_ANALYSIS_LABELS), analyses)
  keys <- keys[keys %in% names(results)]

  # build the body first so the scope slide lists only what really made it in
  body <- list()
  for (k in keys) {
    items <- slb_items(k, results[[k]])
    items <- Filter(function(it) !is.null(it) && it$kind %in% outputs, items)
    if (!length(items)) { skipped <- c(skipped, paste0(SLB_ANALYSIS_LABELS[[k]], " (nothing to show for the chosen output types)")); next }
    body[[k]] <- items
  }

  pres <- slb_title_slide(pres, meta, pal)
  pres <- slb_scope_slide(pres, meta, SLB_ANALYSIS_LABELS[names(body)], pal, footer)

  for (k in names(body)) {
    progress(paste("Adding", SLB_ANALYSIS_LABELS[[k]]))
    ok_any <- FALSE
    for (it in body[[k]]) {
      res <- tryCatch({
        pres <- switch(it$type,
          plot = slb_add_plot(pres, it$obj, it$title, it$sub, pal, footer),
          table = slb_add_table(pres, it$obj, it$title, it$sub, pal, footer),
          metrics = slb_add_metrics(pres, it$obj, it$title, it$sub, pal, footer))
        TRUE
      }, error = function(e) {
        skipped <<- c(skipped, paste0(it$title, ": ", conditionMessage(e)))
        FALSE
      })
      if (isTRUE(res)) ok_any <- TRUE
    }
    if (ok_any) included <- c(included, SLB_ANALYSIS_LABELS[[k]])
  }

  pres <- slb_closing_slide(pres, meta, skipped, pal, footer)
  list(pres = pres, skipped = skipped, included = included, n_slides = length(pres))
}

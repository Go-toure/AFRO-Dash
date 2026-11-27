# slides_modern.R -- the dashboard's MODERN views as slide figures.   Author: Gorgui Ba Toure
# One place for the figures of the weekly technical deck and of the "Generate slides" tab: same functions as the Admin, LQAS, Reasons and IM Overview tabs,
# same arguments. Each analysis returns list(plot, w, h) (a ggplot drawn for a 13 in wide page) or stops with the reason.
# Needs (already loaded by app.R): admin_heatmap_modern.R, lqas_summary_modern.R, reasons_refusal_modern.R, im_slides.R, im_slides_modern.R.
#
# ctx = list(dat, admin_data, afro_blocks, months, years, block, countries, provinces, districts, geo_args, lookups, im_df)

SLM_LABELS <- c(
  m_admin_vacc = "Admin: children vaccinated by country and month",
  m_admin_cov  = "Admin: districts reaching 95% coverage by country and month",
  m_lqas_exec  = "LQAS: performance over time (executive view)",
  m_reasons    = "LQAS: reasons children are missed (heatmap)",
  m_im_cards   = "IM: intelligence report (key numbers)",
  m_im_cv95    = "IM: districts at 95% coverage",
  m_im_opfail  = "IM: operational failure",
  m_im_sm      = "IM: social mobilisation")
SLM_KEYS <- names(SLM_LABELS)
# the nine slides of the weekly technical deck, in order ("scope" is the existing SIA scope analysis)
SLM_WEEKLY <- c("scope", "m_admin_vacc", "m_admin_cov", "m_lqas_exec", "m_reasons", "m_im_cards", "m_im_cv95", "m_im_opfail", "m_im_sm")

slm_call <- function(fn, args, keep_null = character(0)) {
  fm <- names(formals(fn))
  if (!("..." %in% fm)) args <- args[names(args) %in% fm]
  nul <- vapply(args, is.null, logical(1)); args <- args[!nul | names(args) %in% keep_null]
  do.call(fn, args)
}
slm_fig <- function(plot, h, w = 13) { if (is.null(plot)) stop("nothing to draw for these filters"); list(plot = plot, w = w, h = h) }
slm_period_txt <- function(ctx) if (!is.null(ctx$months)) paste("Last", ctx$months, "months") else if (!is.null(ctx$years)) paste("Year", paste(ctx$years, collapse = ", ")) else "All available data"

# the Independent Monitoring data, filtered like the IM Overview tab (once per generation)
slm_im_df <- function(ctx) {
  ims_prepare(ims_load(), afro_blocks = ctx$afro_blocks, block = ctx$block, countries = ctx$countries,
              provinces = ctx$provinces, districts = ctx$districts, x_months = ctx$months, years = ctx$years)
}

slm_admin_args <- function(ctx) list(admin_data = ctx$admin_data, afro_blocks = ctx$afro_blocks, x_months = ctx$months, year_selection = ctx$years,
                                     block_selection = if (is.null(ctx$block)) "All" else ctx$block, country_selection = ctx$countries)

slm_build <- function(key, ctx) {
  switch(key,
    m_admin_vacc = {
      r <- slm_call(admin_vaccinated_summary, slm_admin_args(ctx), keep_null = "x_months")
      if (is.null(r$doses_df) || !nrow(r$doses_df)) stop("no administrative data for these filters")
      slm_fig(admin_vacc_with_cards_gg(r$doses_df, r$block_display, r$period_text, r$metrics), admin_vacc_height_cards(r$doses_df))
    },
    m_admin_cov = {
      r <- slm_call(admin_coverage_summary, slm_admin_args(ctx), keep_null = "x_months")
      if (is.null(r$dist_df) || !nrow(r$dist_df)) stop("no administrative data for these filters")
      slm_fig(admin_cm_with_cards_gg(r$dist_df, r$block_display, r$period_text, r$metrics), admin_country_month_height_cards(r$dist_df))
    },
    m_lqas_exec = {
      single_year <- if (length(ctx$years) == 1) ctx$years else NULL
      lq <- slm_call(generate_lqas_summary_map_optimized,
                     c(list(data = ctx$dat, x = ctx$months, y = single_year, performance_ranges = NULL, show_province_labels = FALSE, block_type = "afro"), ctx$geo_args, ctx$lookups))
      g <- lqas_exec_gg(lq)
      slm_fig(g, lqas_exec_height(lq))
    },
    m_reasons = {
      single_year <- if (length(ctx$years) == 1) ctx$years else NULL
      filt <- refusal_filter_lqas(ctx$dat, x_months = if (is.null(single_year)) ctx$months else NULL, specific_month = NULL, specific_year = single_year,
                                  block_selection = ctx$block, country_selection = ctx$countries,
                                  province_selection = ctx$provinces, district_selection = ctx$districts, block_definition = ctx$afro_blocks)
      g <- reason_heat_gg(filt, c("traditional", "nc", "absence"), "country", attr(filt, "period_label"))
      slm_fig(g$plot, g$height)
    },
    m_im_cards  = { df <- ctx$im_df; if (is.null(df)) stop("no Independent Monitoring records for these filters"); slm_fig(imw_cards_fig(df, slm_period_txt(ctx)), 5.2) },
    m_im_cv95   = { df <- ctx$im_df; if (is.null(df)) stop("no Independent Monitoring records for these filters"); r <- imw_cv95_fig(df, slm_period_txt(ctx)); slm_fig(r$plot, r$height) },
    m_im_opfail = { df <- ctx$im_df; if (is.null(df)) stop("no Independent Monitoring records for these filters"); slm_fig(imw_opfail_fig(df, slm_period_txt(ctx)), 6) },
    m_im_sm     = { df <- ctx$im_df; if (is.null(df)) stop("no Independent Monitoring records for these filters"); slm_fig(imw_sm_fig(df, slm_period_txt(ctx)), 6.4) },
    stop("unknown analysis: ", key))
}

# PNG of a figure (the same device settings as the slide builder)
slm_png <- function(fig, dpi = 150) {
  f <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(f, width = fig$w, height = fig$h, units = "in", res = dpi, background = "white")
  else grDevices::png(f, width = fig$w, height = fig$h, units = "in", res = dpi, bg = "white")
  ok <- tryCatch({ print(fig$plot); TRUE }, error = function(e) e)
  grDevices::dev.off()
  if (!isTRUE(ok)) stop(conditionMessage(ok))
  f
}

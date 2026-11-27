# Weekly technical deck of the SIA dashboard, built headless with the dashboard's own functions.   Author: Gorgui Ba Toure
# Run from the AFRO-Dash folder:   Rscript <path>/weekly_deck.R [--months=12] [--out=<folder>] [--template=default|minimal|corporate] [--block=WA] [--countries=NIGERIA,CHAD]
# It evaluates everything in app.R that comes BEFORE the `ui <-` line (libraries, data, definitions, modules), then does what the
# "Generate slides" button does: same analysis functions, same arguments, then slb_build_pptx(). Same definitions as the dashboard.
a <- commandArgs(trailingOnly = TRUE)
opt <- function(k, d = NULL) { h <- grep(paste0("^--", k, "="), a, value = TRUE); if (length(h)) sub(paste0("^--", k, "="), "", h[1]) else d }
months <- as.integer(opt("months", "12")); template <- opt("template", "default")
out_dir <- opt("out", file.path("..", "_agent_jobs", "outputs")); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
block <- opt("block"); countries <- if (!is.null(opt("countries"))) strsplit(opt("countries"), ",")[[1]] else NULL
if (!file.exists("app.R")) stop("run this from the AFRO-Dash folder (app.R not found here)")

cat("== loading the dashboard definitions and data (app.R before ui) ==\n")
exprs <- parse("app.R", keep.source = FALSE)
is_ui <- vapply(exprs, function(e) is.call(e) && length(e) >= 3 && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("ui")), logical(1))
stopifnot(any(is_ui))
for (i in seq_len(which(is_ui)[1] - 1)) {
  tryCatch(eval(exprs[[i]], globalenv()), error = function(e) cat("[app.R expression", i, "skipped]", conditionMessage(e), "\n"))
}
for (obj in c("dat", "scope", "admin_data", "afro_blocks", "slb_build_pptx", "ims_load")) if (!exists(obj)) cat("WARNING: not defined after loading app.R:", obj, "\n")

geo_args <- list(block_selection = block, country_selection = countries, province_selection = NULL, district_selection = NULL)
lookups <- list(afro_blocks = get0("afro_blocks"), ist_blocks = get0("ist_blocks"), all_countries = get0("all_countries"),
                all_provinces = get0("all_provinces"), all_districts = get0("all_districts"),
                ambiguous_district_pairs = get0("AMBIGUOUS_DISTRICT_PAIRS"))
call_fn <- function(fn, args, keep_null = character(0)) {
  fm <- names(formals(fn))
  if (!("..." %in% fm)) args <- args[names(args) %in% fm]
  nul <- vapply(args, is.null, logical(1)); args <- args[!nul | names(args) %in% keep_null]
  do.call(fn, args)
}
failures <- character(0)
bsel <- if (is.null(block)) "All" else block
period_txt <- paste0("last ", months, " months")

# ---- the figures: the dashboard's MODERN views (the ones of the Admin, LQAS and Reasons tabs), same arguments ----
figs <- list()   # each: list(title, png, w, h)
png_of <- function(plot, w, h, dpi = 150) {
  f <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(f, width = w, height = h, units = "in", res = dpi, background = "white")
  else grDevices::png(f, width = w, height = h, units = "in", res = dpi, bg = "white")
  ok <- tryCatch({ print(plot); TRUE }, error = function(e) e)
  grDevices::dev.off()
  if (!isTRUE(ok)) stop(conditionMessage(ok))
  f
}
add_fig <- function(title, expr_fn, sub = NULL) {
  cat("-- figure:", title, "\n")
  r <- tryCatch(expr_fn(), error = function(e) { failures <<- c(failures, paste0(title, ": ", conditionMessage(e))); cat("   FAILED:", conditionMessage(e), "\n"); NULL })
  figs <<- c(figs, list(if (is.null(r)) NULL else c(list(title = title, sub = sub), r)))   # a failed figure keeps its slot, so the slides never shift
}

# 1  SIA scope (monthly maps)
scope_res <- tryCatch(call_fn(generate_scope_plots_app, c(list(scope_data = scope), geo_args, list(x_months = months, year_selection = NULL), lookups)), error = function(e) { failures <<- c(failures, paste("scope:", conditionMessage(e))); NULL })
add_fig(paste("SIA scope in the", period_txt), function() { if (is.null(scope_res$plot)) stop("no scope plot"); list(png = png_of(scope_res$plot, 13, 7.6), w = 13, h = 7.6) })

# 2-9  the modern views, built by the same code as the "Generate slides" tab (slides_modern.R): admin x2, LQAS executive view, reasons heatmap, IM x4
if (!exists("slm_build")) source("slides_modern.R")
ctx <- list(dat = dat, admin_data = admin_data, afro_blocks = afro_blocks, months = months, years = NULL, block = block, countries = countries,
            provinces = NULL, districts = NULL, geo_args = geo_args, lookups = lookups)
ctx$im_df <- tryCatch(slm_im_df(ctx), error = function(e) { failures <<- c(failures, paste("IM:", conditionMessage(e))); NULL })
for (key in SLM_WEEKLY[-1]) local({
  k <- key
  add_fig(SLM_LABELS[[k]], function() { r <- slm_build(k, ctx); list(png = slm_png(r), w = r$w, h = r$h) })
})

cat("== building the deck ==\n")
script_dir <- { f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]); if (is.na(f)) "." else dirname(normalizePath(f)) }
tpl <- opt("template-file", file.path(script_dir, "..", "catalog", "weekly_deck_template.pptx"))
if (!file.exists(tpl)) stop("the weekly deck template is missing: ", tpl)
suppressMessages(library(xml2))
pres <- officer::read_pptx(tpl)
box <- list(left = 0.15, top = 0.62, width = 13.03, height = 5.78)
slots <- length(pres)
for (i in seq_len(slots)) {
  fg <- if (i <= length(figs)) figs[[i]] else NULL
  sl <- pres$slide$get_slide(i)$get()
  for (nd in xml2::xml_find_all(sl, "/p:sld/p:cSld/p:spTree/p:pic | /p:sld/p:cSld/p:spTree/p:grpSp")) xml2::xml_remove(nd)   # never keep last week's picture
  pres <- officer::on_slide(pres, i)
  if (is.null(fg)) {
    pres <- officer::ph_with(pres, "This slide could not be built this week (see the job log).", location = officer::ph_location(left = 1, top = 3, width = 11, height = 1))
    next
  }
  sc <- min(box$width / fg$w, box$height / fg$h); w <- fg$w * sc; h <- fg$h * sc
  pres <- officer::ph_with(pres, officer::external_img(fg$png, width = w, height = h, unit = "in"),
                           location = officer::ph_location(left = box$left + (box$width - w) / 2, top = box$top + (box$height - h) / 2, width = w, height = h))
}
f <- file.path(out_dir, paste0("SIA_weekly_update_", format(Sys.Date(), "%d%m%Y"), ".pptx"))
print(pres, target = f)
nb <- sum(!vapply(figs, is.null, logical(1)))
cat("DECK:", normalizePath(f), "\n", "slides:", slots, "\n", "figures built:", nb, " of ", slots, "\n", sep = "")
if (length(failures)) cat("NOT BUILT:", paste(failures, collapse = " | "), "\n")
quit(status = if (nb == slots) 0 else 1)

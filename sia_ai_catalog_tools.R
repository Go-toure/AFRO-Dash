# ============================================================
# SIA AI catalogue tools: describe_data and query_data
# Author: Gorgui Ba Toure
#
# Two tools for the dashboard assistant (SmartAgent) that answer from the data with the OFFICIAL definitions of the catalogue
# (SIA_data_operations_and_analytics/catalog/semantic_layer.json), instead of a fixed list of questions:
#   catalog_describe(dataset)               what the dataset is, one row, metrics, dimensions, warnings
#   catalog_query(data_sources, dataset, metric, by, country, block, year, months, vaccine, ...)
#                                           one metric, optionally grouped (country, year, month, ...), with unit, period, formula and warnings
# No free code and no free column names are accepted: the metric and the grouping come from fixed lists (a language model can only choose among them),
# the result is aggregated (counts, sums, rates), never a record. Base R only.
# Datasets: "lqas" (data_sources$main_data) and "admin" (data_sources$admin_data), as loaded by the dashboard.
# ============================================================

CATALOG_TOOLS_VERSION <- "2026-11-26"

.cat_reason_cols <- c(
  "non-compliance" = "r_non_compliance", "house not visited" = "r_house_not_visited", "child absent" = "r_childabsent",
  "child asleep" = "r_Child_was_asleep", "child is a visitor" = "r_Child_is_a_visitor", "vaccinated but not finger-marked" = "r_Vaccinated_but_not_FM",
  "security" = "r_security", "child not born" = "r_childnotborn", "other (also holds unsampled children of under-reported lots)" = "other_r"
)

.cat_spec <- list(
  lqas = list(
    title = "LQAS lots (Lot Quality Assurance Sampling after a campaign)",
    row = "one row = one LQAS lot of one district and round: up to 6 clusters of 10 children, so at most 60 children sampled",
    source = "main_data", date_col = "lqas_start_date",
    dims = c(country = "country", year = "year", month = "month", vaccine = "vaccine.type", status = "status", performance = "performance",
             round = "roundNumber", response = "response", block = "afro_block", province = "province", district = "district",
             missed_in_lot = "total_missed"),
    metrics = list(
      lots = list(unit = "lots", def = "number of lots assessed", formula = "number of rows"),
      pass_rate = list(unit = "percent of lots", def = "share of lots that pass (at most 3 children missed in the lot of 60)", formula = "100 * mean(status == 'Pass')"),
      missed_children_rate = list(unit = "percent of children sampled", def = "the dashboard missed rate: sampled children not found vaccinated over children sampled", formula = "100 * (1 - sum(total_vaccinated) / sum(total_sampled))"),
      coverage_of_sampled = list(unit = "percent of children sampled", def = "the dashboard LQAS coverage: sampled children found vaccinated over children sampled", formula = "100 * sum(total_vaccinated) / sum(total_sampled)"),
      children_sampled = list(unit = "children", def = "children sampled", formula = "sum(total_sampled)"),
      children_vaccinated = list(unit = "children", def = "sampled children found vaccinated (NOT the number of children vaccinated in the country)", formula = "sum(total_vaccinated)"),
      children_missed = list(unit = "children", def = "sampled children not found vaccinated", formula = "sum(total_sampled) - sum(total_vaccinated)"),
      total_missed_lot_measure = list(unit = "children (lot measure)", def = "60 minus vaccinated in every lot: also counts the children never sampled in under-reported lots; used for pass or fail", formula = "sum(total_missed)"),
      unsampled_counted_missed = list(unit = "children", def = "children never sampled but counted as missed in under-reported lots (60 minus sampled)", formula = "sum(pmax(60 - total_sampled, 0))"),
      under_reported_lots = list(unit = "lots", def = "lots with fewer than 60 children sampled", formula = "sum(total_sampled < 60)"),
      districts = list(unit = "districts", def = "distinct districts with at least one lot", formula = "distinct (country, province, district)"),
      countries = list(unit = "countries", def = "distinct countries with at least one lot", formula = "distinct country"),
      campaigns = list(unit = "campaigns", def = "distinct campaign names (response) with lots", formula = "distinct response"),
      lots_below_3_clusters = list(unit = "lots", def = "lots with fewer than 30 children sampled (1 or 2 clusters), which all fail", formula = "sum(total_sampled < 30)"),
      share_without_geo_code = list(unit = "percent of lots", def = "share of lots with no AFROGDB geo_code (cannot be joined to the geographic reference)", formula = "100 * mean(is.na(geo_code))"),
      latest_date = list(unit = "date", def = "date of the most recent survey (round start date)", formula = "max(lqas_start_date)"),
      reasons = list(unit = "children", def = "reasons for missed children (sum of the r_* counts) with their share; by default over complete lots (60 sampled) only", formula = "sum of r_* columns over lots with total_sampled == 60")
    )
  ),
  admin = list(
    title = "Administrative vaccination data (polio SIAs, one record per district, round and vaccine)",
    row = "one row = one district, one campaign round and one vaccine; a co-administered campaign is ONE combined row",
    source = "admin_data", date_col = "SIA_date",
    dims = c(country = "Country", year = "Year", month = "month", vaccine = "Vaccine_type", block = "AFRO_block", response = "Response", round = "Round_Add", province = "Province"),
    metrics = list(
      records = list(unit = "records", def = "number of district-round-vaccine records", formula = "number of rows"),
      children_vaccinated = list(unit = "children", def = "children vaccinated with polio vaccine (measles-rubella records excluded); for co-administered rows the larger of the two vaccine totals", formula = "sum(TotalNbrVaccPolio) where Vaccine_type != 'MR'"),
      target_population = list(unit = "children", def = "polio target population of the records (MR excluded)", formula = "sum(PopPolio) where Vaccine_type != 'MR'"),
      coverage = list(unit = "ratio (1.00 = 100%)", def = "administrative coverage: children vaccinated over target, over records that have both figures and a target above zero, MR excluded", formula = "sum(TotalNbrVaccPolio) / sum(PopPolio)"),
      share_districts_95 = list(unit = "percent of records", def = "share of district records with coverage of at least 95%", formula = "100 * mean(TotalNbrVaccPolio / PopPolio >= 0.95)"),
      records_above_1_5 = list(unit = "records", def = "records with coverage above 1.5 (not credible)", formula = "sum(TotalNbrVaccPolio / PopPolio > 1.5)"),
      campaigns = list(unit = "campaigns", def = "distinct campaign names (Response)", formula = "distinct Response"),
      countries = list(unit = "countries", def = "distinct countries with records", formula = "distinct Country"),
      share_co_administered = list(unit = "percent of records", def = "share of records of co-administered campaigns (nOPV2 & bOPV, or nOPV2_bOPV_IPV)", formula = "100 * mean(Vaccine_type in co-administered)"),
      records_without_geo_code = list(unit = "records", def = "records with no AFROGDB geo_code", formula = "sum(is.na(geo_code))"),
      latest_date = list(unit = "date", def = "latest SIA start date in the data", formula = "max(SIA_date)")
    )
  )
)

.cat_warnings <- list(
  lqas = c(
    "A row is a lot, not a child and not a district.",
    "LQAS samples children: it never gives the number of children vaccinated; use the administrative data for that.",
    "total_missed (lot measure) counts children never sampled in under-reported lots, so it is higher than the dashboard missed rate.",
    "DR Congo, Algeria, South Sudan, Nigeria, Rwanda and Ethiopia use their own lot selection logic."
  ),
  admin = c(
    "Coverage above 1.5 is not credible (target too low): quote the number of such records beside any coverage.",
    "Measles-rubella (MR) rows are not polio and are left out of polio figures.",
    "For co-administered rows the total is the larger of the two vaccine totals: never add nOPV2 and bOPV.",
    "Absence of a record is not zero coverage: pending rounds are not in the file."
  )
)

.cat_df <- function(ds, data_sources) {
  d <- data_sources[[.cat_spec[[ds]]$source]]
  if (is.null(d)) return(NULL)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  dc <- .cat_spec[[ds]]$date_col
  if (dc %in% names(d)) {
    dd <- suppressWarnings(as.Date(substr(as.character(d[[dc]]), 1, 10)))
    d$.date <- dd
    d$month <- format(dd, "%Y-%m")
  } else {
    d$.date <- as.Date(NA); d$month <- NA_character_
  }
  d
}

catalog_describe <- function(dataset = "lqas") {
  dataset <- tolower(dataset %||% "lqas")
  if (!dataset %in% names(.cat_spec)) return(list(error = "unknown dataset", datasets = names(.cat_spec)))
  s <- .cat_spec[[dataset]]
  list(
    dataset = dataset, title = s$title, one_row_is = s$row,
    metrics = lapply(s$metrics, function(m) list(unit = m$unit, definition = m$def, formula = m$formula)),
    group_by_options = names(s$dims), filters = c("country", "block", "year", "months", "vaccine", if (dataset == "lqas") c("performance", "status"), "min_n (smallest number of rows for a group to be listed)"),
    warnings = .cat_warnings[[dataset]], catalogue_version = CATALOG_TOOLS_VERSION
  )
}

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

catalog_query <- function(data_sources, dataset = "lqas", metric = NULL, by = NULL, country = NULL, block = NULL, year = NULL,
                          months = NULL, vaccine = NULL, performance = NULL, status = NULL, min_n = NULL, complete_lots_only = TRUE, top_n = 20) {
  dataset <- tolower(dataset %||% "lqas")
  if (!dataset %in% names(.cat_spec)) return(list(error = "unknown dataset: use 'lqas' or 'admin'", datasets = names(.cat_spec)))
  s <- .cat_spec[[dataset]]
  metric <- tolower(metric %||% "")
  if (!metric %in% names(s$metrics)) return(list(error = paste0("unknown metric for ", dataset), valid_metrics = names(s$metrics)))
  by <- unlist(by); by <- by[!is.na(by) & nzchar(by)]
  if (length(by) > 2) by <- by[1:2]
  if (length(by) && !all(by %in% names(s$dims))) return(list(error = "unknown grouping", valid_group_by = names(s$dims)))
  d <- .cat_df(dataset, data_sources)
  if (is.null(d) || nrow(d) == 0) return(list(error = paste0("no ", dataset, " data loaded")))
  applied <- list()
  col <- function(k) s$dims[[k]]
  if (!is.null(country) && length(country)) {
    cc <- toupper(trimws(as.character(unlist(country)))); have <- unique(toupper(as.character(d[[col("country")]])))
    miss <- setdiff(cc, have)
    if (length(miss)) return(list(error = paste0("country not found: ", paste(miss, collapse = ", ")), suggestions = unique(unlist(lapply(miss, function(m) agrep(m, have, value = TRUE, max.distance = 0.2))))))
    d <- d[toupper(as.character(d[[col("country")]])) %in% cc, , drop = FALSE]; applied$country <- cc
  }
  if (!is.null(block) && length(block) && col("block") %in% names(d)) {
    b <- toupper(trimws(as.character(unlist(block)))); d <- d[toupper(as.character(d[[col("block")]])) %in% b, , drop = FALSE]; applied$block <- b
  }
  if (!is.null(vaccine) && length(vaccine)) {
    v <- tolower(trimws(as.character(unlist(vaccine)))); d <- d[tolower(as.character(d[[col("vaccine")]])) %in% v, , drop = FALSE]; applied$vaccine <- v
  }
  if (dataset == "lqas" && !is.null(performance) && length(performance)) {
    pf <- tolower(trimws(as.character(unlist(performance)))); d <- d[tolower(as.character(d$performance)) %in% pf, , drop = FALSE]; applied$performance <- pf
  }
  if (dataset == "lqas" && !is.null(status) && length(status)) {
    st <- tolower(trimws(as.character(unlist(status)))); d <- d[tolower(as.character(d$status)) %in% st, , drop = FALSE]; applied$status <- st
  }
  if (!is.null(year) && length(year)) {
    y <- suppressWarnings(as.integer(unlist(year))); d <- d[as.integer(d[[col("year")]]) %in% y, , drop = FALSE]; applied$year <- y
  }
  if (!is.null(months) && is.numeric(months) && months > 0 && any(!is.na(d$.date))) {
    mx <- max(d$.date, na.rm = TRUE); lt <- seq(mx, by = paste0("-", as.integer(months), " months"), length.out = 2)[2]
    d <- d[!is.na(d$.date) & d$.date > lt, , drop = FALSE]; applied$last_months <- as.integer(months)
  }
  if (nrow(d) == 0) return(list(error = "no records match these filters", filters = applied))
  warn <- character(0)
  num <- function(x) suppressWarnings(as.numeric(x))

  # ---- one value of a metric over a subset of rows
  value_of <- function(x) {
    if (dataset == "lqas") {
      samp <- num(x$total_sampled); vac <- num(x$total_vaccinated)
      switch(metric,
        lots = nrow(x), pass_rate = 100 * mean(x$status == "Pass", na.rm = TRUE),
        missed_children_rate = 100 * (1 - sum(vac, na.rm = TRUE) / sum(samp, na.rm = TRUE)),
        coverage_of_sampled = 100 * sum(vac, na.rm = TRUE) / sum(samp, na.rm = TRUE),
        children_sampled = sum(samp, na.rm = TRUE), children_vaccinated = sum(vac, na.rm = TRUE),
        children_missed = sum(samp, na.rm = TRUE) - sum(vac, na.rm = TRUE),
        total_missed_lot_measure = sum(num(x$total_missed), na.rm = TRUE),
        unsampled_counted_missed = sum(pmax(60 - samp, 0), na.rm = TRUE), under_reported_lots = sum(samp < 60, na.rm = TRUE),
        districts = nrow(unique(x[, intersect(c("country", "province", "district"), names(x)), drop = FALSE])),
        countries = length(unique(x$country)), campaigns = length(unique(x$response[!is.na(x$response)])),
        lots_below_3_clusters = sum(samp < 30, na.rm = TRUE), share_without_geo_code = 100 * mean(is.na(x$geo_code) | x$geo_code == ""),
        latest_date = if (any(!is.na(x$.date))) as.numeric(max(x$.date, na.rm = TRUE)) else NA_real_,
        NA_real_)
    } else {
      pol <- x[is.na(x$Vaccine_type) | x$Vaccine_type != "MR", , drop = FALSE]
      v <- num(pol$TotalNbrVaccPolio); p <- num(pol$PopPolio); ok <- !is.na(v) & !is.na(p) & p > 0
      switch(metric,
        records = nrow(x), children_vaccinated = sum(v, na.rm = TRUE), target_population = sum(p, na.rm = TRUE),
        coverage = if (any(ok)) sum(v[ok]) / sum(p[ok]) else NA_real_,
        share_districts_95 = if (any(ok)) 100 * mean(v[ok] / p[ok] >= 0.95) else NA_real_,
        records_above_1_5 = { va <- num(x$TotalNbrVaccPolio); pa <- num(x$PopPolio); sum(!is.na(va) & !is.na(pa) & pa > 0 & va / pa > 1.5) },
        campaigns = length(unique(x$Response[!is.na(x$Response)])), countries = length(unique(x$Country)),
        share_co_administered = 100 * mean(x$Vaccine_type %in% c("nOPV2 & bOPV", "nOPV2_bOPV_IPV")),
        records_without_geo_code = sum(is.na(x$geo_code) | x$geo_code == ""),
        latest_date = if (any(!is.na(x$.date))) as.numeric(max(x$.date, na.rm = TRUE)) else NA_real_,
        NA_real_)
    }
  }

  period <- if (any(!is.na(d$.date))) paste0(format(min(d$.date, na.rm = TRUE)), " to ", format(max(d$.date, na.rm = TRUE))) else "dates not available"
  if (is.null(applied$year) && is.null(applied$last_months)) warn <- c(warn, "No period was chosen: the figure covers all dates in the data; say it, or ask the user for a period.")

  result <- list(dataset = dataset, metric = metric, unit = s$metrics[[metric]]$unit, definition = s$metrics[[metric]]$def,
                 formula = s$metrics[[metric]]$formula, filters = applied, period = period, rows_used = nrow(d))

  if (dataset == "lqas" && metric == "reasons") {
    x <- d
    if (isTRUE(complete_lots_only)) { x <- d[num(d$total_sampled) == 60, , drop = FALSE]; result$note <- "reasons are counted over complete lots (60 children sampled): 'other' is inflated in under-reported lots" }
    cols <- .cat_reason_cols[.cat_reason_cols %in% names(x)]
    tot <- vapply(cols, function(cn) sum(num(x[[cn]]), na.rm = TRUE), numeric(1))
    tb <- data.frame(reason = names(cols), children = as.integer(tot), share_percent = round(100 * tot / sum(tot), 1), stringsAsFactors = FALSE)
    tb <- tb[order(-tb$children), ]; rownames(tb) <- NULL
    result$rows_used <- nrow(x); result$table <- utils::head(tb, top_n)
  } else if (length(by) == 0) {
    v <- value_of(d)
    result$value <- if (metric == "latest_date") format(as.Date(v, origin = "1970-01-01")) else round(v, 4)
  } else {
    key <- do.call(paste, c(lapply(by, function(k) as.character(d[[col(k)]])), sep = " | "))
    vals <- vapply(split(seq_len(nrow(d)), key), function(ix) value_of(d[ix, , drop = FALSE]), numeric(1))
    tb <- data.frame(group = names(vals), value = round(as.numeric(vals), 4), n_rows = as.integer(vapply(split(seq_len(nrow(d)), key), length, integer(1))), stringsAsFactors = FALSE)
    if (!is.null(min_n) && is.numeric(min_n)) { tb <- tb[tb$n_rows >= min_n, , drop = FALSE]; applied$min_rows_per_group <- min_n; result$filters <- applied }
    ord <- if (any(by %in% c("year", "month"))) order(tb$group) else order(-tb$value)
    tb <- tb[ord, , drop = FALSE]; rownames(tb) <- NULL
    result$group_by <- by; result$groups_total <- nrow(tb); result$table <- utils::head(tb, top_n)
    if (nrow(tb) > top_n) warn <- c(warn, paste0("Only the first ", top_n, " of ", nrow(tb), " groups are shown."))
    result$overall <- if (metric == "latest_date") format(as.Date(value_of(d), origin = "1970-01-01")) else round(value_of(d), 4)
  }

  # ---- warnings that touch THIS figure
  if (dataset == "lqas") {
    n_under <- sum(num(d$total_sampled) < 60, na.rm = TRUE)
    if (n_under > 0 && metric %in% c("pass_rate", "total_missed_lot_measure", "lots", "reasons")) warn <- c(warn, paste0(n_under, " of ", nrow(d), " lots are under-reported (fewer than 60 children sampled); they count the unsampled children as missed."))
    if (metric == "total_missed_lot_measure") warn <- c(warn, "This is the lot measure; the dashboard missed rate (missed_children_rate) is lower.")
    if (metric == "children_vaccinated") warn <- c(warn, "These are sampled children found vaccinated, not the number of children vaccinated: use the administrative data for that.")
    if (any(toupper(as.character(d$country)) == "NIGERIA") && metric %in% c("pass_rate", "lots")) {
      n12 <- sum(toupper(as.character(d$country)) == "NIGERIA" & num(d$total_sampled) < 30, na.rm = TRUE)
      if (n12 > 0) warn <- c(warn, paste0(n12, " Nigerian lot(s) have only 1 or 2 clusters (under 30 children) and all fail."))
    }
  } else {
    nabove <- { va <- num(d$TotalNbrVaccPolio); pa <- num(d$PopPolio); sum(!is.na(va) & !is.na(pa) & pa > 0 & va / pa > 1.5) }
    if (metric %in% c("coverage", "share_districts_95", "children_vaccinated") && nabove > 0) warn <- c(warn, paste0(nabove, " record(s) in this selection have coverage above 1.5 (not credible)."))
    if (metric %in% c("children_vaccinated", "target_population", "coverage", "share_districts_95")) warn <- c(warn, "Measles-rubella (MR) records are excluded from this polio figure.")
    if (metric == "children_vaccinated" && any(d$Vaccine_type %in% c("nOPV2 & bOPV", "nOPV2_bOPV_IPV"), na.rm = TRUE)) warn <- c(warn, "Co-administered rows count the larger of the two vaccine totals, not their sum.")
  }
  result$warnings <- unique(warn)
  result$catalogue_version <- CATALOG_TOOLS_VERSION
  result
}

# ============================================================
# SHAPEFILE PREP UTILS -- shared by app.R (runtime, session-independent
# global setup) and scripts/prebuild_shapefiles.R (offline, run as part
# of the same GPEI/AFRO_admin_data/LQAS refresh pipeline that already
# produces Scope.rds / AFRO_admin_data.rds / AFRO_LQAS_data_c.rds).
#
# WHY THIS FILE EXISTS: app.R used to define preprocess_shapefile(),
# compute_ambiguous_district_pairs(), normalize_key_text(), and
# build_district_match_key() inline, and ran all of them -- including
# rmapshaper::ms_simplify() on the full-resolution africa_districts.rds
# (70MB, ~9,453 polygons) plus the country-name-disambiguation scan --
# EVERY time the Shiny process cold-starts. That work depends only on
# the shapefiles themselves (which almost never change), never on the
# survey data (dat/scope/admin_data) that DOES get refreshed on a
# schedule, so it was being redone from scratch on every cold start for
# no reason -- exactly the same shape of problem the GPEI scope-fetch
# pipeline had before it was moved offline.
#
# By pulling these functions into their own file, sourced by BOTH
# app.R and prebuild_shapefiles.R, a single offline run of
# prebuild_shapefiles.R (wired into
# run_prepare_the_AFRO_SIA_Dashboard_input.py alongside the .rds
# conversion step) can do the simplification and disambiguation-pair
# computation ONCE and save the results as
# africa_<layer>_simplified.rds / ambiguous_district_pairs.rds. app.R
# then just readRDS()s those pre-built files at startup instead of
# recomputing them -- see load_shapefiles_safely() in app.R, which
# tries the "_simplified" filename first and only falls back to doing
# the full live computation if that file isn't there yet. This also
# means the two copies of this logic can never silently drift apart,
# since there's only one copy.
#
# Never edit preprocess_shapefile()/compute_ambiguous_district_pairs()/
# normalize_key_text() separately in app.R and prebuild_shapefiles.R --
# they both source this one file.
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Text normalization for cross-source district/province name joins ----
normalize_key_text <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[‘’‚′]", "'", x)
  x <- gsub("[“”„″]", '"', x)
  # Treat hyphens the same as underscores -- province names are recorded
  # inconsistently across the shapefile and the survey data (e.g. data-side
  # "CUANZA-NORTE" vs shapefile "CUANZA NORTE"); collapsing both to a space
  # lets these match instead of silently failing the disambiguation join.
  x <- gsub("[_-]", " ", x)
  # Drop apostrophes entirely (not just curly->straight) -- they're recorded
  # inconsistently too (shapefile "N'DJAMENA" vs data "NDJAMENA").
  x <- gsub("'", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  # Strip Senegal's "RM " (Region Medicale) health-administrative prefix that
  # the survey data prepends to some province names (e.g. data-side
  # "RM SAINT LOUIS" vs shapefile "SAINT LOUIS", "RM MATAM" vs "MATAM").
  # Confirmed as a systematic naming convention across multiple Senegal
  # regions -- not a per-district guess.
  x <- sub("^(?i)RM\\s+", "", x, perl = TRUE)
  toupper(x)
}

# ---- Which (country, district) pairs recur in more than one province ----
# Depends ONLY on the district shapefile's own ADM0/ADM1/ADM2 names -- never
# on survey data -- so it's safe to compute once offline and cache.
compute_ambiguous_district_pairs <- function(district_shapefile) {
  if (is.null(district_shapefile) ||
      !all(c("ADM0_NAME", "ADM2_NAME") %in% names(district_shapefile))) {
    return(character(0))
  }
  df <- sf::st_drop_geometry(district_shapefile)
  df$.country  <- normalize_key_text(df$ADM0_NAME)
  df$.district <- normalize_key_text(df$ADM2_NAME)
  df$.province <- if ("ADM1_NAME" %in% names(df)) normalize_key_text(df$ADM1_NAME) else NA_character_
  df$.pair     <- paste(df$.country, df$.district, sep = "||")

  counts <- df |>
    dplyr::distinct(.pair, .province) |>
    dplyr::count(.pair, name = "n_provinces")

  counts$.pair[counts$n_provinces > 1]
}

# Same key-building logic applied to BOTH sides of a join (shapefile and
# survey data) so they only require a province match on the names that are
# actually ambiguous within their country.
build_district_match_key <- function(country, district, province, ambiguous_pairs) {
  country_n  <- normalize_key_text(country)
  district_n <- normalize_key_text(district)
  province_n <- normalize_key_text(province)
  pair <- paste(country_n, district_n, sep = "||")
  ifelse(pair %in% ambiguous_pairs,
         paste(pair, province_n, sep = "||"),
         pair)
}

# ---- Column pruning + topology-aware geometry simplification ----
# already_simplified = TRUE skips the rmapshaper::ms_simplify() step (and
# its before/after validity scan) entirely -- use this when `data` was
# already produced by this same function during an offline
# prebuild_shapefiles.R run, so a live app cold start never re-simplifies
# an already-simplified layer. The column-select is still applied either
# way; it's cheap (no geometry work) and guards against a shapefile that
# was regenerated upstream with extra columns.
preprocess_shapefile <- function(data, layer_name = NULL, already_simplified = FALSE) {
  if (is.null(data)) return(NULL)
  label <- layer_name %||% "shapefile"
  if (!inherits(data, "sf")) return(data)

  # Keep WORLD_CONTINENTS and WHO_REGION (used to derive the Africa/AFRO
  # background layers in the District Performance and LQAS Summary Map
  # views) alongside the admin-name columns actually used for joins.
  data <- data %>% dplyr::select(dplyr::any_of(c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "WORLD_CONTINENTS", "WHO_REGION", "geometry")))

  if (already_simplified) {
    cat("   ℹ️", label, "loaded pre-simplified (prebuilt offline) -- skipping runtime ms_simplify()\n")
    return(data)
  }

  # ---- Safe geometry simplification ----
  # An earlier attempt used sf::st_simplify() polygon-by-polygon. That does
  # NOT preserve shared borders between neighboring polygons in the same
  # layer (preserveTopology only guarantees a single polygon stays valid,
  # not that it still lines up with its neighbors), so it collapsed small
  # districts and created visible seams. It was reverted.
  #
  # rmapshaper::ms_simplify() is topology-aware across the WHOLE layer: it
  # builds one shared arc network for every polygon in `data` before
  # simplifying, so adjacent polygons keep matching edges, and
  # keep_shapes = TRUE guarantees no polygon is ever fully dropped. This is
  # still validated below and falls back to the untouched full-resolution
  # layer on any doubt -- correctness (no missing/altered districts) always
  # wins over speed.
  #
  # The validation compares against this layer's OWN baseline rather than
  # requiring perfection: these real-world admin shapefiles already ship
  # with a handful of pre-existing invalid/empty geometries (confirmed on
  # the district layer: 34 invalid, 1 empty, out of 9453 -- before any
  # simplification). Requiring zero would reject simplification every time
  # for a reason that has nothing to do with simplifying. The real bar is
  # "did not make it worse": same feature count, and invalid/empty counts
  # no higher than they already were.
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    n_before <- nrow(data)
    n_invalid_before <- suppressWarnings(sum(!sf::st_is_valid(data), na.rm = TRUE))
    n_empty_before <- sum(sf::st_is_empty(data))

    simplified <- tryCatch(
      rmapshaper::ms_simplify(data, keep = 0.15, keep_shapes = TRUE, sys = FALSE),
      error = function(e) {
        cat("   ⚠️ Simplification of", label, "failed (", e$message, ") -- keeping full resolution\n")
        NULL
      }
    )

    if (!is.null(simplified)) {
      n_invalid_after <- suppressWarnings(sum(!sf::st_is_valid(simplified), na.rm = TRUE))
      n_empty_after <- sum(sf::st_is_empty(simplified))

      if (nrow(simplified) == n_before &&
          n_invalid_after <= n_invalid_before &&
          n_empty_after <= n_empty_before) {
        cat("   ✅ Simplified", label, "geometry (", n_before, "features; invalid", n_invalid_before, "->", n_invalid_after,
            ", empty", n_empty_before, "->", n_empty_after, ")\n")
        data <- simplified
      } else {
        cat("   ⚠️ Simplification of", label, "made geometry worse (features", n_before, "->", nrow(simplified),
            ", invalid", n_invalid_before, "->", n_invalid_after,
            ", empty", n_empty_before, "->", n_empty_after, ") -- keeping full resolution\n")
      }
    }
  } else {
    cat("   ℹ️", label, "not simplified -- install.packages(\"rmapshaper\") to speed up map rendering\n")
  }

  data
}

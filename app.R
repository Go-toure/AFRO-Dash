

# ============================================================
# WHO AFRO SIA Dashboard
# Author: Gorgui Ba TOURE
# ============================================================
options(shiny.launch.browser = TRUE)
# ---- Version and metadata ----
DASHBOARD_VERSION <- "v1.0"
DASHBOARD_DATE <- format(Sys.Date(), "%B %Y")

# ---- Explicit package loading (required for deployment) ----
library(future)
library(promises)  
library(shinycssloaders)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(htmltools)
library(DT)
library(qrencoder)
# ---- Data manipulation ----
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(forcats)
library(tibble)
library(purrr)
library(lubridate)

# ---- Visualization ----
library(ggplot2)
library(scales)
library(patchwork)
library(ggspatial)
library(ggforce)
library(osmdata)
library(cowplot)
library(ggplotify)

# ---- Data handling & outputs ----
library(flextable)
library(openxlsx)
library(writexl)
library(officer)
library(tools)
library(treemapify)
library(ggradar)
library(networkD3)

# ---- Spatial ----
library(sf)

# ---- Utilities ----
library(grid)
library(conflicted)

plan(multisession) # Enables parallel background processing

scope_analysis <- reactiveVal(NULL)

# Keep deployment light
if (interactive()) {
  future::plan(future::multisession)  # parallel locally
} else {
  # On shinyapps.io, use sequential plan to avoid worker crashes/disconnects
  future::plan(future::sequential)
}


# ---- Deployment Detection ----
is_deployment <- Sys.getenv("SHINY_PORT") != "" || !interactive()
is_local_dev <- interactive() && (Sys.getenv("RSTUDIO") == "1" || Sys.getenv("POSITRON") == "1")

cat("🎯 Environment:", if(is_deployment) "DEPLOYMENT" else "LOCAL DEVELOPMENT", "\n")

# ---- Core options ----
options(
  shiny.maxRequestSize = 50 * 1024^2,
  sf_use_s2 = FALSE,
  stringsAsFactors = FALSE,
  dplyr.summarise.inform = FALSE,
  shiny.fullstacktrace = is_local_dev,  # Full traces only in local dev
  rsconnect.force.quarto = FALSE  # Added as requested
)

# ---- Environment configuration ----
if (is_local_dev) {
  options(shiny.autoreload = FALSE)  # Prevent reload loops in local dev
  cat("🔧 Local development mode activated\n")
} else {
  options(
    shiny.port = as.numeric(Sys.getenv("PORT", 8080)),
    shiny.host = "0.0.0.0",
    shiny.autoreload = FALSE
  )
  cat("🚀 Deployment mode activated\n")
}

# ============================================================
# ---- Auto-generate QR Code (Dynamic & Deployment-Safe) ----
# ============================================================

# Load lightweight QR package
if (!requireNamespace("qrencoder", quietly = TRUE)) {
  install.packages("qrencoder", dependencies = TRUE)
}
library(qrencoder)

# Define dashboard URL
dashboard_url <- "https://afropepdim.shinyapps.io/AFRO-SIA_Dashboard/"

# Detect app directory dynamically (works locally & in deployment)
app_dir <- getwd()
www_folder <- file.path(app_dir, "www")
qr_file <- file.path(www_folder, "AFRO_SIA_Dashboard_QR.png")

# Ensure www folder exists
if (!dir.exists(www_folder)) {
  dir.create(www_folder, recursive = TRUE)
  cat("📁 Created 'www' folder for static assets\n")
}

# Generate QR code only if missing or outdated (>1 day)
needs_update <- !file.exists(qr_file) ||
  difftime(Sys.time(), file.info(qr_file)$mtime, units = "days") > 1

if (needs_update) {
  cat("🔄 Generating fresh QR code for:", dashboard_url, "\n")
  qr_matrix <- qrencoder::qrencode(dashboard_url)
  
  png(qr_file, width = 500, height = 500, bg = "white")
  par(mar = c(0, 0, 0, 0))
  image(t(apply(qr_matrix, 2, rev)), col = c("white", "black"), axes = FALSE, asp = 1)
  dev.off()
  
  cat("✅ QR code saved at:", qr_file, "\n")
} else {
  cat("ℹ️ QR code already up-to-date at:", qr_file, "\n")
}


# ============================================================
# ---- Robust Package Loading ----
# ============================================================

required_packages <- c(
  # Shiny core
  "shiny", "shinydashboard", "htmltools", "DT",
  
  # Data manipulation
  "dplyr", "readr", "stringr", "tidyr", "forcats", "tibble", "purrr", "lubridate",
  
  # Visualization
  "ggplot2", "scales", "patchwork", "ggspatial", "ggplotify", "cowplot", "ggforce",
  
  # Data handling & outputs
  "flextable", "openxlsx", "writexl", "officer",
  
  # Spatial
  "sf", "osmdata",
  
  # Utilities
  "grid", "conflicted", "tools", "shinyjs", "qrencoder"
)


# Function to safely load packages with installation fallback
safe_package_load <- function(pkg_list) {
  cat("📦 Loading packages...\n")
  
  for (pkg in pkg_list) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("🔧 Installing missing package:", pkg, "\n")
      tryCatch({
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      }, error = function(e) {
        warning("Failed to install ", pkg, ": ", e$message)
      })
    }
    
    # Try to load the package
    success <- tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
      cat("✅", pkg, "\n")
      TRUE
    }, error = function(e) {
      cat("❌", pkg, "-", e$message, "\n")
      FALSE
    })
    
    # Critical packages check
    if (!success && pkg %in% c("shiny", "dplyr", "ggplot2", "sf")) {
      stop("❌ Critical package '", pkg, "' failed to load. App cannot continue.")
    }
  }
  cat("✅ All packages loaded successfully\n")
}

# Load packages
safe_package_load(required_packages)

# ============================================================
# ---- Load external map function ----
# ============================================================

# Load external map function
# 
# Replace this line:
# source("scope_analysis.R", local = TRUE)

# With this:
source("district_performance_optimized.R")
source("scope_analysis_optimized.R")
source("lqas_summary_map_optimized.R")
source("unresolved_cases_optimized.R")
source("plot_map.R")
# source("R/sia_ai_mwanza_pro_v2.R")

# preprocess_shapefile() / normalize_key_text() / compute_ambiguous_district_pairs()
# / build_district_match_key() now live in shapefile_prep_utils.R, shared with
# scripts/prebuild_shapefiles.R in the GPEI scope pipeline (REPOSITORIES\input\scope)
# so the offline prebuild step and this app can never silently drift apart. See that
# file for why this split exists -- short version: none of that logic depends on the
# survey data below, only on the shapefiles themselves, so it's safe (and much
# faster) to do it once offline instead of on every Shiny cold start.
source("shapefile_prep_utils.R")

cat("✅ External map functions loaded from optimized files\n")
# ============================================================
# ---- List numeric .rds files ----
# ============================================================


# Define input folder
input_folder <- "input/"

# List all .rds files in the folder
all_files <- list.files(
  path = input_folder,
  pattern = "\\.rds$",
  full.names = FALSE
)

# Filter to keep only files with numeric-only names (e.g., "272.rds")
file_list <- all_files[grepl("^\\d+\\.rds$", all_files)]

# Remove file extension for later use
file_list_no_ext <- tools::file_path_sans_ext(file_list)

cat("📁 Found", length(file_list), "numeric .rds files in input folder:\n")
if (length(file_list) > 0) {
  cat("   -", paste(file_list, collapse = "\n   - "), "\n")
} else {
  cat("   ⚠️ No numeric .rds files found\n")
}

# ============================================================
# ---- Environment Diagnostics ----
# ============================================================
cat("\n", strrep("=", 70), "\n")
cat("🔍 ENVIRONMENT DIAGNOSTICS\n")
cat(strrep("=", 70), "\n")
cat("Current working directory:", getwd(), "\n")
cat("Running in RStudio:", ifelse(Sys.getenv("RSTUDIO") == "1", "YES", "NO"), "\n")
cat("Deployment mode:", ifelse(exists("is_deployment") && is_deployment, "DEPLOYMENT", "LOCAL"), "\n")

# List contents of key directories
check_dir <- function(dir_name) {
  if (dir.exists(dir_name)) {
    files <- list.files(dir_name, pattern = "\\.rds$")
    cat("📁", dir_name, "folder contains", length(files), "RDS files\n")
    if (length(files) > 0) {
      cat("   ", paste(head(files, 5), collapse = ", "), 
          if(length(files) > 5) paste("... and", length(files)-5, "more"), "\n")
    }
  } else {
    cat("📁", dir_name, "folder not found\n")
  }
}

check_dir("input")
check_dir("data")
check_dir("www")
check_dir("app/input")
cat(strrep("=", 70), "\n\n")

# ============================================================
# ---- Conflict resolution ----
# ============================================================

# Ensure the conflicted package is active
if ("conflicted" %in% loadedNamespaces()) {
  conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflict_prefer("lag", "dplyr", quiet = TRUE)
  conflict_prefer("select", "dplyr", quiet = TRUE)
  conflict_prefer("rename", "dplyr", quiet = TRUE)
  conflict_prefer("compose", "purrr", quiet = TRUE)
  conflict_prefer("discard", "purrr", quiet = TRUE)
  conflict_prefer("col_factor", "readr", quiet = TRUE)
  conflict_prefer("box", "shinydashboard", quiet = TRUE)
  conflict_prefer("intersect", "base", quiet = TRUE)
  conflict_prefer("year", "lubridate", quiet = TRUE)  # Add this line
  conflict_prefer("month", "lubridate", quiet = TRUE) # Also good to add month if you use it
  conflict_prefer("day", "lubridate", quiet = TRUE)   # Also good to add day if you use it
  cat("✅ Conflict resolution applied\n")
} else {
  cat("⚠️ conflicted package not available for conflict resolution\n")
}

# ---- Font fallback ----
theme_set(theme_minimal(base_family = "sans"))

# ============================================================
# ---- DEPLOYMENT-READY Data Loading ----
# ============================================================

ESSENTIAL_COLS <- c("country", "province", "district", "response", "vaccine.type", "roundNumber", 
                    "numbercluster", "round_start_date", "lqas_start_date", "lqas_end_date", 
                    "year", "male_sampled", "female_sampled", "total_sampled", "male_vaccinated", 
                    "female_vaccinated", "total_vaccinated", "total_missed", "status", 
                    "performance", "r_non_compliance", "r_house_not_visited", "r_childabsent", 
                    "r_child_was_asleep", "r_child_is_a_visitor", "r_vaccinated_but_not_FM", 
                    "r_childnotborn", "r_security", "other_r", "prct_care_giver_informed_SIA", 
                    "prct_r_non_compliance", "prct_r_house_not_visited", "prct_r_childabsent", 
                    "prct_r_child_was_asleep", "prct_r_child_is_a_visitor", "prct_r_vaccinated_but_not_FM", 
                    "prct_r_security", "prct_r_childnotborn", "prct_other_r")

# Enhanced file discovery for deployment (no absolute paths)
find_data_file <- function(filename) {
  # Get the current working directory
  current_dir <- getwd()
  
  # Define base directories to check (relative paths only)
  base_dirs <- c(
    "input",
    "data",
    "www",
    "app/input",
    ".",
    "..",
    "../input",
    "../../input"
  )
  
  # Build all possible relative paths
  possible_paths <- unique(c(
    # Direct paths
    filename,
    file.path("input", filename),
    file.path("data", filename),
    file.path("www", filename),
    file.path("app", "input", filename),
    file.path("..", "input", filename),
    file.path("..", "data", filename),
    file.path("..", "www", filename),
    file.path("..", "app", "input", filename),
    file.path("..", "..", "input", filename),
    file.path("..", "..", "data", filename),
    file.path("..", "..", "www", filename),
    
    # With current_dir for clarity (still relative, just expanded)
    file.path(current_dir, "input", filename),
    file.path(current_dir, "data", filename),
    file.path(current_dir, "www", filename),
    file.path(current_dir, "app", "input", filename),
    file.path(dirname(current_dir), "input", filename),
    file.path(dirname(current_dir), "data", filename)
  ))
  
  # Filter to keep only relative paths (no drive letters)
  possible_paths <- possible_paths[!grepl("^[A-Za-z]:", possible_paths)]
  
  for (path in possible_paths) {
    if (file.exists(path)) {
      cat("📁 Found", filename, "at:", path, "\n")
      return(path)
    }
  }
  
  # If not found with relative paths, try to find any matching file recursively
  cat("⚠️", filename, "not found in standard locations. Searching recursively...\n")
  
  # Look for the file in the current directory and subdirectories (max depth 3)
  search_result <- tryCatch({
    list.files(pattern = paste0("^", filename, "$"), 
               recursive = TRUE, 
               full.names = TRUE,
               include.dirs = FALSE)
  }, error = function(e) character(0))
  
  if (length(search_result) > 0) {
    cat("📁 Found", filename, "at:", search_result[1], "\n")
    return(search_result[1])
  }
  
  cat("❌", filename, "not found. Checked locations:\n")
  cat("   -", paste(head(possible_paths, 10), collapse = "\n   - "), "\n")
  if (length(possible_paths) > 10) {
    cat("   ... and", length(possible_paths) - 10, "more locations\n")
  }
  
  return(NULL)
}

# Robust data loading with memory management
load_data_simple <- function(filename, sample_size = NULL, required = FALSE) {
  file_path <- find_data_file(filename)
  
  if (is.null(file_path)) {
    if (required) {
      stop("❌ REQUIRED file not found: ", filename)
    }
    warning("⚠️ Optional file not found: ", filename)
    return(NULL)
  }
  
  cat("🔄 Loading", filename, "from:", file_path, "\n")
  
  data <- tryCatch({
    result <- readRDS(file_path)
    
    # Memory optimization for deployment
    if (is_deployment && !is.null(sample_size) && nrow(result) > sample_size) {
      cat("📊 Sampling data for deployment:", nrow(result), "->", sample_size, "rows\n")
      set.seed(123)
      result <- dplyr::sample_n(result, sample_size)
    } else if (is_local_dev && !is.null(sample_size) && nrow(result) > sample_size) {
      cat("🔬 Local dev sampling:", nrow(result), "->", sample_size, "rows\n")
      set.seed(123)
      result <- dplyr::sample_n(result, sample_size)
    }
    
    cat("✅ Loaded", filename, "-", nrow(result), "rows,", ncol(result), "cols\n")
    result
  }, error = function(e) {
    if (required) {
      stop("❌ Failed to load REQUIRED file ", filename, ": ", e$message)
    }
    warning("⚠️ Failed to load ", filename, ": ", e$message)
    return(NULL)
  })
  
  return(data)
}

# Data optimization
optimize_data_types <- function(data) {
  if (is.null(data)) return(NULL)
  
  # Convert date columns - but only if they're not already Date
  date_cols <- names(data)[grepl("date|Date", names(data))]
  for (col in date_cols) {
    if (col %in% names(data) && !lubridate::is.Date(data[[col]])) {
      tryCatch({
        data[[col]] <- as.Date(data[[col]])
      }, error = function(e) {
        cat("   Could not convert", col, "to Date:", e$message, "\n")
      })
    }
  }
  
  # Convert character columns to factor for memory efficiency in deployment
  if (is_deployment) {
    # Exclude geographic identifier columns from factor conversion. These
    # feed dropdown "choices" built via patterns like c("All" = "", values)
    # across many tabs (Scope, District Performance, LQAS, Reasons,
    # Unresolved, Missed Children, Coverage, Admin Overview, ...) -- base
    # R's c() silently converts a factor to its underlying integer level
    # codes when combined with a plain character vector that way, which
    # showed up as country pickers listing "1, 2, 3, ..." instead of names
    # once deployed (this branch only runs when is_deployment is TRUE, so
    # it was never seen in local testing). Keep these as plain character;
    # still factor-ize everything else eligible for the memory savings.
    exclude_from_factor <- "country|province|district"
    char_cols <- names(data)[sapply(data, is.character)]
    char_cols <- char_cols[!grepl(exclude_from_factor, char_cols, ignore.case = TRUE)]
    for (col in char_cols) {
      if (col %in% names(data) && n_distinct(data[[col]]) < 1000) {
        data[[col]] <- as.factor(data[[col]])
      }
    }
  }
  
  data
}

# ============================================================
# 🧩 Helper: Standardize Admin Data (works for both sources)
# ============================================================
standardize_admin_data <- function(admin_data) {
  if (is.null(admin_data) || nrow(admin_data) == 0) return(admin_data)
  
  cat("🔄 Standardizing admin data...\n")
  cat("   Input columns:", paste(names(admin_data), collapse = ", "), "\n")
  cat("   Input rows:", nrow(admin_data), "\n")
  
  nm <- names(admin_data)
  
  rename_map <- c(
    "Total_Nbr_0doseVaccPolio_0.59m" = "Total_Nbr_0doseVaccPolio_0-59m",
    "Total_Nbr_1dose_Plus_vaccPolio_0.59m" = "Total_Nbr_1dose_Plus_vaccPolio_0-59m",
    "Doses.UsedPolio" = "Doses UsedPolio"
  )
  
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% nm && !(new %in% nm)) {
      admin_data <- dplyr::rename(admin_data, !!new := !!rlang::sym(old))
    }
  }
  
  admin_data <- admin_data |>
    dplyr::mutate(
      Country  = if ("Country"  %in% names(admin_data)) trimws(as.character(Country))  else NA_character_,
      Province = if ("Province" %in% names(admin_data)) trimws(as.character(Province)) else NA_character_,
      District = if ("District" %in% names(admin_data)) trimws(as.character(District)) else NA_character_
    )
  
  if ("SIA_date" %in% names(admin_data)) {
    # Check the class of SIA_date
    date_class <- class(admin_data$SIA_date)[1]
    cat("   SIA_date class:", date_class, "\n")
    
    # Show sample of original dates (safely)
    tryCatch({
      if (lubridate::is.Date(admin_data$SIA_date)) {
        sample_orig <- head(format(admin_data$SIA_date[!is.na(admin_data$SIA_date)], "%Y-%m-%d"), 5)
      } else if (lubridate::is.POSIXt(admin_data$SIA_date)) {
        sample_orig <- head(format(admin_data$SIA_date[!is.na(admin_data$SIA_date)], "%Y-%m-%d"), 5)
      } else {
        sample_orig <- head(as.character(admin_data$SIA_date[!is.na(admin_data$SIA_date) & admin_data$SIA_date != ""]), 5)
      }
      cat("   Sample original SIA_dates:", paste(sample_orig, collapse = ", "), "\n")
    }, error = function(e) {
      cat("   Could not display sample dates:", e$message, "\n")
    })
    
    # Only parse if not already Date class
    if (!lubridate::is.Date(admin_data$SIA_date)) {
      cat("   Converting SIA_date to Date class...\n")
      
      admin_data <- admin_data |>
        dplyr::mutate(
          SIA_date = dplyr::case_when(
            # Handle numeric Excel dates
            is.numeric(SIA_date) ~ as.Date(SIA_date, origin = "1899-12-30"),
            
            # Handle POSIXct/POSIXlt with time component
            lubridate::is.POSIXt(SIA_date) ~ as.Date(SIA_date),
            
            # Handle character strings
            TRUE ~ {
              x <- as.character(SIA_date)
              
              # First try standard formats including those with time
              parsed <- suppressWarnings(lubridate::parse_date_time(
                x,
                orders = c(
                  "ymd HMS", "ymd HM", "ymd",  # 2020-09-18 00:00:00
                  "dmy HMS", "dmy HM", "dmy",  # 18-09-2020 00:00:00
                  "mdy HMS", "mdy HM", "mdy",  # 09-18-2020 00:00:00
                  "Ymd HMS", "Ymd HM", "Ymd",  # 20200918 000000
                  "dmY HMS", "dmY HM", "dmY",  # 18092020 000000
                  "mdY HMS", "mdY HM", "mdY",  # 09182020 000000
                  "Y-m-d H:M:S", "Y/m/d H:M:S",  # 2020-09-18 00:00:00
                  "d/m/Y H:M:S", "m/d/Y H:M:S"   # 18/09/2020 00:00:00
                )
              ))
              
              # If standard parsing fails, try French month abbreviations
              if (all(is.na(parsed))) {
                # Create comprehensive mapping for French month abbreviations
                french_months <- c(
                  "janv" = "01", "jan" = "01", "janvier" = "01",
                  "févr" = "02", "fév" = "02", "février" = "02",
                  "mars" = "03", "mar" = "03",
                  "avr" = "04", "avril" = "04",
                  "mai" = "05",
                  "juin" = "06",
                  "juil" = "07", "juillet" = "07",
                  "août" = "08", "aout" = "08",
                  "sept" = "09", "septembre" = "09",
                  "oct" = "10", "octobre" = "10",
                  "nov" = "11", "novembre" = "11",
                  "déc" = "12", "decembre" = "12", "décembre" = "12"
                )
                
                # Try multiple French date formats
                parsed <- sapply(x, function(date_str) {
                  # Remove any time component if present
                  date_str <- trimws(gsub("\\s+.*$", "", date_str))
                  
                  # Try different patterns
                  patterns <- list(
                    # dd-mmm-yy (18-sept-20)
                    list(
                      regex = "^[0-9]{1,2}[- ][a-z]{3,9}[- ][0-9]{2}$",
                      split = "[- ]",
                      year_digits = 2
                    ),
                    # dd-mmm-yyyy (18-sept-2020)
                    list(
                      regex = "^[0-9]{1,2}[- ][a-z]{3,9}[- ][0-9]{4}$",
                      split = "[- ]",
                      year_digits = 4
                    ),
                    # mmm-dd-yy (sept-18-20)
                    list(
                      regex = "^[a-z]{3,9}[- ][0-9]{1,2}[- ][0-9]{2}$",
                      split = "[- ]",
                      year_digits = 2,
                      month_first = TRUE
                    ),
                    # mmm-dd-yyyy (sept-18-2020)
                    list(
                      regex = "^[a-z]{3,9}[- ][0-9]{1,2}[- ][0-9]{4}$",
                      split = "[- ]",
                      year_digits = 4,
                      month_first = TRUE
                    )
                  )
                  
                  for (pattern in patterns) {
                    if (grepl(pattern$regex, date_str, ignore.case = TRUE)) {
                      parts <- strsplit(date_str, pattern$split)[[1]]
                      parts <- trimws(parts)
                      
                      if (length(parts) == 3) {
                        if (!is.null(pattern$month_first) && pattern$month_first) {
                          # Format: month day year
                          month_abbr <- tolower(parts[1])
                          day <- parts[2]
                          year <- parts[3]
                        } else {
                          # Format: day month year
                          day <- parts[1]
                          month_abbr <- tolower(parts[2])
                          year <- parts[3]
                        }
                        
                        # Convert 2-digit year to 4-digit year
                        year_num <- as.numeric(year)
                        if (pattern$year_digits == 2) {
                          year_full <- ifelse(year_num >= 70, 1900 + year_num, 2000 + year_num)
                        } else {
                          year_full <- year_num
                        }
                        
                        # Look up month number
                        month_num <- french_months[month_abbr]
                        
                        if (!is.null(month_num) && !is.na(month_num) && 
                            !is.na(day) && !is.na(year_full)) {
                          # Create date string in YYYY-MM-DD format
                          date_str_fixed <- sprintf("%04d-%02s-%02s", 
                                                    year_full, 
                                                    month_num, 
                                                    sprintf("%02d", as.numeric(day)))
                          return(as.Date(date_str_fixed))
                        }
                      }
                    }
                  }
                  return(NA)
                })
                
                # Convert the list back to Date vector
                parsed <- do.call(c, parsed)
              }
              
              # Convert to Date (extract date part if datetime)
              as.Date(parsed)
            }
          )
        )
      
      # Show sample of parsed dates
      sample_parsed <- head(admin_data$SIA_date[!is.na(admin_data$SIA_date)], 5)
      cat("   Sample parsed SIA_dates:", paste(as.character(sample_parsed), collapse = ", "), "\n")
      
    } else {
      cat("   SIA_date already in Date class, no conversion needed\n")
    }
    
    # Count parsed vs unparsed
    n_parsed <- sum(!is.na(admin_data$SIA_date))
    n_total <- nrow(admin_data)
    cat("   Valid dates:", n_parsed, "of", n_total, "(", round(100 * n_parsed/n_total, 1), "%)\n")
    
    # Show date range if we have parsed dates
    if (n_parsed > 0) {
      date_range <- range(admin_data$SIA_date, na.rm = TRUE)
      cat("   Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
    }
    
  } else {
    cat("   ⚠️ No SIA_date column found in admin data\n")
  }
  
  cat("✅ Admin data standardization complete\n")
  admin_data
}

# ---- Preprocessing helpers ----
preprocess_main_data <- function(data) {
  if (is.null(data)) return(NULL)
  
  data %>%
    mutate(
      round_start_date = as.Date(round_start_date),
      country = toupper(trimws(country)),
      district = toupper(trimws(district)),
      vaccine.type = if_else(is.na(vaccine.type), "nOPV2", as.character(vaccine.type))
    ) %>%
    filter(!is.na(round_start_date)) %>%
    # ESSENTIAL_COLS only lists the traditional r_*/prct_r_* reason columns by
    # exact name. The Reasons Analysis "Combined Overview" tab also needs the
    # Absence (abs_reason_*) and Non-Compliance (nc_reason_*) columns, whose
    # exact suffixes vary by dataset -- select(any_of(ESSENTIAL_COLS)) alone
    # was silently dropping ALL of them here, so absence_result/nc_result were
    # always NULL downstream and only Traditional Reasons ever appeared in the
    # combined heatmap, no matter which combination radio button was picked.
    # Keep the essential whitelist AND any column matching those two prefixes.
    select(any_of(ESSENTIAL_COLS) | matches("^abs_reason_|^nc_reason_")) %>%
    optimize_data_types()
}

preprocess_scope_data <- function(data) {
  if (is.null(data)) return(NULL)

  result <- data %>%
    mutate(
      round_start_date = as.Date(round_start_date),
      floor_date = floor_date(round_start_date, "month"),
      year = year(round_start_date),
      yearmonth = floor_date,
      country = toupper(trimws(country)),
      district = toupper(trimws(district)),
      vaccine.type = if_else(is.na(vaccine.type), "nOPV2", as.character(vaccine.type))
    )

  n_before <- nrow(result)
  result <- result %>% filter(!is.na(round_start_date))
  n_after <- nrow(result)
  if (n_before != n_after) {
    cat("ℹ️ Scope data:", n_before - n_after, "row(s) dropped for missing/unparseable round_start_date (kept",
        n_after, "of", n_before, ")\n")
  }

  result %>% optimize_data_types()
}

# preprocess_shapefile() now lives in shapefile_prep_utils.R (sourced above).

preprocess_admin_data <- function(data) {
  if (is.null(data)) return(NULL)
  
  cat("🔄 Preprocessing admin data...\n")
  cat("   Input columns:", paste(names(data), collapse = ", "), "\n")
  cat("   Input rows:", nrow(data), "\n")
  
  result <- data
  
  # Standardize country names (upper-case, trimmed)
  if ("country" %in% names(result)) {
    result <- result %>% mutate(country = toupper(trimws(country)))
  }
  if ("Country" %in% names(result)) {
    result <- result %>% mutate(Country = toupper(trimws(Country)))
  }
  
  # ✅ KEEP original column names for admin functions
  # Do NOT rename SIA_date → round_start_date
  # Do NOT rename Vaccine_type → vaccine.type
  
  # --- Call the comprehensive standardization function
  result <- standardize_admin_data(result)
  
  # --- Optional: ensure Vaccine_type column exists (fallback if missing)
  if (!"Vaccine_type" %in% names(result) && "vaccine.type" %in% names(result)) {
    result <- result %>% rename(Vaccine_type = vaccine.type)
    cat("   Renamed vaccine.type to Vaccine_type\n")
  }
  
  # --- Ensure we have the required columns for the admin functions
  required_cols <- c("Country", "SIA_date", "Vaccine_type", "CVPolio", "TotalNbrVaccPolio")
  missing_cols <- setdiff(required_cols, names(result))
  
  if (length(missing_cols) > 0) {
    cat("⚠️ Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    # Add missing columns as NA
    for (col in missing_cols) {
      result[[col]] <- NA
    }
  }
  
  # --- Convert CVPolio to numeric if it's character
  if ("CVPolio" %in% names(result) && is.character(result$CVPolio)) {
    result$CVPolio <- as.numeric(result$CVPolio)
  }
  
  # --- Keep remaining optimizations but be careful with date columns
  # Don't let optimize_data_types convert our already-converted dates
  result <- optimize_data_types(result)
  
  cat("✅ Admin data preprocessing complete\n")
  cat("   Output columns:", paste(names(result), collapse = ", "), "\n")
  cat("   Output rows:", nrow(result), "\n")
  
  # Return the result explicitly
  return(result)
} 

# ============================================================
# ---- DEPLOYMENT-READY Dataset Loading ----
# ============================================================

cat("🚀 Starting deployment-ready data loading...\n")

# Memory management for deployment
if (is_deployment) {
  cat("💾 Deployment memory optimization active\n")
  gc()  # Clean up memory before loading
}

# Determine sampling strategy
SAMPLE_DATA <- is_local_dev  # Only sample in local development
# NOTE: dat/scope/admin_data are intentionally never sampled, even in local dev.
# They drive the district-level maps (District Performance, SIA Scope Analysis,
# Unresolved Cases, ...), and dplyr::sample_n() taking a random row subset makes
# those maps show a sparse, scattered subset of districts and much lower
# high/moderate/poor counts than the true total -- it looks exactly like
# missing/broken data even though nothing is actually wrong. On disk these files
# are tiny (AFRO_LQAS_data_c.rds ~370KB, Scope.rds ~40KB, AFRO_admin_data.rds
# ~1.2MB) -- loading them in full, locally or deployed, is not a performance
# concern. (The real size in this app is the shapefiles -- africa_districts.rds
# alone is 70MB -- which is why the "quick wins" perf work focused there.)
sample_size_main <- NULL
sample_size_scope <- NULL
sample_size_admin <- NULL

cat("📊 Sampling strategy: INACTIVE (dat/scope/admin_data always loaded in full)\n")

# ---- Load Main Dataset (REQUIRED) ----
dat <- tryCatch({
  raw_data <- load_data_simple("AFRO_LQAS_data_c.rds", 
                               sample_size = sample_size_main, 
                               required = TRUE)
  if (!is.null(raw_data)) {
    preprocess_main_data(raw_data)
  } else {
    stop("Main dataset is NULL after loading")
  }
}, error = function(e) {
  stop("❌ CRITICAL: Main dataset failed to load: ", e$message, "\nApp cannot continue.")
})

# Validate main dataset
if (is.null(dat) || nrow(dat) == 0) {
  stop("❌ CRITICAL: Main dataset is empty or invalid after loading")
}
cat("✅ Main dataset ready:", nrow(dat), "rows\n")

# ---- Load Scope Data (OPTIONAL) ----
scope <- tryCatch({
  raw_scope <- load_data_simple("Scope.rds", 
                                sample_size = sample_size_scope, 
                                required = FALSE)
  if (!is.null(raw_scope)) {
    preprocess_scope_data(raw_scope)
  } else {
    cat("ℹ️ Scope data not available - some features will be disabled\n")
    NULL
  }
}, error = function(e) {
  cat("⚠️ Scope data loading failed: ", e$message, "\n")
  NULL
})

# ---- Load Admin Data (OPTIONAL with fallback) ----
# ============================================================
# ---- Admin Data Loading with Case-Insensitive File Search ----
# ============================================================

admin_data <- tryCatch({
  cat("\n", strrep("=", 70), "\n")
  cat("🔍 ADMIN DATA LOADING DIAGNOSTICS\n")
  cat(strrep("=", 70), "\n")
  
  # Step 1: Check if input folder exists and list all files
  cat("\n1️⃣ Checking input folder contents:\n")
  
  # Function to safely check directories (case-insensitive on file system)
  find_admin_file <- function() {
    # Define base directories to check
    directories <- c("input", "data", ".")
    
    for (dir in directories) {
      if (!dir.exists(dir)) next
      
      cat("\n   📁 Checking directory:", dir, "\n")
      
      # Get all files in directory
      all_files <- list.files(dir, full.names = TRUE)
      
      if (length(all_files) > 0) {
        cat("      Found", length(all_files), "files\n")
        
        # Look for admin RDS files (case-insensitive pattern matching)
        admin_patterns <- c("admin.*\\.rds$", "afro.*admin.*\\.rds$")
        
        for (pattern in admin_patterns) {
          # Use regex with ignore.case = TRUE for pattern matching
          matches <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
          
          if (length(matches) > 0) {
            # Found potential matches
            cat("      ✅ Found", length(matches), "potential admin files:\n")
            for (match in matches) {
              cat("         -", basename(match), "\n")
            }
            
            # Try each match until one works
            for (match_file in matches) {
              cat("\n      Attempting to read:", basename(match_file), "... ")
              
              tryCatch({
                data <- readRDS(match_file)
                cat("✅ SUCCESS\n")
                cat("         Rows:", nrow(data), "| Columns:", ncol(data), "\n")
                return(list(data = data, path = match_file))
              }, error = function(e) {
                cat("❌ Failed:", e$message, "\n")
                return(NULL)
              })
            }
          }
        }
        
        # If no RDS found, look for CSV files as fallback
        csv_patterns <- c("admin.*\\.csv$", "afro.*admin.*\\.csv$")
        
        for (pattern in csv_patterns) {
          matches <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
          
          if (length(matches) > 0) {
            cat("      ✅ Found", length(matches), "potential admin CSV files:\n")
            
            for (match_file in matches) {
              cat("\n      Attempting to read CSV:", basename(match_file), "... ")
              
              tryCatch({
                data <- read.csv(match_file, stringsAsFactors = FALSE)
                cat("✅ SUCCESS\n")
                cat("         Rows:", nrow(data), "| Columns:", ncol(data), "\n")
                return(list(data = data, path = match_file))
              }, error = function(e) {
                cat("❌ Failed:", e$message, "\n")
                return(NULL)
              })
            }
          }
        }
      }
    }
    
    # If we get here, no file was found
    return(NULL)
  }
  
  # Step 2: Search for admin file
  cat("\n2️⃣ Searching for admin data files...\n")
  result <- find_admin_file()
  
  if (!is.null(result)) {
    raw_admin <- result$data
    loaded_path <- result$path
    
    # Step 3: Examine the data structure
    cat("\n3️⃣ Raw admin data structure:\n")
    cat("   File loaded from:", loaded_path, "\n")
    cat("   Dimensions:", nrow(raw_admin), "rows x", ncol(raw_admin), "columns\n")
    cat("   Column names:", paste(names(raw_admin), collapse = ", "), "\n")
    
    # Convert to tibble if needed
    if (!inherits(raw_admin, "tbl_df")) {
      raw_admin <- as_tibble(raw_admin)
    }
    
    # Step 4: Check for required columns (case-insensitive)
    required_cols <- c("SIA_date", "Country", "Vaccine_type", "CVPolio", "TotalNbrVaccPolio")
    col_mapping <- list()
    
    cat("\n4️⃣ Checking for required columns (case-insensitive):\n")
    for (col in required_cols) {
      # Case-insensitive match
      matches <- names(raw_admin)[tolower(names(raw_admin)) == tolower(col)]
      
      if (length(matches) > 0) {
        actual_col <- matches[1]
        col_mapping[[col]] <- actual_col
        cat("   ✅", col, "found as:", actual_col, "\n")
        
        # Show sample values
        sample_vals <- head(raw_admin[[actual_col]][!is.na(raw_admin[[actual_col]])], 3)
        if (length(sample_vals) > 0) {
          cat("      Sample values:", paste(sample_vals, collapse = ", "), "\n")
        }
      } else {
        # Check for similar columns
        similar <- names(raw_admin)[grepl(tolower(col), tolower(names(raw_admin)))]
        if (length(similar) > 0) {
          cat("   ⚠️", col, "not found, but found similar:", paste(similar, collapse = ", "), "\n")
        } else {
          cat("   ❌", col, "not found\n")
        }
      }
    }
    
    # Rename columns if needed
    if (length(col_mapping) > 0) {
      raw_admin <- raw_admin %>% rename(!!!col_mapping)
    }
    
    # Step 5: Process the data
    cat("\n5️⃣ Processing admin data...\n")
    processed_admin <- preprocess_admin_data(raw_admin)
    
    # Step 6: Validate processed data
    cat("\n6️⃣ Processed admin data validation:\n")
    if (!is.null(processed_admin) && nrow(processed_admin) > 0) {
      cat("   ✅ Processing successful\n")
      cat("   Final dimensions:", nrow(processed_admin), "rows x", ncol(processed_admin), "columns\n")
      
      if ("SIA_date" %in% names(processed_admin)) {
        n_dates <- sum(!is.na(processed_admin$SIA_date))
        cat("   SIA_date:", n_dates, "non-NA dates\n")
      }
      
      processed_admin
    } else {
      cat("   ❌ Processing failed or returned empty data\n")
      cat("   Creating empty admin data structure as fallback\n")
      
      empty_admin <- tibble(
        Country = character(),
        SIA_date = as.Date(character()),
        Vaccine_type = character(),
        CVPolio = numeric(),
        TotalNbrVaccPolio = numeric(),
        Province = character(),
        District = character()
      )
      preprocess_admin_data(empty_admin)
    }
    
  } else {
    cat("\n❌ Could not load admin data from any location\n")
    cat("   Creating empty admin data structure as fallback\n")
    
    empty_admin <- tibble(
      Country = character(),
      SIA_date = as.Date(character()),
      Vaccine_type = character(),
      CVPolio = numeric(),
      TotalNbrVaccPolio = numeric(),
      Province = character(),
      District = character()
    )
    preprocess_admin_data(empty_admin)
  }
  
}, error = function(e) {
  cat("\n❌ Admin data loading failed with error:\n")
  cat("   ", e$message, "\n")
  cat("   Creating empty admin data structure\n")
  
  tibble(
    Country = character(),
    SIA_date = as.Date(character()),
    Vaccine_type = character(),
    CVPolio = numeric(),
    TotalNbrVaccPolio = numeric(),
    Province = character(),
    District = character()
  ) %>% preprocess_admin_data()
})

# Final status
cat("\n", strrep("=", 70), "\n")
cat("📊 FINAL ADMIN DATA STATUS:\n")
cat(strrep("=", 70), "\n")
if (!is.null(admin_data) && nrow(admin_data) > 0) {
  cat("✅ Admin data is AVAILABLE\n")
  cat("   Rows:", nrow(admin_data), "\n")
  cat("   Columns:", paste(names(admin_data), collapse = ", "), "\n")
  
  if ("SIA_date" %in% names(admin_data)) {
    n_valid_dates <- sum(!is.na(admin_data$SIA_date))
    cat("   Valid dates:", n_valid_dates, "out of", nrow(admin_data), "\n")
    if (n_valid_dates > 0) {
      date_range <- range(admin_data$SIA_date, na.rm = TRUE)
      cat("   Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
    }
  }
  
  if ("Country" %in% names(admin_data)) {
    n_countries <- length(unique(admin_data$Country[!is.na(admin_data$Country)]))
    cat("   Unique countries:", n_countries, "\n")
  }
} else {
  cat("❌ Admin data is NOT AVAILABLE\n")
  cat("   The app will still run but Admin Overview tab will show errors\n")
}
cat(strrep("=", 70), "\n\n")

# ---- Load Shapefiles (OPTIONAL but recommended) ----
# Tries the "_simplified" file first for each layer -- produced offline by
# scripts/prebuild_shapefiles.R (part of the GPEI scope pipeline) -- and
# only falls back to the raw shapefile (doing the full column-prune +
# rmapshaper::ms_simplify() live, same as before) if the prebuilt one
# isn't there yet. Either way the app works; the prebuilt path is just
# much faster to cold-start with. See shapefile_prep_utils.R.
load_shapefile_layer <- function(base_name, label) {
  simplified <- tryCatch(
    load_data_simple(paste0(base_name, "_simplified.rds"), required = FALSE),
    error = function(e) NULL
  )
  if (!is.null(simplified)) {
    return(preprocess_shapefile(simplified, label, already_simplified = TRUE))
  }

  raw <- tryCatch(
    load_data_simple(paste0(base_name, ".rds"), required = FALSE),
    error = function(e) NULL
  )
  if (!is.null(raw)) {
    cat("   ℹ️ No prebuilt", paste0(base_name, "_simplified.rds"), "found -- simplifying", label, "at startup instead ",
        "(run scripts\\prebuild_shapefiles.R to do this offline and speed up cold start)\n")
    return(preprocess_shapefile(raw, label, already_simplified = FALSE))
  }

  cat("⚠️", label, "shapes not available\n")
  NULL
}

load_shapefiles_safely <- function() {
  cat("🗺️ Loading shapefiles...\n")

  shapes <- list(
    countries = tryCatch(load_shapefile_layer("africa_countries", "countries"), error = function(e) {
      cat("⚠️ Country shapes failed: ", e$message, "\n")
      NULL
    }),

    provinces = tryCatch(load_shapefile_layer("africa_provinces", "provinces"), error = function(e) {
      cat("⚠️ Province shapes failed: ", e$message, "\n")
      NULL
    }),

    districts = tryCatch(load_shapefile_layer("africa_districts", "districts"), error = function(e) {
      cat("⚠️ District shapes failed: ", e$message, "\n")
      NULL
    })
  )

  # Check if we have at least one shapefile
  shape_count <- sum(sapply(shapes, function(x) !is.null(x)))
  if (shape_count == 0) {
    cat("❌ No shapefiles loaded - mapping features will be disabled\n")
  } else {
    cat("✅ Loaded", shape_count, "shapefile(s)\n")
  }

  shapes
}

shapefiles <- load_shapefiles_safely()
all_countries <- shapefiles$countries
all_provinces <- shapefiles$provinces
all_districts <- shapefiles$districts

# ============================================================
# ---- District name disambiguation (country + province + district) ----
# ============================================================
# The district shapefile has thousands of rows but a meaningfully smaller
# number of unique (country, district name) combinations -- the same district
# name recurs in more than one province within the same country for a real
# share of rows (confirmed directly against africa_districts.rds: ~29%). Every
# map-building function in this app (Scope Analysis, District Performance,
# LQAS Summary, Unresolved Cases) joins survey data onto district polygons
# using country+district name alone, which means a record for one district can
# also match the wrong, identically-named district in a different province.
# Requiring province everywhere isn't safe either -- it only helps where
# province spelling lines up cleanly between the shapefile and the survey
# data, and would regress the majority of districts whose name IS already
# unique within their country. So: build a single match key per row that only
# pulls in province for the names that actually need it to disambiguate, and
# leaves every other district's join untouched.
#
# normalize_key_text() / compute_ambiguous_district_pairs() /
# build_district_match_key() now live in shapefile_prep_utils.R (sourced
# above), shared with scripts/prebuild_shapefiles.R.
#
# AMBIGUOUS_DISTRICT_PAIRS itself depends only on the district shapefile, not
# on any survey data, so prebuild_shapefiles.R can (and by default does)
# compute it offline and save it as ambiguous_district_pairs.rds -- tried
# first here, with the live computation kept as a fallback for whenever that
# prebuilt file hasn't been generated yet.
ambiguous_pairs_path <- find_data_file("ambiguous_district_pairs.rds")
AMBIGUOUS_DISTRICT_PAIRS <- if (!is.null(ambiguous_pairs_path)) {
  prebuilt_pairs <- tryCatch(readRDS(ambiguous_pairs_path), error = function(e) NULL)
  if (!is.null(prebuilt_pairs)) {
    cat("ℹ️ Using prebuilt ambiguous_district_pairs.rds from:", ambiguous_pairs_path,
        "(computed offline by scripts\\prebuild_shapefiles.R)\n")
    prebuilt_pairs
  } else {
    cat("⚠️ ambiguous_district_pairs.rds found but failed to read -- computing at startup instead\n")
    compute_ambiguous_district_pairs(all_districts)
  }
} else {
  cat("ℹ️ No prebuilt ambiguous_district_pairs.rds found -- computing at startup instead ",
      "(run scripts\\prebuild_shapefiles.R to do this offline and speed up cold start)\n")
  compute_ambiguous_district_pairs(all_districts)
}
cat("🔑 District disambiguation:", length(AMBIGUOUS_DISTRICT_PAIRS),
    "district name(s) recur in more than one province within their country -- these require a province match to join correctly; all other districts are unaffected.\n")

# ============================================================
# ---- Deployment Data Summary ----
# ============================================================

print_data_summary <- function() {
  cat("\n", strrep("=", 70), "\n")
  cat("📊 DEPLOYMENT DATA LOADING SUMMARY\n")
  cat(strrep("=", 70), "\n")
  
  info <- function(label, data) {
    if (is.null(data) || nrow(data) == 0) {
      cat("❌ ", label, " NOT AVAILABLE\n")
    } else {
      size_mb <- format(utils::object.size(data), units = "MB")
      cat("✅ ", label, " ", nrow(data), "rows,", ncol(data), "cols (", size_mb, ")\n")
    }
  }
  
  info("Main dataset:", dat)
  info("Scope data:", scope)
  info("Admin data:", admin_data)

  # ---- Sentinel/placeholder value check ----
  # Legacy GIS/survey convention: an obviously-impossible number (-999, -9999,
  # -99, 9999, 999999) standing in for "no data" in a numeric field, since old
  # shapefile (DBF) attribute tables didn't handle true NULLs. Left unfiltered,
  # one of these in a field like CVPolio or TotPop would badly skew a choropleth
  # color scale or a mean/sum. This is diagnostic only -- it never filters
  # anything automatically -- so a hit here is a prompt to go look at that
  # column, not proof of a bug.
  check_sentinel_values <- function(label, data) {
    if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
    sentinels <- c(-999, -9999, -99, 9999, 999999)
    num_cols <- names(data)[sapply(data, is.numeric)]
    for (col in num_cols) {
      n_hit <- sum(data[[col]] %in% sentinels, na.rm = TRUE)
      if (n_hit > 0) {
        cat("⚠️ ", label, "column '", col, "' has", n_hit, "value(s) matching a common sentinel/placeholder pattern -- verify these aren't disguised missing data\n")
      }
    }
  }
  check_sentinel_values("Main dataset:", dat)
  check_sentinel_values("Admin data:", admin_data)

  cat("🗺️  Country shapes:", if(!is.null(all_countries)) "LOADED" else "NOT LOADED", "\n")
  cat("🗺️  Province shapes:", if(!is.null(all_provinces)) "LOADED" else "NOT LOADED", "\n")
  cat("🗺️  District shapes:", if(!is.null(all_districts)) "LOADED" else "NOT LOADED", "\n")
  
  cat("🌍 Environment:", if(is_deployment) "DEPLOYMENT" else "LOCAL DEVELOPMENT", "\n")
  cat("📊 Sampling: INACTIVE (dat/scope/admin_data always loaded in full)\n")
  
  # Memory usage
  mem_usage <- sum(sapply(list(dat, scope, admin_data, all_countries, all_provinces, all_districts), 
                          function(x) if(!is.null(x)) object.size(x) else 0))
  cat("💾 Total memory usage:", format(mem_usage, units = "MB"), "\n")
  
  cat(strrep("=", 70), "\n\n")
}

print_data_summary()

# ============================================================
# ---- Define AFRO Blocks ----
# ============================================================

afro_blocks <- list(
  "LCB" = c("NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC"),
  "WA" = c("ALGERIA", "BURKINA FASO", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", 
           "COTE D IVOIRE", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "GAMBIA", "SENEGAL"),
  "ESA" = c("BOTSWANA", "BURUNDI", "ERITREA", "ESWATINI", "ETHIOPIA", "KENYA", "LESOTHO", 
            "MADAGASCAR", "MALAWI", "MAURITIUS", "MOZAMBIQUE", "NAMIBIA", "RWANDA", "SEYCHELLES", 
            "SOUTH AFRICA", "SOUTH SUDAN", "UNITED REPUBLIC OF TANZANIA", "UGANDA", "ZAMBIA", "ZIMBABWE"),
  "DRC" = c("ANGOLA", "DEMOCRATIC REPUBLIC OF THE CONGO"),
  "ECA" = c("CONGO", "GABON", "EQUATORIAL GUINEA")
)


ist_blocks <- list(
  "CENTRAL" = c("ANGOLA", "BURUNDI", "CAMEROON", "CENTRAL AFRICAN REPUBLIC", "CONGO", 
                "DEMOCRATIC REPUBLIC OF THE CONGO", "EQUATORIAL GUINEA", "GABON", 
                "SAO TOME AND PRINCIPE", "TCHAD"),
  "ESA" = c("BOTSWANA", "COMOROES", "ERITREA", "ESWATINI", "ETHIOPIA", "KENYA", 
            "LESOTHO", "MADAGASCAR", "MALAWI", "MAURITIUS", "MOZAMBIQUE", "NAMIBIA", 
            "RWANDA", "SEYCHELLES", "SOUTH AFRICA", "SOUTH SUDAN", "TANZANIA", 
            "UGANDA", "ZAMBIA", "ZIMBABWE"),
  "WEST" = c("ALGERIA", "BENIN", "BURKINA FASO", "CABO VERDE", "COTE D IVOIRE", 
             "GAMBIA", "GHANA", "GUINEA", "GUINEA-BISSAU", "LIBERIA", "MALI", 
             "MAURITANIA", "NIGER", "NIGERIA", "SENEGAL", "SIERRA LEONE", "TOGO")
)

# ============================================================
# ---- Country Abbreviations ----
# ============================================================

country_abbrs_list <- list(
  c("NIGERIA"="NIE", "NIGER"="NIG", "CAMEROON"="CAE", "CHAD"="CHA", "CENTRAL AFRICAN REPUBLIC"="CAR"),
  c("ALGERIA"="ALG", "BURKINA FASO"="BFA", "MAURITANIA"="MAU", "MALI"="MAI", "GUINEA"="GUI", "GHANA"="GHA", "TOGO"="TOG", "BENIN"="BEN", "COTE D IVOIRE"="CIV", "SIERRA LEONE"="SIL", "LIBERIA"="LIB", "GUINEA-BISSAU"="GBU", "GAMBIA"="GAM", "SENEGAL"="SEN"),
  c("ANGOLA" = "ANG", "BOTSWANA" = "BWA", "BURUNDI" = "BUU", "ERITREA" = "ERI", "ESWATINI" = "SWZ", "ETHIOPIA" = "ETH", "KENYA" = "KEN", "LESOTHO" = "LES", "MADAGASCAR" = "MAD", "MALAWI" = "MAL", "MAURITIUS" = "MAS", "MOZAMBIQUE" = "MOZ", "NAMIBIA" = "NAM", "RWANDA" = "RWA", "SEYCHELLES" = "SEY", "SOUTH AFRICA" = "SOA", "SOUTH SUDAN" = "SSD", "UNITED REPUBLIC OF TANZANIA" = "TAN", "UGANDA" = "UGA", "ZAMBIA" = "ZAM", "ZIMBABWE" = "ZIM"),
  c("DEMOCRATIC REPUBLIC OF THE CONGO" = "DRC"),
  c("CONGO"="CNG", "GABON"="GBN", "EQUATORIAL GUINEA"="EQG")
)

country_abbrs <- unlist(country_abbrs_list)
cat("🌍 Country abbreviations loaded for", length(country_abbrs), "countries\n")

# ============================================================
# ---- Auto-detect available countries ----
# ============================================================

update_blocks_with_actual_data <- function(blocks, data) {
  if (is.null(data)) return(blocks)
  available_countries <- unique(toupper(data$country))
  updated_blocks <- lapply(blocks, function(x) intersect(x, available_countries))
  updated_blocks <- updated_blocks[sapply(updated_blocks, length) > 0]
  
  if (length(updated_blocks) == 0) {
    cat("⚠️ No AFRO blocks match available data - using all blocks\n")
    return(blocks)
  }
  
  updated_blocks
}

afro_blocks <- update_blocks_with_actual_data(afro_blocks, dat)
cat("🌍 AFRO blocks updated based on available data\n")
cat("Available blocks:", paste(names(afro_blocks), collapse = ", "), "\n\n")

# Final memory cleanup
if (is_deployment) {
  gc()
  cat("💾 Final memory cleanup completed\n")
}

cat("🎉 Deployment-ready data loading completed! Starting Shiny app...\n")

# ============================================================
# 👶 ADMIN coverage - COMPUTATIONALLY IDENTICAL with proper legend
# ============================================================
admin_coverage_summary <- function(
    admin_data,
    afro_blocks = NULL,
    ist_blocks = NULL,
    block_type = "afro",
    block_selection = "All",
    country_selection = NULL,
    x_months = 12,
    year_selection = NULL
) {
  flextable::set_flextable_defaults(
    background.color = "white",
    font.family = "Calibri",
    font.size = 10
  )
  
  # admin_data should already be standardized
  admin_data <- standardize_admin_data(admin_data)
  block_definition <- if (block_type == "afro") afro_blocks else ist_blocks
  
  # Set locale for English month names
  original_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", original_locale))
  
  # STEP 1: Initial data preparation (EXACTLY like original)
  df <- admin_data |>
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "DEMOCRATIC REPUBLIC OF THE CONGO" ~ "DEMOCRATIC REPUBLIC OF THE CONGO", #"DRC",
        Country == "CENTRAL AFRICAN REPUBLIC" ~ "CAR",
        TRUE ~ Country
      ),
      date = lubridate::as_date(SIA_date),  # EXACT as_date() function
      floor_date = lubridate::floor_date(date, unit = "months"),  # EXACT unit parameter
      vaccine.type = Vaccine_type
    )
  
  # STEP 2: Apply country/block filtering FIRST (EXACT order)
  if (!is.null(block_definition) && !is.null(block_selection) && block_selection != "All") {
    df <- df |> dplyr::filter(Country %in% block_definition[[block_selection]])
  }
  if (!is.null(country_selection) && length(country_selection) > 0) {
    df <- df |> dplyr::filter(Country %in% country_selection)
  }
  
  # STEP 3: Apply date filtering EXACTLY as original
  if (!is.null(x_months) && is.numeric(x_months)) {
    start_date <- lubridate::floor_date(Sys.Date(), unit = "months") - months(x_months)
    df <- df |> dplyr::filter(date >= start_date)
  }
  
  # STEP 4: Remove NA dates
  df <- df |> dplyr::filter(!is.na(date))
  
  if (nrow(df) == 0) stop("No data found for filters.")
  
  # STEP 5: Select EXACT columns in EXACT order (like original)
  df <- df |>
    dplyr::select(Country, District, vaccine.type, floor_date, CVPolio)
  
  # STEP 6: Get vaccine types used (EXACT logic)
  used_vaccines <- df |>
    dplyr::group_by(Country, vaccine.type) |>
    dplyr::summarise(has_data = dplyr::n() > 0, .groups = "drop") |>
    dplyr::filter(has_data) |>
    dplyr::select(-has_data)
  
  # STEP 7: District summary (EXACT calculations)
  district_summary <- df |>
    dplyr::inner_join(used_vaccines, by = c("Country", "vaccine.type")) |>
    dplyr::mutate(
      CVPolio = suppressWarnings(as.numeric(CVPolio)),  # EXACT conversion
      cv_category = dplyr::case_when(
        CVPolio >= 0.95 ~ "≥95%",
        CVPolio >= 0.80 ~ "80-94%",
        TRUE ~ "<80%"
      )
    ) |>
    dplyr::group_by(Country, vaccine.type, floor_date, cv_category) |>
    dplyr::summarise(n_districts = dplyr::n_distinct(District), .groups = "drop") |>
    tidyr::complete(Country, vaccine.type, floor_date, cv_category, fill = list(n_districts = 0))
  
  # STEP 8: Total districts (EXACT calculation)
  total_districts <- df |>
    dplyr::inner_join(used_vaccines, by = c("Country", "vaccine.type")) |>
    dplyr::group_by(Country, vaccine.type, floor_date) |>
    dplyr::summarise(total = dplyr::n_distinct(District), .groups = "drop")
  
  # STEP 9: Merge and compute percentages (EXACT rounding)
  prop_data <- dplyr::left_join(district_summary, total_districts,
                                by = c("Country", "vaccine.type", "floor_date")) |>
    dplyr::mutate(percent = round((n_districts / total) * 100, 0)) |>
    dplyr::filter(cv_category == "≥95%") |>
    dplyr::select(Country, vaccine.type, floor_date, percent)
  
  # STEP 10: Create wide format
  report_df <- prop_data |>
    dplyr::mutate(Month_Yr = format_ISO8601(floor_date, precision = "ym")) |>
    dplyr::select(-floor_date) |>
    tidyr::pivot_wider(names_from = Month_Yr, values_from = percent) |>
    dplyr::arrange(Country, vaccine.type) |>
    dplyr::rename(`Vaccine Type` = vaccine.type)
  
  # STEP 11: Remove empty rows (EXACT logic)
  if (ncol(report_df) > 2) {
    # Create a logical vector for rows with any non-NA in month columns
    month_cols <- 3:ncol(report_df)
    keep_rows <- apply(!is.na(report_df[, month_cols, drop = FALSE]), 1, any)
    report_df <- report_df[keep_rows, , drop = FALSE]
  }
  
  if (nrow(report_df) == 0) stop("No coverage data available after filtering.")
  
  # STEP 12: Format headers
  month_labels <- colnames(report_df)[3:ncol(report_df)]
  month_abbr <- format(as.Date(paste0(month_labels, "-01")), "%b")
  year_labels <- format(as.Date(paste0(month_labels, "-01")), "%Y")
  
  # STEP 13: Color picker (EXACT same bins)
  bg_picker <- scales::col_bin(
    palette = c("red", "yellow", "green"),
    domain = c(0, 100),
    bins = c(0, 80, 95, 100)
  )
  
  # STEP 14: Create header text
  period_text <- if (!is.null(x_months) && is.numeric(x_months)) {
    paste("Last", x_months, "Months")
  } else if (!is.null(year_selection) && length(year_selection) > 0) {
    if (length(year_selection) == 1) paste("Year", year_selection)
    else paste("Years", paste(year_selection, collapse = ", "))
  } else "Selected Period"
  
  block_display <- if (!is.null(block_selection) && block_selection != "All") {
    paste(block_selection)
  } else {
    "All Countries"
  }
  
  # STEP 15: Create flextable with proper legend formatting
  ft <- flextable::flextable(report_df) |>
    flextable::set_header_df(
      mapping = data.frame(
        keys = c("Country", "Vaccine Type", month_labels),
        Year = c("", "", year_labels),
        Month = c("Country", "Vaccine Type", month_abbr)
      ),
      key = "keys"
    ) |>
    flextable::merge_h(part = "header") |>
    flextable::merge_v(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(align = "center", part = "body") |>
    flextable::bg(j = month_labels, bg = bg_picker, part = "body") |>
    flextable::hline(part = "all") |>
    flextable::vline(part = "all") |>
    flextable::width(j = 1, width = 1.5) |>
    flextable::width(j = 2, width = 1.2) |>
    flextable::set_table_properties(layout = "fixed") |>
    flextable::add_header_lines("Proportion of Districts with Coverage ≥95%") |>
    flextable::compose(
      i = 1, j = 1, part = "header",
      value = flextable::as_paragraph(
        flextable::as_chunk("Admin Data Overview for ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(block_display, props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" - ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(period_text, props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(" - ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(format(Sys.Date(), "%Y-%m-%d"), props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(" | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Green", props = officer::fp_text(color = "green", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(" = ≥95% | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Yellow", props = officer::fp_text(color = "orange", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(" = 80-94% | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Red", props = officer::fp_text(color = "red", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(" = <80%", props = officer::fp_text(font.family = "Calibri"))
      )
    ) |>
    flextable::add_footer_lines(paste(
      "The table is showing months with SIA data \nSource: PEP SIA repository © WHO AFRO, Generated on:",
      Sys.Date()
    ))
  
  # STEP 16: Calculate metrics (EXACT same calculations)
  avg_coverage <- mean(prop_data$percent, na.rm = TRUE)
  pct_ge95 <- round(mean(prop_data$percent >= 95, na.rm = TRUE) * 100, 1)
  n_countries <- dplyr::n_distinct(prop_data$Country)
  
  # Debug output to verify calculations
  cat("\n📊 ADMIN COVERAGE CALCULATION VERIFICATION:\n")
  cat("   Total rows in prop_data:", nrow(prop_data), "\n")
  cat("   Unique countries:", n_countries, "\n")
  cat("   Average coverage:", round(avg_coverage, 1), "%\n")
  cat("   % districts ≥95%:", pct_ge95, "%\n")
  cat("   Date range:", min(prop_data$floor_date), "to", max(prop_data$floor_date), "\n")
  
  list(
    flextable = ft,
    metrics = list(
      avg_coverage = avg_coverage,
      pct_ge95 = pct_ge95,
      n_countries = n_countries
    )
  )
}

# ============================================================
# 👶 ADMIN VACCINATED SUMMARY (chronological + styled)
# ============================================================
admin_vaccinated_summary <- function(
    admin_data,
    afro_blocks = NULL,
    ist_blocks = NULL,
    block_type = "afro",
    block_selection = "All",
    country_selection = NULL,
    x_months = 12,
    year_selection = NULL
) {
  flextable::set_flextable_defaults(
    background.color = "white",
    font.family = "Calibri",
    font.size = 10
  )
  
  # admin_data should already be standardized, but ensure it is
  admin_data <- standardize_admin_data(admin_data)
  block_definition <- if (block_type == "afro") afro_blocks else ist_blocks
  
  original_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", original_locale))
  
  df <- admin_data |>
    dplyr::mutate(
      Country = dplyr::case_when(
        Country == "DEMOCRATIC REPUBLIC OF THE CONGO" ~ "DRC",
        Country == "CENTRAL AFRICAN REPUBLIC" ~ "CAR",
        TRUE ~ Country
      ),
      date = SIA_date,  # Use already standardized date
      floor_date = lubridate::floor_date(date, "month"),
      vaccine.type = Vaccine_type
    ) |>
    dplyr::filter(!is.na(date))
  
  # Filter by months
  if (!is.null(x_months) && is.numeric(x_months)) {
    start_date <- as.Date(paste0(format(Sys.Date() - months(x_months), "%Y-%m"), "-01"))
    df <- df |> dplyr::filter(date >= start_date)
  }
  
  # Filter by years
  if (!is.null(year_selection) && length(year_selection) > 0) {
    df <- df |> dplyr::filter(lubridate::year(date) %in% year_selection)
  }
  
  # Filter by block
  if (!is.null(block_definition) && !is.null(block_selection) && block_selection != "All") {
    df <- df |> dplyr::filter(Country %in% block_definition[[block_selection]])
  }
  
  # Filter by country
  if (!is.null(country_selection) && length(country_selection) > 0) {
    df <- df |> dplyr::filter(Country %in% country_selection)
  }
  
  if (nrow(df) == 0) stop("No data found. (Check Country names/blocks and SIA_date parsing.)")
  
  total_doses <- sum(df$TotalNbrVaccPolio, na.rm = TRUE)
  n_countries <- dplyr::n_distinct(df$Country)
  
  children_per_round <- df |>
    dplyr::group_by(Country, floor_date) |>
    dplyr::summarise(children_vaccinated = sum(TotalNbrVaccPolio, na.rm = TRUE), .groups = "drop")
  
  total_children_vaccnted <- children_per_round |>
    dplyr::group_by(Country) |>
    dplyr::summarise(total_children_vaccnted = max(children_vaccinated, na.rm = TRUE), .groups = "drop") |>
    dplyr::summarise(total_children_vaccnted = sum(total_children_vaccnted, na.rm = TRUE))
  
  ft_data <- df |>
    dplyr::select(Country, vaccine.type, floor_date, TotalNbrVaccPolio) |>
    dplyr::mutate(
      vaccine.type = dplyr::case_when(
        vaccine.type == "nOPV2&bOPV" ~ "nOPV2 & bOPV",
        TRUE ~ vaccine.type
      )
    ) |>
    dplyr::group_by(Country, vaccine.type, floor_date) |>
    dplyr::summarise(NVAC = sum(TotalNbrVaccPolio, na.rm = TRUE) / 1000000, .groups = "drop") |>
    dplyr::mutate(NVAC = round(NVAC, 2)) |>
    dplyr::arrange(floor_date)
  
  report_df <- ft_data |>
    dplyr::mutate(Month_Yr = format_ISO8601(floor_date, precision = "ym")) |>
    dplyr::select(Country, vaccine.type, Month_Yr, NVAC) |>
    tidyr::pivot_wider(names_from = Month_Yr, values_from = NVAC) |>
    dplyr::rename(`Vaccine Type` = vaccine.type) |>
    dplyr::arrange(Country, `Vaccine Type`)
  
  # Fix: Replace across() with direct column selection
  if (ncol(report_df) > 2) {
    # Get month columns (all columns after the first 2)
    month_cols <- 3:ncol(report_df)
    # Filter rows that have at least one non-NA in month columns
    report_df <- report_df[apply(!is.na(report_df[, month_cols, drop = FALSE]), 1, any), , drop = FALSE]
  }
  
  if (nrow(report_df) == 0) stop("No vaccination data available after filtering.")
  
  month_labels <- colnames(report_df)[3:ncol(report_df)]
  month_abbr <- sapply(month_labels, function(x) format(as.Date(paste0(x, "-01")), "%b"))
  year_labels <- format(as.Date(paste0(month_labels, "-01")), "%Y")
  
  total_vaccinated_by_vaccine <- df |>
    dplyr::group_by(Vaccine_type) |>
    dplyr::summarise(Total_Vaccinated = sum(TotalNbrVaccPolio, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Vaccine_Type = Vaccine_type,
      Total_Vaccinated = format(Total_Vaccinated, big.mark = ",")
    ) |>
    dplyr::select(Vaccine_Type, Total_Vaccinated)
  
  vaccine_totals_text <- total_vaccinated_by_vaccine |>
    dplyr::summarise(Footnote = paste(Vaccine_Type, ":", Total_Vaccinated, collapse = "; ")) |>
    dplyr::pull(Footnote)
  
  footnote_text <- paste(
    "Number of countries:", n_countries,
    "\nTotal doses administrated by Vaccine Type:", vaccine_totals_text
  )
  
  period_text <- if (!is.null(x_months) && is.numeric(x_months)) {
    paste("Last", x_months, "Months")
  } else if (!is.null(year_selection) && length(year_selection) > 0) {
    if (length(year_selection) == 1) paste("Year", year_selection)
    else paste("Years", paste(year_selection, collapse = ", "))
  } else "Selected Period"
  
  block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
  
  ft <- flextable::flextable(report_df) |>
    flextable::set_header_df(
      mapping = data.frame(
        keys = c("Country", "Vaccine Type", month_labels),
        Year = c("", "", year_labels),
        Month = c("Country", "Vaccine Type", month_abbr)
      ),
      key = "keys"
    ) |>
    flextable::merge_h(part = "header") |>
    flextable::merge_v(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(align = "center", part = "body") |>
    flextable::bg(j = month_labels, bg = "lightblue", part = "body") |>
    flextable::hline(part = "all") |>
    flextable::vline(part = "all") |>
    flextable::width(j = 1, width = 1.5) |>
    flextable::width(j = 2, width = 1.2) |>
    flextable::set_table_properties(layout = "fixed") |>
    flextable::add_header_lines("Number of Children Vaccinated (in millions)") |>
    flextable::compose(
      i = 1, j = 1, part = "header",
      value = flextable::as_paragraph(
        flextable::as_chunk("Admin Data Overview | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(block_type_text, props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" | Period: ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(period_text, props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(" | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Note: Multiply each figure by 1,000,000",
                            props = officer::fp_text(font.family = "Calibri", italic = TRUE))
      )
    ) |>
    flextable::add_footer_lines(footnote_text) |>
    flextable::add_footer_lines(paste(
      "The table is showing months with SIA data \nSource: PEP SIA repository © WHO AFRO, Generated on:",
      Sys.Date()
    ))
  
  list(
    flextable = ft,
    metrics = list(
      total_dose_administrated = round(total_doses / 1e6, 1),
      total_children_vaccinated = round(total_children_vaccnted$total_children_vaccnted / 1e6, 1),
      n_countries = n_countries
    )
  )
}






# ============================================================
# 👶 missed children seggregated by sex
# ✅ FIXES:
#   - Works with OLD lubridate (no add_with_rollback(months=...), no %m-% months())
#   - Robust "Last X Months" date range using base seq() month stepping
#   - Proper Year-grouped chronological ordering (Year -> Month -> Gender)
#   - Prevents flextable HTML rendering crashes (gsub/htmlize) via UTF-8 sanitizing
# ============================================================
missed_children_disaggregated <- function(
    data,
    year = NULL, Q = NULL,
    last_n_quarters = NULL,
    x_months = NULL,
    block_selection = NULL,
    country_selection = NULL,
    afro_blocks = NULL,
    ist_blocks = NULL,
    block_type = "afro"
) {
  
  # --------------------------
  # UTF-8 sanitizer (prevents htmlize/gsub crashes in flextable rendering)
  # --------------------------
  safe_utf8 <- function(x) {
    if (is.null(x)) return(x)
    if (inherits(x, "Date")) return(x)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- iconv(x, from = "", to = "UTF-8", sub = "")
      x[is.na(x)] <- ""
    }
    x
  }
  safe_utf8_df <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    for (nm in names(df)) {
      if (is.factor(df[[nm]]) || is.character(df[[nm]])) {
        df[[nm]] <- safe_utf8(df[[nm]])
      }
    }
    df
  }
  
  # --------------------------
  # Select appropriate block definition
  # --------------------------
  block_definition <- if (identical(block_type, "afro")) afro_blocks else ist_blocks
  
  # --------------------------
  # Apply geographic filters
  # --------------------------
  filtered_data <- data
  
  if (!is.null(block_selection) && block_selection != "All") {
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data %>% dplyr::filter(country %in% block_countries)
  }
  
  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data %>% dplyr::filter(country %in% country_selection)
  }
  
  # --------------------------
  # Define quarter ranges
  # --------------------------
  quarter_ranges <- list(
    "1" = c("01", "03"),
    "2" = c("04", "06"),
    "3" = c("07", "09"),
    "4" = c("10", "12")
  )
  
  # --------------------------
  # Determine date range (ROBUST for old lubridate)
  # --------------------------
  if (!is.null(x_months)) {
    
    if (x_months < 1) stop("`x_months` must be at least 1.")
    
    # Last day of previous month
    end_date <- lubridate::floor_date(Sys.Date(), unit = "month") - lubridate::days(1)
    
    # First day of end_date month
    start_anchor <- as.Date(format(end_date, "%Y-%m-01"))
    
    # Go back (x_months - 1) months using base seq() stepping
    # Example: x_months=1 => same month; x_months=6 => 6th element backwards
    if (x_months > 1) {
      start_date <- seq(from = start_anchor, by = "-1 month", length.out = x_months)[x_months]
    } else {
      start_date <- start_anchor
    }
    
    start_date <- lubridate::floor_date(start_date, unit = "month")
    
    period_text <- paste("Last", x_months, "month(s):", format(start_date, "%b %Y"), "to", format(end_date, "%b %Y"))
    dynamic_period <- paste("Last", x_months, "Months")
    
  } else if (!is.null(last_n_quarters)) {
    
    current_year <- lubridate::year(Sys.Date())
    
    if (last_n_quarters < 1 || last_n_quarters > 4) {
      stop("`last_n_quarters` must be between 1 and 4.")
    }
    
    start_date <- as.Date(paste0(current_year, "-", quarter_ranges[["1"]][1], "-01"))
    end_q <- as.character(last_n_quarters)
    
    end_date <- lubridate::ceiling_date(
      as.Date(paste0(current_year, "-", quarter_ranges[[end_q]][2], "-01")),
      unit = "month"
    ) - lubridate::days(1)
    
    if (last_n_quarters == 1) {
      period_text <- paste("Quarter 1", current_year)
      dynamic_period <- paste("Quarter 1", current_year)
    } else {
      period_text <- paste("Quarters 1 to", last_n_quarters, current_year)
      dynamic_period <- paste("Quarters 1-", last_n_quarters, current_year)
    }
    
  } else if (!is.null(year) && !is.null(Q)) {
    
    if (Q < 1 || Q > 4) stop("`Q` must be between 1 and 4.")
    
    start_date <- as.Date(paste0(year, "-", quarter_ranges[[as.character(Q)]][1], "-01"))
    end_date <- lubridate::ceiling_date(
      as.Date(paste0(year, "-", quarter_ranges[[as.character(Q)]][2], "-01")),
      unit = "month"
    ) - lubridate::days(1)
    
    period_text <- paste("Quarter", Q, year)
    dynamic_period <- paste("Quarter", Q, year)
    
  } else {
    stop("Please provide either (year and Q), last_n_quarters, or x_months")
  }
  
  # --------------------------
  # Prepare the data with robust date parsing
  # --------------------------
  filtered_data <- filtered_data |>
    dplyr::mutate(country = dplyr::case_when(
      country == "Ethiopia" ~ "ETH",
      TRUE ~ country
    )) |>
    dplyr::mutate(date = tryCatch({
      lubridate::as_date(round_start_date)
    }, warning = function(w) {
      message("Warning in date parsing: ", w$message)
      if (is.character(round_start_date)) {
        lubridate::as_date(lubridate::parse_date_time(round_start_date, orders = c("ymd", "dmy", "mdy")))
      } else {
        lubridate::as_date(round_start_date)
      }
    }, error = function(e) {
      message("Error in date parsing: ", e$message)
      NA_Date_
    }))
  
  filtered_data <- filtered_data |> dplyr::filter(!is.na(date))
  
  # Debug messages
  message(paste("Date range requested:", start_date, "to", end_date))
  
  available_min <- if (nrow(filtered_data) > 0) min(filtered_data$date, na.rm = TRUE) else NA
  available_max <- if (nrow(filtered_data) > 0) max(filtered_data$date, na.rm = TRUE) else NA
  
  message(paste("Available data range:", available_min, "to", available_max))
  message(paste("Rows before date filtering:", nrow(filtered_data)))
  
  if (nrow(filtered_data) > 0) {
    filtered_data <- filtered_data |> dplyr::filter(date >= start_date & date <= end_date)
  }
  
  message(paste("Rows after date filtering:", nrow(filtered_data)))
  
  # --------------------------
  # Empty handling
  # --------------------------
  if (nrow(filtered_data) == 0) {
    
    available_data_range <- paste(
      if (!is.null(data$round_start_date) && length(data$round_start_date) > 0) min(data$round_start_date, na.rm = TRUE) else "No data",
      "to",
      if (!is.null(data$round_start_date) && length(data$round_start_date) > 0) max(data$round_start_date, na.rm = TRUE) else "No data"
    )
    
    warning_msg <- paste(
      "No data available for the selected filters.",
      "Date range:", start_date, "to", end_date,
      "Available data in source:", available_data_range
    )
    warning(warning_msg)
    
    return(list(
      flextable = flextable::flextable(tibble::tibble(Note = "No data available for the selected date range and filters.")),
      summary_data = tibble::tibble(),
      raw_data = tibble::tibble(),
      period_text = period_text,
      geo_info = NA,
      dynamic_period = dynamic_period,
      overall_missed = list(perc_m = 0, perc_f = 0)
    ))
  }
  
  # --------------------------
  # Create summary data for flextable
  # --------------------------
  summary_data <- filtered_data |>
    dplyr::select(
      ctry = country, vaccine.type, floor_date = date,
      mv = male_vaccinated, fv = female_vaccinated,
      ms = male_sampled, fs = female_sampled
    ) |>
    dplyr::mutate(floor_date = lubridate::floor_date(floor_date, unit = "months")) |>
    dplyr::mutate(
      mm = ms - mv,
      fm = fs - fv
    ) |>
    dplyr::group_by(ctry, vaccine.type, floor_date) |>
    dplyr::summarise(
      mm = sum(mm, na.rm = TRUE),
      fm = sum(fm, na.rm = TRUE),
      ms = sum(ms, na.rm = TRUE),
      fs = sum(fs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prop_m = ifelse(ms > 0, mm / ms, 0),
      prop_f = ifelse(fs > 0, fm / fs, 0),
      perc_m = round(prop_m * 100, 2),
      perc_f = round(prop_f * 100, 2)
    ) |>
    dplyr::mutate(Month_Yr = lubridate::format_ISO8601(floor_date, precision = "ym")) |>
    tidyr::separate(Month_Yr, sep = "-", into = c("year", "mo")) |>
    dplyr::mutate(
      mo = as.numeric(mo),
      mo_label = dplyr::case_when(
        mo == 1 ~ "Jan", mo == 2 ~ "Feb", mo == 3 ~ "Mar",
        mo == 4 ~ "Apr", mo == 5 ~ "May", mo == 6 ~ "Jun",
        mo == 7 ~ "Jul", mo == 8 ~ "Aug", mo == 9 ~ "Sep",
        mo == 10 ~ "Oct", mo == 11 ~ "Nov", mo == 12 ~ "Dec",
        TRUE ~ NA_character_
      ),
      Month_Yr = paste0(year, "_", mo_label)
    ) |>
    dplyr::select(-c(year, mo, mo_label, mm, fm, ms, fs, floor_date, prop_m, prop_f)) |>
    tidyr::pivot_longer(
      cols = c(perc_m, perc_f),
      names_to = "gender",
      values_to = "perc"
    ) |>
    dplyr::mutate(
      gender = ifelse(gender == "perc_m", "M", "F"),
      header = paste0(Month_Yr, "_", gender)
    ) |>
    dplyr::select(-c(Month_Yr, gender)) |>
    tidyr::pivot_wider(
      names_from = header,
      values_from = perc
    ) |>
    dplyr::arrange(ctry, vaccine.type) |>
    dplyr::rename("Country" = ctry, "Vaccine Type" = vaccine.type)
  
  # sanitize strings before flextable rendering
  summary_data <- safe_utf8_df(summary_data)
  
  # --------------------------
  # FIXED: Proper chronological order grouped by YEAR then MONTH
  # --------------------------
  time_cols <- names(summary_data)[stringr::str_detect(names(summary_data), "^\\d{4}_.+_")]
  
  if (length(time_cols) > 0) {
    sorted_cols <- time_cols |>
      tibble::enframe(name = NULL, value = "col") |>
      dplyr::mutate(
        year = as.numeric(stringr::str_extract(col, "^\\d{4}")),
        mon = stringr::str_extract(col, "(?<=_)\\w{3}(?=_)"),
        mon = factor(mon, levels = month.abb, ordered = TRUE),
        month_num = match(mon, month.abb)
      ) |>
      dplyr::arrange(year, month_num) |>
      dplyr::pull(col)
    
    summary_data <- summary_data |> dplyr::select(dplyr::all_of(c("Country", "Vaccine Type", sorted_cols)))
    time_cols <- sorted_cols
  }
  
  # --------------------------
  # Background color bins
  # --------------------------
  bg_picker <- scales::col_bin(
    palette = c("green", "red"),
    domain = c(0, 100),
    bins = c(0, 2, 100)
  )
  
  cols <- summary_data |> dplyr::select(dplyr::starts_with("2")) |> names()
  
  # --------------------------
  # Overall missed proportions by sex
  # --------------------------
  overall_missed <- filtered_data |>
    dplyr::summarise(
      missed_m = sum(male_sampled - male_vaccinated, na.rm = TRUE),
      total_m  = sum(male_sampled, na.rm = TRUE),
      missed_f = sum(female_sampled - female_vaccinated, na.rm = TRUE),
      total_f  = sum(female_sampled, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      prop_m = ifelse(total_m > 0, missed_m / total_m, 0),
      prop_f = ifelse(total_f > 0, missed_f / total_f, 0),
      perc_m = round(prop_m * 100, 2),
      perc_f = round(prop_f * 100, 2)
    )
  
  n_countries <- summary_data |> dplyr::pull(Country) |> unique() |> length()
  
  target_status <- dplyr::case_when(
    overall_missed$prop_m <= 0.02 & overall_missed$prop_f <= 0.02 ~ "Both are within the ≤ 2% target.",
    overall_missed$prop_m <= 0.02 & overall_missed$prop_f >  0.02 ~ "Only boys are within the ≤ 2% target.",
    overall_missed$prop_m >  0.02 & overall_missed$prop_f <= 0.02 ~ "Only girls are within the ≤ 2% target.",
    TRUE ~ "Both exceed the 2% target."
  )
  
  # --------------------------
  # Geo info text
  # --------------------------
  geo_info <- ""
  if (!is.null(block_selection) && block_selection != "All") {
    block_type_text0 <- ifelse(identical(block_type, "afro"), "AFRO Block", "IST Block")
    geo_info <- paste0(" | ", block_type_text0, ": ", block_selection)
  }
  if (!is.null(country_selection) && length(country_selection) > 0) {
    geo_info <- paste0(" | Countries: ", paste(country_selection, collapse = ", "))
  }
  
  summary_line <- paste0(
    "Number of countries that implemented vaccination campaigns: n = ", n_countries,
    ". The overall missed rate for boys is ", overall_missed$perc_m,
    "%, and for girls, it is ", overall_missed$perc_f, "%. ", target_status,
    " Period: ", period_text, geo_info
  )
  
  # sanitize strings used in header/footer
  period_text     <- safe_utf8(period_text)
  dynamic_period  <- safe_utf8(dynamic_period)
  geo_info        <- safe_utf8(geo_info)
  summary_line    <- safe_utf8(summary_line)
  
  # --------------------------
  # Build header structure
  # --------------------------
  header_keys <- names(summary_data)
  
  # if no time columns, return a basic table
  if (length(time_cols) == 0) {
    ft0 <- flextable::flextable(summary_data) |> flextable::autofit()
    return(list(
      flextable = ft0,
      summary_data = summary_data,
      raw_data = filtered_data,
      period_text = period_text,
      geo_info = geo_info,
      dynamic_period = dynamic_period,
      overall_missed = overall_missed
    ))
  }
  
  year_labels <- c("", "", vapply(time_cols, function(x) strsplit(x, "_")[[1]][1], character(1)))
  month_labels <- c("", "", vapply(time_cols, function(x) strsplit(x, "_")[[1]][2], character(1)))
  gender_labels <- c("", "", vapply(time_cols, function(x) strsplit(x, "_")[[1]][3], character(1)))
  
  header_df <- data.frame(
    keys = header_keys,
    Year = c("", "", year_labels[3:length(year_labels)]),
    Month = c("", "", month_labels[3:length(month_labels)]),
    Gender = c("Country", "Vaccine Type", gender_labels[3:length(gender_labels)]),
    stringsAsFactors = FALSE
  )
  header_df <- safe_utf8_df(header_df)
  
  # --------------------------
  # Build flextable
  # --------------------------
  ft <- flextable::flextable(summary_data) |>
    flextable::set_header_df(mapping = header_df, key = "keys") |>
    flextable::merge_h(part = "header") |>
    flextable::merge_v(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(align = "center", part = "body")
  
  if (length(cols) > 0) {
    ft <- ft |> flextable::bg(j = cols, bg = bg_picker)
  }
  
  ft <- ft |>
    flextable::hline(part = "all") |>
    flextable::vline(part = "all") |>
    flextable::width(j = 1, width = 1.5) |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::autofit() |>
    flextable::set_table_properties(layout = "autofit")
  
  block_type_text <- ifelse(identical(block_type, "afro"), "AFRO Blocks", "IST Blocks")
  block_type_text <- safe_utf8(block_type_text)
  
  ft <- ft |>
    flextable::add_header_lines("Missed Children Analysis") |>
    flextable::compose(
      i = 1, j = 1, part = "header",
      value = flextable::as_paragraph(
        flextable::as_chunk("Missed Children Segregated by Sex",
                            props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Block Type: ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(block_type_text, props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" | Period: ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(dynamic_period, props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(" | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Green", props = officer::fp_text(color = "green", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(": ≤2% | ", props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Red", props = officer::fp_text(color = "red", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(": >2%", props = officer::fp_text(font.family = "Calibri"))
      )
    ) |>
    flextable::add_footer_lines(summary_line) |>
    flextable::add_footer_lines(
      paste("The table is showing months with SIA data \nSource: PEP SIA repository © WHO AFRO, Generated on:", Sys.Date())
    )
  
  return(list(
    flextable = ft,
    summary_data = summary_data,
    raw_data = filtered_data,
    period_text = period_text,
    geo_info = geo_info,
    dynamic_period = dynamic_period,
    overall_missed = overall_missed
  ))
}





# ============================================================
# 📈 COVERAGE ANALYSIS (FIXED with Proper Year-Grouped Chronological Ordering)
# ============================================================
coverage_disaggregated <- function(data, year = NULL, Q = NULL, last_n_quarters = NULL, x_months = NULL,
                                   block_selection = NULL, country_selection = NULL,
                                   afro_blocks = NULL, ist_blocks = NULL, block_type = "afro") {
  
  # --------------------------
  # Select appropriate block definition
  # --------------------------
  if (block_type == "afro") {
    block_definition <- afro_blocks
  } else {
    block_definition <- ist_blocks
  }
  
  # --------------------------
  # Apply geographic filters
  # --------------------------
  filtered_data <- data
  
  # Filter by block if specified
  if (!is.null(block_selection) && block_selection != "All") {
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data %>% dplyr::filter(country %in% block_countries)
  }
  
  # Filter by specific countries if specified
  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data %>% dplyr::filter(country %in% country_selection)
  }
  
  # --------------------------
  # Define quarter ranges
  # --------------------------
  quarter_ranges <- list(
    "1" = c("01", "03"),
    "2" = c("04", "06"),
    "3" = c("07", "09"),
    "4" = c("10", "12")
  )
  
  # --------------------------
  # Determine date range (FIXED)
  # --------------------------
  if (!is.null(x_months)) {
    # Last x months functionality - FIXED CALCULATION
    if (x_months < 1) {
      stop("`x_months` must be at least 1.")
    }
    
    # Improved date range calculation
    current_date <- Sys.Date()
    end_date <- floor_date(current_date, unit = "month") - days(1)  # Last day of previous month
    start_date <- end_date %m-% months(x_months - 1)  # Go back x-1 months from end_date
    start_date <- floor_date(start_date, unit = "month")  # First day of that month
    
    period_text <- paste("Last", x_months, "month(s):", format(start_date, "%b %Y"), "to", format(end_date, "%b %Y"))
    dynamic_period <- paste("Last", x_months, "Months")
    
  } else if (!is.null(last_n_quarters)) {
    # Cumulative quarters functionality - FIXED
    current_year <- year(Sys.Date())
    
    if (last_n_quarters < 1 || last_n_quarters > 4) {
      stop("`last_n_quarters` must be between 1 and 4.")
    }
    
    start_date <- as.Date(paste0(current_year, "-", quarter_ranges[["1"]][1], "-01"))
    end_q <- as.character(last_n_quarters)
    end_date <- ceiling_date(
      as.Date(paste0(current_year, "-", quarter_ranges[[end_q]][2], "-01")),
      unit = "month"
    ) - days(1)
    
    if (last_n_quarters == 1) {
      period_text <- paste("Quarter 1", current_year)
      dynamic_period <- paste("Quarter 1", current_year)
    } else {
      period_text <- paste("Quarters 1 to", last_n_quarters, current_year)
      dynamic_period <- paste("Quarters 1-", last_n_quarters, current_year)
    }
    
  } else if (!is.null(year) && !is.null(Q)) {
    # Original quarter functionality - FIXED to use provided year
    if (Q < 1 || Q > 4) {
      stop("`Q` must be between 1 and 4.")
    }
    
    start_date <- as.Date(paste0(year, "-", quarter_ranges[[as.character(Q)]][1], "-01"))
    end_date <- ceiling_date(as.Date(paste0(year, "-", quarter_ranges[[as.character(Q)]][2], "-01")), unit = "month") - days(1)
    period_text <- paste("Quarter", Q, year)
    dynamic_period <- paste("Quarter", Q, year)
    
  } else {
    stop("Please provide either (year and Q), last_n_quarters, or x_months")
  }
  
  # FIXED: Prepare the data with proper date parsing and debugging
  filtered_data <- filtered_data |> 
    mutate(country = if_else(country == "Ethiopia", "ETH", country)) |> 
    # FIXED: Handle date parsing with multiple formats and provide better error handling
    mutate(date = tryCatch({
      as_date(round_start_date)
    }, warning = function(w) {
      message("Warning in date parsing: ", w$message)
      # Try alternative parsing methods
      if (is.character(round_start_date)) {
        as_date(parse_date_time(round_start_date, orders = c("ymd", "dmy", "mdy")))
      } else {
        as_date(round_start_date)
      }
    }, error = function(e) {
      message("Error in date parsing: ", e$message)
      NA_Date_
    }))
  
  # Remove rows with NA dates
  filtered_data <- filtered_data |> filter(!is.na(date))
  
  # Debug information - FIXED to handle empty data
  message(paste("Coverage Analysis - Date range requested:", start_date, "to", end_date))
  
  available_min <- if(nrow(filtered_data) > 0) min(filtered_data$date, na.rm = TRUE) else NA
  available_max <- if(nrow(filtered_data) > 0) max(filtered_data$date, na.rm = TRUE) else NA
  
  message(paste("Coverage Analysis - Available data range:", available_min, "to", available_max))
  message(paste("Coverage Analysis - Rows before date filtering:", nrow(filtered_data)))
  
  # Apply date filtering only if we have data
  if(nrow(filtered_data) > 0) {
    filtered_data <- filtered_data |> 
      dplyr::filter(date >= start_date & date <= end_date)
  }
  
  message(paste("Coverage Analysis - Rows after date filtering:", nrow(filtered_data)))
  
  # FIXED: Better empty data handling
  if(nrow(filtered_data) == 0) {
    available_data_range <- paste(
      if(!is.null(data$round_start_date) && length(data$round_start_date) > 0) {
        min(data$round_start_date, na.rm = TRUE)
      } else "No data",
      "to",
      if(!is.null(data$round_start_date) && length(data$round_start_date) > 0) {
        max(data$round_start_date, na.rm = TRUE)
      } else "No data"
    )
    
    warning_msg <- paste("No data available for the selected filters.", 
                         "Date range:", start_date, "to", end_date,
                         "Available data in source:", available_data_range)
    
    warning(warning_msg)
    
    return(list(
      flextable = flextable(tibble(Note = "No data available for the selected date range and filters.")),
      summary_data = tibble(),
      raw_data = tibble(),
      period_text = period_text,
      geo_info = if(exists("geo_info")) geo_info else NA,
      dynamic_period = dynamic_period,
      coverage_data = list(perc_m = 0, perc_f = 0)
    ))
  }
  
  # Create the summary data for the flextable
  summary_data <- filtered_data |> 
    dplyr::select(ctry = country, vaccine.type, floor_date = date,
                  mv = male_vaccinated, fv = female_vaccinated,
                  ms = male_sampled, fs = female_sampled) |> 
    mutate(floor_date = floor_date(floor_date, unit = "months")) |>
    group_by(ctry, vaccine.type, floor_date) |> 
    summarise(
      mv = sum(mv, na.rm = TRUE),
      fv = sum(fv, na.rm = TRUE),
      ms = sum(ms, na.rm = TRUE),
      fs = sum(fs, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(
      prop_m = ifelse(ms > 0, mv / ms, NA_real_),
      prop_f = ifelse(fs > 0, fv / fs, NA_real_),
      perc_m = round(prop_m * 100, 2),
      perc_f = round(prop_f * 100, 2)
    ) |> 
    mutate(Month_Yr = format_ISO8601(floor_date, precision = "ym")) |> 
    separate(Month_Yr, sep = "-", into = c("year", "mo")) |> 
    mutate(
      mo = as.numeric(mo),
      mo_label = month.abb[mo],
      Month_Yr = paste0(year, "_", mo_label)
    ) |> 
    dplyr::select(-c(year, mo, mo_label, mv, fv, ms, fs, floor_date, prop_m, prop_f)) |> 
    pivot_longer(
      cols = c(perc_m, perc_f),
      names_to = "gender",
      values_to = "perc"
    ) |> 
    mutate(
      gender = ifelse(gender == "perc_m", "M", "F"),
      header = paste0(Month_Yr, "_", gender)
    ) |> 
    dplyr::select(-c(Month_Yr, gender)) |> 
    pivot_wider(
      names_from = header,
      values_from = perc
    ) |> 
    arrange(ctry, vaccine.type) |> 
    rename("Country" = ctry, "Vaccine Type" = vaccine.type)
  
  # --------------------------
  # FIXED: Group columns by YEAR in chronological order
  # --------------------------
  time_cols <- names(summary_data)[str_detect(names(summary_data), "^\\d{4}_.+_")]
  
  if(length(time_cols) > 0) {
    # Group by year first, then sort months within each year
    sorted_cols <- time_cols |>
      enframe(name = NULL, value = "col") |>
      mutate(
        year = as.numeric(str_extract(col, "^\\d{4}")),
        mon = str_extract(col, "(?<=_)\\w{3}(?=_)"),
        mon = factor(mon, levels = month.abb, ordered = TRUE),
        month_num = match(mon, month.abb)
      ) |>
      # Group by year and sort months within each year
      arrange(year, month_num) |>  
      pull(col)
    
    summary_data <- summary_data |> dplyr::select(all_of(c("Country", "Vaccine Type", sorted_cols)))
    
    # UPDATE: Use the SORTED columns for header creation
    time_cols <- sorted_cols
  }
  
  # --------------------------
  # Color scale
  # --------------------------
  bg_picker <- scales::col_bin(
    palette = c("red", "green"),
    domain = c(0, 100),
    bins = c(0, 80, 100)
  )
  
  # --------------------------
  # Footer coverage summary
  # --------------------------
  cov_data <- filtered_data |> 
    summarise(
      mv = sum(male_vaccinated, na.rm = TRUE),
      ms = sum(male_sampled, na.rm = TRUE),
      fv = sum(female_vaccinated, na.rm = TRUE),
      fs = sum(female_sampled, na.rm = TRUE)
    ) |> 
    mutate(
      cov_m = ifelse(ms > 0, mv / ms, NA_real_),
      cov_f = ifelse(fs > 0, fv / fs, NA_real_),
      perc_m = round(cov_m * 100, 2),
      perc_f = round(cov_f * 100, 2)
    )
  
  n_countries <- summary_data |> pull(Country) |> unique() |> length()
  
  coverage_status <- case_when(
    cov_data$cov_m >= 0.8 & cov_data$cov_f >= 0.8 ~ "Both boys and girls meet the ≥ 80% target.",
    cov_data$cov_m >= 0.8 & cov_data$cov_f < 0.8 ~ "Only boys meet the ≥ 80% target.",
    cov_data$cov_m < 0.8 & cov_data$cov_f >= 0.8 ~ "Only girls meet the ≥ 80% target.",
    TRUE ~ "Both boys and girls fall below the 80% target."
  )
  
  # Add geographic filter info to period text
  geo_info <- ""
  if (!is.null(block_selection) && block_selection != "All") {
    block_type_text <- ifelse(block_type == "afro", "AFRO Block", "IST Block")
    geo_info <- paste0(" | ", block_type_text, ": ", block_selection)
  }
  if (!is.null(country_selection) && length(country_selection) > 0) {
    geo_info <- paste0(" | Countries: ", paste(country_selection, collapse = ", "))
  }
  
  coverage_summary <- paste0(
    "Number of countries that implemented vaccination campaigns: n = ", n_countries,
    ". Overall vaccination coverage: Boys = ", cov_data$perc_m,
    "%, Girls = ", cov_data$perc_f, "%. ", coverage_status,
    " Period: ", period_text, geo_info
  )
  
  # --------------------------
  # Create custom header structure with Country and Vaccine Type aligned with Gender
  # --------------------------
  header_keys <- names(summary_data)
  
  # FIXED: Extract year and month labels from the SORTED columns
  year_labels <- c("", "", sapply(time_cols, function(x) str_split(x, "_")[[1]][1]))
  month_labels <- c("", "", sapply(time_cols, function(x) str_split(x, "_")[[1]][2]))
  gender_labels <- c("", "", sapply(time_cols, function(x) str_split(x, "_")[[1]][3]))
  
  # Fix the header values - Country and Vaccine Type in the Gender row to align with M/F
  header_df <- data.frame(
    keys = header_keys,
    Year = c("", "", year_labels[3:length(year_labels)]),
    Month = c("", "", month_labels[3:length(month_labels)]),
    Gender = c("Country", "Vaccine Type", gender_labels[3:length(gender_labels)])
  )
  
  # --------------------------
  # Build Flextable with custom headers
  # --------------------------
  cols_color <- summary_data |> dplyr::select(starts_with("2")) |> names()
  
  LQAS3 <- flextable(summary_data) |> 
    set_header_df(
      mapping = header_df,
      key = "keys"
    ) |>
    merge_h(part = "header") |>
    merge_v(part = "header") |>
    align(align = "center", part = "header") |>
    align(align = "center", part = "body")
  
  # Apply background colors
  if(length(cols_color) > 0) {
    LQAS3 <- LQAS3 |> bg(j = cols_color, bg = bg_picker)
  }
  
  # Add styling
  LQAS3 <- LQAS3 |> 
    hline(part = "all") |> 
    vline(part = "all") |> 
    width(j = 1, width = 1.5) |> 
    fontsize(size = 8, part = "all") |> 
    autofit() |> 
    set_table_properties(layout = "autofit")
  
  # ADDED: Block type indicator
  block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
  
  # --------------------------
  # Add the main header with colored legend and dynamic period
  # --------------------------
  LQAS3 <- LQAS3 |>
    flextable::add_header_lines("Coverage Analysis") |>
    flextable::compose(
      i = 1, j = 1, part = "header",
      value = flextable::as_paragraph(
        flextable::as_chunk("Proportion of Children Finger Marked by Sampled", 
                            props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" | ", 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Block Type: ", 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(block_type_text, 
                            props = officer::fp_text(font.family = "Calibri", bold = TRUE)),
        flextable::as_chunk(" | Period: ", 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(dynamic_period, 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk(" | ", 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Green", 
                            props = officer::fp_text(color = "green", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(": >=80% | ", 
                            props = officer::fp_text(font.family = "Calibri")),
        flextable::as_chunk("Red", 
                            props = officer::fp_text(color = "red", bold = TRUE, font.family = "Calibri")),
        flextable::as_chunk(": <80%", 
                            props = officer::fp_text(font.family = "Calibri"))
      )
    ) |>
    flextable::add_footer_lines(coverage_summary) |>
    flextable::add_footer_lines(paste(
      "The table is showing months with SIA data \nSource: PEP SIA repository © WHO AFRO, Generated on:", Sys.Date()
    )) |> 
    fontsize(i = 1:2, part = "footer", size = 8)
  
  # Return both the flextable and the raw data
  return(list(
    flextable = LQAS3,
    summary_data = summary_data,
    raw_data = filtered_data,
    period_text = period_text,
    geo_info = geo_info,
    dynamic_period = dynamic_period,
    coverage_data = cov_data
  ))
}


# Helper function to create grob from flextable
gen_grob <- function(x, fit = "auto") {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("flextable package is required for this function")
  }
  flextable::as_raster(x, fit = fit)
}


# ---- LQAS Summary Map (AFRO blocks) ----




# ============================================================
# 🔍 COMPREHENSIVE REASONS ANALYSIS FUNCTION (with All Viz Types)
#    + Priority Country Drill‑down + Trends + Block Summary
# ============================================================

# ============================================================
# 📝 ENHANCED HELPER FUNCTIONS
# ============================================================

analyze_district_trends <- function(data, districts_list, reason_col, 
                                    time_unit = "month") {
  # Filter data for selected districts
  trend_data <- data %>%
    filter(district %in% districts_list) %>%
    mutate(
      year_month = format(date, "%Y-%m"),
      year_quarter = paste0(year(date), "-Q", quarter(date)),
      year = year(date)
    )
  
  # Aggregate by time unit
  time_col <- switch(time_unit,
                     "month" = "year_month",
                     "quarter" = "year_quarter",
                     "year" = "year")
  
  trend_summary <- trend_data %>%
    group_by(district, !!sym(time_col)) %>%
    summarise(
      total_absent = sum(!!sym(reason_col), na.rm = TRUE),
      total_visits = n(),
      absent_rate = (total_absent / total_visits) * 100,
      .groups = "drop"
    ) %>%
    arrange(district, !!sym(time_col))
  
  # Calculate trend metrics
  trend_metrics <- trend_summary %>%
    group_by(district) %>%
    summarise(
      avg_rate = mean(absent_rate, na.rm = TRUE),
      min_rate = min(absent_rate, na.rm = TRUE),
      max_rate = max(absent_rate, na.rm = TRUE),
      trend_direction = ifelse(
        cor(as.numeric(factor(!!sym(time_col))), absent_rate, use = "complete.obs") > 0,
        "Increasing", "Decreasing"
      ),
      volatility = sd(absent_rate, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    trend_data = trend_summary,
    trend_metrics = trend_metrics,
    time_unit = time_unit
  )
}

create_block_summary <- function(drilldown_results, reason, block_name, block_type) {
  if (is.null(drilldown_results) || is.null(drilldown_results[[reason]])) {
    return(NULL)
  }
  
  prov_data <- drilldown_results[[reason]]$provinces
  dist_data <- drilldown_results[[reason]]$districts
  
  # Create comprehensive block summary
  block_summary <- data.frame(
    Metric = c(
      "Block Name",
      "Block Type",
      "Total Countries in Block",
      "Countries Meeting Threshold",
      "Total Provinces Identified",
      "Total Districts Identified",
      "Top Contributing Province",
      "Top Province Contribution (%)",
      "Top Contributing District", 
      "Top District Contribution (%)",
      "Average Province Contribution (%)",
      "Average District Contribution (%)",
      "Highest Country",
      "Most Volatile District"
    ),
    Value = c(
      block_name,
      if(block_type == "afro") "AFRO Blocks" else "IST Blocks",
      if(!is.null(drilldown_results[[reason]]$priority_countries)) 
        length(unique(c(prov_data$country, dist_data$country))) else 0,
      n_distinct(prov_data$country),
      nrow(prov_data),
      nrow(dist_data),
      if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A",
      if(nrow(prov_data) > 0) paste0(round(max(prov_data$prov_pct), 1), "%") else "N/A",
      if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A",
      if(nrow(dist_data) > 0) paste0(round(max(dist_data$dist_pct), 1), "%") else "N/A",
      if(nrow(prov_data) > 0) paste0(round(mean(prov_data$prov_pct, na.rm = TRUE), 1), "%") else "N/A",
      if(nrow(dist_data) > 0) paste0(round(mean(dist_data$dist_pct, na.rm = TRUE), 1), "%") else "N/A",
      if(nrow(prov_data) > 0) prov_data$country[which.max(prov_data$prov_pct)] else "N/A",
      if(!is.null(drilldown_results[[reason]]$trend_analysis)) {
        ta <- drilldown_results[[reason]]$trend_analysis
        if(nrow(ta$trend_metrics) > 0) {
          ta$trend_metrics$district[which.max(ta$trend_metrics$volatility)]
        } else "N/A"
      } else "N/A"
    )
  )
  
  return(block_summary)
}

# ============================================================
# 📝 ENHANCED FOOTNOTE GENERATOR - 1 to 3 Key Sentences
# ============================================================
generate_drilldown_footnote <- function(drilldown_results, reason, 
                                        priority_threshold, province_threshold,
                                        district_threshold, dynamic_threshold,
                                        period_label, block_name) {
  
  if (is.null(drilldown_results) || is.null(drilldown_results[[reason]])) {
    return("No drill-down data available")
  }
  
  prov_data <- drilldown_results[[reason]]$provinces
  dist_data <- drilldown_results[[reason]]$districts
  
  reason_name <- if(reason == "childabsent") "Child Absence" else "Non-Compliance"
  
  # Get top contributors
  top_province <- if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A"
  top_province_pct <- if(nrow(prov_data) > 0) round(max(prov_data$prov_pct), 1) else 0
  top_district <- if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A"
  top_district_pct <- if(nrow(dist_data) > 0) round(max(dist_data$dist_pct), 1) else 0
  
  # Calculate averages
  avg_prov_pct <- if(nrow(prov_data) > 0) round(mean(prov_data$prov_pct, na.rm = TRUE), 1) else 0
  avg_dist_pct <- if(nrow(dist_data) > 0) round(mean(dist_data$dist_pct, na.rm = TRUE), 1) else 0
  
  # Create 3 key summary sentences
  footnote <- paste0(
    "1.", reason_name, " Analysis: ", 
    n_distinct(prov_data$country), " countries in ", block_name, " block exceed ", 
    priority_threshold, "% threshold, with ", nrow(prov_data), " provinces and ", 
    nrow(dist_data), " districts identified.\n\n",
    
    "2. Top Contributors: ", top_province, " leads provinces (", top_province_pct, 
    "% of country total), while ", top_district, " leads districts (", top_district_pct, 
    "% of province total). Average province contribution: ", avg_prov_pct, 
    "%, average district contribution: ", avg_dist_pct, "%.\n\n",
    
    "3. Methodology: Applied ", if(dynamic_threshold) "Pareto (80% cumulative)" else "fixed", 
    " thresholds (Country ≥", priority_threshold, "%, Province ≥", province_threshold, 
    "%, District ≥", district_threshold, "%) for period: ", period_label, "."
  )
  
  return(footnote)
}

prepare_sankey <- function(prov_data, dist_data, reason) {
  # Create nodes: all unique countries, provinces, districts
  countries <- unique(prov_data$country)
  provinces <- unique(prov_data$province)
  districts <- unique(dist_data$district)
  
  nodes <- data.frame(
    name = c(countries, provinces, districts),
    group = c(rep("country", length(countries)),
              rep("province", length(provinces)),
              rep("district", length(districts))),
    stringsAsFactors = FALSE
  )
  nodes$id <- 0:(nrow(nodes)-1)
  
  # Links: country -> province using percentage
  links1 <- prov_data %>%
    left_join(nodes %>% select(id, name), by = c("country" = "name"), relationship = "many-to-many") %>%
    rename(source = id) %>%
    left_join(nodes %>% select(id, name), by = c("province" = "name"), relationship = "many-to-many") %>%
    rename(target = id) %>%
    mutate(value = prov_pct) %>%
    select(source, target, value)
  
  # Links: province -> district using percentage
  links2 <- dist_data %>%
    left_join(nodes %>% select(id, name), by = c("province" = "name"), relationship = "many-to-many") %>%
    rename(source = id) %>%
    left_join(nodes %>% select(id, name), by = c("district" = "name"), relationship = "many-to-many") %>%
    rename(target = id) %>%
    mutate(value = dist_pct) %>%
    select(source, target, value)
  
  links <- bind_rows(links1, links2)
  
  list(nodes = nodes, links = links, reason = reason)
}

# ============================================================
# MAIN FUNCTION
# ============================================================

reasons_heatmap_analysis <- function(data, x_months = NULL, specific_month = NULL, specific_year = NULL,
                                     block_selection = NULL, country_selection = NULL,
                                     province_selection = NULL, district_selection = NULL,
                                     afro_blocks = NULL, ist_blocks = NULL, block_type = "afro",
                                     geo_level = "country", viz_type = "auto",
                                     combined_types = c("traditional", "non_compliance", "absence"),
                                     
                                     # Priority Drill‑down Parameters
                                     priority_enabled = FALSE,
                                     priority_reason = c("childabsent", "non_compliance"),
                                     priority_threshold = 30,
                                     province_threshold = 10,
                                     district_threshold = 5,
                                     dynamic_threshold = FALSE) {
  
  # --------------------------
  # Select appropriate block definition
  # --------------------------
  if (block_type == "afro") {
    block_definition <- afro_blocks
  } else {
    block_definition <- ist_blocks
  }
  
  # Apply filters dynamically
  filtered_data <- data
  
  # Filter by block if specified
  if (!is.null(block_definition) && !is.null(block_selection) && block_selection != "All") {
    block_countries <- block_definition[[block_selection]]
    filtered_data <- filtered_data %>% dplyr::filter(country %in% block_countries)
  }
  
  # Filter by specific countries if specified
  if (!is.null(country_selection) && length(country_selection) > 0) {
    filtered_data <- filtered_data %>% dplyr::filter(country %in% country_selection)
  }
  
  # Filter by specific provinces if specified
  if (!is.null(province_selection) && length(province_selection) > 0) {
    filtered_data <- filtered_data %>% dplyr::filter(province %in% province_selection)
  }
  
  # Filter by specific districts if specified
  if (!is.null(district_selection) && length(district_selection) > 0) {
    filtered_data <- filtered_data %>% dplyr::filter(district %in% district_selection)
  }
  
  # ============================================================
  # Date parsing and filtering
  # ============================================================
  filtered_data <- filtered_data |> 
    mutate(date = tryCatch({
      as_date(round_start_date)
    }, warning = function(w) {
      if (is.character(round_start_date)) {
        as_date(parse_date_time(round_start_date, orders = c("ymd", "dmy", "mdy")))
      } else {
        as_date(round_start_date)
      }
    }, error = function(e) {
      NA_Date_
    })) %>%
    filter(!is.na(date))
  
  if(nrow(filtered_data) == 0) {
    return(list(plot = NULL, data = tibble(), summary_data = tibble(Note = "No data available after date parsing")))
  }
  
  # Date range filtering
  if (!is.null(x_months)) {
    latest_date <- max(filtered_data$date, na.rm = TRUE)
    if(is.infinite(latest_date)) {
      return(list(plot = NULL, data = tibble(), summary_data = tibble(Note = "No valid dates found")))
    }
    cutoff_date <- latest_date %m-% months(x_months)
    filtered_data <- filtered_data %>% dplyr::filter(date >= cutoff_date)
    period_label <- paste0("Last ", x_months, " Months")
    
  } else if (!is.null(specific_month) & !is.null(specific_year)) {
    target_date <- lubridate::make_date(specific_year, specific_month, 1)
    end_date <- target_date %m+% months(1) - lubridate::days(1)
    filtered_data <- filtered_data %>%
      dplyr::filter(date >= target_date & date <= end_date)
    period_label <- paste(month.name[specific_month], specific_year)
  } else {
    period_label <- "All Time"
  }
  
  if (nrow(filtered_data) == 0) {
    return(list(plot = NULL, data = tibble(), summary_data = tibble(Note = "No data available for selected filters")))
  }
  
  # ============================================================
  # SET GEOGRAPHIC LEVEL FOR ANALYSIS
  # ============================================================
  geo_col <- switch(geo_level,
                    "country" = "country",
                    "province" = "province",
                    "district" = "district",
                    "country")
  
  if (!geo_col %in% names(filtered_data)) {
    warning(paste("Geographic column", geo_col, "not found. Falling back to country level."))
    geo_col <- "country"
    geo_level <- "country"
  }
  
  filtered_data <- filtered_data %>% filter(!is.na(!!sym(geo_col)))
  
  geo_display <- case_when(
    geo_level == "country" ~ "Country",
    geo_level == "province" ~ "Province",
    geo_level == "district" ~ "District"
  )
  
  unique_geo_units <- n_distinct(filtered_data[[geo_col]])
  
  # ============================================================
  # IMPROVED HELPER FUNCTIONS FOR HIGH-QUALITY VISUALIZATIONS
  # ============================================================
  
  # Base theme for consistent high-quality look – LARGER TEXT
  base_theme <- theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray30"),
      # Make axis text LARGER
      axis.text = element_text(size = 16, color = "gray3"),
      axis.title = element_text(size = 18, color = "gray3"),
      # Axis lines (will be overridden in individual functions)
      axis.line = element_line(linewidth = 0.8, color = "gray30"),
      axis.ticks = element_line(linewidth = 0.8, color = "gray30"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      strip.background = element_rect(fill = "#2c3e50", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  create_pie_chart <- function(data, title, subtitle) {
    data %>%
      group_by(reasons) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      ggplot(aes(x = "", y = value, fill = reasons)) +
      geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.3) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = title, subtitle = subtitle, fill = "Reason") +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_donut_chart <- function(data, title, subtitle) {
    data %>%
      group_by(reasons) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      mutate(ymax = cumsum(value), ymin = c(0, head(ymax, n = -1))) %>%
      ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = reasons)) +
      geom_rect(color = "white", linewidth = 0.3) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = title, subtitle = subtitle, fill = "Reason") +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_treemap <- function(data, title, subtitle) {
    if (!requireNamespace("treemapify", quietly = TRUE)) {
      warning("treemapify not installed. Falling back to bar chart.")
      return(create_bar_chart(data, title, subtitle))
    }
    data %>%
      group_by(reasons) %>%
      summarise(value = mean(value, na.rm = TRUE)) %>%
      ggplot(aes(area = value, fill = reasons, label = reasons)) +
      treemapify::geom_treemap(color = "white", linewidth = 0.5) +
      treemapify::geom_treemap_text(colour = "white", place = "centre", size = 16) +
      scale_fill_brewer(palette = "Set3", guide = "none") +
      labs(title = title, subtitle = subtitle) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30")
      )
  }
  
  create_sunburst <- function(data, title, subtitle) {
    # Simplified sunburst using treemap
    warning("Sunburst chart requires plotly/ggsunburst. Showing treemap instead.")
    create_treemap(data, title, paste(subtitle, "(treemap approximation)"))
  }
  
  create_radar <- function(data, title, subtitle) {
    if (!requireNamespace("ggradar", quietly = TRUE)) {
      warning("ggradar not installed. Falling back to bar chart.")
      return(create_bar_chart(data, title, subtitle))
    }
    radar_data <- data %>%
      group_by(reasons, geo_unit) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = reasons, values_from = value, values_fill = 0)
    
    ggradar::ggradar(radar_data, 
                     grid.label.size = 7, 
                     axis.label.size = 6,
                     group.point.size = 4,
                     group.line.width = 1.2,
                     background.circle.colour = "gray90",
                     gridline.min.colour = "gray70",
                     gridline.mid.colour = "gray70",
                     gridline.max.colour = "gray70") +
      labs(title = title, subtitle = subtitle) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 16, hjust = 0.5, color = "gray30"),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_lollipop <- function(data, title, subtitle) {
    data %>%
      group_by(reasons) %>%
      summarise(avg = mean(value, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(reasons, avg), y = avg)) +
      geom_segment(aes(xend = reasons, yend = 0), color = "#2c7fb8", linewidth = 1.2) +
      geom_point(size = 5, color = "#253494") +
      coord_flip() +
      labs(title = title, subtitle = subtitle, x = NULL, y = "Average Percentage (%)") +
      base_theme +
      theme(
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 10)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank()
      )
  }
  
  create_waterfall <- function(data, title, subtitle) {
    data %>%
      group_by(reasons) %>%
      summarise(avg = mean(value, na.rm = TRUE)) %>%
      arrange(desc(avg)) %>%
      mutate(cumulative = cumsum(avg),
             id = row_number()) %>%
      ggplot(aes(x = reorder(reasons, -avg), y = avg, fill = reasons)) +
      geom_col(show.legend = FALSE) +
      geom_line(aes(x = id, y = cumulative), color = "#d73027", linewidth = 1.2) +
      geom_point(aes(x = id, y = cumulative), color = "#d73027", size = 4) +
      labs(title = title, subtitle = subtitle, x = NULL, y = "Percentage (%)") +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 18, margin = margin(r = 10)),
        axis.line = element_blank(),
        axis.ticks = element_blank()
      )
  }
  
  create_heatmap <- function(data, title, subtitle) {
    # Adjust text size based on number of cells
    n_rows <- n_distinct(data$reasons)
    n_cols <- n_distinct(data$geo_unit)
    text_size <- ifelse(n_rows * n_cols > 100, 5, 6)
    
    # Add a column for text color based on fill intensity (threshold ~50%)
    data <- data %>%
      mutate(text_color = ifelse(value < 50, "black", "white"))
    
    ggplot(data, aes(x = geo_unit, y = reasons, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f", value), color = text_color), 
                size = text_size, fontface = "bold", show.legend = FALSE) +
      scale_color_identity() +
      scale_fill_gradientn(colors = c("#f7fbff", "#6baed6", "#08306b"),
                           name = "Percentage (%)",
                           na.value = "grey90") +
      labs(title = title, subtitle = subtitle, x = geo_display, y = NULL) +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 12)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_bar_chart <- function(data, title, subtitle) {
    ggplot(data, aes(x = geo_unit, y = value, fill = reasons)) +
      geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.2) +
      scale_fill_brewer(palette = "Set3", name = "Reason") +
      labs(title = title, subtitle = subtitle, x = geo_display, y = "Percentage (%)") +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 12)),
        axis.title.y = element_text(size = 18, margin = margin(r = 12)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_stacked_bar <- function(data, title, subtitle) {
    ggplot(data, aes(x = geo_unit, y = value, fill = reasons)) +
      geom_col(position = "stack", width = 0.7, color = "white", linewidth = 0.2) +
      scale_fill_brewer(palette = "Set3", name = "Reason") +
      labs(title = title, subtitle = subtitle, x = geo_display, y = "Percentage (%)") +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 12)),
        axis.title.y = element_text(size = 18, margin = margin(r = 12)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  create_faceted_bar <- function(data, title, subtitle) {
    ggplot(data, aes(x = geo_unit, y = value, fill = reasons)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.2) +
      facet_wrap(~reasons, scales = "free_y", ncol = 2) +
      scale_fill_brewer(palette = "Set3", guide = "none") +
      labs(title = title, subtitle = subtitle, x = geo_display, y = "Percentage (%)") +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, margin = margin(t = 12)),
        axis.title.y = element_text(size = 18, margin = margin(r = 12)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(face = "bold", size = 16)
      )
  }
  
  create_bubble <- function(data, title, subtitle) {
    ggplot(data, aes(x = geo_unit, y = reasons, size = value, color = value)) +
      geom_point(alpha = 0.8) +
      scale_size_continuous(range = c(4, 16), name = "Percentage (%)") +
      scale_color_gradient(low = "#2c7fb8", high = "#d73027", name = "Percentage (%)") +
      labs(title = title, subtitle = subtitle, x = geo_display, y = NULL) +
      base_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, margin = margin(t = 12)),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15, face = "bold")
      )
  }
  
  # ============================================================
  # 1. ABSENCE REASONS (abs_reason_*)
  # ============================================================
  absence_reason_columns <- grep("^abs_reason_", names(filtered_data), value = TRUE)
  existing_absence_columns <- absence_reason_columns[absence_reason_columns %in% names(filtered_data)]
  
  absence_result <- NULL
  if (length(existing_absence_columns) > 0) {
    absence_dat <- filtered_data %>%
      dplyr::group_by(!!sym(geo_col)) %>%
      dplyr::summarise(across(all_of(existing_absence_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::mutate(total_absence = rowSums(dplyr::select(., all_of(existing_absence_columns)), na.rm = TRUE)) %>%
      dplyr::filter(total_absence > 0)
    
    if (nrow(absence_dat) > 0) {
      absence_dat <- absence_dat %>% rename(geo_unit = !!sym(geo_col))
      absence_raw <- absence_dat
      absence_dat_pct <- absence_dat %>%
        dplyr::mutate(across(all_of(existing_absence_columns), ~ (.x / total_absence) * 100)) %>%
        dplyr::select(-total_absence)
      
      absence_dh <- absence_dat_pct %>%
        tidyr::pivot_longer(cols = all_of(existing_absence_columns), names_to = "reasons", values_to = "value") %>%
        dplyr::mutate(
          reasons = gsub("abs_reason_", "", reasons),
          reasons = gsub("_", " ", reasons),
          reasons = stringr::str_to_sentence(reasons),
          geo_unit = factor(geo_unit, levels = unique(geo_unit))
        ) %>%
        dplyr::filter(!is.na(value) & value > 0)
      
      title <- paste("Absence Reasons -", geo_display, "Level -", ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks"))
      subtitle <- period_label
      if (!is.null(block_selection) && block_selection != "All") subtitle <- paste(subtitle, "| Block:", block_selection)
      if (!is.null(country_selection) && length(country_selection) > 0) subtitle <- paste(subtitle, "| Countries:", paste(country_selection, collapse = ", "))
      
      absence_plot <- switch(viz_type,
                             "heatmap" = create_heatmap(absence_dh, title, subtitle),
                             "bar" = create_bar_chart(absence_dh, title, subtitle),
                             "stacked_bar" = create_stacked_bar(absence_dh, title, subtitle),
                             "faceted_bar" = create_faceted_bar(absence_dh, title, subtitle),
                             "bubble" = create_bubble(absence_dh, title, subtitle),
                             "pie" = create_pie_chart(absence_dh, title, subtitle),
                             "donut" = create_donut_chart(absence_dh, title, subtitle),
                             "treemap" = create_treemap(absence_dh, title, subtitle),
                             "sunburst" = create_sunburst(absence_dh, title, subtitle),
                             "radar" = create_radar(absence_dh, title, subtitle),
                             "lollipop" = create_lollipop(absence_dh, title, subtitle),
                             "waterfall" = create_waterfall(absence_dh, title, subtitle),
                             "auto" = {
                               if (unique_geo_units <= 8) create_pie_chart(absence_dh, title, subtitle)
                               else if (unique_geo_units <= 20) create_heatmap(absence_dh, title, subtitle)
                               else create_bar_chart(absence_dh, title, subtitle)
                             }
      )
      
      absence_metrics <- list(
        total_units = n_distinct(absence_dh$geo_unit),
        most_common = absence_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          slice_max(avg, n = 1) %>% pull(reasons) %>% .[1],
        avg_percentage = round(mean(absence_dh$value, na.rm = TRUE), 1),
        max_percentage = round(max(absence_dh$value, na.rm = TRUE), 1),
        min_percentage = round(min(absence_dh$value, na.rm = TRUE), 1),
        total_categories = n_distinct(absence_dh$reasons),
        top_5 = absence_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          arrange(desc(avg)) %>%
          head(5) %>% pull(reasons)
      )
      
      absence_result <- list(
        plot = absence_plot,
        data_pct = absence_dat_pct,
        data_raw = absence_raw,
        long_data = absence_dh,
        metrics = absence_metrics,
        viz_type = viz_type
      )
    }
  }
  
  # ============================================================
  # 2. NON-COMPLIANCE REASONS (nc_reason_*)
  # ============================================================
  nc_reason_columns <- grep("^nc_reason_", names(filtered_data), value = TRUE)
  existing_nc_columns <- nc_reason_columns[nc_reason_columns %in% names(filtered_data)]
  
  nc_result <- NULL
  if (length(existing_nc_columns) > 0) {
    nc_dat <- filtered_data %>%
      dplyr::group_by(!!sym(geo_col)) %>%
      dplyr::summarise(across(all_of(existing_nc_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::mutate(total_nc = rowSums(dplyr::select(., all_of(existing_nc_columns)), na.rm = TRUE)) %>%
      dplyr::filter(total_nc > 0)
    
    if (nrow(nc_dat) > 0) {
      nc_dat <- nc_dat %>% rename(geo_unit = !!sym(geo_col))
      nc_raw <- nc_dat
      nc_dat_pct <- nc_dat %>%
        dplyr::mutate(across(all_of(existing_nc_columns), ~ (.x / total_nc) * 100)) %>%
        dplyr::select(-total_nc)
      
      nc_dh <- nc_dat_pct %>%
        tidyr::pivot_longer(cols = all_of(existing_nc_columns), names_to = "reasons", values_to = "value") %>%
        dplyr::mutate(
          reasons = gsub("nc_reason_", "", reasons),
          reasons = gsub("_", " ", reasons),
          reasons = stringr::str_to_sentence(reasons),
          geo_unit = factor(geo_unit, levels = unique(geo_unit))
        ) %>%
        dplyr::filter(!is.na(value) & value > 0)
      
      title <- paste("Non-Compliance Reasons -", geo_display, "Level -", ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks"))
      subtitle <- period_label
      if (!is.null(block_selection) && block_selection != "All") subtitle <- paste(subtitle, "| Block:", block_selection)
      if (!is.null(country_selection) && length(country_selection) > 0) subtitle <- paste(subtitle, "| Countries:", paste(country_selection, collapse = ", "))
      
      nc_plot <- switch(viz_type,
                        "heatmap" = create_heatmap(nc_dh, title, subtitle),
                        "bar" = create_bar_chart(nc_dh, title, subtitle),
                        "stacked_bar" = create_stacked_bar(nc_dh, title, subtitle),
                        "faceted_bar" = create_faceted_bar(nc_dh, title, subtitle),
                        "bubble" = create_bubble(nc_dh, title, subtitle),
                        "pie" = create_pie_chart(nc_dh, title, subtitle),
                        "donut" = create_donut_chart(nc_dh, title, subtitle),
                        "treemap" = create_treemap(nc_dh, title, subtitle),
                        "sunburst" = create_sunburst(nc_dh, title, subtitle),
                        "radar" = create_radar(nc_dh, title, subtitle),
                        "lollipop" = create_lollipop(nc_dh, title, subtitle),
                        "waterfall" = create_waterfall(nc_dh, title, subtitle),
                        "auto" = {
                          if (unique_geo_units <= 8) create_pie_chart(nc_dh, title, subtitle)
                          else if (unique_geo_units <= 20) create_heatmap(nc_dh, title, subtitle)
                          else create_bar_chart(nc_dh, title, subtitle)
                        }
      )
      
      nc_metrics <- list(
        total_units = n_distinct(nc_dh$geo_unit),
        most_common = nc_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          slice_max(avg, n = 1) %>% pull(reasons) %>% .[1],
        avg_percentage = round(mean(nc_dh$value, na.rm = TRUE), 1),
        max_percentage = round(max(nc_dh$value, na.rm = TRUE), 1),
        min_percentage = round(min(nc_dh$value, na.rm = TRUE), 1),
        total_categories = n_distinct(nc_dh$reasons),
        top_5 = nc_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          arrange(desc(avg)) %>%
          head(5) %>% pull(reasons)
      )
      
      nc_result <- list(
        plot = nc_plot,
        data_pct = nc_dat_pct,
        data_raw = nc_raw,
        long_data = nc_dh,
        metrics = nc_metrics,
        viz_type = viz_type
      )
    }
  }
  
  # ============================================================
  # 3. TRADITIONAL REASONS
  # ============================================================
  reason_columns <- c(
    "r_non_compliance", "r_childabsent", "r_child_is_a_visitor",
    "r_child_was_asleep", "r_house_not_visited", "r_vaccinated_but_not_FM",
    "r_childnotborn", "r_security", "other_r"
  )
  existing_reason_columns <- reason_columns[reason_columns %in% names(filtered_data)]
  
  traditional_result <- NULL
  if (length(existing_reason_columns) > 0) {
    trad_dat <- filtered_data %>%
      dplyr::group_by(!!sym(geo_col)) %>%
      dplyr::summarise(across(all_of(existing_reason_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::mutate(total_reasons = rowSums(dplyr::select(., all_of(existing_reason_columns)), na.rm = TRUE)) %>%
      dplyr::filter(total_reasons > 0)
    
    if (nrow(trad_dat) > 0) {
      trad_dat <- trad_dat %>% rename(geo_unit = !!sym(geo_col))
      trad_raw <- trad_dat
      trad_dat_pct <- trad_dat %>%
        dplyr::mutate(across(all_of(existing_reason_columns), ~ (.x / total_reasons) * 100)) %>%
        dplyr::select(-total_reasons)
      
      trad_dh <- trad_dat_pct %>%
        tidyr::pivot_longer(cols = all_of(existing_reason_columns), names_to = "reasons", values_to = "value") %>%
        dplyr::mutate(
          reasons = dplyr::recode(reasons,
                                  r_non_compliance = "Non compliance",
                                  r_childabsent = "Child absent",
                                  r_child_is_a_visitor = "Child is a visitor",
                                  r_child_was_asleep = "Child was asleep",
                                  r_house_not_visited = "House not visited",
                                  r_vaccinated_but_not_FM = "Vaccinated but not FM",
                                  r_childnotborn = "Child not born",
                                  r_security = "Security",
                                  other_r = "Other"),
          geo_unit = factor(geo_unit, levels = unique(geo_unit))
        ) %>%
        dplyr::filter(!is.na(value) & value > 0)
      
      title <- paste("Traditional Reasons -", geo_display, "Level -", ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks"))
      subtitle <- period_label
      if (!is.null(block_selection) && block_selection != "All") subtitle <- paste(subtitle, "| Block:", block_selection)
      if (!is.null(country_selection) && length(country_selection) > 0) subtitle <- paste(subtitle, "| Countries:", paste(country_selection, collapse = ", "))
      
      traditional_plot <- switch(viz_type,
                                 "heatmap" = create_heatmap(trad_dh, title, subtitle),
                                 "bar" = create_bar_chart(trad_dh, title, subtitle),
                                 "stacked_bar" = create_stacked_bar(trad_dh, title, subtitle),
                                 "faceted_bar" = create_faceted_bar(trad_dh, title, subtitle),
                                 "bubble" = create_bubble(trad_dh, title, subtitle),
                                 "pie" = create_pie_chart(trad_dh, title, subtitle),
                                 "donut" = create_donut_chart(trad_dh, title, subtitle),
                                 "treemap" = create_treemap(trad_dh, title, subtitle),
                                 "sunburst" = create_sunburst(trad_dh, title, subtitle),
                                 "radar" = create_radar(trad_dh, title, subtitle),
                                 "lollipop" = create_lollipop(trad_dh, title, subtitle),
                                 "waterfall" = create_waterfall(trad_dh, title, subtitle),
                                 "auto" = {
                                   if (unique_geo_units <= 8) create_pie_chart(trad_dh, title, subtitle)
                                   else if (unique_geo_units <= 20) create_heatmap(trad_dh, title, subtitle)
                                   else create_bar_chart(trad_dh, title, subtitle)
                                 }
      )
      
      traditional_metrics <- list(
        total_units = n_distinct(trad_dh$geo_unit),
        most_common = trad_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          slice_max(avg, n = 1) %>% pull(reasons) %>% .[1],
        avg_percentage = round(mean(trad_dh$value, na.rm = TRUE), 1),
        max_percentage = round(max(trad_dh$value, na.rm = TRUE), 1),
        min_percentage = round(min(trad_dh$value, na.rm = TRUE), 1),
        total_categories = n_distinct(trad_dh$reasons),
        top_5 = trad_dh %>%
          group_by(reasons) %>%
          summarise(avg = mean(value, na.rm = TRUE)) %>%
          arrange(desc(avg)) %>%
          head(5) %>% pull(reasons)
      )
      
      traditional_result <- list(
        plot = traditional_plot,
        data_pct = trad_dat_pct,
        data_raw = trad_raw,
        long_data = trad_dh,
        metrics = traditional_metrics,
        viz_type = viz_type
      )
    }
  }
  
  # ============================================================
  # COMBINED DATA – FULL (all three types, for summaries)
  # ============================================================
  full_long <- bind_rows(
    if (!is.null(absence_result$long_data)) absence_result$long_data %>% mutate(reason_type = "Absence Reasons") else NULL,
    if (!is.null(nc_result$long_data)) nc_result$long_data %>% mutate(reason_type = "Non-Compliance Reasons") else NULL,
    if (!is.null(traditional_result$long_data)) traditional_result$long_data %>% mutate(reason_type = "Traditional Reasons") else NULL
  )
  
  # ============================================================
  # COMBINED DATA – FILTERED (according to combined_types, for combined plot)
  # ============================================================
  combined_long <- bind_rows(
    if (!is.null(absence_result$long_data) && "absence" %in% combined_types) absence_result$long_data %>% mutate(reason_type = "Absence Reasons") else NULL,
    if (!is.null(nc_result$long_data) && "non_compliance" %in% combined_types) nc_result$long_data %>% mutate(reason_type = "Non-Compliance Reasons") else NULL,
    if (!is.null(traditional_result$long_data) && "traditional" %in% combined_types) traditional_result$long_data %>% mutate(reason_type = "Traditional Reasons") else NULL
  )
  
  # ============================================================
  # COMBINED FACETED PLOT (Filtered reason types) - WITH AXIS LINES KEPT
  # ============================================================
  combined_plot <- NULL
  if (nrow(combined_long) > 0) {
    block_type_text <- ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks")
    geo_info <- ""
    if (!is.null(block_selection) && block_selection != "All") geo_info <- paste0(" | Block: ", block_selection)
    if (!is.null(country_selection) && length(country_selection) > 0) geo_info <- paste0(geo_info, " | Countries: ", paste(country_selection, collapse = ", "))
    
    if (viz_type == "heatmap") {
      # Force heatmap version regardless of unit count
      combined_long <- combined_long %>%
        mutate(text_color = ifelse(value < 50, "black", "white"))
      
      n_rows <- n_distinct(combined_long$reasons)
      n_cols <- n_distinct(combined_long$geo_unit)
      text_size <- ifelse(n_rows * n_cols > 100, 5, 6)
      
      combined_plot <- ggplot(combined_long, aes(x = geo_unit, y = reasons, fill = value)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = sprintf("%.1f", value), color = text_color), 
                  size = text_size, fontface = "bold", show.legend = FALSE) +
        scale_color_identity() +
        scale_fill_gradientn(colors = c("#f7fbff", "#6baed6", "#08306b"),
                             name = "Percentage (%)") +
        facet_grid(reason_type ~ ., scales = "free_y", space = "free_y",
                   labeller = label_wrap_gen(width = 10)) +
        labs(title = paste("Comprehensive Reasons Analysis -", geo_display, "Level -", block_type_text),
             subtitle = paste(period_label, geo_info),
             x = geo_display, y = NULL) +
        theme_minimal(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 10)),
          axis.line = element_line(color = "grey70", linewidth = 0.5),
          axis.ticks = element_line(color = "grey70", linewidth = 0.5),
          strip.text = element_text(face = "bold", size = 12, color = "white", lineheight = 0.85),
          strip.background = element_rect(fill = "#2c3e50", color = NA),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14, face = "bold")
        )
      
    } else if (viz_type %in% c("bar", "stacked_bar", "faceted_bar")) {
      # Force bar/dodge version
      combined_plot <- ggplot(combined_long, aes(x = geo_unit, y = value, fill = reasons)) +
        geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.2) +
        facet_wrap(~reason_type, scales = "free_y", ncol = 1) +
        scale_fill_brewer(palette = "Set3", name = "Reason") +
        labs(title = paste("Comprehensive Reasons Analysis -", geo_display, "Level -", block_type_text),
             subtitle = paste(period_label, geo_info, "| Bar Chart View"),
             x = geo_display, y = "Percentage (%)") +
        base_theme +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 10)),
          axis.title.y = element_text(size = 16, margin = margin(r = 10)),
          axis.line = element_line(color = "grey70", linewidth = 0.5),
          axis.ticks = element_line(color = "grey70", linewidth = 0.5),
          strip.text = element_text(face = "bold", size = 16, color = "white"),
          strip.background = element_rect(fill = "#2c3e50", color = NA),
          legend.position = "right",
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14, face = "bold")
        )
      
    } else {
      # Auto mode: choose based on number of units
      if (unique_geo_units > 15) {
        combined_plot <- ggplot(combined_long, aes(x = geo_unit, y = value, fill = reasons)) +
          geom_col(position = "dodge", width = 0.7, color = "white", linewidth = 0.2) +
          facet_wrap(~reason_type, scales = "free_y", ncol = 1) +
          scale_fill_brewer(palette = "Set3", name = "Reason") +
          labs(title = paste("Comprehensive Reasons Analysis -", geo_display, "Level -", block_type_text),
               subtitle = paste(period_label, geo_info, "| Bar Chart View"),
               x = geo_display, y = "Percentage (%)") +
          base_theme +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16, margin = margin(t = 10)),
            axis.title.y = element_text(size = 16, margin = margin(r = 10)),
            axis.line = element_line(color = "grey70", linewidth = 0.5),
            axis.ticks = element_line(color = "grey70", linewidth = 0.5),
            strip.text = element_text(face = "bold", size = 16, color = "white"),
            strip.background = element_rect(fill = "#2c3e50", color = NA),
            legend.position = "right",
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14, face = "bold")
          )
      } else {
        combined_long <- combined_long %>%
          mutate(text_color = ifelse(value < 50, "black", "white"))
        
        n_rows <- n_distinct(combined_long$reasons)
        n_cols <- n_distinct(combined_long$geo_unit)
        text_size <- ifelse(n_rows * n_cols > 100, 3.5, 4.5)
        
        combined_plot <- ggplot(combined_long, aes(x = geo_unit, y = reasons, fill = value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = sprintf("%.1f", value), color = text_color), 
                    size = text_size, fontface = "bold", show.legend = FALSE) +
          scale_color_identity() +
          scale_fill_gradientn(colors = c("#f7fbff", "#6baed6", "#08306b"),
                               name = "Percentage (%)") +
          facet_grid(reason_type ~ ., scales = "free_y", space = "free_y",
                     labeller = label_wrap_gen(width = 10)) +
          labs(title = paste("Comprehensive Reasons Analysis -", geo_display, "Level -", block_type_text),
               subtitle = paste(period_label, geo_info),
               x = geo_display, y = NULL) +
          base_theme +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16, margin = margin(t = 10)),
            axis.line = element_line(color = "grey70", linewidth = 0.5),
            axis.ticks = element_line(color = "grey70", linewidth = 0.5),
            strip.text = element_text(face = "bold", size = 12, color = "white", lineheight = 0.85),
            strip.background = element_rect(fill = "#2c3e50", color = NA),
            legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14, face = "bold")
          )
      }
    }
  }
  
  # ============================================================
  # TOP REASONS SUMMARY (based on full data)
  # ============================================================
  top_reasons_summary <- NULL
  if (nrow(full_long) > 0) {
    top_reasons_summary <- full_long %>%
      group_by(reason_type, reasons) %>%
      summarise(
        avg_percentage = mean(value, na.rm = TRUE),
        max_percentage = max(value, na.rm = TRUE),
        min_percentage = min(value, na.rm = TRUE),
        units_with_data = n_distinct(geo_unit),
        .groups = "drop"
      ) %>%
      arrange(reason_type, desc(avg_percentage))
  }
  
  # ============================================================
  # HIERARCHICAL SUMMARY (always compute if country, province, district exist)
  # ============================================================
  hierarchical_summary <- NULL
  if ("province" %in% names(filtered_data) && "district" %in% names(filtered_data)) {
    if (length(existing_absence_columns) > 0) {
      absence_hier <- filtered_data %>%
        group_by(country, province, district) %>%
        summarise(across(all_of(existing_absence_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(total_absence = rowSums(select(., all_of(existing_absence_columns)), na.rm = TRUE)) %>%
        filter(total_absence > 0)
      if (nrow(absence_hier) > 0) hierarchical_summary$absence <- absence_hier
    }
    if (length(existing_nc_columns) > 0) {
      nc_hier <- filtered_data %>%
        group_by(country, province, district) %>%
        summarise(across(all_of(existing_nc_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(total_nc = rowSums(select(., all_of(existing_nc_columns)), na.rm = TRUE)) %>%
        filter(total_nc > 0)
      if (nrow(nc_hier) > 0) hierarchical_summary$non_compliance <- nc_hier
    }
    if (length(existing_reason_columns) > 0) {
      trad_hier <- filtered_data %>%
        group_by(country, province, district) %>%
        summarise(across(all_of(existing_reason_columns), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(total_reasons = rowSums(select(., all_of(existing_reason_columns)), na.rm = TRUE)) %>%
        filter(total_reasons > 0)
      if (nrow(trad_hier) > 0) hierarchical_summary$traditional <- trad_hier
    }
  }
  
  # ============================================================
  # 🆕 ENHANCED PRIORITY COUNTRY DRILL‑DOWN with Trends & Summary
  # ============================================================
  drilldown <- NULL
  if (priority_enabled && geo_level == "country" && !is.null(traditional_result$data_raw)) {
    
    # Get traditional reason counts per country
    trad_country <- traditional_result$data_raw %>%
      rename(country = geo_unit) %>%
      mutate(
        total = rowSums(across(all_of(existing_reason_columns))),
        pct_childabsent = r_childabsent / total * 100,
        pct_non_compliance = r_non_compliance / total * 100
      )
    
    # Identify priority countries for each reason
    priority_list <- list()
    for (reason in priority_reason) {
      col_pct <- if (reason == "childabsent") "pct_childabsent" else "pct_non_compliance"
      col_count <- if (reason == "childabsent") "r_childabsent" else "r_non_compliance"
      
      priority_countries <- trad_country %>%
        filter(!!sym(col_pct) >= priority_threshold) %>%
        pull(country)
      
      if (length(priority_countries) == 0) next
      
      # Get hierarchical data for this reason
      hier <- hierarchical_summary$traditional %>%
        filter(country %in% priority_countries) %>%
        select(country, province, district, !!sym(col_count))
      
      if (nrow(hier) == 0) next
      
      # Compute province contributions
      prov_data <- hier %>%
        group_by(country, province) %>%
        summarise(province_total = sum(!!sym(col_count), na.rm = TRUE), .groups = "drop") %>%
        group_by(country) %>%
        mutate(
          country_total = sum(province_total),
          prov_pct = province_total / country_total * 100
        ) %>%
        ungroup()
      
      # Apply province threshold
      if (dynamic_threshold) {
        prov_data <- prov_data %>%
          arrange(country, desc(province_total)) %>%
          group_by(country) %>%
          mutate(
            cum_pct = cumsum(province_total) / sum(province_total) * 100,
            keep = cum_pct <= 80 | (cum_pct > 80 & lag(cum_pct, default = 0) < 80)
          ) %>%
          filter(keep) %>%
          select(-cum_pct, -keep)
      } else {
        prov_data <- prov_data %>% filter(prov_pct >= province_threshold)
      }
      
      # Get selected provinces
      selected_provinces <- prov_data$province
      
      # Compute district contributions for selected provinces
      dist_data <- hier %>%
        filter(province %in% selected_provinces) %>%
        group_by(country, province, district) %>%
        summarise(district_total = sum(!!sym(col_count), na.rm = TRUE), .groups = "drop") %>%
        group_by(country, province) %>%
        mutate(
          prov_total = sum(district_total),
          dist_pct = district_total / prov_total * 100
        ) %>%
        ungroup()
      
      if (dynamic_threshold) {
        dist_data <- dist_data %>%
          arrange(country, province, desc(district_total)) %>%
          group_by(country, province) %>%
          mutate(
            cum_pct = cumsum(district_total) / sum(district_total) * 100,
            keep = cum_pct <= 80 | (cum_pct > 80 & lag(cum_pct, default = 0) < 80)
          ) %>%
          filter(keep) %>%
          select(-cum_pct, -keep)
      } else {
        dist_data <- dist_data %>% filter(dist_pct >= district_threshold)
      }
      
      # Get selected districts for trend analysis
      selected_districts <- dist_data$district
      
      # ============================================================
      # 🔍 TREND ANALYSIS for selected districts
      # ============================================================
      trend_analysis <- NULL
      if (length(selected_districts) > 0) {
        trend_analysis <- analyze_district_trends(
          data = filtered_data,
          districts_list = selected_districts,
          reason_col = col_count,
          time_unit = "month"
        )
      }
      
      # Prepare Sankey data
      sankey <- prepare_sankey(prov_data, dist_data, reason)
      
      priority_list[[reason]] <- list(
        provinces = prov_data,
        districts = dist_data,
        sankey = sankey,
        priority_countries = priority_countries,
        trend_analysis = trend_analysis
      )
    }
    
    if (length(priority_list) > 0) {
      drilldown <- priority_list
    }
  }
  
  # ============================================================
  # RETURN RESULTS
  # ============================================================
  list(
    geo_level = geo_level,
    geo_display = geo_display,
    geo_col = geo_col,
    unique_geo_units = unique_geo_units,
    viz_type = viz_type,
    combined_plot = combined_plot,
    absence = absence_result,
    non_compliance = nc_result,
    traditional = traditional_result,
    data = list(
      absence = if (!is.null(absence_result$data_pct)) absence_result$data_pct else NULL,
      non_compliance = if (!is.null(nc_result$data_pct)) nc_result$data_pct else NULL,
      traditional = if (!is.null(traditional_result$data_pct)) traditional_result$data_pct else NULL,
      all_long = full_long,
      combined_long = combined_long
    ),
    top_reasons_summary = top_reasons_summary,
    hierarchical = hierarchical_summary,
    drilldown = drilldown,
    metrics = list(
      period_label = period_label,
      block_type_text = ifelse(block_type == "afro", "AFRO Blocks", "IST Blocks"),
      geo_info = geo_info,
      block_selection = block_selection,
      country_selection = country_selection,
      province_selection = province_selection,
      district_selection = district_selection
    )
  )
}





# NOTE: the block previously here re-loaded `dat`, `scope`, and the three
# shapefiles with raw readRDS() calls (no column pruning / preprocessing),
# silently overwriting the already-optimized versions loaded earlier in this
# file, and redefined afro_blocks() without the update_blocks_with_actual_data()
# enhancement applied at load time. It also repeated the same three
# `options()`/deployment-config blocks with no effect. Removed as dead/harmful
# duplication -- the real loads and afro_blocks definition are earlier in this file.

# Handle font issues
if (!"Lato" %in% extrafont::fonts()) {
  message("Lato font not found - using default font")
  # Set a default font in your theme
}

# dashboardHeader(
#   title = tags$div(
#     tags$span("WHO AFRO SIA Dashboard"),
#     tags$span("WHO AFRO SIA Dashboard",
#               style = "font-weight: bold; font-size: 18px; color: #0072BC;"),
#     tags$span(
#       paste("v1.0 |", format(Sys.Date(), "%d %b %Y")),
#       style = "margin-left: 12px; font-size: 12px; color: #666666;"
#     )
#   ),



# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  dashboardHeader(
    title = paste(
      "WHO AFRO SIA Dashboard", DASHBOARD_VERSION, "|", DASHBOARD_DATE
    ),
    titleWidth = 300,
    
    # 🔘 DARK MODE TOGGLE (top right, before Refresh) — DEFAULT ON
    tags$li(
      class = "dropdown",
      style = "padding: 8px 5px 8px 10px;",
      div(
        class = "dark-mode-toggle",
        tags$span(icon("moon"), style = "margin-right: 4px;"),
        tags$span("Dark", style = "font-size: 12px; margin-right: 6px;"),
        checkboxInput(
          "dark_mode_toggle",
          label = NULL,
          value = TRUE,   # default ON – overridden by localStorage if present
          width = "20px"
        )
      )
    ),
    
    # 🔄 REFRESH BUTTON
    tags$li(
      class = "dropdown",
      style = "padding: 8px 15px;",
      actionButton(
        "refresh_data",
        "🔄 Refresh",
        class  = "btn-primary",
        style  = "background-color: #28a745; border: none; padding: 6px 12px;"
      )
    )
  ),
  
  
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      # ============================================================
      # 🟦 WHO AFRO LOGO HEADER (Home Button + Visual Feedback)
      # ============================================================
      tags$div(
        id = "whoHomeHeader",
        style = "
      text-align: center; 
      padding: 15px 10px 10px 10px; 
      background-color: #f8f9fa; 
      border-bottom: 1px solid #dee2e6;
      cursor: pointer;
      transition: box-shadow 0.3s ease;
    ",
        
        # JS trigger for home navigation + animation
        onclick = "
      this.classList.add('clicked-home');
      setTimeout(() => this.classList.remove('clicked-home'), 300);
      Shiny.setInputValue('go_home', Math.random());
    ",
        
        tags$img(
          src = 'who_afro_logo.png',
          height = '60px',
          style = 'margin-bottom: 5px;'
        ),
        
        tags$h4(
          'WHO AFRO – SIA Dashboard',
          style = '
        font-weight: bold; 
        color: #0072B2; 
        font-size: 15px; 
        margin: 5px 0;
      '
        ),
        
        tags$p(
          paste(DASHBOARD_VERSION, '| Updated', DASHBOARD_DATE),
          style = '
        font-size: 12px; 
        color: #6c757d; 
        margin-top: -5px;
        margin-bottom: 0;
      '
        )
      ),
      
      # NEW: Add Power BI Dashboard as the first menu item
      menuItem("Campaigns Overview (Power BI)", 
               tabName = "powerbi_dashboard", 
               icon = icon("chart-line")),
      
      menuItem("LQAS Repository Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("SIA Scope Analysis", tabName = "scope", icon = icon("syringe")),
      menuItem("Admin Data Overview", tabName = "admin_overview", icon = icon("database")),
      menuItem("Missed Children", tabName = "missed_children", icon = icon("child")),
      menuItem("Coverage Analysis", tabName = "coverage", icon = icon("chart-line")),
      menuItem("District Performance", tabName = "district_perf", icon = icon("map-marked-alt")),
      menuItem("LQAS Summary Maps", tabName = "lqas_maps", icon = icon("globe-africa")),
      menuItem("Reasons Analysis", tabName = "reasons", icon = icon("search")),
      menuItem("Unresolved Cases", tabName = "unresolved", icon = icon("exclamation-triangle")),
      menuItem("IM Settlement Maps", tabName = "settlement_maps", icon = icon("map")),
      menuItem("Generate Slides", tabName = "generate_slides", icon = icon("file-powerpoint")),
      menuItem("Quick Start Guide", tabName = "guide", icon = icon("info-circle")),
      
      # Performance indicators - UPDATED
      tags$div(
        style = "position: absolute; bottom: 0; left: 0; right: 0; padding: 15px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
        tags$small(
          style = "color: #6c757d;",
          tags$div("💾 Stable Data Loading", style = "margin-bottom: 5px;"),
          tags$div("⚡ Performance Optimized", style = "margin-bottom: 5px;"),
          tags$div("🛡️ Error Protected"),
          tags$div(paste("Dev Mode:", if(interactive()) "ON" else "OFF"), 
                   style = "margin-top: 8px; font-weight: bold; color: #28a745;")
        )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* ===========================
       GLOBAL THEME TOKENS (LIGHT)
       =========================== */
    :root {
      --kpi-bg: #ffffff;
      --kpi-text: #111827;
      --kpi-subtext: #6b7280;
      --kpi-border: rgba(37, 99, 235, 0.18);
      --kpi-shadow: 0 0 0 1px rgba(148, 163, 184, 0.35),
                    0 10px 20px rgba(15, 23, 42, 0.10);
      --kpi-shadow-hover: 0 0 0 1px rgba(37, 99, 235, 0.45),
                          0 16px 30px rgba(15, 23, 42, 0.25);
    }

    /* ===========================
       DARK MODE (manual toggle via .dark-mode on <body>)
       =========================== */
    .dark-mode {
      --kpi-bg: #020617;
      --kpi-text: #e5e7eb;
      --kpi-subtext: #9ca3af;
      --kpi-border: rgba(59, 130, 246, 0.55);
      --kpi-shadow: 0 0 0 1px rgba(31, 41, 55, 0.9),
                    0 14px 30px rgba(15, 23, 42, 0.85);
      --kpi-shadow-hover: 0 0 0 1px rgba(59, 130, 246, 0.8),
                          0 18px 38px rgba(15, 23, 42, 0.95);
    }

    .dark-mode .content-wrapper,
    .dark-mode .right-side {
      background-color: #020617;
      color: var(--kpi-text);
    }

    .dark-mode .box {
      background-color: #020617;
      border-color: #111827;
    }

    .dark-mode .sidebar-menu li a {
      color: #e5e7eb;
    }

    .dark-mode .sidebar-menu li.active > a {
      background-color: #0b1120 !important;
      color: #f9fafb !important;
    }

    /* ===========================
       MAIN BOX / FORM / BUTTONS
       =========================== */
    .box { 
      border-radius: 10px; 
      margin-bottom: 10px; 
      box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      border: 1px solid #e5e7eb;
      position: relative; /* Ensure proper stacking context */
    }
    
    /* Hide all tab content by default, show only active */
    .tab-content > .tab-pane {
      display: none;
    }
    
    .tab-content > .active {
      display: block;
    }
    
    .form-group { 
      margin-bottom: 8px !important; 
    }
    
    .control-label { 
      font-weight: 600; 
      font-size: 13px; 
      color: #2c3e50;
    }
    
    .btn-primary-custom { 
      background-color: #367fa9; 
      color: white; 
      border: none; 
      padding: 10px 16px; 
      border-radius: 6px;
      width: 100%;
      font-weight: bold;
      transition: all 0.25s ease;
      margin-top: 10px;
    }
    
    .btn-primary-custom:hover {
      background-color: #2c6a8d;
      transform: translateY(-1px);
      box-shadow: 0 6px 14px rgba(0,0,0,0.25);
    }
    
    .btn-primary-custom:active {
      transform: translateY(0);
      box-shadow: none;
    }
    
    .shiny-output-error { 
      color: #d9534f; 
      font-weight: bold; 
      padding: 15px;
      border-radius: 6px;
      background-color: #f8d7da;
      border: 1px solid #f5c6cb;
    }
    
    .shiny-output-error:before { 
      content: '⚠️ '; 
    }
    
    .shiny-output-success {
      color: #155724;
      background-color: #d4edda;
      border: 1px solid #c3e6cb;
      border-radius: 6px;
      padding: 15px;
      margin: 10px 0;
    }
    
    .shiny-spinner-output-container {
      position: relative;
    }
    
    .shiny-spinner-hidden {
      display: none;
    }
    
    .value-box .icon { 
      font-size: 32px; 
    }
    
    .small-box { 
      margin-bottom: 10px; 
    }
    
    .analysis-controls { 
      background-color: #f8f9fa; 
      padding: 15px; 
      border-radius: 8px; 
      margin-bottom: 15px;
      border-left: 4px solid #367fa9;
    }
    
    .flextable-output {
      overflow-x: auto;
      margin: 15px 0;
      border: 1px solid #e9ecef;
      border-radius: 8px;
      padding: 15px;
      background-color: white;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    
    .flextable {
      width: 100% !important;
      margin-bottom: 0 !important;
    }
    
    .tab-content {
      padding: 15px;
    }
    
    .alert-warning {
      background-color: #fff3cd;
      border: 1px solid #ffeaa7;
      border-radius: 8px;
      padding: 15px;
      margin: 10px 0;
      color: #856404;
    }
    
    .alert-info {
      background-color: #d1ecf1;
      border: 1px solid #bee5eb;
      border-radius: 8px;
      padding: 15px;
      margin: 10px 0;
      color: #0c5460;
    }

    .alert-success {
      background-color: #d4edda;
      border: 1px solid #c3e6cb;
      border-radius: 8px;
      padding: 15px;
      margin: 10px 0;
      color: #155724;
    }
    
    .selectize-input {
      border-radius: 6px;
      border: 1px solid #ced4da;
      padding: 8px 12px;
    }
    
    .selectize-dropdown {
      border-radius: 6px;
      border: 1px solid #ced4da;
    }
    
    .shiny-bound-output {
      transform: translateZ(0);
    }
    
    .dataTables_wrapper {
      border-radius: 8px;
      overflow: hidden;
    }
    
    .sidebar-menu li.active {
      border-left: 4px solid #367fa9;
      background-color: #f8f9fa;
    }
    
    .main-header .logo {
      background-color: #367fa9;
      font-weight: bold;
    }
    
    @media (max-width: 768px) {
      .box {
        margin-bottom: 15px;
      }
      .value-box {
        margin-bottom: 10px;
      }
    }
    
    ::-webkit-scrollbar {
      width: 8px;
    }
    
    ::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 4px;
    }
    
    ::-webkit-scrollbar-thumb {
      background: #c1c1c1;
      border-radius: 4px;
    }
    
    ::-webkit-scrollbar-thumb:hover {
      background: #a8a8a8;
    }
    
    .loading-overlay {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(255, 255, 255, 0.8);
      display: flex;
      justify-content: center;
      align-items: center;
      z-index: 9999;
      display: none;
    }
    
    .loading-spinner {
      border: 5px solid #f3f3f3;
      border-top: 5px solid #367fa9;
      border-radius: 50%;
      width: 50px;
      height: 50px;
      animation: spin 1s linear infinite;
    }
    
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
    
    .connection-status {
      position: fixed;
      top: 10px;
      right: 10px;
      padding: 5px 10px;
      border-radius: 15px;
      font-size: 12px;
      z-index: 10000;
      background-color: #28a745;
      color: white;
      display: none;
    }
    
    .connection-status.disconnected {
      background-color: #dc3545;
    }
    
    #whoHomeHeader.clicked-home {
      box-shadow: 0 0 20px 2px rgba(0, 114, 178, 0.6);
    }

    /* HEADER DARK-MODE TOGGLE STYLING */
    .dark-mode-toggle {
      display: flex;
      align-items: center;
      gap: 4px;
      color: #f9fafb;
    }

    .dark-mode-toggle .checkbox {
      margin: 0;
    }

    .dark-mode-toggle input[type='checkbox'] {
      width: 16px;
      height: 16px;
      cursor: pointer;
    }

    /* ==============================================
       MODERN KPI VALUE BOXES (ALL TABS)
       ============================================== */

    .valuebox-flex-container {
      display: flex;
      flex-wrap: nowrap;
      gap: 10px;
      margin-bottom: 18px;
    }

    .valuebox-flex-container .small-box,
    .content .small-box {
      border-radius: 16px;
      background: radial-gradient(circle at top left,
                                  rgba(59, 130, 246, 0.08),
                                  rgba(15, 23, 42, 0.01)),
                  var(--kpi-bg);
      border: 1px solid var(--kpi-border);
      box-shadow: var(--kpi-shadow);
      
      height: 78px;
      padding: 10px 14px;
      padding-right: 52px;
      position: relative;
      overflow: hidden;
      margin-bottom: 0;
      
      transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
    }

    .valuebox-flex-container .small-box:hover,
    .content .small-box:hover {
      transform: translateY(-1px);
      box-shadow: var(--kpi-shadow-hover);
      border-color: rgba(37, 99, 235, 0.65);
    }

    .valuebox-flex-container .small-box .inner,
    .content .small-box .inner {
      text-align: left;
      padding: 0;
      margin: 0;
      color: var(--kpi-text);
    }

    .valuebox-flex-container .small-box .inner h3,
    .content .small-box .inner h3 {
      font-size: 20px !important;
      line-height: 22px !important;
      margin: 0 0 3px 0;
      font-weight: 700;
    }

    .valuebox-flex-container .small-box .inner p,
    .content .small-box .inner p {
      font-size: 11px !important;
      margin: 0;
      color: var(--kpi-subtext);
      text-transform: uppercase;
      letter-spacing: 0.04em;
    }

    .valuebox-flex-container .small-box .icon,
    .content .small-box .icon {
      position: absolute !important;
      right: 12px !important;
      top: 50% !important;
      transform: translateY(-50%);
      
      width: 34px;
      height: 34px;
      
      display: flex;
      align-items: center;
      justify-content: center;
      
      border-radius: 999px;
      background: rgba(15, 23, 42, 0.03);
      box-shadow: 0 0 0 1px rgba(148, 163, 184, 0.5),
                  0 0 18px rgba(37, 99, 235, 0.28);
      color: #1d4ed8;
      padding: 0;
      margin: 0;
    }

    .valuebox-flex-container .small-box .icon i,
    .content .small-box .icon i {
      font-size: 20px !important;
      line-height: 1 !important;
      margin: 0 !important;
      padding: 0 !important;
    }

    /* Dark-mode tweaks for KPI cards */
    .dark-mode .valuebox-flex-container .small-box,
    .dark-mode .content .small-box {
      background: radial-gradient(circle at top left,
                                  rgba(59, 130, 246, 0.22),
                                  rgba(15, 23, 42, 0.92));
    }

    .dark-mode .valuebox-flex-container .small-box .icon,
    .dark-mode .content .small-box .icon {
      background: rgba(15, 23, 42, 0.9);
      box-shadow: 0 0 0 1px rgba(37, 99, 235, 0.9),
                  0 0 24px rgba(59, 130, 246, 0.85);
      color: #93c5fd;
    }

    /* ==============================================
       POWER BI IFRAME STYLING (NEW) - FIXED
       ============================================== */
    iframe {
      border: none;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: box-shadow 0.3s ease;
      position: relative; /* Ensure proper positioning */
      z-index: 1; /* Normal stacking order */
    }

    iframe:hover {
      box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    }

    /* Ensure iframe doesn't overflow on mobile */
    @media (max-width: 768px) {
      iframe {
        height: 600px !important;
      }
    }

    /* Loading overlay for iframe - FIXED VERSION */
    .iframe-loading-container {
      position: relative;
      min-height: 200px;
    }
    
    .iframe-loading-overlay {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(248, 249, 250, 0.95);
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      z-index: 2; /* Below iframe but above container */
      border-radius: 8px;
      transition: opacity 0.3s ease;
    }
    
    .dark-mode .iframe-loading-overlay {
      background: rgba(15, 23, 42, 0.95);
    }
    
    .iframe-loading-overlay.hidden {
      opacity: 0;
      pointer-events: none;
      display: none !important;
    }
    
    .iframe-loading-spinner {
      border: 4px solid #f3f3f3;
      border-top: 4px solid #367fa9;
      border-radius: 50%;
      width: 40px;
      height: 40px;
      animation: spin 1s linear infinite;
      margin-bottom: 15px;
    }
    
    .dark-mode .iframe-loading-spinner {
      border: 4px solid rgba(255, 255, 255, 0.1);
      border-top: 4px solid #93c5fd;
    }
    
    .iframe-loading-text {
      color: #666;
      font-size: 16px;
      font-weight: 500;
      text-align: center;
    }
    
    .dark-mode .iframe-loading-text {
      color: #9ca3af;
    }

    /* Dark mode adjustments for iframe container */
    .dark-mode .box iframe {
      box-shadow: 0 2px 4px rgba(0,0,0,0.3);
    }

    /* Smooth iframe appearance */
    iframe {
      animation: fadeIn 0.5s ease-in;
    }

    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }
    
    /* ==============================================
       FIX FOR TAB CONTENT DISPLAY
       ============================================== */
    /* Ensure only active tab content is visible */
    .tab-content {
      position: relative;
      width: 100%;
    }
    
    .tab-pane {
      display: none;
      opacity: 0;
      transition: opacity 0.3s ease;
    }
    
    .tab-pane.active {
      display: block;
      opacity: 1;
    }
    
    /* Prevent iframe from covering other content */
    .content-wrapper, .right-side {
      overflow: hidden; /* Prevent iframe overflow */
      position: relative;
    }
    
    /* Force proper tab switching behavior */
    [data-value='powerbi_dashboard'] {
      display: none;
    }
    
    [data-value='powerbi_dashboard'].active {
      display: block;
    }
    
    /* ==============================================
       DOWNLOAD BUTTON STYLES FOR PRIORITY DRILL-DOWN
       ============================================== */
    .btn-sm {
      padding: 3px 10px;
      font-size: 12px;
      line-height: 1.5;
      border-radius: 3px;
      margin-top: -3px;
    }
    
    .btn-primary-custom.btn-sm {
      background-color: #3c8dbc;
      border-color: #367fa9;
      color: white;
    }
    
    .btn-success-custom.btn-sm {
      background-color: #00a65a;
      border-color: #008d4c;
      color: white;
    }
    
    .btn-primary-custom.btn-sm:hover,
    .btn-success-custom.btn-sm:hover {
      opacity: 0.9;
      cursor: pointer;
    }
    
    .btn-primary-custom.btn-sm:active,
    .btn-success-custom.btn-sm:active {
      transform: translateY(1px);
    }
    
    /* Ensure buttons in box headers are properly positioned */
    .box-header .box-title div {
      width: 100%;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    
    /* Fix for button spacing in headers */
    .box-header .btn-sm {
      margin-left: 10px;
    }
  ")),
      
      # 🔁 JS: Dark mode with persisted preference + iframe handling
      tags$script(HTML("
    // 🔒 Persist dark mode preference using localStorage
    // Key used for storage
    var DARK_MODE_STORAGE_KEY = 'afro_sia_dark_mode';

    // On Shiny connection: apply stored preference (default = dark)
    $(document).on('shiny:connected', function() {
      var stored = localStorage.getItem(DARK_MODE_STORAGE_KEY);
      var isDark = (stored === null) ? true : (stored === 'true');

      var $cb = $('#dark_mode_toggle');
      if ($cb.length) {
        $cb.prop('checked', isDark);
      }

      if (isDark) {
        $('body').addClass('dark-mode');
      } else {
        $('body').removeClass('dark-mode');
      }

      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('dark_mode_toggle', isDark, {priority: 'event'});
      }
    });

    // When the Shiny input changes, toggle class + update storage
    $(document).on('shiny:inputchanged', function(e) {
      if (e.name === 'dark_mode_toggle') {
        if (e.value) {
          $('body').addClass('dark-mode');
        } else {
          $('body').removeClass('dark-mode');
        }
        // Persist preference
        localStorage.setItem(DARK_MODE_STORAGE_KEY, e.value ? 'true' : 'false');
      }
    });

    // Improved iframe loading state handling with tab switching
    $(document).ready(function() {
      // Function to handle iframe loading
      function setupIframeLoading() {
        $('iframe').each(function() {
          var $iframe = $(this);
          var $container = $iframe.parent();
          var $tabPane = $iframe.closest('.tab-pane');
          
          // Only set up iframe if it's in the active tab
          if (!$tabPane.hasClass('active')) {
            // Hide iframe when not in active tab
            $iframe.css('visibility', 'hidden');
            return;
          }
          
          // Show iframe when in active tab
          $iframe.css('visibility', 'visible');
          
          // Create loading overlay if it doesn't exist
          if (!$container.find('.iframe-loading-overlay').length) {
            var $overlay = $('<div class=\"iframe-loading-overlay\"><div class=\"iframe-loading-spinner\"></div><div class=\"iframe-loading-text\">Loading Power BI dashboard...</div></div>');
            $container.addClass('iframe-loading-container').prepend($overlay);
          } else {
            // Show loading overlay again when tab becomes active
            $container.find('.iframe-loading-overlay').removeClass('hidden');
          }
          
          // Handle iframe load event
          $iframe.off('load').on('load', function() {
            console.log('Iframe loaded successfully');
            $container.find('.iframe-loading-overlay').addClass('hidden');
          });
          
          // Handle iframe error
          $iframe.off('error').on('error', function() {
            console.log('Iframe failed to load');
            $container.find('.iframe-loading-overlay')
              .html('<div class=\"iframe-loading-text\" style=\"color: #dc3545;\">Failed to load Power BI dashboard. Please check your connection.</div>')
              .delay(3000)
              .fadeOut(500, function() {
                $(this).addClass('hidden');
              });
          });
          
          // Set timeout to hide loading after 30 seconds (fallback)
          setTimeout(function() {
            $container.find('.iframe-loading-overlay').addClass('hidden');
          }, 30000);
        });
      }
      
      // Run setup on document ready
      setupIframeLoading();
      
      // Handle tab switching - CRITICAL FIX
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'tabs') {
          // Hide ALL iframes first
          $('iframe').css('visibility', 'hidden');
          
          // Small delay to ensure tab transition is complete
          setTimeout(function() {
            // Show iframe only in active tab
            var activeTab = $('.tab-pane.active');
            activeTab.find('iframe').css('visibility', 'visible');
            
            // Reinitialize iframe loading for active tab
            setupIframeLoading();
          }, 100);
        }
        
        // Special handling for Power BI tab
        if (e.name === 'tabs' && e.value === 'powerbi_dashboard') {
          // Force reload iframe when switching to Power BI tab
          setTimeout(function() {
            var $iframe = $('[data-value=\"powerbi_dashboard\"] iframe');
            if ($iframe.length) {
              // Show loading overlay
              var $container = $iframe.parent();
              $container.find('.iframe-loading-overlay').removeClass('hidden');
              
              // Force iframe reload by setting src again
              var currentSrc = $iframe.attr('src');
              $iframe.attr('src', currentSrc);
            }
          }, 300);
        }
      });
      
      // Also handle manual tab clicks (for Bootstrap tabs)
      $('a[data-toggle=\"tab\"], a[data-toggle=\"pill\"]').on('shown.bs.tab', function() {
        // Hide ALL iframes
        $('iframe').css('visibility', 'hidden');
        
        // Show iframe only in newly active tab
        setTimeout(function() {
          var activeTab = $('.tab-pane.active');
          activeTab.find('iframe').css('visibility', 'visible');
          setupIframeLoading();
        }, 100);
      });
    });
  "))),
    
    
    
    
    # Connection status indicator
    tags$div(id = "connectionStatus", class = "connection-status connected", "Connected"),
    
    # Loading overlay (manually controlled)
    tags$div(
      id = "loadingOverlay",
      class = "loading-overlay",
      tags$div(class = "loading-spinner")
    ),
    
    # Keep-alive hidden element for session management
    tags$div(style = "display: none;",
             textOutput("keep_alive")),
    
    
    
    # ============================================================
    # 📊 POWER BI DASHBOARD - Campaigns Overview
    # ============================================================
    tabItem(
      tabName = "powerbi_dashboard",
      fluidRow(
        box(
          title = tags$div(icon("chart-line"), "Campaigns Overview - Power BI Dashboard"),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          height = "850px",
          
          # Power BI Embedded iframe
          tags$iframe(
            src = "https://app.powerbi.com/view?r=eyJrIjoiOTJjZDc5ZDctNGFmMi00NTI3LWEyYmEtYjhlMGQ0ZmUxZTZhIiwidCI6IjIxZmZhYjVkLWJlNzEtNGQ2ZS05YzRjLTFmYWZkYWUwNjdhOCIsImMiOjl9",
            style = "width: 100%; height: 800px; border: none;",
            frameborder = "0",
            allowfullscreen = TRUE
          ),
          
          # Optional: Add some descriptive text
          tags$div(
            style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
            tags$p(
              "This embedded Power BI dashboard provides an overview of vaccination campaigns across the AFRO region.",
              style = "margin: 0; font-size: 14px; color: #666;"
            )
          )
        )
      )
    ),
    
    
    # ============================================================
    # 📊 LQAS REPOSITORY OVERVIEW
    # ============================================================
    tabItem(
      tabName = "overview",
      fluidRow(
        # ---- FILTER PANEL ----
        conditionalPanel(
          condition = "input.tabs == 'overview'",
          box(
            title = tags$div(icon("filter"), "Overview Filters"),
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            style = "height: 90vh; overflow-y: auto;",
            
            # ADDED: Block Type Selection
            selectizeInput(
              "overview_block_type", 
              "Block Type:",
              choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"), 
              selected = "afro",
              options = list(
                placeholder = 'Select Block Type...',
                dropdownParent = 'body'
              )
            ),
            
            # MODIFIED: Dynamic Block Selection
            selectizeInput(
              "overview_block", 
              "Selected Block:",
              choices = c("All", names(afro_blocks)), 
              selected = "All",
              options = list(
                placeholder = 'Select Block...',
                dropdownParent = 'body'
              )
            ),
            
            uiOutput("overview_countries_ui"),
            uiOutput("overview_provinces_ui"),
            
            # UPDATED: Server-side optimized district selector
            selectizeInput(
              "overview_districts", 
              "Districts (optional):",
              choices = character(0),  # Start empty
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = 'Type to search districts...',
                maxItems = 15,
                dropdownParent = 'body',
                openOnFocus = FALSE,
                create = FALSE
              )
            ),
            
            selectizeInput(
              "overview_time_type", 
              "Timeframe Type:",
              choices = c("Date Range" = "range", "Specific Year" = "years"),
              selected = "range",
              options = list(dropdownParent = 'body')
            ),
            
            conditionalPanel(
              condition = "input.overview_time_type == 'range'",
              dateRangeInput(
                "overview_daterange", "Select Date Range:",
                start = Sys.Date() - 365,
                end   = Sys.Date(),
                format   = "yyyy-mm-dd",
                startview = "year"
              )
            ),
            
            conditionalPanel(
              condition = "input.overview_time_type == 'years'",
              textInput(
                "overview_years", "Year(s):", 
                placeholder = "e.g. 2023, 2024 or 2023 2024",
                value = format(Sys.Date(), "%Y")
              ),
              helpText(
                "Enter years separated by commas or spaces", 
                style = "font-size: 11px; color: #666; margin-top: -10px;"
              )
            ),
            
            checkboxGroupInput(
              "overview_perf_filter",
              "District Performance:",
              choices = c(
                "Always High Performing" = "high",
                "Never High Performing"  = "poor"
              ),
              selected = NULL,
              inline = FALSE
            ),
            
            # ADDED: Debug checkbox
            checkboxInput("overview_debug", "Enable Debug Mode", value = FALSE),
            
            actionButton(
              "overview_analyze", "🚀 Run Overview", 
              class = "btn-primary-custom",
              style = "margin-top: 15px;"
            )
          )
        ),
        
        # ---- RESULTS PANEL ----
        conditionalPanel(
          condition = "input.tabs == 'overview'",
          box(
            title = tags$div(icon("database"), "LQAS Repository Overview"),
            width = 9,
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            
            # 🔹 AUTO-FIT, ALIGNED VALUE BOXES
            div(
              class = "valuebox-flex-container",
              valueBoxOutput("overview_total_campaigns"),
              valueBoxOutput("overview_total_countries"),
              valueBoxOutput("overview_total_provinces"),   # ⬅️ Provinces box
              valueBoxOutput("overview_total_districts"),
              valueBoxOutput("overview_data_period")
            ),
            
            # ADDED: Debug info panel
            conditionalPanel(
              condition = "input.overview_debug == true",
              wellPanel(
                h4("🔍 Debug Information"),
                verbatimTextOutput("overview_debug_info"),
                style = "background-color: #f8f9fa; margin-bottom: 20px;"
              )
            ),
            
            div(
              class = "analysis-controls",
              h4(icon("table"), "Data Explorer"),
              DTOutput("overview_explorer_table")
            ),
            
            div(
              style = "text-align: center; margin-top: 20px;",
              downloadButton(
                "download_overview_data", 
                "📥 Download Filtered Data", 
                class = "btn-primary-custom",
                style = "width: 300px;"
              )
            )
          )
        )
      )
    ),
    
    
    
    # ============================================================
    # 💉 SIA SCOPE ANALYSIS UI (UPDATED WITH BLOCK TYPE SELECTION)
    # ============================================================
    tabItem(
      tabName = "scope",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'scope'",
          column(
            width = 3,
            box(
              title = tags$div(icon("cog"), "Scope Analysis Settings"),
              width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
              
              selectInput("scope_time_type", "Timeframe Type:",
                          choices = c("Last X Months" = "months",
                                      "Specific Year(s)" = "years",
                                      "All Data" = "all"),
                          selected = "months"),
              conditionalPanel("input.scope_time_type == 'months'",
                               numericInput("scope_months", "Number of Months:", value = 6, min = 1, max = 36, step = 1)),
              conditionalPanel("input.scope_time_type == 'years'",
                               textInput("scope_years", "Year(s):",
                                         placeholder = "e.g., 2023,2024",
                                         value = format(Sys.Date(), "%Y"))),
              
              # ADDED: Block Type Selection
              selectInput("scope_block_type", "Block Type:",
                          choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                          selected = "afro"),
              
              # MODIFIED: Dynamic Block Selection
              selectInput("scope_block", "Selected Block:",
                          choices = c("All", names(afro_blocks)), selected = "All"),
              
              # Country filter - DROPDOWN (FIXED Z-INDEX)
              selectizeInput("scope_countries", "Country/Countries:",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more countries',
                               maxItems = 10,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'  # CRITICAL FIX
                             )),
              
              # Province filter - DROPDOWN (FIXED Z-INDEX)
              selectizeInput("scope_provinces", "Province(s):",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more provinces',
                               maxItems = 20,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'  # CRITICAL FIX
                             )),
              
              # District filter - DROPDOWN (FIXED Z-INDEX)
              selectizeInput("scope_districts", "District(s):",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more districts',
                               maxItems = 30,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'  # CRITICAL FIX
                             )),
              
              selectInput("scope_vaccine", "Vaccine Type:",
                          choices = c("All", "nOPV2", "bOPV", "nOPV2 & bOPV"),
                          selected = "All"),
              actionButton("scope_analyze", "🔍 Run Scope Analysis",
                           class = "btn-primary-custom", style = "width:100%; font-weight:bold;")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'scope'",
          column(
            width = 9,
            fluidRow(
              valueBoxOutput("scope_campaigns", width = 3),
              valueBoxOutput("scope_countries_box", width = 3),
              valueBoxOutput("scope_districts_box", width = 3),
              valueBoxOutput("scope_vaccines", width = 3)
            ),
            box(
              title = tags$div(icon("map"), "SIA Scope Analysis Results"),
              width = 12, solidHeader = TRUE, status = "info", collapsible = TRUE,
              tabsetPanel(
                id = "scope_tabs",
                tabPanel(tags$div(icon("globe"), "Scope Map"), value = "scope_map",
                         div(style = "text-align:center;",
                             shinycssloaders::withSpinner(
                               plotOutput("scope_map_plot", height = "650px", width = "100%"),
                               type = 8, color = "#0072BC", size = 1.2)),
                         div(style = "text-align:center; margin-top:15px;",
                             downloadButton("download_scope_map", "🗺️ Download Map",
                                            class = "btn-primary-custom", style = "width:200px;"))
                ),
                tabPanel(tags$div(icon("layer-group"), "Scope Summary Map"), value = "scope_summary_map",
                         div(style = "margin: 10px 0 12px 0; padding:10px 12px; background:#f8fafc; border-left:4px solid #1f4e79; border-radius:6px; font-size:12px; color:#374151;",
                             tags$b("Definition: "),
                             "One map aggregated over the whole selected period -- a district is shown as in scope if it appeared in ANY round. ",
                             "Districts covered with more than one vaccine type across the period's rounds are shown as \"Mixed (varies by round)\" rather than defaulting to a single round's value."),
                         div(style = "text-align:center;",
                             shinycssloaders::withSpinner(
                               plotOutput("scope_summary_map_plot", height = "650px", width = "100%"),
                               type = 8, color = "#0072BC", size = 1.2)),
                         uiOutput("scope_summary_footnote"),
                         div(style = "text-align:center; margin-top:15px;",
                             downloadButton("download_scope_summary_map", "🗺️ Download Summary Map",
                                            class = "btn-primary-custom", style = "width:220px; margin-right:10px;"),
                             downloadButton("download_scope_summary_map_pdf", "📄 Download as PDF",
                                            class = "btn-primary-custom", style = "width:220px;"))
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # 🏛️ ADMIN DATA OVERVIEW (UPDATED WITH BLOCK TYPE SELECTION)
    # ============================================================
    tabItem(
      tabName = "admin_overview",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'admin_overview'",
          box(
            title = tags$div(icon("filter"), "Filters"),
            width = 3,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            # ADDED: Block Type Selection
            selectInput("admin_block_type", "Block Type:", 
                        choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"), 
                        selected = "afro"),
            
            # MODIFIED: Dynamic Block Selection
            selectInput("admin_block", "Selected Block:", 
                        choices = c("All", names(afro_blocks)), 
                        selected = "All"),
            
            uiOutput("admin_countries_ui"),
            
            selectInput("admin_time_type", "Time Mode:", 
                        choices = c("Last X Months" = "months", 
                                    "Specific Years" = "years"), 
                        selected = "months"),
            
            conditionalPanel(
              condition = "input.admin_time_type == 'months'",
              numericInput("admin_months", "Last X Months:", 
                           value = 12, min = 1, max = 36)
            ),
            
            conditionalPanel(
              condition = "input.admin_time_type == 'years'",
              textInput("admin_years", "Year(s):", 
                        placeholder = "e.g. 2023,2024",
                        value = format(Sys.Date(), "%Y"))
            ),
            
            actionButton("admin_analyze", "📊 Run Admin Overview", 
                         class = "btn-primary-custom")
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'admin_overview'",
          box(
            title = tags$div(icon("database"), "Admin Data Overview"),
            width = 9,
            solidHeader = TRUE,
            status = "info",
            
            uiOutput("admin_summary_text"),
            
            fluidRow(
              valueBoxOutput("admin_total_vaccinated", width = 3),
              valueBoxOutput("admin_children_vaccinated", width = 3),
              valueBoxOutput("admin_countries_reporting", width = 3),
              valueBoxOutput("admin_avg_coverage", width = 3)
            ),
            
            tabsetPanel(
              tabPanel(
                tags$div(icon("chart-bar"), "District Coverage ≥95%"), 
                uiOutput("admin_coverage_table_ui")
              ),
              tabPanel(
                tags$div(icon("child"), "Children Vaccinated (Millions)"), 
                uiOutput("admin_vaccinated_table_ui")
              )
            ),
            
            fluidRow(
              style = "margin-top: 20px;",
              column(6, 
                     downloadButton("download_admin_coverage", 
                                    "📥 Download Coverage Table", 
                                    class = "btn-primary-custom")
              ),
              column(6, 
                     downloadButton("download_admin_vaccinated", 
                                    "📥 Download Vaccinated Table", 
                                    class = "btn-primary-custom")
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # 👶 MISSED CHILDREN (Updated with Block Type Selection)
    # ============================================================
    tabItem(
      tabName = "missed_children",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'missed_children'",
          column(
            width = 3,
            box(
              title = tags$div(icon("search"), "Missed Children Analysis"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "primary",
              
              # ADDED: Block Type Selection
              selectInput("missed_block_type", "Block Type:",
                          choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                          selected = "afro"),
              
              # MODIFIED: Dynamic Block Selection
              selectInput("missed_block", "Selected Block:",
                          choices = c("All", names(afro_blocks)), 
                          selected = "All"),
              
              selectInput("missed_time_type", "Time Period Type:",
                          choices = c("Last X Months" = "x_months",
                                      "Specific Quarter" = "quarter", 
                                      "Last N Quarters" = "last_quarters"),
                          selected = "x_months"),
              
              conditionalPanel(
                condition = "input.missed_time_type == 'x_months'",
                numericInput("missed_months", "Number of Months:", 
                             value = 12, min = 1, max = 24)
              ),
              
              conditionalPanel(
                condition = "input.missed_time_type == 'quarter'",
                numericInput("missed_year", "Year:", 
                             value = year(Sys.Date()), min = 2020, max = year(Sys.Date())),
                selectInput("missed_quarter", "Quarter:", choices = 1:4)
              ),
              
              conditionalPanel(
                condition = "input.missed_time_type == 'last_quarters'",
                selectInput("missed_quarters", "Last N Quarters:", 
                            choices = 1:4, selected = 2)
              ),
              
              uiOutput("missed_countries_ui"),
              
              actionButton("missed_analyze", "🔍 Analyze Missed Children", 
                           class = "btn-primary-custom", style = "width:100%; font-weight:bold; margin-bottom:10px;"),
              
              # Download Filtered Data Button
              conditionalPanel(
                condition = "output.missed_analysis_exists",
                div(style = "text-align:center; margin-top:10px;",
                    downloadButton("download_missed_filtered_data", 
                                   "📥 Download Filtered Data", 
                                   class = "btn-success", 
                                   style = "width:100%; font-weight:bold;")
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'missed_children'",
          column(
            width = 9,
            fluidRow(
              valueBoxOutput("missed_overall_rate", width = 4),
              valueBoxOutput("missed_boys_rate", width = 4),
              valueBoxOutput("missed_girls_rate", width = 4)
            ),
            
            box(
              title = tags$div(icon("chart-bar"), "Missed Children Analysis Results"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "info",
              
              tabsetPanel(
                tabPanel(
                  tags$div(icon("table"), "Summary Table"), 
                  uiOutput("missed_flextable_ui"),
                  fluidRow(
                    column(6,
                           downloadButton("download_missed_table", 
                                          "📊 Download Table", 
                                          class = "btn-primary-custom")
                    ),
                    column(6,
                           downloadButton("download_missed_data", 
                                          "💾 Download Raw Data", 
                                          class = "btn-primary-custom")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # 📈 COVERAGE ANALYSIS (Updated with Block Type Selection)
    # ============================================================
    tabItem(
      tabName = "coverage",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'coverage'",
          column(
            width = 3,
            box(
              title = tags$div(icon("cog"), "Coverage Analysis Settings"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "primary",
              
              # ADDED: Block Type Selection
              selectInput("coverage_block_type", "Block Type:",
                          choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                          selected = "afro"),
              
              # MODIFIED: Dynamic Block Selection
              selectInput("coverage_block", "Selected Block:",
                          choices = c("All", names(afro_blocks)), 
                          selected = "All"),
              
              selectInput("coverage_time_type", "Time Period Type:",
                          choices = c("Last X Months" = "x_months",
                                      "Specific Quarter" = "quarter",
                                      "Last N Quarters" = "last_quarters"),
                          selected = "x_months"),
              
              conditionalPanel(
                condition = "input.coverage_time_type == 'x_months'",
                numericInput("coverage_months", "Number of Months:", 
                             value = 12, min = 1, max = 24)
              ),
              
              conditionalPanel(
                condition = "input.coverage_time_type == 'quarter'",
                numericInput("coverage_year", "Year:", 
                             value = year(Sys.Date()), min = 2020, max = year(Sys.Date())),
                selectInput("coverage_quarter", "Quarter:", choices = 1:4)
              ),
              
              conditionalPanel(
                condition = "input.coverage_time_type == 'last_quarters'",
                selectInput("coverage_quarters", "Last N Quarters:", 
                            choices = 1:4, selected = 2)
              ),
              
              uiOutput("coverage_countries_ui"),
              
              actionButton("coverage_analyze", "📊 Analyze Coverage", 
                           class = "btn-primary-custom", style = "width:100%; font-weight:bold; margin-bottom:10px;"),
              
              # Download Filtered Data Button
              conditionalPanel(
                condition = "output.coverage_analysis_exists",
                div(style = "text-align:center; margin-top:10px;",
                    downloadButton("download_coverage_filtered_data", 
                                   "📥 Download Filtered Data", 
                                   class = "btn-success", 
                                   style = "width:100%; font-weight:bold;")
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'coverage'",
          column(
            width = 9,
            fluidRow(
              valueBoxOutput("coverage_overall", width = 4),
              valueBoxOutput("coverage_boys", width = 4),
              valueBoxOutput("coverage_girls", width = 4)
            ),
            
            box(
              title = tags$div(icon("chart-line"), "Coverage Analysis Results"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "info",
              
              tabsetPanel(
                tabPanel(
                  tags$div(icon("table"), "Coverage Table"), 
                  uiOutput("coverage_flextable_ui"),
                  fluidRow(
                    column(6,
                           downloadButton("download_coverage_table", 
                                          "📊 Download Table", 
                                          class = "btn-primary-custom")
                    ),
                    column(6,
                           downloadButton("download_coverage_data", 
                                          "💾 Download Raw Data", 
                                          class = "btn-primary-custom")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    
    # ============================================================
    # 🗺️ DISTRICT PERFORMANCE + FACETED HEATMAP
    # ============================================================
    
    tabItem(
      tabName = "district_perf",
      
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'district_perf'",
          
          column(
            width = 3,
            
            box(
              title = tags$div(icon("cog"), "District Performance Settings"),
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              
              selectInput(
                "perf_block_type",
                "Block Type:",
                choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                selected = "afro"
              ),
              
              selectInput(
                "perf_block",
                "Selected Block:",
                choices = c("All"),
                selected = "All"
              ),
              
              selectInput(
                "perf_time_type",
                "Time Filter:",
                choices = c(
                  "Last X Months" = "x_months",
                  "Last X Consecutive Round(s)" = "x_data_months",
                  "Specific Year" = "year",
                  "Year + Last X Months" = "year_months",
                  "Specific Year + Month(s)" = "year_month"
                ),
                selected = "x_months"
              ),
              
              conditionalPanel(
                condition = "input.perf_time_type == 'x_months'",
                numericInput(
                  "perf_months",
                  "Number of Months:",
                  value = 12,
                  min = 1,
                  max = 24
                )
              ),
              
              conditionalPanel(
                condition = "input.perf_time_type == 'x_data_months'",
                numericInput(
                  "perf_data_months",
                  "Last X Data Months:",
                  value = 6,
                  min = 1,
                  max = 24
                ),
                helpText("This selects the last X months where each country has data. Gaps are allowed.")
              ),
              
              conditionalPanel(
                condition = "input.perf_time_type == 'year'",
                numericInput(
                  "perf_year",
                  "Year:",
                  value = as.numeric(format(Sys.Date(), "%Y")),
                  min = 2020,
                  max = as.numeric(format(Sys.Date(), "%Y"))
                )
              ),
              
              conditionalPanel(
                condition = "input.perf_time_type == 'year_months'",
                numericInput(
                  "perf_year_months",
                  "Year:",
                  value = as.numeric(format(Sys.Date(), "%Y")),
                  min = 2020,
                  max = as.numeric(format(Sys.Date(), "%Y"))
                ),
                numericInput(
                  "perf_months_year",
                  "Last X Months in Year:",
                  value = 6,
                  min = 1,
                  max = 12
                )
              ),
              
              conditionalPanel(
                condition = "input.perf_time_type == 'year_month'",
                
                numericInput(
                  "perf_year_month",
                  "Year:",
                  value = as.numeric(format(Sys.Date(), "%Y")),
                  min = 2020,
                  max = as.numeric(format(Sys.Date(), "%Y"))
                ),
                
                selectizeInput(
                  "perf_month_name",
                  "Month(s):",
                  choices = setNames(1:12, month.name),
                  selected = as.integer(format(Sys.Date(), "%m")),
                  multiple = TRUE,
                  options = list(
                    placeholder = "Select one or more months",
                    plugins = list("remove_button"),
                    dropdownParent = "body"
                  )
                ),
                
                tags$small("Tip: you can select multiple months, for example Oct–Dec.")
              ),
              
              selectizeInput(
                "perf_countries",
                "Countries (Optional):",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more countries",
                  maxItems = 15,
                  plugins = list("remove_button"),
                  dropdownParent = "body"
                )
              ),
              
              selectizeInput(
                "perf_provinces",
                "Province(s):",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more provinces",
                  maxItems = 20,
                  plugins = list("remove_button"),
                  dropdownParent = "body"
                )
              ),
              
              selectizeInput(
                "perf_districts",
                "District(s):",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more districts",
                  maxItems = 30,
                  plugins = list("remove_button"),
                  dropdownParent = "body"
                )
              ),
              
              selectInput(
                "perf_heatmap_geo_level",
                "Heatmap Geography Level:",
                choices = c(
                  "Country" = "country",
                  "Province" = "province"
                ),
                selected = "country"
              ),
              
              shinyWidgets::materialSwitch(
                inputId = "perf_show_district_labels",
                label = "Show district names on maps",
                value = FALSE,
                status = "primary",
                right = FALSE
              ),
              
              checkboxGroupInput(
                "perf_status_filter",
                "Performance Status (Optional):",
                choices = c(
                  "Never High Performing" = "never_high",
                  "Always High Performing" = "always_high"
                ),
                selected = character(0)
              ),
              
              helpText("Note: Leave unselected to show all districts."),
              
              actionButton(
                "perf_analyze",
                "🗺️ Analyze Performance",
                class = "btn-primary-custom",
                style = "width:100%; font-weight:bold; margin-bottom:10px;"
              ),
              
              conditionalPanel(
                condition = "output.perf_analysis_exists",
                div(
                  style = "text-align:center; margin-top:10px;",
                  downloadButton(
                    "download_filtered_data",
                    "📥 Download Filtered Data",
                    class = "btn-success",
                    style = "width:100%; font-weight:bold;"
                  )
                )
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'district_perf'",
          
          column(
            width = 9,
            
            fluidRow(
              valueBoxOutput("perf_total_districts", width = 4),
              valueBoxOutput("perf_high_performing", width = 4),
              valueBoxOutput("perf_poor_performing", width = 4)
            ),
            
            box(
              title = tags$div(icon("map"), "District Performance Results"),
              width = 12,
              solidHeader = TRUE,
              status = "info",
              collapsible = TRUE,
              
              tabsetPanel(
                id = "perf_tabs",
                
                tabPanel(
                  tags$div(icon("globe"), "Performance Maps"),
                  
                  div(
                    style = "text-align:center;",
                    shinycssloaders::withSpinner(
                      plotOutput("perf_maps", height = "650px", width = "100%"),
                      type = 8,
                      color = "#0072BC",
                      size = 1.2
                    )
                  ),
                  
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_perf_maps",
                      "🗺️ Download Maps",
                      class = "btn-primary-custom",
                      style = "width:200px;"
                    )
                  )
                ),
                
                tabPanel(
                  tags$div(icon("th"), "Faceted Heatmap"),
                  
                  div(
                    style = "text-align:center;",
                    shinycssloaders::withSpinner(
                      plotOutput("perf_heatmap", height = "650px", width = "100%"),
                      type = 8,
                      color = "#0072BC",
                      size = 1.2
                    )
                  ),
                  
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_perf_heatmap",
                      "🔥 Download Heatmap",
                      class = "btn-primary-custom",
                      style = "width:220px;"
                    )
                  )
                ),
                
                tabPanel(
                  tags$div(icon("table"), "Summary Table"),
                  
                  uiOutput("perf_summary_table_ui"),
                  
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_perf_table",
                      "📊 Download Table",
                      class = "btn-primary-custom",
                      style = "width:200px;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    
    
    # ============================================================
    # 🌍 LQAS SUMMARY MAPS — FULL UPDATED UI (LIVE + CONSISTENT)
    # ✅ Province labels added as optional checkbox
    # ============================================================
    
    tabItem(
      tabName = "lqas_maps",
      
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'lqas_maps'",
          
          column(
            width = 3,
            box(
              title = tags$div(icon("cog"), "LQAS Map Settings"),
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              
              selectInput(
                "lqas_block_type", "Block Type:",
                choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                selected = "afro"
              ),
              
              selectInput(
                "lqas_block", "Selected Block:",
                choices = c("All", names(afro_blocks)),
                selected = "All"
              ),
              
              selectInput(
                "lqas_time_type", "Timeframe Type:",
                choices = c(
                  "Last X Months"     = "months",
                  "Specific Year(s)"  = "years",
                  "All Data"          = "all"
                ),
                selected = "months"
              ),
              
              conditionalPanel(
                condition = "input.lqas_time_type == 'months'",
                numericInput("lqas_months", "Number of Months:", value = 6, min = 1, max = 24)
              ),
              
              conditionalPanel(
                condition = "input.lqas_time_type == 'years'",
                textInput(
                  "lqas_years", "Year(s):",
                  placeholder = "e.g., 2023,2024",
                  value = format(Sys.Date(), "%Y")
                )
              ),
              
              selectizeInput(
                "lqas_countries", "Countries (Optional):",
                choices = character(0),
                multiple = TRUE,
                options = list(plugins = list("remove_button"), dropdownParent = "body")
              ),
              
              selectizeInput(
                "lqas_provinces", "Province(s) (Optional):",
                choices = character(0),
                multiple = TRUE,
                options = list(plugins = list("remove_button"), dropdownParent = "body")
              ),
              
              selectizeInput(
                "lqas_districts", "District(s) (Optional):",
                choices = character(0),
                multiple = TRUE,
                options = list(plugins = list("remove_button"), dropdownParent = "body")
              ),
              
              checkboxGroupInput(
                "lqas_perf_filter",
                "Performance Range (Optional):",
                choices = c(
                  "High Performing (80-100%)" = "high_performing",
                  "Low Performing (0-25%)"    = "low_performing"
                )
              ),
              
              checkboxInput(
                "lqas_show_province_labels",
                "Show province labels on province map",
                value = FALSE
              ),
              
              actionButton(
                "lqas_analyze",
                "🌍 Generate / Refresh",
                class = "btn-primary-custom"
              ),
              
              div(
                style = "text-align:center; margin-top:15px;",
                downloadButton(
                  "download_lqas_data",
                  "📥 Download Filtered Data",
                  class = "btn-success"
                )
              ),
              
              tags$hr(),
              tags$div(
                style = "font-size:12px; color:#6b7280;",
                tags$b("Note: "),
                "All tabs update LIVE as you change filters. ",
                "Downloads always match the current filters."
              )
            )
          ),
          
          column(
            width = 9,
            
            fluidRow(
              valueBoxOutput("lqas_total_districts", 4),
              valueBoxOutput("lqas_high_performance", 4),
              valueBoxOutput("lqas_low_performance", 4)
            ),
            
            box(
              title = tags$div(icon("globe-africa"), "LQAS Summary Maps"),
              width = 12,
              solidHeader = TRUE,
              status = "info",
              
              tabsetPanel(
                tabPanel(
                  title = tags$div(icon("map"), "District Summary Map"),
                  plotOutput("lqas_summary_map", height = "600px"),
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_lqas_map",
                      "🗺️ Download District Map",
                      class = "btn-primary-custom"
                    )
                  )
                ),
                
                tabPanel(
                  title = tags$div(icon("layer-group"), "Province LQAS Summary Map"),
                  div(
                    style = "margin: 10px 0 12px 0; padding:10px 12px; background:#f8fafc; border-left:4px solid #1f4e79; border-radius:6px; font-size:12px; color:#374151;",
                    tags$b("Definition: "),
                    "Province high (%) = count of records where performance = 'high' / total count of performance records in the province for the selected period. ",
                    "The map bins provinces into: 0-25%, 25-50%, 50-80%, 80-100%."
                  ),
                  plotOutput("lqas_province_summary_map", height = "600px"),
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_lqas_province_map",
                      "🗺️ Download Province Map",
                      class = "btn-primary-custom"
                    )
                  )
                ),
                
                tabPanel(
                  title = tags$div(icon("table"), "Performance Table"),
                  uiOutput("lqas_summary_table_ui"),
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_lqas_table",
                      "📊 Download Table as Image",
                      class = "btn-primary-custom"
                    )
                  )
                ),
                
                tabPanel(
                  title = tags$div(icon("building"), "Province Summary"),
                  fluidRow(
                    column(
                      width = 12,
                      div(
                        style = "display:flex; justify-content:space-between; align-items:center; margin: 10px 0 12px 0; gap:10px; flex-wrap:wrap;",
                        tags$div(
                          style = "font-weight:600; color:#374151;",
                          "Power BI-like table: country, province, high (%)"
                        ),
                        div(
                          style = "display:flex; gap:10px; flex-wrap:wrap;",
                          downloadButton(
                            "download_lqas_province_summary_image",
                            "🖼️ Download Province Summary Image",
                            class = "btn-primary-custom"
                          ),
                          downloadButton(
                            "download_lqas_province_summary_excel",
                            "📥 Download Province Summary Excel",
                            class = "btn-success"
                          )
                        )
                      ),
                      div(
                        style = "margin: 0 0 12px 0; padding:10px 12px; background:#f8fafc; border-left:4px solid #1f4e79; border-radius:6px; font-size:12px; color:#374151;",
                        tags$b("Definition: "),
                        "high (%) = count of records where performance = 'high' ",
                        "/ total count of performance records in the province ",
                        "for the selected period."
                      ),
                      uiOutput("lqas_province_summary_ui")
                    )
                  )
                ),
                
                tabPanel(
                  title = tags$div(icon("database"), "Data Preview"),
                  DT::dataTableOutput("lqas_data_preview"),
                  div(
                    style = "text-align:center; margin-top:15px;",
                    downloadButton(
                      "download_lqas_data_preview",
                      "📥 Download Preview Data",
                      class = "btn-primary-custom"
                    )
                  )
                ),
                
                tabPanel(
                  title = tags$div(icon("th-large"), "Filtered Data (PowerBI view)"),
                  div(
                    style = "position:relative; z-index:9999; display:flex; justify-content:flex-end; margin: 8px 0 10px 0;",
                    actionButton(
                      "lqas_pbi_download_btn",
                      "⬇️ Download mini-tables",
                      style = "background:#00a65a; color:white; border:none; border-radius:6px; padding:8px 12px;"
                    )
                  ),
                  uiOutput("lqas_powerbi_view_ui")
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # COMPREHENSIVE REASONS ANALYSIS UI - COMPLETE (FIXED)
    # ============================================================
    tabItem(
      tabName = "reasons",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'reasons'",
          column(
            width = 3,
            box(
              title = tags$div(icon("cog"), "Reasons Analysis Settings"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "primary",
              
              # Geographic Level Selection
              selectInput("reasons_geo_level", "Geographic Level:",
                          choices = c(
                            "Country Level" = "country",
                            "Province Level" = "province",
                            "District Level" = "district"
                          ),
                          selected = "country"),
              
              # Visualization Type Selection
              selectInput("reasons_viz_type", "Visualization Type:",
                          choices = c(
                            "Auto (smart selection)" = "auto",
                            "🔥 Heatmap" = "heatmap",
                            "📊 Grouped Bar Chart" = "bar",
                            "📊 Stacked Bar Chart" = "stacked_bar",
                            "🔲 Faceted Bar Chart" = "faceted_bar",
                            "⚪ Bubble Chart" = "bubble",
                            "🥧 Pie Chart" = "pie",
                            "🍩 Donut Chart" = "donut",
                            "🌳 Treemap" = "treemap",
                            "☀️ Sunburst Chart" = "sunburst",
                            "📡 Radar Chart" = "radar",
                            "🍭 Lollipop Chart" = "lollipop",
                            "💧 Waterfall Chart" = "waterfall"
                          ),
                          selected = "auto"),
              
              # Combined Overview inclusion
              radioButtons("reasons_combined_choice", "Include in Combined Overview:",
                           choices = c(
                             "All three (Traditional + Non-Compliance + Absence)" = "all",
                             "Traditional + Non-Compliance only" = "trad_nc",
                             "Traditional + Absence only" = "trad_absence"
                           ),
                           selected = "all"),
              
              # Block Type Selection
              selectInput("reasons_block_type", "Block Type:",
                          choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                          selected = "afro"),
              
              # Dynamic Block Selection
              selectInput("reasons_block", "Selected Block:",
                          choices = c("All", names(afro_blocks)),
                          selected = "All"),
              
              selectInput("reasons_time_type", "Time Period:",
                          choices = c("Last X Months" = "x_months",
                                      "Specific Month" = "specific_month"),
                          selected = "x_months"),
              
              conditionalPanel(
                condition = "input.reasons_time_type == 'x_months'",
                numericInput("reasons_months", "Number of Months:", 
                             value = 12, min = 1, max = 24)
              ),
              
              conditionalPanel(
                condition = "input.reasons_time_type == 'specific_month'",
                numericInput("reasons_year", "Year:", 
                             value = year(Sys.Date()), min = 2020, max = year(Sys.Date())),
                selectInput("reasons_month", "Month:", 
                            choices = setNames(1:12, month.name),
                            selected = month(Sys.Date()))
              ),
              
              uiOutput("reasons_countries_ui"),
              
              conditionalPanel(
                condition = "input.reasons_geo_level == 'province' || input.reasons_geo_level == 'district'",
                uiOutput("reasons_provinces_ui")
              ),
              
              conditionalPanel(
                condition = "input.reasons_geo_level == 'district'",
                uiOutput("reasons_districts_ui")
              ),
              
              actionButton("reasons_analyze", "🔍 Analyze Reasons", 
                           class = "btn-primary-custom", 
                           style = "width: 100%; margin-top: 10px;")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'reasons'",
          column(
            width = 9,
            
            fluidRow(
              column(12,
                     div(style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                         uiOutput("reasons_geo_indicator"),
                         uiOutput("reasons_viz_indicator")
                     )
              )
            ),
            
            tabsetPanel(
              id = "reasons_main_tabs",
              
              # Combined Overview Tab
              tabPanel(
                title = tags$div(icon("chart-pie"), "Combined Overview"),
                value = "combined_overview",
                fluidRow(
                  valueBoxOutput("reasons_total_units_all", width = 3),
                  valueBoxOutput("reasons_total_categories_all", width = 3),
                  valueBoxOutput("reasons_avg_percentage_all", width = 3),
                  valueBoxOutput("reasons_viz_recommendation", width = 3)
                ),
                box(
                  title = tags$div(icon("chart-bar"), "Combined Analysis - All Reason Types"), 
                  width = 12, solidHeader = TRUE, status = "info",
                  plotOutput("reasons_combined_heatmap", height = "1100px"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_reasons_combined_plot", 
                                     "📊 Download Combined Visualization", 
                                     class = "btn-primary-custom")
                  )
                )
              ),
              
              # Absence Reasons Tab
              tabPanel(
                title = tags$div(icon("user-clock"), "Absence Reasons"),
                value = "absence_reasons",
                fluidRow(
                  valueBoxOutput("absence_total_units", width = 2),
                  valueBoxOutput("absence_top_reason", width = 3),
                  valueBoxOutput("absence_avg_pct", width = 2),
                  valueBoxOutput("absence_total_cats", width = 2),
                  valueBoxOutput("absence_viz_type", width = 3)
                ),
                box(
                  title = tags$div(icon("chart-bar"), "Absence Reasons Visualization"), 
                  width = 12, solidHeader = TRUE, status = "info",
                  plotOutput("absence_heatmap", height = "600px"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_absence_plot", 
                                     "📊 Download Absence Visualization", 
                                     class = "btn-primary-custom")
                  )
                ),
                tabsetPanel(
                  tabPanel(tags$div(icon("table"), "Percentage Data"), DTOutput("absence_table_pct")),
                  tabPanel(tags$div(icon("table"), "Raw Counts"), DTOutput("absence_table_raw")),
                  tabPanel(tags$div(icon("chart-line"), "Top 5 Reasons"), plotOutput("absence_top5_plot", height = "400px"))
                )
              ),
              
              # Non-Compliance Reasons Tab
              tabPanel(
                title = tags$div(icon("gavel"), "Non-Compliance Reasons"),
                value = "nc_reasons",
                fluidRow(
                  valueBoxOutput("nc_total_units", width = 2),
                  valueBoxOutput("nc_top_reason", width = 3),
                  valueBoxOutput("nc_avg_pct", width = 2),
                  valueBoxOutput("nc_total_cats", width = 2),
                  valueBoxOutput("nc_viz_type", width = 3)
                ),
                box(
                  title = tags$div(icon("chart-bar"), "Non-Compliance Reasons Visualization"), 
                  width = 12, solidHeader = TRUE, status = "info",
                  plotOutput("nc_heatmap", height = "600px"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_nc_plot", "📊 Download NC Visualization", class = "btn-primary-custom")
                  )
                ),
                tabsetPanel(
                  tabPanel(tags$div(icon("table"), "Percentage Data"), DTOutput("nc_table_pct")),
                  tabPanel(tags$div(icon("table"), "Raw Counts"), DTOutput("nc_table_raw")),
                  tabPanel(tags$div(icon("chart-line"), "Top 5 Reasons"), plotOutput("nc_top5_plot", height = "400px"))
                )
              ),
              
              # Traditional Reasons Tab
              tabPanel(
                title = tags$div(icon("history"), "Traditional Reasons"),
                value = "traditional_reasons",
                fluidRow(
                  valueBoxOutput("trad_total_units", width = 2),
                  valueBoxOutput("trad_top_reason", width = 3),
                  valueBoxOutput("trad_avg_pct", width = 2),
                  valueBoxOutput("trad_total_cats", width = 2),
                  valueBoxOutput("trad_viz_type", width = 3)
                ),
                box(
                  title = tags$div(icon("chart-bar"), "Traditional Reasons Visualization"), 
                  width = 12, solidHeader = TRUE, status = "info",
                  plotOutput("traditional_heatmap", height = "600px"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_traditional_plot", "📊 Download Traditional Visualization", class = "btn-primary-custom")
                  )
                ),
                tabsetPanel(
                  tabPanel(tags$div(icon("table"), "Percentage Data"), DTOutput("traditional_table_pct")),
                  tabPanel(tags$div(icon("table"), "Raw Counts"), DTOutput("traditional_table_raw")),
                  tabPanel(tags$div(icon("chart-line"), "Top 5 Reasons"), plotOutput("traditional_top5_plot", height = "400px"))
                )
              ),
              
              # Comparative Summary Tab
              tabPanel(
                title = tags$div(icon("chart-line"), "Comparative Summary"),
                value = "comparative_summary",
                fluidRow(
                  valueBoxOutput("summary_absence_cats", width = 3),
                  valueBoxOutput("summary_nc_cats", width = 3),
                  valueBoxOutput("summary_trad_cats", width = 3),
                  valueBoxOutput("summary_total_cats", width = 3)
                ),
                box(
                  title = tags$div(icon("table"), "Comparative Metrics by Reason Type"), 
                  width = 12, solidHeader = TRUE, status = "info",
                  DTOutput("comparative_summary_table"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_comparative_summary", "💾 Download Comparison", class = "btn-primary-custom")
                  )
                ),
                box(
                  title = tags$div(icon("chart-bar"), "Top Reasons Comparison"), 
                  width = 12, solidHeader = TRUE, status = "success",
                  plotOutput("top_reasons_comparison", height = "500px")
                )
              ),
              
              # Hierarchical View Tab
              tabPanel(
                title = tags$div(icon("sitemap"), "Hierarchical View"),
                value = "hierarchical_view",
                box(
                  title = tags$div(icon("sitemap"), "Country > Province > District Breakdown"), 
                  width = 12, solidHeader = TRUE, status = "success",
                  conditionalPanel(
                    condition = "input.reasons_geo_level == 'country'",
                    tabsetPanel(
                      tabPanel(tags$div(icon("user-clock"), "Absence Reasons"), DTOutput("hierarchical_absence_table")),
                      tabPanel(tags$div(icon("gavel"), "Non-Compliance Reasons"), DTOutput("hierarchical_nc_table")),
                      tabPanel(tags$div(icon("history"), "Traditional Reasons"), DTOutput("hierarchical_traditional_table"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.reasons_geo_level != 'country'",
                    div(style = "text-align: center; padding: 50px;",
                        h4("Hierarchical view is available only at Country level"),
                        p("Please select 'Country Level' in settings to see the hierarchical breakdown"))
                  )
                )
              ),
              
              # Priority Drill‑down Tab (with bar chart + tables)
              tabPanel(
                title = tags$div(icon("sitemap"), "Priority Drill‑down"),
                value = "priority_drilldown",
                fluidRow(
                  column(
                    width = 3,
                    box(
                      title = "Drill‑down Settings", width = 12, solidHeader = TRUE, status = "warning",
                      
                      checkboxInput("drilldown_enable", "Enable Priority Drill‑down", value = FALSE),
                      
                      conditionalPanel(
                        condition = "input.drilldown_enable == true",
                        
                        selectInput("drilldown_reason", "Priority Reason:",
                                    choices = c("Child Absent" = "childabsent",
                                                "Non‑compliance" = "non_compliance"),
                                    selected = "childabsent"),
                        
                        numericInput("drilldown_priority_threshold", "Country Threshold (%)",
                                     value = 30, min = 0, max = 100, step = 1),
                        
                        numericInput("drilldown_province_threshold", "Province Threshold (%)",
                                     value = 10, min = 0, max = 100, step = 1),
                        
                        numericInput("drilldown_district_threshold", "District Threshold (%)",
                                     value = 5, min = 0, max = 100, step = 1),
                        
                        checkboxInput("drilldown_dynamic", "Use dynamic (Pareto) thresholds?",
                                      value = FALSE),
                        
                        helpText("Dynamic thresholds select top units until cumulative 80% is reached.")
                      )
                    )
                  ),
                  column(
                    width = 9,
                    
                    # Priority Plot Box with Download Button
                    box(
                      title = div(
                        "Priority Breakdown: Country → Province → District",
                        div(style = "float:right; margin-left:10px;",
                            downloadButton("download_priority_plot", 
                                           label = "Download PNG", 
                                           icon = icon("download"),
                                           class = "btn-primary-custom btn-sm")
                        )
                      ), 
                      width = 12,
                      solidHeader = TRUE, 
                      status = "success",
                      plotOutput("priority_plot", height = "600px"),
                      
                      # Footnote placeholder - will be filled by server
                      div(
                        style = "margin-top: 15px; padding: 12px; background-color: #f8f9fa; border-left: 4px solid #2c3e50; border-radius: 4px; font-size: 13px; line-height: 1.5;",
                        uiOutput("priority_footnote")
                      )
                    ),
                    
                    # District Trend Analysis as Table
                    conditionalPanel(
                      condition = "input.drilldown_enable == true && output.drilldown_has_trend == true",
                      box(
                        title = div(
                          "District Trend Analysis",
                          div(style = "float:right; margin-left:10px;",
                              downloadButton("download_trend_table", 
                                             label = "Download Excel", 
                                             icon = icon("file-excel"),
                                             class = "btn-success-custom btn-sm")
                          )
                        ),
                        width = 12,
                        solidHeader = TRUE,
                        status = "warning",
                        DTOutput("district_trend_table", height = "400px"),
                        div(
                          style = "margin-top: 10px; font-size: 12px; color: #666;",
                          "Monthly trend analysis showing child absent rates for selected districts. ",
                          "Green = Decreasing trend, Red = Increasing trend, Gray = Stable."
                        )
                      )
                    ),
                    
                    # Block Summary - FIXED: using textOutput instead of input in UI
                    conditionalPanel(
                      condition = "input.drilldown_enable == true && output.drilldown_has_data == true",
                      box(
                        title = div(
                          textOutput("block_summary_title"),
                          div(style = "float:right; margin-left:10px;",
                              downloadButton("download_block_summary", 
                                             label = "Download Excel", 
                                             icon = icon("file-excel"),
                                             class = "btn-success-custom btn-sm")
                          )
                        ),
                        width = 12,
                        solidHeader = TRUE,
                        status = "info",
                        collapsible = TRUE,
                        collapsed = FALSE,
                        DTOutput("block_summary_table")
                      )
                    ),
                    
                    fluidRow(
                      column(
                        width = 6,
                        box(
                          title = div(
                            "Selected Provinces",
                            div(style = "float:right; margin-left:10px;",
                                downloadButton("download_drilldown_provinces", 
                                               label = "Download Excel", 
                                               icon = icon("file-excel"),
                                               class = "btn-success-custom btn-sm")
                            )
                          ), 
                          width = 12, 
                          collapsible = TRUE, 
                          solidHeader = TRUE,
                          DTOutput("drilldown_provinces")
                        )
                      ),
                      column(
                        width = 6,
                        box(
                          title = div(
                            "Selected Districts",
                            div(style = "float:right; margin-left:10px;",
                                downloadButton("download_drilldown_districts", 
                                               label = "Download Excel", 
                                               icon = icon("file-excel"),
                                               class = "btn-success-custom btn-sm")
                            )
                          ), 
                          width = 12, 
                          collapsible = TRUE, 
                          solidHeader = TRUE,
                          DTOutput("drilldown_districts")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # ⚠️ UNRESOLVED CASES (Updated with Block Type Selection)
    # ============================================================
    tabItem(
      tabName = "unresolved",
      fluidRow(
        conditionalPanel(
          condition = "input.tabs == 'unresolved'",
          column(
            width = 3,
            box(
              title = tags$div(icon("cog"), "Unresolved Cases Settings"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "primary",
              
              selectInput("unresolved_time_type", "Timeframe Type:",
                          choices = c("Last X Months" = "months", 
                                      "Specific Year" = "year",
                                      "All Data" = "all"),
                          selected = "months"),
              
              conditionalPanel(
                condition = "input.unresolved_time_type == 'months'",
                numericInput("unresolved_months", "Number of Months:", 
                             value = 6, min = 1, max = 24)
              ),
              
              conditionalPanel(
                condition = "input.unresolved_time_type == 'year'",
                numericInput("unresolved_year", "Year:", 
                             value = year(Sys.Date()), min = 2020, max = year(Sys.Date()))
              ),
              
              # ADDED: Block Type Selection
              selectInput("unresolved_block_type", "Block Type:",
                          choices = c("AFRO Blocks" = "afro", "IST Blocks" = "ist"),
                          selected = "afro"),
              
              # MODIFIED: Dynamic Block Selection
              selectInput("unresolved_block", "Selected Block:",
                          choices = c("All", names(afro_blocks)),
                          selected = "All"),
              
              # Country filter - DROPDOWN
              selectizeInput("unresolved_countries", "Countries (Optional):",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more countries',
                               maxItems = 15,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'
                             )),
              
              # Province filter - DROPDOWN
              selectizeInput("unresolved_provinces", "Province(s) (Optional):",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more provinces',
                               maxItems = 20,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'
                             )),
              
              # District filter - DROPDOWN
              selectizeInput("unresolved_districts", "District(s) (Optional):",
                             choices = character(0),
                             multiple = TRUE,
                             options = list(
                               placeholder = 'Select one or more districts',
                               maxItems = 30,
                               plugins = list('remove_button'),
                               dropdownParent = 'body'
                             )),
              
              # Filter status display
              tags$div(
                style = "margin-top: 10px; font-size: 12px; color: #666;",
                textOutput("unresolved_filter_status")
              ),
              
              selectInput("unresolved_reason", "Select Reason:",
                          choices = c("Non compliance", "Child absent", "House not visited", 
                                      "Child was asleep", "Child is visitor", "Vaccinated NFM",
                                      "Child not born", "Security", "Other"),
                          selected = "Non compliance"),
              
              actionButton("unresolved_analyze", "⚠️ Analyze Unresolved Cases", 
                           class = "btn-primary-custom")
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'unresolved'",
          column(
            width = 9,
            fluidRow(
              valueBoxOutput("unresolved_total_cases", width = 4),
              valueBoxOutput("unresolved_countries_box", width = 4),
              valueBoxOutput("unresolved_districts_box", width = 4)
            ),
            
            box(
              title = tags$div(icon("exclamation-triangle"), "Unresolved Cases Analysis"), 
              width = 12, 
              solidHeader = TRUE, 
              status = "info",
              
              tabsetPanel(
                tabPanel(
                  tags$div(icon("map"), "Reason Map"), 
                  plotOutput("unresolved_map", height = "600px"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_unresolved_map", 
                                     "🗺️ Download Map", 
                                     class = "btn-primary-custom")
                  )
                ),
                tabPanel(
                  tags$div(icon("table"), "Summary Data"), 
                  DTOutput("unresolved_data_table"),
                  div(style = "text-align: center; margin-top: 15px;",
                      downloadButton("download_unresolved_data", 
                                     "💾 Download Data", 
                                     class = "btn-primary-custom")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================
    # 🗺️ IM SETTLEMENT MAPS (Includes both Map and Data Explorer)
    # ============================================================
    tabItem(
      tabName = "settlement_maps",
      conditionalPanel(
        condition = "input.tabs == 'settlement_maps'",
        # Tabset for switching between Map and Data views
        tabsetPanel(
          id = "settlement_tabs",
          type = "tabs",
          
          # Map Tab
          tabPanel(
            "Settlement Map",
            fluidRow(
              box(
                title = "Input Parameters", width = 4, solidHeader = TRUE, status = "primary",
                selectInput("file_id", "File ID", choices = file_list_no_ext),
                dateInput("selected_date", "Select Date", 
                          value = Sys.Date(),
                          min = as.Date("2000-01-01"),
                          max = Sys.Date()),
                selectInput("ctry", "Country", choices = NULL),
                selectInput("Province", "Province", choices = NULL),
                selectInput("District", "District", choices = NULL),
                selectInput("Facility", "Facility", choices = NULL),
                selectInput("response", "Response", choices = NULL),
                selectInput("roundNumber", "Round Number", choices = NULL),
                selectInput("settlement", "Settlement", choices = NULL),
                numericInput("bbox_m", "Bounding Box Margin (meters)", value = 300),
                actionButton("generate", "Generate Map"),
                downloadButton("download_map", "Download Map", style = "margin-top: 10px;")
              ),
              box(
                title = "Map Output", width = 8, solidHeader = TRUE, status = "primary",
                plotOutput("mapPlot", height = "600px")
              )
            )
          ),
          
          # Data Explorer Tab
          tabPanel(
            "Settlement Data",
            fluidRow(
              box(
                title = "Data Filters", width = 3, solidHeader = TRUE, status = "primary",
                selectInput("settlement_file_id", "File ID", choices = file_list_no_ext),
                dateRangeInput("settlement_date_range", "Date Range:",
                               start = Sys.Date() - 30,
                               end = Sys.Date(),
                               min = as.Date("2000-01-01"),
                               max = Sys.Date()),
                selectInput("settlement_ctry", "Country", choices = NULL, multiple = TRUE),
                selectInput("settlement_province", "Province", choices = NULL, multiple = TRUE),
                selectInput("settlement_district", "District", choices = NULL, multiple = TRUE),
                selectInput("settlement_facility", "Facility", choices = NULL, multiple = TRUE),
                selectInput("settlement_response", "Response", choices = NULL, multiple = TRUE),
                selectInput("settlement_roundNumber", "Round Number", choices = NULL, multiple = TRUE),
                selectInput("settlement_settlement", "Settlement", choices = NULL, multiple = TRUE),
                actionButton("settlement_apply_filters", "Apply Filters", class = "btn-primary"),
                actionButton("settlement_reset_filters", "Reset Filters", class = "btn-warning"),
                br(), br(),
                downloadButton("settlement_download_csv", "Download as CSV", style = "margin: 2px;"),
                downloadButton("settlement_download_excel", "Download as Excel", style = "margin: 2px;")
              ),
              box(
                title = "Data Preview", width = 9, solidHeader = TRUE, status = "primary",
                div(
                  style = "overflow-x: auto; max-height: 600px;",
                  DTOutput("settlement_data_table")
                ),
                br(),
                verbatimTextOutput("settlement_data_summary")
              )
            )
          )
        )
      )
    ),
    
    
    # ============================================================
    # 📊 POWERPOINT SLIDE GENERATION
    # ============================================================
    tabItem(
      tabName = "generate_slides",
      fluidRow(
        column(
          width = 12,
          # PowerPoint Settings Box - Initially hidden
          conditionalPanel(
            condition = "input.tabs == 'generate_slides'",
            box(
              title = tags$div(icon("cog"), "PowerPoint Slide Generation Settings"),
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              id = "slide_settings_box",
              
              # Slide Overview Filters
              h4("Slide Overview Filters"),
              fluidRow(
                column(6,
                       selectInput("slide_time_type", "Timeframe Type:",
                                   choices = c("Last X Months" = "months",
                                               "Specific Year(s)" = "years",
                                               "All Data" = "all"),
                                   selected = "months"),
                       conditionalPanel(
                         condition = "input.slide_time_type == 'months'",
                         numericInput("slide_months", "Number of Months:", value = 6, min = 1, max = 36)
                       ),
                       conditionalPanel(
                         condition = "input.slide_time_type == 'years'",
                         textInput("slide_years", "Year(s):", 
                                   placeholder = "e.g., 2023,2024",
                                   value = format(Sys.Date(), "%Y"))
                       ),
                       selectInput("slide_block", "AFRO Block:",
                                   choices = c("All", names(afro_blocks)), 
                                   selected = "All")
                ),
                column(6,
                       selectizeInput("slide_countries", "Countries (Optional):",
                                      choices = character(0),
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Select one or more countries',
                                        maxItems = 15,
                                        plugins = list('remove_button'),
                                        dropdownParent = 'body'
                                      )),
                       selectizeInput("slide_provinces", "Provinces (Optional):",
                                      choices = character(0),
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Select one or more provinces',
                                        maxItems = 20,
                                        plugins = list('remove_button'),
                                        dropdownParent = 'body'
                                      )),
                       selectizeInput("slide_districts", "Districts (Optional):",
                                      choices = character(0),
                                      multiple = TRUE,
                                      options = list(
                                        placeholder = 'Select one or more districts',
                                        maxItems = 30,
                                        plugins = list('remove_button'),
                                        dropdownParent = 'body'
                                      ))
                )
              ),
              
              # Analysis Selection
              h4("Select Analyses to Include in Slides"),
              fluidRow(
                column(4,
                       checkboxGroupInput("slide_analyses", "Analyses:",
                                          choices = c(
                                            "SIA Scope Analysis" = "scope",
                                            "Admin Data Overview" = "admin",
                                            "Missed Children Analysis" = "missed",
                                            "Coverage Analysis" = "coverage",
                                            "District Performance" = "district_perf",
                                            "LQAS Summary Maps" = "lqas_maps",
                                            "Reasons Analysis" = "reasons",
                                            "Unresolved Cases" = "unresolved"
                                          ),
                                          selected = c("scope", "admin", "missed", "coverage"))
                ),
                column(4,
                       checkboxGroupInput("slide_outputs", "Output Types:",
                                          choices = c(
                                            "Summary Tables" = "tables",
                                            "Maps" = "maps",
                                            "Charts/Plots" = "charts",
                                            "Value Box Metrics" = "metrics"
                                          ),
                                          selected = c("tables", "maps", "metrics"))
                ),
                column(4,
                       textInput("slide_presentation_title", "Presentation Title:",
                                 value = "WHO AFRO SIA Dashboard Analysis"),
                       textInput("slide_author", "Author:",
                                 value = "WHO AFRO DIM Team"),
                       selectInput("slide_template", "PowerPoint Template:",
                                   choices = c("Default WHO Template" = "default",
                                               "Minimal" = "minimal",
                                               "Corporate" = "corporate")),
                       
                       # Unresolved Cases Reason Selector
                       conditionalPanel(
                         condition = "input.slide_analyses.includes('unresolved')",
                         selectInput("slide_unresolved_reason", "Unresolved Cases - Show Specific Reason:",
                                     choices = c("All", "Non compliance", "Child absent", "House not visited", 
                                                 "Child was asleep", "Child is visitor", "Vaccinated NFM",
                                                 "Child not born", "Security", "Other"),
                                     selected = "All")
                       )
                )
              ),
              
              # Action Buttons
              fluidRow(
                column(12,
                       actionButton("generate_slides", "📊 Generate PowerPoint Slides", 
                                    class = "btn-primary-custom",
                                    style = "width:100%; font-weight:bold; margin-top:20px;")
                )
              )
            )
          ),
          
          # Slide Preview Box - Initially hidden, appears after generation
          conditionalPanel(
            condition = "input.tabs == 'generate_slides' && output.slides_generated",
            box(
              title = tags$div(icon("eye"), "Slide Preview"),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              id = "slide_preview_box",
              
              uiOutput("slide_preview_ui"),
              
              # Slide navigation
              conditionalPanel(
                condition = "output.slides_generated",
                fluidRow(
                  column(12, style = "text-align:center; margin-top:15px;",
                         actionButton("prev_slide", "◀ Previous", class = "btn-default"),
                         actionButton("next_slide", "Next ▶", class = "btn-default"),
                         textOutput("slide_counter", inline = TRUE)
                  )
                )
              )
            )
          ),
          
          # Initial message when no slides generated yet
          conditionalPanel(
            condition = "input.tabs == 'generate_slides' && !output.slides_generated",
            box(
              title = tags$div(icon("info-circle"), "Getting Started"),
              width = 12,
              status = "info",
              solidHeader = TRUE,
              tags$div(
                style = "text-align: center; padding: 40px;",
                tags$h4("Ready to Create Your Presentation?"),
                tags$p("Configure your settings in the panel above and click 'Generate PowerPoint Slides' to create your presentation."),
                tags$p("You can select which analyses to include and customize the output to your needs."),
                tags$br(),
                tags$div(
                  style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #0072B2;",
                  tags$h5("💡 Tips for Best Results:"),
                  tags$ul(
                    style = "text-align: left;",
                    tags$li("Start with specific time periods rather than 'All Data' for faster generation"),
                    tags$li("Select 3-5 key analyses to keep your presentation focused"),
                    tags$li("Use geographic filters to focus on specific regions or countries"),
                    tags$li("Preview each slide before downloading the final presentation")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    
    
    # ============================================================
    # ℹ️ QUICK START GUIDE (Fixed with conditional panel)
    # ============================================================
    tabItem(
      tabName = "guide",
      conditionalPanel(  # ADD THIS CONDITIONAL PANEL
        condition = "input.tabs == 'guide'",
        fluidPage(
          # ---- HEADER BANNER ----
          tags$div(
            style = "
          background-color: #0072B2; 
          color: white; 
          padding: 20px; 
          border-radius: 8px; 
          text-align: center; 
          margin-bottom: 20px;
        ",
            tags$img(
              src = 'https://www.who.int/ResourcePackages/WHO/assets/dist/images/logos/en/h-logo-blue.svg',
              height = '50px', 
              style = 'margin-bottom: 10px;'
            ),
            h2("WHO AFRO SIA Dashboard"),
            p("Integrated Digital Platform for Monitoring Supplementary Immunization Activities (SIA) Across the WHO African Region")
          ),
          
          # ---- OVERVIEW + QR CODE ----
          fluidRow(
            column(
              width = 7,
              tags$div(
                class = "alert-info",
                h4("About this Dashboard"),
                p("The WHO AFRO SIA Dashboard provides a comprehensive view of Supplementary Immunization Activities (SIA) data across the African Region."),
                p("It integrates multiple data streams: including LQAS, administrative coverage, missed children, and performance monitoring into a single, interactive platform for real-time analysis and visualization."),
                br(),
                h4("Key Objectives"),
                tags$ul(
                  tags$li("Track and compare SIA performance across countries and regions"),
                  tags$li("Support data-driven decision-making and field coordination"),
                  tags$li("Improve accountability and timeliness of data reporting"),
                  tags$li("Facilitate continuous improvement in campaign quality and coverage")
                )
              )
            ),
            
            column(
              width = 5,
              tags$div(
                style = "
              text-align: center; 
              border: 2px solid #0072B2; 
              border-radius: 12px; 
              padding: 20px; 
              background-color: #f8f9fa;
            ",
                h4("📱 Access the Dashboard Anywhere"),
                p("Scan this QR code to open the live dashboard on your mobile device:"),
                tags$img(
                  src = "AFRO_SIA_Dashboard_QR.png",
                  width = "250px",
                  alt = "WHO AFRO SIA Dashboard QR Code",
                  style = "border: 3px solid #0072B2; border-radius: 12px; margin-top: 10px;"
                ),
                br(), br(),
                tags$p(
                  style = "text-align:center; font-size:13px; color:gray;",
                  paste0("© WHO AFRO | ", DASHBOARD_VERSION, " | Updated ", DASHBOARD_DATE)
                )
              )
            )
          ),
          
          # ---- QUICK START SECTIONS ----
          br(),
          tags$div(
            class = "alert-info",
            h4("Quick Start Guide"),
            tags$p("This dashboard provides comprehensive analysis of Supplementary Immunization Activities across the AFRO region with optimized performance and memory management."),
            
            h4("Key Features"),
            tags$div(class = "row",
                     tags$div(class = "col-md-6",
                              tags$ul(
                                tags$li(tags$b("SIA Scope Analysis:"), " View vaccination campaign scope and coverage by geographic area"),
                                tags$li(tags$b("Missed Children:"), " Analyze children missed during vaccination campaigns"),
                                tags$li(tags$b("Coverage Analysis:"), " Examine vaccination coverage rates by gender and geography"),
                                tags$li(tags$b("🗺️ District Performance:"), " Monitor LQAS performance at district level"),
                                tags$li(tags$b("🌍 IM Settlement Maps:"), " Visualize settlement-level footprint maps to locate missed children and guide field teams effectively")
                              )
                     ),
                     tags$div(class = "col-md-6",
                              tags$ul(
                                tags$li(tags$b("🌍 LQAS Summary Maps:"), " Visual overview of campaign performance"),
                                tags$li(tags$b("Reasons Analysis:"), " Understand reasons for non-vaccination"),
                                tags$li(tags$b("Unresolved Cases:"), " Track unresolved non-compliance cases"),
                                tags$li(tags$b("Admin Data:"), " Comprehensive administrative data overview"),
                                tags$li(tags$b("Dynamic Reports & Exports:"), " Download results as maps, tables, or PowerPoint summaries")
                              )
                     )
            ),
            
            h4("Performance Tips"),
            tags$ul(
              tags$li("Start with specific time periods rather than 'All Data'"),
              tags$li("Use AFRO Block filters to focus on specific regions"),
              tags$li("Limit country selections to 10–15 countries for optimal performance"),
              tags$li("Use the refresh button to clear memory and restart analysis")
            ),
            
            h4("Technical Features"),
            tags$ul(
              tags$li("Stable Data Loading"),
              tags$li("Performance Optimized"),
              tags$li("Error Protected"),
              tags$li("📱 Responsive design for all devices")
            ),
            
            h4("Data Sources"),
            tags$p("This dashboard uses data from the PEP SIA repository and includes:"),
            tags$ul(
              tags$li("Vaccination campaign data"),
              tags$li("LQAS survey results"),
              tags$li("Geographic shapefiles for mapping"),
              tags$li("District and settlement-level performance metrics"),
              tags$li("Administrative coverage data")
            ),
            
            h4("👥 Technical Support"),
            tags$p(
              "For technical assistance or data questions, please contact the WHO AFRO DIM Team:",
              br(),
              tags$a(href = "mailto:dimafro@who.int", "dimafro@who.int"),
              " | ",
              tags$a(href = "mailto:tourayk@who.int", "tourayk@who.int"),
              " | ",
              tags$a(href = "mailto:mwanzam@who.int", "mwanzam@who.int")
            ),
            
            tags$div(style = "
          text-align: center; 
          margin-top: 20px; 
          padding: 15px; 
          background-color: #f8f9fa; 
          border-radius: 8px;
        ",
                     tags$p(tags$b("System Status:"), " ✅ Stable | 🟢 Optimized"),
                     tags$p(tags$em("Version:"), DASHBOARD_VERSION),
                     tags$p(tags$em("Last Updated:"), DASHBOARD_DATE),
                     tags$p(tags$em("Environment:"), if (interactive()) "Development (Sample Data)" else "Production (Full Data)")
            )
          ),
          
          # ---- DEBUG PANEL (Collapsed by default) ----
          box(
            title = "🔧 System Debug Information",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            tags$div(
              style = "font-size: 12px;",
              p("This section shows technical information for debugging purposes."),
              verbatimTextOutput("busy_debug")
            )
          )
        )
      )
    )
  )
)



# ============================================================
# ---- Helper: Safe ValueBox Rendering Function ----
# ============================================================
vbox_fun <- function(value, subtitle, icon_name, color = "blue") {
  formatted_value <- if (is.null(value) || all(is.na(value))) {
    "—"
  } else if (is.numeric(value) || inherits(value, "Date")) {
    format(value, trim = TRUE, big.mark = ",")
  } else {
    as.character(value)
  }
  
  valueBox(
    value = formatted_value,
    subtitle = subtitle,
    icon = icon(icon_name),
    color = color
  )
}




# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  options(error = function() {
    cat("\n--- Full traceback ---\n")
    print(sys.calls())
    cat("----------------------\n")
  })
  
  # ============================================================
  # 🚀 PERFORMANCE & MEMORY OPTIMIZATION
  # ============================================================
  
  
  # Memory management - ENHANCED VERSION
  session$allowReconnect(TRUE)
  
  # CRITICAL: Future-specific memory optimizations
  options(future.globals.maxSize = 800 * 1024^2)  # Increase to 800MB
  options(future.fork.enable = FALSE)  # Disable forking (causes issues on some systems)
  options(future.wait.interval = 0.1)  # Faster response
  
  # Shiny options
  options(shiny.maxRequestSize = 50 * 1024^2)
  options(shiny.launch.browser = TRUE)
  options(shiny.fullstacktrace = FALSE)
  
  # Use sequential plan instead of multisession to reduce memory
  future::plan(future::sequential)  # ← CHANGE TO SEQUENTIAL
  
  # NOTE: this used to also run a reactiveTimer(30000) that forced a full gc()
  # sweep every 30 seconds for the entire life of every session, regardless of
  # activity. gc() is a stop-the-world blocking call; on a single shared R
  # process, that's a periodic full-process pause for every concurrent user,
  # independent of whether anyone is running a heavy analysis. R's own automatic
  # garbage collector already runs as needed, so this was pure added latency with
  # no real memory benefit. Removed; the targeted force_gc() calls below (after
  # specific heavy operations) are left in place.
  force_gc <- function() {
    invisible(gc(verbose = FALSE))
  }
  
  # ============================================================
  # 🚀 IMMEDIATE PERFORMANCE FIXES
  # ============================================================
  
  # Add debouncing to heavy inputs
  lqas_countries_debounced <- debounce(reactive(input$lqas_countries), 1000)
  perf_countries_debounced <- debounce(reactive(input$perf_countries), 1000) 
  scope_countries_debounced <- debounce(reactive(input$scope_countries), 1000)
  
  # Add explicit apply buttons for heavy tabs
  observeEvent(input$quick_apply_filters, {
    # Force garbage collection
    force_gc()
    
    # Run minimal processing
    showNotification("🔄 Applying filters...", type = "message", duration = 2)
  })
  
  
  # ---- SAFE FLEXSUPPORT (Shiny HTML converter) ----
  htmltools_value <- function(x, ...) {
    if (inherits(x, "flextable")) return(flextable::htmltools_value(x))
    if (inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list")) return(x)
    if (is.character(x) || is.numeric(x)) return(HTML(paste(x, collapse = " ")))
    if (is.null(x)) return(tags$div())
    HTML(paste(capture.output(str(x)), collapse = "\n"))
  }
  
  # ============================================================
  # 🛡️ ERROR HANDLING & PROGRESS MANAGEMENT
  # ============================================================
  
  # Safe analysis wrapper with progress and error handling
  safe_analysis <- function(expr, operation_name = "Analysis") {
    tryCatch({
      # Show progress
      progress <- Progress$new(session, min = 0, max = 1)
      progress$set(message = paste("Running", operation_name, "..."), value = 0.3)
      on.exit(progress$close())
      
      # Execute with timeout protection
      result <- withCallingHandlers(
        expr,
        warning = function(w) {
          message("Warning in ", operation_name, ": ", w$message)
        }
      )
      
      progress$set(value = 1.0)
      showNotification(paste("✅", operation_name, "completed!"), type = "message")
      force_gc()
      return(result)
      
    }, error = function(e) {
      showNotification(paste("❌ Error in", operation_name, ":", e$message), 
                       type = "error", duration = 10)
      return(NULL)
    })
  }
  
  # ============================================================
  # 🔄 REACTIVE VALUES WITH MEMORY MANAGEMENT
  # ============================================================
  
  # Reactive values with automatic cleanup
  scope_analysis <- reactiveVal(NULL)
  missed_analysis <- reactiveVal(NULL)
  coverage_analysis <- reactiveVal(NULL)
  district_perf_analysis <- reactiveVal(NULL)
  lqas_analysis <- reactiveVal(NULL)
  reasons_analysis <- reactiveVal(NULL)
  unresolved_analysis <- reactiveVal(NULL)
  admin_overview <- reactiveVal(NULL)
  
  # Cleanup reactive values when session ends
  session$onSessionEnded(function() {
    rm(list = ls())
    force_gc()
  })
  
  # Enhanced memory management for LQAS analysis
  observe({
    # Clean up LQAS analysis when tab changes
    if (input$tabs != "lqas_maps" && !is.null(lqas_analysis())) {
      lqas_analysis(NULL)
      force_gc()
    }
  })
  
  # Auto garbage collection every 2 minutes
  observe({
    invalidateLater(120000) # 2 minutes
    force_gc()
  })
  
  # ============================================================
  # 🌍 OPTIMIZED COUNTRY SELECTION UIs
  # ============================================================
  
  # Generic country selection function with performance optimizations
  create_country_selector <- function(block_input, data_source, output_id, label = "Countries (optional):", country_column = "country") {
    renderUI({
      block <- input[[block_input]]
      
      # Get available countries with caching
      isolate({
        choices <- if (!is.null(block) && block != "All") {
          afro_blocks[[block]]
        } else {
          # Handle different column names in different datasets
          if (country_column %in% names(data_source)) {
            available_countries <- sort(unique(data_source[[country_column]]))
          } else if ("Country" %in% names(data_source)) {
            available_countries <- sort(unique(data_source$Country))
          } else if ("ADM0_NAME" %in% names(data_source)) {
            available_countries <- sort(unique(data_source$ADM0_NAME))
          } else {
            # Fallback to empty list
            character(0)
          }
          
          # Limit choices for large datasets
          if (length(available_countries) > 50) {
            # For large datasets, show most frequent countries first
            if (country_column %in% names(data_source)) {
              country_counts <- table(data_source[[country_column]])
            } else if ("Country" %in% names(data_source)) {
              country_counts <- table(data_source$Country)
            } else {
              country_counts <- table(available_countries)
            }
            top_countries <- names(sort(country_counts, decreasing = TRUE))[1:30]
            top_countries
          } else {
            available_countries
          }
        }
      })
      
      selectizeInput(
        output_id, label,
        choices = choices, 
        multiple = TRUE,
        options = list(
          placeholder = 'Select countries...',
          maxItems = 15,
          plugins = list('remove_button')
        )
      )
    })
  }
  
  # Update all country selection UIs with correct column names
  output$scope_countries_ui <- create_country_selector("scope_block", scope, "scope_countries", country_column = "country")
  output$missed_countries_ui <- create_country_selector("missed_block", dat, "missed_countries", country_column = "country")
  output$coverage_countries_ui <- create_country_selector("coverage_block", dat, "coverage_countries", country_column = "country")
  output$perf_countries_ui <- create_country_selector("perf_block", dat, "perf_countries", country_column = "country")
  output$lqas_countries_ui <- create_country_selector("lqas_block", dat, "lqas_countries", country_column = "country")
  output$reasons_countries_ui <- create_country_selector("reasons_block", dat, "reasons_countries", country_column = "country")
  output$unresolved_countries_ui <- create_country_selector("unresolved_block", dat, "unresolved_countries", country_column = "country")
  output$admin_countries_ui <- create_country_selector("admin_block", admin_data, "admin_countries", "Countries (optional):", country_column = "Country")
  
  
  # ============================================================
  # 📊 LQAS REPOSITORY OVERVIEW
  # ============================================================
  
  # ADDED: First, let's check the data structure at startup
  observe({
    cat("=== DATASET STRUCTURE ===\n")
    cat("Total rows in dat:", nrow(dat), "\n")
    cat("Date column name:", ifelse("round_start_date" %in% names(dat), "round_start_date exists", "NOT FOUND"), "\n")
    if ("round_start_date" %in% names(dat)) {
      cat("Date column class:", class(dat$round_start_date), "\n")
      cat("Sample dates:", paste(head(dat$round_start_date, 3), collapse = ", "), "\n")
      cat("Date range in data:", paste(range(dat$round_start_date, na.rm = TRUE), collapse = " to "), "\n")
    }
    cat("========================\n")
  })
  
  # ADDED: Reactive block choices
  overview_block_choices <- reactive({
    if (input$overview_block_type == "afro") {
      return(afro_blocks)
    } else {
      return(ist_blocks)
    }
  })
  
  # ADDED: Reactive block names
  overview_block_names <- reactive({
    if (input$overview_block_type == "afro") {
      return(names(afro_blocks))
    } else {
      return(names(ist_blocks))
    }
  })
  
  # ADDED: Update block selection when block type changes
  observe({
    updateSelectizeInput(session, "overview_block", 
                         choices = c("All", overview_block_names()))
  })
  
  # ---- Dynamic Country Filter ----
  output$overview_countries_ui <- renderUI({
    block <- input$overview_block
    choices <- if (!is.null(block) && block != "All") {
      overview_block_choices()[[block]]
    } else {
      available_countries <- sort(unique(dat$country))
      if (length(available_countries) > 40) {
        country_counts <- table(dat$country)
        names(sort(country_counts, decreasing = TRUE))[1:30]
      } else {
        available_countries
      }
    }
    
    selectizeInput(
      "overview_countries", "Countries (optional):",
      choices = choices, multiple = TRUE,
      options = list(
        placeholder = 'Select countries...', 
        maxItems = 12,
        dropdownParent = 'body',
        openOnFocus = FALSE
      )
    )
  })
  
  # ---- Dynamic Province Filter ----
  output$overview_provinces_ui <- renderUI({
    req(dat)
    
    selected_countries <- input$overview_countries
    
    if (!is.null(selected_countries) && length(selected_countries) > 0) {
      available_provinces <- dat %>%
        dplyr::filter(country %in% selected_countries) %>%
        dplyr::pull(province) %>%
        unique() %>%
        sort()
    } else if (!is.null(input$overview_block) && input$overview_block != "All") {
      block_countries <- overview_block_choices()[[input$overview_block]]
      available_provinces <- dat %>%
        dplyr::filter(country %in% block_countries) %>%
        dplyr::pull(province) %>%
        unique() %>%
        sort()
    } else {
      available_provinces <- sort(unique(dat$province))
    }
    
    selectizeInput(
      "overview_provinces", "Provinces (optional):",
      choices = available_provinces, 
      multiple = TRUE,
      options = list(
        placeholder = 'Select provinces...', 
        maxItems = 10,
        dropdownParent = 'body',
        openOnFocus = FALSE
      )
    )
  })
  
  # ---- Server-side District Filter (FIXED) ----
  # Reactive to get filtered districts based on current selections
  overview_district_options <- reactive({
    req(dat)
    
    selected_countries <- input$overview_countries
    selected_provinces <- input$overview_provinces
    
    # Start with filtered data
    filtered_data <- dat
    
    # Apply country filter if selected
    if (!is.null(selected_countries) && length(selected_countries) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(country %in% selected_countries)
    } else if (!is.null(input$overview_block) && input$overview_block != "All") {
      # Apply block filter if no countries selected
      block_countries <- overview_block_choices()[[input$overview_block]]
      filtered_data <- filtered_data %>%
        dplyr::filter(country %in% block_countries)
    }
    
    # Apply province filter if selected
    if (!is.null(selected_provinces) && length(selected_provinces) > 0) {
      filtered_data <- filtered_data %>%
        dplyr::filter(province %in% selected_provinces)
    }
    
    # Get unique districts
    districts <- filtered_data %>%
      dplyr::distinct(district) %>%
      dplyr::filter(!is.na(district)) %>%
      dplyr::arrange(district) %>%
      dplyr::pull(district)
    
    return(districts)
  })
  
  # Observer to update district selector with server-side processing
  observe({
    districts <- overview_district_options()
    
    # Update with server-side selectize
    updateSelectizeInput(
      session, 
      "overview_districts",
      choices = districts,
      server = TRUE,  # CRITICAL: This enables server-side processing
      options = list(
        placeholder = if(length(districts) > 0) 'Type to search districts...' else 'No districts available',
        maxItems = 15
      )
    )
  })
  
  # ---- Optimized Reactive Dataset ----
  overview_filtered <- reactive({
    req(dat)
    
    # Create a debug log
    debug_log <- list()
    
    safe_analysis({
      df <- dat
      
      # DEBUG: Log initial state
      debug_log$initial_rows <- nrow(df)
      debug_log$date_column_type <- class(df$round_start_date)
      debug_log$date_sample <- as.character(head(df$round_start_date, 3))
      
      # FIXED: Convert date column to proper Date format
      if (!inherits(df$round_start_date, "Date")) {
        original_dates <- df$round_start_date
        
        date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", 
                          "%d-%m-%Y", "%Y/%m/%d", "%Y.%m.%d",
                          "%d %b %Y", "%d %B %Y", "%b %d, %Y")
        
        converted <- NULL
        successful_format <- NULL
        
        for (fmt in date_formats) {
          test_conversion <- as.Date(original_dates, format = fmt)
          if (sum(!is.na(test_conversion)) > sum(!is.na(converted))) {
            converted <- test_conversion
            successful_format <- fmt
          }
        }
        
        if (!is.null(successful_format)) {
          df$round_start_date <- converted
          debug_log$date_conversion <- paste("Converted using format:", successful_format)
        } else {
          if (requireNamespace("lubridate", quietly = TRUE)) {
            df$round_start_date <- lubridate::anydate(original_dates)
            debug_log$date_conversion <- "Used lubridate::anydate"
          }
        }
      }
      
      # Remove NA dates
      rows_before <- nrow(df)
      df <- df %>% filter(!is.na(round_start_date))
      debug_log$na_dates_removed <- rows_before - nrow(df)
      debug_log$rows_after_na_removal <- nrow(df)
      
      # --- Time filtering ---
      debug_log$time_filter_type <- input$overview_time_type
      
      if (input$overview_time_type == "range" && !is.null(input$overview_daterange)) {
        start_date <- as.Date(input$overview_daterange[1])
        end_date   <- as.Date(input$overview_daterange[2])
        
        debug_log$date_range_selected <- paste(start_date, "to", end_date)
        debug_log$data_date_range <- paste(min(df$round_start_date), "to", max(df$round_start_date))
        
        rows_before_filter <- nrow(df)
        df <- df %>% 
          filter(round_start_date >= start_date & round_start_date <= end_date)
        debug_log$rows_after_date_filter <- nrow(df)
        debug_log$rows_filtered_by_date <- rows_before_filter - nrow(df)
        
      } else if (input$overview_time_type == "years") {
        years_input <- input$overview_years
        
        if (!is.null(years_input) && nzchar(trimws(years_input))) {
          years_clean <- trimws(years_input)
          years_list <- unlist(strsplit(years_clean, "[, ]+"))
          years_numeric <- suppressWarnings(as.numeric(years_list))
          years_numeric <- years_numeric[!is.na(years_numeric)]
          
          if (length(years_numeric) > 0) {
            df_years <- lubridate::year(df$round_start_date)
            df <- df[df_years %in% years_numeric, ]
            
            debug_log$year_filter_applied <- TRUE
            debug_log$selected_years <- years_numeric
            debug_log$available_years_in_data <- unique(df_years)
            debug_log$rows_after_year_filter <- nrow(df)
          } else {
            debug_log$year_filter_note <- "No valid numeric years found in input"
          }
        } else {
          debug_log$year_filter_note <- "No year input provided"
        }
      }
      
      # --- Block filtering ---
      if (!is.null(input$overview_block) && input$overview_block != "All") {
        valid_countries <- overview_block_choices()[[input$overview_block]]
        if (!is.null(valid_countries) && length(valid_countries) > 0) {
          rows_before <- nrow(df)
          df <- df |> dplyr::filter(!is.na(country) & country %in% valid_countries)
          debug_log$rows_after_block_filter <- nrow(df)
          debug_log$rows_filtered_by_block <- rows_before - nrow(df)
        }
      }
      
      # --- Country filtering ---
      if (!is.null(input$overview_countries) && length(input$overview_countries) > 0) {
        rows_before <- nrow(df)
        df <- df |> dplyr::filter(!is.na(country) & country %in% input$overview_countries)
        debug_log$rows_after_country_filter <- nrow(df)
        debug_log$rows_filtered_by_country <- rows_before - nrow(df)
      }
      
      # --- Province filtering ---
      if (!is.null(input$overview_provinces) && length(input$overview_provinces) > 0) {
        rows_before <- nrow(df)
        df <- df |> dplyr::filter(!is.na(province) & province %in% input$overview_provinces)
        debug_log$rows_after_province_filter <- nrow(df)
        debug_log$rows_filtered_by_province <- rows_before - nrow(df)
      }
      
      # --- District filtering ---
      if (!is.null(input$overview_districts) && length(input$overview_districts) > 0) {
        rows_before <- nrow(df)
        df <- df |> dplyr::filter(!is.na(district) & district %in% input$overview_districts)
        debug_log$rows_after_district_filter <- nrow(df)
        debug_log$rows_filtered_by_district <- rows_before - nrow(df)
      }
      
      # --- Performance filter ---
      if (!is.null(input$overview_perf_filter) && length(input$overview_perf_filter) > 0) {
        perf_summary <- df %>%
          dplyr::group_by(country, district) %>%
          dplyr::summarise(
            total_rounds = dplyr::n(),
            high_rounds  = sum(tolower(performance) == "high", na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Always_High = total_rounds > 0 & high_rounds == total_rounds,
            Never_High  = total_rounds > 0 & high_rounds == 0
          )
        
        selected_districts <- character()
        if ("high" %in% input$overview_perf_filter)
          selected_districts <- c(selected_districts, perf_summary$district[perf_summary$Always_High])
        if ("poor" %in% input$overview_perf_filter)
          selected_districts <- c(selected_districts, perf_summary$district[perf_summary$Never_High])
        
        rows_before <- nrow(df)
        df <- df %>% dplyr::filter(district %in% selected_districts)
        debug_log$rows_after_perf_filter <- nrow(df)
        debug_log$rows_filtered_by_perf <- rows_before - nrow(df)
      }
      
      debug_log$final_rows <- nrow(df)
      
      if (input$overview_debug) {
        assign("overview_debug_log", debug_log, envir = .GlobalEnv)
      }
      
      return(df)
    }, "Data Filtering")
  }) %>% bindCache(
    input$overview_time_type,
    input$overview_daterange,
    input$overview_years,
    input$overview_block_type,
    input$overview_block,
    input$overview_countries,
    input$overview_provinces,
    input$overview_districts,
    input$overview_perf_filter,
    input$overview_debug
  )
  
  # ---- Debug Info Output ----
  output$overview_debug_info <- renderText({
    req(overview_filtered())
    
    debug_text <- c(
      "=== DEBUG INFORMATION ===",
      paste("Total rows after filtering:", nrow(overview_filtered())),
      paste("Date column type:", class(overview_filtered()$round_start_date)),
      paste("Available date range in filtered data:", 
            paste(range(overview_filtered()$round_start_date, na.rm = TRUE), collapse = " to ")),
      "",
      "=== FILTER SETTINGS ===",
      paste("Time filter type:", input$overview_time_type)
    )
    
    if (input$overview_time_type == "range") {
      debug_text <- c(debug_text,
                      paste("Selected date range:", paste(input$overview_daterange, collapse = " to ")))
    } else {
      debug_text <- c(debug_text,
                      paste("Selected years input:", input$overview_years))
      
      if (exists("overview_debug_log") && !is.null(overview_debug_log$selected_years)) {
        debug_text <- c(debug_text,
                        paste("Parsed years:", paste(overview_debug_log$selected_years, collapse = ", ")))
      }
    }
    
    debug_text <- c(debug_text,
                    paste("Selected block:", input$overview_block),
                    paste("Selected countries:", paste(input$overview_countries %||% "None", collapse = ", ")),
                    paste("Selected provinces:", paste(input$overview_provinces %||% "None", collapse = ", ")),
                    paste("Selected districts:", paste(input$overview_districts %||% "None", collapse = ", ")),
                    paste("Performance filters:", paste(input$overview_perf_filter %||% "None", collapse = ", ")),
                    "",
                    "=== DATA SAMPLE ===",
                    paste("First 3 dates:", paste(head(overview_filtered()$round_start_date, 3), collapse = ", "))
    )
    
    if (nrow(overview_filtered()) > 0) {
      years_in_data <- unique(lubridate::year(overview_filtered()$round_start_date))
      debug_text <- c(debug_text,
                      paste("Years in filtered data:", paste(sort(years_in_data), collapse = ", ")))
    }
    
    paste(debug_text, collapse = "\n")
  })
  
  # ---- Run Button ----
  observeEvent(input$overview_analyze, {
    req(overview_filtered())
    
    cat("\n=== OVERVIEW ANALYSIS RUN ===\n")
    cat("Filtered rows:", nrow(overview_filtered()), "\n")
    
    if (input$overview_time_type == "range") {
      cat("Date range selected:", paste(input$overview_daterange, collapse = " to "), "\n")
    } else {
      cat("Years selected:", input$overview_years, "\n")
    }
    
    cat("Dates in data:", paste(range(overview_filtered()$round_start_date, na.rm = TRUE), collapse = " to "), "\n")
    cat("==============================\n")
    
    showNotification("📊 Updating LQAS Repository Overview...", type = "message")
  })
  
  # ---- Value Boxes ----
  output$overview_total_campaigns <- renderValueBox({
    req(overview_filtered())
    df <- overview_filtered()
    
    # 1. Keep only rows with all key fields present
    df_clean <- df %>%
      dplyr::filter(
        !is.na(country),
        !is.na(response),
        !is.na(vaccine.type),
        !is.na(roundNumber),
        !is.na(round_start_date)
      ) %>%
      # 2. Standardize text & round number
      dplyr::mutate(
        country      = trimws(toupper(country)),
        response     = trimws(toupper(response)),
        vaccine.type = trimws(toupper(vaccine.type)),
        roundNumber  = as.character(roundNumber)
      ) %>%
      # 3. Define ONE campaign per (country, response, vaccine, round)
      #    using the earliest start date (to avoid split by small date differences)
      dplyr::group_by(country, response, vaccine.type, roundNumber) %>%
      dplyr::summarise(
        round_start_date = min(round_start_date, na.rm = TRUE),
        .groups = "drop"
      )
    
    n_campaigns <- nrow(df_clean)
    
    # Optional: console debug when debug mode is ON
    if (isTRUE(input$overview_debug)) {
      cat("\n---- CAMPAIGN DEBUG ----\n")
      cat("Distinct campaigns:", n_campaigns, "\n")
      cat("Sample campaigns:\n")
      print(head(df_clean, 10))
      cat("------------------------\n")
    }
    
    valueBox(
      value    = n_campaigns,
      subtitle = "Campaigns",
      icon     = icon("calendar"),
      color    = "aqua"
    )
  })
  
  
  output$overview_total_countries <- renderValueBox({
    req(overview_filtered())
    n_countries <- dplyr::n_distinct(overview_filtered()$country, na.rm = TRUE)
    valueBox(n_countries, "Countries", icon = icon("globe-africa"), color = "green")
  })
  
  output$overview_total_provinces <- renderValueBox({
    req(overview_filtered())
    n_provinces <- dplyr::n_distinct(overview_filtered()$province, na.rm = TRUE)
    
    valueBox(
      n_provinces,
      "Provinces",
      icon  = icon("map"),
      color = "teal"
    )
  })
  
  output$overview_total_districts <- renderValueBox({
    req(overview_filtered())
    n_districts <- dplyr::n_distinct(overview_filtered()$district, na.rm = TRUE)
    valueBox(n_districts, "Districts", icon = icon("map-marker-alt"), color = "light-blue")
  })
  
  # ---- Data Period ----
  output$overview_data_period <- renderValueBox({
    req(overview_filtered())
    df <- overview_filtered()
    
    valid_dates <- df$round_start_date[!is.na(df$round_start_date)]
    
    if (length(valid_dates) > 0) {
      min_date <- min(valid_dates, na.rm = TRUE)
      max_date <- max(valid_dates, na.rm = TRUE)
      
      if (is.finite(min_date) && is.finite(max_date)) {
        period <- paste(format(min_date, "%b %Y"), "to", format(max_date, "%b %Y"))
        color <- "purple"
      } else {
        period <- "Invalid dates"
        color <- "red"
      }
    } else {
      period <- "No data available"
      color <- "red"
    }
    
    small_text <- HTML(sprintf("<span style='font-size:14px;'>%s</span>", period))
    
    valueBox(
      value    = small_text,
      subtitle = HTML("<span style='font-size:12px;'>SIA Date</span>"),
      icon     = icon("clock"),
      color    = color
    )
  })
  
  # ---- Data Explorer ----
  output$overview_explorer_table <- renderDT({
    req(overview_filtered())
    
    display_data <- overview_filtered() %>%
      arrange(desc(round_start_date))
    
    datatable(
      display_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy',  text = '📋 Copy'),
          list(extend = 'csv',   text = '📄 CSV'),
          list(extend = 'excel', text = '📊 Excel'),
          list(extend = 'colvis', text = '👁️ Show/Hide Columns')
        ),
        scrollX     = TRUE,
        scrollY     = 400,
        scroller    = TRUE,
        deferRender = TRUE,
        pageLength  = 10,
        autoWidth   = TRUE
      ),
      filter   = "top",
      rownames = FALSE
    )
  })
  
  # ---- Download Filtered Data ----
  output$download_overview_data <- downloadHandler(
    filename = function() paste0("Dashboard_Overview_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(overview_filtered(), file, row.names = FALSE)
    }
  )
  
  
  
  # ============================================================
  # 🔬 SCOPE ANALYSIS (UPDATED WITH DYNAMIC BLOCK SWITCHING)
  # ============================================================
  scope_analysis <- reactiveVal(NULL)
  scope_analysis_running <- reactiveVal(FALSE)
  scope_summary_analysis <- reactiveVal(NULL)
  scope_summary_params <- reactiveVal(NULL)

  # ADDED: Reactive block choices for scope analysis
  scope_block_choices <- reactive({
    if (input$scope_block_type == "afro") {
      return(afro_blocks)
    } else {
      return(ist_blocks)
    }
  })
  
  # ADDED: Reactive block names for scope analysis
  scope_block_names <- reactive({
    if (input$scope_block_type == "afro") {
      return(names(afro_blocks))
    } else {
      return(names(ist_blocks))
    }
  })
  
  # ADDED: Update block selection when block type changes
  observe({
    updateSelectInput(session, "scope_block", 
                      choices = c("All", scope_block_names()))
  })
  
  # Track last values to prevent infinite loops
  last_scope_block <- reactiveVal("All")
  last_scope_countries <- reactiveVal(character(0))
  last_scope_provinces <- reactiveVal(character(0))
  
  # Reactive for scope data with dynamic AFRO_block creation
  scope_data_reactive <- reactive({
    req(scope)
    
    # MODIFIED: Use reactive block choices for dynamic block assignment
    current_blocks <- scope_block_choices()
    
    # Add dynamic block column based on current block definition
    scope_with_blocks <- scope %>%
      mutate(
        dynamic_block = case_when(
          toupper(country) %in% current_blocks$LCB ~ "LCB",
          toupper(country) %in% current_blocks$WA ~ "WA", 
          toupper(country) %in% current_blocks$ESA ~ "ESA",
          toupper(country) %in% current_blocks$DRC ~ "DRC",
          toupper(country) %in% current_blocks$ECA ~ "ECA",
          TRUE ~ "Other"
        )
      )
    
    return(scope_with_blocks)
  })
  
  # Initialize country choices when data loads (RUNS ONLY ONCE)
  observeEvent(scope_data_reactive(), {
    req(scope_data_reactive())
    
    countries <- scope_data_reactive() %>%
      distinct(country) %>%
      pull(country) %>%
      sort() %>%
      na.omit()
    
    updateSelectizeInput(
      session, 
      "scope_countries",
      choices = c("All" = "", countries),
      selected = character(0)
    )
  }, once = TRUE)
  
  # Update Country choices based on selected block (FIXED - No infinite loop)
  observe({
    req(scope_data_reactive(), input$scope_block)
    
    current_block <- input$scope_block
    previous_block <- last_scope_block()
    
    # Only update if block actually changed
    if (current_block != previous_block) {
      last_scope_block(current_block)
      
      countries_data <- scope_data_reactive()
      
      # Filter by dynamic_block if selected
      if (current_block != "All") {
        # MODIFIED: Use reactive block choices to get countries for selected block
        block_countries <- scope_block_choices()[[current_block]]
        countries_data <- countries_data %>%
          filter(country %in% block_countries)
      }
      
      countries <- countries_data %>%
        distinct(country) %>%
        pull(country) %>%
        sort() %>%
        na.omit()
      
      # Update choices but preserve current selection if possible
      current_selection <- input$scope_countries
      valid_selection <- intersect(current_selection, countries)
      
      updateSelectizeInput(
        session, 
        "scope_countries",
        choices = c("All" = "", countries),
        selected = if (length(valid_selection) > 0) valid_selection else character(0)
      )
      
      # Reset provinces and districts when block changes
      updateSelectizeInput(session, "scope_provinces", selected = character(0))
      updateSelectizeInput(session, "scope_districts", selected = character(0))
    }
  })
  
  # Update Province choices based on selected countries - DEBOUNCED VERSION
  observe({
    req(scope_data_reactive(), scope_countries_debounced())
    
    current_countries <- scope_countries_debounced()
    previous_countries <- last_scope_countries()
    
    # Only update if countries actually changed
    if (!identical(sort(current_countries), sort(previous_countries))) {
      last_scope_countries(current_countries)
      
      if (length(current_countries) > 0 && !all(current_countries == "")) {
        provinces <- scope_data_reactive() %>%
          filter(country %in% current_countries) %>%
          distinct(province) %>%
          pull(province) %>%
          sort() %>%
          na.omit()
        
        # Update choices but preserve current selection if possible
        current_selection <- input$scope_provinces
        valid_selection <- intersect(current_selection, provinces)
        
        updateSelectizeInput(
          session, 
          "scope_provinces",
          choices = c("All" = "", provinces),
          selected = if (length(valid_selection) > 0) valid_selection else character(0)
        )
      } else {
        updateSelectizeInput(
          session, 
          "scope_provinces",
          choices = c("All" = ""),
          selected = character(0)
        )
      }
      
      # Reset districts when countries change
      updateSelectizeInput(session, "scope_districts", selected = character(0))
    }
  })
  
  # Update District choices based on selected countries and provinces (FIXED)
  observe({
    req(scope_data_reactive(), input$scope_countries)
    
    # Only run when countries or provinces change
    current_provinces <- input$scope_provinces
    previous_provinces <- last_scope_provinces()
    
    if (!identical(sort(current_provinces), sort(previous_provinces))) {
      last_scope_provinces(current_provinces)
      
      if (length(input$scope_countries) > 0 && !all(input$scope_countries == "")) {
        districts_data <- scope_data_reactive() %>%
          filter(country %in% input$scope_countries)
        
        # Apply province filtering if any provinces are selected
        provinces_selected <- !is.null(current_provinces) && 
          length(current_provinces) > 0 && 
          !all(current_provinces == "")
        
        if (provinces_selected) {
          districts_data <- districts_data %>%
            filter(province %in% current_provinces)
        }
        
        districts <- districts_data %>%
          distinct(district) %>%
          pull(district) %>%
          sort() %>%
          na.omit()
        
        # Update choices but preserve current selection if possible
        current_selection <- input$scope_districts
        valid_selection <- intersect(current_selection, districts)
        
        updateSelectizeInput(
          session, 
          "scope_districts",
          choices = c("All" = "", districts),
          selected = if (length(valid_selection) > 0) valid_selection else character(0)
        )
      } else {
        updateSelectizeInput(
          session, 
          "scope_districts",
          choices = c("All" = ""),
          selected = character(0)
        )
      }
    }
  })
  
  # Builds the aggregated Scope Summary Map from a snapshotted parameter set
  # (captured at Analyze-time so it stays consistent with the per-round map
  # even if filter inputs change before this actually runs -- used both for
  # an immediate build below and for the lazy tab-switch build further down).
  run_scope_summary_analysis <- function(params) {
    tryCatch(
      generate_scope_summary_map(
        scope_data = scope,
        x_months = params$x_months,
        year_selection = params$year_selection,
        block_selection = params$block_selection,
        country_selection = params$country_selection,
        province_selection = params$province_selection,
        district_selection = params$district_selection,
        vaccine_selection = params$vaccine_selection,
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,
        all_countries = all_countries,
        all_provinces = all_provinces,
        all_districts = all_districts,
        block_type = params$block_type,
        ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
      ),
      error = function(e) {
        showNotification(paste("Scope Summary Map failed:", e$message), type = "warning")
        NULL
      }
    )
  }

  # FIXED: Analysis with proper multi-select handling and dynamic block switching
  observeEvent(input$scope_analyze, {
    if (input$tabs != "scope") return()
    if (scope_analysis_running()) return()
    req(scope)
    
    # Set running flag
    scope_analysis_running(TRUE)
    
    # Debug: Check if function exists
    if (!exists("generate_scope_plots_app")) {
      showNotification("❌ ERROR: Scope analysis function not found!", type = "error", duration = 10)
      scope_analysis_running(FALSE)
      return()
    }
    
    safe_analysis({
      # ---- Setup parameters with multi-select support ----
      x_months <- if (input$scope_time_type == "months") input$scope_months else NULL
      year_selection <- if (input$scope_time_type == "years" && nzchar(input$scope_years)) {
        yrs <- strsplit(input$scope_years, ",")[[1]]
        as.numeric(trimws(yrs))
      } else NULL
      block_selection   <- if (input$scope_block != "All") input$scope_block else NULL
      
      # Multi-select country handling
      country_selection <- if (!is.null(input$scope_countries) && 
                               length(input$scope_countries) > 0 && 
                               !all(input$scope_countries == "")) {
        input$scope_countries
      } else NULL
      
      # Multi-select province handling
      province_selection <- if (!is.null(input$scope_provinces) && 
                                length(input$scope_provinces) > 0 && 
                                !all(input$scope_provinces == "")) {
        input$scope_provinces
      } else NULL
      
      # Multi-select district handling
      district_selection <- if (!is.null(input$scope_districts) && 
                                length(input$scope_districts) > 0 && 
                                !all(input$scope_districts == "")) {
        input$scope_districts
      } else NULL
      
      vaccine_selection <- input$scope_vaccine %||% "All"
      
      # Clear previous results to free memory
      scope_analysis(NULL)
      force_gc()
      
      # ---- Run SEQUENTIAL analysis ----
      result <- generate_scope_plots_app(
        scope_data = scope,
        x_months = x_months,
        year_selection = if (length(year_selection) > 0) year_selection else NULL,
        block_selection = block_selection,
        country_selection = country_selection,
        province_selection = province_selection,
        district_selection = district_selection,
        vaccine_selection = vaccine_selection,
        # Pass required objects explicitly
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,  # ADDED: Pass IST blocks
        all_countries = all_countries,
        all_provinces = all_provinces,
        all_districts = all_districts,
        block_type = input$scope_block_type,  # ADDED: Pass block type
        ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
      )

      scope_analysis(result)
      force_gc()

      # ---- Companion aggregated summary map (same filters/period) ----
      # Just snapshot the params here -- the level-triggered observer below
      # (not an edge-triggered observeEvent) picks this up and builds the
      # summary map only if/when that sub-tab is actually the one on screen,
      # whether that's true right now or becomes true later by switching
      # tabs. Keeps "Analyze" itself fast (mirrors the Unresolved Cases
      # lazy-reason build).
      scope_summary_analysis(NULL)
      summary_params <- list(
        x_months = x_months,
        year_selection = if (length(year_selection) > 0) year_selection else NULL,
        block_selection = block_selection,
        country_selection = country_selection,
        province_selection = province_selection,
        district_selection = district_selection,
        vaccine_selection = vaccine_selection,
        block_type = input$scope_block_type
      )
      scope_summary_params(summary_params)

    }, "SIA Scope Analysis")
    
    # Reset running flag when done
    scope_analysis_running(FALSE)
  })

  # Builds the Scope Summary Map whenever its sub-tab is the one currently
  # on screen and a build is actually pending -- level-triggered (re-checks
  # the current state on every relevant change) rather than edge-triggered,
  # so it also covers "already on that tab when Analyze was clicked" and
  # "tab was already selected before this session's Analyze ever ran",
  # not just the moment of switching into it.
  observe({
    if (is.null(input$tabs) || input$tabs != "scope") return()
    if (!identical(input$scope_tabs, "scope_summary_map")) return()
    params <- scope_summary_params()
    req(params)
    if (!is.null(scope_summary_analysis())) return()
    isolate({
      safe_analysis({
        scope_summary_analysis(run_scope_summary_analysis(params))
        force_gc()
      }, "SIA Scope Analysis (lazy summary map)")
    })
  })
  
  # Reset running flag if analysis fails
  observe({
    if (!is.null(scope_analysis())) {
      scope_analysis_running(FALSE)
    }
  })
  
  # ---- Value Boxes ----
  output$scope_campaigns <- renderValueBox({
    req(scope_analysis())
    result <- scope_analysis()
    
    df <- result$filtered_scope
    n_campaigns <- 0
    
    if (!is.null(df) && nrow(df) > 0) {
      df_clean <- df %>%
        dplyr::filter(
          !is.na(country),
          !is.na(response),
          !is.na(vaccine.type),
          !is.na(roundNumber),
          !is.na(round_start_date)
        ) %>%
        dplyr::mutate(
          country      = trimws(toupper(country)),
          response     = trimws(toupper(response)),
          vaccine.type = trimws(toupper(vaccine.type)),
          roundNumber  = as.character(roundNumber)
        ) %>%
        dplyr::group_by(country, response, vaccine.type, roundNumber) %>%
        dplyr::summarise(
          round_start_date = min(round_start_date, na.rm = TRUE),
          .groups = "drop"
        )
      
      n_campaigns <- nrow(df_clean)
      
      # Optional: console debug for scope campaigns
      # cat("\n---- SCOPE CAMPAIGN DEBUG ----\n")
      # cat("Distinct campaigns:", n_campaigns, "\n")
      # print(head(df_clean, 10))
      # cat("------------------------------\n")
    }
    
    valueBox(
      value    = n_campaigns,
      subtitle = "Campaigns",
      icon     = icon("calendar"),
      color    = "aqua"
    )
  })
  
  
  output$scope_countries_box <- renderValueBox({
    req(scope_analysis())
    result <- scope_analysis()
    n <- if (!is.null(result$data_summary$overall_summary) && nrow(result$data_summary$overall_summary) > 0) {
      result$data_summary$overall_summary$total_countries
    } else {
      if (nrow(result$filtered_scope) > 0) {
        dplyr::n_distinct(result$filtered_scope$country)
      } else {
        0
      }
    }
    valueBox(n, "Countries", icon = icon("flag"), color = "green")
  })
  
  output$scope_districts_box <- renderValueBox({
    req(scope_analysis())
    result <- scope_analysis()
    n <- if (!is.null(result$data_summary$overall_summary) && nrow(result$data_summary$overall_summary) > 0) {
      result$data_summary$overall_summary$total_districts
    } else {
      if (nrow(result$filtered_scope) > 0) {
        dplyr::n_distinct(result$filtered_scope$district)
      } else {
        0
      }
    }
    valueBox(n, "Districts", icon = icon("map-marker-alt"), color = "light-blue")
  })
  
  output$scope_vaccines <- renderValueBox({
    req(scope_analysis())
    result <- scope_analysis()
    n <- if (!is.null(result$data_summary$overall_summary) && nrow(result$data_summary$overall_summary) > 0) {
      length(strsplit(result$data_summary$overall_summary$vaccines_used, ", ")[[1]])
    } else if ("vaccine.type" %in% names(result$filtered_scope) && nrow(result$filtered_scope) > 0) {
      dplyr::n_distinct(result$filtered_scope$vaccine.type)
    } else {
      0
    }
    valueBox(n, "Vaccine Types", icon = icon("syringe"), color = "purple")
  })
  
  # ---- Map ----
  output$scope_map_plot <- renderPlot({
    req(scope_analysis())
    result <- scope_analysis()
    if (is.null(result$plot)) {
      ggplot() + theme_void() +
        labs(title = "No map available for the selected filters") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, colour = "gray40"))
    } else {
      result$plot
    }
  }, height = function() {
    max_h <- 800; min_h <- 400
    calc_h <- session$clientData$output_scope_map_plot_width * 0.7
    pmin(pmax(calc_h, min_h), max_h)
  }, width = "auto")
  
  # ---- Downloads ----
  output$download_scope_map <- downloadHandler(
    filename = function() {
      paste0("sia-scope-map-", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating map download...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          req(scope_analysis())
          incProgress(0.6, detail = "Saving image...")
          ggsave(file, plot = scope_analysis()$plot,
                 width = 14, height = 9, dpi = 300, limitsize = FALSE)
          incProgress(1, detail = "Map saved!")
        }
      )
    }
  )

  # ---- Summary Map (aggregated across the whole selected period) ----
  output$scope_summary_map_plot <- renderPlot({
    req(scope_summary_analysis())
    result <- scope_summary_analysis()
    if (is.null(result$map)) {
      ggplot() + theme_void() +
        labs(title = "No summary map available for the selected filters") +
        theme(plot.title = element_text(hjust = 0.5, size = 14, colour = "gray40"))
    } else {
      result$map
    }
  }, height = function() {
    max_h <- 800; min_h <- 400
    calc_h <- session$clientData$output_scope_summary_map_plot_width * 0.7
    pmin(pmax(calc_h, min_h), max_h)
  }, width = "auto")

  output$scope_summary_footnote <- renderUI({
    req(scope_summary_analysis())
    footnote <- scope_summary_analysis()$footnote_text
    if (is.null(footnote) || !nzchar(footnote)) return(NULL)

    HTML(paste0(
      "<div style='font-family: Arial, sans-serif; font-size:12px; margin-top:12px; padding:10px 12px; background:#fffbeb; border-left:4px solid #d97706; border-radius:6px; color:#374151;'>",
      gsub("\n", "<br>",
           gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", footnote)),
      "</div>"
    ))
  })

  output$download_scope_summary_map <- downloadHandler(
    filename = function() {
      paste0("sia-scope-summary-map-", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating summary map download...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          req(scope_summary_analysis())
          req(scope_summary_analysis()$map)
          incProgress(0.6, detail = "Saving image...")
          export_plot <- scope_summary_analysis()$map_with_footnote %||% scope_summary_analysis()$map
          ggsave(file, plot = export_plot,
                 width = 12, height = 9, dpi = 300, limitsize = FALSE)
          incProgress(1, detail = "Map saved!")
        }
      )
    }
  )

  output$download_scope_summary_map_pdf <- downloadHandler(
    filename = function() {
      paste0("sia-scope-summary-map-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      withProgress(
        message = 'Generating summary map PDF...',
        detail = 'Rendering PDF...',
        value = 0.3,
        {
          req(scope_summary_analysis())
          req(scope_summary_analysis()$map)
          incProgress(0.6, detail = "Saving PDF...")
          ggsave(file, plot = scope_summary_analysis()$map,
                 device = "pdf", width = 12, height = 9, dpi = 300,
                 useDingbats = FALSE, limitsize = FALSE)
          incProgress(1, detail = "PDF saved!")
        }
      )
    }
  )


  # ============================================================
  # 🧮 ADMIN DATA OVERVIEW (Optimized with Dynamic Block Switching)
  # ============================================================
  
  # ADDED: Reactive block choices for admin overview
  admin_block_choices <- reactive({
    if (input$admin_block_type == "afro") {
      return(afro_blocks)
    } else {
      return(ist_blocks)
    }
  })
  
  # ADDED: Reactive block names for admin overview
  admin_block_names <- reactive({
    if (input$admin_block_type == "afro") {
      return(names(afro_blocks))
    } else {
      return(names(ist_blocks))
    }
  })
  
  # ADDED: Update block selection when block type changes
  observe({
    updateSelectInput(session, "admin_block", 
                      choices = c("All", admin_block_names()))
  })
  
  # MODIFIED: Update country selection to use reactive block choices
  output$admin_countries_ui <- renderUI({
    req(admin_data)
    
    available_countries <- if (!is.null(input$admin_block) && input$admin_block != "All") {
      # MODIFIED: Use reactive block choices
      admin_block_choices()[[input$admin_block]]
    } else {
      sort(unique(admin_data$Country))
    }
    
    selectizeInput(
      "admin_countries", "Countries (Optional):",
      choices = c("All" = "", available_countries),
      multiple = TRUE,
      options = list(
        placeholder = 'Select one or more countries',
        maxItems = 15,
        plugins = list('remove_button'),
        dropdownParent = 'body'
      )
    )
  })
  
  # MODIFIED: Analysis with dynamic block switching
  observeEvent(input$admin_analyze, {
    if (input$tabs != "admin_overview") return()  # Only run if admin tab is active
    req(admin_data)
    
    safe_analysis({
      # ---- Filters ----
      x_months <- if (input$admin_time_type == "months") input$admin_months else NULL
      year_selection <- if (input$admin_time_type == "years" && input$admin_years != "") {
        as.numeric(strsplit(input$admin_years, ",")[[1]])
      } else NULL
      block_selection <- if (input$admin_block != "All") input$admin_block else NULL
      country_selection <- if (!is.null(input$admin_countries) && length(input$admin_countries) > 0)
        input$admin_countries else NULL
      
      # ---- Run coverage and vaccinated summaries ----
      coverage_res <- admin_coverage_summary(
        admin_data = admin_data,
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,  # ADDED: Pass IST blocks
        block_type = input$admin_block_type,  # ADDED: Pass block type
        block_selection = block_selection,
        country_selection = country_selection,
        x_months = x_months,
        year_selection = year_selection
      )
      
      vacc_res <- admin_vaccinated_summary(
        admin_data = admin_data,
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,  # ADDED: Pass IST blocks
        block_type = input$admin_block_type,  # ADDED: Pass block type
        block_selection = block_selection,
        country_selection = country_selection,
        x_months = x_months,
        year_selection = year_selection
      )
      
      # ---- Compute Key Metrics ----
      avg_cov <- ifelse(is.na(coverage_res$metrics$avg_coverage), 0, coverage_res$metrics$avg_coverage)
      pct_ge95 <- ifelse(is.na(coverage_res$metrics$pct_ge95), 0, coverage_res$metrics$pct_ge95)
      total_vacc <- ifelse(is.na(vacc_res$metrics$total_dose_administrated), 0, vacc_res$metrics$total_dose_administrated)
      n_countries <- ifelse(is.na(coverage_res$metrics$n_countries), 0, coverage_res$metrics$n_countries)
      
      # ---- Build color-coded summary ----
      cov_icon <- dplyr::case_when(
        avg_cov >= 90 ~ "🟩 **High Performance**",
        avg_cov >= 80 ~ "🟨 **Moderate**",
        TRUE ~ "🟥 **Low**"
      )
      
      ge95_icon <- dplyr::case_when(
        pct_ge95 >= 70 ~ "🟩",
        pct_ge95 >= 40 ~ "🟨",
        TRUE ~ "🟥"
      )
      
      # ADDED: Block type indicator in summary
      block_type_text <- ifelse(input$admin_block_type == "afro", "AFRO Blocks", "IST Blocks")
      
      # ---- Text summary ----
      insights <- paste0(
        "📊 <strong>", block_type_text, " Analysis</strong><br>",
        cov_icon, ": ", round(avg_cov, 1), "% average coverage across ", n_countries, " countries.  <br>",
        ge95_icon, " ", pct_ge95, "% of districts reached ≥95% coverage.  <br>",
        "👶 **", total_vacc, " million of doses administrated** in the selected period."
      )
      
      # ---- Store results ----
      admin_overview(list(
        coverage = coverage_res,
        vaccinated = vacc_res,
        summary_text = insights
      ))
      
    }, "Admin Overview")
  })
  
  # ---- Value Boxes ----
  output$admin_total_vaccinated <- renderValueBox({
    req(admin_overview())
    val <- admin_overview()$vaccinated$metrics$total_dose_administrated
    val <- ifelse(is.na(val), 0, val)
    valueBox(
      paste0(val, " M"), 
      "Doses Administrated",
      icon = icon("syringe"), 
      color = "green"
    )
  })
  
  output$admin_children_vaccinated <- renderValueBox({
    req(admin_overview())
    val <- admin_overview()$vaccinated$metrics$total_children_vaccinated
    val <- ifelse(is.na(val), 0, val)
    valueBox(
      paste0(val, " M"), 
      "Children Vaccinated",
      icon = icon("child"), 
      color = "light-blue"
    )
  })
  
  output$admin_countries_reporting <- renderValueBox({
    req(admin_overview())
    val <- admin_overview()$vaccinated$metrics$n_countries
    val <- ifelse(is.na(val), 0, val)
    valueBox(val, "Countries Reporting", icon = icon("flag"), color = "aqua")
  })
  
  output$admin_avg_coverage <- renderValueBox({
    req(admin_overview())
    val <- admin_overview()$coverage$metrics$pct_ge95
    val <- ifelse(is.na(val), 0, val)
    valueBox(
      paste0(val, "%"), "Districts ≥95% Coverage",
      icon = icon("shield-alt"),
      color = ifelse(val >= 80, "green", ifelse(val >= 50, "yellow", "red"))
    )
  })
  
  # ---- Summary Text ----
  output$admin_summary_text <- renderUI({
    req(admin_overview())
    HTML(paste0(
      "<div style='font-size:1.1em; line-height:1.5; text-align:left;'>",
      admin_overview()$summary_text,
      "</div>"
    ))
  })
  
  # ---- Coverage Table ----
  output$admin_coverage_table_ui <- renderUI({
    req(admin_overview())
    ft <- admin_overview()$coverage$flextable
    if (!is.null(ft)) {
      div(class = "flextable-output", htmltools_value(ft))
    } else {
      tags$div(class = "alert alert-warning", "No coverage data available.")
    }
  })
  
  # ---- Vaccinated Table ----
  output$admin_vaccinated_table_ui <- renderUI({
    req(admin_overview())
    ft <- admin_overview()$vaccinated$flextable
    if (!is.null(ft)) {
      div(class = "flextable-output", htmltools_value(ft))
    } else {
      tags$div(class = "alert alert-warning", "No vaccination data available.")
    }
  })
  
  # ---- Downloads ----
  output$download_admin_coverage <- downloadHandler(
    filename = function() {
      paste0("Admin_Coverage_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating coverage table...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          req(admin_overview())
          incProgress(0.6, detail = "Saving image...")
          flextable::save_as_image(admin_overview()$coverage$flextable, path = file)
          incProgress(1, detail = "Download ready!")
        }
      )
    }
  )
  
  output$download_admin_vaccinated <- downloadHandler(
    filename = function() {
      paste0("Admin_Vaccinated_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating vaccinated table...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          req(admin_overview())
          incProgress(0.6, detail = "Saving image...")
          flextable::save_as_image(admin_overview()$vaccinated$flextable, path = file)
          incProgress(1, detail = "Download ready!")
        }
      )
    }
  )
  
  
  # ============================================================
  # 👶 MISSED CHILDREN (Updated with Dynamic Block Switching)
  # ✅ FIXES INCLUDED:
  #   1) Robust block switching (AFRO/IST) + dynamic block list
  #   2) Safe country selection (treat "All" = "" as NULL)
  #   3) Protect flextable HTML rendering (gsub/htmlize crashes) via tryCatch
  #   4) All outputs kept alive for conditionalPanel (outputOptions)
  #   5) Download handlers (table PNG + raw XLSX + filtered workbook XLSX)
  # ============================================================
  
  # Reactive to store missed children analysis results
  missed_analysis <- reactiveVal(NULL)
  
  # Reactive to check if missed analysis results exist (for conditional panel)
  output$missed_analysis_exists <- reactive({
    !is.null(missed_analysis())
  })
  outputOptions(output, "missed_analysis_exists", suspendWhenHidden = FALSE)
  
  # ---------------------------
  # Dynamic block switching
  # ---------------------------
  missed_block_choices <- reactive({
    if (identical(input$missed_block_type, "afro")) afro_blocks else ist_blocks
  })
  
  missed_block_names <- reactive({
    if (identical(input$missed_block_type, "afro")) names(afro_blocks) else names(ist_blocks)
  })
  
  observe({
    updateSelectInput(
      session, "missed_block",
      choices = c("All", missed_block_names()),
      selected = "All"
    )
  })
  
  # ---------------------------
  # Countries selector UI
  # ---------------------------
  output$missed_countries_ui <- renderUI({
    req(dat)
    
    available_countries <- if (!is.null(input$missed_block) && nzchar(input$missed_block) && input$missed_block != "All") {
      missed_block_choices()[[input$missed_block]]
    } else {
      sort(unique(dat$country))
    }
    
    selectizeInput(
      "missed_countries", "Countries (Optional):",
      choices = c("All" = "", available_countries),
      multiple = TRUE,
      options = list(
        placeholder = "Select one or more countries",
        maxItems = 15,
        plugins = list("remove_button"),
        dropdownParent = "body"
      )
    )
  })
  
  # ---------------------------
  # Run analysis
  # ---------------------------
  observeEvent(input$missed_analyze, {
    if (!is.null(input$tabs) && input$tabs != "missed_children") return()
    
    showNotification("🔍 Analyzing missed children data...", type = "message", duration = 3)
    
    safe_analysis({
      
      # ---- validate inputs ----
      if (identical(input$missed_time_type, "x_months")) {
        if (is.null(input$missed_months) || input$missed_months < 1) {
          stop("Please enter a valid number of months (1-24)")
        }
      } else if (identical(input$missed_time_type, "quarter")) {
        if (is.null(input$missed_year) || is.null(input$missed_quarter)) {
          stop("Please select both year and quarter")
        }
      } else if (identical(input$missed_time_type, "last_quarters")) {
        if (is.null(input$missed_quarters)) {
          stop("Please select number of quarters")
        }
      } else {
        stop("Unknown time period type.")
      }
      
      # ---- build params ----
      params <- list(data = dat)
      
      if (identical(input$missed_time_type, "x_months")) {
        params$x_months <- as.numeric(input$missed_months)
      } else if (identical(input$missed_time_type, "quarter")) {
        params$year <- as.numeric(input$missed_year)
        params$Q    <- as.numeric(input$missed_quarter)
      } else if (identical(input$missed_time_type, "last_quarters")) {
        params$last_n_quarters <- as.numeric(input$missed_quarters)
      }
      
      params$block_selection <- if (!is.null(input$missed_block) && input$missed_block != "All") input$missed_block else NULL
      
      # IMPORTANT: treat "All" (= "") as NULL and drop blanks
      params$country_selection <- if (!is.null(input$missed_countries) &&
                                      length(input$missed_countries) > 0 &&
                                      any(nzchar(input$missed_countries))) {
        input$missed_countries[nzchar(input$missed_countries)]
      } else {
        NULL
      }
      
      params$block_type  <- input$missed_block_type
      params$afro_blocks <- afro_blocks
      params$ist_blocks  <- ist_blocks
      
      # ---- run ----
      result <- do.call(missed_children_disaggregated, params)
      missed_analysis(result)
      
      showNotification("✅ Analysis completed successfully!", type = "message", duration = 3)
      
    }, "Missed Children Analysis")
  })
  
  # ---------------------------
  # Value boxes
  # ---------------------------
  output$missed_overall_rate <- renderValueBox({
    req(missed_analysis())
    
    tryCatch({
      overall <- missed_analysis()$raw_data %>%
        dplyr::summarise(
          total_sampled    = sum(male_sampled + female_sampled, na.rm = TRUE),
          total_vaccinated = sum(male_vaccinated + female_vaccinated, na.rm = TRUE),
          missed_rate      = ifelse(total_sampled > 0, round((1 - total_vaccinated / total_sampled) * 100, 1), 0)
        )
      
      valueBox(
        paste0(overall$missed_rate, "%"),
        "Overall Missed Rate",
        icon  = icon("child"),
        color = ifelse(overall$missed_rate > 2, "red", "green")
      )
    }, error = function(e) {
      valueBox("N/A", "Overall Missed Rate", icon = icon("child"), color = "yellow")
    })
  })
  
  output$missed_boys_rate <- renderValueBox({
    req(missed_analysis())
    
    tryCatch({
      boys <- missed_analysis()$raw_data %>%
        dplyr::summarise(
          total_sampled    = sum(male_sampled, na.rm = TRUE),
          total_vaccinated = sum(male_vaccinated, na.rm = TRUE),
          missed_rate      = ifelse(total_sampled > 0, round((1 - total_vaccinated / total_sampled) * 100, 1), 0)
        )
      
      valueBox(
        paste0(boys$missed_rate, "%"),
        "Boys Missed Rate",
        icon  = icon("male"),
        color = ifelse(boys$missed_rate > 2, "red", "green")
      )
    }, error = function(e) {
      valueBox("N/A", "Boys Missed Rate", icon = icon("male"), color = "yellow")
    })
  })
  
  output$missed_girls_rate <- renderValueBox({
    req(missed_analysis())
    
    tryCatch({
      girls <- missed_analysis()$raw_data %>%
        dplyr::summarise(
          total_sampled    = sum(female_sampled, na.rm = TRUE),
          total_vaccinated = sum(female_vaccinated, na.rm = TRUE),
          missed_rate      = ifelse(total_sampled > 0, round((1 - total_vaccinated / total_sampled) * 100, 1), 0)
        )
      
      valueBox(
        paste0(girls$missed_rate, "%"),
        "Girls Missed Rate",
        icon  = icon("female"),
        color = ifelse(girls$missed_rate > 2, "red", "green")
      )
    }, error = function(e) {
      valueBox("N/A", "Girls Missed Rate", icon = icon("female"), color = "yellow")
    })
  })
  
  # ---------------------------
  # Flextable UI rendering (protected)
  # ---------------------------
  output$missed_flextable_ui <- renderUI({
    req(missed_analysis())
    
    if (!is.null(missed_analysis()$flextable) &&
        !is.null(missed_analysis()$summary_data) &&
        nrow(missed_analysis()$summary_data) > 0) {
      
      tryCatch({
        ft_html <- flextable::htmltools_value(missed_analysis()$flextable)
        
        div(
          class = "flextable-output",
          style = "overflow-x: auto; margin: 10px 0;",
          ft_html
        )
      }, error = function(e) {
        tags$div(
          class = "alert alert-danger",
          tags$h4("Flextable rendering failed"),
          tags$p("Usually caused by non-UTF8 / invalid characters in data."),
          tags$pre(style = "white-space: pre-wrap;", conditionMessage(e))
        )
      })
      
    } else {
      tags$div(
        class = "alert alert-warning",
        tags$h4("No data available"),
        tags$p("No data found for the selected filters. Try:"),
        tags$ul(
          tags$li("Broadening the time period"),
          tags$li("Selecting different countries"),
          tags$li("Choosing a different block type or selection")
        )
      )
    }
  })
  
  # ---------------------------
  # Downloads
  # ---------------------------
  output$download_missed_table <- downloadHandler(
    filename = function() paste0("Missed_Children_Table_", Sys.Date(), ".png"),
    content = function(file) {
      withProgress(
        message = "Generating missed children table...",
        detail  = "Rendering high-quality image...",
        value   = 0.3,
        {
          req(missed_analysis())
          incProgress(0.6, detail = "Saving image...")
          flextable::save_as_image(missed_analysis()$flextable, path = file)
          incProgress(1, detail = "Download ready!")
        }
      )
    }
  )
  
  output$download_missed_data <- downloadHandler(
    filename = function() paste0("missed_children_raw_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(missed_analysis())
      writexl::write_xlsx(missed_analysis()$raw_data, file)
    }
  )
  
  output$download_missed_filtered_data <- downloadHandler(
    filename = function() paste0("filtered_missed_children_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(missed_analysis())
      
      summary_data <- missed_analysis()$summary_data
      raw_data     <- missed_analysis()$raw_data
      
      filters_applied <- data.frame(
        Parameter = c(
          "Date Generated",
          "Block Type",
          "Selected Block",
          "Countries",
          "Time Period Type",
          "Analysis Period",
          "Total Records (Raw)",
          "Total Records (Summary)"
        ),
        Value = c(
          as.character(Sys.Date()),
          if (!is.null(input$missed_block_type)) input$missed_block_type else "afro",
          if (!is.null(input$missed_block)) input$missed_block else "All",
          if (!is.null(input$missed_countries) && length(input$missed_countries) > 0 && any(nzchar(input$missed_countries)))
            paste(input$missed_countries[nzchar(input$missed_countries)], collapse = ", ") else "All",
          if (!is.null(input$missed_time_type)) input$missed_time_type else "x_months",
          if (!is.null(missed_analysis()$period_text)) missed_analysis()$period_text else "Not specified",
          format(nrow(raw_data), big.mark = ","),
          format(nrow(summary_data), big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
      
      time_params <- data.frame(Parameter = character(), Value = character(), stringsAsFactors = FALSE)
      
      if (identical(input$missed_time_type, "x_months")) {
        time_params <- rbind(time_params, data.frame(Parameter = "Number of Months", Value = as.character(input$missed_months)))
      } else if (identical(input$missed_time_type, "quarter")) {
        time_params <- rbind(
          time_params,
          data.frame(Parameter = "Year", Value = as.character(input$missed_year)),
          data.frame(Parameter = "Quarter", Value = as.character(input$missed_quarter))
        )
      } else if (identical(input$missed_time_type, "last_quarters")) {
        time_params <- rbind(time_params, data.frame(Parameter = "Last N Quarters", Value = as.character(input$missed_quarters)))
      }
      
      all_filters <- rbind(filters_applied, time_params)
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Filter Information")
      openxlsx::writeData(wb, "Filter Information", "Missed Children Analysis - Filter Settings", startRow = 1)
      openxlsx::writeData(wb, "Filter Information", all_filters, startRow = 3)
      openxlsx::setColWidths(wb, "Filter Information", cols = 1:2, widths = c(25, 50))
      
      openxlsx::addWorksheet(wb, "Summary Data")
      openxlsx::writeData(wb, "Summary Data", summary_data, startRow = 1)
      
      if (nrow(summary_data) > 0) {
        chronological_data <- summary_data
        tcols <- names(chronological_data)[stringr::str_detect(names(chronological_data), "^\\d{4}_.+_")]
        
        if (length(tcols) > 0) {
          male_cols   <- tcols[stringr::str_detect(tcols, "_M$")]
          female_cols <- tcols[stringr::str_detect(tcols, "_F$")]
          
          chronological_data <- chronological_data %>%
            dplyr::mutate(
              Avg_Missed_Male   = if (length(male_cols) > 0) rowMeans(dplyr::select(., dplyr::all_of(male_cols)), na.rm = TRUE) else NA_real_,
              Avg_Missed_Female = if (length(female_cols) > 0) rowMeans(dplyr::select(., dplyr::all_of(female_cols)), na.rm = TRUE) else NA_real_,
              Max_Missed_Male   = if (length(male_cols) > 0) apply(dplyr::select(., dplyr::all_of(male_cols)), 1, max, na.rm = TRUE) else NA_real_,
              Max_Missed_Female = if (length(female_cols) > 0) apply(dplyr::select(., dplyr::all_of(female_cols)), 1, max, na.rm = TRUE) else NA_real_,
              Min_Missed_Male   = if (length(male_cols) > 0) apply(dplyr::select(., dplyr::all_of(male_cols)), 1, min, na.rm = TRUE) else NA_real_,
              Min_Missed_Female = if (length(female_cols) > 0) apply(dplyr::select(., dplyr::all_of(female_cols)), 1, min, na.rm = TRUE) else NA_real_
            ) %>%
            dplyr::mutate(dplyr::across(
              c(Avg_Missed_Male, Avg_Missed_Female, Max_Missed_Male, Max_Missed_Female, Min_Missed_Male, Min_Missed_Female),
              ~ round(., 2)
            ))
        }
        
        openxlsx::addWorksheet(wb, "Chronological Analysis")
        openxlsx::writeData(wb, "Chronological Analysis", chronological_data, startRow = 1)
      }
      
      openxlsx::addWorksheet(wb, "Raw Data")
      openxlsx::writeData(wb, "Raw Data", raw_data, startRow = 1)
      
      openxlsx::addWorksheet(wb, "Analysis Metrics")
      
      overall_metrics <- raw_data %>%
        dplyr::summarise(
          total_male_sampled      = sum(male_sampled, na.rm = TRUE),
          total_male_vaccinated   = sum(male_vaccinated, na.rm = TRUE),
          total_female_sampled    = sum(female_sampled, na.rm = TRUE),
          total_female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
          male_missed_rate        = ifelse(total_male_sampled > 0, round((1 - total_male_vaccinated / total_male_sampled) * 100, 1), 0),
          female_missed_rate      = ifelse(total_female_sampled > 0, round((1 - total_female_vaccinated / total_female_sampled) * 100, 1), 0),
          overall_missed_rate     = ifelse((total_male_sampled + total_female_sampled) > 0,
                                           round((1 - (total_male_vaccinated + total_female_vaccinated) /
                                                    (total_male_sampled + total_female_sampled)) * 100, 1), 0)
        )
      
      metrics_data <- data.frame(
        Metric = c(
          "Overall Missed Rate",
          "Boys Missed Rate",
          "Girls Missed Rate",
          "Total Boys Sampled",
          "Total Boys Vaccinated",
          "Total Girls Sampled",
          "Total Girls Vaccinated",
          "Number of Countries",
          "Number of Vaccine Types",
          "Time Period Covered",
          "Block Type",
          "Selected Block"
        ),
        Value = c(
          paste0(overall_metrics$overall_missed_rate, "%"),
          paste0(overall_metrics$male_missed_rate, "%"),
          paste0(overall_metrics$female_missed_rate, "%"),
          format(overall_metrics$total_male_sampled, big.mark = ","),
          format(overall_metrics$total_male_vaccinated, big.mark = ","),
          format(overall_metrics$total_female_sampled, big.mark = ","),
          format(overall_metrics$total_female_vaccinated, big.mark = ","),
          length(unique(raw_data$country)),
          length(unique(raw_data$vaccine.type)),
          if (!is.null(missed_analysis()$period_text)) missed_analysis()$period_text else "Not specified",
          if (!is.null(input$missed_block_type)) input$missed_block_type else "afro",
          if (!is.null(input$missed_block)) input$missed_block else "All"
        ),
        stringsAsFactors = FALSE
      )
      
      openxlsx::writeData(wb, "Analysis Metrics", metrics_data, startRow = 1)
      openxlsx::setColWidths(wb, "Analysis Metrics", cols = 1:2, widths = c(25, 25))
      
      openxlsx::addWorksheet(wb, "Data Description")
      
      description_data <- data.frame(
        Sheet_Name = c("Summary Data", "Chronological Analysis", "Raw Data", "Analysis Metrics", "Filter Information"),
        Description = c(
          "Processed data shown in the flextable",
          "Summary data plus averages/min/max by sex",
          "Original raw data",
          "Key calculated metrics",
          "Filters applied"
        ),
        Contents = c(
          "Country, Vaccine Type, month-by-sex missed %",
          "Summary + Avg/Min/Max missed by sex",
          "All original variables",
          "Missed rates + totals",
          "Time & geo parameters"
        ),
        stringsAsFactors = FALSE
      )
      
      openxlsx::writeData(wb, "Data Description", "Data Structure Overview", startRow = 1)
      openxlsx::writeData(wb, "Data Description", description_data, startRow = 3)
      openxlsx::setColWidths(wb, "Data Description", cols = 1:3, widths = c(20, 40, 50))
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
  
  
  # ============================================================
  # 📈 COVERAGE ANALYSIS (Updated with Dynamic Block Switching)
  # ============================================================
  
  # Reactive to store coverage analysis results
  coverage_analysis <- reactiveVal(NULL)
  
  # Reactive to check if coverage analysis results exist (for conditional panel)
  output$coverage_analysis_exists <- reactive({
    return(!is.null(coverage_analysis()))
  })
  
  # Ensure this output is available for conditional panels
  outputOptions(output, "coverage_analysis_exists", suspendWhenHidden = FALSE)
  
  # ADDED: Reactive block choices for coverage analysis
  coverage_block_choices <- reactive({
    if (input$coverage_block_type == "afro") {
      return(afro_blocks)
    } else {
      return(ist_blocks)
    }
  })
  
  # ADDED: Reactive block names for coverage analysis
  coverage_block_names <- reactive({
    if (input$coverage_block_type == "afro") {
      return(names(afro_blocks))
    } else {
      return(names(ist_blocks))
    }
  })
  
  # ADDED: Update block selection when block type changes
  observe({
    updateSelectInput(session, "coverage_block", 
                      choices = c("All", coverage_block_names()))
  })
  
  # MODIFIED: Update country selection to use reactive block choices
  output$coverage_countries_ui <- renderUI({
    req(dat)
    
    available_countries <- if (!is.null(input$coverage_block) && input$coverage_block != "All") {
      # MODIFIED: Use reactive block choices
      coverage_block_choices()[[input$coverage_block]]
    } else {
      sort(unique(dat$country))
    }
    
    selectizeInput(
      "coverage_countries", "Countries (Optional):",
      choices = c("All" = "", available_countries),
      multiple = TRUE,
      options = list(
        placeholder = 'Select one or more countries',
        maxItems = 15,
        plugins = list('remove_button'),
        dropdownParent = 'body'
      )
    )
  })
  
  # Main analysis function - UPDATED with dynamic block switching
  observeEvent(input$coverage_analyze, {
    if (input$tabs != "coverage") return()  # Only run if coverage tab is active
    
    showNotification("📊 Analyzing coverage data...", type = "message", duration = 3)
    
    safe_analysis({
      # Validate inputs
      if (input$coverage_time_type == "x_months" && (is.null(input$coverage_months) || input$coverage_months < 1)) {
        stop("Please enter a valid number of months (1-24)")
      }
      
      if (input$coverage_time_type == "quarter" && (is.null(input$coverage_year) || is.null(input$coverage_quarter))) {
        stop("Please select both year and quarter")
      }
      
      if (input$coverage_time_type == "last_quarters" && is.null(input$coverage_quarters)) {
        stop("Please select number of quarters")
      }
      
      # Prepare parameters
      params <- list(data = dat)
      
      if (input$coverage_time_type == "x_months") {
        params$x_months <- as.numeric(input$coverage_months)
      } else if (input$coverage_time_type == "quarter") {
        params$year <- as.numeric(input$coverage_year)
        params$Q <- as.numeric(input$coverage_quarter)
      } else if (input$coverage_time_type == "last_quarters") {
        params$last_n_quarters <- as.numeric(input$coverage_quarters)
      }
      
      # MODIFIED: Use block type and dynamic block selection
      params$block_selection <- if (input$coverage_block != "All") input$coverage_block else NULL
      params$country_selection <- if (!is.null(input$coverage_countries) && length(input$coverage_countries) > 0) input$coverage_countries else NULL
      params$block_type <- input$coverage_block_type  # ADDED: Pass block type
      params$afro_blocks <- afro_blocks  # ADDED: Pass AFRO blocks
      params$ist_blocks <- ist_blocks    # ADDED: Pass IST blocks
      
      # Run analysis
      result <- do.call(coverage_disaggregated, params)
      coverage_analysis(result)
      
      showNotification("✅ Coverage analysis completed successfully!", type = "message", duration = 3)
      
    }, "Coverage Analysis")
  })
  
  # Coverage value boxes
  output$coverage_overall <- renderValueBox({
    req(coverage_analysis())
    
    tryCatch({
      overall <- coverage_analysis()$raw_data %>%
        summarise(
          total_sampled = sum(male_sampled + female_sampled, na.rm = TRUE),
          total_vaccinated = sum(male_vaccinated + female_vaccinated, na.rm = TRUE),
          coverage = ifelse(total_sampled > 0, round((total_vaccinated/total_sampled) * 100, 1), 0)
        )
      valueBox(paste0(overall$coverage, "%"), "Overall Coverage", 
               icon = icon("shield-alt"), color = ifelse(overall$coverage >= 90, "green", "red"))
    }, error = function(e) {
      valueBox("N/A", "Overall Coverage", icon = icon("shield-alt"), color = "yellow")
    })
  })
  
  output$coverage_boys <- renderValueBox({
    req(coverage_analysis())
    
    tryCatch({
      boys <- coverage_analysis()$raw_data %>%
        summarise(
          total_sampled = sum(male_sampled, na.rm = TRUE),
          total_vaccinated = sum(male_vaccinated, na.rm = TRUE),
          coverage = ifelse(total_sampled > 0, round((total_vaccinated/total_sampled) * 100, 1), 0)
        )
      valueBox(paste0(boys$coverage, "%"), "Boys Coverage", 
               icon = icon("male"), color = ifelse(boys$coverage >= 90, "green", "red"))
    }, error = function(e) {
      valueBox("N/A", "Boys Coverage", icon = icon("male"), color = "yellow")
    })
  })
  
  output$coverage_girls <- renderValueBox({
    req(coverage_analysis())
    
    tryCatch({
      girls <- coverage_analysis()$raw_data %>%
        summarise(
          total_sampled = sum(female_sampled, na.rm = TRUE),
          total_vaccinated = sum(female_vaccinated, na.rm = TRUE),
          coverage = ifelse(total_sampled > 0, round((total_vaccinated/total_sampled) * 100, 1), 0)
        )
      valueBox(paste0(girls$coverage, "%"), "Girls Coverage", 
               icon = icon("female"), color = ifelse(girls$coverage >= 90, "green", "red"))
    }, error = function(e) {
      valueBox("N/A", "Girls Coverage", icon = icon("female"), color = "yellow")
    })
  })
  
  output$coverage_flextable_ui <- renderUI({
    req(coverage_analysis())
    if (!is.null(coverage_analysis()$flextable) &&
        !is.null(coverage_analysis()$summary_data) &&
        nrow(coverage_analysis()$summary_data) > 0) {
      ft_html <- htmltools_value(coverage_analysis()$flextable)
      div(class = "flextable-output", style = "overflow-x: auto; margin: 10px 0;", ft_html)
    } else {
      tags$div(class = "alert alert-warning",
               tags$h4("No data available"),
               tags$p("No coverage data found for the selected filters."))
    }
  })
  
  # Download handlers for coverage analysis
  output$download_coverage_table <- downloadHandler(
    filename = function() {
      paste("coverage_table_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(coverage_analysis())
      writexl::write_xlsx(coverage_analysis()$summary_data, file)
    }
  )
  
  output$download_coverage_data <- downloadHandler(
    filename = function() {
      paste("coverage_raw_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(coverage_analysis())
      writexl::write_xlsx(coverage_analysis()$raw_data, file)
    }
  )
  
  # NEW: Download handler for filtered coverage data
  output$download_coverage_filtered_data <- downloadHandler(
    filename = function() {
      paste("filtered_coverage_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(coverage_analysis())
      
      # Get the summary data (this is what's shown in the flextable)
      summary_data <- coverage_analysis()$summary_data
      raw_data <- coverage_analysis()$raw_data
      
      # Create filter information
      filters_applied <- data.frame(
        Parameter = c(
          "Date Generated",
          "Block Type",
          "Selected Block", 
          "Countries",
          "Time Period Type",
          "Analysis Period",
          "Total Records (Raw)",
          "Total Records (Summary)"
        ),
        Value = c(
          as.character(Sys.Date()),
          if (!is.null(input$coverage_block_type)) input$coverage_block_type else "afro",
          if (!is.null(input$coverage_block)) input$coverage_block else "All",
          if (!is.null(input$coverage_countries) && length(input$coverage_countries) > 0) 
            paste(input$coverage_countries, collapse = ", ") else "All",
          if (!is.null(input$coverage_time_type)) input$coverage_time_type else "x_months",
          if (!is.null(coverage_analysis()$period_text)) coverage_analysis()$period_text else "Not specified",
          format(nrow(raw_data), big.mark = ","),
          format(nrow(summary_data), big.mark = ",")
        )
      )
      
      # Add time-specific parameters
      time_params <- data.frame(
        Parameter = character(),
        Value = character()
      )
      
      if (input$coverage_time_type == "x_months") {
        time_params <- rbind(time_params, data.frame(
          Parameter = "Number of Months",
          Value = as.character(input$coverage_months)
        ))
      } else if (input$coverage_time_type == "quarter") {
        time_params <- rbind(time_params, 
                             data.frame(Parameter = "Year", Value = as.character(input$coverage_year)),
                             data.frame(Parameter = "Quarter", Value = as.character(input$coverage_quarter))
        )
      } else if (input$coverage_time_type == "last_quarters") {
        time_params <- rbind(time_params, data.frame(
          Parameter = "Last N Quarters",
          Value = as.character(input$coverage_quarters)
        ))
      }
      
      # Combine all filter information
      all_filters <- rbind(filters_applied, time_params)
      
      # Create a workbook with multiple sheets
      wb <- openxlsx::createWorkbook()
      
      # Add filter information sheet
      openxlsx::addWorksheet(wb, "Filter Information")
      openxlsx::writeData(wb, "Filter Information", 
                          "Coverage Analysis - Filter Settings", 
                          startRow = 1)
      openxlsx::writeData(wb, "Filter Information", all_filters, startRow = 3)
      openxlsx::setColWidths(wb, "Filter Information", cols = 1:2, widths = c(25, 50))
      
      # Add the main summary data sheet (this is what's in the flextable)
      openxlsx::addWorksheet(wb, "Summary Data")
      openxlsx::writeData(wb, "Summary Data", summary_data, startRow = 1)
      
      # Add a sheet with chronological analysis data (wide format with additional metrics)
      if (nrow(summary_data) > 0) {
        # Create a chronological analysis sheet with additional metrics
        chronological_data <- summary_data
        
        # Add calculated columns for analysis
        time_cols <- names(chronological_data)[str_detect(names(chronological_data), "^\\d{4}_.+_")]
        
        if (length(time_cols) > 0) {
          # Calculate average coverage rates by gender
          male_cols <- time_cols[str_detect(time_cols, "_M$")]
          female_cols <- time_cols[str_detect(time_cols, "_F$")]
          
          chronological_data <- chronological_data %>%
            mutate(
              Avg_Coverage_Male = if (length(male_cols) > 0) rowMeans(select(., all_of(male_cols)), na.rm = TRUE) else NA,
              Avg_Coverage_Female = if (length(female_cols) > 0) rowMeans(select(., all_of(female_cols)), na.rm = TRUE) else NA,
              Min_Coverage_Male = if (length(male_cols) > 0) apply(select(., all_of(male_cols)), 1, min, na.rm = TRUE) else NA,
              Min_Coverage_Female = if (length(female_cols) > 0) apply(select(., all_of(female_cols)), 1, min, na.rm = TRUE) else NA,
              Max_Coverage_Male = if (length(male_cols) > 0) apply(select(., all_of(male_cols)), 1, max, na.rm = TRUE) else NA,
              Max_Coverage_Female = if (length(female_cols) > 0) apply(select(., all_of(female_cols)), 1, max, na.rm = TRUE) else NA,
              # Calculate if targets are met
              Male_Target_Met = ifelse(Avg_Coverage_Male >= 90, "Yes", "No"),
              Female_Target_Met = ifelse(Avg_Coverage_Female >= 90, "Yes", "No")
            ) %>%
            # Round the calculated columns
            mutate(across(c(Avg_Coverage_Male, Avg_Coverage_Female, 
                            Min_Coverage_Male, Min_Coverage_Female,
                            Max_Coverage_Male, Max_Coverage_Female), 
                          ~ round(., 2)))
        }
        
        openxlsx::addWorksheet(wb, "Chronological Analysis")
        openxlsx::writeData(wb, "Chronological Analysis", chronological_data, startRow = 1)
      }
      
      # Add raw data sheet
      openxlsx::addWorksheet(wb, "Raw Data")
      openxlsx::writeData(wb, "Raw Data", raw_data, startRow = 1)
      
      # Add analysis metrics sheet
      openxlsx::addWorksheet(wb, "Analysis Metrics")
      
      # Calculate metrics from raw data
      overall_metrics <- raw_data %>%
        summarise(
          total_male_sampled = sum(male_sampled, na.rm = TRUE),
          total_male_vaccinated = sum(male_vaccinated, na.rm = TRUE),
          total_female_sampled = sum(female_sampled, na.rm = TRUE),
          total_female_vaccinated = sum(female_vaccinated, na.rm = TRUE),
          male_coverage = ifelse(total_male_sampled > 0, 
                                 round((total_male_vaccinated/total_male_sampled) * 100, 1), 0),
          female_coverage = ifelse(total_female_sampled > 0, 
                                   round((total_female_vaccinated/total_female_sampled) * 100, 1), 0),
          overall_coverage = ifelse((total_male_sampled + total_female_sampled) > 0,
                                    round(((total_male_vaccinated + total_female_vaccinated) / 
                                             (total_male_sampled + total_female_sampled)) * 100, 1), 0)
        )
      
      metrics_data <- data.frame(
        Metric = c(
          "Overall Coverage",
          "Boys Coverage", 
          "Girls Coverage",
          "Total Boys Sampled",
          "Total Boys Vaccinated",
          "Total Girls Sampled", 
          "Total Girls Vaccinated",
          "Boys Target Met (≥90%)",
          "Girls Target Met (≥90%)",
          "Number of Countries",
          "Number of Vaccine Types",
          "Time Period Covered",
          "Block Type",
          "Selected Block"
        ),
        Value = c(
          paste0(overall_metrics$overall_coverage, "%"),
          paste0(overall_metrics$male_coverage, "%"),
          paste0(overall_metrics$female_coverage, "%"),
          format(overall_metrics$total_male_sampled, big.mark = ","),
          format(overall_metrics$total_male_vaccinated, big.mark = ","),
          format(overall_metrics$total_female_sampled, big.mark = ","),
          format(overall_metrics$total_female_vaccinated, big.mark = ","),
          ifelse(overall_metrics$male_coverage >= 90, "Yes", "No"),
          ifelse(overall_metrics$female_coverage >= 90, "Yes", "No"),
          length(unique(raw_data$country)),
          length(unique(raw_data$vaccine.type)),
          if (!is.null(coverage_analysis()$period_text)) coverage_analysis()$period_text else "Not specified",
          if (!is.null(input$coverage_block_type)) input$coverage_block_type else "afro",
          if (!is.null(input$coverage_block)) input$coverage_block else "All"
        )
      )
      
      openxlsx::writeData(wb, "Analysis Metrics", metrics_data, startRow = 1)
      openxlsx::setColWidths(wb, "Analysis Metrics", cols = 1:2, widths = c(25, 25))
      
      # Add a sheet explaining the data structure
      openxlsx::addWorksheet(wb, "Data Description")
      
      description_data <- data.frame(
        Sheet_Name = c(
          "Summary Data",
          "Chronological Analysis", 
          "Raw Data",
          "Analysis Metrics",
          "Filter Information"
        ),
        Description = c(
          "Processed data showing coverage percentages by country, vaccine type, month, and gender (same as displayed in the flextable)",
          "Summary data with additional calculated metrics (averages, min/max, target status) for trend analysis",
          "Original raw data with all variables before processing",
          "Key calculated metrics and summary statistics",
          "Details of all filters applied to generate this analysis"
        ),
        Contents = c(
          "Country, Vaccine Type, and monthly coverage percentages for males (M) and females (F) in chronological order",
          "All Summary Data columns plus: Avg_Coverage_Male, Avg_Coverage_Female, Min_Coverage_Male, Min_Coverage_Female, Max_Coverage_Male, Max_Coverage_Female, Male_Target_Met, Female_Target_Met",
          "All original variables including country, province, district, dates, sampled/vaccinated counts",
          "Overall and gender-specific coverage rates, total counts, target achievement, and other key metrics",
          "Time period, geographic filters, and other parameters used in the analysis"
        )
      )
      
      openxlsx::writeData(wb, "Data Description", "Data Structure Overview", startRow = 1)
      openxlsx::writeData(wb, "Data Description", description_data, startRow = 3)
      openxlsx::setColWidths(wb, "Data Description", cols = 1:3, widths = c(20, 40, 50))
      
      # Save the workbook
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  # ============================================================
  # 🗺️ DISTRICT PERFORMANCE + FACETED HEATMAP SERVER
  # ============================================================
  
  district_perf_analysis <- reactiveVal(NULL)
  district_perf_running  <- reactiveVal(FALSE)
  
  last_perf_block     <- reactiveVal("All")
  last_perf_countries <- reactiveVal(character(0))
  last_perf_provinces <- reactiveVal(character(0))
  
  perf_countries_debounced <- debounce(reactive(input$perf_countries), 500)
  perf_provinces_debounced <- debounce(reactive(input$perf_provinces), 500)
  perf_districts_debounced <- debounce(reactive(input$perf_districts), 500)
  
  perf_data_reactive <- reactive({
    req(dat)
    dat
  })
  
  perf_block_choices <- reactive({
    if (identical(input$perf_block_type, "afro")) afro_blocks else ist_blocks
  })
  
  perf_block_names <- reactive({
    if (identical(input$perf_block_type, "afro")) names(afro_blocks) else names(ist_blocks)
  })
  
  observe({
    updateSelectInput(
      session,
      "perf_block",
      choices = c("All", perf_block_names()),
      selected = "All"
    )
  })
  
  observeEvent(perf_data_reactive(), {
    req(perf_data_reactive())
    
    countries <- perf_data_reactive() %>%
      dplyr::distinct(country) %>%
      dplyr::pull(country) %>%
      as.character() %>%
      sort() %>%
      stats::na.omit()
    
    updateSelectizeInput(
      session,
      "perf_countries",
      choices = c("All" = "", countries),
      selected = character(0),
      server = TRUE
    )
  }, once = TRUE)
  
  observe({
    req(perf_data_reactive(), input$perf_block)
    
    current_block  <- input$perf_block
    previous_block <- last_perf_block()
    
    if (!identical(current_block, previous_block)) {
      
      last_perf_block(current_block)
      
      available_countries <- if (!identical(current_block, "All")) {
        perf_block_choices()[[current_block]]
      } else {
        sort(unique(as.character(perf_data_reactive()$country)))
      }
      
      current_selection <- input$perf_countries
      valid_selection   <- intersect(current_selection, available_countries)
      
      updateSelectizeInput(
        session,
        "perf_countries",
        choices = c("All" = "", available_countries),
        selected = if (length(valid_selection) > 0) valid_selection else character(0),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "perf_provinces",
        choices = c("All" = ""),
        selected = character(0),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "perf_districts",
        choices = c("All" = ""),
        selected = character(0),
        server = TRUE
      )
    }
  })
  
  observe({
    req(perf_data_reactive(), perf_countries_debounced())
    
    current_countries  <- perf_countries_debounced()
    previous_countries <- last_perf_countries()
    
    if (!identical(sort(current_countries), sort(previous_countries))) {
      
      last_perf_countries(current_countries)
      
      if (length(current_countries) > 0 && !all(current_countries == "")) {
        
        provinces <- perf_data_reactive() %>%
          dplyr::filter(country %in% current_countries) %>%
          dplyr::distinct(province) %>%
          dplyr::pull(province) %>%
          sort() %>%
          stats::na.omit()
        
        current_selection <- input$perf_provinces
        valid_selection   <- intersect(current_selection, provinces)
        
        updateSelectizeInput(
          session,
          "perf_provinces",
          choices = c("All" = "", provinces),
          selected = if (length(valid_selection) > 0) valid_selection else character(0),
          server = TRUE
        )
        
      } else {
        
        updateSelectizeInput(
          session,
          "perf_provinces",
          choices = c("All" = ""),
          selected = character(0),
          server = TRUE
        )
      }
      
      updateSelectizeInput(
        session,
        "perf_districts",
        choices = c("All" = ""),
        selected = character(0),
        server = TRUE
      )
    }
  })
  
  observe({
    req(perf_data_reactive(), input$perf_countries)
    
    current_provinces  <- input$perf_provinces
    previous_provinces <- last_perf_provinces()
    
    if (!identical(sort(current_provinces), sort(previous_provinces))) {
      
      last_perf_provinces(current_provinces)
      
      if (length(input$perf_countries) > 0 && !all(input$perf_countries == "")) {
        
        districts_data <- perf_data_reactive() %>%
          dplyr::filter(country %in% input$perf_countries)
        
        provinces_selected <- !is.null(current_provinces) &&
          length(current_provinces) > 0 &&
          !all(current_provinces == "")
        
        if (isTRUE(provinces_selected)) {
          districts_data <- districts_data %>%
            dplyr::filter(province %in% current_provinces)
        }
        
        districts <- districts_data %>%
          dplyr::distinct(district) %>%
          dplyr::pull(district) %>%
          sort() %>%
          stats::na.omit()
        
        current_selection <- input$perf_districts
        valid_selection   <- intersect(current_selection, districts)
        
        updateSelectizeInput(
          session,
          "perf_districts",
          choices = c("All" = "", districts),
          selected = if (length(valid_selection) > 0) valid_selection else character(0),
          server = TRUE
        )
        
      } else {
        
        updateSelectizeInput(
          session,
          "perf_districts",
          choices = c("All" = ""),
          selected = character(0),
          server = TRUE
        )
      }
    }
  })
  
  observeEvent(input$perf_analyze, {
    
    if (!identical(input$tabs, "district_perf")) return()
    if (isTRUE(district_perf_running())) return()
    
    req(dat)
    
    district_perf_running(TRUE)
    
    on.exit({
      district_perf_running(FALSE)
    }, add = TRUE)
    
    if (!exists("district_lqas_performance")) {
      showNotification(
        "❌ ERROR: district_lqas_performance() not found!",
        type = "error",
        duration = 10
      )
      return()
    }
    
    safe_analysis({
      
      current_perf_block <- if (is.null(input$perf_block)) "All" else input$perf_block
      
      current_perf_countries <- if (!is.null(input$perf_countries) &&
                                    length(input$perf_countries) > 0 &&
                                    !all(input$perf_countries == "")) {
        input$perf_countries
      } else {
        NULL
      }
      
      current_perf_provinces <- if (!is.null(input$perf_provinces) &&
                                    length(input$perf_provinces) > 0 &&
                                    !all(input$perf_provinces == "")) {
        input$perf_provinces
      } else {
        NULL
      }
      
      current_perf_districts <- if (!is.null(input$perf_districts) &&
                                    length(input$perf_districts) > 0 &&
                                    !all(input$perf_districts == "")) {
        input$perf_districts
      } else {
        NULL
      }
      
      current_perf_time_type <- if (is.null(input$perf_time_type)) {
        "x_months"
      } else {
        input$perf_time_type
      }
      
      current_perf_status_filter <- if (is.null(input$perf_status_filter)) {
        character(0)
      } else {
        input$perf_status_filter
      }
      
      never_high  <- "never_high" %in% current_perf_status_filter
      always_high <- "always_high" %in% current_perf_status_filter
      
      if (never_high && always_high) {
        showNotification(
          "Cannot select both. Using 'Always High Performing'.",
          type = "warning",
          duration = 6
        )
        never_high <- FALSE
      }
      
      x_months <- NULL
      year_selection <- NULL
      year_filter <- NULL
      month_filter <- NULL
      last_data_months <- NULL
      
      if (identical(current_perf_time_type, "x_months")) {
        
        x_months <- if (!is.null(input$perf_months)) input$perf_months else 6
        
      } else if (identical(current_perf_time_type, "x_data_months")) {
        
        last_data_months <- if (!is.null(input$perf_data_months)) input$perf_data_months else 6
        
      } else if (identical(current_perf_time_type, "year")) {
        
        year_selection <- if (!is.null(input$perf_year)) {
          input$perf_year
        } else {
          as.numeric(format(Sys.Date(), "%Y"))
        }
        
      } else if (identical(current_perf_time_type, "year_months")) {
        
        year_selection <- if (!is.null(input$perf_year_months)) {
          input$perf_year_months
        } else {
          as.numeric(format(Sys.Date(), "%Y"))
        }
        
        x_months <- if (!is.null(input$perf_months_year)) {
          input$perf_months_year
        } else {
          6
        }
        
      } else if (identical(current_perf_time_type, "year_month")) {
        
        year_filter <- if (!is.null(input$perf_year_month)) {
          input$perf_year_month
        } else {
          as.numeric(format(Sys.Date(), "%Y"))
        }
        
        month_filter <- if (!is.null(input$perf_month_name) &&
                            length(input$perf_month_name) > 0) {
          as.integer(input$perf_month_name)
        } else {
          as.integer(format(Sys.Date(), "%m"))
        }
      }
      
      heatmap_geo_level <- if (!is.null(input$perf_heatmap_geo_level)) {
        input$perf_heatmap_geo_level
      } else {
        "country"
      }
      
      show_district_labels <- isTRUE(input$perf_show_district_labels)
      
      district_perf_analysis(NULL)
      force_gc()
      
      result <- tryCatch(
        district_lqas_performance(
          data = dat,
          
          block_selection = if (!identical(current_perf_block, "All")) {
            current_perf_block
          } else {
            NULL
          },
          
          country_selection  = current_perf_countries,
          province_selection = current_perf_provinces,
          district_selection = current_perf_districts,
          
          x_months = x_months,
          year_selection = year_selection,
          year_filter = year_filter,
          month_filter = month_filter,
          last_data_months = last_data_months,
          
          never_high_performing = if (isTRUE(never_high)) TRUE else NULL,
          always_high_performing = if (isTRUE(always_high)) TRUE else NULL,
          
          show_district_labels = show_district_labels,
          heatmap_geo_level = heatmap_geo_level,
          
          afro_blocks = afro_blocks,
          ist_blocks = ist_blocks,
          block_type = input$perf_block_type,
          
          all_countries = all_countries,
          all_provinces = all_provinces,
          all_districts = all_districts,
          ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
        ),

        error = function(e) {
          showNotification(
            paste0("❌ District Performance failed: ", e$message),
            type = "error",
            duration = 12
          )
          NULL
        }
      )
      
      if (is.null(result)) return()
      
      district_perf_analysis(result)
      force_gc()
      
    }, "District Performance Analysis")
  })
  
  output$perf_analysis_exists <- reactive(!is.null(district_perf_analysis()))
  outputOptions(output, "perf_analysis_exists", suspendWhenHidden = FALSE)
  
  output$perf_total_districts <- renderValueBox({
    req(district_perf_analysis())
    
    total <- if (!is.null(district_perf_analysis()$summary_metrics)) {
      district_perf_analysis()$summary_metrics$total_districts
    } else if (!is.null(district_perf_analysis()$summary_data)) {
      sum(district_perf_analysis()$summary_data$total_districts, na.rm = TRUE)
    } else {
      0
    }
    
    valueBox(total, "Total Districts", icon = icon("map"), color = "aqua")
  })
  
  output$perf_high_performing <- renderValueBox({
    req(district_perf_analysis())
    
    high <- if (!is.null(district_perf_analysis()$summary_metrics)) {
      district_perf_analysis()$summary_metrics$high_performing_districts
    } else if (!is.null(district_perf_analysis()$summary_data)) {
      sum(district_perf_analysis()$summary_data$high_performing_districts, na.rm = TRUE)
    } else {
      0
    }
    
    valueBox(high, "Always High Performing", icon = icon("check-circle"), color = "green")
  })
  
  output$perf_poor_performing <- renderValueBox({
    req(district_perf_analysis())
    
    poor <- if (!is.null(district_perf_analysis()$summary_metrics)) {
      district_perf_analysis()$summary_metrics$poor_performing_districts
    } else if (!is.null(district_perf_analysis()$summary_data)) {
      sum(district_perf_analysis()$summary_data$poor_performing_districts, na.rm = TRUE)
    } else {
      0
    }
    
    valueBox(poor, "Never High Performing", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$perf_maps <- renderPlot({
    req(district_perf_analysis())
    district_perf_analysis()$multiplot
  })
  
  output$perf_heatmap <- renderPlot({
    req(district_perf_analysis())
    district_perf_analysis()$heatmap
  })
  
  output$perf_summary_table_ui <- renderUI({
    req(district_perf_analysis())
    
    if (!is.null(district_perf_analysis()$summary_table) &&
        !is.null(district_perf_analysis()$summary_data) &&
        nrow(district_perf_analysis()$summary_data) > 0) {
      
      ft_html <- htmltools_value(district_perf_analysis()$summary_table)
      
      div(
        class = "flextable-output",
        style = "overflow-x: auto; margin: 10px 0;",
        ft_html
      )
      
    } else {
      
      tags$div(
        class = "alert alert-warning",
        tags$h4("No data available"),
        tags$p("No performance data found for the selected filters.")
      )
    }
  })
  
  output$download_perf_maps <- downloadHandler(
    filename = function() {
      paste0("district_performance_maps_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(district_perf_analysis())
      
      ggplot2::ggsave(
        file,
        plot = district_perf_analysis()$multiplot,
        width = 16,
        height = 12,
        dpi = 300
      )
    }
  )
  
  output$download_perf_heatmap <- downloadHandler(
    filename = function() {
      paste0(
        "lqas_high_performing_heatmap_",
        district_perf_analysis()$heatmap_geo_level,
        "_",
        Sys.Date(),
        ".png"
      )
    },
    content = function(file) {
      req(district_perf_analysis())
      
      ggplot2::ggsave(
        file,
        plot = district_perf_analysis()$heatmap,
        width = 16,
        height = 9,
        dpi = 300
      )
    }
  )
  
  output$download_perf_table <- downloadHandler(
    filename = function() {
      paste0("district_performance_table_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(district_perf_analysis())
      writexl::write_xlsx(district_perf_analysis()$summary_data, file)
    }
  )
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      paste0("filtered_district_performance_data_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      req(district_perf_analysis())
      
      filtered_data <- district_perf_analysis()$filtered_data
      
      time_filter_label <- input$perf_time_type
      
      if (identical(input$perf_time_type, "year_month")) {
        mm <- suppressWarnings(as.integer(input$perf_month_name))
        mm <- mm[!is.na(mm)]
        mm_txt <- if (length(mm) > 0) paste(month.abb[mm], collapse = ", ") else "NA"
        
        time_filter_label <- paste0(
          "Specific Year + Month(s) (",
          input$perf_year_month,
          ": ",
          mm_txt,
          ")"
        )
      }
      
      if (identical(input$perf_time_type, "x_data_months")) {
        time_filter_label <- paste0(
          "Last X Data Months (X=",
          input$perf_data_months,
          ")"
        )
      }
      
      filters_applied <- data.frame(
        Parameter = c(
          "Date Generated",
          "Block Type",
          "Selected Block",
          "Countries",
          "Provinces",
          "Districts",
          "Time Filter Type",
          "Heatmap Geography Level",
          "Performance Filters",
          "Data Period",
          "Total Records"
        ),
        Value = c(
          as.character(Sys.Date()),
          if (!is.null(input$perf_block_type)) input$perf_block_type else "afro",
          if (!is.null(input$perf_block)) input$perf_block else "All",
          if (!is.null(input$perf_countries) && length(input$perf_countries) > 0) {
            paste(input$perf_countries[input$perf_countries != ""], collapse = ", ")
          } else {
            "All"
          },
          if (!is.null(input$perf_provinces) && length(input$perf_provinces) > 0) {
            paste(input$perf_provinces[input$perf_provinces != ""], collapse = ", ")
          } else {
            "All"
          },
          if (!is.null(input$perf_districts) && length(input$perf_districts) > 0) {
            paste(input$perf_districts[input$perf_districts != ""], collapse = ", ")
          } else {
            "All"
          },
          time_filter_label,
          if (!is.null(input$perf_heatmap_geo_level)) input$perf_heatmap_geo_level else "country",
          if (!is.null(input$perf_status_filter) && length(input$perf_status_filter) > 0) {
            paste(input$perf_status_filter, collapse = ", ")
          } else {
            "None"
          },
          district_perf_analysis()$period_info,
          format(nrow(filtered_data), big.mark = ",")
        )
      )
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(wb, "Filter Information")
      openxlsx::writeData(
        wb,
        "Filter Information",
        "District Performance Analysis - Filter Settings",
        startRow = 1
      )
      openxlsx::writeData(wb, "Filter Information", filters_applied, startRow = 3)
      openxlsx::setColWidths(wb, "Filter Information", cols = 1:2, widths = c(25, 60))
      
      openxlsx::addWorksheet(wb, "Filtered Data")
      openxlsx::writeData(wb, "Filtered Data", filtered_data, startRow = 1)
      
      openxlsx::addWorksheet(wb, "Summary Data")
      openxlsx::writeData(wb, "Summary Data", district_perf_analysis()$summary_data, startRow = 1)
      
      openxlsx::addWorksheet(wb, "Performance Metrics")
      
      if (!is.null(district_perf_analysis()$summary_metrics)) {
        
        metrics <- district_perf_analysis()$summary_metrics
        
        metrics_data <- data.frame(
          Metric = c(
            "Total Districts",
            "Always High Performing Districts",
            "Never High Performing Districts",
            "Block Type",
            "Selected Block",
            "Heatmap Geography Level",
            "Analysis Period"
          ),
          Value = c(
            as.character(metrics$total_districts),
            as.character(metrics$high_performing_districts),
            as.character(metrics$poor_performing_districts),
            if (!is.null(input$perf_block_type)) input$perf_block_type else "afro",
            if (!is.null(input$perf_block)) input$perf_block else "All",
            if (!is.null(input$perf_heatmap_geo_level)) input$perf_heatmap_geo_level else "country",
            district_perf_analysis()$period_info
          )
        )
        
        openxlsx::writeData(wb, "Performance Metrics", metrics_data, startRow = 1)
        openxlsx::setColWidths(wb, "Performance Metrics", cols = 1:2, widths = c(35, 35))
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  # ============================================================
  # 🌍 LQAS SUMMARY MAPS — FULL UPDATED SERVER (READY TO PASTE)
  # ✅ Province labels now optional
  # ============================================================
  
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
  
  build_lqas_filtered_powerbi_ui <- function(filtered_df,
                                             rows_per_panel = 28,
                                             max_cols_per_row = 2) {
    
    if (is.null(filtered_df) || nrow(filtered_df) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        tags$b("No data available for the selected filters.")
      ))
    }
    
    needed <- c("country","province","district","round_start_date","performance","High.perf.summary")
    for (nm in needed) if (!nm %in% names(filtered_df)) filtered_df[[nm]] <- NA
    
    filtered_df <- filtered_df |>
      dplyr::select(dplyr::all_of(needed)) |>
      dplyr::arrange(.data$country, .data$province, .data$district, .data$round_start_date)
    
    country_colors <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
      "#bcbd22", "#17becf"
    )
    
    chunk_df_local <- function(x, n = 28) {
      if (is.null(x) || nrow(x) == 0) return(list())
      if (nrow(x) <= n) return(list(x))
      idx <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / n))
      lapply(idx, function(i) x[i, , drop = FALSE])
    }
    
    mini_table_card <- function(df_part, country_name, header_bg) {
      df_part[] <- lapply(df_part, as.character)
      
      tags$div(
        style = "
      border: 1px solid #e5e7eb;
      border-radius: 10px;
      padding: 8px;
      background: #ffffff;
      box-shadow: 0 1px 2px rgba(0,0,0,0.06);
      margin-bottom: 10px;
    ",
        tags$div(
          style = paste0("
        background:", header_bg, ";
        color: white;
        font-weight: 700;
        padding: 6px 10px;
        border-radius: 8px;
        margin-bottom: 6px;
        font-size: 12px;
        display:flex;
        justify-content:space-between;
        align-items:center;
      "),
          tags$span(country_name),
          tags$span(paste0(nrow(df_part), " rows"), style = "opacity:0.9; font-weight:600;")
        ),
        tags$div(
          style = "overflow-x:auto; width:100%;",
          tags$table(
            class = "table table-condensed table-striped",
            style = "
          margin-bottom:0;
          font-size:11px;
          table-layout: fixed;
          width: 100%;
        ",
            tags$style(HTML("
          .table td, .table th {
            padding: 4px 6px !important;
            vertical-align: top !important;
            white-space: normal !important;
            word-break: break-word !important;
            overflow-wrap: anywhere !important;
          }
        ")),
            tags$thead(
              tags$tr(
                lapply(names(df_part), function(nm) tags$th(style = "font-weight:700;", nm))
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(df_part)), function(i) {
                tags$tr(
                  lapply(df_part[i, , drop = FALSE], function(v) tags$td(as.character(v)))
                )
              })
            )
          )
        )
      )
    }
    
    countries <- sort(unique(as.character(filtered_df$country)))
    panels <- list()
    
    for (k in seq_along(countries)) {
      ctry <- countries[k]
      header_bg <- country_colors[(k - 1) %% length(country_colors) + 1]
      sub <- filtered_df |> dplyr::filter(.data$country == ctry)
      parts <- chunk_df_local(sub, n = rows_per_panel)
      
      for (p in seq_along(parts)) {
        df_part <- parts[[p]]
        country_label <- if (length(parts) > 1) paste0(ctry, " (", p, "/", length(parts), ")") else ctry
        panels[[length(panels) + 1]] <- mini_table_card(df_part, country_label, header_bg)
      }
    }
    
    rows <- split(seq_along(panels), ceiling(seq_along(panels) / max_cols_per_row))
    
    tags$div(
      lapply(rows, function(ix) {
        fluidRow(
          lapply(ix, function(i) {
            column(width = floor(12 / max_cols_per_row), panels[[i]])
          })
        )
      })
    )
  }
  
  build_lqas_province_summary_ui <- function(province_df,
                                             rows_per_panel = 24,
                                             max_cols_per_row = 2) {
    
    if (is.null(province_df) || nrow(province_df) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        tags$b("No province summary available for the selected filters.")
      ))
    }
    
    needed <- c("country", "province", "high (%)")
    for (nm in needed) if (!nm %in% names(province_df)) province_df[[nm]] <- NA
    
    province_df <- province_df |>
      dplyr::select(dplyr::all_of(needed)) |>
      dplyr::arrange(.data$country, dplyr::desc(.data$`high (%)`), .data$province)
    
    country_colors <- c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
      "#bcbd22", "#17becf"
    )
    
    chunk_df_local <- function(x, n = 24) {
      if (is.null(x) || nrow(x) == 0) return(list())
      if (nrow(x) <= n) return(list(x))
      idx <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / n))
      lapply(idx, function(i) x[i, , drop = FALSE])
    }
    
    mini_table_card <- function(df_part, country_name, header_bg) {
      df_part[] <- lapply(df_part, as.character)
      
      tags$div(
        style = "
      border: 1px solid #e5e7eb;
      border-radius: 12px;
      padding: 8px;
      background: #ffffff;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      margin-bottom: 12px;
    ",
        tags$div(
          style = paste0("
        background:", header_bg, ";
        color:white;
        font-weight:700;
        padding:8px 12px;
        border-radius:8px;
        margin-bottom:8px;
        font-size:13px;
        display:flex;
        justify-content:space-between;
        align-items:center;
      "),
          tags$span(country_name),
          tags$span(paste0(nrow(df_part), " provinces"), style = "opacity:0.9; font-weight:600;")
        ),
        tags$div(
          style = "overflow-x:auto; width:100%;",
          tags$table(
            class = "table table-condensed table-striped",
            style = "
          margin-bottom:0;
          font-size:12px;
          table-layout: fixed;
          width:100%;
        ",
            tags$thead(
              tags$tr(
                lapply(names(df_part), function(nm) {
                  tags$th(style = "font-weight:700; background:#F3F4F6;", nm)
                })
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(df_part)), function(i) {
                tags$tr(
                  lapply(seq_along(df_part[i, , drop = FALSE]), function(j) {
                    v <- df_part[i, j][[1]]
                    style_txt <- if (names(df_part)[j] == "high (%)") "font-weight:700; text-align:center;" else ""
                    tags$td(style = style_txt, as.character(v))
                  })
                )
              })
            )
          )
        )
      )
    }
    
    countries <- sort(unique(as.character(province_df$country)))
    panels <- list()
    
    for (k in seq_along(countries)) {
      ctry <- countries[k]
      header_bg <- country_colors[(k - 1) %% length(country_colors) + 1]
      sub <- province_df |> dplyr::filter(.data$country == ctry)
      parts <- chunk_df_local(sub, n = rows_per_panel)
      
      for (p in seq_along(parts)) {
        df_part <- parts[[p]]
        country_label <- if (length(parts) > 1) paste0(ctry, " (", p, "/", length(parts), ")") else ctry
        panels[[length(panels) + 1]] <- mini_table_card(df_part, country_label, header_bg)
      }
    }
    
    rows <- split(seq_along(panels), ceiling(seq_along(panels) / max_cols_per_row))
    
    tags$div(
      lapply(rows, function(ix) {
        fluidRow(
          lapply(ix, function(i) {
            column(width = floor(12 / max_cols_per_row), panels[[i]])
          })
        )
      })
    )
  }
  
  # ============================================================
  # Dynamic block switching
  # ============================================================
  lqas_block_choices <- reactive({
    if (input$lqas_block_type == "afro") afro_blocks else ist_blocks
  })
  lqas_block_names <- reactive({
    if (input$lqas_block_type == "afro") names(afro_blocks) else names(ist_blocks)
  })
  
  observe({
    updateSelectInput(session, "lqas_block", choices = c("All", lqas_block_names()))
  })
  
  # ============================================================
  # Cascading inputs
  # ============================================================
  observe({
    req(dat, input$lqas_block)
    
    available_countries <- if (!is.null(input$lqas_block) && input$lqas_block != "All") {
      lqas_block_choices()[[input$lqas_block]]
    } else {
      sort(unique(dat$country))
    }
    
    updateSelectizeInput(
      session, "lqas_countries",
      choices = c("All" = "", available_countries),
      selected = character(0)
    )
  })
  
  observe({
    req(dat)
    
    if (!is.null(input$lqas_countries) && length(input$lqas_countries) > 0 && !all(input$lqas_countries == "")) {
      provinces <- dat %>%
        dplyr::filter(.data$country %in% input$lqas_countries) %>%
        dplyr::distinct(.data$province) %>%
        dplyr::pull(.data$province) %>%
        sort() %>%
        stats::na.omit()
      
      updateSelectizeInput(session, "lqas_provinces",
                           choices = c("All" = "", provinces),
                           selected = character(0))
    } else {
      updateSelectizeInput(session, "lqas_provinces",
                           choices = c("All" = ""),
                           selected = character(0))
    }
  })
  
  observe({
    req(dat)
    
    if (!is.null(input$lqas_countries) && length(input$lqas_countries) > 0 && !all(input$lqas_countries == "")) {
      districts_data <- dat %>% dplyr::filter(.data$country %in% input$lqas_countries)
      
      if (!is.null(input$lqas_provinces) && length(input$lqas_provinces) > 0 && !all(input$lqas_provinces == "")) {
        districts_data <- districts_data %>% dplyr::filter(.data$province %in% input$lqas_provinces)
      }
      
      districts <- districts_data %>%
        dplyr::distinct(.data$district) %>%
        dplyr::pull(.data$district) %>%
        sort() %>%
        stats::na.omit()
      
      updateSelectizeInput(session, "lqas_districts",
                           choices = c("All" = "", districts),
                           selected = character(0))
    } else {
      updateSelectizeInput(session, "lqas_districts",
                           choices = c("All" = ""),
                           selected = character(0))
    }
  })
  
  observe({
    req(input$lqas_block)
    updateSelectizeInput(session, "lqas_provinces", selected = character(0))
    updateSelectizeInput(session, "lqas_districts", selected = character(0))
  })
  
  # ============================================================
  # Run analysis
  # ============================================================
  observeEvent(input$lqas_analyze, {
    if (input$tabs != "lqas_maps") return()
    
    showNotification("🔄 Running optimized LQAS analysis...", type = "message", duration = 3)
    
    safe_analysis({
      
      x <- if (input$lqas_time_type == "months") input$lqas_months else NULL
      y <- if (input$lqas_time_type == "years" && !is.null(input$lqas_years) && nzchar(input$lqas_years)) {
        suppressWarnings(as.numeric(trimws(strsplit(input$lqas_years, ",")[[1]])))
      } else NULL
      
      selected_pf <- input$lqas_perf_filter %||% character(0)
      high_performing <- "high_performing" %in% selected_pf
      low_performing  <- "low_performing"  %in% selected_pf
      
      country_selection <- if (!is.null(input$lqas_countries) && length(input$lqas_countries) > 0 && !all(input$lqas_countries == "")) {
        input$lqas_countries
      } else NULL
      
      province_selection <- if (!is.null(input$lqas_provinces) && length(input$lqas_provinces) > 0 && !all(input$lqas_provinces == "")) {
        input$lqas_provinces
      } else NULL
      
      district_selection <- if (!is.null(input$lqas_districts) && length(input$lqas_districts) > 0 && !all(input$lqas_districts == "")) {
        input$lqas_districts
      } else NULL
      
      lqas_analysis(NULL)
      force_gc()
      
      result <- generate_lqas_summary_map_optimized(
        data = dat,
        x = x,
        y = y,
        block_selection = if (!is.null(input$lqas_block) && input$lqas_block != "All") input$lqas_block else NULL,
        country_selection = country_selection,
        province_selection = province_selection,
        district_selection = district_selection,
        high_performing_filter = isTRUE(high_performing),
        low_performing_filter  = isTRUE(low_performing),
        show_province_labels   = isTRUE(input$lqas_show_province_labels),
        all_countries = all_countries,
        all_provinces = all_provinces,
        all_districts = all_districts,
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,
        block_type = input$lqas_block_type,
        ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
      )

      lqas_analysis(result)
      force_gc()
      
    }, "LQAS Maps Analysis (Optimized)")
  })
  
  # ============================================================
  # KPI boxes
  # ============================================================
  output$lqas_total_districts <- renderValueBox({
    req(lqas_analysis())
    sm <- lqas_analysis()$summary_metrics
    total <- if (!is.null(sm$total_districts)) sm$total_districts else 0
    valueBox(format(total, big.mark = ","), "Total Districts", icon("map"), color = "aqua")
  })
  
  output$lqas_high_performance <- renderValueBox({
    req(lqas_analysis())
    sm <- lqas_analysis()$summary_metrics
    always_high <- if (!is.null(sm$always_high)) sm$always_high else 0
    valueBox(format(always_high, big.mark = ","), "Always High Performing", icon("check-circle"), color = "green")
  })
  
  output$lqas_low_performance <- renderValueBox({
    req(lqas_analysis())
    sm <- lqas_analysis()$summary_metrics
    never_high <- if (!is.null(sm$never_high)) sm$never_high else 0
    valueBox(format(never_high, big.mark = ","), "Never High Performing", icon("exclamation-triangle"), color = "red")
  })
  
  # ============================================================
  # Maps
  # ============================================================
  output$lqas_summary_map <- renderPlot({
    req(lqas_analysis())
    req(lqas_analysis()$map)
    lqas_analysis()$map
  })
  
  output$lqas_province_summary_map <- renderPlot({
    req(lqas_analysis())
    if (is.null(lqas_analysis()$province_map)) {
      plot.new()
      title("No province map available for the selected filters")
    } else {
      lqas_analysis()$province_map
    }
  })
  
  # ============================================================
  # Performance table
  # ============================================================
  output$lqas_summary_table_ui <- renderUI({
    req(lqas_analysis())
    
    td <- lqas_analysis()$table_data
    ft <- lqas_analysis()$table_flex
    
    if ((is.null(ft) || inherits(ft, "try-error")) && !is.null(td) && nrow(td) > 0) {
      if (!requireNamespace("flextable", quietly = TRUE)) {
        return(tags$div(
          class = "alert alert-warning",
          tags$h4("Flextable not available"),
          tags$p("Package 'flextable' is not installed/loaded, so the summary table cannot be rendered.")
        ))
      }
      ft <- flextable::flextable(td) |>
        flextable::theme_vanilla() |>
        flextable::autofit()
    }
    
    if (!is.null(td) && nrow(td) > 0) {
      ft_html <- flextable::htmltools_value(ft)
      div(class = "flextable-output", style = "overflow-x: auto; margin: 10px 0;", ft_html)
    } else {
      tags$div(
        class = "alert alert-warning",
        tags$h4("No data available"),
        tags$p("No LQAS summary data found for the selected filters.")
      )
    }
  })
  
  # ============================================================
  # Province summary
  # ============================================================
  lqas_province_summary_df <- reactive({
    req(lqas_analysis())
    dfp <- lqas_analysis()$province_summary_data
    
    if (is.null(dfp) || nrow(dfp) == 0) return(dfp)
    
    dfp |>
      dplyr::mutate(
        country  = trimws(as.character(.data$country)),
        province = trimws(as.character(.data$province))
      ) |>
      dplyr::filter(
        !is.na(.data$country), nzchar(.data$country),
        !is.na(.data$province), nzchar(.data$province)
      ) |>
      dplyr::arrange(.data$country, dplyr::desc(.data$`high (%)`), .data$province)
  })
  
  output$lqas_province_summary_ui <- renderUI({
    req(lqas_analysis())
    dfp <- lqas_province_summary_df()
    
    if (is.null(dfp) || nrow(dfp) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        tags$b("No province summary available under current filters.")
      ))
    }
    
    tryCatch(
      build_lqas_province_summary_ui(
        province_df = dfp,
        rows_per_panel = 24,
        max_cols_per_row = 2
      ),
      error = function(e) {
        tags$div(
          class = "alert alert-danger",
          tags$b("Province Summary failed to render."),
          tags$div(style = "margin-top:6px;", tags$code(conditionMessage(e)))
        )
      }
    )
  })
  
  # ============================================================
  # Data preview
  # ============================================================
  output$lqas_data_preview <- DT::renderDataTable({
    req(lqas_analysis())
    req(lqas_analysis()$filtered_data)
    
    DT::datatable(
      lqas_analysis()$filtered_data,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy','csv','excel')),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # ============================================================
  # PowerBI-like view
  # ============================================================
  lqas_pbi_df <- reactive({
    req(lqas_analysis())
    req(lqas_analysis()$filtered_data)
    
    dfv <- lqas_analysis()$filtered_data
    
    must <- c("country", "province", "district", "round_start_date", "performance", "High.perf.summary")
    for (nm in must) if (!nm %in% names(dfv)) dfv[[nm]] <- NA
    
    dfv <- dfv %>%
      dplyr::mutate(
        country  = trimws(as.character(.data$country)),
        district = trimws(as.character(.data$district)),
        province = as.character(.data$province)
      ) %>%
      dplyr::filter(!is.na(.data$country), nzchar(.data$country), !is.na(.data$district), nzchar(.data$district))
    
    mset <- lqas_analysis()$map_district_set
    if (!is.null(mset) && nrow(mset) > 0) {
      mkeys <- mset %>%
        dplyr::transmute(
          country  = trimws(as.character(.data$country)),
          district = trimws(as.character(.data$district))
        ) %>%
        dplyr::distinct()
      
      dfv <- dfv %>% dplyr::semi_join(mkeys, by = c("country", "district"))
    }
    
    selected <- input$lqas_perf_filter %||% character(0)
    keep_levels <- character(0)
    if ("high_performing" %in% selected) keep_levels <- c(keep_levels, "80-100%")
    if ("low_performing"  %in% selected) keep_levels <- c(keep_levels, "0-25%")
    
    if (length(keep_levels) > 0) {
      dfv <- dfv %>% dplyr::filter(.data$High.perf.summary %in% keep_levels)
    }
    
    dfv %>%
      dplyr::select(dplyr::all_of(must)) %>%
      dplyr::arrange(.data$country, .data$province, .data$district, .data$round_start_date)
  })
  
  output$lqas_powerbi_view_ui <- renderUI({
    req(lqas_analysis())
    dfv <- lqas_pbi_df()
    
    if (is.null(dfv) || nrow(dfv) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        tags$b("No rows to display under current filters."),
        tags$div("Tip: clear Performance Range filter or widen your time/country filters.")
      ))
    }
    
    tryCatch(
      build_lqas_filtered_powerbi_ui(
        filtered_df      = dfv,
        rows_per_panel   = 28,
        max_cols_per_row = 2
      ),
      error = function(e) {
        tags$div(
          class = "alert alert-danger",
          tags$b("PowerBI-like view failed to render."),
          tags$div(style = "margin-top:6px;", tags$code(conditionMessage(e)))
        )
      }
    )
  })
  
  # ============================================================
  # Helpers for downloads
  # ============================================================
  chunk_df <- function(x, n = 28) {
    if (is.null(x) || nrow(x) == 0) return(list())
    if (nrow(x) <= n) return(list(x))
    idx <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / n))
    lapply(idx, function(i) x[i, , drop = FALSE])
  }
  
  observeEvent(input$lqas_pbi_download_btn, {
    dfv <- lqas_pbi_df()
    
    ctrys <- dfv %>%
      dplyr::mutate(country = trimws(as.character(.data$country))) %>%
      dplyr::filter(!is.na(.data$country), nzchar(.data$country)) %>%
      dplyr::distinct(.data$country) %>%
      dplyr::arrange(.data$country) %>%
      dplyr::pull(.data$country)
    
    showModal(modalDialog(
      title = "Download PowerBI-like tables",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p("Choose what to download:"),
      downloadButton("lqas_pbi_download_all", "📦 Download ALL tables (ZIP of Excel by country)", class = "btn btn-primary"),
      tags$hr(),
      selectInput("lqas_pbi_country_pick", "Country:", choices = ctrys, selected = if (length(ctrys) > 0) ctrys[1] else NULL),
      downloadButton("lqas_pbi_download_one_imgs", "🖼️ Download selected country (ZIP of PNG tables for PPT)", class = "btn btn-success"),
      tags$hr(),
      tags$p(
        style = "color:#6b7280; font-size:12px; margin:0;",
        "PNG tables are generated per mini-table chunk (table_01.png, table_02.png, ...). ",
        "Each PNG is auto-sized to capture FULL content (no cropping)."
      )
    ))
  })
  
  output$lqas_pbi_download_all <- downloadHandler(
    filename = function() paste0("lqas_powerbi_tables_ALL_", Sys.Date(), ".zip"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required.")
      if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required.")
      
      dfv <- lqas_pbi_df()
      req(nrow(dfv) > 0)
      
      dfv <- dfv %>%
        dplyr::mutate(country = trimws(as.character(.data$country)),
                      country = dplyr::na_if(.data$country, "")) %>%
        dplyr::filter(!is.na(.data$country), nzchar(.data$country))
      
      td <- tempfile("lqas_pbi_all_")
      dir.create(td, showWarnings = FALSE)
      
      for (ctry in unique(dfv$country)) {
        ctry_clean <- trimws(as.character(ctry))
        sub <- dfv %>% dplyr::filter(.data$country == ctry_clean)
        if (nrow(sub) == 0) next
        
        parts <- chunk_df(sub, n = 28)
        wb <- openxlsx::createWorkbook()
        for (i in seq_along(parts)) {
          sh <- paste0("table_", i)
          openxlsx::addWorksheet(wb, sh)
          openxlsx::writeData(wb, sh, parts[[i]])
        }
        safe <- gsub("[^A-Za-z0-9_]+", "_", ctry_clean)
        out_xlsx <- file.path(td, paste0("lqas_", safe, "_", Sys.Date(), ".xlsx"))
        openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
      }
      
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      zip::zipr(file, list.files(td))
    }
  )
  
  output$lqas_pbi_download_one_imgs <- downloadHandler(
    filename = function() {
      ctry <- input$lqas_pbi_country_pick %||% "country"
      ctry <- gsub("[^A-Za-z0-9_]+", "_", ctry)
      paste0("lqas_powerbi_tables_", ctry, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      if (!requireNamespace("flextable", quietly = TRUE)) stop("Package 'flextable' is required.")
      if (!requireNamespace("webshot2", quietly = TRUE)) stop("Package 'webshot2' is required.")
      if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required.")
      
      dfv <- lqas_pbi_df()
      req(nrow(dfv) > 0)
      
      dfv <- dfv %>%
        dplyr::mutate(country = trimws(as.character(.data$country)),
                      country = dplyr::na_if(.data$country, "")) %>%
        dplyr::filter(!is.na(.data$country), nzchar(.data$country))
      
      ctry <- input$lqas_pbi_country_pick
      if (is.null(ctry) || !nzchar(ctry)) stop("Please select a country.")
      ctry_clean <- trimws(as.character(ctry))
      
      sub <- dfv %>% dplyr::filter(.data$country == ctry_clean)
      if (nrow(sub) == 0) stop("No rows for the selected country under current filters.")
      
      parts <- chunk_df(sub, n = 28)
      if (length(parts) == 0) stop("No tables to export.")
      
      td <- tempfile("lqas_pbi_one_imgs_")
      dir.create(td, showWarnings = FALSE)
      
      for (i in seq_along(parts)) {
        dat_i <- parts[[i]]
        
        ft <- flextable::flextable(dat_i) |>
          flextable::theme_vanilla() |>
          flextable::fontsize(size = 8, part = "all") |>
          flextable::bold(part = "header") |>
          flextable::bg(part = "header", bg = "#F3F4F6") |>
          flextable::align(align = "left", part = "all") |>
          flextable::set_table_properties(layout = "autofit") |>
          flextable::autofit()
        
        out_png <- file.path(td, sprintf("table_%02d.png", i))
        
        nr <- nrow(dat_i)
        nc <- ncol(dat_i)
        px_per_col <- 220
        px_per_row <- 45
        header_px  <- 140
        
        vwidth  <- min(3200, max(1400, nc * px_per_col + 200))
        vheight <- min(2400, max(700,  header_px + nr * px_per_row))
        
        flextable::save_as_image(ft, path = out_png, zoom = 2, vwidth = vwidth, vheight = vheight)
      }
      
      old <- setwd(td); on.exit(setwd(old), add = TRUE)
      zip::zipr(file, list.files(td, pattern = "\\.png$", full.names = FALSE))
    }
  )
  
  output$download_lqas_province_summary_image <- downloadHandler(
    filename = function() paste0("LQAS_Province_Summary_", Sys.Date(), ".png"),
    content = function(file) {
      req(lqas_analysis())
      req(lqas_analysis()$province_summary_flex)
      flextable::save_as_image(lqas_analysis()$province_summary_flex, path = file)
    }
  )
  
  output$download_lqas_province_summary_excel <- downloadHandler(
    filename = function() paste0("LQAS_Province_Summary_", Sys.Date(), ".xlsx"),
    content = function(file) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required.")
      
      req(lqas_analysis())
      dfp <- lqas_province_summary_df()
      req(!is.null(dfp), nrow(dfp) > 0)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Province Summary")
      openxlsx::writeData(wb, "Province Summary", dfp)
      
      if (!is.null(lqas_analysis()$map_province_set) && nrow(lqas_analysis()$map_province_set) > 0) {
        openxlsx::addWorksheet(wb, "Province Map Data")
        openxlsx::writeData(wb, "Province Map Data", lqas_analysis()$map_province_set)
      }
      
      filter_info <- data.frame(
        Parameter = c("Time Filter", "Block Type", "Selected Block", "Countries", "Provinces", "Districts",
                      "Performance Filters", "Data Period", "Province Summary Formula", "Province Labels"),
        Value = c(
          input$lqas_time_type,
          input$lqas_block_type,
          input$lqas_block,
          if (!is.null(input$lqas_countries) && length(input$lqas_countries) > 0) paste(input$lqas_countries, collapse = ", ") else "All",
          if (!is.null(input$lqas_provinces) && length(input$lqas_provinces) > 0) paste(input$lqas_provinces, collapse = ", ") else "All",
          if (!is.null(input$lqas_districts) && length(input$lqas_districts) > 0) paste(input$lqas_districts, collapse = ", ") else "All",
          if (length(input$lqas_perf_filter %||% character(0)) > 0) paste(input$lqas_perf_filter, collapse = ", ") else "All",
          lqas_analysis()$period_info,
          "high (%) = count(performance == 'high') / total count(performance records) * 100",
          ifelse(isTRUE(input$lqas_show_province_labels), "Shown", "Hidden")
        )
      )
      
      openxlsx::addWorksheet(wb, "Filter Information")
      openxlsx::writeData(wb, "Filter Information", filter_info)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_lqas_province_map <- downloadHandler(
    filename = function() paste0("LQAS_Province_Summary_Map_", Sys.Date(), ".png"),
    content = function(file) {
      req(lqas_analysis())
      if (is.null(lqas_analysis()$province_map)) stop("No province map available for the selected filters.")
      ggplot2::ggsave(file, plot = lqas_analysis()$province_map, width = 16, height = 12, dpi = 300)
    }
  )
  
  output$download_lqas_map <- downloadHandler(
    filename = function() paste0("LQAS_District_Summary_Map_", Sys.Date(), ".png"),
    content = function(file) {
      withProgress(message = "Generating LQAS district map...", detail = "Rendering high-quality image...", value = 0.3, {
        req(lqas_analysis())
        incProgress(0.6, detail = "Saving image...")
        ggplot2::ggsave(file, plot = lqas_analysis()$map, width = 16, height = 12, dpi = 300)
        incProgress(1, detail = "Download ready!")
      })
    }
  )
  
  output$download_lqas_table <- downloadHandler(
    filename = function() paste0("LQAS_Performance_Table_", Sys.Date(), ".png"),
    content = function(file) {
      withProgress(message = "Generating LQAS performance table...", detail = "Rendering high-quality image...", value = 0.3, {
        req(lqas_analysis())
        incProgress(0.6, detail = "Saving image...")
        flextable::save_as_image(lqas_analysis()$table_flex, path = file)
        incProgress(1, detail = "Download ready!")
      })
    }
  )
  
  output$download_lqas_data <- downloadHandler(
    filename = function() paste0("lqas_filtered_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(lqas_analysis())
      req(lqas_analysis()$filtered_data)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Filtered Data")
      openxlsx::writeData(wb, "Filtered Data", lqas_analysis()$filtered_data)
      
      openxlsx::addWorksheet(wb, "Performance Summary (FULL)")
      openxlsx::writeData(wb, "Performance Summary (FULL)", lqas_analysis()$performance_data)
      
      openxlsx::addWorksheet(wb, "Summary Table (KPIs)")
      openxlsx::writeData(wb, "Summary Table (KPIs)", lqas_analysis()$table_data)
      
      if (!is.null(lqas_analysis()$province_summary_data) && nrow(lqas_analysis()$province_summary_data) > 0) {
        openxlsx::addWorksheet(wb, "Province Summary")
        openxlsx::writeData(wb, "Province Summary", lqas_analysis()$province_summary_data)
      }
      
      if (!is.null(lqas_analysis()$map_district_set) && nrow(lqas_analysis()$map_district_set) > 0) {
        openxlsx::addWorksheet(wb, "Displayed (District Map)")
        openxlsx::writeData(wb, "Displayed (District Map)", lqas_analysis()$map_district_set)
      }
      
      if (!is.null(lqas_analysis()$map_province_set) && nrow(lqas_analysis()$map_province_set) > 0) {
        openxlsx::addWorksheet(wb, "Displayed (Province Map)")
        openxlsx::writeData(wb, "Displayed (Province Map)", lqas_analysis()$map_province_set)
      }
      
      pbi_now <- tryCatch(lqas_pbi_df(), error = function(e) NULL)
      if (!is.null(pbi_now) && nrow(pbi_now) > 0) {
        openxlsx::addWorksheet(wb, "Displayed (PowerBI)")
        openxlsx::writeData(wb, "Displayed (PowerBI)", pbi_now)
      }
      
      prov_now <- tryCatch(lqas_province_summary_df(), error = function(e) NULL)
      if (!is.null(prov_now) && nrow(prov_now) > 0) {
        openxlsx::addWorksheet(wb, "Displayed (Province Summary)")
        openxlsx::writeData(wb, "Displayed (Province Summary)", prov_now)
      }
      
      filter_info <- data.frame(
        Parameter = c("Time Filter", "Block Type", "Selected Block", "Countries", "Provinces", "Districts",
                      "Performance Filters (LIVE)", "Data Period", "Province Labels"),
        Value = c(
          input$lqas_time_type,
          input$lqas_block_type,
          input$lqas_block,
          if (!is.null(input$lqas_countries)) paste(input$lqas_countries, collapse = ", ") else "All",
          if (!is.null(input$lqas_provinces)) paste(input$lqas_provinces, collapse = ", ") else "All",
          if (!is.null(input$lqas_districts)) paste(input$lqas_districts, collapse = ", ") else "All",
          if (length(input$lqas_perf_filter %||% character(0)) > 0) paste(input$lqas_perf_filter, collapse = ", ") else "All",
          lqas_analysis()$period_info,
          ifelse(isTRUE(input$lqas_show_province_labels), "Shown", "Hidden")
        )
      )
      openxlsx::addWorksheet(wb, "Filter Information")
      openxlsx::writeData(wb, "Filter Information", filter_info)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_lqas_data_preview <- downloadHandler(
    filename = function() paste0("lqas_data_preview_", Sys.Date(), ".csv"),
    content = function(file) {
      req(lqas_analysis())
      req(lqas_analysis()$filtered_data)
      write.csv(lqas_analysis()$filtered_data, file, row.names = FALSE)
    }
  )
  
  
  # ============================================================
  # COMPREHENSIVE REASONS ANALYSIS SERVER - COMPLETE WITH MERGED FOOTNOTES
  # ============================================================
  
  # ============================================================
  # HELPER FUNCTIONS FOR DRILLDOWN ANALYSIS
  # ============================================================
  
  analyze_district_trends <- function(data, districts_list, reason_col, 
                                      time_unit = "month") {
    # Filter data for selected districts
    trend_data <- data %>%
      filter(district %in% districts_list) %>%
      mutate(
        year_month = format(date, "%Y-%m"),
        year_quarter = paste0(year(date), "-Q", quarter(date)),
        year = year(date)
      )
    
    # Aggregate by time unit
    time_col <- switch(time_unit,
                       "month" = "year_month",
                       "quarter" = "year_quarter",
                       "year" = "year")
    
    trend_summary <- trend_data %>%
      group_by(district, !!sym(time_col)) %>%
      summarise(
        total_absent = sum(!!sym(reason_col), na.rm = TRUE),
        total_visits = n(),
        absent_rate = (total_absent / total_visits) * 100,
        .groups = "drop"
      ) %>%
      arrange(district, !!sym(time_col))
    
    # Calculate trend metrics
    trend_metrics <- trend_summary %>%
      group_by(district) %>%
      summarise(
        avg_rate = mean(absent_rate, na.rm = TRUE),
        min_rate = min(absent_rate, na.rm = TRUE),
        max_rate = max(absent_rate, na.rm = TRUE),
        trend_direction = ifelse(
          cor(as.numeric(factor(!!sym(time_col))), absent_rate, use = "complete.obs") > 0,
          "Increasing", "Decreasing"
        ),
        volatility = sd(absent_rate, na.rm = TRUE),
        .groups = "drop"
      )
    
    list(
      trend_data = trend_summary,
      trend_metrics = trend_metrics,
      time_unit = time_unit
    )
  }
  
  create_block_summary <- function(drilldown_results, reason, block_name, block_type) {
    if (is.null(drilldown_results) || is.null(drilldown_results[[reason]])) {
      return(NULL)
    }
    
    prov_data <- drilldown_results[[reason]]$provinces
    dist_data <- drilldown_results[[reason]]$districts
    
    # Create comprehensive block summary
    block_summary <- data.frame(
      Metric = c(
        "Block Name",
        "Block Type",
        "Total Countries in Block",
        "Countries Meeting Threshold",
        "Total Provinces Identified",
        "Total Districts Identified",
        "Top Contributing Province",
        "Top Province Contribution (%)",
        "Top Contributing District", 
        "Top District Contribution (%)",
        "Average Province Contribution (%)",
        "Average District Contribution (%)",
        "Highest Country",
        "Most Volatile District"
      ),
      Value = c(
        block_name,
        if(block_type == "afro") "AFRO Blocks" else "IST Blocks",
        if(!is.null(drilldown_results[[reason]]$priority_countries)) 
          length(unique(c(prov_data$country, dist_data$country))) else 0,
        n_distinct(prov_data$country),
        nrow(prov_data),
        nrow(dist_data),
        if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A",
        if(nrow(prov_data) > 0) paste0(round(max(prov_data$prov_pct), 1), "%") else "N/A",
        if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A",
        if(nrow(dist_data) > 0) paste0(round(max(dist_data$dist_pct), 1), "%") else "N/A",
        if(nrow(prov_data) > 0) paste0(round(mean(prov_data$prov_pct, na.rm = TRUE), 1), "%") else "N/A",
        if(nrow(dist_data) > 0) paste0(round(mean(dist_data$dist_pct, na.rm = TRUE), 1), "%") else "N/A",
        if(nrow(prov_data) > 0) prov_data$country[which.max(prov_data$prov_pct)] else "N/A",
        if(!is.null(drilldown_results[[reason]]$trend_analysis)) {
          ta <- drilldown_results[[reason]]$trend_analysis
          if(nrow(ta$trend_metrics) > 0) {
            ta$trend_metrics$district[which.max(ta$trend_metrics$volatility)]
          } else "N/A"
        } else "N/A"
      )
    )
    
    return(block_summary)
  }
  
  generate_drilldown_footnote <- function(drilldown_results, reason, 
                                          priority_threshold, province_threshold,
                                          district_threshold, dynamic_threshold,
                                          period_label, block_name) {
    
    if (is.null(drilldown_results) || is.null(drilldown_results[[reason]])) {
      return("No drill-down data available")
    }
    
    prov_data <- drilldown_results[[reason]]$provinces
    dist_data <- drilldown_results[[reason]]$districts
    
    reason_name <- if(reason == "childabsent") "Child Absence" else "Non-Compliance"
    
    # Get top contributors
    top_province <- if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A"
    top_province_pct <- if(nrow(prov_data) > 0) round(max(prov_data$prov_pct), 1) else 0
    top_district <- if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A"
    top_district_pct <- if(nrow(dist_data) > 0) round(max(dist_data$dist_pct), 1) else 0
    
    # Calculate averages
    avg_prov_pct <- if(nrow(prov_data) > 0) round(mean(prov_data$prov_pct, na.rm = TRUE), 1) else 0
    avg_dist_pct <- if(nrow(dist_data) > 0) round(mean(dist_data$dist_pct, na.rm = TRUE), 1) else 0
    
    # Create 3 key summary sentences
    footnote <- paste0(
      "1. ", reason_name, " Analysis: ", 
      n_distinct(prov_data$country), " countries in ", block_name, " block exceed ", 
      priority_threshold, "% threshold, with ", nrow(prov_data), " provinces and ", 
      nrow(dist_data), " districts identified.\n\n",
      
      "2. Top Contributors: ", top_province, " leads provinces (", top_province_pct, 
      "% of country total), while ", top_district, " leads districts (", top_district_pct, 
      "% of province total). Average province contribution: ", avg_prov_pct, 
      "%, average district contribution: ", avg_dist_pct, "%.\n\n",
      
      "3. Methodology: Applied ", if(dynamic_threshold) "Pareto (80% cumulative)" else "fixed", 
      " thresholds (Country ≥", priority_threshold, "%, Province ≥", province_threshold, 
      "%, District ≥", district_threshold, "%) for period: ", period_label, "."
    )
    
    return(footnote)
  }
  
  prepare_sankey <- function(prov_data, dist_data, reason) {
    # Create nodes: all unique countries, provinces, districts
    countries <- unique(prov_data$country)
    provinces <- unique(prov_data$province)
    districts <- unique(dist_data$district)
    
    nodes <- data.frame(
      name = c(countries, provinces, districts),
      group = c(rep("country", length(countries)),
                rep("province", length(provinces)),
                rep("district", length(districts))),
      stringsAsFactors = FALSE
    )
    nodes$id <- 0:(nrow(nodes)-1)
    
    # Links: country -> province using percentage
    links1 <- prov_data %>%
      left_join(nodes %>% select(id, name), by = c("country" = "name"), relationship = "many-to-many") %>%
      rename(source = id) %>%
      left_join(nodes %>% select(id, name), by = c("province" = "name"), relationship = "many-to-many") %>%
      rename(target = id) %>%
      mutate(value = prov_pct) %>%
      select(source, target, value)
    
    # Links: province -> district using percentage
    links2 <- dist_data %>%
      left_join(nodes %>% select(id, name), by = c("province" = "name"), relationship = "many-to-many") %>%
      rename(source = id) %>%
      left_join(nodes %>% select(id, name), by = c("district" = "name"), relationship = "many-to-many") %>%
      rename(target = id) %>%
      mutate(value = dist_pct) %>%
      select(source, target, value)
    
    links <- bind_rows(links1, links2)
    
    list(nodes = nodes, links = links, reason = reason)
  }
  
  # ============================================================
  # REASONS ANALYSIS SERVER CODE
  # ============================================================
  
  # Reactive block choices
  reasons_block_choices <- reactive({
    if (input$reasons_block_type == "afro") afro_blocks else ist_blocks
  })
  
  reasons_block_names <- reactive({
    if (input$reasons_block_type == "afro") names(afro_blocks) else names(ist_blocks)
  })
  
  observe({
    updateSelectInput(session, "reasons_block",
                      choices = c("All", reasons_block_names()))
  })
  
  # Country selector UI
  output$reasons_countries_ui <- renderUI({
    req(dat)
    available_countries <- if (!is.null(input$reasons_block) && input$reasons_block != "All") {
      reasons_block_choices()[[input$reasons_block]]
    } else {
      sort(unique(dat$country))
    }
    selectizeInput("reasons_countries", "Countries (Optional):",
                   choices = c("All" = "", available_countries), multiple = TRUE,
                   options = list(placeholder = 'Select one or more countries', maxItems = 15,
                                  plugins = list('remove_button'), dropdownParent = 'body'))
  })
  
  # Province selector UI
  output$reasons_provinces_ui <- renderUI({
    req(dat, input$reasons_countries)
    filtered_dat <- dat
    if (!is.null(input$reasons_countries) && length(input$reasons_countries) > 0) {
      filtered_dat <- filtered_dat %>% filter(country %in% input$reasons_countries)
    }
    available_provinces <- sort(unique(filtered_dat$province[!is.na(filtered_dat$province)]))
    selectizeInput("reasons_provinces", "Provinces (Optional):",
                   choices = c("All" = "", available_provinces), multiple = TRUE,
                   options = list(placeholder = 'Select one or more provinces', maxItems = 20,
                                  plugins = list('remove_button'), dropdownParent = 'body'))
  })
  
  # District selector UI
  output$reasons_districts_ui <- renderUI({
    req(dat, input$reasons_countries, input$reasons_provinces)
    filtered_dat <- dat
    if (!is.null(input$reasons_countries) && length(input$reasons_countries) > 0) {
      filtered_dat <- filtered_dat %>% filter(country %in% input$reasons_countries)
    }
    if (!is.null(input$reasons_provinces) && length(input$reasons_provinces) > 0) {
      filtered_dat <- filtered_dat %>% filter(province %in% input$reasons_provinces)
    }
    available_districts <- sort(unique(filtered_dat$district[!is.na(filtered_dat$district)]))
    selectizeInput("reasons_districts", "Districts (Optional):",
                   choices = c("All" = "", available_districts), multiple = TRUE,
                   options = list(placeholder = 'Select one or more districts', maxItems = 30,
                                  plugins = list('remove_button'), dropdownParent = 'body'))
  })
  
  # Indicators
  output$reasons_geo_indicator <- renderUI({
    geo_level <- input$reasons_geo_level
    geo_text <- case_when(
      geo_level == "country" ~ "🌍 Analyzing at COUNTRY level",
      geo_level == "province" ~ "🏛️ Analyzing at PROVINCE level",
      geo_level == "district" ~ "📍 Analyzing at DISTRICT level"
    )
    div(icon("info-circle"), strong(geo_text), style = "color: #2c3e50; font-size: 14px;")
  })
  
  output$reasons_viz_indicator <- renderUI({
    viz_type <- input$reasons_viz_type
    viz_text <- case_when(
      viz_type == "heatmap" ~ "🔥 Heatmap visualization selected",
      viz_type == "bar" ~ "📊 Grouped Bar Chart selected",
      viz_type == "stacked_bar" ~ "📊 Stacked Bar Chart selected",
      viz_type == "faceted_bar" ~ "🔲 Faceted Bar Chart selected",
      viz_type == "bubble" ~ "⚪ Bubble Chart selected",
      viz_type == "pie" ~ "🥧 Pie Chart selected",
      viz_type == "donut" ~ "🍩 Donut Chart selected",
      viz_type == "treemap" ~ "🌳 Treemap selected",
      viz_type == "sunburst" ~ "☀️ Sunburst Chart selected",
      viz_type == "radar" ~ "📡 Radar Chart selected",
      viz_type == "lollipop" ~ "🍭 Lollipop Chart selected",
      viz_type == "waterfall" ~ "💧 Waterfall Chart selected",
      viz_type == "auto" ~ "🤖 Auto mode - will choose best visualization",
      TRUE ~ "Visualization ready"
    )
    div(icon("chart-bar"), strong(viz_text), style = "color: #2c3e50; font-size: 14px; margin-top: 5px;")
  })
  
  # Reactive value to store analysis results
  reasons_analysis <- reactiveVal(NULL)
  
  # Block Summary Title - FIXED
  output$block_summary_title <- renderText({
    req(input$reasons_block_type)
    block_type <- if(input$reasons_block_type == "afro") "AFRO" else "IST"
    paste("Block Summary:", block_type, "Blocks")
  })
  
  # Main analysis execution
  observeEvent(input$reasons_analyze, {
    if (input$tabs != "reasons") return()
    
    safe_analysis({
      x_months <- if (input$reasons_time_type == "x_months") input$reasons_months else NULL
      specific_month <- if (input$reasons_time_type == "specific_month") as.numeric(input$reasons_month) else NULL
      specific_year <- if (input$reasons_time_type == "specific_month") input$reasons_year else NULL
      
      viz_type <- input$reasons_viz_type
      
      combined_types <- switch(input$reasons_combined_choice,
                               "all" = c("traditional", "non_compliance", "absence"),
                               "trad_nc" = c("traditional", "non_compliance"),
                               "trad_absence" = c("traditional", "absence"),
                               c("traditional", "non_compliance", "absence"))
      
      result <- reasons_heatmap_analysis(
        data = dat,
        x_months = x_months,
        specific_month = specific_month,
        specific_year = specific_year,
        block_selection = if (input$reasons_block != "All") input$reasons_block else NULL,
        country_selection = if (!is.null(input$reasons_countries) && length(input$reasons_countries) > 0) input$reasons_countries else NULL,
        province_selection = if (!is.null(input$reasons_provinces) && length(input$reasons_provinces) > 0) input$reasons_provinces else NULL,
        district_selection = if (!is.null(input$reasons_districts) && length(input$reasons_districts) > 0) input$reasons_districts else NULL,
        afro_blocks = afro_blocks,
        ist_blocks = ist_blocks,
        block_type = input$reasons_block_type,
        geo_level = input$reasons_geo_level,
        viz_type = viz_type,
        combined_types = combined_types,
        
        # Drill‑down parameters
        priority_enabled = input$drilldown_enable,
        priority_reason = input$drilldown_reason,
        priority_threshold = input$drilldown_priority_threshold,
        province_threshold = input$drilldown_province_threshold,
        district_threshold = input$drilldown_district_threshold,
        dynamic_threshold = input$drilldown_dynamic
      )
      
      reasons_analysis(result)
    }, "Comprehensive Reasons Analysis")
  })
  
  # ============================================================
  # COMBINED OVERVIEW OUTPUTS
  # ============================================================
  output$reasons_total_units_all <- renderValueBox({
    req(reasons_analysis())
    units <- if (!is.null(reasons_analysis()$data$combined_long)) 
      n_distinct(reasons_analysis()$data$combined_long$geo_unit) else 0
    valueBox(units, paste("Total", reasons_analysis()$geo_display, "s Analyzed"), 
             icon = icon("map-marked-alt"), color = "aqua")
  })
  
  output$reasons_total_categories_all <- renderValueBox({
    req(reasons_analysis())
    total <- if (!is.null(reasons_analysis()$data$combined_long))
      n_distinct(reasons_analysis()$data$combined_long$reasons) else 0
    valueBox(total, "Total Reason Categories (selected types)", 
             icon = icon("list"), color = "purple")
  })
  
  output$reasons_avg_percentage_all <- renderValueBox({
    req(reasons_analysis())
    avg_pct <- if (!is.null(reasons_analysis()$data$combined_long) && 
                   nrow(reasons_analysis()$data$combined_long) > 0) {
      round(mean(reasons_analysis()$data$combined_long$value, na.rm = TRUE), 1)
    } else 0
    valueBox(paste0(avg_pct, "%"), 
             paste("Average Percentage -", reasons_analysis()$metrics$period_label), 
             icon = icon("percentage"), color = "light-blue")
  })
  
  output$reasons_viz_recommendation <- renderValueBox({
    req(reasons_analysis())
    units <- reasons_analysis()$unique_geo_units
    viz_type <- reasons_analysis()$viz_type
    recommendation <- if (viz_type == "auto") {
      if (units <= 8) "Pie/Donut recommended"
      else if (units <= 20) "Heatmap recommended"
      else "Bar chart recommended"
    } else {
      case_when(
        viz_type == "heatmap" ~ "Heatmap active",
        viz_type == "bar" ~ "Bar chart active",
        viz_type == "stacked_bar" ~ "Stacked bar active",
        viz_type == "faceted_bar" ~ "Faceted bar active",
        viz_type == "bubble" ~ "Bubble chart active",
        viz_type == "pie" ~ "Pie chart active",
        viz_type == "donut" ~ "Donut chart active",
        viz_type == "treemap" ~ "Treemap active",
        viz_type == "sunburst" ~ "Sunburst active",
        viz_type == "radar" ~ "Radar active",
        viz_type == "lollipop" ~ "Lollipop active",
        viz_type == "waterfall" ~ "Waterfall active",
        TRUE ~ "Custom view"
      )
    }
    valueBox(paste(units, ifelse(units == 1, "unit", "units")), recommendation, 
             icon = icon("eye"), color = "teal")
  })
  
  output$reasons_combined_heatmap <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$combined_plot)) {
      reasons_analysis()$combined_plot
    } else {
      ggplot() + theme_void() + labs(title = "No combined data available") +
        theme(plot.title = element_text(size = 16, hjust = 0.5, color = "gray50"))
    }
  })
  
  # ============================================================
  # ABSENCE REASONS OUTPUTS
  # ============================================================
  output$absence_total_units <- renderValueBox({
    req(reasons_analysis())
    units <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_units else 0
    valueBox(units, paste(reasons_analysis()$geo_display, "s with Absence Data"), icon = icon("flag"), color = "aqua")
  })
  
  output$absence_top_reason <- renderValueBox({
    req(reasons_analysis())
    top <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$most_common else "No data"
    if (length(top) > 1) top <- top[1]
    valueBox(top, "Most Common Absence Reason", icon = icon("chart-bar"), color = "green")
  })
  
  output$absence_avg_pct <- renderValueBox({
    req(reasons_analysis())
    avg <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$avg_percentage else 0
    valueBox(paste0(avg, "%"), "Average Percentage", icon = icon("percentage"), color = "light-blue")
  })
  
  output$absence_total_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_categories else 0
    valueBox(cats, "Absence Reason Categories", icon = icon("list"), color = "purple")
  })
  
  output$absence_viz_type <- renderValueBox({
    req(reasons_analysis())
    viz <- if (!is.null(reasons_analysis()$absence$viz_type)) reasons_analysis()$absence$viz_type else "N/A"
    viz_display <- case_when(
      viz == "heatmap" ~ "Heatmap", viz == "bar" ~ "Bar Chart", viz == "stacked_bar" ~ "Stacked Bar",
      viz == "faceted_bar" ~ "Faceted Bar", viz == "bubble" ~ "Bubble Chart", viz == "pie" ~ "Pie Chart",
      viz == "donut" ~ "Donut Chart", viz == "treemap" ~ "Treemap", viz == "sunburst" ~ "Sunburst",
      viz == "radar" ~ "Radar", viz == "lollipop" ~ "Lollipop", viz == "waterfall" ~ "Waterfall",
      TRUE ~ "Standard"
    )
    valueBox(viz_display, "Visualization Type", icon = icon("chart-pie"), color = "light-blue")
  })
  
  output$absence_heatmap <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$absence$plot)) {
      reasons_analysis()$absence$plot
    } else {
      ggplot() + theme_void() + labs(title = "No absence reasons data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  output$absence_table_pct <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$absence$data_pct
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Absence Reasons - Percentage within", reasons_analysis()$geo_display)) %>%
        formatRound(columns = 2:ncol(data), digits = 1)
    } else {
      datatable(tibble(Message = "No absence reasons data available"))
    }
  })
  
  output$absence_table_raw <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$absence$data_raw
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Absence Reasons - Raw Counts by", reasons_analysis()$geo_display))
    } else {
      datatable(tibble(Message = "No absence reasons data available"))
    }
  })
  
  output$absence_top5_plot <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$absence$metrics$top_5) && length(reasons_analysis()$absence$metrics$top_5) > 0) {
      top5 <- reasons_analysis()$absence$metrics$top_5
      data <- reasons_analysis()$absence$long_data %>%
        filter(reasons %in% top5) %>%
        group_by(reasons) %>%
        summarise(avg_percentage = mean(value, na.rm = TRUE))
      ggplot(data, aes(x = reorder(reasons, avg_percentage), y = avg_percentage)) +
        geom_segment(aes(xend = reasons, yend = 0), color = "skyblue", size = 1.5) +
        geom_point(aes(color = reasons), size = 5, show.legend = FALSE) +
        coord_flip() + scale_color_brewer(palette = "Set3") +
        labs(title = "Top 5 Absence Reasons (Average %)", x = NULL, y = "Average Percentage (%)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              panel.grid.major.y = element_blank())
    } else {
      ggplot() + theme_void() + labs(title = "No top 5 data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  # ============================================================
  # NON-COMPLIANCE REASONS OUTPUTS
  # ============================================================
  output$nc_total_units <- renderValueBox({
    req(reasons_analysis())
    units <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_units else 0
    valueBox(units, paste(reasons_analysis()$geo_display, "s with NC Data"), icon = icon("flag"), color = "aqua")
  })
  
  output$nc_top_reason <- renderValueBox({
    req(reasons_analysis())
    top <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$most_common else "No data"
    if (length(top) > 1) top <- top[1]
    valueBox(top, "Most Common NC Reason", icon = icon("chart-bar"), color = "orange")
  })
  
  output$nc_avg_pct <- renderValueBox({
    req(reasons_analysis())
    avg <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$avg_percentage else 0
    valueBox(paste0(avg, "%"), "Average Percentage", icon = icon("percentage"), color = "light-blue")
  })
  
  output$nc_total_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_categories else 0
    valueBox(cats, "NC Reason Categories", icon = icon("list"), color = "maroon")
  })
  
  output$nc_viz_type <- renderValueBox({
    req(reasons_analysis())
    viz <- if (!is.null(reasons_analysis()$non_compliance$viz_type)) reasons_analysis()$non_compliance$viz_type else "N/A"
    viz_display <- case_when(
      viz == "heatmap" ~ "Heatmap", viz == "bar" ~ "Bar Chart", viz == "stacked_bar" ~ "Stacked Bar",
      viz == "faceted_bar" ~ "Faceted Bar", viz == "bubble" ~ "Bubble Chart", viz == "pie" ~ "Pie Chart",
      viz == "donut" ~ "Donut Chart", viz == "treemap" ~ "Treemap", viz == "sunburst" ~ "Sunburst",
      viz == "radar" ~ "Radar", viz == "lollipop" ~ "Lollipop", viz == "waterfall" ~ "Waterfall",
      TRUE ~ "Standard"
    )
    valueBox(viz_display, "Visualization Type", icon = icon("chart-pie"), color = "orange")
  })
  
  output$nc_heatmap <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$non_compliance$plot)) {
      reasons_analysis()$non_compliance$plot
    } else {
      ggplot() + theme_void() + labs(title = "No non-compliance data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  output$nc_table_pct <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$non_compliance$data_pct
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Non-Compliance Reasons - Percentage within", reasons_analysis()$geo_display)) %>%
        formatRound(columns = 2:ncol(data), digits = 1)
    } else {
      datatable(tibble(Message = "No non-compliance reasons data available"))
    }
  })
  
  output$nc_table_raw <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$non_compliance$data_raw
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Non-Compliance Reasons - Raw Counts by", reasons_analysis()$geo_display))
    } else {
      datatable(tibble(Message = "No non-compliance reasons data available"))
    }
  })
  
  output$nc_top5_plot <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$non_compliance$metrics$top_5) && length(reasons_analysis()$non_compliance$metrics$top_5) > 0) {
      top5 <- reasons_analysis()$non_compliance$metrics$top_5
      data <- reasons_analysis()$non_compliance$long_data %>%
        filter(reasons %in% top5) %>%
        group_by(reasons) %>%
        summarise(avg_percentage = mean(value, na.rm = TRUE))
      ggplot(data, aes(x = reorder(reasons, avg_percentage), y = avg_percentage)) +
        geom_segment(aes(xend = reasons, yend = 0), color = "orange", size = 1.5) +
        geom_point(aes(color = reasons), size = 5, show.legend = FALSE) +
        coord_flip() + scale_color_brewer(palette = "Set3") +
        labs(title = "Top 5 Non-Compliance Reasons (Average %)", x = NULL, y = "Average Percentage (%)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              panel.grid.major.y = element_blank())
    } else {
      ggplot() + theme_void() + labs(title = "No top 5 data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  # ============================================================
  # TRADITIONAL REASONS OUTPUTS
  # ============================================================
  output$trad_total_units <- renderValueBox({
    req(reasons_analysis())
    units <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_units else 0
    valueBox(units, paste(reasons_analysis()$geo_display, "s with Traditional Data"), icon = icon("flag"), color = "aqua")
  })
  
  output$trad_top_reason <- renderValueBox({
    req(reasons_analysis())
    top <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$most_common else "No data"
    if (length(top) > 1) top <- top[1]
    valueBox(top, "Most Common Traditional Reason", icon = icon("chart-bar"), color = "teal")
  })
  
  output$trad_avg_pct <- renderValueBox({
    req(reasons_analysis())
    avg <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$avg_percentage else 0
    valueBox(paste0(avg, "%"), "Average Percentage", icon = icon("percentage"), color = "light-blue")
  })
  
  output$trad_total_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_categories else 0
    valueBox(cats, "Traditional Reason Categories", icon = icon("list"), color = "navy")
  })
  
  output$trad_viz_type <- renderValueBox({
    req(reasons_analysis())
    viz <- if (!is.null(reasons_analysis()$traditional$viz_type)) reasons_analysis()$traditional$viz_type else "N/A"
    viz_display <- case_when(
      viz == "heatmap" ~ "Heatmap", viz == "bar" ~ "Bar Chart", viz == "stacked_bar" ~ "Stacked Bar",
      viz == "faceted_bar" ~ "Faceted Bar", viz == "bubble" ~ "Bubble Chart", viz == "pie" ~ "Pie Chart",
      viz == "donut" ~ "Donut Chart", viz == "treemap" ~ "Treemap", viz == "sunburst" ~ "Sunburst",
      viz == "radar" ~ "Radar", viz == "lollipop" ~ "Lollipop", viz == "waterfall" ~ "Waterfall",
      TRUE ~ "Standard"
    )
    valueBox(viz_display, "Visualization Type", icon = icon("chart-pie"), color = "teal")
  })
  
  output$traditional_heatmap <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$traditional$plot)) {
      reasons_analysis()$traditional$plot
    } else {
      ggplot() + theme_void() + labs(title = "No traditional reasons data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  output$traditional_table_pct <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$traditional$data_pct
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Traditional Reasons - Percentage within", reasons_analysis()$geo_display)) %>%
        formatRound(columns = 2:ncol(data), digits = 1)
    } else {
      datatable(tibble(Message = "No traditional reasons data available"))
    }
  })
  
  output$traditional_table_raw <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$traditional$data_raw
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 10),
                caption = paste("Traditional Reasons - Raw Counts by", reasons_analysis()$geo_display))
    } else {
      datatable(tibble(Message = "No traditional reasons data available"))
    }
  })
  
  output$traditional_top5_plot <- renderPlot({
    req(reasons_analysis())
    if (!is.null(reasons_analysis()$traditional$metrics$top_5) && length(reasons_analysis()$traditional$metrics$top_5) > 0) {
      top5 <- reasons_analysis()$traditional$metrics$top_5
      data <- reasons_analysis()$traditional$long_data %>%
        filter(reasons %in% top5) %>%
        group_by(reasons) %>%
        summarise(avg_percentage = mean(value, na.rm = TRUE))
      ggplot(data, aes(x = reorder(reasons, avg_percentage), y = avg_percentage)) +
        geom_segment(aes(xend = reasons, yend = 0), color = "skyblue", size = 1.5) +
        geom_point(aes(color = reasons), size = 5, show.legend = FALSE) +
        coord_flip() + scale_color_brewer(palette = "Set3") +
        labs(title = "Top 5 Traditional Reasons (Average %)", x = NULL, y = "Average Percentage (%)") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              panel.grid.major.y = element_blank())
    } else {
      ggplot() + theme_void() + labs(title = "No top 5 data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  # ============================================================
  # HIERARCHICAL VIEW OUTPUTS
  # ============================================================
  output$hierarchical_absence_table <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$hierarchical$absence
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 15),
                caption = "Absence Reasons - Hierarchical View (Country > Province > District)")
    } else {
      datatable(tibble(Message = "No hierarchical data available for absence reasons"))
    }
  })
  
  output$hierarchical_nc_table <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$hierarchical$non_compliance
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 15),
                caption = "Non-Compliance Reasons - Hierarchical View (Country > Province > District)")
    } else {
      datatable(tibble(Message = "No hierarchical data available for non-compliance reasons"))
    }
  })
  
  output$hierarchical_traditional_table <- renderDT({
    req(reasons_analysis())
    data <- reasons_analysis()$hierarchical$traditional
    if (!is.null(data) && nrow(data) > 0) {
      datatable(data, options = list(scrollX = TRUE, pageLength = 15),
                caption = "Traditional Reasons - Hierarchical View (Country > Province > District)")
    } else {
      datatable(tibble(Message = "No hierarchical data available for traditional reasons"))
    }
  })
  
  # ============================================================
  # COMPARATIVE SUMMARY OUTPUTS
  # ============================================================
  output$summary_absence_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_categories else 0
    valueBox(cats, "Absence Categories", icon = icon("user-clock"), color = "green")
  })
  
  output$summary_nc_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_categories else 0
    valueBox(cats, "NC Categories", icon = icon("gavel"), color = "orange")
  })
  
  output$summary_trad_cats <- renderValueBox({
    req(reasons_analysis())
    cats <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_categories else 0
    valueBox(cats, "Traditional Categories", icon = icon("history"), color = "teal")
  })
  
  output$summary_total_cats <- renderValueBox({
    req(reasons_analysis())
    abs_cats <- if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_categories else 0
    nc_cats <- if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_categories else 0
    trad_cats <- if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_categories else 0
    total <- abs_cats + nc_cats + trad_cats
    valueBox(total, "Total Categories", icon = icon("list"), color = "purple")
  })
  
  output$comparative_summary_table <- renderDT({
    req(reasons_analysis())
    geo_display <- reasons_analysis()$geo_display
    comparison <- data.frame(
      Metric = c(paste(geo_display, "s with Data"), "Most Common Reason", "Average %", "Max %", "Min %", "Categories Count"),
      Absence = c(
        if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_units else 0,
        if (!is.null(reasons_analysis()$absence$metrics)) paste(reasons_analysis()$absence$metrics$most_common, collapse = ", ") else "N/A",
        if (!is.null(reasons_analysis()$absence$metrics)) paste0(reasons_analysis()$absence$metrics$avg_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$absence$metrics)) paste0(reasons_analysis()$absence$metrics$max_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$absence$metrics)) paste0(reasons_analysis()$absence$metrics$min_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_categories else 0
      ),
      `Non-Compliance` = c(
        if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_units else 0,
        if (!is.null(reasons_analysis()$non_compliance$metrics)) paste(reasons_analysis()$non_compliance$metrics$most_common, collapse = ", ") else "N/A",
        if (!is.null(reasons_analysis()$non_compliance$metrics)) paste0(reasons_analysis()$non_compliance$metrics$avg_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$non_compliance$metrics)) paste0(reasons_analysis()$non_compliance$metrics$max_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$non_compliance$metrics)) paste0(reasons_analysis()$non_compliance$metrics$min_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_categories else 0
      ),
      Traditional = c(
        if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_units else 0,
        if (!is.null(reasons_analysis()$traditional$metrics)) paste(reasons_analysis()$traditional$metrics$most_common, collapse = ", ") else "N/A",
        if (!is.null(reasons_analysis()$traditional$metrics)) paste0(reasons_analysis()$traditional$metrics$avg_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$traditional$metrics)) paste0(reasons_analysis()$traditional$metrics$max_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$traditional$metrics)) paste0(reasons_analysis()$traditional$metrics$min_percentage, "%") else "N/A",
        if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_categories else 0
      )
    )
    datatable(comparison, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$top_reasons_comparison <- renderPlot({
    req(reasons_analysis())
    all_data <- reasons_analysis()$top_reasons_summary
    if (!is.null(all_data) && nrow(all_data) > 0) {
      top_data <- all_data %>%
        group_by(reason_type) %>%
        slice_max(avg_percentage, n = 3) %>%
        ungroup()
      ggplot(top_data, aes(x = reorder(reasons, avg_percentage), y = avg_percentage, color = reason_type)) +
        geom_point(size = 4) +
        geom_segment(aes(xend = reasons, yend = 0), size = 1, alpha = 0.5) +
        coord_flip() +
        scale_color_brewer(palette = "Set1", name = "Reason Type") +
        labs(title = "Top Reasons Comparison Across Categories", x = NULL, y = "Average Percentage (%)") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5),
              legend.position = "bottom",
              panel.grid.major.y = element_blank())
    } else {
      ggplot() + theme_void() + labs(title = "No comparison data available") +
        theme(plot.title = element_text(size = 14, hjust = 0.5, color = "gray50"))
    }
  })
  
  # ============================================================
  # PRIORITY DRILL‑DOWN OUTPUTS (bar chart + tables)
  # ============================================================
  
  # Priority bar chart – excludes zero‑data entries, PowerPoint‑optimized
  output$priority_plot <- renderPlot({
    req(reasons_analysis())
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    req(drill[[reason]]$provinces, drill[[reason]]$districts)
    
    # Prepare province data
    prov <- drill[[reason]]$provinces %>%
      mutate(
        level = "Province",
        parent = country,
        name = province,
        pct = prov_pct,
        sort_key = paste(country, sprintf("%06.2f", 100 - pct), name)
      ) %>%
      select(country, parent, name, pct, level, sort_key)
    
    # Prepare district data
    dist <- drill[[reason]]$districts %>%
      mutate(
        level = "District",
        parent = province,
        name = district,
        pct = dist_pct,
        sort_key = paste(country, parent, sprintf("%06.2f", 100 - pct), name)
      ) %>%
      select(country, parent, name, pct, level, sort_key)
    
    # Combine and exclude zero‑percentage entries
    plot_data <- bind_rows(prov, dist) %>%
      filter(pct > 0) %>%
      mutate(name_wrapped = stringr::str_wrap(name, width = 12)) %>%
      arrange(sort_key) %>%
      mutate(name_wrapped = factor(name_wrapped, levels = unique(name_wrapped)))
    
    if (nrow(plot_data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data meeting thresholds", size = 6) +
          theme_void()
      )
    }
    
    total_bars <- nrow(plot_data)
    label_size <- if (total_bars > 30) 2.5 else 3
    axis_text_size <- if (total_bars > 30) 7 else 8
    
    ggplot(plot_data, aes(x = name_wrapped, y = pct, fill = level)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.3) +
      geom_text(aes(label = sprintf("%.1f%%", pct)),
                vjust = -0.5, size = label_size, fontface = "bold") +
      facet_grid(level ~ country, scales = "free_x", space = "free_x") +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_fill_manual(values = c("Province" = "#2c7fb8", "District" = "#7fcdbb"), guide = "none") +
      labs(
        title = paste("Priority Breakdown –", 
                      if(reason == "childabsent") "Child Absent" else "Non‑compliance"),
        subtitle = "Percentages within each country (provinces) and within each province (districts)",
        x = NULL,
        y = "Percentage (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size, lineheight = 0.8),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(face = "bold", size = 11, color = "white"),
        strip.background = element_rect(fill = "#2c3e50", color = NA),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 20, 10)
      )
  }, height = 600)
  
  # Priority Footnote - For UI display (3-sentence summary)
  output$priority_footnote <- renderUI({
    req(reasons_analysis(), reasons_analysis()$drilldown)
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    
    # Get block name
    block_name <- if(input$reasons_block != "All") {
      input$reasons_block
    } else {
      if(input$reasons_block_type == "afro") "All AFRO" else "All IST"
    }
    
    footnote <- generate_drilldown_footnote(
      drill, reason,
      input$drilldown_priority_threshold,
      input$drilldown_province_threshold,
      input$drilldown_district_threshold,
      input$drilldown_dynamic,
      reasons_analysis()$metrics$period_label,
      block_name
    )
    
    # Format with bullet points and styling
    HTML(paste0(
      "<div style='font-family: Arial, sans-serif;'>",
      gsub("\n\n", "<br><br>", 
           gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", footnote)),
      "</div>"
    ))
  })
  
  # District Trend Table
  output$district_trend_table <- renderDT({
    req(reasons_analysis(), reasons_analysis()$drilldown)
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    
    req(drill[[reason]]$trend_analysis)
    
    trend_data <- drill[[reason]]$trend_analysis$trend_data
    trend_metrics <- drill[[reason]]$trend_analysis$trend_metrics
    
    # Reshape data for better display
    trend_table <- trend_data %>%
      pivot_wider(
        id_cols = district,
        names_from = year_month,
        values_from = absent_rate,
        values_fill = list(absent_rate = NA)
      ) %>%
      left_join(trend_metrics %>% select(district, avg_rate, trend_direction, volatility), 
                by = "district")
    
    # Round numeric columns
    numeric_cols <- names(trend_table)[sapply(trend_table, is.numeric)]
    trend_table <- trend_table %>%
      mutate(across(all_of(numeric_cols), ~ round(., 1)))
    
    # Create formatted datatable with conditional coloring
    dt <- datatable(
      trend_table,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      caption = paste("Monthly Child Absent Rates (%) -", 
                      if(reason == "childabsent") "Selected Districts" else "Non-Compliance Districts")
    )
    
    # Add conditional coloring for trend direction
    if("trend_direction" %in% names(trend_table)) {
      dt <- dt %>%
        formatStyle(
          "trend_direction",
          backgroundColor = styleEqual(
            c("Increasing", "Decreasing", "Stable"),
            c("#ffcdd2", "#c8e6c9", "#fff9c4")
          )
        )
    }
    
    return(dt)
  })
  
  # Block Summary Table
  output$block_summary_table <- renderDT({
    req(reasons_analysis(), reasons_analysis()$drilldown)
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    
    req(drill[[reason]]$provinces, drill[[reason]]$districts)
    
    prov_data <- drill[[reason]]$provinces
    dist_data <- drill[[reason]]$districts
    trend_data <- drill[[reason]]$trend_analysis
    
    # Get block name
    block_name <- if(input$reasons_block != "All") {
      input$reasons_block
    } else {
      if(input$reasons_block_type == "afro") "All AFRO Blocks" else "All IST Blocks"
    }
    
    # Get block countries
    block_countries <- if(input$reasons_block != "All") {
      if(input$reasons_block_type == "afro") {
        afro_blocks[[input$reasons_block]]
      } else {
        ist_blocks[[input$reasons_block]]
      }
    } else {
      unique(c(prov_data$country, dist_data$country))
    }
    
    # Calculate most volatile district
    most_volatile <- "N/A"
    if(!is.null(trend_data) && !is.null(trend_data$trend_metrics) && nrow(trend_data$trend_metrics) > 0) {
      most_volatile <- trend_data$trend_metrics$district[which.max(trend_data$trend_metrics$volatility)]
    }
    
    # Create block summary
    block_summary <- data.frame(
      Metric = c(
        "Block Name",
        "Block Type",
        "Total Countries in Block",
        "Countries Meeting Threshold",
        "Total Provinces Identified",
        "Total Districts Identified",
        "Top Contributing Province",
        "Top Province Contribution (%)",
        "Top Contributing District", 
        "Top District Contribution (%)",
        "Average Province Contribution (%)",
        "Average District Contribution (%)",
        "Highest Country",
        "Most Volatile District"
      ),
      Value = c(
        block_name,
        if(input$reasons_block_type == "afro") "AFRO Blocks" else "IST Blocks",
        length(block_countries),
        n_distinct(prov_data$country),
        nrow(prov_data),
        nrow(dist_data),
        if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A",
        if(nrow(prov_data) > 0) paste0(round(max(prov_data$prov_pct), 1), "%") else "N/A",
        if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A",
        if(nrow(dist_data) > 0) paste0(round(max(dist_data$dist_pct), 1), "%") else "N/A",
        if(nrow(prov_data) > 0) paste0(round(mean(prov_data$prov_pct, na.rm = TRUE), 1), "%") else "N/A",
        if(nrow(dist_data) > 0) paste0(round(mean(dist_data$dist_pct, na.rm = TRUE), 1), "%") else "N/A",
        if(nrow(prov_data) > 0) prov_data$country[which.max(prov_data$prov_pct)] else "N/A",
        most_volatile
      )
    )
    
    datatable(
      block_summary,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-right', targets = 1)
        )
      ),
      rownames = FALSE,
      caption = paste("Summary for", 
                      if(input$reasons_block_type == "afro") "AFRO" else "IST", 
                      "Blocks")
    ) %>%
      formatStyle(
        "Metric",
        fontWeight = "bold",
        backgroundColor = "#f8f9fa"
      )
  })
  
  # Provinces table
  output$drilldown_provinces <- renderDT({
    req(reasons_analysis(), reasons_analysis()$drilldown)
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    if (is.null(drill[[reason]]$provinces) || nrow(drill[[reason]]$provinces) == 0) {
      return(datatable(data.frame(Message = "No provinces meet the threshold. Try lowering the province threshold.")))
    }
    
    drill[[reason]]$provinces %>%
      select(country, province, province_total, prov_pct) %>%
      arrange(country, desc(prov_pct)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound("prov_pct", 1)
  })
  
  # Districts table
  output$drilldown_districts <- renderDT({
    req(reasons_analysis(), reasons_analysis()$drilldown)
    drill <- reasons_analysis()$drilldown
    reason <- input$drilldown_reason
    if (is.null(drill[[reason]]$districts) || nrow(drill[[reason]]$districts) == 0) {
      return(datatable(data.frame(Message = "No districts meet the threshold. Try lowering the district threshold.")))
    }
    
    drill[[reason]]$districts %>%
      select(country, province, district, district_total, dist_pct) %>%
      arrange(country, province, desc(dist_pct)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE) %>%
      formatRound("dist_pct", 1)
  })
  
  # Conditional outputs for UI
  output$drilldown_has_trend <- reactive({
    req(reasons_analysis())
    !is.null(reasons_analysis()$drilldown) && 
      !is.null(reasons_analysis()$drilldown[[input$drilldown_reason]]$trend_analysis) &&
      nrow(reasons_analysis()$drilldown[[input$drilldown_reason]]$trend_analysis$trend_data) > 0
  })
  outputOptions(output, "drilldown_has_trend", suspendWhenHidden = FALSE)
  
  output$drilldown_has_data <- reactive({
    req(reasons_analysis())
    !is.null(reasons_analysis()$drilldown) && 
      !is.null(reasons_analysis()$drilldown[[input$drilldown_reason]])
  })
  outputOptions(output, "drilldown_has_data", suspendWhenHidden = FALSE)
  
  # ============================================================
  # DOWNLOAD HANDLERS
  # ============================================================
  
  # Download Combined Plot
  output$download_reasons_combined_plot <- downloadHandler(
    filename = function() {
      paste0("Combined_Reasons_", input$reasons_geo_level, "_", input$reasons_viz_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(reasons_analysis())
      ggsave(file, plot = reasons_analysis()$combined_plot, device = "png",
             width = 33, height = 18, dpi = 300, bg = "white", limitsize = FALSE)
      showNotification("Download successful! Image saved with WHO AFRO specifications (width=33, height=18, dpi=300).", 
                       type = "message", duration = 5)
    }
  )
  
  # Download Absence Plot
  output$download_absence_plot <- downloadHandler(
    filename = function() {
      paste0("Absence_Reasons_", input$reasons_geo_level, "_", input$reasons_viz_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(reasons_analysis())
      ggsave(file, plot = reasons_analysis()$absence$plot, device = "png",
             width = 33, height = 18, dpi = 300, bg = "white", limitsize = FALSE)
      showNotification("Download successful! Image saved with WHO AFRO specifications (width=33, height=18, dpi=300).", 
                       type = "message", duration = 5)
    }
  )
  
  # Download Non-Compliance Plot
  output$download_nc_plot <- downloadHandler(
    filename = function() {
      paste0("NC_Reasons_", input$reasons_geo_level, "_", input$reasons_viz_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(reasons_analysis())
      ggsave(file, plot = reasons_analysis()$non_compliance$plot, device = "png",
             width = 33, height = 18, dpi = 300, bg = "white", limitsize = FALSE)
      showNotification("Download successful! Image saved with WHO AFRO specifications (width=33, height=18, dpi=300).", 
                       type = "message", duration = 5)
    }
  )
  
  # Download Traditional Plot
  output$download_traditional_plot <- downloadHandler(
    filename = function() {
      paste0("Traditional_Reasons_", input$reasons_geo_level, "_", input$reasons_viz_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(reasons_analysis())
      ggsave(file, plot = reasons_analysis()$traditional$plot, device = "png",
             width = 33, height = 18, dpi = 300, bg = "white", limitsize = FALSE)
      showNotification("Download successful! Image saved with WHO AFRO specifications (width=33, height=18, dpi=300).", 
                       type = "message", duration = 5)
    }
  )
  
  # Download Priority Plot with MERGED FOOTNOTES
  output$download_priority_plot <- downloadHandler(
    filename = function() {
      paste0("Priority_", input$drilldown_reason, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        req(reasons_analysis())
        drill <- reasons_analysis()$drilldown
        reason <- input$drilldown_reason
        
        if (is.null(drill) || is.null(drill[[reason]])) {
          showNotification("No drill‑down data available. Run the analysis first.", type = "error")
          return()
        }
        
        if (is.null(drill[[reason]]$provinces) || is.null(drill[[reason]]$districts)) {
          showNotification("Province or district data missing.", type = "error")
          return()
        }
        
        # Prepare province data
        prov <- drill[[reason]]$provinces %>%
          mutate(
            level = "Province",
            parent = country,
            name = province,
            pct = prov_pct,
            sort_key = paste(country, sprintf("%06.2f", 100 - pct), name)
          ) %>%
          select(country, parent, name, pct, level, sort_key)
        
        # Prepare district data
        dist <- drill[[reason]]$districts %>%
          mutate(
            level = "District",
            parent = province,
            name = district,
            pct = dist_pct,
            sort_key = paste(country, parent, sprintf("%06.2f", 100 - pct), name)
          ) %>%
          select(country, parent, name, pct, level, sort_key)
        
        # Combine and exclude zero‑percentage entries
        plot_data <- bind_rows(prov, dist) %>%
          filter(pct > 0) %>%
          mutate(name_wrapped = stringr::str_wrap(name, width = 15)) %>%
          arrange(sort_key) %>%
          mutate(name_wrapped = factor(name_wrapped, levels = unique(name_wrapped)))
        
        if (nrow(plot_data) == 0) {
          showNotification("No data to download – thresholds may be too high.", type = "warning")
          return()
        }
        
        # Get block name for footnote
        block_name <- if(input$reasons_block != "All") {
          input$reasons_block
        } else {
          if(input$reasons_block_type == "afro") "All AFRO" else "All IST"
        }
        
        # ============================================================
        # MERGED FOOTNOTE - Combines both technical and analysis info
        # ============================================================
        
        # Part 1: Generate the 3-sentence analysis summary
        analysis_footnote <- generate_drilldown_footnote(
          drill, reason,
          input$drilldown_priority_threshold,
          input$drilldown_province_threshold,
          input$drilldown_district_threshold,
          input$drilldown_dynamic,
          reasons_analysis()$metrics$period_label,
          block_name
        )
        
        # Part 2: Generate the technical thresholds info
        reason_name <- if(reason == "childabsent") "Absence" else "Non-Compliance"
        
        # Calculate country summaries for technical info
        country_summary <- plot_data %>%
          filter(level == "Province") %>%
          group_by(country) %>%
          summarise(
            n_prov = n(),
            n_dist = sum(plot_data$level == "District" & plot_data$country == first(country)),
            .groups = "drop"
          )
        
        country_text <- paste(
          sapply(1:nrow(country_summary), function(i) {
            paste0(country_summary$country[i], ": ", 
                   country_summary$n_prov[i], " province", 
                   if(country_summary$n_prov[i] != 1) "s",
                   ", ", country_summary$n_dist[i], " district", 
                   if(country_summary$n_dist[i] != 1) "s")
          }),
          collapse = "; "
        )
        
        technical_footnote <- paste0(
          "Technical thresholds (", reason_name, "): Country ≥", input$drilldown_priority_threshold, 
          "%, Province ≥", input$drilldown_province_threshold, 
          "%, District ≥", input$drilldown_district_threshold, "% | ",
          if(input$drilldown_dynamic) "Pareto (80% cumulative)" else "Fixed thresholds",
          " | Geographic coverage: ", country_text
        )
        
        # Merge both footnotes with a separator
        merged_footnote <- paste0(
          analysis_footnote,
          "\n\n",
          "──────────────────────────────────────────────────\n",
          technical_footnote
        )
        
        # merged_footnote_wrapped <- stringr::str_wrap(merged_footnote, width = 120)
        merged_footnote_wrapped <- stringr::str_wrap(merged_footnote, width = 200)
        
        total_bars <- nrow(plot_data)
        label_size <- if (total_bars > 40) 5 else 6
        axis_text_size <- if (total_bars > 40) 14 else 16
        
        p <- ggplot(plot_data, aes(x = name_wrapped, y = pct, fill = level)) +
          geom_col(width = 0.7, color = "white", linewidth = 0.3) +
          geom_text(aes(label = sprintf("%.1f%%", pct)),
                    vjust = -0.5, size = label_size, fontface = "bold") +
          facet_grid(level ~ country, scales = "free_x", space = "free_x") +
          scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                             expand = expansion(mult = c(0, 0.1))) +
          scale_fill_manual(values = c("Province" = "#2c7fb8", "District" = "#7fcdbb"), guide = "none") +
          labs(
            title = paste("Priority Breakdown –", 
                          if(reason == "childabsent") "Child Absent" else "Non‑compliance"),
            subtitle = "Percentages within each country (provinces) and within each province (districts)",
            caption = merged_footnote_wrapped,
            x = NULL,
            y = "Percentage (%)"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size, color = "gray4", lineheight = 0.8),
            axis.text.y = element_text(size = 15, color = "gray30"),
            strip.text = element_text(face = "bold", size = 17, color = "white"),
            strip.background = element_rect(fill = "#2c3e50", color = NA),
            panel.spacing = unit(1, "lines"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 21),
            plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 14),
            plot.caption = element_text(hjust = 0, size = 14, face = "bold", lineheight = 1.3, 
                                        margin = margin(t = 20), color = "gray4"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(10, 10, 30, 10)
          )
        
        ggsave(file, plot = p, device = "png",
               width = 33, height = 18, dpi = 300, bg = "white", limitsize = FALSE)
        
        showNotification("Download successful! Image saved with WHO AFRO specifications (width=33, height=18, dpi=300).", 
                         type = "message", duration = 8)
        
      }, error = function(e) {
        showNotification(paste("Download failed:", e$message), type = "error", duration = 10)
      })
    }
  )
  
  # Download Trend Table as Excel
  output$download_trend_table <- downloadHandler(
    filename = function() {
      paste0("Trend_Table_", input$drilldown_reason, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(reasons_analysis())
      drill <- reasons_analysis()$drilldown
      reason <- input$drilldown_reason
      
      req(drill[[reason]]$trend_analysis)
      
      trend_data <- drill[[reason]]$trend_analysis$trend_data
      trend_metrics <- drill[[reason]]$trend_analysis$trend_metrics
      
      # Create wide format for export
      trend_table <- trend_data %>%
        pivot_wider(
          id_cols = district,
          names_from = year_month,
          values_from = absent_rate,
          values_fill = list(absent_rate = NA)
        ) %>%
        left_join(trend_metrics %>% select(district, avg_rate, trend_direction, volatility), 
                  by = "district") %>%
        mutate(across(where(is.numeric), ~ round(., 1)))
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Trend Analysis")
      openxlsx::writeData(wb, "Trend Analysis", trend_table)
      
      # Add metadata
      openxlsx::addWorksheet(wb, "Metadata")
      metadata <- data.frame(
        Parameter = c("Reason", "Period", "Districts Analyzed", "Time Unit"),
        Value = c(
          if(reason == "childabsent") "Child Absent" else "Non-Compliance",
          reasons_analysis()$metrics$period_label,
          nrow(trend_metrics),
          "Monthly"
        )
      )
      openxlsx::writeData(wb, "Metadata", metadata)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Trend table downloaded successfully!", type = "message")
    }
  )
  
  # Download Block Summary as Excel
  output$download_block_summary <- downloadHandler(
    filename = function() {
      paste0("Block_Summary_", input$reasons_block, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(reasons_analysis())
      drill <- reasons_analysis()$drilldown
      reason <- input$drilldown_reason
      
      req(drill[[reason]]$provinces, drill[[reason]]$districts)
      
      prov_data <- drill[[reason]]$provinces
      dist_data <- drill[[reason]]$districts
      trend_data <- drill[[reason]]$trend_analysis
      
      # Get block name
      block_name <- if(input$reasons_block != "All") {
        input$reasons_block
      } else {
        if(input$reasons_block_type == "afro") "All AFRO Blocks" else "All IST Blocks"
      }
      
      # Get block countries
      block_countries <- if(input$reasons_block != "All") {
        if(input$reasons_block_type == "afro") {
          afro_blocks[[input$reasons_block]]
        } else {
          ist_blocks[[input$reasons_block]]
        }
      } else {
        unique(c(prov_data$country, dist_data$country))
      }
      
      # Calculate most volatile district
      most_volatile <- "N/A"
      if(!is.null(trend_data) && !is.null(trend_data$trend_metrics) && nrow(trend_data$trend_metrics) > 0) {
        most_volatile <- trend_data$trend_metrics$district[which.max(trend_data$trend_metrics$volatility)]
      }
      
      wb <- openxlsx::createWorkbook()
      
      # Block Summary Sheet
      openxlsx::addWorksheet(wb, "Block Summary")
      block_summary <- data.frame(
        Metric = c(
          "Block Name", "Block Type", "Total Countries in Block", 
          "Countries Meeting Threshold", "Total Provinces Identified", 
          "Total Districts Identified", "Top Contributing Province",
          "Top Province Contribution (%)", "Top Contributing District", 
          "Top District Contribution (%)", "Average Province Contribution (%)",
          "Average District Contribution (%)", "Highest Country",
          "Most Volatile District", "Threshold Type", "Analysis Period"
        ),
        Value = c(
          block_name,
          if(input$reasons_block_type == "afro") "AFRO Blocks" else "IST Blocks",
          length(block_countries),
          n_distinct(prov_data$country),
          nrow(prov_data),
          nrow(dist_data),
          if(nrow(prov_data) > 0) prov_data$province[which.max(prov_data$prov_pct)] else "N/A",
          if(nrow(prov_data) > 0) round(max(prov_data$prov_pct), 1) else "N/A",
          if(nrow(dist_data) > 0) dist_data$district[which.max(dist_data$dist_pct)] else "N/A",
          if(nrow(dist_data) > 0) round(max(dist_data$dist_pct), 1) else "N/A",
          if(nrow(prov_data) > 0) round(mean(prov_data$prov_pct, na.rm = TRUE), 1) else "N/A",
          if(nrow(dist_data) > 0) round(mean(dist_data$dist_pct, na.rm = TRUE), 1) else "N/A",
          if(nrow(prov_data) > 0) prov_data$country[which.max(prov_data$prov_pct)] else "N/A",
          most_volatile,
          if(input$drilldown_dynamic) "Pareto (80% cumulative)" else "Fixed Thresholds",
          reasons_analysis()$metrics$period_label
        )
      )
      openxlsx::writeData(wb, "Block Summary", block_summary)
      
      # Provinces Detail Sheet
      openxlsx::addWorksheet(wb, "Provinces")
      prov_export <- prov_data %>%
        select(country, province, province_total, prov_pct) %>%
        arrange(country, desc(prov_pct)) %>%
        mutate(
          prov_pct = round(prov_pct, 1),
          province_total = as.integer(province_total)
        )
      openxlsx::writeData(wb, "Provinces", prov_export)
      
      # Districts Detail Sheet
      openxlsx::addWorksheet(wb, "Districts")
      dist_export <- dist_data %>%
        select(country, province, district, district_total, dist_pct) %>%
        arrange(country, province, desc(dist_pct)) %>%
        mutate(
          dist_pct = round(dist_pct, 1),
          district_total = as.integer(district_total)
        )
      openxlsx::writeData(wb, "Districts", dist_export)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Block summary downloaded successfully!", type = "message")
    }
  )
  
  # Download Provinces Table as Excel
  output$download_drilldown_provinces <- downloadHandler(
    filename = function() {
      paste0("Selected_Provinces_", input$drilldown_reason, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(reasons_analysis())
      drill <- reasons_analysis()$drilldown
      reason <- input$drilldown_reason
      
      if (is.null(drill) || is.null(drill[[reason]]$provinces)) {
        showNotification("No province data available", type = "error")
        return()
      }
      
      prov_data <- drill[[reason]]$provinces %>%
        select(country, province, province_total, prov_pct) %>%
        arrange(country, desc(prov_pct)) %>%
        mutate(
          prov_pct = round(prov_pct, 1),
          province_total = as.integer(province_total)
        ) %>%
        rename(
          `Country` = country,
          `Province` = province,
          `Total Count` = province_total,
          `Percentage (%)` = prov_pct
        )
      
      # Add metadata
      attr(prov_data, "threshold_info") <- paste(
        "Thresholds - Country:", input$drilldown_priority_threshold,
        "%, Province:", input$drilldown_province_threshold, "%",
        if(input$drilldown_dynamic) "(Pareto)" else ""
      )
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Selected Provinces")
      openxlsx::writeData(wb, "Selected Provinces", prov_data)
      
      # Add metadata as a comment/note
      openxlsx::writeData(wb, "Selected Provinces", 
                          data.frame(Note = attr(prov_data, "threshold_info")), 
                          startRow = nrow(prov_data) + 3)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Provinces data downloaded successfully!", type = "message")
    }
  )
  
  # Download Districts Table as Excel
  output$download_drilldown_districts <- downloadHandler(
    filename = function() {
      paste0("Selected_Districts_", input$drilldown_reason, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(reasons_analysis())
      drill <- reasons_analysis()$drilldown
      reason <- input$drilldown_reason
      
      if (is.null(drill) || is.null(drill[[reason]]$districts)) {
        showNotification("No district data available", type = "error")
        return()
      }
      
      dist_data <- drill[[reason]]$districts %>%
        select(country, province, district, district_total, dist_pct) %>%
        arrange(country, province, desc(dist_pct)) %>%
        mutate(
          dist_pct = round(dist_pct, 1),
          district_total = as.integer(district_total)
        ) %>%
        rename(
          `Country` = country,
          `Province` = province,
          `District` = district,
          `Total Count` = district_total,
          `Percentage (%)` = dist_pct
        )
      
      # Add metadata
      attr(dist_data, "threshold_info") <- paste(
        "Thresholds - Country:", input$drilldown_priority_threshold,
        "%, Province:", input$drilldown_province_threshold,
        "%, District:", input$drilldown_district_threshold, "%",
        if(input$drilldown_dynamic) "(Pareto)" else ""
      )
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Selected Districts")
      openxlsx::writeData(wb, "Selected Districts", dist_data)
      
      # Add metadata as a comment/note
      openxlsx::writeData(wb, "Selected Districts", 
                          data.frame(Note = attr(dist_data, "threshold_info")), 
                          startRow = nrow(dist_data) + 3)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Districts data downloaded successfully!", type = "message")
    }
  )
  
  # Download Comparative Summary Excel
  output$download_comparative_summary <- downloadHandler(
    filename = function() {
      paste0("Comparative_Reasons_", input$reasons_geo_level, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(reasons_analysis())
      
      wb <- openxlsx::createWorkbook()
      
      if (!is.null(reasons_analysis()$absence$data_pct)) {
        openxlsx::addWorksheet(wb, "Absence Reasons (%)")
        openxlsx::writeData(wb, "Absence Reasons (%)", reasons_analysis()$absence$data_pct)
      }
      
      if (!is.null(reasons_analysis()$non_compliance$data_pct)) {
        openxlsx::addWorksheet(wb, "NC Reasons (%)")
        openxlsx::writeData(wb, "NC Reasons (%)", reasons_analysis()$non_compliance$data_pct)
      }
      
      if (!is.null(reasons_analysis()$traditional$data_pct)) {
        openxlsx::addWorksheet(wb, "Traditional Reasons (%)")
        openxlsx::writeData(wb, "Traditional Reasons (%)", reasons_analysis()$traditional$data_pct)
      }
      
      if (!is.null(reasons_analysis()$hierarchical$absence)) {
        openxlsx::addWorksheet(wb, "Hierarchical - Absence")
        openxlsx::writeData(wb, "Hierarchical - Absence", reasons_analysis()$hierarchical$absence)
      }
      
      if (!is.null(reasons_analysis()$hierarchical$non_compliance)) {
        openxlsx::addWorksheet(wb, "Hierarchical - NC")
        openxlsx::writeData(wb, "Hierarchical - NC", reasons_analysis()$hierarchical$non_compliance)
      }
      
      if (!is.null(reasons_analysis()$hierarchical$traditional)) {
        openxlsx::addWorksheet(wb, "Hierarchical - Traditional")
        openxlsx::writeData(wb, "Hierarchical - Traditional", reasons_analysis()$hierarchical$traditional)
      }
      
      if (!is.null(reasons_analysis()$top_reasons_summary)) {
        openxlsx::addWorksheet(wb, "Top Reasons Summary")
        openxlsx::writeData(wb, "Top Reasons Summary", reasons_analysis()$top_reasons_summary)
      }
      
      openxlsx::addWorksheet(wb, "Comparative Metrics")
      
      geo_display <- reasons_analysis()$geo_display
      
      metrics_df <- data.frame(
        Metric = c(paste("Absence -", geo_display, "Count"), 
                   "Absence - Categories", 
                   "Absence - Most Common", 
                   "Absence - Avg %",
                   paste("NC -", geo_display, "Count"), 
                   "NC - Categories", 
                   "NC - Most Common", 
                   "NC - Avg %",
                   paste("Traditional -", geo_display, "Count"), 
                   "Traditional - Categories", 
                   "Traditional - Most Common", 
                   "Traditional - Avg %"),
        Value = c(
          if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_units else 0,
          if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$total_categories else 0,
          if (!is.null(reasons_analysis()$absence$metrics)) paste(reasons_analysis()$absence$metrics$most_common, collapse = ", ") else "N/A",
          if (!is.null(reasons_analysis()$absence$metrics)) reasons_analysis()$absence$metrics$avg_percentage else 0,
          
          if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_units else 0,
          if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$total_categories else 0,
          if (!is.null(reasons_analysis()$non_compliance$metrics)) paste(reasons_analysis()$non_compliance$metrics$most_common, collapse = ", ") else "N/A",
          if (!is.null(reasons_analysis()$non_compliance$metrics)) reasons_analysis()$non_compliance$metrics$avg_percentage else 0,
          
          if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_units else 0,
          if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$total_categories else 0,
          if (!is.null(reasons_analysis()$traditional$metrics)) paste(reasons_analysis()$traditional$metrics$most_common, collapse = ", ") else "N/A",
          if (!is.null(reasons_analysis()$traditional$metrics)) reasons_analysis()$traditional$metrics$avg_percentage else 0
        )
      )
      
      openxlsx::writeData(wb, "Comparative Metrics", metrics_df)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      showNotification("Comparative summary downloaded successfully!", type = "message")
    }
  )
  
  
  # ============================================================
  # ⚠️ UNRESOLVED CASES (Updated with Dynamic Block Switching)
  # ============================================================
  
  # ADDED: Reactive block choices for unresolved cases
  unresolved_block_choices <- reactive({
    if (input$unresolved_block_type == "afro") {
      return(afro_blocks)
    } else {
      return(ist_blocks)
    }
  })
  
  # ADDED: Reactive block names for unresolved cases
  unresolved_block_names <- reactive({
    if (input$unresolved_block_type == "afro") {
      return(names(afro_blocks))
    } else {
      return(names(ist_blocks))
    }
  })
  
  # ADDED: Update block selection when block type changes
  observe({
    updateSelectInput(session, "unresolved_block", 
                      choices = c("All", unresolved_block_names()))
  })
  
  # Initialize country choices for Unresolved Cases
  observe({
    req(dat, input$unresolved_block)
    
    available_countries <- if (input$unresolved_block != "All") {
      # MODIFIED: Use reactive block choices
      unresolved_block_choices()[[input$unresolved_block]]
    } else {
      sort(unique(dat$country))
    }
    
    updateSelectizeInput(
      session, 
      "unresolved_countries",
      choices = c("All" = "", available_countries),
      selected = character(0)
    )
  })
  
  # Update province choices based on selected countries for Unresolved Cases
  observe({
    req(dat, input$unresolved_countries)
    
    if (length(input$unresolved_countries) > 0 && !all(input$unresolved_countries == "")) {
      provinces <- dat %>%
        filter(country %in% input$unresolved_countries) %>%
        distinct(province) %>%
        pull(province) %>%
        sort() %>%
        na.omit()
      
      updateSelectizeInput(
        session, 
        "unresolved_provinces",
        choices = c("All" = "", provinces),
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        session, 
        "unresolved_provinces",
        choices = c("All" = ""),
        selected = character(0)
      )
    }
  })
  
  # Update district choices based on selected countries and provinces for Unresolved Cases
  observe({
    req(dat, input$unresolved_countries)
    
    if (length(input$unresolved_countries) > 0 && !all(input$unresolved_countries == "")) {
      districts_data <- dat %>%
        filter(country %in% input$unresolved_countries)
      
      # Apply province filtering if any provinces are selected
      if (!is.null(input$unresolved_provinces) && 
          length(input$unresolved_provinces) > 0 && 
          !all(input$unresolved_provinces == "")) {
        districts_data <- districts_data %>%
          filter(province %in% input$unresolved_provinces)
      }
      
      districts <- districts_data %>%
        distinct(district) %>%
        pull(district) %>%
        sort() %>%
        na.omit()
      
      updateSelectizeInput(
        session, 
        "unresolved_districts",
        choices = c("All" = "", districts),
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        session, 
        "unresolved_districts",
        choices = c("All" = ""),
        selected = character(0)
      )
    }
  })
  
  # Reset provinces and districts when block changes for Unresolved Cases
  observe({
    req(input$unresolved_block)
    
    # Reset provinces and districts when block changes
    updateSelectizeInput(session, "unresolved_provinces", selected = character(0))
    updateSelectizeInput(session, "unresolved_districts", selected = character(0))
  })
  
  # Filter status display
  output$unresolved_filter_status <- renderText({
    status <- c()
    
    # ADDED: Block type information
    block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
    status <- c(status, paste("Block Type:", block_type_text))
    
    if (!is.null(input$unresolved_block) && input$unresolved_block != "All") {
      status <- c(status, paste("Block:", input$unresolved_block))
    }
    
    if (!is.null(input$unresolved_countries) && length(input$unresolved_countries) > 0) {
      status <- c(status, paste(length(input$unresolved_countries), "countries"))
    }
    if (!is.null(input$unresolved_provinces) && length(input$unresolved_provinces) > 0) {
      status <- c(status, paste(length(input$unresolved_provinces), "provinces"))
    }
    if (!is.null(input$unresolved_districts) && length(input$unresolved_districts) > 0) {
      status <- c(status, paste(length(input$unresolved_districts), "districts"))
    }
    
    if (length(status) > 0) {
      paste("Active filters:", paste(status, collapse = ", "))
    } else {
      "No geographic filters applied"
    }
  })
  
  # Shared helper: builds one reason's unresolved-cases result (or all 9
  # when selected_reason is NULL/omitted, e.g. for the slide-deck generator
  # which wants the full set). Extracted so the Analyze button below and the
  # lazy reason-rebuild observer right after it read the exact same filter
  # inputs the exact same way and can't silently drift apart.
  run_unresolved_analysis <- function(selected_reason = NULL) {
    x <- if (input$unresolved_time_type == "months") input$unresolved_months else NULL
    y <- if (input$unresolved_time_type == "year") input$unresolved_year else NULL

    # Multi-select country handling
    country_selection <- if (!is.null(input$unresolved_countries) &&
                             length(input$unresolved_countries) > 0 &&
                             !all(input$unresolved_countries == "")) {
      input$unresolved_countries
    } else NULL

    # Multi-select province handling
    province_selection <- if (!is.null(input$unresolved_provinces) &&
                              length(input$unresolved_provinces) > 0 &&
                              !all(input$unresolved_provinces == "")) {
      input$unresolved_provinces
    } else NULL

    # Multi-select district handling
    district_selection <- if (!is.null(input$unresolved_districts) &&
                              length(input$unresolved_districts) > 0 &&
                              !all(input$unresolved_districts == "")) {
      input$unresolved_districts
    } else NULL

    # Debug output
    print(paste("Block Type:", input$unresolved_block_type))
    print(paste("Selected Block:", input$unresolved_block))
    print(paste("Country selection:", paste(country_selection, collapse = ", ")))
    print(paste("Province selection:", paste(province_selection, collapse = ", ")))
    print(paste("District selection:", paste(district_selection, collapse = ", ")))
    print(paste("Selected reason:", if (is.null(selected_reason)) "ALL" else selected_reason))

    generate_unresolved_NC_plots_DS_optimized(
      data = dat,
      x = x,
      y = y,
      block_selection = if (input$unresolved_block != "All") input$unresolved_block else NULL,
      country_selection = country_selection,
      province_selection = province_selection,
      district_selection = district_selection,
      all_countries = all_countries,
      all_provinces = all_provinces,
      all_districts = all_districts,
      afro_blocks = afro_blocks,
      ist_blocks = ist_blocks,  # ADDED: Pass IST blocks
      block_type = input$unresolved_block_type,  # ADDED: Pass block type
      ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS,
      selected_reason = selected_reason
    )
  }

  # Unresolved Cases Analysis with dynamic block switching
  observeEvent(input$unresolved_analyze, {
    if (input$tabs != "unresolved") return()  # Only run if unresolved cases tab is active

    # Show loading state
    showNotification("🔄 Running optimized unresolved cases analysis...", type = "message", duration = 3)

    safe_analysis({
      # Clear previous results to free memory
      unresolved_analysis(NULL)
      force_gc()

      # --- Run OPTIMIZED analysis with dynamic block switching ---
      # Only builds the currently-selected reason's maps: output$unresolved_map
      # (and the export/download handlers) only ever display
      # plots[[input$unresolved_reason]], so building the other ~8 reasons on
      # every Analyze click was pure wasted work. Switching the "Select
      # Reason" dropdown afterwards lazily builds + caches just that one
      # reason via the observeEvent(input$unresolved_reason, ...) below,
      # instead of eagerly building all 9 up front.
      result <- run_unresolved_analysis(selected_reason = input$unresolved_reason)

      unresolved_analysis(result)
      force_gc()

    }, "Unresolved Cases Analysis (Optimized)")
  })

  # Lazily build + cache a single reason's plot when the reason dropdown
  # changes AFTER Analyze has already run, so switching reasons still feels
  # instant once a reason has been seen, without ever building all 9 up
  # front. No-op if nothing has been analyzed yet, or if that reason's plot
  # is already cached from a previous build.
  observeEvent(input$unresolved_reason, {
    if (input$tabs != "unresolved") return()
    current <- unresolved_analysis()
    req(current)
    if (is.null(current$processed_data) || nrow(current$processed_data) == 0) return()
    if (!is.null(current$plots) && input$unresolved_reason %in% names(current$plots)) return()

    safe_analysis({
      extra <- run_unresolved_analysis(selected_reason = input$unresolved_reason)
      if (!is.null(extra$plots) && length(extra$plots) > 0) {
        latest <- unresolved_analysis()
        latest$plots <- utils::modifyList(latest$plots, extra$plots)
        unresolved_analysis(latest)
      }
      force_gc()
    }, "Unresolved Cases Analysis (lazy reason build)")
  }, ignoreInit = TRUE)

  # Unresolved Value Boxes - ENHANCED with block type
  output$unresolved_total_cases <- renderValueBox({
    req(unresolved_analysis(), input$unresolved_reason)
    
    total_cases <- 0
    data <- unresolved_analysis()$summary_data
    
    if (!is.null(data) && nrow(data) > 0 && !"message" %in% names(data)) {
      # The summary_data has columns like: country, r_non_compliance_sum, r_childabsent_sum, etc.
      # Convert reason name to column name format used in the data
      reason_mapping <- list(
        "Non compliance" = "r_non_compliance_sum",
        "Child absent" = "r_childabsent_sum", 
        "House not visited" = "r_house_not_visited_sum",
        "Child was asleep" = "r_child_was_asleep_sum",
        "Child is visitor" = "r_child_is_a_visitor_sum",
        "Vaccinated NFM" = "r_vaccinated_but_not_FM_sum",
        "Child not born" = "r_childnotborn_sum",
        "Security" = "r_security_sum",
        "Other" = "other_r_sum"
      )
      
      reason_col <- reason_mapping[[input$unresolved_reason]]
      
      if (!is.null(reason_col) && reason_col %in% names(data)) {
        total_cases <- sum(data[[reason_col]], na.rm = TRUE)
      } else {
        # If specific reason column not found, sum all reason columns
        reason_cols <- grep("_sum$", names(data), value = TRUE)
        if (length(reason_cols) > 0) {
          total_cases <- sum(sapply(data[reason_cols], sum, na.rm = TRUE), na.rm = TRUE)
        }
      }
    }
    
    # ADDED: Block type indicator
    block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
    
    valueBox(
      value = format(total_cases, big.mark = ","),
      subtitle = paste("Total", input$unresolved_reason, "Cases in", block_type_text),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$unresolved_districts_box <- renderValueBox({
    req(unresolved_analysis(), input$unresolved_reason)
    
    n_districts <- 0
    
    # Use the processed data from the optimized analysis
    if (!is.null(unresolved_analysis()$processed_data)) {
      data <- unresolved_analysis()$processed_data
      
      if (!is.null(data) && nrow(data) > 0) {
        # Convert reason name to actual column name in the data
        reason_mapping <- list(
          "Non compliance" = "r_non_compliance",
          "Child absent" = "r_childabsent", 
          "House not visited" = "r_house_not_visited",
          "Child was asleep" = "r_child_was_asleep",
          "Child is visitor" = "r_child_is_a_visitor",
          "Vaccinated NFM" = "r_vaccinated_but_not_FM",
          "Child not born" = "r_childnotborn",
          "Security" = "r_security",
          "Other" = "other_r"
        )
        
        reason_col <- reason_mapping[[input$unresolved_reason]]
        
        if (!is.null(reason_col) && reason_col %in% names(data)) {
          # Count distinct districts that reported this specific reason
          district_data <- data %>%
            dplyr::filter(
              !is.na(district) & 
                !is.na(.data[[reason_col]]) & 
                .data[[reason_col]] > 0
            )
          
          if (nrow(district_data) > 0) {
            n_districts <- district_data %>%
              dplyr::distinct(country, district) %>%
              nrow()
          }
        }
      }
    } else {
      # Fallback: if no processed data, use summary_data to estimate
      data <- unresolved_analysis()$summary_data
      if (!is.null(data) && nrow(data) > 0 && !"message" %in% names(data)) {
        # Count countries and estimate districts (fallback method)
        n_countries <- n_distinct(data$country)
        n_districts <- n_countries * 8  # Average ~8 districts per country
        
        block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
        
        valueBox(
          value = paste0("~", n_districts),
          subtitle = paste("Estimated Districts with", input$unresolved_reason, "in", block_type_text),
          icon = icon("map-marker-alt"),
          color = "light-blue"
        )
        return()
      }
    }
    
    valueBox(
      value = n_districts,
      subtitle = paste("Districts Reporting", input$unresolved_reason),
      icon = icon("map-marker-alt"),
      color = "light-blue"
    )
  })
  
  output$unresolved_countries_box <- renderValueBox({
    req(unresolved_analysis(), input$unresolved_reason)
    
    n_countries <- 0
    data <- unresolved_analysis()$summary_data
    
    if (!is.null(data) && nrow(data) > 0 && !"message" %in% names(data)) {
      # Count countries that have data for this specific reason
      reason_mapping <- list(
        "Non compliance" = "r_non_compliance_sum",
        "Child absent" = "r_childabsent_sum", 
        "House not visited" = "r_house_not_visited_sum",
        "Child was asleep" = "r_child_was_asleep_sum",
        "Child is visitor" = "r_child_is_a_visitor_sum",
        "Vaccinated NFM" = "r_vaccinated_but_not_FM_sum",
        "Child not born" = "r_childnotborn_sum",
        "Security" = "r_security_sum",
        "Other" = "other_r_sum"
      )
      
      reason_col <- reason_mapping[[input$unresolved_reason]]
      
      if (!is.null(reason_col) && reason_col %in% names(data)) {
        # Count countries with non-zero values for this reason
        n_countries <- data %>%
          dplyr::filter(!is.na(.data[[reason_col]]) & .data[[reason_col]] > 0) %>%
          dplyr::distinct(country) %>%
          nrow()
      }
    }
    
    # ADDED: Block type indicator
    block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
    
    valueBox(
      value = n_countries,
      subtitle = paste("Countries Reporting", input$unresolved_reason, "in", block_type_text),
      icon = icon("flag"),
      color = "aqua"
    )
  })
  
  output$unresolved_map <- renderPlot({
    req(unresolved_analysis(), input$unresolved_reason)
    plots <- unresolved_analysis()$plots
    if (!is.null(plots) && length(plots) > 0 && input$unresolved_reason %in% names(plots)) {
      plots[[input$unresolved_reason]]
    } else {
      # Show informative message when no data
      block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
      ggplot2::ggplot() + 
        ggplot2::theme_void() + 
        ggplot2::labs(
          title = paste("No data available for", input$unresolved_reason, "in selected", block_type_text),
          subtitle = "Try adjusting your time period, geographic selection, or reason type"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, hjust = 0.5, color = "gray50"),
          plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "gray40")
        )
    }
  })
  
  output$unresolved_data_table <- renderDT({
    req(unresolved_analysis())
    data <- unresolved_analysis()$summary_data
    if (!is.null(data) && nrow(data) > 0 && !"message" %in% names(data)) {
      datatable(
        data, 
        options = list(
          scrollX = TRUE, 
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      )
    } else {
      block_type_text <- ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks")
      datatable(
        tibble(Message = paste("No unresolved cases data available for the selected", block_type_text, "and filters")),
        options = list(dom = 't')
      )
    }
  })
  
  # Unresolved Cases Downloads - ENHANCED with block type
  output$download_unresolved_map <- downloadHandler(
    filename = function() {
      block_type <- ifelse(input$unresolved_block_type == "afro", "AFRO", "IST")
      paste0("unresolved-cases-", input$unresolved_reason, "-", block_type, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating unresolved cases map...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          req(unresolved_analysis(), input$unresolved_reason)
          incProgress(0.6, detail = "Saving image...")
          plots <- unresolved_analysis()$plots
          if (!is.null(plots) && input$unresolved_reason %in% names(plots)) {
            ggsave(
              file, 
              plot = plots[[input$unresolved_reason]], 
              width = 16, 
              height = 12, 
              dpi = 300,
              bg = "white"
            )
          }
          incProgress(1, detail = "Download ready!")
        }
      )
    }
  )
  
  output$download_unresolved_data <- downloadHandler(
    filename = function() {
      block_type <- ifelse(input$unresolved_block_type == "afro", "AFRO", "IST")
      paste0("unresolved-cases-", block_type, "-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      withProgress(
        message = 'Preparing unresolved cases data...',
        detail = 'Processing unresolved cases data...',
        value = 0.3,
        {
          req(unresolved_analysis())
          incProgress(0.5, detail = "Writing Excel file...")
          
          # Create enhanced Excel workbook with multiple sheets
          wb <- openxlsx::createWorkbook()
          
          # Sheet 1: Summary Data
          openxlsx::addWorksheet(wb, "Summary Data")
          openxlsx::writeData(wb, "Summary Data", unresolved_analysis()$summary_data)
          
          # Sheet 2: Filter Information
          openxlsx::addWorksheet(wb, "Filter Information")
          
          filter_info <- data.frame(
            Parameter = c(
              "Block Type",
              "Selected Block",
              "Time Period Type",
              "Time Period Value",
              "Selected Reason",
              "Countries Selected",
              "Provinces Selected", 
              "Districts Selected",
              "Analysis Period",
              "Date Generated"
            ),
            Value = c(
              ifelse(input$unresolved_block_type == "afro", "AFRO Blocks", "IST Blocks"),
              if (!is.null(input$unresolved_block)) input$unresolved_block else "All",
              input$unresolved_time_type,
              if (input$unresolved_time_type == "months") paste(input$unresolved_months, "months") else 
                if (input$unresolved_time_type == "year") paste("Year", input$unresolved_year) else "All",
              input$unresolved_reason,
              if (!is.null(input$unresolved_countries) && length(input$unresolved_countries) > 0) 
                paste(length(input$unresolved_countries), "countries") else "All",
              if (!is.null(input$unresolved_provinces) && length(input$unresolved_provinces) > 0) 
                paste(length(input$unresolved_provinces), "provinces") else "All",
              if (!is.null(input$unresolved_districts) && length(input$unresolved_districts) > 0) 
                paste(length(input$unresolved_districts), "districts") else "All",
              if (!is.null(unresolved_analysis()$period_info)) unresolved_analysis()$period_info else "Not specified",
              as.character(Sys.Date())
            )
          )
          
          openxlsx::writeData(wb, "Filter Information", filter_info)
          
          # Sheet 3: Processed Data (if available)
          if (!is.null(unresolved_analysis()$processed_data) && nrow(unresolved_analysis()$processed_data) > 0) {
            openxlsx::addWorksheet(wb, "Processed Data")
            openxlsx::writeData(wb, "Processed Data", unresolved_analysis()$processed_data)
          }
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          incProgress(1, detail = "Download ready!")
        }
      )
    }
  )
  
  # ============================================================
  # 🗺️ IM SETTLEMENT MAPS SERVER LOGIC (RDS VERSION - CLEAN)
  # ============================================================
  
  # Load RDS data with date filtering
  data_reactive <- reactive({
    req(input$file_id)
    input_file <- paste0(input_folder, input$file_id, ".rds")
    
    if (file.exists(input_file)) {
      # SIMPLE RDS loading - no extra parameters
      im_data <- readRDS(input_file)
      names(im_data) <- toupper(names(im_data))
      
      # Check if date column exists
      date_col <- grep("DATE", names(im_data), value = TRUE, ignore.case = TRUE)
      
      if (length(date_col) > 0) {
        date_col_name <- date_col[1]
        
        # Try to convert to date and filter for selected date
        im_data <- tryCatch({
          im_data %>%
            mutate(!!date_col_name := as.Date(.data[[date_col_name]])) %>%
            filter(.data[[date_col_name]] == input$selected_date)
        }, error = function(e) {
          showNotification(paste("Date conversion error:", e$message), type = "warning")
          return(im_data)
        })
        
        if (nrow(im_data) == 0) {
          showNotification("No data available for selected date", type = "warning")
        }
      } else {
        showNotification("No date column found - showing all data", type = "warning")
      }
      
      return(im_data)
    } else {
      showNotification(paste("File", input_file, "not found!"), type = "error")
      return(NULL)
    }
  })
  
  # Update date input based on selected file's data
  observe({
    req(input$file_id)
    input_file <- paste0(input_folder, input$file_id, ".rds")
    
    if (file.exists(input_file)) {
      # SIMPLE RDS loading - no extra parameters
      date_data <- readRDS(input_file)
      names(date_data) <- toupper(names(date_data))
      
      date_col <- grep("DATE", names(date_data), value = TRUE, ignore.case = TRUE)
      
      if (length(date_col) > 0) {
        date_col_name <- date_col[1]
        
        date_data <- tryCatch({
          date_data %>%
            mutate(!!date_col_name := as.Date(.data[[date_col_name]]))
        }, error = function(e) {
          showNotification(paste("Date column found but couldn't convert:", e$message), 
                           type = "warning")
          return(NULL)
        })
        
        if (!is.null(date_data)) {
          available_dates <- unique(date_data[[date_col_name]])
          updateDateInput(session, "selected_date",
                          value = max(available_dates, na.rm = TRUE),
                          min = min(available_dates, na.rm = TRUE),
                          max = max(available_dates, na.rm = TRUE))
          shinyjs::enable("selected_date")
        } else {
          shinyjs::disable("selected_date")
        }
      } else {
        shinyjs::disable("selected_date")
      }
    }
  })
  
  # Store last generated plot for download
  map_plot_reactive <- reactiveVal(NULL)
  
  # ------------------------ Country ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data)
    
    updateSelectInput(session, "ctry", choices = unique(na.omit(im_data$COUNTRY)))
    updateSelectInput(session, "Province", choices = NULL)
    updateSelectInput(session, "District", choices = NULL)
    updateSelectInput(session, "Facility", choices = NULL)
    updateSelectInput(session, "response", choices = NULL)
    updateSelectInput(session, "roundNumber", choices = NULL)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ Province ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$ctry)
    
    provinces <- im_data %>%
      filter(COUNTRY == input$ctry) %>%
      pull(PROVINCE) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "Province", choices = provinces)
    updateSelectInput(session, "District", choices = NULL)
    updateSelectInput(session, "Facility", choices = NULL)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ District ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$ctry, input$Province)
    
    districts <- im_data %>%
      filter(COUNTRY == input$ctry, PROVINCE == input$Province) %>%
      pull(DISTRICT) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "District", choices = districts)
    updateSelectInput(session, "Facility", choices = NULL)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ Facility ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$ctry, input$Province, input$District)
    
    facilities <- im_data %>%
      filter(COUNTRY == input$ctry, PROVINCE == input$Province, DISTRICT == input$District) %>%
      pull(FACILITY) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "Facility", choices = facilities)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ Response ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$Facility)
    
    responses <- im_data %>%
      filter(FACILITY == input$Facility) %>%
      pull(RESPONSE) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "response", choices = responses)
    updateSelectInput(session, "roundNumber", choices = NULL)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ Round Number ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$Facility, input$response)
    
    rounds <- im_data %>%
      filter(FACILITY == input$Facility, RESPONSE == input$response) %>%
      pull(ROUNDNUMBER) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "roundNumber", choices = rounds)
    updateSelectInput(session, "settlement", choices = NULL)
  })
  
  # ------------------------ Settlement ------------------------
  observe({
    im_data <- data_reactive()
    req(im_data, input$Facility, input$response, input$roundNumber)
    
    settlements <- im_data %>%
      filter(FACILITY == input$Facility,
             RESPONSE == input$response,
             ROUNDNUMBER == input$roundNumber) %>%
      pull(SETTLEMENT) %>%
      unique() %>%
      na.omit()
    
    updateSelectInput(session, "settlement", choices = settlements)
  }) 
  
  # ------------------------ Generate Map ------------------------
  observeEvent(input$generate, {
    output$mapPlot <- renderPlot({
      req(
        input$file_id,
        input$ctry,
        input$Province,
        input$District,
        input$Facility,
        input$response,
        input$roundNumber,
        input$settlement
      )
      
      tryCatch({
        plot <- create_settlement_map(
          x = input$bbox_m,
          ctry = input$ctry,
          province = input$Province,
          district = input$District,
          facility = input$Facility,
          Response = input$response,
          roundNumber = input$roundNumber,
          settlement = input$settlement,
          bbox_m = input$bbox_m,
          file_id = input$file_id
        )
        map_plot_reactive(plot)
        plot
      }, error = function(e) {
        showNotification(paste("Error generating map:", e$message), type = "error")
        NULL
      })
    }, height = 600)
  })
  
  # ------------------------ Download Map ------------------------
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("map_", input$ctry, "_", input$settlement, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      withProgress(
        message = 'Generating map download...',
        detail = 'Rendering high-quality image...',
        value = 0.3,
        {
          plot <- map_plot_reactive()
          if (!is.null(plot)) {
            incProgress(0.6, detail = "Saving image...")
            ggsave(file, plot = plot, width = 12, height = 7, dpi = 300)
            incProgress(1, detail = "Download ready!")
          } else {
            showNotification("No map available to download.", type = "error")
          }
        }
      )
    }
  )
  
  # ============================================================
  # 📊 SETTLEMENT DATA SERVER LOGIC (With Date Range Filtering)
  # ============================================================
  
  # Reactive data for settlement data explorer - loads entire dataset
  settlement_data_reactive <- reactive({
    req(input$settlement_file_id)
    input_file <- paste0(input_folder, input$settlement_file_id, ".rds")
    
    if (file.exists(input_file)) {
      im_data <- readRDS(input_file)
      names(im_data) <- toupper(names(im_data))
      return(im_data)
    } else {
      showNotification(paste("File", input_file, "not found!"), type = "error")
      return(NULL)
    }
  })
  
  # Filtered data based on user selections including date range
  settlement_filtered_data_reactive <- reactive({
    data <- settlement_data_reactive()
    req(data)
    
    # Apply date range filter if date column exists
    date_col <- grep("DATE", names(data), value = TRUE, ignore.case = TRUE)
    if (length(date_col) > 0) {
      date_col_name <- date_col[1]
      data <- tryCatch({
        data %>%
          mutate(!!date_col_name := as.Date(.data[[date_col_name]])) %>%
          filter(.data[[date_col_name]] >= input$settlement_date_range[1] & 
                   .data[[date_col_name]] <= input$settlement_date_range[2])
      }, error = function(e) {
        showNotification(paste("Date filtering error:", e$message), type = "warning")
        return(data)
      })
    }
    
    # Apply other filters only if they are selected
    if (!is.null(input$settlement_ctry) && length(input$settlement_ctry) > 0) {
      data <- data %>% filter(COUNTRY %in% input$settlement_ctry)
    }
    if (!is.null(input$settlement_province) && length(input$settlement_province) > 0) {
      data <- data %>% filter(PROVINCE %in% input$settlement_province)
    }
    if (!is.null(input$settlement_district) && length(input$settlement_district) > 0) {
      data <- data %>% filter(DISTRICT %in% input$settlement_district)
    }
    if (!is.null(input$settlement_facility) && length(input$settlement_facility) > 0) {
      data <- data %>% filter(FACILITY %in% input$settlement_facility)
    }
    if (!is.null(input$settlement_response) && length(input$settlement_response) > 0) {
      data <- data %>% filter(RESPONSE %in% input$settlement_response)
    }
    if (!is.null(input$settlement_roundNumber) && length(input$settlement_roundNumber) > 0) {
      data <- data %>% filter(ROUNDNUMBER %in% input$settlement_roundNumber)
    }
    if (!is.null(input$settlement_settlement) && length(input$settlement_settlement) > 0) {
      data <- data %>% filter(SETTLEMENT %in% input$settlement_settlement)
    }
    
    return(data)
  })
  
  # Update date range input based on selected file's data
  observe({
    req(input$settlement_file_id)
    input_file <- paste0(input_folder, input$settlement_file_id, ".rds")
    
    if (file.exists(input_file)) {
      date_data <- readRDS(input_file)
      names(date_data) <- toupper(names(date_data))
      
      date_col <- grep("DATE", names(date_data), value = TRUE, ignore.case = TRUE)
      if (length(date_col) > 0) {
        date_col_name <- date_col[1]
        date_data <- tryCatch({
          date_data %>%
            mutate(!!date_col_name := as.Date(.data[[date_col_name]]))
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(date_data)) {
          available_dates <- unique(date_data[[date_col_name]])
          min_date <- min(available_dates, na.rm = TRUE)
          max_date <- max(available_dates, na.rm = TRUE)
          
          updateDateRangeInput(session, "settlement_date_range",
                               start = max_date - 30,  # Default to last 30 days
                               end = max_date,
                               min = min_date,
                               max = max_date)
        }
      }
    }
  })
  
  # Update filter choices for settlement data explorer
  observe({
    data <- settlement_data_reactive()
    req(data)
    
    updateSelectInput(session, "settlement_ctry", choices = unique(na.omit(data$COUNTRY)))
    updateSelectInput(session, "settlement_province", choices = unique(na.omit(data$PROVINCE)))
    updateSelectInput(session, "settlement_district", choices = unique(na.omit(data$DISTRICT)))
    updateSelectInput(session, "settlement_facility", choices = unique(na.omit(data$FACILITY)))
    updateSelectInput(session, "settlement_response", choices = unique(na.omit(data$RESPONSE)))
    updateSelectInput(session, "settlement_roundNumber", choices = unique(na.omit(data$ROUNDNUMBER)))
    updateSelectInput(session, "settlement_settlement", choices = unique(na.omit(data$SETTLEMENT)))
  })
  
  # Reset filters for settlement data
  observeEvent(input$settlement_reset_filters, {
    updateSelectInput(session, "settlement_ctry", selected = character(0))
    updateSelectInput(session, "settlement_province", selected = character(0))
    updateSelectInput(session, "settlement_district", selected = character(0))
    updateSelectInput(session, "settlement_facility", selected = character(0))
    updateSelectInput(session, "settlement_response", selected = character(0))
    updateSelectInput(session, "settlement_roundNumber", selected = character(0))
    updateSelectInput(session, "settlement_settlement", selected = character(0))
    
    # Reset date range to default (last 30 days)
    req(input$settlement_file_id)
    input_file <- paste0(input_folder, input$settlement_file_id, ".rds")
    if (file.exists(input_file)) {
      date_data <- readRDS(input_file)
      names(date_data) <- toupper(names(date_data))
      
      date_col <- grep("DATE", names(date_data), value = TRUE, ignore.case = TRUE)
      if (length(date_col) > 0) {
        date_col_name <- date_col[1]
        date_data <- tryCatch({
          date_data %>%
            mutate(!!date_col_name := as.Date(.data[[date_col_name]]))
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(date_data)) {
          max_date <- max(date_data[[date_col_name]], na.rm = TRUE)
          updateDateRangeInput(session, "settlement_date_range",
                               start = max_date - 30,
                               end = max_date)
        }
      }
    }
  })
  
  # Render settlement data table
  output$settlement_data_table <- renderDT({
    data <- settlement_filtered_data_reactive()
    req(data)
    
    datatable(
      data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE,
        pageLength = 25,
        autoWidth = TRUE
      ),
      class = 'display nowrap',
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Settlement data summary
  output$settlement_data_summary <- renderText({
    data <- settlement_filtered_data_reactive()
    req(data)
    
    total_records <- nrow(data)
    unique_countries <- length(unique(data$COUNTRY))
    unique_settlements <- length(unique(data$SETTLEMENT))
    total_missed <- if("TOTAL_CHILD_MISSED" %in% names(data)) sum(data$TOTAL_CHILD_MISSED, na.rm = TRUE) else "N/A"
    
    paste(
      "Settlement Data Summary:\n",
      "Total Records:", total_records, "\n",
      "Unique Countries:", unique_countries, "\n", 
      "Unique Settlements:", unique_settlements, "\n",
      "Total Children Missed:", total_missed
    )
  })
  
  # Download handlers for settlement data
  output$settlement_download_csv <- downloadHandler(
    filename = function() {
      paste0("settlement_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- settlement_filtered_data_reactive()
      req(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$settlement_download_excel <- downloadHandler(
    filename = function() {
      paste0("settlement_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data <- settlement_filtered_data_reactive()
      req(data)
      writexl::write_xlsx(data, file)
    }
  )
  
  # ============================================================
  # 📊 POWERPOINT SLIDE GENERATION
  # ============================================================
  
  # Reactive values for slide generation
  slide_analysis_results <- reactiveVal(list())
  current_slide_index <- reactiveVal(1)
  slides_generated <- reactiveVal(FALSE)
  
  # Initialize country selection for slides
  observe({
    req(dat, input$slide_block)
    
    available_countries <- if (input$slide_block != "All") {
      afro_blocks[[input$slide_block]]
    } else {
      sort(unique(dat$country))
    }
    
    updateSelectizeInput(
      session, 
      "slide_countries",
      choices = c("All" = "", available_countries),
      selected = character(0)
    )
  })
  
  # Update province choices for slides
  observe({
    req(dat, input$slide_countries)
    
    if (length(input$slide_countries) > 0 && !all(input$slide_countries == "")) {
      provinces <- dat %>%
        filter(country %in% input$slide_countries) %>%
        distinct(province) %>%
        pull(province) %>%
        sort() %>%
        na.omit()
      
      updateSelectizeInput(
        session, 
        "slide_provinces",
        choices = c("All" = "", provinces),
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        session, 
        "slide_provinces",
        choices = c("All" = ""),
        selected = character(0)
      )
    }
  })
  
  # Update district choices for slides
  observe({
    req(dat, input$slide_countries)
    
    if (length(input$slide_countries) > 0 && !all(input$slide_countries == "")) {
      districts_data <- dat %>%
        filter(country %in% input$slide_countries)
      
      if (!is.null(input$slide_provinces) && 
          length(input$slide_provinces) > 0 && 
          !all(input$slide_provinces == "")) {
        districts_data <- districts_data %>%
          filter(province %in% input$slide_provinces)
      }
      
      districts <- districts_data %>%
        distinct(district) %>%
        pull(district) %>%
        sort() %>%
        na.omit()
      
      updateSelectizeInput(
        session, 
        "slide_districts",
        choices = c("All" = "", districts),
        selected = character(0)
      )
    } else {
      updateSelectizeInput(
        session, 
        "slide_districts",
        choices = c("All" = ""),
        selected = character(0)
      )
    }
  })
  
  # Reset slides when leaving the tab
  observeEvent(input$tabs, {
    if (input$tabs != "generate_slides") {
      # Reset slide state when leaving the tab
      slides_generated(FALSE)
      slide_analysis_results(list())
      current_slide_index(1)
    }
  })
  
  # Generate Slides Function
  observeEvent(input$generate_slides, {
    if (input$tabs != "generate_slides") return()
    
    showNotification("🔄 Generating analysis results for PowerPoint...", type = "message", duration = 5)
    
    safe_analysis({
      # Clear previous results
      slide_analysis_results(list())
      current_slide_index(1)
      slides_generated(FALSE)
      
      # Prepare common parameters
      common_params <- list()
      
      # Time parameters
      if (input$slide_time_type == "months") {
        common_params$x_months <- input$slide_months
      } else if (input$slide_time_type == "years" && input$slide_years != "") {
        common_params$year_selection <- as.numeric(strsplit(input$slide_years, ",")[[1]])
      }
      
      # Geographic parameters
      common_params$block_selection <- if (input$slide_block != "All") input$slide_block else NULL
      common_params$country_selection <- if (!is.null(input$slide_countries) && 
                                             length(input$slide_countries) > 0) input$slide_countries else NULL
      
      results <- list()
      
      # 1. SIA Scope Analysis
      if ("scope" %in% input$slide_analyses) {
        showNotification("Running Scope Analysis...", type = "message")
        scope_result <- tryCatch({
          do.call(generate_scope_plots_app, c(
            list(scope_data = scope),
            common_params,
            list(
              afro_blocks = afro_blocks,
              all_countries = all_countries,
              all_provinces = all_provinces,
              all_districts = all_districts,
              ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
            )
          ))
        }, error = function(e) {
          showNotification(paste("Scope Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(scope_result)) results$scope <- scope_result
      }
      
      # 2. Admin Data Overview
      if ("admin" %in% input$slide_analyses) {
        showNotification("Running Admin Overview...", type = "message")
        admin_result <- tryCatch({
          list(
            coverage = admin_coverage_summary(
              admin_data = admin_data,
              afro_blocks = afro_blocks,
              block_selection = common_params$block_selection,
              country_selection = common_params$country_selection,
              x_months = common_params$x_months,
              year_selection = common_params$year_selection
            ),
            vaccinated = admin_vaccinated_summary(
              admin_data = admin_data,
              afro_blocks = afro_blocks,
              block_selection = common_params$block_selection,
              country_selection = common_params$country_selection,
              x_months = common_params$x_months,
              year_selection = common_params$year_selection
            )
          )
        }, error = function(e) {
          showNotification(paste("Admin Overview failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(admin_result)) results$admin <- admin_result
      }
      
      # 3. Missed Children Analysis
      if ("missed" %in% input$slide_analyses) {
        showNotification("Running Missed Children Analysis...", type = "message")
        missed_result <- tryCatch({
          do.call(missed_children_disaggregated, c(
            list(data = dat),
            common_params
          ))
        }, error = function(e) {
          showNotification(paste("Missed Children Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(missed_result)) results$missed <- missed_result
      }
      
      # 4. Coverage Analysis
      if ("coverage" %in% input$slide_analyses) {
        showNotification("Running Coverage Analysis...", type = "message")
        coverage_result <- tryCatch({
          do.call(coverage_disaggregated, c(
            list(data = dat),
            common_params
          ))
        }, error = function(e) {
          showNotification(paste("Coverage Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(coverage_result)) results$coverage <- coverage_result
      }
      
      # 5. District Performance
      if ("district_perf" %in% input$slide_analyses) {
        showNotification("Running District Performance Analysis...", type = "message")
        district_result <- tryCatch({
          do.call(district_lqas_performance, c(
            list(data = dat),
            common_params,
            list(
              afro_blocks = afro_blocks,
              all_countries = all_countries,
              all_provinces = all_provinces,
              all_districts = all_districts,
              ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
            )
          ))
        }, error = function(e) {
          showNotification(paste("District Performance Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(district_result)) results$district_perf <- district_result
      }
      
      # 6. LQAS Summary Maps
      if ("lqas_maps" %in% input$slide_analyses) {
        showNotification("Running LQAS Summary Maps Analysis...", type = "message")
        lqas_result <- tryCatch({
          do.call(generate_lqas_summary_map_optimized, c(
            list(data = dat),
            common_params,
            list(
              all_countries = all_countries,
              all_provinces = all_provinces,
              all_districts = all_districts,
              afro_blocks = afro_blocks,
              ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS
            )
          ))
        }, error = function(e) {
          showNotification(paste("LQAS Summary Maps Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(lqas_result)) results$lqas_maps <- lqas_result
      }
      
      # 7. Reasons Analysis
      if ("reasons" %in% input$slide_analyses) {
        showNotification("Running Reasons Analysis...", type = "message")
        reasons_result <- tryCatch({
          do.call(reasons_heatmap_analysis, c(
            list(data = dat),
            common_params
          ))
        }, error = function(e) {
          showNotification(paste("Reasons Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(reasons_result)) results$reasons <- reasons_result
      }
      
      # 8. Unresolved Cases
      if ("unresolved" %in% input$slide_analyses) {
        showNotification("Running Unresolved Cases Analysis...", type = "message")
        unresolved_result <- tryCatch({
          do.call(generate_unresolved_NC_plots_DS_optimized, c(
            list(data = dat),
            common_params,
            list(
              all_countries = all_countries,
              all_provinces = all_provinces,
              all_districts = all_districts,
              afro_blocks = afro_blocks,
              ambiguous_district_pairs = AMBIGUOUS_DISTRICT_PAIRS,
              # "All" (the default) preserves the previous behavior of
              # building every reason; picking a specific reason here now
              # builds only that one -- both faster, and it makes the
              # "Show Specific Reason" dropdown actually take effect for
              # the slide preview above, which previously always showed
              # plots[[1]] regardless of this selection.
              selected_reason = if (!is.null(input$slide_unresolved_reason) &&
                                    !identical(input$slide_unresolved_reason, "All")) {
                input$slide_unresolved_reason
              } else NULL
            )
          ))
        }, error = function(e) {
          showNotification(paste("Unresolved Cases Analysis failed:", e$message), type = "warning")
          NULL
        })
        if (!is.null(unresolved_result)) results$unresolved <- unresolved_result
      }
      
      # Store results
      slide_analysis_results(results)
      slides_generated(TRUE)
      
      showNotification("✅ Analysis results ready for PowerPoint generation!", type = "message", duration = 5)
      
    }, "Slide Generation Analysis")
  })
  
  # Slide Preview UI
  output$slide_preview_ui <- renderUI({
    if (!slides_generated() || length(slide_analysis_results()) == 0) {
      return(
        tags$div(
          class = "alert alert-info",
          style = "text-align:center; padding:40px;",
          tags$h4("No slides generated yet"),
          tags$p("Configure your settings and click 'Generate PowerPoint Slides' to create your presentation."),
          tags$p("You can select which analyses to include and customize the output.")
        )
      )
    }
    
    current_index <- current_slide_index()
    results <- slide_analysis_results()
    analysis_names <- names(results)
    
    if (current_index > length(analysis_names)) {
      current_index <- 1
      current_slide_index(1)
    }
    
    current_analysis <- analysis_names[current_index]
    analysis_data <- results[[current_analysis]]
    
    # Create preview based on analysis type
    preview_content <- switch(
      current_analysis,
      scope = {
        if (!is.null(analysis_data$plot)) {
          tagList(
            tags$h4("SIA Scope Analysis - Map Preview"),
            plotOutput("slide_preview_scope_plot", height = "400px")
          )
        } else {
          tags$div(class = "alert alert-warning", "No map available for scope analysis")
        }
      },
      admin = {
        tagList(
          tags$h4("Admin Data Overview Preview"),
          tags$p("Coverage and vaccination summary tables will be included in the presentation."),
          fluidRow(
            column(6, uiOutput("slide_preview_admin_coverage")),
            column(6, uiOutput("slide_preview_admin_vaccinated"))
          )
        )
      },
      missed = {
        if (!is.null(analysis_data$flextable)) {
          tagList(
            tags$h4("Missed Children Analysis Preview"),
            uiOutput("slide_preview_missed_table")
          )
        } else {
          tags$div(class = "alert alert-warning", "No table available for missed children analysis")
        }
      },
      coverage = {
        if (!is.null(analysis_data$flextable)) {
          tagList(
            tags$h4("Coverage Analysis Preview"),
            uiOutput("slide_preview_coverage_table")
          )
        } else {
          tags$div(class = "alert alert-warning", "No table available for coverage analysis")
        }
      },
      district_perf = {
        if (!is.null(analysis_data$multiplot)) {
          tagList(
            tags$h4("District Performance Analysis - Map Preview"),
            plotOutput("slide_preview_district_plot", height = "400px")
          )
        } else {
          tags$div(class = "alert alert-warning", "No map available for district performance analysis")
        }
      },
      lqas_maps = {
        if (!is.null(analysis_data$map)) {
          tagList(
            tags$h4("LQAS Summary Maps - Preview"),
            plotOutput("slide_preview_lqas_plot", height = "400px")
          )
        } else {
          tags$div(class = "alert alert-warning", "No map available for LQAS summary analysis")
        }
      },
      reasons = {
        if (!is.null(analysis_data$plot)) {
          tagList(
            tags$h4("Reasons Analysis - Heatmap Preview"),
            plotOutput("slide_preview_reasons_plot", height = "400px")
          )
        } else {
          tags$div(class = "alert alert-warning", "No heatmap available for reasons analysis")
        }
      },
      unresolved = {
        if (!is.null(analysis_data$plots) && length(analysis_data$plots) > 0) {
          tagList(
            tags$h4("Unresolved Cases - Map Preview"),
            plotOutput("slide_preview_unresolved_plot", height = "400px")
          )
        } else {
          tags$div(class = "alert alert-warning", "No map available for unresolved cases analysis")
        }
      },
      tags$div(class = "alert alert-info", "Preview not available for this analysis type")
    )
    
    return(preview_content)
  })
  
  # Preview plots and tables
  output$slide_preview_scope_plot <- renderPlot({
    results <- slide_analysis_results()
    if (!is.null(results$scope$plot)) {
      results$scope$plot
    }
  })
  
  output$slide_preview_missed_table <- renderUI({
    results <- slide_analysis_results()
    if (!is.null(results$missed$flextable)) {
      div(class = "flextable-output", htmltools_value(results$missed$flextable))
    }
  })
  
  output$slide_preview_coverage_table <- renderUI({
    results <- slide_analysis_results()
    if (!is.null(results$coverage$flextable)) {
      div(class = "flextable-output", htmltools_value(results$coverage$flextable))
    }
  })
  
  output$slide_preview_district_plot <- renderPlot({
    results <- slide_analysis_results()
    if (!is.null(results$district_perf$multiplot)) {
      results$district_perf$multiplot
    }
  })
  
  output$slide_preview_lqas_plot <- renderPlot({
    results <- slide_analysis_results()
    if (!is.null(results$lqas_maps$map)) {
      results$lqas_maps$map
    }
  })
  
  output$slide_preview_reasons_plot <- renderPlot({
    results <- slide_analysis_results()
    if (!is.null(results$reasons$plot)) {
      results$reasons$plot
    }
  })
  
  output$slide_preview_unresolved_plot <- renderPlot({
    results <- slide_analysis_results()
    if (!is.null(results$unresolved$plots) && length(results$unresolved$plots) > 0) {
      # Show first available plot for preview
      results$unresolved$plots[[1]]
    }
  })
  
  output$slide_preview_admin_coverage <- renderUI({
    results <- slide_analysis_results()
    if (!is.null(results$admin$coverage$flextable)) {
      div(class = "flextable-output", style = "max-height: 300px; overflow-y: auto;", 
          htmltools_value(results$admin$coverage$flextable))
    }
  })
  
  output$slide_preview_admin_vaccinated <- renderUI({
    results <- slide_analysis_results()
    if (!is.null(results$admin$vaccinated$flextable)) {
      div(class = "flextable-output", style = "max-height: 300px; overflow-y: auto;", 
          htmltools_value(results$admin$vaccinated$flextable))
    }
  })
  
  # Slide navigation
  observeEvent(input$next_slide, {
    results <- slide_analysis_results()
    current_index <- current_slide_index()
    if (current_index < length(results)) {
      current_slide_index(current_index + 1)
    }
  })
  
  observeEvent(input$prev_slide, {
    current_index <- current_slide_index()
    if (current_index > 1) {
      current_slide_index(current_index - 1)
    }
  })
  
  output$slide_counter <- renderText({
    results <- slide_analysis_results()
    current_index <- current_slide_index()
    paste("Slide", current_index, "of", length(results))
  })
  
  # Enhanced PowerPoint Generation Function
  generate_powerpoint <- function() {
    req(slides_generated())
    results <- slide_analysis_results()
    
    showNotification("🔄 Creating PowerPoint presentation...", type = "message", duration = 10)
    
    tryCatch({
      # Create a new PowerPoint presentation
      pres <- officer::read_pptx()
      
      # Add title slide
      pres <- pres %>%
        officer::add_slide(layout = "Title Slide", master = "Office Theme") %>%
        officer::ph_with(value = input$slide_presentation_title, 
                         location = officer::ph_location_type(type = "ctrTitle")) %>%
        officer::ph_with(value = paste("Generated:", Sys.Date()), 
                         location = officer::ph_location_type(type = "subTitle")) %>%
        officer::ph_with(value = input$slide_author, 
                         location = officer::ph_location_type(type = "dt"))
      
      # ... rest of the PowerPoint generation code remains the same ...
      
      return(pres)
      
    }, error = function(e) {
      showNotification(paste("PowerPoint generation failed:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Download handler for PowerPoint
  output$download_slides <- downloadHandler(
    filename = function() {
      paste0("SIA_Analysis_Presentation_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      withProgress(
        message = 'Generating PowerPoint file...',
        detail = 'This may take a few moments...',
        value = 0.3,
        {
          pres <- generate_powerpoint()
          incProgress(0.7, detail = "Saving presentation...")
          
          if (!is.null(pres)) {
            print(pres, target = file)
            incProgress(1, detail = "Download ready!")
            showNotification("✅ PowerPoint presentation generated successfully!", type = "message")
          } else {
            showNotification("❌ Failed to generate PowerPoint", type = "error")
          }
        }
      )
    }
  )
  
  # Conditional panel output
  output$slides_generated <- reactive({
    slides_generated()
  })
  outputOptions(output, "slides_generated", suspendWhenHidden = FALSE)
  
  
  
  # ---- Go Home Handler (for logo click) ----
  observeEvent(input$go_home, {
    updateTabItems(session, "tabs", "overview")
  })
  
  # ============================================================
  # 🐛 DEBUG: Monitor busy states (Remove in production)
  # ============================================================
  
  output$busy_debug <- renderPrint({
    cat("Active reactives:\n")
    cat("- overview_filtered:", ifelse(!is.null(overview_filtered()), "ACTIVE", "INACTIVE"), "\n")
    cat("- scope_analysis:", ifelse(!is.null(scope_analysis()), "ACTIVE", "INACTIVE"), "\n")
    cat("- Current tab:", input$tabs, "\n")
    cat("- Memory usage:", format(utils::object.size(x = ls(envir = .GlobalEnv)), units = "MB"), "\n")
  })
  
  # Final cleanup
  onStop(function() {
    message("Shiny app stopped - cleaning up memory")
    force_gc()
  })
  
} # End of server function

# ============================================================
# Run the application
# ============================================================
shinyApp(ui, server)








# ============================================================
# WHO-AFRO – Auto-Publish Dashboard Script
# (Lightweight deployment-only version)
# Author: Gorgui Ba TOURE
# ============================================================

# ---- Packages ----
if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
library(rsconnect)

# ---- Paths & Config ----
dashboard_path <- "C:/Users/TOURE/Documents/AFRO-SIA_Dashboard"
app_name       <- "WHO-AFRO_SIA_Dashboard"
account_name   <- "afropepdim"   # or who-afro / gorguiba1 as fallback

# ---- Exclude local files from upload (.rscignore alternative) ----
exclude_patterns <- c(
  "auto_publish_dashboard.R",
  "auto_fetch_data_wrapper.R",
  "auto_fetch_data_wrapper.log",
  "auto_fetch_data.bat",
  "*.log",
  "*.zip",
  "*.csv",
  "*.xlsx",
  "*.xlsm",
  "backup/",
  "outputs/",
  "temp/",
  "__MACOSX/",
  "africa_countries.rds",
  "africa_provinces.rds",
  "africa_districts.rds"
)
rsc_path <- file.path(dashboard_path, ".rscignore")
if (file.exists(rsc_path)) {
  current <- readLines(rsc_path, warn = FALSE)
  add <- setdiff(exclude_patterns, current)
  if (length(add)) writeLines(c(current, add), rsc_path)
} else {
  writeLines(exclude_patterns, rsc_path)
}

# ---- Deployment ----
message("🚀 Starting WHO-AFRO SIA Dashboard deployment…")

rsconnect::setAccountInfo(
  name   = account_name,
  token  = Sys.getenv("AFROPEPDIM_TOKEN"),
  secret = Sys.getenv("AFROPEPDIM_SECRET")
)

rsconnect::deployApp(
  appDir        = dashboard_path,
  appName       = app_name,
  account       = account_name,
  logLevel      = "normal",
  launch.browser = FALSE,
  forceUpdate   = TRUE
)

message("✅ Deployment completed successfully for ", app_name)

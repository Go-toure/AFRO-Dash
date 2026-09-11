required_files <- c("app.R", "renv.lock")

missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  stop("Missing required files: ", paste(missing, collapse = ", "))
}

message("Basic file check passed.")

# Optional bundle-size check
all_files <- list.files(".", recursive = TRUE, full.names = TRUE, all.files = FALSE)
all_files <- all_files[file.exists(all_files)]
bundle_size_mb <- sum(file.info(all_files)$size, na.rm = TRUE) / (1024^2)
message(sprintf("Approx project size: %.1f MB", bundle_size_mb))

if (bundle_size_mb > 250) {
  warning(sprintf("Project is large (%.1f MB). Consider excluding raw data/shapefiles from deployment bundle.", bundle_size_mb))
}

# Startup smoke test
options(shiny.testmode = TRUE)

suppressPackageStartupMessages({
  library(shiny)
})

message("Testing app load...")

app_obj <- shiny::shinyAppDir(
  appDir = ".",
  options = list(
    launch.browser = FALSE,
    port = 9999,
    host = "127.0.0.1"
  )
)

if (is.null(app_obj$ui) || is.null(app_obj$server)) {
  stop("App object did not load correctly.")
}

message("Shiny app loads successfully.")
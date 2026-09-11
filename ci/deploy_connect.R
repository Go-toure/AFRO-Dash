server_url <- Sys.getenv("CONNECT_SERVER")
api_key    <- Sys.getenv("CONNECT_API_KEY")
account    <- Sys.getenv("CONNECT_ACCOUNT")
app_name   <- Sys.getenv("CONNECT_APP_NAME")

if (server_url == "" || api_key == "" || account == "" || app_name == "") {
  stop("Missing one or more required environment variables: CONNECT_SERVER, CONNECT_API_KEY, CONNECT_ACCOUNT, CONNECT_APP_NAME")
}

suppressPackageStartupMessages({
  library(rsconnect)
})

# Register server + API user for this CI runner
server_name <- "posit-connect-ci"

try(rsconnect::addServer(url = server_url, name = server_name), silent = TRUE)
try(
  rsconnect::connectApiUser(
    server = server_name,
    apiKey = api_key,
    account = account
  ),
  silent = TRUE
)

# Deploy app directory
rsconnect::deployApp(
  appDir = ".",
  appName = app_name,
  account = account,
  server = server_name,
  launch.browser = FALSE,
  forceUpdate = TRUE
)
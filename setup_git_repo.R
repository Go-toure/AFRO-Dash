# ===============================================================
# WHO AFRO – Auto Git Setup and Sync Script (Full + Email Retry)
# Author: Gorgui Ba TOURE
# ===============================================================
# Features:
#   ✅ Auto-detects and commits changes
#   ✅ Pushes to GitHub only when needed
#   ✅ Creates & updates logs/git_sync_log.csv
#   ✅ Sends email summary to gorguiba1@gmail.com
#   ✅ Retries email once if sending fails
# ===============================================================

library(blastula)
library(keyring)

# 1️⃣ Working directory
setwd("C:/Users/TOURE/Documents/AFRO-SIA_Dashboard")

# 2️⃣ Git identity
system('git config --global user.name "Go-toure"')
system('git config --global user.email "gorguiba1@gmail.com"')

# 3️⃣ Initialize Git if needed
if (!dir.exists(".git")) {
  system("git init")
  message("✅ Git repository initialized.")
} else {
  message("ℹ️ Git repository already exists.")
}

# 4️⃣ Stage all project files
system("git add .")

# 5️⃣ Detect pending changes
changes <- system('git status --porcelain', intern = TRUE)

# --- Helper function to send email (with retry) ---
send_git_email <- function(status_msg, push_status, summary_table = NULL, attempt = 1) {
  email <- compose_email(
    body = md(glue::glue("
    **WHO AFRO SIA Dashboard – Git Sync Notification**

    Status: {push_status}  
    Message: {status_msg}  
    Timestamp: {format(Sys.time(), '%Y-%m-%d %H:%M')}

    ---
    {if (!is.null(summary_table)) knitr::kable(summary_table) else ''}
    "))
  )
  
  tryCatch(
    {
      smtp_send(
        email,
        from = "gorguiba1@gmail.com",
        to = "gorguiba1@gmail.com",
        subject = paste("Git Sync – AFRO-SIA Dashboard:", push_status),
        credentials = creds_key("gmail_creds")
      )
      message("✅ Email notification sent successfully.")
    },
    error = function(e) {
      if (attempt == 1) {
        message("⚠️ Email failed to send. Retrying in 10 seconds...")
        Sys.sleep(10)
        send_git_email(status_msg, push_status, summary_table, attempt = 2)
      } else {
        message("❌ Email failed twice. Check internet or Gmail credentials.")
      }
    }
  )
}

# --- Git workflow ---
if (length(changes) == 0) {
  message("✅ No changes detected — working tree clean.")
  log_entry <- data.frame(
    DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
    Added = 0, Modified = 0, Deleted = 0, Renamed = 0, Other = 0,
    Push_Status = "No changes", stringsAsFactors = FALSE
  )
  send_git_email("No new commits detected.", "No changes")
} else {
  added     <- sum(grepl("^A", changes))
  modified  <- sum(grepl("^ M", changes))
  deleted   <- sum(grepl("^ D", changes))
  renamed   <- sum(grepl("^R", changes))
  others    <- length(changes) - (added + modified + deleted + renamed)
  
  commit_msg <- paste0("Auto commit: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  system(paste('git commit -m', shQuote(commit_msg)))
  
  system("git branch -M main")
  system('git remote set-url origin https://github.com/Go-toure/AFRO-SIA_Dashboard.git')
  
  push_result <- system("git push -u origin main")
  push_status <- if (push_result == 0) "✅ Successful" else "⚠️ Failed"
  
  summary_tbl <- data.frame(
    Metric = c("Added", "Modified", "Deleted", "Renamed", "Other", "Push Status", "Timestamp"),
    Count  = c(added, modified, deleted, renamed, others, push_status, format(Sys.time(), "%Y-%m-%d %H:%M")),
    stringsAsFactors = FALSE
  )
  
  cat("\n==============================\n")
  cat(" GIT SYNC SUMMARY – AFRO-SIA Dashboard\n")
  cat("==============================\n\n")
  print(summary_tbl, row.names = FALSE)
  cat("\n🚀 Git sync completed!\n")
  
  log_entry <- data.frame(
    DateTime = format(Sys.time(), "%Y-%m-%d %H:%M"),
    Added = added, Modified = modified, Deleted = deleted,
    Renamed = renamed, Other = others, Push_Status = push_status,
    stringsAsFactors = FALSE
  )
  
  send_git_email("Auto-sync completed successfully.", push_status, summary_tbl)
}

# 6️⃣ Ensure logs folder
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir)

# 7️⃣ Append to log CSV
log_path <- file.path(log_dir, "git_sync_log.csv")

if (file.exists(log_path)) {
  old_log <- tryCatch(read.csv(log_path, stringsAsFactors = FALSE), error = function(e) NULL)
  new_log <- if (is.data.frame(old_log)) rbind(old_log, log_entry) else log_entry
} else {
  new_log <- log_entry
}
write.csv(new_log, log_path, row.names = FALSE)

# 8️⃣ Commit and push the log
system(paste("git add", shQuote(log_path)))
system(paste('git commit -m', shQuote(paste0("Updated git_sync_log.csv: ", format(Sys.time(), "%Y-%m-%d %H:%M")))))
system("git push -u origin main")

# 9️⃣ Show latest commit summary
system("git log -1 --oneline")




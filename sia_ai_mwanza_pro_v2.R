# ============================================================
# 🤖 TRULY SMART AI ASSISTANT - MWANZA PRO (PUBLIC-SAFE v2)
# ============================================================
# ✅ Public-safe: XSS protection, rate limiting, strict tool-call JSON parsing
# ✅ Keeps your design: SmartAgent (R6) + Shiny module UI/server
# ✅ Fixes: missing %||%, fragile JSON regex, safer init, better prompts
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(R6)
  library(jsonlite)
  library(ellmer)
  library(htmltools)
})

# ============================================================
# ✅ HELPERS (PUBLIC SAFE)
# ============================================================

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

escape_html <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# Session-only rate limiter (good baseline for public apps)
rate_limiter <- local({
  env <- new.env(parent = emptyenv())
  function(session_id, max_per_min = 12) {
    now <- as.numeric(Sys.time())
    key <- paste0("rl_", session_id)
    times <- env[[key]]
    if (is.null(times)) times <- numeric(0)
    times <- times[times > (now - 60)]
    if (length(times) >= max_per_min) return(FALSE)
    env[[key]] <- c(times, now)
    TRUE
  }
})

parse_tool_call_strict <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(NULL)
  
  # Require the whole response to be JSON
  if (!grepl("^\\{[\\s\\S]*\\}$", x)) return(NULL)
  
  obj <- tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  
  if (is.null(obj$tool) || is.null(obj$args)) return(NULL)
  if (!is.character(obj$tool) || length(obj$tool) != 1) return(NULL)
  if (!is.list(obj$args)) return(NULL)
  
  obj
}

# ============================================================
# 🧠 SMART AGENT CLASS
# ============================================================

SmartAgent <- R6Class(
  "SmartAgent",
  public = list(
    llm_client = NULL,
    tools = list(),
    memory = list(),
    max_memory = 20,
    using_llm = FALSE,
    quota_exceeded = FALSE,
    data_sources = NULL,
    block_definitions = NULL,
    
    initialize = function(llm_client = NULL, block_definitions = NULL) {
      self$llm_client <- llm_client
      self$using_llm <- !is.null(llm_client)
      self$block_definitions <- block_definitions
      self$register_tools()
    },
    
    # Register all available tools with their schemas
    register_tools = function() {
      self$tools <- list(
        list(
          name = "get_missed_children",
          description = "Get missed children rates by country, block, or compare entities",
          parameters = list(
            type = "object",
            properties = list(
              country = list(type = "string", description = "Country name (e.g., Nigeria, Cameroon)"),
              block = list(type = "string", description = "Block name (LCB, ESA, WA, DRC, ECA)"),
              months = list(type = "integer", description = "Months to look back", default = 6),
              compare = list(type = "array", items = list(type = "string"), description = "Blocks/countries to compare")
            )
          ),
          handler = self$tool_get_missed
        ),
        list(
          name = "get_coverage",
          description = "Get vaccination coverage rates (LQAS or administrative)",
          parameters = list(
            type = "object",
            properties = list(
              country = list(type = "string"),
              block = list(type = "string"),
              months = list(type = "integer", default = 6),
              type = list(type = "string", enum = c("lqas", "administrative"), default = "lqas")
            )
          ),
          handler = self$tool_get_coverage
        ),
        list(
          name = "get_reasons",
          description = "Get reasons for missed vaccinations (summary metrics)",
          parameters = list(
            type = "object",
            properties = list(
              country = list(type = "string"),
              months = list(type = "integer", default = 6),
              top_n = list(type = "integer", default = 5)
            )
          ),
          handler = self$tool_get_reasons
        ),
        list(
          name = "get_district_performance",
          description = "Get district performance classification metrics",
          parameters = list(
            type = "object",
            properties = list(
              country = list(type = "string"),
              block = list(type = "string"),
              months = list(type = "integer", default = 6),
              performance_type = list(type = "string",
                                      enum = c("all", "always_high", "never_high", "improved", "declined"),
                                      default = "all")
            )
          ),
          handler = self$tool_get_district_performance
        ),
        list(
          name = "get_admin_summary",
          description = "Get administrative data summary (coverage and/or doses)",
          parameters = list(
            type = "object",
            properties = list(
              country = list(type = "string"),
              months = list(type = "integer", default = 12),
              metric = list(type = "string", enum = c("coverage", "doses", "both"), default = "both")
            )
          ),
          handler = self$tool_get_admin_summary
        ),
        list(
          name = "get_anomalies",
          description = "Detect anomalies in administrative coverage data",
          parameters = list(
            type = "object",
            properties = list(
              threshold = list(type = "number", default = 2)
            )
          ),
          handler = self$tool_get_anomalies
        ),
        list(
          name = "get_trend",
          description = "Get simple trend over time for a metric",
          parameters = list(
            type = "object",
            properties = list(
              metric = list(type = "string", enum = c("missed", "coverage", "doses")),
              country = list(type = "string"),
              months = list(type = "integer", default = 12)
            )
          ),
          handler = self$tool_get_trend
        ),
        list(
          name = "get_scope_summary",
          description = "Get SIA scope summary (overall summary table)",
          parameters = list(
            type = "object",
            properties = list(
              block = list(type = "string"),
              year = list(type = "integer")
            )
          ),
          handler = self$tool_get_scope_summary
        )
      )
    },
    
    set_data = function(data_sources) {
      self$data_sources <- data_sources
    },
    
    add_to_memory = function(role, content) {
      self$memory <- c(self$memory, list(list(role = role, content = content)))
      if (length(self$memory) > self$max_memory) {
        self$memory <- tail(self$memory, self$max_memory)
      }
    },
    
    get_context = function() {
      if (length(self$memory) == 0) return("")
      context <- "Previous conversation:\n"
      for (i in seq_len(min(5, length(self$memory)))) {
        m <- self$memory[[length(self$memory) - i + 1]]
        context <- paste0(context, toupper(m$role), ": ", m$content, "\n")
      }
      context
    },
    
    # ============================================================
    # TOOL IMPLEMENTATIONS (REAL DATA)
    # ============================================================
    
    tool_get_missed = function(args) {
      fn <- get0("missed_children_disaggregated", mode = "function")
      if (!is.function(fn)) return(list(error = "Function missed_children_disaggregated() not available"))
      
      # Comparison mode
      if (!is.null(args$compare) && length(args$compare) > 1) {
        results <- list()
        
        for (entity in args$compare) {
          countries <- entity
          if (!is.null(self$block_definitions)) {
            if (entity %in% names(self$block_definitions$afro)) {
              countries <- self$block_definitions$afro[[entity]]
            } else if (entity %in% names(self$block_definitions$ist)) {
              countries <- self$block_definitions$ist[[entity]]
            }
          }
          
          res <- fn(
            data = self$data_sources$main_data,
            x_months = args$months %||% 6,
            country_selection = countries,
            afro_blocks = self$block_definitions$afro,
            ist_blocks  = self$block_definitions$ist
          )
          
          if (!is.null(res$overall_missed)) {
            results[[entity]] <- list(
              boys  = round(res$overall_missed$perc_m, 1),
              girls = round(res$overall_missed$perc_f, 1),
              avg   = round((res$overall_missed$perc_m + res$overall_missed$perc_f) / 2, 1)
            )
          }
        }
        
        if (length(results) > 0) return(list(success = TRUE, type = "comparison", data = results))
      }
      
      # Single query
      country_sel <- args$country %||% args$block
      if (!is.null(args$block) && !is.null(self$block_definitions) &&
          args$block %in% names(self$block_definitions$afro)) {
        country_sel <- self$block_definitions$afro[[args$block]]
      }
      
      result <- fn(
        data = self$data_sources$main_data,
        x_months = args$months %||% 6,
        country_selection = country_sel,
        afro_blocks = self$block_definitions$afro,
        ist_blocks  = self$block_definitions$ist
      )
      
      if (!is.null(result$overall_missed)) {
        m <- result$overall_missed
        return(list(
          success = TRUE,
          data = list(
            boys  = round(m$perc_m, 1),
            girls = round(m$perc_f, 1),
            avg   = round((m$perc_m + m$perc_f) / 2, 1)
          )
        ))
      }
      
      list(error = "No data found")
    },
    
    tool_get_coverage = function(args) {
      type <- args$type %||% "lqas"
      
      if (identical(type, "administrative")) {
        fn <- get0("admin_coverage_summary", mode = "function")
        if (!is.function(fn)) return(list(error = "Function admin_coverage_summary() not available"))
        
        result <- fn(
          admin_data = self$data_sources$admin_data,
          x_months = args$months %||% 6,
          country_selection = args$country,
          afro_blocks = self$block_definitions$afro,
          ist_blocks  = self$block_definitions$ist
        )
        
        if (!is.null(result$metrics)) {
          return(list(
            success = TRUE,
            data = list(
              avg_coverage = round(result$metrics$avg_coverage, 1),
              pct_ge95     = round(result$metrics$pct_ge95, 1),
              n_countries  = result$metrics$n_countries
            )
          ))
        }
        
      } else {
        fn <- get0("coverage_disaggregated", mode = "function")
        if (!is.function(fn)) return(list(error = "Function coverage_disaggregated() not available"))
        
        result <- fn(
          data = self$data_sources$main_data,
          x_months = args$months %||% 6,
          country_selection = args$country,
          afro_blocks = self$block_definitions$afro,
          ist_blocks  = self$block_definitions$ist
        )
        
        if (!is.null(result$coverage_data)) {
          return(list(
            success = TRUE,
            data = list(
              boys  = round(result$coverage_data$perc_m, 1),
              girls = round(result$coverage_data$perc_f, 1),
              avg   = round((result$coverage_data$perc_m + result$coverage_data$perc_f) / 2, 1)
            )
          ))
        }
      }
      
      list(error = "No data found")
    },
    
    tool_get_reasons = function(args) {
      fn <- get0("reasons_heatmap_analysis", mode = "function")
      if (!is.function(fn)) return(list(error = "Function reasons_heatmap_analysis() not available"))
      
      result <- fn(
        data = self$data_sources$main_data,
        x_months = args$months %||% 6,
        country_selection = args$country,
        afro_blocks = self$block_definitions$afro,
        ist_blocks  = self$block_definitions$ist
      )
      
      if (!is.null(result$metrics)) {
        return(list(
          success = TRUE,
          data = list(
            top_reason       = result$metrics$most_common_reason,
            avg_pct          = round(result$metrics$avg_percentage, 1),
            total_countries  = result$metrics$total_countries
          )
        ))
      }
      
      list(error = "No data found")
    },
    
    tool_get_district_performance = function(args) {
      fn <- get0("district_lqas_performance", mode = "function")
      if (!is.function(fn)) return(list(error = "Function district_lqas_performance() not available"))
      
      result <- fn(
        data = self$data_sources$main_data,
        x_months = args$months %||% 6,
        country_selection = args$country,
        afro_blocks = self$block_definitions$afro,
        ist_blocks  = self$block_definitions$ist,
        all_countries  = self$data_sources$all_countries,
        all_provinces  = self$data_sources$all_provinces,
        all_districts  = self$data_sources$all_districts
      )
      
      if (!is.null(result$summary_metrics)) {
        m <- result$summary_metrics
        return(list(
          success = TRUE,
          data = list(
            total      = m$total_districts,
            always_high = m$high_performing_districts,
            never_high  = m$poor_performing_districts,
            high_pct = round(m$high_performing_districts / m$total_districts * 100, 1),
            poor_pct = round(m$poor_performing_districts / m$total_districts * 100, 1)
          )
        ))
      }
      
      list(error = "No data found")
    },
    
    tool_get_admin_summary = function(args) {
      results <- list()
      metric <- args$metric %||% "both"
      
      if (metric %in% c("coverage", "both")) {
        fn_cov <- get0("admin_coverage_summary", mode = "function")
        if (is.function(fn_cov)) {
          cov <- fn_cov(
            admin_data = self$data_sources$admin_data,
            x_months = args$months %||% 12,
            country_selection = args$country,
            afro_blocks = self$block_definitions$afro,
            ist_blocks  = self$block_definitions$ist
          )
          if (!is.null(cov$metrics)) {
            results$coverage <- list(
              avg = round(cov$metrics$avg_coverage, 1),
              pct_ge95 = round(cov$metrics$pct_ge95, 1)
            )
          }
        }
      }
      
      if (metric %in% c("doses", "both")) {
        fn_doses <- get0("admin_vaccinated_summary", mode = "function")
        if (is.function(fn_doses)) {
          doses <- fn_doses(
            admin_data = self$data_sources$admin_data,
            x_months = args$months %||% 12,
            country_selection = args$country,
            afro_blocks = self$block_definitions$afro,
            ist_blocks  = self$block_definitions$ist
          )
          if (!is.null(doses$metrics)) {
            results$doses <- list(
              total    = round(doses$metrics$total_dose_administrated, 1),
              children = round(doses$metrics$total_children_vaccinated, 1)
            )
          }
        }
      }
      
      if (length(results) > 0) return(list(success = TRUE, data = results))
      list(error = "No data found")
    },
    
    tool_get_anomalies = function(args) {
      df <- self$data_sources$admin_data
      if (is.null(df) || !"CVPolio" %in% names(df)) {
        return(list(error = "No administrative data available (CVPolio not found)"))
      }
      
      threshold <- args$threshold %||% 2
      df_clean <- df[!is.na(df$CVPolio), , drop = FALSE]
      if (nrow(df_clean) == 0) return(list(error = "No non-missing CVPolio records"))
      
      above_100 <- df_clean[df_clean$CVPolio > 100, , drop = FALSE]
      below_0   <- df_clean[df_clean$CVPolio < 0,   , drop = FALSE]
      
      z_scores <- as.numeric(scale(as.numeric(df_clean$CVPolio)))
      outliers <- df_clean[abs(z_scores) > threshold & !is.na(z_scores), , drop = FALSE]
      
      examples <- head(outliers[, intersect(c("Country", "District", "CVPolio"), names(outliers)), drop = FALSE], 5)
      examples_list <- list()
      if (nrow(examples) > 0 && all(c("Country", "District", "CVPolio") %in% names(examples))) {
        for (i in seq_len(nrow(examples))) {
          examples_list[[i]] <- list(
            country = as.character(examples$Country[i]),
            district = as.character(examples$District[i]),
            value = round(examples$CVPolio[i], 1)
          )
        }
      }
      
      list(
        success = TRUE,
        data = list(
          above_100 = nrow(above_100),
          below_0   = nrow(below_0),
          outliers  = nrow(outliers),
          examples  = examples_list
        )
      )
    },
    
    tool_get_trend = function(args) {
      metric <- args$metric %||% "missed"
      
      if (identical(metric, "missed")) {
        fn <- get0("missed_children_disaggregated", mode = "function")
        if (!is.function(fn)) return(list(error = "Function missed_children_disaggregated() not available"))
        
        months_total <- args$months %||% 12
        country <- args$country
        
        trends <- list()
        for (m in c(3, 6, 12)) {
          if (m <= months_total) {
            res <- fn(
              data = self$data_sources$main_data,
              x_months = m,
              country_selection = country,
              afro_blocks = self$block_definitions$afro,
              ist_blocks  = self$block_definitions$ist
            )
            if (!is.null(res$overall_missed)) {
              avg <- (res$overall_missed$perc_m + res$overall_missed$perc_f) / 2
              trends[[paste0(m, "m")]] <- round(avg, 1)
            }
          }
        }
        
        if (length(trends) > 0) {
          values <- unlist(trends)
          direction <- if (length(values) >= 2) {
            if (values[length(values)] < values[1]) "improving" else "worsening"
          } else "stable"
          
          return(list(
            success = TRUE,
            data = list(
              trend = trends,
              direction = direction,
              current = values[length(values)]
            )
          ))
        }
      }
      
      list(error = "Trend analysis not available for this metric yet")
    },
    
    tool_get_scope_summary = function(args) {
      fn <- get0("generate_scope_plots_app", mode = "function")
      if (!is.function(fn)) return(list(error = "Function generate_scope_plots_app() not available"))
      
      countries <- NULL
      if (!is.null(args$block) && !is.null(self$block_definitions) &&
          args$block %in% names(self$block_definitions$afro)) {
        countries <- self$block_definitions$afro[[args$block]]
      }
      
      result <- fn(
        scope_data = self$data_sources$scope_data,
        year_selection = args$year,
        country_selection = countries,
        afro_blocks = self$block_definitions$afro,
        ist_blocks  = self$block_definitions$ist,
        all_countries = self$data_sources$all_countries,
        all_provinces = self$data_sources$all_provinces,
        all_districts = self$data_sources$all_districts
      )
      
      if (!is.null(result$data_summary) && !is.null(result$data_summary$overall_summary)) {
        return(list(success = TRUE, data = result$data_summary$overall_summary))
      }
      
      list(error = "No scope data found")
    },
    
    # ============================================================
    # PROCESS WITH OPENAI (STRICT TOOL JSON)
    # ============================================================
    
    process_with_llm = function(message) {
      if (!self$using_llm || self$quota_exceeded) return(NULL)
      
      tryCatch({
        tools_desc <- paste(
          vapply(self$tools, function(t) {
            paste0(
              "- ", t$name, ": ", t$description, "\n",
              "  Parameters: ", toJSON(t$parameters, auto_unbox = TRUE)
            )
          }, character(1)),
          collapse = "\n\n"
        )
        
        system_prompt <- paste0(
          "You are MWANZA Pro, an assistant for the WHO AFRO SIA Dashboard.\n",
          "You MUST use tools to get real numbers. Never invent any data.\n\n",
          "IMPORTANT TOOL RULE:\n",
          "If you decide to use a tool, respond with ONLY a JSON object and nothing else.\n",
          "Format exactly: {\"tool\":\"tool_name\",\"args\":{...}}\n",
          "If you need clarification, ask ONE short question (no JSON).\n\n",
          "Available tools:\n", tools_desc, "\n\n",
          "Public dashboard policy:\n",
          "- Do not request or output individual-level data.\n",
          "- Provide aggregated indicators only.\n"
        )
        
        context <- self$get_context()
        
        response <- self$llm_client$chat(paste(
          system_prompt,
          context,
          paste0("User query: ", message),
          sep = "\n\n"
        ))
        
        tool_call <- parse_tool_call_strict(response)
        
        if (!is.null(tool_call)) {
          tool <- NULL
          for (t in self$tools) {
            if (identical(t$name, tool_call$tool)) { tool <- t; break }
          }
          
          if (!is.null(tool)) {
            result <- tool$handler(tool_call$args)
            
            final_prompt <- paste0(
              "User query: ", message, "\n\n",
              "Tool result JSON:\n", toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n\n",
              "Write a concise, friendly response with the real numbers from the tool result. ",
              "Use short bullets when useful. Emojis are okay but not too many."
            )
            
            return(self$llm_client$chat(final_prompt))
          }
        }
        
        # If LLM didn't output strict JSON, treat it as plain response
        response
        
      }, error = function(e) {
        if (grepl("insufficient_quota|quota", e$message, ignore.case = TRUE)) {
          self$quota_exceeded <- TRUE
          warning("OpenAI quota exceeded - switching to fallback mode")
        } else {
          warning("LLM processing failed: ", e$message)
        }
        NULL
      })
    },
    
    # ============================================================
    # FALLBACK RULE-BASED PROCESSING
    # ============================================================
    
    process_fallback = function(message) {
      message_lower <- tolower(message %||% "")
      
      # Months extraction
      months <- 6
      if (grepl("last (\\d+) months?", message_lower)) {
        months <- suppressWarnings(as.numeric(gsub(".*last (\\d+).*", "\\1", message_lower)))
        if (is.na(months)) months <- 6
      }
      if (grepl("last year", message_lower)) months <- 12
      
      # Extract country (best-effort)
      country <- NULL
      if (!is.null(self$data_sources$main_data) && "country" %in% names(self$data_sources$main_data)) {
        countries <- unique(as.character(self$data_sources$main_data$country))
        countries <- countries[!is.na(countries) & nzchar(countries)]
        for (c in countries) {
          if (grepl(tolower(c), message_lower, fixed = TRUE)) {
            country <- c
            break
          }
        }
      }
      
      # Compare blocks/countries (simple)
      if (grepl("compare", message_lower) &&
          grepl("(lcb|esa|wa|drc|eca)", message_lower)) {
        blocks <- unlist(regmatches(message_lower, gregexpr("(lcb|esa|wa|drc|eca)", message_lower)))
        blocks <- unique(toupper(blocks))
        if (length(blocks) >= 2) {
          result <- self$tool_get_missed(list(compare = blocks[1:2], months = months))
          if (!is.null(result$success) && result$success) {
            response <- paste0("📊 Missed children comparison (last ", months, " months):\n\n")
            for (b in names(result$data)) {
              d <- result$data[[b]]
              response <- paste0(response, "• ", b, ": ", d$avg, "% (boys: ", d$boys, "%, girls: ", d$girls, "%)\n")
            }
            return(response)
          }
        }
      }
      
      # Missed children
      if (grepl("missed|not vaccinated|unvaccinated", message_lower) && !is.null(country)) {
        result <- self$tool_get_missed(list(country = country, months = months))
        if (!is.null(result$success) && result$success) {
          d <- result$data
          status <- if (d$boys <= 2 && d$girls <= 2) "✅ Meets target" else "⚠️ Above target"
          return(paste0(
            "📊 Missed children in ", country, " (last ", months, " months):\n\n",
            "• Boys: ", d$boys, "%\n",
            "• Girls: ", d$girls, "%\n",
            "• Average: ", d$avg, "%\n\n",
            status
          ))
        }
      }
      
      # Coverage (LQAS)
      if (grepl("coverage", message_lower) &&
          !grepl("administrative|admin", message_lower) &&
          !is.null(country)) {
        result <- self$tool_get_coverage(list(country = country, months = months, type = "lqas"))
        if (!is.null(result$success) && result$success) {
          d <- result$data
          return(paste0(
            "📈 Coverage in ", country, " (last ", months, " months):\n\n",
            "• Boys: ", d$boys, "%\n",
            "• Girls: ", d$girls, "%\n",
            "• Average: ", d$avg, "%"
          ))
        }
      }
      
      # Reasons
      if (grepl("reason|why|non-compliance", message_lower)) {
        args <- list(months = months)
        if (!is.null(country)) args$country <- country
        result <- self$tool_get_reasons(args)
        if (!is.null(result$success) && result$success) {
          d <- result$data
          location <- if (!is.null(country)) paste0(" in ", country) else ""
          return(paste0(
            "🔍 Reasons for missed", location, " (last ", months, " months):\n\n",
            "• Most common: ", d$top_reason, "\n",
            "• Average: ", d$avg_pct, "%\n",
            "• Countries analyzed: ", d$total_countries
          ))
        }
      }
      
      # District performance
      if (grepl("district.*performance|high performing|low performing|poor performing", message_lower)) {
        args <- list(months = months)
        if (!is.null(country)) args$country <- country
        result <- self$tool_get_district_performance(args)
        if (!is.null(result$success) && result$success) {
          d <- result$data
          location <- if (!is.null(country)) paste0(" in ", country) else ""
          return(paste0(
            "🗺️ District performance", location, " (last ", months, " months):\n\n",
            "• Total districts: ", format(d$total, big.mark = ","), "\n",
            "• Always high: ", format(d$always_high, big.mark = ","), " (", d$high_pct, "%)\n",
            "• Never high: ", format(d$never_high, big.mark = ","), " (", d$poor_pct, "%)\n\n",
            "Check the District Performance tab for maps."
          ))
        }
      }
      
      # Administrative coverage
      if (grepl("administrative coverage|admin coverage", message_lower)) {
        args <- list(months = months, metric = "coverage")
        if (!is.null(country)) args$country <- country
        result <- self$tool_get_admin_summary(args)
        if (!is.null(result$success) && result$success && !is.null(result$data$coverage)) {
          d <- result$data$coverage
          location <- if (!is.null(country)) paste0(" in ", country) else ""
          return(paste0(
            "📋 Administrative coverage", location, " (last ", months, " months):\n\n",
            "• Average: ", d$avg, "%\n",
            "• Districts ≥95%: ", d$pct_ge95, "%"
          ))
        }
      }
      
      # Doses
      if (grepl("doses?|vaccinated|administered", message_lower)) {
        args <- list(months = months, metric = "doses")
        if (!is.null(country)) args$country <- country
        result <- self$tool_get_admin_summary(args)
        if (!is.null(result$success) && result$success && !is.null(result$data$doses)) {
          d <- result$data$doses
          location <- if (!is.null(country)) paste0(" in ", country) else ""
          return(paste0(
            "💉 Doses administered", location, " (last ", months, " months):\n\n",
            "• Total doses: ", format(d$total, big.mark = ","), " million\n",
            "• Children vaccinated: ", format(d$children, big.mark = ","), " million"
          ))
        }
      }
      
      # Anomalies
      if (grepl("anomalies|outliers", message_lower)) {
        result <- self$tool_get_anomalies(list())
        if (!is.null(result$success) && result$success) {
          d <- result$data
          response <- paste0(
            "🔍 Data anomalies detected:\n\n",
            "• Coverage >100%: ", d$above_100, " records\n",
            "• Coverage <0%: ", d$below_0, " records\n",
            "• Statistical outliers: ", d$outliers, " records\n"
          )
          if (length(d$examples) > 0) {
            response <- paste0(response, "\nExamples:\n")
            for (ex in d$examples) {
              response <- paste0(response, "  • ", ex$country, " - ", ex$district, ": ", ex$value, "%\n")
            }
          }
          return(response)
        }
      }
      
      # Default help
      paste0(
        "I can help with these topics (aggregated only). Try:\n\n",
        "• Compare missed children between LCB and ESA (last 6 months)\n",
        "• Missed children in Nigeria last 6 months\n",
        "• Coverage in Cameroon last year\n",
        "• Top reasons for missed in Chad\n",
        "• District performance in Ethiopia last 6 months\n",
        "• Administrative coverage in Ghana\n",
        "• Find anomalies in admin coverage\n",
        "• Trend for missed in Niger last 12 months"
      )
    },
    
    # ============================================================
    # MAIN PROCESS METHOD
    # ============================================================
    
    process = function(message) {
      self$add_to_memory("user", message)
      
      if (self$using_llm && !self$quota_exceeded) {
        response <- self$process_with_llm(message)
        if (!is.null(response)) {
          self$add_to_memory("assistant", response)
          return(response)
        }
      }
      
      response <- self$process_fallback(message)
      self$add_to_memory("assistant", response)
      response
    }
  )
)

# ============================================================
# 🤖 UI MODULE
# ============================================================

sia_ai_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "ai-assistant-container",
      style = "display: flex; flex-direction: column; height: 600px;",
      
      div(
        id = ns("chat_history"),
        style = "flex: 1; overflow-y: auto; padding: 15px; background-color: #f9f9f9; border-radius: 8px; min-height: 400px;",
        uiOutput(ns("chat_messages"))
      ),
      
      div(
        style = "margin-top: 15px; display: flex; gap: 10px;",
        textAreaInput(
          ns("user_input"),
          label = NULL,
          placeholder = "Ask me anything about the SIA data...",
          width = "100%",
          rows = 2
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 5px;",
          actionButton(
            ns("send_message"),
            label = tagList(icon("paper-plane"), "Send"),
            class = "btn-primary",
            style = "height: 50px; width: 110px;"
          ),
          actionButton(
            ns("clear_chat"),
            label = tagList(icon("trash"), "Clear"),
            class = "btn-default",
            style = "height: 40px; width: 110px;"
          )
        )
      ),
      
      div(
        style = "margin-top: 10px; font-size: 12px; color: #6c757d; display: flex; justify-content: space-between;",
        span(textOutput(ns("status"), inline = TRUE)),
        span(icon("robot"), "MWANZA Pro • WHO AFRO SIA Assistant")
      )
    )
  )
}

# ============================================================
# 🧠 SERVER MODULE
# ============================================================

sia_ai_server <- function(
    id,
    filtered_data = NULL,          # must be a reactive returning list(main_data, admin_data, scope_data, all_*)
    context = NULL,
    analysis_functions = NULL,
    block_definitions = NULL,
    country_abbreviations = NULL,
    llm_provider = "openai",
    model = "gpt-4o",
    max_per_min = 12               # public rate limit
) {
  
  moduleServer(id, function(input, output, session) {
    
    # ----------------------------
    # Initialize LLM client (server-side key only)
    # ----------------------------
    llm_client <- tryCatch({
      if (identical(llm_provider, "openai")) {
        api_key <- Sys.getenv("OPENAI_API_KEY")
        if (nzchar(api_key)) {
          chat_openai(model = model, api_key = api_key)
        } else {
          NULL
        }
      } else {
        NULL
      }
    }, error = function(e) {
      warning("LLM client initialization failed: ", e$message)
      NULL
    })
    
    agent <- reactiveVal(NULL)
    
    # Initialize when data is ready (tolerant)
    observe({
      req(is.function(filtered_data))
      fd <- filtered_data()
      req(!is.null(fd))
      
      agent(SmartAgent$new(
        llm_client = llm_client,
        block_definitions = block_definitions
      ))
      
      agent()$set_data(list(
        main_data = fd$main_data,
        admin_data = fd$admin_data,
        scope_data = fd$scope_data,
        all_countries = fd$all_countries,
        all_provinces = fd$all_provinces,
        all_districts = fd$all_districts
      ))
    })
    
    values <- reactiveValues(
      chat_history = list(),
      processing = FALSE
    )
    
    # Welcome message (once)
    observeEvent(TRUE, {
      status <- if (!is.null(llm_client)) "AI mode active" else "offline mode (no API key)"
      welcome <- paste0(
        "👋 Hello! I'm MWANZA Pro (", status, ").\n\n",
        "✅ Public dashboard safe mode: I provide aggregated indicators only (no individual line-lists).\n\n",
        "Try:\n",
        "• Compare missed children between LCB and ESA (last 6 months)\n",
        "• Missed children in Nigeria last 6 months\n",
        "• Top reasons for missed in Cameroon\n",
        "• District performance in Ethiopia\n",
        "• Administrative coverage in Ghana\n",
        "• Doses administered in Nigeria\n",
        "• Anomalies in coverage data\n",
        "• Trend for missed in Niger last 12 months\n\n",
        "What would you like to know?"
      )
      
      values$chat_history <- list(list(role = "assistant", content = welcome))
    }, once = TRUE)
    
    # Render messages (XSS-safe)
    output$chat_messages <- renderUI({
      history <- values$chat_history
      
      messages <- lapply(history, function(msg) {
        is_user <- identical(msg$role, "user")
        
        style <- if (is_user) {
          "background-color: #0072BC; color: white; margin-left: auto;"
        } else {
          "background-color: #e9ecef; color: #212529;"
        }
        
        safe_txt <- escape_html(msg$content)
        safe_txt <- gsub("\n", "<br>", safe_txt, fixed = TRUE)
        
        div(
          style = "display: flex; margin-bottom: 15px;",
          div(
            style = paste0(style, " padding: 10px 15px; border-radius: 18px; max-width: 80%; word-wrap: break-word;"),
            tags$p(style = "margin: 0; white-space: pre-wrap;", HTML(safe_txt))
          )
        )
      })
      
      do.call(tagList, messages)
    })
    
    # Status
    output$status <- renderText({
      if (values$processing) {
        "🤖 Thinking..."
      } else if (!is.null(agent())) {
        if (!is.null(llm_client) && !agent()$quota_exceeded) "✅ AI Active"
        else if (!is.null(llm_client) && agent()$quota_exceeded) "⚠️ Quota exceeded (fallback)"
        else "✅ Ready (fallback)"
      } else {
        "⚠️ Initializing..."
      }
    })
    
    add_message <- function(role, content) {
      values$chat_history <- c(values$chat_history, list(list(role = role, content = content)))
    }
    
    process_message <- function(txt) {
      if (nchar(trimws(txt)) == 0 || is.null(agent())) return()
      
      # Public protection: rate limit per session
      if (!rate_limiter(session$token, max_per_min = max_per_min)) {
        add_message("assistant", "⚠️ Too many requests too quickly. Please wait a moment and try again.")
        updateTextAreaInput(session, "user_input", value = "")
        return()
      }
      
      add_message("user", txt)
      values$processing <- TRUE
      
      tryCatch({
        response <- agent()$process(txt)
        add_message("assistant", response)
      }, error = function(e) {
        add_message("assistant", paste0("Sorry: ", e$message))
      }, finally = {
        values$processing <- FALSE
        updateTextAreaInput(session, "user_input", value = "")
      })
    }
    
    observeEvent(input$send_message, {
      process_message(input$user_input)
    })
    
    observeEvent(input$user_input, {
      if (grepl("\n$", input$user_input %||% "")) {
        process_message(trimws(input$user_input))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$clear_chat, {
      values$chat_history <- list(list(role = "assistant", content = "👋 Chat cleared!"))
      if (!is.null(agent())) agent()$memory <- list()
    })
  })
}

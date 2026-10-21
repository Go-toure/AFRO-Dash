# ============================================================
# AFRO IM OVERVIEW  (Independent Monitoring)  --  Shiny module
# ============================================================
# Ported from the AFRO Independent Monitoring dashboard ("Overview" tab).
#
#   UI     : im_overview_ui("im")      -> put inside a shinydashboard tabItem
#   Server : im_overview_server("im")  -> call once in the app's server()
#
# Data    : input/AFRO_IM_data.rds  (written by the IM workflow; see
#           scripts/publish_to_afro_dash.py / run_workflow.R --push-afro-dash).
#           Column names are lower-case; the file is the cleaned IM repository.
#
# Notes   : * Every id is namespaced, so nothing clashes with the other tabs.
#           * plotly is NOT attached (the app uses conflicted); the module
#             looks plotly's functions up in a private environment instead.
#           * Dark mode follows the app's `.dark-mode` body class (CSS only).
# ============================================================

IM_DATA_FILE <- file.path("input", "AFRO_IM_data.rds")

IM_PAL <- list(
  blue   = "#005C97", blue2  = "#003F6B", blue3  = "#1A7BBF",
  teal   = "#00C9C8", teal2  = "#007B8A",
  green  = "#27AE60", green2 = "#1A7A44",
  orange = "#E8730A", orange2= "#B8520A",
  red    = "#C0392B", red2   = "#922B21",
  purple = "#8E44AD", purple2= "#5D2A7A",
  dark   = "#1A2637", dark2  = "#243447",
  bg     = "#F0F4F8", muted  = "#6C7A8D"
)

# bootstrap-icons names used by the original app  ->  Font Awesome (shiny::icon)
.im_icon_map <- c(
  "globe-europe-africa" = "globe-africa", "tools" = "tools", "bullseye" = "bullseye",
  "map-fill" = "map", "speedometer2" = "tachometer-alt",
  "diagram-3-fill" = "project-diagram",
  "exclamation-triangle-fill" = "exclamation-triangle", "search" = "search",
  "graph-up" = "chart-line", "fire" = "fire", "pie-chart-fill" = "chart-pie",
  "bar-chart-line-fill" = "chart-bar", "bar-chart-fill" = "chart-bar",
  "geo-alt-fill" = "map-marker-alt", "trophy-fill" = "trophy",
  "camera-fill" = "camera", "download" = "download",
  "arrow-counterclockwise" = "undo", "cloud-download-fill" = "cloud-download-alt",
  "funnel-fill" = "filter", "calendar3" = "calendar-alt",
  "clipboard2-pulse-fill" = "clipboard-check"
)
bs_icon <- function(name, ...) {
  if (identical(name, "download")) return(NULL)   # downloadButton() already draws its own icon
  fa <- unname(.im_icon_map[name]); if (is.na(fa)) fa <- "circle"
  shiny::icon(fa)
}

# ---- data loading ------------------------------------------------------------
im_load_data <- function(path = IM_DATA_FILE) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(df)) return(NULL)
  published <- attr(df, "im_published_at")
  df <- as.data.frame(df)
  names(df) <- tolower(names(df))
  if ("region" %in% names(df) && !"province" %in% names(df)) names(df)[names(df) == "region"] <- "province"
  if ("vaccine.type" %in% names(df) && !"vaccine_type" %in% names(df)) names(df)[names(df) == "vaccine.type"] <- "vaccine_type"
  if (!"cv" %in% names(df) && all(c("u5_fm", "u5_present") %in% names(df)))
    df$cv <- ifelse(df$u5_present > 0, df$u5_fm / df$u5_present, NA_real_)
  if ("cv" %in% names(df)) df$cv <- pmin(pmax(df$cv, 0), 1)
  for (dc in intersect(c("round_start_date", "start_date_im_end", "start_date"), names(df)))
    df[[dc]] <- tryCatch(as.Date(df[[dc]]), error = function(e) as.Date(NA))
  attr(df, "im_published_at") <- if (is.null(published)) file.mtime(path) else published
  df
}

# ---- styling (scoped to .im-ov; light + dark) ---------------------------------
IM_CSS <- "
.im-ov{--im-card:#ffffff;--im-card2:#f5f7fa;--im-text:#1A2637;--im-muted:#6C7A8D;
  --im-border:rgba(0,92,151,.14);--im-shadow:0 4px 18px rgba(0,92,151,.10);--im-accent:#005C97;}
.dark-mode .im-ov{--im-card:#0b1220;--im-card2:#111b2e;--im-text:#e5e7eb;--im-muted:#a8b3c4;
  --im-border:rgba(148,163,184,.28);--im-shadow:0 6px 22px rgba(0,0,0,.55);}
.im-ov{color:var(--im-text);font-family:'Inter','Source Sans Pro',sans-serif;}
.im-ov .row{margin-left:-8px;margin-right:-8px;}
.im-ov .row>[class*='col-']{padding-left:8px;padding-right:8px;}

/* panels */
.im-card{background:var(--im-card);border:1px solid var(--im-border);border-radius:14px;
  box-shadow:var(--im-shadow);margin-bottom:16px;overflow:hidden;}
.im-card-h{display:flex;align-items:center;justify-content:space-between;gap:10px;flex-wrap:wrap;
  padding:12px 16px 10px;border-bottom:1px solid var(--im-border);font-weight:700;font-size:14.4px;color:var(--im-text);}
.im-card-h i{margin-right:8px;color:var(--im-accent);}
.dark-mode .im-card-h i{color:#7cc4ff;}
.im-card-b{padding:6px 10px 10px;}
.im-legend{font-size:10.9px;font-weight:500;display:flex;gap:8px;flex-wrap:wrap;color:var(--im-muted);}

/* filters */
.im-filters{background:var(--im-card);border:1px solid var(--im-border);border-radius:14px;
  box-shadow:var(--im-shadow);padding:12px 16px 4px;margin-bottom:16px;}
.im-filters .im-sec{font-size:11.2px;font-weight:800;letter-spacing:.08em;text-transform:uppercase;
  color:var(--im-accent);margin:0 0 6px;}
.dark-mode .im-filters .im-sec{color:#7cc4ff;}
.im-filters label{font-size:11.5px;font-weight:700;color:var(--im-muted);text-transform:uppercase;letter-spacing:.04em;margin-bottom:2px;}
.im-filters .form-group{margin-bottom:10px;}
.im-filters .form-control,.im-filters .selectize-input{font-size:13.4px;border-radius:8px;min-height:34px;}
.dark-mode .im-filters .form-control,.dark-mode .im-filters .selectize-input,
.dark-mode .im-filters .selectize-dropdown{background:#111b2e!important;color:#e5e7eb!important;border-color:rgba(148,163,184,.35)!important;}
.dark-mode .im-filters .selectize-input .item{color:#e5e7eb;}
.dark-mode .im-filters .selectize-dropdown .active{background:#1e2a44!important;color:#fff!important;}
.dark-mode .im-filters option{background:#111b2e;color:#e5e7eb;}
.im-actions{display:flex;gap:8px;flex-wrap:wrap;align-items:center;margin-bottom:10px;}
.im-btn,.im-ov .btn-dl{display:inline-flex;align-items:center;gap:6px;border:1.5px solid var(--im-accent);
  color:var(--im-accent);background:transparent;border-radius:8px;font-size:11.8px;font-weight:700;
  padding:3px 10px;line-height:1.4;cursor:pointer;text-decoration:none;}
.im-btn:hover,.im-ov .btn-dl:hover{background:var(--im-accent);color:#fff;text-decoration:none;}
.dark-mode .im-btn,.dark-mode .im-ov .btn-dl{border-color:#7cc4ff;color:#7cc4ff;}
.dark-mode .im-btn:hover,.dark-mode .im-ov .btn-dl:hover{background:#7cc4ff;color:#06101f;}
.im-badges{display:flex;flex-wrap:wrap;gap:4px;margin-top:4px;}
.im-badge{background:rgba(0,92,151,.12);color:var(--im-accent);font-size:10.6px;font-weight:700;padding:2px 8px;border-radius:6px;}
.dark-mode .im-badge{background:rgba(124,196,255,.18);color:#bfe3ff;}
.im-filter-note{font-size:11.5px;color:var(--im-muted);}

/* KPI strip */
.im-kpis{display:grid;grid-template-columns:repeat(auto-fit,minmax(128px,1fr));gap:10px;margin-bottom:16px;}
.im-kpi{background:var(--im-card);border:1px solid var(--im-border);border-top:4px solid var(--k,#005C97);
  border-radius:12px;box-shadow:var(--im-shadow);padding:10px 14px 12px;min-height:82px;}
.im-kpi-l{font-size:10.2px;font-weight:800;letter-spacing:.08em;text-transform:uppercase;color:var(--im-muted);}
.im-kpi-v{font-size:23.2px;font-weight:800;line-height:1.15;color:var(--im-text);margin-top:2px;}
.im-kpi-v.sm{font-size:16px;margin-top:6px;}
.im-kpi-s{font-size:9.9px;color:var(--im-muted);margin-top:1px;}
.im-kpi-bar{height:4px;border-radius:3px;background:rgba(128,140,160,.25);margin-top:6px;overflow:hidden;}
.im-kpi-bar>div{height:100%;background:var(--k,#005C97);border-radius:3px;}

/* hero */
.im-ov .hero-ctx{background:var(--im-card);border:1px solid var(--im-border);border-left:5px solid #009ADE;
  border-radius:14px;box-shadow:var(--im-shadow);padding:14px 18px;margin-bottom:16px;}
.im-ov .hero-ctx-title{font-size:16.8px;font-weight:800;color:var(--im-text);}
.im-ov .hero-ctx-sub{font-size:12.2px;color:var(--im-muted);margin-top:4px;font-weight:500;}
.im-ov .hero-ctx-sub i{margin-right:6px;color:#009ADE;}
.im-ov .hero-stats{display:flex;align-items:center;}
.im-ov .hero-stat{padding:0 18px;border-left:1px solid var(--im-border);text-align:center;}
.im-ov .hero-stat:first-child{border-left:none;}
.im-ov .hero-stat-val{font-size:21.6px;font-weight:800;color:var(--im-text);}
.im-ov .hero-stat-lbl{font-size:10.2px;font-weight:700;letter-spacing:.07em;text-transform:uppercase;color:var(--im-muted);}

/* misc */
.im-ov .text-muted{color:var(--im-muted)!important;}
.im-empty{padding:28px;text-align:center;color:var(--im-muted);}
.im-toast{position:fixed;bottom:22px;right:22px;background:#003A70;color:#fff;padding:10px 16px;border-radius:8px;
  font-size:12.8px;z-index:99999;box-shadow:0 6px 20px rgba(0,0,0,.35);}
.im-toast.err{background:#C0392B;}

/* plotly toolbar: light, unobtrusive */
.im-ov .modebar{background:transparent!important;}
.im-ov .modebar-group{background:transparent!important;padding:0 2px!important;}
.im-ov .modebar-btn path{fill:#8a97a8!important;}
.im-ov .modebar-btn:hover path{fill:#005C97!important;}

/* plotly in dark mode: light text, soft grid */
.dark-mode .im-ov .js-plotly-plot .plotly text{fill:#e5e7eb!important;}
.dark-mode .im-ov .js-plotly-plot .gridlayer path,
.dark-mode .im-ov .js-plotly-plot .zerolinelayer path{stroke:rgba(148,163,184,.22)!important;}
.dark-mode .im-ov .js-plotly-plot .xaxislayer-above path,
.dark-mode .im-ov .js-plotly-plot .yaxislayer-above path{stroke:rgba(148,163,184,.45)!important;}
.dark-mode .im-ov .js-plotly-plot .modebar-btn path{fill:#a8b3c4!important;}
.dark-mode .im-ov .js-plotly-plot .legend .bg{fill:transparent!important;stroke:none!important;}
@media (max-width:768px){.im-ov .hero-stats{width:100%;justify-content:space-between;margin-top:8px;}}
"

IM_JS <- "
function imToast(msg, err){
  var d=document.createElement('div'); d.className='im-toast'+(err?' err':''); d.textContent=msg;
  document.body.appendChild(d); setTimeout(function(){d.remove();}, 3200);
}
function imLoadH2C(cb){
  if (window.html2canvas) return cb(true);
  var s=document.createElement('script');
  s.src='https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js';
  s.onload=function(){cb(true);}; s.onerror=function(){cb(false);};
  document.head.appendChild(s);
}
function imCaptureExec(id){
  var el=document.getElementById(id);
  if(!el){imToast('Could not find the snapshot section.', true); return;}
  imToast('Generating image...', false);
  imLoadH2C(function(ok){
    if(!ok){imToast('Image export library could not load - use the CSV button instead.', true); return;}
    var btns=el.querySelectorAll('.btn, button, .im-btn');
    btns.forEach(function(b){b.style.visibility='hidden';});
    html2canvas(el,{backgroundColor:'#F0F4F8',scale:2,useCORS:true}).then(function(c){
      btns.forEach(function(b){b.style.visibility='';});
      var a=document.createElement('a');
      a.download='AFRO_IM_Executive_Snapshot_'+new Date().toISOString().slice(0,10)+'.png';
      a.href=c.toDataURL('image/png'); a.click(); imToast('Image downloaded', false);
    }).catch(function(e){btns.forEach(function(b){b.style.visibility='';}); imToast('Image export failed', true);});
  });
}
// plotly charts on a tab that was hidden at first render need a resize once visible
$(document).on('shown.bs.tab', function(){ setTimeout(function(){window.dispatchEvent(new Event('resize'));}, 80); });
$(document).on('click', 'a[data-toggle=\"tab\"], .sidebar-menu a', function(){
  setTimeout(function(){window.dispatchEvent(new Event('resize'));}, 250);
});
"

# ---- UI -----------------------------------------------------------------------
im_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  C  <- IM_PAL
  ico <- function(n) bs_icon(n)

  dl <- function(out) shiny::downloadButton(ns(out), shiny::tagList(ico("download"), "CSV"), class = "btn-dl")
  card <- function(title = NULL, body, out_dl = NULL, legend = NULL, title_ui = NULL, icon_name = NULL) {
    shiny::div(class = "im-card",
      shiny::div(class = "im-card-h",
        shiny::div(style = "display:flex;align-items:center;gap:14px;flex-wrap:wrap;",
          if (!is.null(title_ui)) title_ui else shiny::div(ico(icon_name), title),
          legend),
        if (!is.null(out_dl)) dl(out_dl)),
      shiny::div(class = "im-card-b", body))
  }
  plt <- function(out, h) plotly::plotlyOutput(ns(out), height = h)
  sp  <- function(col, txt) shiny::tags$span(style = paste0("color:", col, ";"), txt)
  H   <- shiny::HTML

  shiny::div(class = "im-ov",
    shiny::tags$head(shiny::tags$style(H(IM_CSS)), shiny::tags$script(H(IM_JS))),

    shiny::uiOutput(ns("im_empty")),

    # ---- filters ----------------------------------------------------------------
    shiny::div(class = "im-filters",
      shiny::fluidRow(
        shiny::column(2, shiny::selectInput(ns("ov_block"),    "AFRO Block", c("All", "LCB", "WA", "DRC", "CEA", "ESA"))),
        shiny::column(2, shiny::selectInput(ns("ov_country"),  "Country",  "All")),
        shiny::column(2, shiny::selectInput(ns("ov_province"), "Province", "All")),
        shiny::column(2, shiny::selectInput(ns("ov_district"), "District", "All")),
        shiny::column(2, shiny::selectInput(ns("ov_vaccine"),  "Vaccine Type", "All")),
        shiny::column(2, shiny::selectInput(ns("ov_response"), "Response", "All"))
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::selectizeInput(ns("ov_round"), "Round Number(s)", choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All rounds", plugins = list("remove_button")))),
        shiny::column(3, shiny::selectInput(ns("ov_date_type"), "Period",
                         c("All time", "Last N months", "Last N years", "Date range",
                           "Last N consecutive rounds", "Last N rounds in last 12 months"))),
        shiny::column(3,
          shiny::conditionalPanel(sprintf("input['%s'] == 'Last N months'", ns("ov_date_type")),
            shiny::numericInput(ns("ov_n_months"), "Months", 6, min = 1, max = 120, step = 1)),
          shiny::conditionalPanel(sprintf("input['%s'] == 'Last N years'", ns("ov_date_type")),
            shiny::numericInput(ns("ov_n_years"), "Years", 1, min = 1, max = 10, step = 1)),
          shiny::conditionalPanel(sprintf("input['%s'] == 'Date range'", ns("ov_date_type")),
            shiny::dateRangeInput(ns("ov_daterange"), "Range", start = Sys.Date() - 365, end = Sys.Date())),
          shiny::conditionalPanel(sprintf("input['%s'] == 'Last N consecutive rounds'", ns("ov_date_type")),
            shiny::numericInput(ns("ov_n_rounds"), "N most recent rounds", 3, min = 1, max = 50, step = 1)),
          shiny::conditionalPanel(sprintf("input['%s'] == 'Last N rounds in last 12 months'", ns("ov_date_type")),
            shiny::numericInput(ns("ov_n_rounds_12m"), "N most recent rounds", 3, min = 1, max = 50, step = 1))
        ),
        shiny::column(3,
          shiny::div(style = "margin-top:20px;",
            shiny::div(class = "im-actions",
              shiny::actionLink(ns("ov_reset"), shiny::tagList(ico("arrow-counterclockwise"), "Reset filters"), class = "im-btn"),
              shiny::downloadLink(ns("dl_ov_all"), shiny::tagList(ico("cloud-download-fill"), "Filtered data (CSV)"), class = "im-btn")),
            shiny::uiOutput(ns("ov_filter_badge"))))
      )
    ),

    # ---- KPI strip + hero + executive snapshot ------------------------------------
    shiny::uiOutput(ns("kpi_strip")),
    shiny::uiOutput(ns("hero_banner")),
    shiny::div(id = ns("exec_kpi_capture"), shiny::uiOutput(ns("exec_kpi_card"))),

    # ---- Row 2 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(6, card("Country-Level IM Risk Profile", plt("chart_country_risk", "360px"), "dl_country_risk", icon_name = "globe-europe-africa")),
      shiny::column(6, card("Operational Failure Profile",   plt("chart_op_failure",   "360px"), "dl_op_failure",   icon_name = "tools"))
    ),
    # ---- Row 3 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(12, card("Top Priority Districts for Advocacy", plt("chart_priority_districts", "640px"), "dl_priority_districts", icon_name = "bullseye"))
    ),
    # ---- Row 4 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(7, card("CV Performance by AFRO Block",   plt("chart_block",     "260px"), "dl_block",     icon_name = "map-fill")),
      shiny::column(5, card("District Risk Classification",   plt("chart_risk_dist", "260px"), "dl_risk_dist", icon_name = "speedometer2"))
    ),
    # ---- Row 5 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(7, card("Social Mobilisation vs Coverage", plt("chart_sm_quadrant", "450px"), "dl_sm_quad", icon_name = "diagram-3-fill",
        legend = shiny::div(class = "im-legend",
          sp(C$red, "\u25CF Low SM+CV"), sp(C$orange, "\u25CF Low SM"), sp(C$purple, "\u25CF Low CV"), sp(C$blue, "\u25CF High SM+CV")))),
      shiny::column(5, card("Countries / Areas Below 80% CV",
        shiny::div(style = "padding:4px 8px;", shiny::uiOutput(ns("alert_low_cv"))), "dl_alert_cv", icon_name = "exclamation-triangle-fill"))
    ),
    # ---- Row 6 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(5, card("Root Causes of Missed Children", plt("chart_root_ov", "360px"), "dl_root_ov", icon_name = "search")),
      shiny::column(7, card(NULL, plt("chart_cv", "360px"), "dl_cv", title_ui = shiny::uiOutput(ns("chart_cv_title"), inline = TRUE),
        legend = shiny::div(class = "im-legend", sp(C$green, "\u25A0 \u226590%"), sp(C$orange, "\u25A0 80\u201390%"), sp(C$red, "\u25A0 <80%"))))
    ),
    # ---- Row 7 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(4, card(NULL, plt("chart_top_bottom", "280px"), "dl_top_bottom", title_ui = shiny::uiOutput(ns("chart_topbot_title"), inline = TRUE))),
      shiny::column(8, card("Records & Coverage Trend Over Time", plt("chart_trend", "280px"), "dl_trend", icon_name = "graph-up"))
    ),
    # ---- Row 8 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(8, card("CV Heatmap \u2014 Area \u00D7 Campaign Round", plt("chart_heatmap", "340px"), "dl_heatmap", icon_name = "fire")),
      shiny::column(4, card("QC Flag Distribution", plt("chart_qc_pie", "340px"), "dl_qc_pie", icon_name = "pie-chart-fill"))
    ),
    # ---- Row 9 ------------------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(12, card("% Districts \u226595% CV \u2014 Country \u00D7 Vaccine Type \u00D7 Month", plt("chart_cv95", "520px"), "dl_cv95", icon_name = "bar-chart-line-fill"))
    )
  )
}

# ---- server -------------------------------------------------------------------
im_overview_server <- function(id, data_path = IM_DATA_FILE) {
  shiny::moduleServer(id, function(input, output, session) {

    C_       <- IM_PAL
    is_admin <- function() TRUE            # regional dashboard: no country scoping

    # ---- helpers (from the IM app: R/data_helpers.R, R/risk_scoring.R, R/ui_helpers.R)
    fmt_pct <- function(x, d = 1) ifelse(is.na(x), "\u2014", paste0(round(x * 100, d), "%"))
    fmt_num <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE)
    date_col_of <- function(df)
      intersect(c("round_start_date", "start_date_im_end", "start_date"), names(df))[1]

    with_risk <- function(df) {
      if (!all(c("u5_present", "u5_fm") %in% names(df))) return(df)
      df$u5_present <- as.numeric(df$u5_present)
      df$u5_fm      <- as.numeric(df$u5_fm)
      if (!"missed_child" %in% names(df))
        df$missed_child <- pmax(df$u5_present - df$u5_fm, 0, na.rm = FALSE)
      df$cv_d        <- ifelse(df$u5_present > 0, df$u5_fm / df$u5_present, NA_real_)
      df$missed_rate <- ifelse(df$u5_present > 0, df$missed_child / df$u5_present, NA_real_)
      has_sm <- all(c("number_of_hh_visited", "care_giver_informed_sia") %in% names(df))
      df$awareness_rate <- if (has_sm)
        ifelse(as.numeric(df$number_of_hh_visited) > 0,
               as.numeric(df$care_giver_informed_sia) / as.numeric(df$number_of_hh_visited),
               NA_real_)
      else NA_real_
      df$risk_score <-
        ifelse(is.na(df$cv_d), 50,
        ifelse(df$cv_d < 0.80, 60, ifelse(df$cv_d < 0.90, 40, 20))) +
        ifelse(is.na(df$missed_rate), 0,
        ifelse(df$missed_rate > 0.20, 30, ifelse(df$missed_rate > 0.10, 15, 0))) +
        ifelse(is.na(df$awareness_rate), 0,
        ifelse(df$awareness_rate < 0.50, 20, ifelse(df$awareness_rate < 0.80, 10, 0)))
      df$risk_class <- ifelse(df$risk_score >= 70, "Critical",
                       ifelse(df$risk_score >= 50, "High",
                       ifelse(df$risk_score >= 30, "Moderate", "Low")))
      df
    }

    plotly_base <- function() {
      list(
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font          = list(family = "Inter, sans-serif", color = "#1A2637", size = 12),
        hoverlabel    = list(bgcolor = "#1A2637", bordercolor = "transparent",
                             font = list(color = "white", family = "Inter", size = 12)),
        margin        = list(t = 10, b = 10, l = 10, r = 10),
        legend        = list(bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(0,0,0,0)")
      )
    }
    im_layout <- function(p, ..., .filename = "im_chart", .title = NULL) {
      base   <- plotly_base()
      extras <- list(...)
      if (!is.null(.title) && !"title" %in% names(extras)) {
        base$margin$t <- 46
        title_arg <- list(title = list(
          text = .title, font = list(family = "Inter, sans-serif", size = 13, color = "#1A2637"),
          x = 0.01, xanchor = "left", pad = list(b = 2)))
      } else title_arg <- list()
      do.call(plotly::layout, c(list(p), base, title_arg, extras)) %>%
        plotly::config(
          displayModeBar = TRUE, displaylogo = FALSE,
          modeBarButtonsToRemove = list("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(format = "png", filename = .filename, height = 550, width = 900, scale = 2))
    }

    # ---- data -------------------------------------------------------------------------
    rv <- shiny::reactiveValues(data = im_data_cached(data_path))

    output$im_empty <- shiny::renderUI({
      if (!is.null(rv$data) && nrow(rv$data) > 0) return(NULL)
      shiny::div(class = "im-card", shiny::div(class = "im-empty",
        shiny::tags$h4("No Independent Monitoring data found"),
        shiny::p(paste0("Expected file: ", data_path, ". Run the IM workflow with --push-afro-dash ",
                        "(or copy Regional_IM_repository_cleaned.rds there as AFRO_IM_data.rds)."))))
    })

    snap <- function() {
      df <- tryCatch(ov_data(), error = function(e) rv$data)
      if (is.null(df)) data.frame() else df
    }
    active <- function() snap()

    # ---- KPI strip (filtered) ----------------------------------------------------------
    output$kpi_strip <- shiny::renderUI({
      df <- tryCatch(ov_data(), error = function(e) NULL)
      tile <- function(label, value, sub = "", col = "#005C97", bar = NULL, small = FALSE)
        shiny::div(class = "im-kpi", style = paste0("--k:", col, ";"),
          shiny::div(class = "im-kpi-l", label),
          shiny::div(class = paste("im-kpi-v", if (small) "sm" else ""), value),
          if (nzchar(sub)) shiny::div(class = "im-kpi-s", sub),
          if (!is.null(bar) && is.finite(bar))
            shiny::div(class = "im-kpi-bar", shiny::div(style = paste0("width:", min(max(bar, 0), 1) * 100, "%;"))))
      pub <- attr(rv$data, "im_published_at")
      run <- if (is.null(pub)) "\u2014" else format(as.POSIXct(pub), "%d %b %Y %H:%M")
      if (is.null(df))
        return(shiny::div(class = "im-kpis",
          tile("Total Records", "\u2014"), tile("Households Visited", "\u2014", col = "#00908F"),
          tile("Coverage Rate", "\u2014", col = "#27AE60"), tile("Missed Children", "\u2014", col = "#C0392B"),
          tile("Caregiver Awareness", "\u2014", col = "#E8730A"), tile("Critical Districts", "\u2014", col = "#8E44AD"),
          tile("Countries", "\u2014", col = "#00908F"), tile("Last Data Update", run, col = "#005C97", small = TRUE)))
      tp <- if ("u5_present" %in% names(df)) sum(as.numeric(df$u5_present), na.rm = TRUE) else NA
      tf <- if ("u5_fm" %in% names(df)) sum(as.numeric(df$u5_fm), na.rm = TRUE) else NA
      cv <- if (!is.na(tp) && tp > 0) tf / tp else NA
      mc <- if (!is.na(tp)) max(tp - tf, 0) else NA
      hh <- if ("number_of_hh_visited" %in% names(df)) sum(as.numeric(df$number_of_hh_visited), na.rm = TRUE) else NA
      dfr  <- tryCatch(with_risk(df), error = function(e) NULL)
      # same definition as the executive snapshot: mean of the row-level awareness rates
      aw <- if (!is.null(dfr) && "awareness_rate" %in% names(dfr)) mean(dfr$awareness_rate, na.rm = TRUE) else NA
      crit <- if (!is.null(dfr) && "risk_class" %in% names(dfr))
        nrow(dplyr::filter(dplyr::summarise(dplyr::group_by(dfr, country, province, district),
               rc = dplyr::first(risk_class), .groups = "drop"), rc == "Critical")) else NA
      shiny::div(class = "im-kpis",
        tile("Total Records", fmt_num(nrow(df)), "filtered"),
        tile("Households Visited", if (is.na(hh)) "\u2014" else fmt_num(hh), "HH reached", "#00908F"),
        tile("Coverage Rate", fmt_pct(cv), "vs 90% target", "#27AE60", bar = cv / 0.9),
        tile("Missed Children", if (is.na(mc)) "\u2014" else fmt_num(mc), "", "#C0392B"),
        tile("Caregiver Awareness", fmt_pct(aw), "target 80%", "#E8730A", bar = aw / 0.8),
        tile("Critical Districts", if (is.na(crit)) "\u2014" else fmt_num(crit), "risk \u2265 70", "#8E44AD"),
        tile("Countries", fmt_num(dplyr::n_distinct(df$country, na.rm = TRUE)), "in scope", "#00908F"),
        tile("Last Data Update", run, "", "#005C97", small = TRUE))
    })

  # ?? Hero context banner ??????????????????????????????????????????????????????
  output$hero_banner <- renderUI({
    df <- tryCatch(ov_data(), error = function(e) NULL)
    df <- if (!is.null(df)) df else rv$data

    n_rec      <- if (!is.null(df)) nrow(df)                                              else 0
    n_country  <- if (!is.null(df) && "country"  %in% names(df)) n_distinct(df$country,  na.rm=TRUE) else 0
    n_district <- if (!is.null(df) && "district" %in% names(df)) n_distinct(df$district, na.rm=TRUE) else 0

    active <- character(0)
    if (!is.null(input$ov_block)    && input$ov_block    != "All") active <- c(active, input$ov_block)
    if (!is.null(input$ov_country)  && input$ov_country  != "All") active <- c(active, input$ov_country)
    if (!is.null(input$ov_vaccine)  && input$ov_vaccine  != "All") active <- c(active, input$ov_vaccine)
    if (!is.null(input$ov_response) && input$ov_response != "All") active <- c(active, input$ov_response)
    if (length(input$ov_round) > 0) active <- c(active, paste("Rnd", paste(input$ov_round, collapse=",")))
    if (!is.null(input$ov_date_type) && input$ov_date_type != "All time") active <- c(active, input$ov_date_type)

    ctx <- if (length(active) > 0) paste(active, collapse=" \u00B7 ") else "All countries \u00B7 All campaigns"

    div(class="hero-ctx fade-up",
      div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:14px;position:relative;z-index:1;",
        div(
          div(class="hero-ctx-title", "AFRO Regional IM Intelligence"),
          div(class="hero-ctx-sub",
            tagList(bs_icon("geo-alt-fill", class = "me-1",
                             style = "vertical-align:-0.1em;"), ctx))
        ),
        div(class="hero-stats",
          div(class="hero-stat",
            div(class="hero-stat-val", formatC(n_rec,      format="d", big.mark=",")),
            div(class="hero-stat-lbl", "Records")
          ),
          div(class="hero-stat",
            div(class="hero-stat-val", n_country),
            div(class="hero-stat-lbl", "Countries")
          ),
          div(class="hero-stat",
            div(class="hero-stat-val", formatC(n_district, format="d", big.mark=",")),
            div(class="hero-stat-lbl", "Districts")
          )
        )
      )
    )
  })
  # ?? Executive KPI snapshot card (live replica of report Visual 0) ???????????
  output$exec_kpi_card <- renderUI({
    df <- tryCatch(ov_data(), error = function(e) NULL)
    df <- if (!is.null(df)) df else rv$data
    if (is.null(df) || !nrow(df)) return(NULL)
    dfr <- tryCatch(with_risk(df), error = function(e) NULL)

    who_blue  <- "#0093D5"; dark_blue <- "#003A70"; alert_red <- "#C00000"
    gold      <- "#F2B600"; soft_grey <- "#F5F7FA"

    n_country  <- if ("country" %in% names(df)) n_distinct(df$country, na.rm=TRUE) else 0
    n_district <- if (all(c("country","province","district") %in% names(df)))
                     n_distinct(paste(df$country, df$province, df$district)) else 0
    n_rounds   <- if (all(c("country","response","roundnumber") %in% names(df)))
                     n_distinct(paste(df$country, df$response, df$roundnumber)) else NA

    hh_visited <- if ("number_of_hh_visited" %in% names(df))
                     sum(as.numeric(df$number_of_hh_visited), na.rm=TRUE) else NA

    tp <- if ("u5_present" %in% names(df)) sum(as.numeric(df$u5_present), na.rm=TRUE) else NA
    tf <- if ("u5_fm"      %in% names(df)) sum(as.numeric(df$u5_fm),      na.rm=TRUE) else NA
    regional_cv  <- if (!is.na(tp) && !is.na(tf) && tp > 0) tf / tp else NA
    total_missed <- if (!is.na(tp) && !is.na(tf)) pmax(tp - tf, 0) else NA

    mean_awareness <- if (!is.null(dfr) && "awareness_rate" %in% names(dfr))
                         mean(dfr$awareness_rate, na.rm=TRUE) else NA
    mean_risk <- if (!is.null(dfr) && "risk_score" %in% names(dfr))
                    mean(dfr$risk_score, na.rm=TRUE) else NA

    risk_counts <- list(critical=0, high=0, moderate=0)
    if (!is.null(dfr) && all(c("risk_class","country","province","district") %in% names(dfr))) {
      rc <- dfr %>%
        group_by(country, province, district) %>%
        summarise(rc = first(risk_class), .groups="drop") %>%
        count(rc)
      g <- function(x) { v <- rc$n[rc$rc == x]; if (length(v)) v else 0 }
      risk_counts <- list(critical=g("Critical"), high=g("High"), moderate=g("Moderate"))
    }

    fmt_pct <- function(x) if (is.na(x)) "\u2014" else paste0(round(x*100,1), "%")
    fmt_n   <- function(x) if (is.na(x)) "\u2014" else fmt_num(x)

    metric_box <- function(val, label, border_col) {
      div(style=paste0(
            "background:#fff;border:2px solid ",border_col,";border-radius:10px;",
            "padding:14px 10px;text-align:center;flex:1;min-width:150px;"),
        div(style=paste0("font-size:30.4px;font-weight:800;color:",border_col,";line-height:1.1;"), val),
        div(style="font-size:11.5px;font-weight:700;color:#3A3A3A;margin-top:4px;text-transform:uppercase;letter-spacing:.03em;",
            label)
      )
    }

    div(class="fade-up", style=paste0(
          "border-radius:16px;overflow:hidden;box-shadow:0 4px 20px rgba(16,24,40,.08);",
          "border-top:4px solid ",dark_blue,";margin-bottom:16px;background:",soft_grey,";"),
      div(style=paste0("background:#fff;padding:14px 20px;color:",dark_blue,";",
                       "border-bottom:1px solid rgba(16,24,40,.08);",
                       "display:flex;justify-content:space-between;align-items:flex-start;gap:10px;flex-wrap:wrap;"),
        div(
          div(style="font-size:16.8px;font-weight:800;letter-spacing:.02em;",
              "AFRO REGIONAL INDEPENDENT MONITORING INTELLIGENCE"),
          div(style="font-size:12.5px;color:#6C7A8D;margin-top:2px;",
              paste0("Generated: ", format(Sys.Date(), "%d %b %Y"), "  |  Live filtered snapshot"))
        ),
        div(style="display:flex;gap:6px;height:fit-content;",
          tags$button(type="button", onclick=sprintf("imCaptureExec('%s')", session$ns("exec_kpi_capture")),
            style=paste0("background:transparent;border:1px solid ",dark_blue,";color:",dark_blue,";font-size:11.2px;padding:2px 10px;border-radius:6px;cursor:pointer;"),
            bs_icon("camera-fill", class="me-1"), "PNG"),
          downloadButton(session$ns("dl_exec_snapshot"), tagList(bs_icon("download", class="me-1"), "CSV"),
            style=paste0("background:transparent;border:1px solid ",dark_blue,";color:",dark_blue,";font-size:11.2px;padding:2px 10px;border-radius:6px;height:fit-content;"))
        )
      ),
      div(style="padding:18px 20px;",
        div(style="display:flex;gap:14px;flex-wrap:wrap;margin-bottom:14px;",
          metric_box(fmt_n(hh_visited),    "Households visited", who_blue),
          metric_box(fmt_pct(regional_cv), "Regional coverage",  who_blue),
          metric_box(fmt_n(total_missed),  "Missed children",    alert_red)
        ),
        div(style="display:flex;gap:14px;flex-wrap:wrap;margin-bottom:16px;",
          metric_box(fmt_pct(mean_awareness), "Caregiver awareness", gold),
          metric_box(if (is.na(mean_risk)) "\u2014" else round(mean_risk,1), "Mean risk score", alert_red),
          metric_box(if (is.na(n_rounds))  "\u2014" else n_rounds,           "Campaign rounds", dark_blue)
        ),
        div(style=paste0("font-size:13.1px;font-weight:700;color:",dark_blue,";margin-bottom:4px;"),
            paste0("Countries analysed: ", n_country, "  |  Districts analysed: ", n_district)),
        div(style="font-size:11.8px;color:#3A3A3A;margin-bottom:10px;",
            paste0("Critical events: ", risk_counts$critical,
                   "  |  High-risk events: ", risk_counts$high,
                   "  |  Moderate-risk events: ", risk_counts$moderate)),
        div(style="font-size:10.9px;color:#808080;",
            "Source: AFRO Independent Monitoring repository | District uniqueness: Country + Province + District")
      )
    )
  })
  # ?? Overview filtered data ????????????????????????????????????????????????????
  ov_data <- reactive({
    df <- rv$data; if (is.null(df)) return(NULL)

    if (!is.null(input$ov_block) && input$ov_block != "All" && "afro_block" %in% names(df))
      df <- df[!is.na(df$afro_block) & df$afro_block == input$ov_block, ]

    if (!is.null(input$ov_country) && input$ov_country != "All" && "country" %in% names(df))
      df <- df[!is.na(df$country) & df$country == input$ov_country, ]

    if (!is.null(input$ov_province) && input$ov_province != "All" && "province" %in% names(df))
      df <- df[!is.na(df$province) & df$province == input$ov_province, ]

    if (!is.null(input$ov_district) && input$ov_district != "All" && "district" %in% names(df))
      df <- df[!is.na(df$district) & df$district == input$ov_district, ]

    if (!is.null(input$ov_vaccine) && input$ov_vaccine != "All" && "vaccine_type" %in% names(df))
      df <- df[!is.na(df$vaccine_type) & df$vaccine_type == input$ov_vaccine, ]

    if (!is.null(input$ov_response) && input$ov_response != "All" && "response" %in% names(df))
      df <- df[!is.na(df$response) & df$response == input$ov_response, ]

    if (!is.null(input$ov_round) && length(input$ov_round) > 0 && "roundnumber" %in% names(df))
      df <- df[!is.na(df$roundnumber) & as.character(df$roundnumber) %in% input$ov_round, ]

    # Date filter
    # NOTE: variable named "df_dates" (not "dates") to avoid masking lubridate::dates generic
    dc    <- date_col_of(df)
    dtype <- input$ov_date_type %||% "All time"
    if (!is.na(dc) && dtype != "All time" && nrow(df) > 0) {
      df_dates <- suppressWarnings(as.Date(df[[dc]]))
      max_d    <- suppressWarnings(max(df_dates, na.rm=TRUE))

      if (!is.na(max_d) && is.finite(as.numeric(max_d))) {
        if (dtype == "Last N months" && !is.null(input$ov_n_months)) {
          cutoff <- max_d %m-% months(as.integer(input$ov_n_months))
          df <- df[!is.na(df_dates) & df_dates >= cutoff, ]

        } else if (dtype == "Last N years" && !is.null(input$ov_n_years)) {
          cutoff <- max_d %m-% years(as.integer(input$ov_n_years))
          df <- df[!is.na(df_dates) & df_dates >= cutoff, ]

        } else if (dtype == "Date range" && !is.null(input$ov_daterange)) {
          df <- df[!is.na(df_dates) & df_dates >= input$ov_daterange[1] &
                     df_dates <= input$ov_daterange[2], ]

        } else if (dtype == "Last N consecutive rounds" &&
                   !is.null(input$ov_n_rounds) && "roundnumber" %in% names(df)) {
          dc_local <- dc
          recent <- df %>%
            mutate(.tmp_d = as.Date(.data[[dc_local]])) %>%
            group_by(roundnumber) %>%
            summarise(last_d = max(.tmp_d, na.rm=TRUE), .groups="drop") %>%
            arrange(desc(last_d)) %>%
            slice_head(n=as.integer(input$ov_n_rounds)) %>%
            pull(roundnumber)
          df <- df[!is.na(df$roundnumber) & df$roundnumber %in% recent, ]

        } else if (dtype == "Last N rounds in last 12 months" &&
                   !is.null(input$ov_n_rounds_12m) && "roundnumber" %in% names(df)) {
          dc_local  <- dc
          cutoff_12m <- max_d %m-% months(12L)
          recent <- df %>%
            mutate(.tmp_d = as.Date(.data[[dc_local]])) %>%
            filter(.tmp_d >= cutoff_12m) %>%          # restrict to last 12 months first
            group_by(roundnumber) %>%
            summarise(last_d = max(.tmp_d, na.rm=TRUE), .groups="drop") %>%
            arrange(desc(last_d)) %>%
            slice_head(n=as.integer(input$ov_n_rounds_12m)) %>%
            pull(roundnumber)
          df <- df[!is.na(df$roundnumber) & df$roundnumber %in% recent, ]
        }
      }
    }
    if (nrow(df) == 0) return(NULL)
    df
  })

  # %||% helper
  `%||%` <- function(a,b) if (!is.null(a)) a else b

  # ?? Overview: dynamic cascade observers ??????????????????????????????????????
  # Block ? Country
  observeEvent(input$ov_block, {
    req(rv$data); df <- rv$data
    if (!"country" %in% names(df)) return()
    ctrs <- if (input$ov_block == "All" || !"afro_block" %in% names(df))
      sort(unique(df$country[!is.na(df$country)]))
    else
      df %>% filter(afro_block==input$ov_block, !is.na(country)) %>%
             pull(country) %>% unique() %>% sort()
    if (!is_admin() && length(ctrs) == 1L) {
      # single-country user: the block choice never un-selects their own country
      provs <- sort(unique(df$province[!is.na(df$province)]))
      dists <- sort(unique(df$district[!is.na(df$district)]))
      updateSelectInput(session,"ov_country",  choices=c("All",ctrs),  selected=ctrs)
      updateSelectInput(session,"ov_province", choices=c("All",provs), selected="All")
      updateSelectInput(session,"ov_district", choices=c("All",dists), selected="All")
      return()
    }
    updateSelectInput(session,"ov_country",  choices=c("All",ctrs), selected="All")
    updateSelectInput(session,"ov_province", choices=c("All"),       selected="All")
    updateSelectInput(session,"ov_district", choices=c("All"),       selected="All")
  }, ignoreInit=TRUE)

  # Country ? Province
  observeEvent(input$ov_country, {
    req(rv$data); df <- rv$data
    if (input$ov_country == "All") {
      updateSelectInput(session,"ov_province",choices=c("All"),selected="All")
      updateSelectInput(session,"ov_district",choices=c("All"),selected="All")
      return()
    }
    df2 <- df
    if (!is.null(input$ov_block) && input$ov_block!="All" && "afro_block" %in% names(df2))
      df2 <- df2[!is.na(df2$afro_block) & df2$afro_block==input$ov_block, ]
    df2 <- df2[!is.na(df2$country) & df2$country==input$ov_country, ]
    provs <- sort(unique(df2$province[!is.na(df2$province)]))
    dists <- sort(unique(df2$district[!is.na(df2$district)]))
    updateSelectInput(session,"ov_province",choices=c("All",provs),selected="All")
    updateSelectInput(session,"ov_district",choices=c("All",dists),selected="All")   # all districts of the country until a province is chosen
  }, ignoreInit=TRUE)

  # Province ? District
  observeEvent(input$ov_province, {
    req(rv$data); df <- rv$data
    if (input$ov_province == "All") {
      dch <- "All"
      if (!is.null(input$ov_country) && input$ov_country != "All" && "country" %in% names(df) && "district" %in% names(df)) {
        d0 <- df[!is.na(df$country) & df$country == input$ov_country, ]
        dch <- c("All", sort(unique(d0$district[!is.na(d0$district)])))
      }
      updateSelectInput(session,"ov_district",choices=dch,selected="All"); return()
    }
    df2 <- df
    if (!is.null(input$ov_country) && input$ov_country!="All" && "country" %in% names(df2))
      df2 <- df2[!is.na(df2$country) & df2$country==input$ov_country, ]
    df2 <- df2[!is.na(df2$province) & df2$province==input$ov_province, ]
    dists <- sort(unique(df2$district[!is.na(df2$district)]))
    updateSelectInput(session,"ov_district",choices=c("All",dists),selected="All")
  }, ignoreInit=TRUE)

  # Populate Overview sidebar choices on data load
  observeEvent(rv$data, {
    req(rv$data); df <- rv$data
    ctrs <- sort(unique(df$country[!is.na(df$country)]))
    vacs <- sort(unique(df$vaccine_type[!is.na(df$vaccine_type)]))
    resp <- sort(unique(df$response[!is.na(df$response)]))
    rnds <- sort(unique(as.character(df$roundnumber[!is.na(df$roundnumber)])))

    updateSelectInput(session,"ov_country",  choices=c("All",ctrs))
    updateSelectInput(session,"ov_province", choices=c("All"))
    updateSelectInput(session,"ov_district", choices=c("All"))
    # A country user with a single country starts on that country, so Province and District are
    # already filled in (country -> province -> district cascade).
    if (!is_admin() && length(ctrs) == 1L)
      updateSelectInput(session,"ov_country", choices=c("All",ctrs), selected=ctrs)
    updateSelectInput(session,"ov_vaccine",  choices=c("All",vacs))
    updateSelectInput(session,"ov_response", choices=c("All",resp))
    updateSelectizeInput(session,"ov_round", choices=rnds, selected=NULL)

    dc <- date_col_of(df)
    if (!is.na(dc)) {
      v <- suppressWarnings(as.Date(df[[dc]])); v <- v[!is.na(v)]
      if (length(v)) updateDateRangeInput(session,"ov_daterange",
                                          start=min(v), end=max(v))
    }
  }, ignoreInit=FALSE)

  # Reset overview filters
  observeEvent(input$ov_reset, {
    req(rv$data); df <- rv$data
    ctrs <- sort(unique(df$country[!is.na(df$country)]))
    vacs <- sort(unique(df$vaccine_type[!is.na(df$vaccine_type)]))
    resp <- sort(unique(df$response[!is.na(df$response)]))
    rnds <- sort(unique(as.character(df$roundnumber[!is.na(df$roundnumber)])))
    updateSelectInput(session,"ov_block",    selected="All")
    if (!is_admin() && length(ctrs) == 1L) {
      # single-country user: reset returns to their own country with its provinces/districts listed
      provs <- sort(unique(df$province[!is.na(df$province)]))
      dists <- sort(unique(df$district[!is.na(df$district)]))
      updateSelectInput(session,"ov_country",  choices=c("All",ctrs),   selected=ctrs)
      updateSelectInput(session,"ov_province", choices=c("All",provs),  selected="All")
      updateSelectInput(session,"ov_district", choices=c("All",dists),  selected="All")
    } else {
      updateSelectInput(session,"ov_country",  choices=c("All",ctrs), selected="All")
      updateSelectInput(session,"ov_province", choices=c("All"),       selected="All")
      updateSelectInput(session,"ov_district", choices=c("All"),       selected="All")
    }
    updateSelectInput(session,"ov_vaccine",  choices=c("All",vacs), selected="All")
    updateSelectInput(session,"ov_response", choices=c("All",resp), selected="All")
    updateSelectizeInput(session,"ov_round", choices=rnds, selected=NULL)
    updateSelectInput(session,"ov_date_type", selected="All time")
    showNotification("All filters reset.", type="message", duration=3)
  })

  # Filter badge
  output$ov_filter_badge <- renderUI({
    active_filters <- c(
      if (!is.null(input$ov_block)    && input$ov_block    != "All")         "Block",
      if (!is.null(input$ov_country)  && input$ov_country  != "All")         "Country",
      if (!is.null(input$ov_province) && input$ov_province != "All")         "Province",
      if (!is.null(input$ov_district) && input$ov_district != "All")         "District",
      if (!is.null(input$ov_vaccine)  && input$ov_vaccine  != "All")         "Vaccine",
      if (!is.null(input$ov_response) && input$ov_response != "All")         "Response",
      if (!is.null(input$ov_round)    && length(input$ov_round) > 0)         "Rounds",
      if (!is.null(input$ov_date_type)&& input$ov_date_type != "All time")   "Date"
    )
    n   <- length(active_filters)
    df  <- ov_data()
    rows <- if (!is.null(df)) fmt_num(nrow(df)) else "\u2014"
    if (n == 0)
      div(style="color:rgba(255,255,255,.45);font-size:11.5px;margin-bottom:4px;",
          paste0("Showing all data (", rows, " records)"))
    else
      div(style="margin-bottom:4px;",
        div(style="color:rgba(255,255,255,.55);font-size:11.2px;",
            paste0(n," filter(s) active \u00B7 ",rows," records")),
        div(style="display:flex;flex-wrap:wrap;gap:4px;margin-top:4px;",
          lapply(active_filters, function(f)
            tags$span(class="badge",
                      style="background:rgba(0,201,200,.25);color:rgba(255,255,255,.9);
                             font-size:10.4px;padding:3px 7px;border-radius:6px;", f)))
      )
  })

  # Dynamic card titles
  output$chart_cv_title <- renderUI({
    geo <- c(input$ov_district, input$ov_province, input$ov_country)
    geo <- geo[!is.null(geo) & geo != "All"]
    if (length(geo))
      tagList(bs_icon("bar-chart-fill", class = "me-2", style = "vertical-align:-0.15em;"),
              "CV by Area in ", tags$b(geo[1]))
    else
      tagList(bs_icon("bar-chart-fill", class = "me-2", style = "vertical-align:-0.15em;"),
              "Coverage (CV) by Country")
  })
  output$chart_topbot_title <- renderUI({
    lv <- if (!is.null(input$ov_country) && input$ov_country != "All") "Province"
          else if (!is.null(input$ov_block) && input$ov_block != "All") "Country"
          else "Country"
    tagList(bs_icon("trophy-fill", class = "me-2", style = "vertical-align:-0.15em;"),
            paste0("Top 5 vs Bottom 5 (", lv, ")"))
  })
  # ?? Overview charts ??????????????????????????????????????????????????????????
  output$chart_cv <- renderPlotly({
    df <- ov_data(); req(df)
    # Decide grouping level based on active filters
    grp <- if (!is.null(input$ov_district) && input$ov_district != "All") "province"
           else if (!is.null(input$ov_province) && input$ov_province != "All") "district"
           else if (!is.null(input$ov_country)  && input$ov_country  != "All") "province"
           else "country"
    grp <- intersect(grp, names(df))
    if (!length(grp) || !all(c("u5_fm","u5_present") %in% names(df))) return(NULL)
    grp <- grp[1]

    smry <- df %>%
      group_by(.data[[grp]]) %>%
      summarise(cv=sum(as.numeric(u5_fm),na.rm=T)/
                   pmax(sum(as.numeric(u5_present),na.rm=T),1),
                n=n(), .groups="drop") %>%
      mutate(cv=pmin(cv,1),
             col=case_when(cv>=.9~C_$green, cv>=.8~C_$orange, TRUE~C_$red)) %>%
      arrange(cv) %>% slice_tail(n=35)

    plot_ly(smry, x=~cv, y=~reorder(.data[[grp]],cv), type="bar", orientation="h",
            marker=list(color=~col, line=list(width=0)),
            text=~paste0(fmt_pct(cv)," \u00B7 ",fmt_num(n)," records"),
            textposition="outside", insidetextanchor="start",
            hovertemplate="%{y}<br>CV: %{x:.1%}<extra></extra>") %>%
      im_layout(
             .title    = paste0("Coverage (CV) by ", tools::toTitleCase(grp)),
             .filename = "IM_cv_chart",
             xaxis=list(title="Coverage Rate", tickformat=".0%", range=c(0,1.15),
                        showgrid=TRUE, gridcolor="rgba(0,92,151,.07)"),
             yaxis=list(title="", tickfont=list(size=11)),
             margin=list(l=130,r=90,b=40),
             shapes=list(
               list(type="line",x0=.9,x1=.9,y0=-.5,y1=nrow(smry)-.5,
                    line=list(color=C_$green, dash="dot",width=1.5)),
               list(type="line",x0=.8,x1=.8,y0=-.5,y1=nrow(smry)-.5,
                    line=list(color=C_$orange,dash="dot",width=1.5))
             ))
  })

  output$chart_qc_pie <- renderPlotly({
    df <- ov_data(); req(df)
    qc_col <- intersect(c("qc_flag","reconciliation_flag"), names(df))[1]
    if (is.na(qc_col)) return(NULL)

    tbl <- df %>% count(!!sym(qc_col),name="n") %>% rename(flag=1) %>%
           filter(!is.na(flag)) %>% arrange(desc(n))

    pal <- case_when(tbl$flag=="OK" ~ C_$green,
                     grepl("review",tbl$flag,ignore.case=T) ~ C_$orange,
                     TRUE ~ C_$red)

    plot_ly(tbl, labels=~flag, values=~n, type="pie",
            marker=list(colors=pal, line=list(color="#F0F4F8",width=2)),
            textinfo="label+percent",
            hovertemplate="%{label}<br><b>%{value:,}</b> records<extra></extra>",
            hole=0.38) %>%
      im_layout(
             .title    = "QC Flag Distribution",
             .filename = "IM_qc_flags",
             showlegend=TRUE,
             legend=list(orientation="h",y=-.12,font=list(size=11)))
  })

  output$chart_trend <- renderPlotly({
    df <- ov_data(); req(df)
    dc <- date_col_of(df); if (is.na(dc)) return(NULL)

    has_cv_cols <- all(c("u5_fm","u5_present") %in% names(df))
    agg <- df %>%
      mutate(period = floor_date(as.Date(.data[[dc]]), "month")) %>%
      filter(!is.na(period), period >= as.Date("2020-01-01")) %>%
      group_by(period) %>%
      summarise(
        records = n(),
        cv = if (has_cv_cols)
               sum(as.numeric(u5_fm), na.rm=TRUE) /
               pmax(sum(as.numeric(u5_present), na.rm=TRUE), 1)
             else NA_real_,
        .groups = "drop"
      )

    p <- plot_ly(agg, x=~period)
    p <- add_bars(p, y=~records, name="Records",
                  marker=list(color=C_$blue, opacity=.75,
                              line=list(color=C_$blue2,width=.5)))
    if (!all(is.na(agg$cv)))
      p <- add_lines(p, y=~cv, name="CV", yaxis="y2",
                     line=list(color=C_$green, width=2.5, shape="spline"),
                     fill="tozeroy", fillcolor="rgba(39,174,96,.08)")

    im_layout(p,
           .title    = "Records & Coverage Trend Over Time",
           .filename = "IM_trend",
           yaxis=list(title="Records", showgrid=TRUE,
                      gridcolor="rgba(0,92,151,.07)"),
           yaxis2=list(title="CV", overlaying="y", side="right",
                       tickformat=".0%", range=c(0,1.1)),
           xaxis=list(title="", showgrid=FALSE),
           legend=list(orientation="h",y=-.22))
  })

  # ?? Block performance chart ???????????????????????????????????????????????????
  output$chart_block <- renderPlotly({
    df <- ov_data(); req(df)
    if (!all(c("u5_fm","u5_present") %in% names(df))) return(NULL)

    # If a specific block is selected, drill down to country level
    block_selected <- !is.null(input$ov_block) && input$ov_block != "All"

    if (block_selected && "country" %in% names(df)) {
      # Country-level bar chart for the selected block
      smry <- df %>%
        group_by(x_var = country) %>%
        summarise(
          cv      = sum(as.numeric(u5_fm),na.rm=T) /
                    pmax(sum(as.numeric(u5_present),na.rm=T),1),
          records = n(), .groups = "drop"
        ) %>%
        filter(!is.na(x_var)) %>%
        mutate(cv = pmin(cv, 1),
               col = case_when(cv>=.9~C_$green, cv>=.8~C_$orange, TRUE~C_$red)) %>%
        arrange(desc(cv))

      x_n    <- nrow(smry)
      x_title <- paste0("Countries in ", input$ov_block)
    } else {
      blocks <- c("LCB","WA","DRC","CEA","ESA")
      if (!"afro_block" %in% names(df)) return(NULL)
      smry <- df %>%
        filter(afro_block %in% blocks) %>%
        group_by(x_var = afro_block) %>%
        summarise(
          cv      = sum(as.numeric(u5_fm),na.rm=T) /
                    pmax(sum(as.numeric(u5_present),na.rm=T),1),
          records = n(), .groups = "drop"
        ) %>%
        mutate(cv = pmin(cv, 1),
               col = case_when(cv>=.9~C_$green, cv>=.8~C_$orange, TRUE~C_$red),
               x_var = factor(x_var, levels=blocks)) %>%
        arrange(x_var)

      x_n     <- 5
      x_title <- "AFRO Block"
    }

    if (!nrow(smry)) return(NULL)
    chart_title <- if (block_selected) paste0("CV by Country \u2014 ", input$ov_block)
                   else "CV Performance by AFRO Block"

    plot_ly(smry,
            x = ~x_var, y = ~cv, type = "bar",
            marker = list(color = ~col,
                          line  = list(color="rgba(255,255,255,.3)", width=1.5)),
            text  = ~paste0(fmt_pct(cv)),
            textposition = "outside",
            customdata = ~records,
            hovertemplate = "<b>%{x}</b><br>CV: %{y:.1%}<br>Records: %{customdata:,}<extra></extra>") %>%
      im_layout(
        .title    = chart_title,
        .filename = "IM_block_cv",
        xaxis  = list(title=x_title, showgrid=FALSE, tickangle=if(x_n>6) -35 else 0),
        yaxis  = list(title="Coverage Rate", tickformat=".0%",
                      range=c(0,1.18), showgrid=TRUE,
                      gridcolor="rgba(0,92,151,.07)"),
        shapes = list(
          list(type="line", x0=-.5, x1=x_n-.5, y0=.9, y1=.9,
               line=list(color=C_$green, dash="dot", width=2)),
          list(type="line", x0=-.5, x1=x_n-.5, y0=.8, y1=.8,
               line=list(color=C_$orange, dash="dot", width=1.5))
        ),
        annotations = list(
          list(x=x_n-.5, y=.9,  text="90% target", showarrow=FALSE,
               font=list(color=C_$green,  size=10), xanchor="right"),
          list(x=x_n-.5, y=.8,  text="80% min",    showarrow=FALSE,
               font=list(color=C_$orange, size=10), xanchor="right")
        )
      )
  })

  # ?? Alert: countries below 80% CV ????????????????????????????????????????????
  output$alert_low_cv <- renderUI({
    df <- ov_data(); req(df)
    if (!all(c("u5_fm","u5_present") %in% names(df))) return(
      div(class="text-muted p-3 small", "CV data not available."))

    # Drill-down: province when country selected, district when province selected
    grp <- if (!is.null(input$ov_province) && input$ov_province != "All") "district"
           else if (!is.null(input$ov_country) && input$ov_country != "All") "province"
           else "country"
    grp <- intersect(grp, names(df))[1]
    if (is.na(grp)) return(div(class="text-muted p-3 small", "Geographic data not available."))

    grp_label <- switch(grp, country="countries", province="provinces", district="districts", grp)

    low <- df %>%
      group_by(.data[[grp]]) %>%
      summarise(cv = sum(as.numeric(u5_fm),na.rm=T) /
                     pmax(sum(as.numeric(u5_present),na.rm=T),1),
                n  = n(), .groups="drop") %>%
      filter(!is.na(.data[[grp]]), cv < .8) %>%
      mutate(cv=pmin(cv,1)) %>%
      arrange(cv) %>%
      slice_head(n=10)

    if (!nrow(low)) return(
      div(class="d-flex align-items-center justify-content-center",
          style="height:160px;",
        div(
          div(style="text-align:center;",
              bs_icon("check-circle-fill", size="40px", style="color:#27AE60;")),
          div(style="color:#27AE60;font-weight:700;text-align:center;margin-top:8px;",
              paste0("All ", grp_label, " above 80% CV!"))
        )
      )
    )

    tagList(
      div(style="font-size:11.5px;color:var(--muted);font-weight:700;
                 letter-spacing:.06em;text-transform:uppercase;padding:8px 12px 4px;",
          paste0(nrow(low), " ", grp_label, " need attention")),
      lapply(seq_len(nrow(low)), function(i) {
        r   <- low[i,]
        pct <- round(r$cv * 100, 1)
        col <- if(pct < 60) C_$red else C_$orange
        div(style="padding:7px 12px;border-bottom:1px solid rgba(0,92,151,.05);
                   display:flex;align-items:center;justify-content:space-between;",
          div(style="font-size:13.1px;font-weight:600;", r[[grp]]),
          div(style="display:flex;align-items:center;gap:8px;",
            div(style=paste0("width:80px;height:6px;background:#eee;border-radius:3px;overflow:hidden;"),
              div(style=paste0("width:",pct,"%  ;height:100%;background:",col,
                               ";border-radius:3px;transition:.4s;"))
            ),
            div(style=paste0("font-size:12.8px;font-weight:700;color:",col,";min-width:38px;"),
                paste0(pct,"%"))
          )
        )
      })
    )
  })

  # ?? Top 5 / Bottom 5 performers ???????????????????????????????????????????????
  output$chart_top_bottom <- renderPlotly({
    df <- ov_data(); req(df)
    if (!all(c("u5_fm","u5_present") %in% names(df))) return(NULL)

    # Drill-down grouping
    grp <- if (!is.null(input$ov_province) && input$ov_province != "All") "district"
           else if (!is.null(input$ov_country) && input$ov_country != "All") "province"
           else "country"
    grp <- intersect(grp, names(df))[1]
    if (is.na(grp)) return(NULL)

    grp_label <- tools::toTitleCase(grp)

    smry <- df %>%
      group_by(.data[[grp]]) %>%
      summarise(cv = sum(as.numeric(u5_fm),na.rm=T) /
                     pmax(sum(as.numeric(u5_present),na.rm=T),1),
                n  = n(), .groups="drop") %>%
      filter(!is.na(.data[[grp]]), n >= 5) %>%
      mutate(cv = pmin(cv,1)) %>%
      arrange(cv)

    if (nrow(smry) < 2) return(NULL)

    n_each <- min(5, floor(nrow(smry)/2))
    top_n  <- tail(smry, n_each) %>% mutate(grp_label=paste0("Top ", n_each))
    bot_n  <- head(smry, n_each) %>% mutate(grp_label=paste0("Bottom ", n_each))
    tbl    <- bind_rows(bot_n, top_n)
    col_map <- setNames(c(C_$green, C_$red),
                        c(paste0("Top ", n_each), paste0("Bottom ", n_each)))

    plot_ly(tbl,
            x = ~cv, y = ~reorder(.data[[grp]], cv), type = "bar", orientation = "h",
            color = ~grp_label, colors = col_map,
            marker = list(line=list(width=0)),
            text   = ~paste0(fmt_pct(cv)),
            textposition = "outside",
            hovertemplate = paste0("%{y}<br>CV: %{x:.1%}<br>Records: %{customdata:,}<extra></extra>"),
            customdata = ~n) %>%
      im_layout(
        .title    = paste0("Top vs Bottom ", grp_label, "s \u2014 Coverage"),
        .filename = "IM_top_bottom",
        xaxis  = list(title=paste0("Coverage Rate by ", grp_label), tickformat=".0%",
                      range=c(0,1.18), showgrid=TRUE,
                      gridcolor="rgba(0,92,151,.07)"),
        yaxis  = list(title="", tickfont=list(size=11)),
        legend = list(orientation="h", y=-.18),
        margin = list(l=110, r=70, b=30),
        shapes = list(
          list(type="line", x0=.9,x1=.9,y0=-.5,y1=nrow(tbl)-.5,
               line=list(color=C_$green,dash="dot",width=1.5))
        )
      )
  })

  # ?? Heatmap: geo-level ? round ????????????????????????????????????????????????
  output$chart_heatmap <- renderPlotly({
    df <- ov_data(); req(df)
    if (!all(c("u5_fm","u5_present") %in% names(df))) return(NULL)

    # Drill-down Y-axis grouping
    grp_y <- if (!is.null(input$ov_province) && input$ov_province != "All") "district"
             else if (!is.null(input$ov_country) && input$ov_country != "All") "province"
             else "country"
    grp_y <- intersect(grp_y, names(df))[1]
    if (is.na(grp_y)) return(NULL)

    y_label <- tools::toTitleCase(grp_y)

    # X-axis: use roundnumber if available, else quarter
    if ("roundnumber" %in% names(df) && !all(is.na(df$roundnumber))) {
      agg <- df %>%
        filter(!is.na(.data[[grp_y]]), !is.na(roundnumber)) %>%
        group_by(geo=.data[[grp_y]], round=as.character(roundnumber)) %>%
        summarise(cv=sum(as.numeric(u5_fm),na.rm=T)/
                       pmax(sum(as.numeric(u5_present),na.rm=T),1),
                  .groups="drop") %>%
        mutate(cv=pmin(pmax(cv,0),1))
      x_title <- "Campaign Round"
    } else {
      dc <- date_col_of(df)
      if (is.na(dc)) return(NULL)
      agg <- df %>%
        filter(!is.na(.data[[grp_y]])) %>%
        mutate(round=format(floor_date(as.Date(.data[[dc]]),"quarter"), "%Y Q%q")) %>%
        filter(!is.na(round)) %>%
        group_by(geo=.data[[grp_y]], round) %>%
        summarise(cv=sum(as.numeric(u5_fm),na.rm=T)/
                       pmax(sum(as.numeric(u5_present),na.rm=T),1),
                  .groups="drop") %>%
        mutate(cv=pmin(pmax(cv,0),1))
      x_title <- "Quarter"
    }

    if (!nrow(agg)) return(NULL)

    # Pivot to matrix
    rounds <- sort(unique(agg$round))
    geos   <- agg %>% group_by(geo) %>%
              summarise(m=mean(cv,na.rm=T),.groups="drop") %>%
              arrange(m) %>% pull(geo)

    mat <- matrix(NA_real_, nrow=length(geos), ncol=length(rounds),
                  dimnames=list(geos, rounds))
    for (i in seq_len(nrow(agg)))
      mat[agg$geo[i], agg$round[i]] <- agg$cv[i]

    text_mat <- matrix(
      ifelse(is.na(mat), "\u2014", paste0(round(mat*100,1),"%")),
      nrow=nrow(mat)
    )

    plot_ly(
      x = rounds, y = geos,
      z = mat,
      type = "heatmap",
      colorscale = list(
        c(0,   "#C0392B"),
        c(0.5, "#E8730A"),
        c(0.8, "#F1C40F"),
        c(0.9, "#27AE60"),
        c(1,   "#1A7A44")
      ),
      zmin=0, zmax=1,
      text = text_mat, hovertemplate=paste0("%{y} \u00B7 %{x}<br>CV: %{text}<extra></extra>"),
      colorbar=list(title="CV",tickformat=".0%",
                    len=0.8, thickness=14,
                    tickvals=c(0,.5,.8,.9,1))
    ) %>%
      im_layout(
        .title    = paste0("CV Heatmap: ", y_label, " \u00D7 ", x_title),
        .filename = "IM_heatmap",
        xaxis  = list(title=x_title, tickangle=-35, showgrid=FALSE),
        yaxis  = list(title=y_label, showgrid=FALSE, tickfont=list(size=11)),
        margin = list(l=120, r=80, b=60)
      )
  })

  # ?? CV95 proportion table (Country ? Vaccine ? Month) ???????????????????????
  cv95_tbl <- reactive({
    df <- ov_data(); req(df)
    dc <- date_col_of(df)
    if (is.na(dc)) return(NULL)
    if (!all(c("u5_fm","u5_present") %in% names(df))) return(NULL)

    uid_cols <- intersect(c("country","province","district"), names(df))
    row_grp  <- intersect(c("country","vaccine_type"), names(df))
    if (!"country" %in% row_grp) return(NULL)

    df2 <- df %>%
      mutate(
        cam_date    = as.Date(.data[[dc]]),
        yr          = lubridate::year(cam_date),
        mo          = lubridate::month(cam_date),
        # Always English: "Nov '25" style
        period      = paste0(month.abb[lubridate::month(cam_date)], " '",
                             substr(as.character(lubridate::year(cam_date)), 3, 4)),
        period_key  = paste0(lubridate::year(cam_date),
                             sprintf("%02d", lubridate::month(cam_date))),
        district_id = do.call(paste, c(as.list(.[uid_cols]), list(sep="|")))
      ) %>%
      filter(!is.na(period), !is.na(district_id))

    df2 %>%
      group_by(across(all_of(c(row_grp, "yr", "mo", "period", "period_key", "district_id")))) %>%
      summarise(
        dist_cv = sum(as.numeric(u5_fm), na.rm=TRUE) /
                  pmax(sum(as.numeric(u5_present), na.rm=TRUE), 1),
        .groups = "drop"
      ) %>%
      group_by(across(all_of(c(row_grp, "yr", "mo", "period", "period_key")))) %>%
      summarise(
        n_dist    = n_distinct(district_id),
        n_cv95    = sum(dist_cv >= 0.95),
        prop_cv95 = n_cv95 / n_dist,
        .groups   = "drop"
      ) %>%
      arrange(period_key)
  })

  output$chart_cv95 <- renderPlotly({
    tbl <- cv95_tbl(); req(tbl)
    if (!nrow(tbl)) return(NULL)

    row_grp <- intersect(c("country","vaccine_type"), names(tbl))

    tbl <- tbl %>%
      mutate(row_lbl = if (length(row_grp) > 1)
               paste0(.data[[row_grp[1]]], "  \u00B7  ", .data[[row_grp[2]]])
             else
               .data[[row_grp[1]]])

    row_order    <- tbl %>% distinct(row_lbl) %>% arrange(row_lbl) %>% pull(row_lbl)
    period_order <- tbl %>% distinct(period_key, period) %>%
                    arrange(period_key) %>% pull(period)

    rows    <- rev(row_order)   # alphabetical top-to-bottom
    periods <- period_order

    # Build value & count matrices
    mat <- matrix(NA_real_, nrow=length(rows), ncol=length(periods),
                  dimnames=list(rows, periods))
    cnt <- matrix("",       nrow=length(rows), ncol=length(periods),
                  dimnames=list(rows, periods))

    for (i in seq_len(nrow(tbl))) {
      r <- tbl$row_lbl[i]; p <- tbl$period[i]
      mat[r, p] <- tbl$prop_cv95[i]
      cnt[r, p] <- paste0(tbl$n_cv95[i], " / ", tbl$n_dist[i], " districts")
    }

    hover_mat <- matrix("", nrow=length(rows), ncol=length(periods))
    for (ri in seq_along(rows))
      for (ci in seq_along(periods)) {
        v <- mat[rows[ri], periods[ci]]
        hover_mat[ri, ci] <- if (!is.na(v))
          paste0(rows[ri], "<br>", periods[ci],
                 "<br><b>", round(v*100,1), "%</b> of districts \u226595% CV",
                 "<br>", cnt[rows[ri], periods[ci]])
        else paste0(rows[ri], "<br>", periods[ci], "<br>No data")
      }

    # Bold percentage annotations inside each filled cell
    annots <- list()
    for (ri in seq_along(rows)) {
      for (ci in seq_along(periods)) {
        v <- mat[rows[ri], periods[ci]]
        if (!is.na(v)) {
          # Light text on dark cells (red/green), dark text on yellow cells
          txt_col <- if (v < 0.78 || v >= 0.95) "white" else "#1A2637"
          annots <- c(annots, list(list(
            x         = periods[ci],
            y         = rows[ri],
            text      = paste0("<b>", round(v * 100, 1), "%</b>"),
            showarrow = FALSE,
            font      = list(size = 11, color = txt_col, family = "Inter, sans-serif")
          )))
        }
      }
    }

    plot_ly(
      x    = periods, y = rows,
      z    = mat,
      type = "heatmap",
      text = hover_mat,
      hovertemplate = "%{text}<extra></extra>",
      # Sharp threshold colour scale: <80 red | 80-95 amber | ?95 green
      colorscale = list(
        c(0,     "#B71C1C"),   # 0 %  deep red
        c(0.599, "#E53935"),   # 59 % red
        c(0.600, "#E53935"),
        c(0.799, "#EF6C00"),   # 79 % orange-red
        c(0.800, "#FFB300"),   # 80 % amber threshold
        c(0.944, "#FFD600"),   # 94 % yellow
        c(0.945, "#2E7D32"),   # 95 % green threshold
        c(1,     "#1B5E20")    # 100 % deep green
      ),
      zmin     = 0, zmax = 1,
      xgap     = 3, ygap = 3,   # modern tile-gap look
      colorbar = list(
        title      = list(text = "% Districts<br>\u2265 95% CV",
                          font = list(size = 11, color = "#4A5568")),
        tickformat = ".0%",
        len        = 0.65, thickness = 12,
        outlinewidth = 0,
        tickvals   = c(0, 0.80, 0.95, 1),
        ticktext   = c("0 %", "80 %", "95 %", "100 %"),
        tickfont   = list(size = 10)
      )
    ) %>%
      im_layout(
        .title    = "% of Districts Achieving \u2265 95% CV \u2014 Country \u00B7 Vaccine Type \u00B7 Month",
        .filename = "IM_cv95_heatmap",
        xaxis      = list(title      = "",
                          tickangle  = -40,
                          showgrid   = FALSE,
                          tickfont   = list(size = 11, color = "#4A5568", family = "Inter")),
        yaxis      = list(title      = "",
                          showgrid   = FALSE,
                          tickfont   = list(size = 10, color = "#1A2637", family = "Inter")),
        margin     = list(l = 210, r = 80, t = 10, b = 70),
        annotations = annots,
        plot_bgcolor  = "#F8FAFC",   # very light blue-grey tile background
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })

  # ?? Risk distribution donut ???????????????????????????????????????????????????
  output$chart_risk_dist <- renderPlotly({
    df <- ov_data(); req(df)
    dfr <- tryCatch(with_risk(df), error=function(e) NULL)
    if (is.null(dfr) || !"risk_class" %in% names(dfr)) return(NULL)

    tbl <- dfr %>%
      group_by(country, province, district) %>%
      summarise(risk_class=first(risk_class), .groups="drop") %>%
      count(risk_class) %>%
      mutate(
        risk_class = factor(risk_class, levels=c("Critical","High","Moderate","Low")),
        col = case_when(
          risk_class=="Critical" ~ C_$red,
          risk_class=="High"     ~ C_$orange,
          risk_class=="Moderate" ~ C_$blue,
          TRUE                   ~ C_$green
        )
      ) %>% arrange(risk_class)

    plot_ly(tbl, labels=~risk_class, values=~n, type="pie", hole=0.52,
            marker=list(colors=~col, line=list(color="#F0F4F8", width=3)),
            textinfo="label+percent",
            hovertemplate="<b>%{label}</b><br>%{value:,} districts<br>%{percent}<extra></extra>") %>%
      im_layout(
        .title    = "District Risk Classification",
        .filename = "IM_risk_dist",
        showlegend=TRUE,
        legend=list(orientation="h", y=-.15, font=list(size=11)),
        annotations=list(list(
          text=paste0("<b>",sum(tbl$n,na.rm=TRUE),"</b><br>districts"),
          x=0.5, y=0.5, showarrow=FALSE,
          font=list(size=14, color=C_$dark)
        ))
      )
  })

  # ?? SM Quadrant bubble chart (star chart) ?????????????????????????????????????
  output$chart_sm_quadrant <- renderPlotly({
    df <- ov_data(); req(df)
    dfr <- tryCatch(with_risk(df), error=function(e) NULL)
    if (is.null(dfr) || !"awareness_rate" %in% names(dfr)) return(
      plotly_empty() %>% im_layout() %>%
        add_annotations(text="\u26A0\uFE0F Awareness data (care_giver_informed_sia) not available in this dataset",
                        showarrow=FALSE, font=list(size=13, color=C_$muted))
    )
    if (all(is.na(dfr$awareness_rate))) return(
      plotly_empty() %>% im_layout() %>%
        add_annotations(text="\u26A0\uFE0F Awareness data is all NA \u2014 column exists but has no values",
                        showarrow=FALSE, font=list(size=13, color=C_$muted))
    )

    grp_cols <- intersect(c("country","province","district"), names(dfr))
    s <- dfr %>%
      group_by(across(all_of(grp_cols))) %>%
      summarise(
        cv_d           = sum(as.numeric(u5_fm),na.rm=T)/pmax(sum(as.numeric(u5_present),na.rm=T),1),
        awareness_rate = mean(awareness_rate, na.rm=TRUE),
        missed_child   = sum(as.numeric(missed_child), na.rm=TRUE),
        .groups="drop"
      ) %>%
      filter(!is.na(awareness_rate), !is.na(cv_d),
             is.finite(awareness_rate), is.finite(cv_d)) %>%
      mutate(
        # Labels match generate_afro_im_intelligence_report() exactly
        quadrant = case_when(
          awareness_rate <  0.80 & cv_d <  0.90 ~ "Low awareness + low coverage",
          awareness_rate <  0.80 & cv_d >= 0.90 ~ "Low awareness but acceptable coverage",
          awareness_rate >= 0.80 & cv_d <  0.90 ~ "High awareness but low coverage",
          TRUE                                   ~ "High awareness + good coverage"
        ),
        sz  = pmax(pmin(sqrt(missed_child + 1) * 1.5, 28), 5),
        # Advocacy message per district (mirrors priority_districts advocacy_message)
        advocacy = case_when(
          awareness_rate < 0.80 ~
            "Pre-campaign social mobilisation and household awareness should be intensified.",
          TRUE ~
            "Continue routine monitoring and rapid corrective action."
        ),
        lbl = paste0(
          if ("district" %in% names(.)) district else "",
          if ("country"  %in% names(.)) paste0(" (", country, ")") else "",
          "<br>Awareness: ", fmt_pct(awareness_rate),
          " | CV: ", fmt_pct(cv_d),
          "<br>Missed: ", fmt_num(missed_child),
          "<br><i>", advocacy, "</i>")
      )

    if (nrow(s) == 0) return(NULL)
    s <- slice_sample(s, n = min(2000, nrow(s)))

    # Quadrant definitions ? labels and colours aligned to report script
    q_defs <- list(
      list(q   = "Low awareness + low coverage",
           col = C_$red,
           adv = "Pre-campaign social mobilisation and household awareness should be intensified."),
      list(q   = "Low awareness but acceptable coverage",
           col = C_$orange,
           adv = "Pre-campaign social mobilisation and household awareness should be intensified."),
      list(q   = "High awareness but low coverage",
           col = C_$purple,
           adv = "Investigate operational barriers; strengthen vaccinator accountability and microplan."),
      list(q   = "High awareness + good coverage",
           col = C_$blue,
           adv = "Continue routine monitoring and rapid corrective action.")
    )

    # Build one trace per quadrant (enables named legend + correct PNG export)
    p <- plot_ly()
    for (qd in q_defs) {
      sub <- s[s$quadrant == qd$q, ]
      if (!nrow(sub)) next
      p <- add_trace(p,
        data        = sub,
        x           = ~awareness_rate, y = ~cv_d,
        type        = "scatter", mode = "markers",
        name        = qd$q,
        legendgroup = qd$q,
        marker      = list(color   = qd$col,
                           size    = ~sz,
                           opacity = 0.72,
                           line    = list(color = "white", width = 0.8)),
        text              = ~lbl,
        hoverinfo         = "text",
        showlegend        = TRUE
      )
    }

    # Count per quadrant for annotation
    q_counts <- table(s$quadrant)
    ann_lbl <- function(q) {
      n <- if (q %in% names(q_counts)) q_counts[[q]] else 0
      paste0("<b>", q, "</b><br><span style='font-size:9px'>", n, " areas</span>")
    }

    # Advocacy annotation: quadrant with most missed children
    worst_q <- s %>%
      group_by(quadrant) %>%
      summarise(tot_missed = sum(missed_child, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(tot_missed)) %>%
      slice_head(n=1)

    adv_msg <- ""
    if (nrow(worst_q) > 0) {
      qd_match <- Filter(function(x) x$q == worst_q$quadrant[1], q_defs)
      if (length(qd_match))
        adv_msg <- paste0("<b>Priority quadrant:</b> ", worst_q$quadrant[1],
                          "<br>", qd_match[[1]]$adv)
    }

    im_layout(p,
      .title    = "Social Mobilisation Awareness vs Vaccination Coverage",
      .filename = "IM_sm_quadrant",
      xaxis = list(title = "Caregiver Awareness Rate (%)", tickformat = ".0%",
                   range = c(-.02, 1.05), showgrid = TRUE,
                   gridcolor = "rgba(0,92,151,.07)", zeroline = FALSE),
      yaxis = list(title = "Vaccination Coverage \u2014 CV (%)", tickformat = ".0%",
                   range = c(-.02, 1.08), showgrid = TRUE,
                   gridcolor = "rgba(0,92,151,.07)", zeroline = FALSE),
      showlegend = TRUE,
      shapes = list(
        list(type="line", x0=.8, x1=.8, y0=0, y1=1.08,
             line=list(color=C_$muted, dash="dot", width=1.5)),
        list(type="line", x0=0, x1=1.05, y0=.9, y1=.9,
             line=list(color=C_$muted, dash="dot", width=1.5))
      ),
      annotations = list(
        # Quadrant corner labels with counts (labels match report script)
        list(x=.39,  y=1.06, text=ann_lbl("Low awareness but acceptable coverage"),
             showarrow=FALSE, font=list(color=C_$orange, size=10),
             bgcolor="rgba(255,255,255,.82)", bordercolor=C_$orange,
             borderwidth=1, borderpad=4, xanchor="center"),
        list(x=.39,  y=.48,  text=ann_lbl("Low awareness + low coverage"),
             showarrow=FALSE, font=list(color=C_$red,    size=10),
             bgcolor="rgba(255,255,255,.82)", bordercolor=C_$red,
             borderwidth=1, borderpad=4, xanchor="center"),
        list(x=.92,  y=.48,  text=ann_lbl("High awareness but low coverage"),
             showarrow=FALSE, font=list(color=C_$purple, size=10),
             bgcolor="rgba(255,255,255,.82)", bordercolor=C_$purple,
             borderwidth=1, borderpad=4, xanchor="center"),
        list(x=.92,  y=1.06, text=ann_lbl("High awareness + good coverage"),
             showarrow=FALSE, font=list(color=C_$blue,   size=10),
             bgcolor="rgba(255,255,255,.82)", bordercolor=C_$blue,
             borderwidth=1, borderpad=4, xanchor="center"),
        # Threshold labels
        list(x=.79, y=1.04, text="Awareness threshold: 80%", showarrow=FALSE,
             textangle=-90, font=list(color=C_$muted, size=9), xanchor="right"),
        list(x=0.85, y=0.87, text="CV target: 90%",
             showarrow=FALSE, font=list(color="grey", size=9)),
        # Advocacy: sits right after the x-axis title, before the legend row
        list(x=1.0, y=-0.19, xref="paper", yref="paper",
             text="Advocacy message: low-awareness districts require intensified pre-campaign mobilisation",
             showarrow=FALSE, xanchor="right", yanchor="top",
             font=list(size=8, color="grey50"))
      ),
      # b=140 keeps y=-0.19 (advocacy) and y=-0.30 (legend) inside the figure
      # for both the 450px browser display and the 640px PNG download
      margin = list(l=60, r=70, t=50, b=140)
    ) %>%
      # Apply legend AFTER im_layout so it overrides plotly_base()'s legend key
      plotly::layout(legend = list(
        orientation = "h",
        x = 0.5, y = -0.30,
        xanchor = "center", yanchor = "top",
        bgcolor = "rgba(255,255,255,0)",
        borderwidth = 0,
        font = list(size = 7, color = C_$dark),
        tracegroupgap = 2
      )) %>%
      plotly::config(toImageButtonOptions = list(
        format   = "png",
        filename = "IM_sm_quadrant",
        height   = 660,
        width    = 960,
        scale    = 2
      ))
  })

  # ?? Root causes (Overview simplified) ????????????????????????????????????????
  output$chart_root_ov <- renderPlotly({
    df <- ov_data(); req(df)

    root_labels <- c(
      r_non_fm_absent                = "Absent",
      r_non_fm_nc                    = "Non-compliance",
      r_non_fm_hh_notvisited         = "House not visited",
      r_non_fm_hh_notrevisited       = "House not revisited",
      r_non_fm_sleep                 = "Sleeping child",
      r_non_fm_vaccinatedroutine     = "Vaccinated (routine)",
      r_non_fm_other                 = "Other reason",
      r_non_fm_vaccinated_but_not_fm = "Vaccinated but not FM",
      r_non_fm_child_is_a_visitor    = "Visitor child",
      r_non_fm_childnotborn          = "Child not born",
      r_non_fm_security              = "Security"
    )

    # Per-cause advocacy text aligned to report recommendation cards
    advocacy_map <- c(
      "Non-compliance"        = "Intensify pre-campaign social mobilisation; engage community and religious leaders.",
      "House not visited"     = "Review vaccinator route plans and strengthen daily supervisory accountability.",
      "House not revisited"   = "'House not revisited' should become a campaign accountability indicator \u2014 strengthen revisit tracking and defaulter follow-up.",
      "Absent"                = "Deploy fixed-post strategies and time-flexible schedules to reach absent caregivers.",
      "Sleeping child"        = "Train vaccinators on appropriate wake-up protocol for sleeping children.",
      "Vaccinated (routine)"  = "Link EPI records and clarify FM eligibility criteria to avoid missed-dose under-reporting.",
      "Vaccinated but not FM" = "Strengthen FM recording and reconciliation at team and district level.",
      "Visitor child"         = "Map and register migrant and transient populations in microplans.",
      "Child not born"        = "Update community microplans with current birth data and dynamic household registers.",
      "Security"              = "Engage traditional/religious leaders for humanitarian access corridors in insecure areas.",
      "Other reason"          = "Use district-level risk profiles to guide microplanning and corrective action."
    )

    # 2-group categorisation matching generate_afro_im_intelligence_report()
    # Priority group: the three behavioural/operational causes singled out in the report
    cat_priority <- c("Non-compliance", "House not visited", "House not revisited")
    # Everything else ? other missed-child driver

    cat_cols <- c(
      "Priority operational/behavioral issue" = "#C00000",   # alert_red (report)
      "Other missed-child driver"             = "#0093D5"    # who_blue  (report)
    )

    rcols <- intersect(names(root_labels), names(df))
    if (!length(rcols)) {
      rcols <- grep("r_non_fm|r_non_", names(df), value=TRUE, ignore.case=TRUE)
      rcols <- rcols[sapply(df[, rcols, drop=FALSE],
                            function(x) is.numeric(x) || is.integer(x))]
      if (!length(rcols)) return(
        plotly_empty() %>% im_layout() %>%
          add_annotations(text = "No root-cause columns (r_non_fm_*) found",
                          showarrow = FALSE, font = list(size=13, color=C_$muted))
      )
      labels <- sub("r_non_fm_|r_non_", "", rcols)
    } else {
      labels <- root_labels[rcols]
    }

    tots <- colSums(df[, rcols, drop=FALSE], na.rm=TRUE)
    tbl  <- data.frame(reason = as.character(labels),
                       total  = as.numeric(tots),
                       stringsAsFactors = FALSE) %>%
      filter(total > 0) %>%
      arrange(desc(total)) %>%
      slice_head(n = 12) %>%
      mutate(
        share    = total / sum(total),
        category = case_when(
          reason %in% cat_priority ~ "Priority operational/behavioral issue",
          TRUE                     ~ "Other missed-child driver"
        ),
        advocacy = advocacy_map[reason],
        lbl      = paste0(fmt_num(total), " (", round(share * 100, 1), "%)")
      )

    if (!nrow(tbl)) return(NULL)

    # Primary advocacy annotation ? recommendation from report script
    top_reason  <- tbl$reason[1]
    top_adv     <- tbl$advocacy[1]
    top_cat     <- tbl$category[1]
    top_col     <- cat_cols[top_cat]
    adv_ann_txt <- paste0(
      "<b>Priority cause \u2014 ", top_reason, ":</b> ",
      if (!is.na(top_adv)) top_adv else "Investigate and address underlying barriers"
    )
    # Fixed caption (from report p_root subtitle)
    caption_txt <- "Advocacy use: tailor corrective action by dominant cause \u2014 use IM risk ranking to focus supervision, partner support and rapid action."


    # Build one trace per category ? named legend entries in PNG download
    p <- plot_ly()
    for (cat_name in names(cat_cols)) {
      sub <- tbl[tbl$category == cat_name, ]
      if (!nrow(sub)) next
      adv_vec <- as.character(sub$advocacy)
      adv_vec[is.na(adv_vec)] <- "Investigate & address underlying barriers"
      p <- add_bars(p,
        data          = sub,
        x             = ~total,
        y             = ~reorder(reason, total),
        orientation   = "h",
        name          = cat_name,
        legendgroup   = cat_name,
        marker        = list(color = cat_cols[cat_name],
                             line  = list(width = 0)),
        text          = ~lbl,
        textposition  = "outside",
        cliponaxis    = FALSE,
        customdata    = adv_vec,
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Missed children: <b>%{x:,}</b><br>",
          "<i>Recommended action:</i> %{customdata}",
          "<extra></extra>"
        ),
        showlegend    = TRUE
      )
    }

    im_layout(p,
      .title    = "Root Causes of Missed Children",
      .filename = "IM_root_causes",
      barmode   = "overlay",
      xaxis     = list(title = "Missed Children", showgrid = TRUE,
                       gridcolor = "rgba(0,92,151,.07)",
                       range = list(0, max(tbl$total, na.rm=TRUE) * 1.25)),
      yaxis     = list(title = "", tickfont = list(size = 11)),
      showlegend = TRUE
    ) %>%
      # Apply legend, margin + annotations AFTER im_layout so plotly_base() doesn't win
      plotly::layout(
        legend = list(
          x = 1.01, y = 0.80, xanchor = "left", yanchor = "top",
          bgcolor = "rgba(255,255,255,.85)",
          bordercolor = "rgba(0,92,151,.12)", borderwidth = 1,
          font = list(size = 11, color = C_$dark),
          title = list(text = "<b>Category</b>", font = list(size = 11))
        ),
        margin = list(l = 170, r = 185, t = 46, b = 130),
        annotations = list(
          list(x = 0, y = -0.22, xref = "paper", yref = "paper",
               text = caption_txt, showarrow = FALSE, xanchor = "left",
               font = list(size = 9, color = "grey50")),
          list(x = 0, y = -0.32, xref = "paper", yref = "paper",
               text = adv_ann_txt, showarrow = FALSE, xanchor = "left",
               font = list(size = 9, color = "grey50"))
        )
      )
  })

  # ?? Country-level IM risk profile (ported from intelligence report Visual 3) ??
  output$chart_country_risk <- renderPlotly({
    df  <- ov_data(); req(df)
    dfr <- tryCatch(with_risk(df), error=function(e) NULL)
    if (is.null(dfr) || !all(c("risk_score","country") %in% names(dfr))) return(
      plotly_empty() %>% im_layout() %>%
        add_annotations(text = "Country risk profile unavailable \u2014 missing u5_present/u5_fm/country columns",
                        showarrow = FALSE, font = list(size=13, color=C_$muted))
    )

    tbl <- dfr %>%
      group_by(country) %>%
      summarise(
        mean_risk_score = mean(risk_score, na.rm = TRUE),
        total_missed    = sum(missed_child, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(mean_risk_score)) %>%
      mutate(
        risk_group = case_when(
          mean_risk_score >= 70 ~ "Critical",
          mean_risk_score >= 50 ~ "High",
          mean_risk_score >= 30 ~ "Moderate",
          TRUE                  ~ "Low"
        ),
        lbl = paste0("Missed: ", fmt_num(total_missed))
      ) %>%
      arrange(mean_risk_score)

    if (!nrow(tbl)) return(NULL)

    risk_cols <- c(Critical = C_$red, High = C_$orange, Moderate = C_$blue, Low = C_$green)

    p <- plot_ly()
    for (grp in names(risk_cols)) {
      sub <- tbl[tbl$risk_group == grp, ]
      if (!nrow(sub)) next
      p <- add_bars(p,
        data          = sub,
        x             = ~mean_risk_score,
        y             = ~reorder(country, mean_risk_score),
        orientation   = "h",
        name          = grp,
        legendgroup   = grp,
        marker        = list(color = risk_cols[[grp]], line = list(width = 0)),
        text          = ~lbl,
        textposition  = "outside",
        cliponaxis    = FALSE,
        hovertemplate = "<b>%{y}</b><br>Mean risk score: <b>%{x:.1f}</b><br>%{text}<extra></extra>",
        showlegend    = TRUE
      )
    }

    im_layout(p,
      .title    = "Country-Level IM Risk Profile",
      .filename = "IM_country_risk",
      barmode   = "overlay",
      xaxis     = list(title = "Mean Risk Score", showgrid = TRUE,
                       gridcolor = "rgba(0,92,151,.07)",
                       range = list(0, max(tbl$mean_risk_score, na.rm=TRUE) * 1.3)),
      yaxis     = list(title = "", tickfont = list(size = 11)),
      showlegend = TRUE
    ) %>%
      plotly::layout(
        legend = list(
          x = 1.01, y = 0.85, xanchor = "left", yanchor = "top",
          bgcolor = "rgba(255,255,255,.85)",
          bordercolor = "rgba(0,92,151,.12)", borderwidth = 1,
          font = list(size = 11, color = C_$dark),
          title = list(text = "<b>Risk Tier</b>", font = list(size = 11))
        ),
        margin = list(l = 150, r = 185, t = 46, b = 60),
        annotations = list(
          list(x = 0, y = -0.14, xref = "paper", yref = "paper",
               text = "Focus countries with both high risk score and high missed-child burden.",
               showarrow = FALSE, xanchor = "left", font = list(size = 9, color = "grey50"))
        )
      )
  })

  # ?? Operational failure profile (ported from intelligence report Visual 5) ???
  output$chart_op_failure <- renderPlotly({
    df <- ov_data(); req(df)

    needed <- c("r_non_fm_absent","r_non_fm_nc","r_non_fm_hh_notvisited","r_non_fm_hh_notrevisited")
    if (!all(needed %in% names(df))) return(
      plotly_empty() %>% im_layout() %>%
        add_annotations(text = "Operational failure profile unavailable \u2014 missing r_non_fm_* columns",
                        showarrow = FALSE, font = list(size=13, color=C_$muted))
    )

    grp_cols <- intersect(c("country","province","district","response","roundnumber"), names(df))
    if (!length(grp_cols)) grp_cols <- intersect(c("country","district"), names(df))
    mc_col <- if ("missed_child" %in% names(df)) "missed_child" else NA

    op <- df %>%
      group_by(across(all_of(grp_cols))) %>%
      summarise(
        missed_child = if (!is.na(mc_col)) sum(.data[[mc_col]], na.rm = TRUE)
                       else sum(pmax(as.numeric(u5_present) - as.numeric(u5_fm), 0), na.rm = TRUE),
        absent               = sum(r_non_fm_absent, na.rm = TRUE),
        non_compliance       = sum(r_non_fm_nc, na.rm = TRUE),
        house_not_visited    = sum(r_non_fm_hh_notvisited, na.rm = TRUE),
        house_not_revisited  = sum(r_non_fm_hh_notrevisited, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        operational_failure_type = case_when(
          house_not_visited >= house_not_revisited &
            house_not_visited >= absent &
            house_not_visited >= non_compliance &
            house_not_visited > 0 ~ "Team did not visit households",
          house_not_revisited >= house_not_visited &
            house_not_revisited >= absent &
            house_not_revisited >= non_compliance &
            house_not_revisited > 0 ~ "Weak revisit / supervision failure",
          absent >= house_not_visited &
            absent >= house_not_revisited &
            absent >= non_compliance &
            absent > 0 ~ "Population absence / timing issue",
          non_compliance >= house_not_visited &
            non_compliance >= house_not_revisited &
            non_compliance >= absent &
            non_compliance > 0 ~ "Community resistance / refusal",
          missed_child == 0 ~ "No operational failure detected",
          TRUE ~ "Mixed / other operational issue"
        )
      )

    tbl <- op %>%
      filter(operational_failure_type != "No operational failure detected") %>%
      group_by(operational_failure_type) %>%
      summarise(total_missed = sum(missed_child, na.rm = TRUE), .groups = "drop") %>%
      filter(total_missed > 0) %>%
      arrange(desc(total_missed)) %>%
      mutate(lbl = fmt_num(total_missed))

    if (!nrow(tbl)) return(NULL)

    p <- plot_ly(tbl,
      x = ~total_missed,
      y = ~reorder(operational_failure_type, total_missed),
      type = "bar",
      orientation = "h",
      marker = list(color = C_$blue2, line = list(width = 0)),
      text = ~lbl,
      textposition = "outside",
      cliponaxis = FALSE,
      hovertemplate = "<b>%{y}</b><br>Missed children: <b>%{x:,}</b><extra></extra>"
    )

    im_layout(p,
      .title    = "Operational Failure Profile",
      .filename = "IM_op_failure",
      xaxis     = list(title = "Missed Children", showgrid = TRUE,
                       gridcolor = "rgba(0,92,151,.07)",
                       range = list(0, max(tbl$total_missed, na.rm=TRUE) * 1.25)),
      yaxis     = list(title = "", tickfont = list(size = 11))
    ) %>%
      plotly::layout(
        margin = list(l = 230, r = 80, t = 46, b = 60),
        annotations = list(
          list(x = 0, y = -0.16, xref = "paper", yref = "paper",
               text = "Use for regional discussion on team deployment, revisit tracking and supervision accountability.",
               showarrow = FALSE, xanchor = "left", font = list(size = 9, color = "grey50"))
        )
      )
  })

  # ?? Top priority districts for advocacy (ported from report Visual 2) ????????
  output$chart_priority_districts <- renderPlotly({
    df  <- ov_data(); req(df)
    dfr <- tryCatch(with_risk(df), error=function(e) NULL)
    if (is.null(dfr) || !all(c("risk_score","country","province","district") %in% names(dfr))) return(
      plotly_empty() %>% im_layout() %>%
        add_annotations(text = "Priority districts unavailable \u2014 missing risk inputs or country/province/district columns",
                        showarrow = FALSE, font = list(size=13, color=C_$muted))
    )

    tbl <- dfr %>%
      group_by(country, province, district) %>%
      summarise(
        mean_risk_score = mean(risk_score, na.rm = TRUE),
        total_missed    = sum(missed_child, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(mean_risk_score)) %>%
      arrange(desc(mean_risk_score)) %>%
      slice_head(n = 25) %>%
      mutate(
        district_label = paste(country, province, district, sep = " | "),
        priority = case_when(
          mean_risk_score >= 70 ~ "Priority 1",
          mean_risk_score >= 50 ~ "Priority 2",
          mean_risk_score >= 30 ~ "Priority 3",
          TRUE                  ~ "Routine"
        ),
        lbl = round(mean_risk_score, 1),
        missed_lbl = fmt_num(total_missed)
      )

    if (!nrow(tbl)) return(NULL)

    pr_cols <- c("Priority 1" = C_$red, "Priority 2" = C_$orange,
                 "Priority 3" = C_$blue, "Routine" = C_$green)

    p <- plot_ly()
    for (grp in names(pr_cols)) {
      sub <- tbl[tbl$priority == grp, ]
      if (!nrow(sub)) next
      p <- add_bars(p,
        data          = sub,
        x             = ~mean_risk_score,
        y             = ~reorder(district_label, mean_risk_score),
        orientation   = "h",
        name          = grp,
        legendgroup   = grp,
        marker        = list(color = pr_cols[[grp]], line = list(width = 0)),
        text          = ~lbl,
        textposition  = "outside",
        cliponaxis    = FALSE,
        customdata    = ~missed_lbl,
        hovertemplate = paste0(
          "<b>%{y}</b><br>Mean risk score: <b>%{x:.1f}</b><br>",
          "Missed children: <b>%{customdata}</b><extra></extra>"
        ),
        showlegend    = TRUE
      )
    }

    im_layout(p,
      .title    = "Top Priority Districts for Advocacy",
      .filename = "IM_priority_districts",
      barmode   = "overlay",
      xaxis     = list(title = "Mean Risk Score", showgrid = TRUE,
                       gridcolor = "rgba(0,92,151,.07)", range = list(0, 100)),
      yaxis     = list(title = "", tickfont = list(size = 10)),
      showlegend = TRUE
    ) %>%
      plotly::layout(
        legend = list(
          x = 1.01, y = 0.92, xanchor = "left", yanchor = "top",
          bgcolor = "rgba(255,255,255,.85)",
          bordercolor = "rgba(0,92,151,.12)", borderwidth = 1,
          font = list(size = 11, color = C_$dark),
          title = list(text = "<b>Priority</b>", font = list(size = 11))
        ),
        margin = list(l = 260, r = 185, t = 46, b = 60),
        annotations = list(
          list(x = 0, y = -0.05, xref = "paper", yref = "paper",
               text = "Use this list for prioritization, partner support and accountability tracking.",
               showarrow = FALSE, xanchor = "left", font = list(size = 9, color = "grey50"))
        )
      )
  })
  # ?? Per-chart CSV download handlers ??????????????????????????????????????????
  output$dl_ov_all <- downloadHandler(
    filename=function() paste0("IM_filtered_",Sys.Date(),".csv"),
    content =function(f) write.csv(snap(),f,row.names=FALSE))
  output$dl_block <- downloadHandler(
    filename=function() paste0("IM_block_",Sys.Date(),".csv"),
    content =function(f) {
      df <- snap()
      if ("afro_block"%in%names(df)&&!is.null(input$ov_block)&&input$ov_block!="All")
        df <- df[df$afro_block==input$ov_block,]
      write.csv(df,f,row.names=FALSE)
    })
  output$dl_risk_dist <- downloadHandler(
    filename=function() paste0("IM_risk_dist_",Sys.Date(),".csv"),
    content =function(f) write.csv(snap(),f,row.names=FALSE))
  output$dl_sm_quad <- downloadHandler(
    filename=function() paste0("IM_sm_quadrant_",Sys.Date(),".csv"),
    content =function(f) {
      df   <- snap()
      cols <- intersect(c("country","province","district","cv",
                          "mean_awareness_rate","sm_gap_flag","sm_priority_flag"),names(df))
      write.csv(df[,cols,drop=FALSE],f,row.names=FALSE)
    })
  output$dl_alert_cv <- downloadHandler(
    filename=function() paste0("IM_low_cv_alerts_",Sys.Date(),".csv"),
    content =function(f) {
      df <- snap()
      if ("cv"%in%names(df)) df <- df[!is.na(df$cv)&df$cv<0.8,]
      write.csv(df,f,row.names=FALSE)
    })
  output$dl_root_ov <- downloadHandler(
    filename=function() paste0("IM_root_causes_",Sys.Date(),".csv"),
    content =function(f) {
      df   <- snap()
      cols <- intersect(c("country","province","district","missed_child",
                          "qc_flag","abs_detail_flag","nc_detail_flag",
                          "reconciliation_flag"),names(df))
      write.csv(df[,cols,drop=FALSE],f,row.names=FALSE)
    })
  output$dl_cv <- downloadHandler(
    filename=function() paste0("IM_cv_",Sys.Date(),".csv"),
    content =function(f) {
      df   <- snap()
      cols <- intersect(c("country","province","district","roundnumber",
                          "cv","vaccine_type","response"),names(df))
      write.csv(df[,cols,drop=FALSE],f,row.names=FALSE)
    })
  output$dl_top_bottom <- downloadHandler(
    filename=function() paste0("IM_top_bottom_",Sys.Date(),".csv"),
    content =function(f) write.csv(snap(),f,row.names=FALSE))
  output$dl_trend <- downloadHandler(
    filename=function() paste0("IM_trend_",Sys.Date(),".csv"),
    content =function(f) {
      df   <- snap()
      cols <- intersect(c("country","province","district","roundnumber",
                          "round_start_date","cv","missed_child"),names(df))
      write.csv(df[,cols,drop=FALSE],f,row.names=FALSE)
    })
  output$dl_heatmap <- downloadHandler(
    filename=function() paste0("IM_heatmap_",Sys.Date(),".csv"),
    content =function(f) write.csv(snap(),f,row.names=FALSE))
  output$dl_qc_pie <- downloadHandler(
    filename=function() paste0("IM_qc_flags_",Sys.Date(),".csv"),
    content =function(f) {
      df <- tryCatch(ov_data(), error=function(e) snap())
      if (is.null(df)) df <- snap()
      write.csv(df,f,row.names=FALSE)
    })
  output$dl_cv95 <- downloadHandler(
    filename=function() paste0("IM_cv95_",Sys.Date(),".csv"),
    content =function(f) {
      tbl <- tryCatch(cv95_tbl(), error=function(e) NULL)
      if (is.null(tbl)) tbl <- data.frame()
      write.csv(tbl,f,row.names=FALSE)
    })


  # ???????????????????????????????????????????????????????????????????????????
  output$dl_exec_snapshot <- downloadHandler(
    filename=function() paste0("IM_executive_snapshot_",Sys.Date(),".csv"),
    content =function(f) {
      df  <- snap()
      dfr <- tryCatch(with_risk(df), error=function(e) NULL)

      n_country  <- if ("country" %in% names(df)) n_distinct(df$country, na.rm=TRUE) else NA
      n_district <- if (all(c("country","province","district") %in% names(df)))
                       n_distinct(paste(df$country, df$province, df$district)) else NA
      n_rounds   <- if (all(c("country","response","roundnumber") %in% names(df)))
                       n_distinct(paste(df$country, df$response, df$roundnumber)) else NA

      hh_visited <- if ("number_of_hh_visited" %in% names(df))
                       sum(as.numeric(df$number_of_hh_visited), na.rm=TRUE) else NA

      tp <- if ("u5_present" %in% names(df)) sum(as.numeric(df$u5_present), na.rm=TRUE) else NA
      tf <- if ("u5_fm"      %in% names(df)) sum(as.numeric(df$u5_fm),      na.rm=TRUE) else NA
      regional_cv  <- if (!is.na(tp) && !is.na(tf) && tp > 0) tf / tp else NA
      total_missed <- if (!is.na(tp) && !is.na(tf)) pmax(tp - tf, 0) else NA

      mean_awareness <- if (!is.null(dfr) && "awareness_rate" %in% names(dfr))
                           mean(dfr$awareness_rate, na.rm=TRUE) else NA
      mean_risk <- if (!is.null(dfr) && "risk_score" %in% names(dfr))
                      mean(dfr$risk_score, na.rm=TRUE) else NA

      risk_counts <- list(critical=0, high=0, moderate=0, low=0)
      if (!is.null(dfr) && all(c("risk_class","country","province","district") %in% names(dfr))) {
        rc <- dfr %>%
          group_by(country, province, district) %>%
          summarise(rc = first(risk_class), .groups="drop") %>%
          count(rc)
        g <- function(x) { v <- rc$n[rc$rc == x]; if (length(v)) v else 0 }
        risk_counts <- list(critical=g("Critical"), high=g("High"), moderate=g("Moderate"), low=g("Low"))
      }

      out <- data.frame(
        generated_on           = as.character(Sys.Date()),
        countries               = n_country,
        districts                = n_district,
        campaign_rounds          = n_rounds,
        total_hh_visited         = hh_visited,
        regional_coverage_pct    = if (is.na(regional_cv))  NA else round(regional_cv*100,1),
        total_missed_children    = total_missed,
        mean_awareness_pct       = if (is.na(mean_awareness)) NA else round(mean_awareness*100,1),
        mean_risk_score          = if (is.na(mean_risk)) NA else round(mean_risk,1),
        critical_risk_events     = risk_counts$critical,
        high_risk_events         = risk_counts$high,
        moderate_risk_events     = risk_counts$moderate,
        low_risk_events          = risk_counts$low
      )
      write.csv(out, f, row.names=FALSE)
    })
  output$dl_country_risk <- downloadHandler(
    filename=function() paste0("IM_country_risk_",Sys.Date(),".csv"),
    content =function(f) {
      df  <- snap()
      dfr <- tryCatch(with_risk(df), error=function(e) NULL)
      if (is.null(dfr) || !all(c("risk_score","country") %in% names(dfr))) {
        write.csv(data.frame(), f, row.names=FALSE); return(invisible())
      }
      tbl <- dfr %>%
        group_by(country) %>%
        summarise(mean_risk_score = mean(risk_score, na.rm=TRUE),
                  total_missed    = sum(missed_child, na.rm=TRUE), .groups="drop") %>%
        arrange(desc(mean_risk_score))
      write.csv(tbl, f, row.names=FALSE)
    })
  output$dl_op_failure <- downloadHandler(
    filename=function() paste0("IM_operational_failure_",Sys.Date(),".csv"),
    content =function(f) {
      df     <- snap()
      needed <- c("r_non_fm_absent","r_non_fm_nc","r_non_fm_hh_notvisited","r_non_fm_hh_notrevisited")
      if (!all(needed %in% names(df))) {
        write.csv(data.frame(), f, row.names=FALSE); return(invisible())
      }
      grp_cols <- intersect(c("country","province","district","response","roundnumber"), names(df))
      if (!length(grp_cols)) grp_cols <- intersect(c("country","district"), names(df))
      mc_col <- if ("missed_child" %in% names(df)) "missed_child" else NA
      op <- df %>%
        group_by(across(all_of(grp_cols))) %>%
        summarise(
          missed_child = if (!is.na(mc_col)) sum(.data[[mc_col]], na.rm = TRUE)
                         else sum(pmax(as.numeric(u5_present) - as.numeric(u5_fm), 0), na.rm = TRUE),
          absent               = sum(r_non_fm_absent, na.rm = TRUE),
          non_compliance       = sum(r_non_fm_nc, na.rm = TRUE),
          house_not_visited    = sum(r_non_fm_hh_notvisited, na.rm = TRUE),
          house_not_revisited  = sum(r_non_fm_hh_notrevisited, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          operational_failure_type = case_when(
            house_not_visited >= house_not_revisited &
              house_not_visited >= absent &
              house_not_visited >= non_compliance &
              house_not_visited > 0 ~ "Team did not visit households",
            house_not_revisited >= house_not_visited &
              house_not_revisited >= absent &
              house_not_revisited >= non_compliance &
              house_not_revisited > 0 ~ "Weak revisit / supervision failure",
            absent >= house_not_visited &
              absent >= house_not_revisited &
              absent >= non_compliance &
              absent > 0 ~ "Population absence / timing issue",
            non_compliance >= house_not_visited &
              non_compliance >= house_not_revisited &
              non_compliance >= absent &
              non_compliance > 0 ~ "Community resistance / refusal",
            missed_child == 0 ~ "No operational failure detected",
            TRUE ~ "Mixed / other operational issue"
          )
        )
      tbl <- op %>%
        group_by(operational_failure_type) %>%
        summarise(total_missed = sum(missed_child, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_missed))
      write.csv(tbl, f, row.names=FALSE)
    })
  output$dl_priority_districts <- downloadHandler(
    filename=function() paste0("IM_priority_districts_",Sys.Date(),".csv"),
    content =function(f) {
      df  <- snap()
      dfr <- tryCatch(with_risk(df), error=function(e) NULL)
      if (is.null(dfr) || !all(c("risk_score","country","province","district") %in% names(dfr))) {
        write.csv(data.frame(), f, row.names=FALSE); return(invisible())
      }
      tbl <- dfr %>%
        group_by(country, province, district) %>%
        summarise(mean_risk_score = mean(risk_score, na.rm=TRUE),
                  total_missed    = sum(missed_child, na.rm=TRUE), .groups="drop") %>%
        arrange(desc(mean_risk_score)) %>%
        mutate(priority = case_when(
          mean_risk_score >= 70 ~ "Priority 1",
          mean_risk_score >= 50 ~ "Priority 2",
          mean_risk_score >= 30 ~ "Priority 3",
          TRUE                  ~ "Routine"
        ))
      write.csv(tbl, f, row.names=FALSE)
    })
  })
}

# ============================================================
# IM REPOSITORY OVERVIEW (sub-tab "Overview")  -- filters + metric cards + data explorer
# Mirrors the layout of the LQAS Repository Overview page.
# ============================================================

.im_cache <- new.env(parent = emptyenv())
im_data_cached <- function(path = IM_DATA_FILE) {
  mt <- if (file.exists(path)) file.mtime(path) else NA
  if (is.null(.im_cache$data) || !identical(.im_cache$mtime, mt)) {
    .im_cache$data  <- im_load_data(path)
    .im_cache$mtime <- mt
  }
  .im_cache$data
}

# columns shown by default in the explorer (everything else stays available via "Show/Hide Columns")
.im_repo_default_cols <- c("country", "province", "district", "afro_block", "response", "vaccine_type",
                           "roundnumber", "round_start_date", "number_of_hh_visited", "u5_present", "u5_fm",
                           "missed_child", "cv", "qc_flag", "reconciliation_flag")
.im_repo_labels <- c(country = "Country", province = "Province", district = "District", afro_block = "AFRO Block",
                     response = "Response", vaccine_type = "Vaccine", roundnumber = "Round",
                     round_start_date = "Round start", number_of_hh_visited = "HH visited",
                     u5_present = "U5 present", u5_fm = "U5 finger-marked", missed_child = "Missed children",
                     cv = "Coverage (CV)", qc_flag = "QC flag", reconciliation_flag = "Reconciliation")

im_repository_ui <- function(id) {
  ns <- shiny::NS(id)
  H  <- shiny::HTML
  shiny::div(class = "im-repo",
    shiny::tags$head(shiny::tags$style(H(IM_CSS)),
      shiny::tags$style(H(".im-repo-cards > div { flex: 1 1 0; min-width: 0; width: auto; float: none; padding: 0; } .im-repo-cards .small-box { margin-bottom: 0; } .dark-mode .im-repo label, .dark-mode .im-repo h4, .dark-mode .im-repo .control-label, .dark-mode .im-repo .dataTables_info, .dark-mode .im-repo .dataTables_filter label { color: #e5e7eb !important; } .dark-mode .im-repo .analysis-controls { background-color: #0b1120; border-left-color: #3b82f6; color: #e5e7eb; } .dark-mode .im-repo table.dataTable, .dark-mode .im-repo table.dataTable td, .dark-mode .im-repo table.dataTable th { color: #e5e7eb; background-color: transparent; } .dark-mode .im-repo table.dataTable tbody tr { background-color: transparent !important; } .dark-mode .im-repo .dataTables_scrollBody { border-bottom-color: #1f2937; }"))),
    shiny::uiOutput(ns("rp_empty")),
    shiny::fluidRow(
      # ---- FILTER PANEL ---------------------------------------------------------------
      shiny::column(12, class = "imx-fcol",
        shinydashboard::box(
          title = shiny::tags$div(shiny::icon("filter"), "IM Repository Filters"),
          width = NULL, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          shiny::selectizeInput(ns("rp_block"), "AFRO Block:", c("All", "LCB", "WA", "DRC", "CEA", "ESA"),
            selected = "All", options = list(dropdownParent = "body")),
          shiny::selectizeInput(ns("rp_country"), "Countries (optional):", choices = NULL, multiple = TRUE,
            options = list(placeholder = "All countries", dropdownParent = "body", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("rp_province"), "Provinces (optional):", choices = NULL, multiple = TRUE,
            options = list(placeholder = "All provinces", dropdownParent = "body", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("rp_district"), "Districts (optional):", choices = NULL, multiple = TRUE,
            options = list(placeholder = "Type to search districts...", maxItems = 25, maxOptions = 100,
                           dropdownParent = "body", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("rp_response"), "Response:", choices = NULL, multiple = TRUE,
            options = list(placeholder = "All responses", dropdownParent = "body", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("rp_vaccine"), "Vaccine type:", choices = NULL, multiple = TRUE,
            options = list(placeholder = "All vaccines", dropdownParent = "body", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("rp_round"), "Round number(s):", choices = NULL, multiple = TRUE,
            options = list(placeholder = "All rounds", dropdownParent = "body", plugins = list("remove_button"))),
          shiny::dateRangeInput(ns("rp_dates"), "Round start date:", format = "yyyy-mm-dd", startview = "year"),
          shiny::selectInput(ns("rp_qc"), "QC flag:", c("All", "OK only", "Needs review only")),
          shiny::actionButton(ns("rp_reset"), shiny::tagList(shiny::icon("undo"), "Reset filters"),
            class = "btn btn-default", style = "margin-top: 8px; width: auto; justify-self: start;")
        )
      ),
      # ---- RESULTS PANEL ---------------------------------------------------------------
      shiny::column(12,
        shinydashboard::box(
          title = shiny::tags$div(shiny::icon("database"), "IM Repository Overview"),
          width = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
          shiny::div(class = "valuebox-flex-container im-repo-cards",
            shinydashboard::valueBoxOutput(ns("rp_records")),
            shinydashboard::valueBoxOutput(ns("rp_rounds")),
            shinydashboard::valueBoxOutput(ns("rp_countries")),
            shinydashboard::valueBoxOutput(ns("rp_provinces")),
            shinydashboard::valueBoxOutput(ns("rp_districts")),
            shinydashboard::valueBoxOutput(ns("rp_period"))
          ),
          shiny::div(class = "analysis-controls",
            shiny::h4(shiny::icon("table"), "Data Explorer"),
            DT::DTOutput(ns("rp_table"))
          ),
          shiny::div(style = "text-align: center; margin-top: 20px;",
            shiny::downloadButton(ns("rp_download"), "Download Filtered Data",
              class = "btn-primary-custom", style = "width: 300px;"))
        )
      )
    )
  )
}

im_repository_server <- function(id, data_path = IM_DATA_FILE) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- im_data_cached(data_path)

    output$rp_empty <- shiny::renderUI({
      if (!is.null(data) && nrow(data) > 0) return(NULL)
      shiny::div(class = "alert alert-warning",
        paste0("No Independent Monitoring data found (expected ", data_path, ")."))
    })
    if (is.null(data) || !nrow(data)) return(invisible(NULL))

    nz  <- function(x) { x <- as.character(x); x[!is.na(x) & nzchar(x)] }
    chk <- function(col) if (col %in% names(data)) sort(unique(nz(data[[col]]))) else character(0)
    en_period <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))

    dts <- if ("round_start_date" %in% names(data)) as.Date(data$round_start_date) else as.Date(NA)
    dts <- dts[!is.na(dts)]
    dmin <- if (length(dts)) min(dts) else NA
    dmax <- if (length(dts)) max(dts) else NA

    set_static_choices <- function() {
      shiny::updateSelectizeInput(session, "rp_response", choices = chk("response"), selected = character(0))
      shiny::updateSelectizeInput(session, "rp_vaccine",  choices = chk("vaccine_type"), selected = character(0))
      shiny::updateSelectizeInput(session, "rp_round",    choices = chk("roundnumber"), selected = character(0))
      if (!is.na(dmin)) shiny::updateDateRangeInput(session, "rp_dates", start = dmin, end = dmax, min = dmin, max = dmax)
    }

    # ---- cascading choices: block -> countries -> provinces -> districts ---------------
    in_block <- shiny::reactive({
      df <- data
      if (!is.null(input$rp_block) && input$rp_block != "All" && "afro_block" %in% names(df))
        df <- df[!is.na(df$afro_block) & df$afro_block == input$rp_block, ]
      df
    })
    shiny::observe({
      ctr <- sort(unique(nz(in_block()$country)))
      shiny::updateSelectizeInput(session, "rp_country", choices = ctr,
                                  selected = intersect(shiny::isolate(input$rp_country), ctr))
    })
    in_country <- shiny::reactive({
      df <- in_block()
      if (length(input$rp_country)) df <- df[!is.na(df$country) & df$country %in% input$rp_country, ]
      df
    })
    shiny::observe({
      prv <- sort(unique(nz(in_country()$province)))
      shiny::updateSelectizeInput(session, "rp_province", choices = prv,
                                  selected = intersect(shiny::isolate(input$rp_province), prv))
    })
    in_province <- shiny::reactive({
      df <- in_country()
      if (length(input$rp_province)) df <- df[!is.na(df$province) & df$province %in% input$rp_province, ]
      df
    })
    shiny::observe({
      dst <- sort(unique(nz(in_province()$district)))
      shiny::updateSelectizeInput(session, "rp_district", choices = dst,
                                  selected = intersect(shiny::isolate(input$rp_district), dst), server = TRUE)
    })
    shiny::observeEvent(TRUE, set_static_choices(), once = TRUE)

    shiny::observeEvent(input$rp_reset, {
      shiny::updateSelectizeInput(session, "rp_block", selected = "All")
      for (i in c("rp_country", "rp_province", "rp_district", "rp_response", "rp_vaccine", "rp_round"))
        shiny::updateSelectizeInput(session, i, selected = character(0))
      shiny::updateSelectInput(session, "rp_qc", selected = "All")
      if (!is.na(dmin)) shiny::updateDateRangeInput(session, "rp_dates", start = dmin, end = dmax)
    })

    # ---- filtered repository ----------------------------------------------------------
    filtered <- shiny::reactive({
      df <- in_province()
      if (length(input$rp_district)) df <- df[!is.na(df$district) & df$district %in% input$rp_district, ]
      if (length(input$rp_response) && "response" %in% names(df))
        df <- df[!is.na(df$response) & df$response %in% input$rp_response, ]
      if (length(input$rp_vaccine) && "vaccine_type" %in% names(df))
        df <- df[!is.na(df$vaccine_type) & df$vaccine_type %in% input$rp_vaccine, ]
      if (length(input$rp_round) && "roundnumber" %in% names(df))
        df <- df[!is.na(df$roundnumber) & as.character(df$roundnumber) %in% input$rp_round, ]
      dr <- input$rp_dates
      if (!is.null(dr) && length(dr) == 2 && !anyNA(dr) && !is.na(dmin) &&
          (dr[1] > dmin || dr[2] < dmax) && "round_start_date" %in% names(df)) {
        d <- as.Date(df$round_start_date)
        df <- df[!is.na(d) & d >= dr[1] & d <= dr[2], ]
      }
      if (!is.null(input$rp_qc) && input$rp_qc != "All" && "qc_flag" %in% names(df)) {
        ok <- !is.na(df$qc_flag) & df$qc_flag == "OK"
        df <- if (input$rp_qc == "OK only") df[ok, ] else df[!ok, ]
      }
      df
    })

    # ---- metric cards ---------------------------------------------------------------------
    nd <- function(x) dplyr::n_distinct(x[!is.na(x)])
    output$rp_records <- shinydashboard::renderValueBox(
      shinydashboard::valueBox(format(nrow(filtered()), big.mark = ","), "Records",
                               icon = shiny::icon("database"), color = "blue"))
    output$rp_rounds <- shinydashboard::renderValueBox({
      df <- filtered()
      n <- if (all(c("country", "response", "roundnumber") %in% names(df)))
        dplyr::n_distinct(paste(df$country, df$response, df$roundnumber)) else 0
      shinydashboard::valueBox(format(n, big.mark = ","), "Campaign Rounds", icon = shiny::icon("calendar"), color = "aqua")
    })
    output$rp_countries <- shinydashboard::renderValueBox(
      shinydashboard::valueBox(nd(filtered()$country), "Countries", icon = shiny::icon("globe-africa"), color = "green"))
    output$rp_provinces <- shinydashboard::renderValueBox(
      shinydashboard::valueBox(format(nd(filtered()$province), big.mark = ","), "Provinces", icon = shiny::icon("map"), color = "teal"))
    output$rp_districts <- shinydashboard::renderValueBox({
      df <- filtered()
      n <- dplyr::n_distinct(paste(df$country, df$province, df$district)[!is.na(df$district)])
      shinydashboard::valueBox(format(n, big.mark = ","), "Districts", icon = shiny::icon("map-marker-alt"), color = "light-blue")
    })
    output$rp_period <- shinydashboard::renderValueBox({
      d <- if ("round_start_date" %in% names(filtered())) as.Date(filtered()$round_start_date) else as.Date(NA)
      d <- d[!is.na(d)]
      txt <- if (length(d)) paste0(en_period(min(d)), " - ", en_period(max(d))) else "No dates"
      shinydashboard::valueBox(shiny::HTML(sprintf("<span style='font-size:13px; display:block; white-space:normal; line-height:1.15;'>%s</span>", txt)),
        shiny::HTML("<span style='font-size:12px;'>Data Period</span>"),
        icon = shiny::icon("clock"), color = "purple")
    })

    # ---- data explorer ---------------------------------------------------------------------
    output$rp_table <- DT::renderDT({
      df <- filtered()
      dc <- if ("round_start_date" %in% names(df)) "round_start_date" else NULL
      if (!is.null(dc)) df <- df[order(df[[dc]], decreasing = TRUE, na.last = TRUE), ]
      keep <- intersect(.im_repo_default_cols, names(df))
      shown <- c(keep, setdiff(names(df), keep))                    # default columns first
      df <- df[, shown, drop = FALSE]
      lab <- ifelse(shown %in% names(.im_repo_labels), unname(.im_repo_labels[shown]), shown)
      hidden <- which(!(shown %in% keep)) - 1L                       # 0-based for DataTables
      w <- DT::datatable(
        df, colnames = lab, rownames = FALSE, filter = "top",
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = "Bfrtip",
          buttons = list(list(extend = "copy", text = "Copy"), list(extend = "csv", text = "CSV"),
                         list(extend = "excel", text = "Excel"), list(extend = "colvis", text = "Show/Hide Columns")),
          columnDefs = list(list(visible = FALSE, targets = hidden)),
          scrollX = TRUE, scrollY = 400, scroller = TRUE, deferRender = TRUE, pageLength = 10, autoWidth = FALSE
        ))
      if ("cv" %in% shown) w <- DT::formatPercentage(w, "cv", 1)
      w
    }, server = TRUE)

    output$rp_download <- shiny::downloadHandler(
      filename = function() paste0("IM_Repository_Overview_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(filtered(), file, row.names = FALSE, na = ""))
  })
}

# ---- keep plotly out of the search path (the app uses `conflicted`) -------------------
.im_env <- new.env(parent = globalenv())
for (.nm in getNamespaceExports("plotly"))
  assign(.nm, getExportedValue("plotly", .nm), envir = .im_env)
rm(.nm)
assign("bs_icon", bs_icon, envir = .im_env); rm(bs_icon)   # keep the icon shim out of the global env
environment(im_overview_ui)     <- .im_env
environment(im_overview_server) <- .im_env
environment(im_repository_ui)   <- .im_env
environment(im_repository_server) <- .im_env

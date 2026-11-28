#!/usr/bin/env python3
"""Assistant core: a language model that answers questions on the SIA data ONLY through the catalogue engine (native tool calling).   Author: Gorgui Ba Toure

The model never sees a record and never writes code: it chooses a tool (list_datasets, describe_data, query_data), the engine (tools/catalog_engine.py) runs a fixed metric of
catalog/engine_spec.json, and the model words the answer with the unit, period and warnings of the result. The rules are catalog/ASK_RULES.md.
Needs: pip install anthropic   and the environment variable ANTHROPIC_API_KEY (never in a file of a repository); model name in SIA_CHAT_MODEL.
Used by chat_app/app.py (Shiny for Python page) and tools/run_assistant_eval.py (the 78 questions, graded by tools/grade_assistant.py).
"""
import copy, json, os, pathlib, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from catalog_engine import Engine, EngineError, HERE  # noqa: E402  (HERE: the folder that holds catalog/, in the agent repository or in AFRO-Dash/assistant_py)

DEFAULT_MODEL = os.environ.get("SIA_CHAT_MODEL", "claude-sonnet-4-5")
MAX_TABLE_ROWS = 60

FILTER_PROPS = {
    "country": {"type": "string", "description": "country name as in the data, any case (for example Nigeria)"},
    "year": {"type": "integer"},
    "vaccine": {"type": "string", "description": "vaccine type, for example nOPV2, bOPV, 'nOPV2 & bOPV'"},
    "status": {"type": "string", "description": "LQAS lot status: Pass or Fail"},
    "performance": {"type": "string", "description": "LQAS performance class, for example 'very poor'"},
    "block": {"type": "string", "description": "AFRO block (WA, LCB, ESA, DRC, CA)"},
    "level": {"type": "string", "description": "report level (Form A, preparedness, social mobilization): National, Regional, District, Facility, STATE, LGA, WARD"},
}
TOOLS = [
    {"name": "list_datasets", "description": "List the datasets (and the raw forms) with their dimensions and metrics. Call it when you do not know which dataset or metric answers the question.",
     "input_schema": {"type": "object", "properties": {}}},
    {"name": "describe_data", "description": "Meaning of one dataset: what a row is, its dimensions, every metric with unit, definition and formula, and the caveats. Call it before choosing a metric you are not sure about.",
     "input_schema": {"type": "object", "properties": {"dataset": {"type": "string"}}, "required": ["dataset"]}},
    {"name": "query_data", "description": "Run ONE catalogue metric on a dataset, with filters and an optional grouping. Returns the value or a table with unit, definition, formula, period, rows used and warnings. Never invent a metric: use only the metrics of describe_data.",
     "input_schema": {"type": "object", "properties": {
         "dataset": {"type": "string"}, "metric": {"type": "string"},
         "by": {"type": "string", "description": "dimension to group by (country, year, month, vaccine, block, level, ...)"},
         **FILTER_PROPS,
         "filters": {"type": "object", "description": "other dimension filters as {dimension: value or list}, for example {\"team_type\": \"H2H\", \"month\": \"2026-06\"}"},
         "months": {"type": "array", "items": {"type": "string"}, "description": "month numbers (1-12) or YYYY-MM"},
         "params": {"type": "object", "description": "metric parameters, for example {\"indicator\": \"ind_house_marked\"} or {\"column\": \"states\"}"},
         "latest_round": {"type": "boolean", "description": "LQAS, admin, IM: keep only the most recent round of the selection (the latest campaign round surveyed, found by date). Use it for 'the last / latest / most recent round'; NEVER pick a round by its label (RND1, RND3...), labels repeat across campaigns and years"},
         "latest_only": {"type": "boolean", "description": "Form A and preparedness: true (default) keeps the latest report of each place and round; false keeps all reports"},
         "complete_lots_only": {"type": "boolean", "description": "LQAS reasons: true (default) over complete lots of 60 children"},
         "min_n": {"type": "integer", "description": "with by: drop groups with fewer rows"}, "top_n": {"type": "integer"}},
         "required": ["dataset", "metric"]}},
]
def build_tools(engine):
    """The tool list with the dataset and metric names of the engine as enums: a model cannot invent a metric."""
    tools = copy.deepcopy(TOOLS)
    ds = [d["dataset"] for d in engine.datasets()]
    metrics = sorted({m for d in engine.datasets() for m in d["metrics"]})
    for tl in tools:
        pr = tl["input_schema"]["properties"]
        if "dataset" in pr:
            pr["dataset"]["enum"] = ds
        if "metric" in pr:
            pr["metric"]["enum"] = metrics
    return tools


CHART_TOOL = {"name": "make_chart", "description": "Draw a chart (an image shown to the user after your reply) of ONE catalogue metric grouped by a dimension: bars for categories (country, vaccine, block...), a line for year or month. Use it when the user asks for a graph, chart or plot. Same dataset, metric, filters and options as query_data; `by` is required. Then describe what the chart shows and restate the warnings of the result.",
              "input_schema": {"type": "object", "properties": {
                  "dataset": {"type": "string"}, "metric": {"type": "string"},
                  "by": {"type": "string", "description": "dimension on the axis (country, year, month, vaccine, block, level, ...)"},
                  "chart": {"type": "string", "enum": ["bar", "line"], "description": "bar (default for categories) or line (default for year and month)"},
                  "title": {"type": "string", "description": "short title"},
                  **FILTER_PROPS,
                  "filters": {"type": "object"}, "months": {"type": "array", "items": {"type": "string"}}, "params": {"type": "object"},
                  "latest_round": {"type": "boolean"}, "latest_only": {"type": "boolean"}, "complete_lots_only": {"type": "boolean"},
                  "min_n": {"type": "integer"}, "top_n": {"type": "integer", "description": "bars to draw (default 15)"}},
                  "required": ["dataset", "metric", "by"]}}
TOOLS.append(CHART_TOOL)
MAP_TOOL = {"name": "make_map", "description": "Draw a MAP (an image shown to the user after your reply) of ONE catalogue metric, one value per place on the AFRO boundaries. Use it whenever the user asks for a map. Level: district by geo code for LQAS, admin and IM; for the other datasets province, state or district by name inside ONE country (give `country`), or the country level. Same dataset, metric, filters and options as query_data, but no `by`. For 'the last round' set latest_round=true. Then say what the map shows, how many places were placed, and restate the warnings of the result.",
            "input_schema": {"type": "object", "properties": {
                "dataset": {"type": "string"}, "metric": {"type": "string"},
                "level": {"type": "string", "enum": ["geo_code", "district", "province", "country"], "description": "optional: district (by geo code or name), province or state, country. Default: the finest level the dataset supports."},
                "title": {"type": "string", "description": "short title"},
                **FILTER_PROPS,
                "filters": {"type": "object"}, "months": {"type": "array", "items": {"type": "string"}}, "params": {"type": "object"},
                "latest_round": {"type": "boolean"}, "latest_only": {"type": "boolean"}, "complete_lots_only": {"type": "boolean"}, "min_n": {"type": "integer"}},
                "required": ["dataset", "metric"]}}
TOOLS.append(MAP_TOOL)
SETTLEMENT_TOOL = {"name": "find_settlement", "description": "Information about ONE settlement or health facility from the route-level IM files (the IM Settlement Maps data): where and when monitors found missed children there, how many, the reasons, the campaign rounds, the district context and the GPS position, with an optional map. Give the place name as written by the user (spelling differences are handled) and the country (and the district when known): the same name exists in many districts. When several places match it returns the candidates: ask the user which one, then call again with the same name and the number in `pick` (1 is the first candidate). Counts only: the files hold no child, caregiver or monitor identity, and only routes that found missed children, so there is no coverage rate per settlement. For totals by country, district or month over many places use query_data on the dataset settlement instead.",
                   "input_schema": {"type": "object", "properties": {
                       "name": {"type": "string", "description": "settlement or health facility name"},
                       "country": {"type": "string"}, "province": {"type": "string"}, "district": {"type": "string"},
                       "pick": {"type": "integer", "description": "number (1-based) of the candidate to describe, after a first call returned several"},
                       "map": {"type": "boolean", "description": "true to draw the routes of the place on the district boundary"}},
                       "required": ["name"]}}
TOOLS.append(SETTLEMENT_TOOL)
FOOTPRINT_TOOL = {"name": "make_footprint_map", "description": "Draw a FOOTPRINT MAP (an image the user can see and download) of the monitor routes of the IM settlement files that found missed children: every route as a line from its start to its end, coloured by the children missed, with a scale bar, on the district boundary. Use it for 'settlement map', 'footprint', 'routes', 'zoom to a settlement / 300 m', 'last round conducted in <country>'. Give the country. With `place` (settlement or health facility name) it zooms on that place; `radius_m` is the half-width of the window around the place (300 gives a window of 600 m); without `place` it draws all the routes of the selection (country, district, year, months, latest_round). `latest_round` keeps the most recent campaign round of the selection. When the place name matches several places it returns the candidates: ask which one, then call again with `pick`. For a district-level shaded map use make_map instead.",
                  "input_schema": {"type": "object", "properties": {
                      "country": {"type": "string"}, "province": {"type": "string"}, "district": {"type": "string"},
                      "place": {"type": "string", "description": "settlement or health facility to zoom on"},
                      "pick": {"type": "integer", "description": "1-based number of the candidate, after a first call returned several"},
                      "radius_m": {"type": "number", "description": "half-width of the window around the place, in metres (for example 300)"},
                      "latest_round": {"type": "boolean", "description": "keep only the most recent campaign round of the country selection"},
                      "year": {"type": "integer"}, "months": {"type": "array", "items": {"type": "string"}},
                      "response": {"type": "string"}, "round": {"type": "string"},
                      "background": {"type": "string", "enum": ["auto", "satellite", "osm", "none"], "description": "under the routes: satellite photo (Esri, as the dashboard settlement map) is the DEFAULT: leave this empty unless the user asks for another background; osm = OpenStreetMap map tiles / street map; none = plain district boundary"},
                      "title": {"type": "string"}},
                      "required": ["country"]}}
TOOLS.append(FOOTPRINT_TOOL)
SP_FLAGS = ("--sharepoint-push", "--upload-only", "--force-upload")
JOB_TOOLS = [
    {"name": "run_job", "description": "Run a pipeline job of the owner's computer through the job runner and return its result: the weekly technical slides (weekly_deck: a PowerPoint the user can download), refresh of the scope, LQAS, IM, Form A, preparedness, team supervision, vaccine accountability, social mobilization, AFP data, admin checks, backups, maps. The jobs download and rebuild LOCAL files and never publish to SharePoint by default. Pass `flags` only from the job's allowed flags (for example --months=12, --countries=NIGERIA,CHAD for weekly_deck). A SharePoint upload (--sharepoint-push, --upload-only, --force-upload) needs `user_confirmed_sharepoint=true`, which you may set ONLY when the user's own latest message explicitly asked for that upload. Long jobs take minutes: the call waits up to `wait_seconds` (default 300); if the job is still running it returns its id and you tell the user, who can ask for the status later.",
     "input_schema": {"type": "object", "properties": {
         "job": {"type": "string"}, "flags": {"type": "array", "items": {"type": "string"}},
         "wait_seconds": {"type": "integer"}, "user_confirmed_sharepoint": {"type": "boolean"}}, "required": ["job"]}},
    {"name": "job_status", "description": "Status and result of a job started with run_job (its id), or the last job when no id is given.",
     "input_schema": {"type": "object", "properties": {"id": {"type": "string"}}}}]
FIGURE = re.compile(r"\d{1,3}(,\d{3})+|\d+\.\d+|\b\d{3,}\b|\d\s?%")


def load_key():
    """ANTHROPIC_API_KEY from the environment (how a deployment sets it), else from chat_app/.env of THIS project (git-ignored, for local runs). Only that variable is read; it is never printed or written."""
    if os.environ.get("ANTHROPIC_API_KEY"):
        return True
    for path in (os.path.join(HERE, "chat_app", ".env"), os.path.join(HERE, ".env")):
        try:
            for line in open(path, encoding="utf-8", errors="replace"):
                line = line.strip()
                if line.startswith("export "):
                    line = line[7:].strip()
                if line.startswith("ANTHROPIC_API_KEY") and "=" in line:
                    v = line.split("=", 1)[1].strip().strip('"').strip("'")
                    if v:
                        os.environ["ANTHROPIC_API_KEY"] = v
                        return True
        except OSError:
            continue
    return False


def system_prompt(here=HERE, engine=None):
    rules = open(os.path.join(here, "catalog", "ASK_RULES.md"), encoding="utf-8").read()
    listing = "\n".join("- %s (%s; group by: %s): %s" % (d["dataset"], d["entity"], ", ".join(d["dimensions"]), ", ".join(d["metrics"])) for d in engine.datasets()) if engine else ""
    return ("You answer questions of the WHO AFRO SIA data team about polio SIA data (LQAS, administrative coverage, Form A, preparedness, vaccine accountability, social mobilization, "
            "team supervision, independent monitoring, and the raw forms). Every figure you state MUST come from a tool result of this conversation: call query_data (use list_datasets / describe_data "
            "when unsure). State the figure with its unit, period (from the result) and dataset, and restate EVERY warning of the result in your answer, one by one, none left out. If no metric answers the question, say that you cannot answer it "
            "from the catalogue; never estimate, never compute from memory. If the question rests on a wrong premise (for example 'districts' for LQAS lots, or adding the two vaccines of a co-administered campaign), say so plainly and say what the figure really is. For a co-administered campaign the figure is ONE combined row, the larger of the two vaccine totals: never add them, and if the question asks to add them, say that they should not be added. "
            "'children vaccinated' is metric children_vaccinated; doses, vaccinations or the sum of all rounds is doses_administered; say which one you give. Use the metric the question names: median_wastage_rate unless the question asks to leave out flagged reports (then median_wastage_rate_unflagged and say so). Counts of reports or visits cover every report unless the question asks for the latest report of each place (latest_only=true). "
            ""
            "For the last, latest or most recent round or campaign use latest_round=true and name the campaign and round the result reports; never choose a round by its label and never group by round to find the latest. "
            "For a question about ONE settlement, village or health facility (what happened there, how many missed children, why, when, where) call find_settlement with the name and the country; if it returns several candidates, list them and ask which one, then call again with pick; add map=true when a map is asked. State that the settlement files hold only routes that found missed children (no coverage rate per settlement) and the routes left out. For totals over many places use query_data on the dataset settlement. "
            "Every table, chart, map, slide deck or file you produce is offered to the user as a download link under your answer: say so briefly, never invent a link or a file path. For a settlement map, a footprint map, routes on a map, a zoom on a settlement (for example 300 m) or the routes of the last round of a country call make_footprint_map (the user can download the image); a district-shaded map of any metric is make_map. For LQAS performance by district use metric district_performance_class (a class per district) with make_map and latest_round=true. When the user asks to run, refresh or update data, or to return the weekly technical (SIA weekly) slides, call run_job: weekly_deck builds the weekly technical slides (PowerPoint, 9 slides) the user downloads; dash_analysis produces ANY analysis of the AFRO dashboard with any filters (flags --analyses=<keys, comma separated> with keys scope, m_admin_vacc, m_admin_cov, m_lqas_exec, m_reasons, m_im_cards, m_im_cv95, m_im_opfail, m_im_sm, admin, missed, coverage, district_perf, lqas_maps, reasons, unresolved, im_overview, im_reasons, im_priority, or all; --months=N or --years=2025,2026; --block=; --countries=; --provinces=; --districts=): it returns a PowerPoint and one PNG per chart or map; 'update the scope data' = run_job scope_refresh; LQAS, IM, Form A, preparedness, team supervision, vaccine accountability, social mobilization, AFP, admin checks and backups are jobs too; refresh_all_data runs them all; sia_review rebuilds the SIA executive review (Chart_by_round table and chart, campaign scores, deck, narrative) from the admin, LQAS, IM and scope data. Refresh jobs rebuild local files and never upload to SharePoint unless the user's own message explicitly asks for the upload; report the job status and what it produced. "
            "For a graph or chart call make_chart (bars for categories, a line for year or month): the image is shown after your reply, so describe what it shows and restate the warnings. For a map call make_map (district by geo code for LQAS, admin and IM; other datasets by province, state or district name inside one country, or by country): the image is shown after your reply, so say what it shows, how many places were placed, and restate the warnings; use latest_round=true for the last round. If the tool says R is missing, say that this computer cannot draw maps and give the table instead. "
            "When the question does not name the dataset, name the dataset you used. A dataset that is not in the list below is not available here: say so, do not guess. Answer in the language of the question, briefly.\n\nDATASETS AND THEIR METRICS (use these exact names):\n" + listing + "\n\nRULES OF THE CATALOGUE:\n" + rules)


def _fmt(v, unit):
    if v is None:
        return ""
    u = (unit or "").lower()
    if "percent" in u:
        return "%.1f%%" % v
    if "ratio" in u:
        return "%.2f" % v
    return "{:,.0f}".format(v) if abs(v) >= 100 or float(v).is_integer() else "%.2f" % v


def render_chart(res, kind=None, title=None, top_n=15):
    """PNG bytes of a bar or line chart of a grouped result. One hue, direct labels, a light grid, the unit and period in the subtitle."""
    import io
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    rows = [r for r in res.get("table", []) if r.get("value") is not None]
    if not rows:
        raise ValueError("nothing to draw: the result has no grouped values")
    by = res.get("by", "")
    kind = kind or ("line" if by in ("year", "month") else "bar")
    unit = res.get("unit", "")
    ink, muted, grid, blue = "#1d2530", "#5b6675", "#e3e7ec", "#1f6fb2"
    sub = " · ".join(x for x in [res.get("dataset", ""), unit, ("%s to %s" % (res["period"]["from"], res["period"]["to"])) if res.get("period") else "", "%s rows" % "{:,}".format(res.get("rows_used", 0))] if x)
    ttl = title or "%s by %s" % (res.get("metric", "").replace("_", " "), by)
    if kind == "line":
        rows = sorted(rows, key=lambda r: str(r["group"]))
        xs = [str(r["group"]) for r in rows]
        ys = [r["value"] for r in rows]
        fig, ax = plt.subplots(figsize=(9, 4.6), dpi=150)
        ax.plot(xs, ys, color=blue, linewidth=2, marker="o", markersize=7, markeredgecolor="white", markeredgewidth=1.5)
        if len(xs) <= 14:
            for x, y in zip(xs, ys):
                ax.annotate(_fmt(y, unit), (x, y), textcoords="offset points", xytext=(0, 9), ha="center", fontsize=8.5, color=ink)
        else:
            plt.setp(ax.get_xticklabels(), rotation=45, ha="right")
        ax.yaxis.grid(True, color=grid, linewidth=0.8)
        ax.set_axisbelow(True)
        ax.set_ylim(bottom=0 if min(ys) >= 0 else None)
        ax.margins(y=0.15)
    else:
        rows = sorted(rows, key=lambda r: -r["value"])[: int(top_n or 15)]
        labels = [str(r["group"]) for r in rows][::-1]
        vals = [r["value"] for r in rows][::-1]
        fig, ax = plt.subplots(figsize=(9, max(3.2, 0.36 * len(rows) + 1.7)), dpi=150)
        ax.barh(labels, vals, color=blue, height=0.62)
        mx = max(vals) if max(vals) > 0 else 1
        for i, v in enumerate(vals):
            ax.text(v + mx * 0.012, i, _fmt(v, unit), va="center", fontsize=8.8, color=ink)
        ax.set_xlim(0, mx * 1.16)
        ax.xaxis.grid(True, color=grid, linewidth=0.8)
        ax.set_axisbelow(True)
        ax.tick_params(axis="y", length=0)
        ax.xaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, _: _fmt(x, unit) if "percent" in unit.lower() or "ratio" in unit.lower() else ("%gM" % (x / 1e6) if abs(x) >= 1e6 else "{:,.0f}".format(x))))
    for s in ("top", "right", "left" if kind != "line" else "top"):
        ax.spines[s].set_visible(False)
    ax.spines["bottom"].set_color(grid)
    ax.tick_params(colors=muted, labelsize=9)
    fig.text(0.012, 0.975, ttl, ha="left", va="top", fontsize=13, fontweight="bold", color=ink)
    fig.text(0.012, 0.925, sub, ha="left", va="top", fontsize=9, color=muted)
    fig.subplots_adjust(top=0.84, left=0.2 if kind != "line" else 0.09, right=0.97, bottom=0.12)
    buf = io.BytesIO()
    fig.savefig(buf, format="png", facecolor="white")
    plt.close(fig)
    return buf.getvalue()


def _supported(msgs):
    """True when this conversation already holds a successful data tool result (so figures in a later reply can come from it)."""
    for m in msgs:
        c = m.get("content")
        if m.get("role") == "user" and isinstance(c, list):
            for b in c:
                if isinstance(b, dict) and b.get("type") == "tool_result" and not str(b.get("content", "")).startswith('{"error"'):
                    return True
    return False


NUDGE = "You stated a figure without a data tool result in this conversation. Use query_data (or say you cannot answer from the catalogue)."


class Assistant:
    def __init__(self, engine=None, client=None, model=None, max_steps=6):
        self.engine = engine or Engine()
        self.model = model or DEFAULT_MODEL
        self.max_steps = max_steps
        self.images = []
        self.files = []   # (path, name): files made during a question (a deck, a map) that the page offers as downloads
        self.system = system_prompt(self.engine.here, self.engine)
        self.tools = build_tools(self.engine)
        self.jobs = self._jobs_module()
        if self.jobs:
            jobs = sorted(self.jobs.JOBS) + sorted(self.jobs.CHAINS)
            for t in copy.deepcopy(JOB_TOOLS):
                if t["name"] == "run_job":
                    t["input_schema"]["properties"]["job"]["enum"] = jobs
                    t["description"] += " Jobs: " + "; ".join("%s = %s [flags: %s]" % (k, v["what"], " ".join(v["flags"]) or "none") for k, v in self.jobs.JOBS.items()) + "; chains (no flags): " + "; ".join("%s = %s" % (k, ", ".join(v)) for k, v in self.jobs.CHAINS.items())
                self.tools.append(t)
        if client is None:
            if not load_key():
                raise SystemExit("ANTHROPIC_API_KEY not found: set the environment variable, or put ANTHROPIC_API_KEY=... in the .env file of this project (chat_app/.env, or .env next to app.py)")
            try:
                import anthropic
            except ImportError:
                raise SystemExit("pip install anthropic (and set ANTHROPIC_API_KEY)")
            client = anthropic.Anthropic()
        self.client = client

    # ----- tools -----
    def dispatch(self, name, args):
        try:
            if name == "list_datasets":
                out = self.engine.datasets()
            elif name == "describe_data":
                out = self.engine.describe(args.get("dataset", ""))
            elif name == "query_data":
                call = dict(args)
                out = self.engine.run_call(call)
                self._offer_table(out, args)
                if isinstance(out.get("table"), list) and len(out["table"]) > MAX_TABLE_ROWS:
                    out["table_rows_total"] = len(out["table"])
                    out["table"] = out["table"][:MAX_TABLE_ROWS]
                    out.setdefault("warnings", []).append("Only the first %d rows of the table are shown." % MAX_TABLE_ROWS)
            elif name == "make_chart":
                call = dict(args)
                res = self.engine.run_call(call)
                if not res.get("table"):
                    out = {"error": "the result is not grouped: give a dimension in `by`"}
                else:
                    self.images.append(render_chart(res, call.get("chart"), call.get("title"), call.get("top_n") or 15))
                    out = {"chart_created": True, "chart_type": call.get("chart") or ("line" if res.get("by") in ("year", "month") else "bar"), "metric": res["metric"], "unit": res["unit"], "by": res.get("by"),
                           "period": res.get("period"), "rows_used": res["rows_used"], "table": res["table"][:30], "warnings": res.get("warnings", []), "notes": res.get("notes", [])}
            elif name == "make_map":
                from map_tools import render_map, MapError
                call = dict(args)
                title, level = call.pop("title", None), call.pop("level", None)
                try:
                    png, info = render_map(self.engine, call, title, level)
                except MapError as e:
                    return json.dumps({"error": str(e)}, ensure_ascii=False)
                res = info["result"]
                self.images.append(png)
                out = {"map_created": True, "level": info["level"], "metric": res["metric"], "unit": res["unit"], "country": info["country"], "period": res.get("period"), "rows_used": res["rows_used"],
                       "places_on_map": info["placed"].get("matched"), "places_in_result": info["placed"].get("total"), "places_not_placed": info["placed"].get("unmatched"),
                       "table": res["table"][:30], "warnings": res.get("warnings", []), "notes": res.get("notes", [])}
            elif name == "find_settlement":
                out = self._find_settlement(args)
            elif name == "make_footprint_map":
                out = self._footprint(args)
            elif name == "run_job":
                out = self._run_job(args)
            elif name == "job_status":
                out = self._job_status(args)
            else:
                out = {"error": "unknown tool " + str(name)}
        except EngineError as e:
            out = {"error": str(e)}
        except Exception as e:  # a bad argument must not stop the conversation
            out = {"error": "the query failed: %s" % str(e)[:200]}
        return json.dumps(out, ensure_ascii=False, default=str)

    def _offer_table(self, res, args):
        """Every table the engine returns is also offered as a CSV download (the full table, not only the rows shown)."""
        try:
            import csv, tempfile
            tab = res.get("table") if isinstance(res, dict) else None
            if not isinstance(tab, list) or not tab or not isinstance(tab[0], dict):
                return
            cols = list(tab[0].keys())
            for r in tab:
                for k in r:
                    if k not in cols:
                        cols.append(k)
            slug = re.sub(r"[^a-z0-9]+", "_", ("%s_%s_%s" % (args.get("dataset", ""), args.get("metric", ""), args.get("by") or "")).lower()).strip("_")[:50] or "table"
            path = os.path.join(tempfile.mkdtemp(prefix="sia_tab_"), "table_%s.csv" % slug)
            with open(path, "w", newline="", encoding="utf-8-sig") as f:
                w = csv.DictWriter(f, fieldnames=cols)
                w.writeheader()
                w.writerows(tab)
            self.files.append((path, os.path.basename(path)))
            res["downloadable"] = "the full table is offered to the user as a CSV download"
        except Exception:
            pass

    # ----- footprint map of settlement routes -----
    def _footprint(self, a):
        import settlement_tools as st, tempfile
        from map_tools import rscript, boundaries_dir, MapError
        if "settlement" not in self.engine.ds or not self.engine.available("settlement"):
            return {"error": "the settlement files (AFRO-Dash/input/<form id>.csv.gz) are not available in this installation"}
        if not a.get("country"):
            return {"error": "give the country"}
        R = rscript()
        if not R:
            return {"error": "R is not installed on this computer: maps need Rscript"}
        df = self.engine.load("settlement")[0]
        d, info = st.select_routes(df, a["country"], a.get("province"), a.get("district"), a.get("year"), a.get("months"), bool(a.get("latest_round")), a.get("response"), a.get("round"))
        if d.empty:
            return {"error": "no route with missed children for this selection (country, district, period)"}
        notes, mask, label = [], None, None
        if a.get("place"):
            cands = st.search_places(df, a["place"], a["country"], a.get("district"), a.get("province"), limit=10)
            if not cands:
                return {"found": False, "message": "no place of this name in the settlement files of %s: check the spelling" % a["country"]}
            exact = [c for c in cands if c["match"] == "exact name"]
            pick = a.get("pick")
            if pick and 1 <= int(pick) <= len(cands):
                ch = cands[int(pick) - 1]
            elif len(cands) == 1:
                ch = cands[0]
            elif len(exact) == 1:
                ch = exact[0]
            else:
                return {"resolved": False, "message": "several places match: ask the user which one, then call again with `pick`", "candidates": [dict(number=i + 1, **c) for i, c in enumerate(cands)]}
            pk = st.norm(ch["place"])
            mask = (d.place_key == pk) & (d.district == ch["district"])
            if not mask.any():
                d2, _ = st.select_routes(df, a["country"], a.get("province"), a.get("district"), a.get("year"), a.get("months"), False, a.get("response"), a.get("round"))
                m2 = (d2.place_key == pk) & (d2.district == ch["district"])
                if not m2.any():
                    return {"error": "this place has no route with missed children for this selection"}
                d, mask = d2, m2
                notes.append("the place has no route in the latest round of the country: all its rounds are drawn")
                info.pop("latest_round", None)
            label = ch["place"].title()
        window = st.footprint_window(d, mask, a.get("radius_m"))
        d = d[(d.lon.between(window[0], window[1]) & d.lat.between(window[2], window[3])) | (d.lon_end.between(window[0], window[1]) & d.lat_end.between(window[2], window[3]))]
        mask = mask.reindex(d.index, fill_value=False) if mask is not None else None
        import pandas as pd
        mk = mask if mask is not None else pd.Series(True, index=d.index)
        lr = info.get("latest_round")
        sub = " | ".join(x for x in [a["country"].title() + ((", " + a["district"].title()) if a.get("district") else ""),
                                      ("latest round %s %s (%s to %s)" % (lr["response"], lr["round"], lr["first_route"], lr["last_route"])) if lr else (str(a["year"]) if a.get("year") else ""),
                                      ("window %s m wide" % int(round((window[1] - window[0]) * 111320 * 0.999))) if a.get("radius_m") else ""] if x)
        title = a.get("title") or (label or "Footprint of IM routes with missed children, %s" % a["country"].title())
        try:
            bgm = {"satellite": "sat", "osm": "osmtile", "none": "none"}.get(a.get("background") or "", "auto")
            png, minfo = st.render_footprint(d, mk, window, os.path.join(tempfile.mkdtemp(prefix="sia_fp_"), "footprint.png"), boundaries_dir(self.engine), R, a["country"], title, sub, bgm,
                                             st.footprint_meta(d[mk], a["country"], title, sub, place=label))
        except (MapError, RuntimeError) as e:
            return {"error": str(e)}
        slug = re.sub(r"[^a-z0-9]+", "_", (label or a["country"]).lower()).strip("_")[:40]
        self.files.append((png, "footprint_%s.png" % slug))
        mine = d[mk]
        return {"map_created": True, "downloadable": True, "place": label, "country": a["country"], "routes_drawn": int(len(mine)), "children_missed_on_drawn_routes": int(mine.children.sum()),
                "first_route": mine.date.min().strftime("%Y-%m-%d") if mine.date.notna().any() else None, "last_route": mine.date.max().strftime("%Y-%m-%d") if mine.date.notna().any() else None,
                "latest_round": lr, "window_width_m": minfo.get("window_width_m"), "scale_bar_m": minfo.get("scale_bar_m"), "background": minfo.get("background"), "satellite_photo": minfo.get("satellite"), "notes": notes,
                "warnings": ["Only monitor routes that found at least one missed child are drawn: routes that found nobody are not in the data.",
                             "Routes with GPS outside the country, more than 100 children on one route or longer than 5 km are left out."] + ([minfo["background_note"]] if minfo.get("background_note") else [])}

    # ----- pipeline jobs through the job runner -----
    @staticmethod
    def _jobs_module():
        try:
            import job_runner
            return job_runner
        except Exception:
            return None

    def _jobs_base(self):
        return self.jobs.BASE

    def _job_result(self, base, jid, t0=None):
        f = base / "done" / (jid + ".json")
        if not f.exists():
            state = "running" if (base / "running" / (jid + ".json")).exists() else "queued" if (base / "queue" / (jid + ".json")).exists() else "unknown id"
            return {"id": jid, "state": state}
        r = json.loads(f.read_text(encoding="utf-8"))
        out = {"id": jid, "job": r.get("job"), "state": "finished", "status": r.get("status"), "exit_code": r.get("exit_code"), "started": r.get("started"), "finished": r.get("finished"), "log_tail": (r.get("tail") or [])[-25:]}
        files = []
        if t0 is None and r.get("started"):
            try:
                import datetime as _dt
                t0 = _dt.datetime.fromisoformat(r["started"]).timestamp()
            except Exception:
                t0 = None
        outdir = base / "outputs"
        if outdir.exists() and t0 and r.get("status") == "ok":
            for p in sorted(outdir.glob("*")):
                if p.is_file() and not p.name.endswith(".json") and p.stat().st_mtime >= t0 - 2:
                    self.files.append((str(p), p.name)); files.append(p.name)
        jd = (self.jobs.JOBS.get(r.get("job")) or {}) if self.jobs else {}
        if jd.get("out") and r.get("status") == "ok":     # jobs that write their own folder (for example the SIA review in SIA_feedbacks\\refresh_<date>)
            import datetime as _dt
            folder = pathlib.Path(str(jd["out"]).replace("{date}", _dt.date.today().strftime("%Y%m%d")))
            if folder.exists():
                for p in sorted(folder.glob("*")):
                    if p.is_file() and p.suffix.lower() in (".xlsx", ".pptx", ".html", ".md", ".png", ".docx") and (t0 is None or p.stat().st_mtime >= t0 - 2):
                        self.files.append((str(p), p.name)); files.append(p.name)
        if files:
            out["files_ready_to_download"] = files
        return out

    def _run_job(self, a):
        import time, datetime
        if not self.jobs:
            return {"error": "pipeline jobs are only available on the owner's computer (the page started from the agent repository with the job runner)"}
        job, flags = a.get("job"), [str(x) for x in (a.get("flags") or [])]
        if job not in self.jobs.JOBS and job not in self.jobs.CHAINS:
            return {"error": "unknown job; jobs: " + ", ".join(sorted(self.jobs.JOBS) + sorted(self.jobs.CHAINS))}
        if any(f.split("=")[0] in SP_FLAGS or f.startswith("--sharepoint") for f in flags) and not a.get("user_confirmed_sharepoint"):
            return {"error": "a SharePoint upload needs the user's explicit yes in their own message; ask them, then call again with user_confirmed_sharepoint=true"}
        base = self._jobs_base()
        (base / "queue").mkdir(parents=True, exist_ok=True)
        t0 = time.time()
        jid = "%s_%s" % (datetime.datetime.now().strftime("%Y%m%d-%H%M%S-%f"), job)
        (base / "queue" / (jid + ".json")).write_text(json.dumps({"id": jid, "job": job, "flags": flags, "by": "assistant"}), encoding="utf-8")
        wait = min(int(a.get("wait_seconds") or 300), 900)
        while time.time() - t0 < wait:
            time.sleep(4)
            r = self._job_result(base, jid, t0)
            if r["state"] == "finished":
                return r
            if r["state"] == "queued" and time.time() - t0 > 25:
                return {"id": jid, "state": "queued", "message": "the job runner does not pick the job up: it is not running on this computer. Start it with: python tools/job_runner.py (the job stays queued and starts as soon as it runs)"}
        return {"id": jid, "state": "running", "message": "still running after %d s: tell the user it is running and to ask for the status later" % wait}

    def _job_status(self, a):
        if not self.jobs:
            return {"error": "pipeline jobs are only available on the owner's computer"}
        base = self._jobs_base()
        jid = a.get("id")
        if not jid:
            allj = sorted(list((base / "done").glob("*.json")) + list((base / "running").glob("*.json")) + list((base / "queue").glob("*.json")), key=lambda p: p.name)
            if not allj:
                return {"error": "no job yet"}
            jid = allj[-1].stem
        return self._job_result(base, jid, None)

    def _find_settlement(self, a):
        import settlement_tools as st
        if "settlement" not in self.engine.ds or not self.engine.available("settlement"):
            return {"error": "the settlement files (AFRO-Dash/input/<form id>.csv.gz) are not available in this installation"}
        df = self.engine.load("settlement")[0]
        cands = st.search_places(df, a.get("name", ""), a.get("country"), a.get("district"), a.get("province"), limit=10)
        if not cands:
            return {"found": False, "message": "no place of this name in the settlement files%s: check the spelling, or give the country; places without any missed child are not in the files" % (" of " + a["country"] if a.get("country") else "")}
        numbered = [dict(number=i + 1, **c) for i, c in enumerate(cands)]
        exact = [c for c in cands if c["match"] == "exact name"]
        pick = a.get("pick")
        if pick:
            if not 1 <= int(pick) <= len(cands):
                return {"error": "pick must be between 1 and %d" % len(cands), "candidates": numbered}
            chosen = cands[int(pick) - 1]
        elif len(cands) == 1:
            chosen = cands[0]
        elif len(exact) == 1:
            chosen = exact[0]
        else:
            return {"resolved": False, "message": "several places match: ask the user which one (country and district), then call again with `pick`", "candidates": numbered}
        prof = st.profile(df, chosen["country"], chosen["district"], chosen["place"], chosen["province"])
        out = {"resolved": True, "other_candidates": [c for c in numbered if c["place"] != chosen["place"] or c["district"] != chosen["district"]][:6], "profile": prof,
               "warnings": ["Only monitor routes that found at least one missed child are in the files: there is no coverage rate or denominator for a settlement.",
                            "Routes with GPS outside the country, over 100 children on one route or longer than 5 km are left out (routes_left_out says how many)."] + ([prof["note"]] if prof and prof.get("note") else [])}
        if a.get("map") and prof and prof.get("routes_used"):
            from map_tools import rscript, boundaries_dir, MapError
            import tempfile
            R = rscript()
            if not R:
                out["map_error"] = "R is not installed on this computer: maps need Rscript"
            else:
                try:
                    png, info = st.render_place_map(df, chosen["country"], chosen["district"], chosen["place"], os.path.join(tempfile.mkdtemp(prefix="sia_place_"), "place.png"), boundaries_dir(self.engine), R, chosen["province"])
                    self.images.append(open(png, "rb").read())
                    out["map_created"] = True
                    out["district_by_gps"] = info.get("district_by_gps"); out["province_by_gps"] = info.get("province_by_gps")
                    if info.get("district_by_gps") and chosen["district"] == "NOT RECORDED":
                        out["warnings"].append("The form does not record the district of this place; the district %s is read from the GPS position on the AFRO-Dash boundaries." % info["district_by_gps"])
                except (MapError, RuntimeError) as e:
                    out["map_error"] = str(e)
        return out

    # ----- one question -----
    def ask(self, user_text, history=None):
        """Returns (reply, history, calls). Charts drawn during the question are in self.images (PNG bytes). history is the list of messages to send back with the next question."""
        self.images = []
        self.files = []
        msgs = list(history or []) + [{"role": "user", "content": user_text}]
        calls, retried, text, extra = [], False, "", []
        for _ in range(self.max_steps):
            r = self.client.messages.create(model=self.model, max_tokens=1500, system=self.system, tools=self.tools, messages=msgs + extra)
            uses = [b for b in r.content if getattr(b, "type", "") == "tool_use"]
            text = "".join(getattr(b, "text", "") for b in r.content if getattr(b, "type", "") == "text").strip()
            if not uses:
                if FIGURE.search(text) and not _supported(msgs) and not retried:  # a figure with no tool result anywhere in the conversation: ask once, without keeping the exchange in the history
                    retried = True
                    extra = [{"role": "assistant", "content": r.content}, {"role": "user", "content": NUDGE}]
                    continue
                msgs.append({"role": "assistant", "content": r.content})
                break
            extra = []
            msgs.append({"role": "assistant", "content": r.content})
            results = []
            for b in uses:
                res = self.dispatch(b.name, dict(b.input))
                calls.append({"tool": b.name, "input": dict(b.input), "error": res.startswith('{"error"')})
                results.append({"type": "tool_result", "tool_use_id": b.id, "content": res})
            msgs.append({"role": "user", "content": results})
        else:
            text = text or "I could not finish this question within the allowed steps."
        if FIGURE.search(text) and not _supported(msgs):
            text = "[Unverified: no data tool result supports the figure below]\n\n" + text
        return text, msgs, calls


def trail(calls):
    """One line per tool call, for the page and the logs."""
    out = []
    for c in calls:
        if c["tool"] in ("run_job", "job_status", "make_footprint_map"):
            i = c["input"]
            out.append("%s %s%s" % (c["tool"].replace("_", " "), i.get("job") or i.get("id") or i.get("place") or i.get("country") or "", "  [error]" if c["error"] else ""))
        elif c["tool"] == "find_settlement":
            i = c["input"]
            out.append("settlement %s%s%s" % (i.get("name", ""), (" (%s)" % ", ".join(str(i[k]) for k in ("district", "country") if i.get(k))) if (i.get("district") or i.get("country")) else "", "  [error]" if c["error"] else ""))
        elif c["tool"] in ("query_data", "make_chart", "make_map"):
            i = c["input"]
            f = ", ".join("%s=%s" % (k, v) for k, v in i.items() if k not in ("dataset", "metric") and v not in (None, "", [], {}))
            out.append("%s%s.%s%s%s" % ({"make_chart": "chart ", "make_map": "map "}.get(c["tool"], ""), i.get("dataset"), i.get("metric"), (" (" + f + ")") if f else "", "  [error]" if c["error"] else ""))
        else:
            out.append(c["tool"] + (" " + str(c["input"].get("dataset", "")) if c["input"].get("dataset") else ""))
    return out

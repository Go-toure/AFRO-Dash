#!/usr/bin/env python3
"""Assistant core: a language model that answers questions on the SIA data ONLY through the catalogue engine (native tool calling).   Author: Gorgui Ba Toure

The model never sees a record and never writes code: it chooses a tool (list_datasets, describe_data, query_data), the engine (tools/catalog_engine.py) runs a fixed metric of
catalog/engine_spec.json, and the model words the answer with the unit, period and warnings of the result. The rules are catalog/ASK_RULES.md.
Needs: pip install anthropic   and the environment variable ANTHROPIC_API_KEY (never in a file of a repository); model name in SIA_CHAT_MODEL.
Used by chat_app/app.py (Shiny for Python page) and tools/run_assistant_eval.py (the 78 questions, graded by tools/grade_assistant.py).
"""
import copy, json, os, re, sys

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
        self.system = system_prompt(self.engine.here, self.engine)
        self.tools = build_tools(self.engine)
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
            else:
                out = {"error": "unknown tool " + str(name)}
        except EngineError as e:
            out = {"error": str(e)}
        except Exception as e:  # a bad argument must not stop the conversation
            out = {"error": "the query failed: %s" % str(e)[:200]}
        return json.dumps(out, ensure_ascii=False, default=str)

    # ----- one question -----
    def ask(self, user_text, history=None):
        """Returns (reply, history, calls). Charts drawn during the question are in self.images (PNG bytes). history is the list of messages to send back with the next question."""
        self.images = []
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
        if c["tool"] in ("query_data", "make_chart", "make_map"):
            i = c["input"]
            f = ", ".join("%s=%s" % (k, v) for k, v in i.items() if k not in ("dataset", "metric") and v not in (None, "", [], {}))
            out.append("%s%s.%s%s%s" % ({"make_chart": "chart ", "make_map": "map "}.get(c["tool"], ""), i.get("dataset"), i.get("metric"), (" (" + f + ")") if f else "", "  [error]" if c["error"] else ""))
        else:
            out.append(c["tool"] + (" " + str(c["input"].get("dataset", "")) if c["input"].get("dataset") else ""))
    return out

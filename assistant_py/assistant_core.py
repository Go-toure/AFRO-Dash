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
            "Use the metric the question names: median_wastage_rate unless the question asks to leave out flagged reports (then median_wastage_rate_unflagged and say so). Counts of reports or visits cover every report unless the question asks for the latest report of each place (latest_only=true). "
            "When the question does not name the dataset, name the dataset you used. A dataset that is not in the list below is not available here: say so, do not guess. Answer in the language of the question, briefly.\n\nDATASETS AND THEIR METRICS (use these exact names):\n" + listing + "\n\nRULES OF THE CATALOGUE:\n" + rules)


class Assistant:
    def __init__(self, engine=None, client=None, model=None, max_steps=6):
        self.engine = engine or Engine()
        self.model = model or DEFAULT_MODEL
        self.max_steps = max_steps
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
            else:
                out = {"error": "unknown tool " + str(name)}
        except EngineError as e:
            out = {"error": str(e)}
        except Exception as e:  # a bad argument must not stop the conversation
            out = {"error": "the query failed: %s" % str(e)[:200]}
        return json.dumps(out, ensure_ascii=False, default=str)

    # ----- one question -----
    def ask(self, user_text, history=None):
        """Returns (reply, history, calls). history is the list of messages to send back with the next question."""
        msgs = list(history or []) + [{"role": "user", "content": user_text}]
        calls, retried, text = [], False, ""
        for _ in range(self.max_steps):
            r = self.client.messages.create(model=self.model, max_tokens=1500, system=self.system, tools=self.tools, messages=msgs)
            msgs.append({"role": "assistant", "content": r.content})
            uses = [b for b in r.content if getattr(b, "type", "") == "tool_use"]
            text = "".join(getattr(b, "text", "") for b in r.content if getattr(b, "type", "") == "text").strip()
            if not uses:
                if FIGURE.search(text) and not calls and not retried:  # a figure without a tool result: ask once to use the tools
                    retried = True
                    msgs.append({"role": "user", "content": "You stated a figure without a tool result. Use query_data (or say you cannot answer from the catalogue)."})
                    continue
                break
            results = []
            for b in uses:
                res = self.dispatch(b.name, dict(b.input))
                calls.append({"tool": b.name, "input": dict(b.input), "error": '"error"' in res[:20]})
                results.append({"type": "tool_result", "tool_use_id": b.id, "content": res})
            msgs.append({"role": "user", "content": results})
        else:
            text = text or "I could not finish this question within the allowed steps."
        if FIGURE.search(text) and not any(c["tool"] == "query_data" and not c["error"] for c in calls):
            text = "[Unverified: no data tool result supports the figure below]\n\n" + text
        return text, msgs, calls


def trail(calls):
    """One line per tool call, for the page and the logs."""
    out = []
    for c in calls:
        if c["tool"] == "query_data":
            i = c["input"]
            f = ", ".join("%s=%s" % (k, v) for k, v in i.items() if k not in ("dataset", "metric") and v not in (None, "", [], {}))
            out.append("%s.%s%s%s" % (i.get("dataset"), i.get("metric"), (" (" + f + ")") if f else "", "  [error]" if c["error"] else ""))
        else:
            out.append(c["tool"] + (" " + str(c["input"].get("dataset", "")) if c["input"].get("dataset") else ""))
    return out

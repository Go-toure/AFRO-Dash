#!/usr/bin/env python3
"""Catalogue query engine: answers a question about the SIA data by running a metric of catalog/engine_spec.json, never by free code.   Author: Gorgui Ba Toure

    python tools/catalog_engine.py list                                   # datasets and metrics
    python tools/catalog_engine.py describe lqas                          # one dataset: grain, dimensions, metrics, caveats
    python tools/catalog_engine.py query lqas pass_rate --year 2026 --by country --min-n 30
    python tools/catalog_engine.py query team_supervision indicator_pct_yes --param indicator=ind_house_marked --filter team_type=H2H
    python tools/catalog_engine.py selftest                               # the 78 questions of catalog/test_questions.json, no model
    python tools/catalog_engine.py check                                  # spec versus semantic_layer.json (every metric documented, every file readable)

A result carries: metric, unit, definition, formula, filters, period, rows_used, value (or table), overall, warnings, data_as_of, catalogue_version.
The metric expressions are written in this repository (engine_spec.json); a user or a model only chooses the dataset, metric, dimension and filters.
Nothing is returned that is a record: only counts, rates and totals. Each dataset has an ordered list of sources (spec "sources"): the data repositories (folder SIA_DATA_ROOT) and, for LQAS and admin, the dashboard's own input files in AFRO-Dash/input (read with pyreadr). A dataset with no source found is "not available" in that installation and is left out of the lists. The admin snapshot is read once and cached in state/admin_subset.pkl (not in git).
It can be imported: from catalog_engine import Engine; Engine().query("lqas", "pass_rate", filters={"year": 2026}).
"""
import argparse, glob, json, os, sys

def _home():
    """The folder that holds catalog/engine_spec.json: the folder of this file when the assistant is deployed (AFRO-Dash/assistant_py), its parent in the agent repository (tools/)."""
    d = os.path.dirname(os.path.abspath(__file__))
    return d if os.path.exists(os.path.join(d, "catalog", "engine_spec.json")) else os.path.dirname(d)


HERE = _home()
ROOT = os.path.dirname(HERE)
DEPLOYED = os.path.exists(os.path.join(HERE, "catalog_engine.py"))      # True inside AFRO-Dash/assistant_py, False in the agent repository
DASH = ROOT if DEPLOYED else os.path.join(ROOT, "AFRO-Dash")           # where the dashboard's own input files are
DATA_ROOT = os.environ.get("SIA_DATA_ROOT") or (None if DEPLOYED else ROOT)  # where the other data repositories are (never guessed when deployed)
ADMIN_COLS = ["Ctry_Code", "Country", "Province", "District", "Year", "SIA_date", "SIAMTH", "Round_Add", "Response", "Vaccine_type", "PopPolio",
              "TotalNbrVaccPolio", "CVPolio", "Tot_Vacc_nOPV2", "Total_Vacc_bOPV", "AFRO_block", "geo_code"]


class EngineError(Exception):
    pass


class Engine:
    def __init__(self, root=None, spec_path=None):
        self.data_root = root or DATA_ROOT
        self.dash = DASH
        self.here = HERE
        sp = spec_path or os.path.join(self.here, "catalog", "engine_spec.json")
        self.spec = json.load(open(sp, encoding="utf-8"))
        self.ds = self.spec["datasets"]
        sem_raw = json.load(open(os.path.join(self.here, "catalog", "semantic_layer.json"), encoding="utf-8"))
        self.sem = dict(sem_raw)
        self.sem["datasets"] = {d.get("id") or d.get("name"): d for d in sem_raw["datasets"]}
        try:
            self.version = json.load(open(os.path.join(self.here, "catalog", "data_catalog.json"), encoding="utf-8")).get("generated_at", "")
        except Exception:
            self.version = self.spec.get("generated_at", "")
        self._cache = {}

    # ---------- loading ----------
    def _resolve(self, pat):
        for tag, base in (("@data/", self.data_root), ("@dash/", self.dash), ("@here/", self.here)):
            if pat.startswith(tag):
                if not base:
                    return None
                full = os.path.join(base, pat[len(tag):])
                if "*" in full:
                    c = glob.glob(full)
                    return max(c, key=os.path.getmtime) if c else None
                return full if os.path.exists(full) else None
        return None

    def source(self, name):
        for src in self.ds[name]["sources"]:
            p = self._resolve(src["path"])
            if p:
                return src["kind"], p
        return None

    def available(self, name):
        return name in self.ds and self.source(name) is not None

    def load(self, name):
        import pandas as pd
        if name in self._cache:
            return self._cache[name]
        if name not in self.ds:
            raise EngineError("unknown dataset '%s'; datasets: %s" % (name, ", ".join(d["dataset"] for d in self.datasets())))
        s = self.ds[name]
        src = self.source(name)
        if not src:
            raise EngineError("dataset '%s' is not available in this installation (no data file found; the data repositories are read from the folder in SIA_DATA_ROOT)" % name)
        kind, p = src
        if kind == "csv":
            cols = s.get("usecols")
            df = pd.read_csv(p, usecols=(lambda c: c in cols) if cols else None, low_memory=False)
        elif kind == "parquet":
            import pyarrow.parquet as pq
            have = set(pq.ParquetFile(p).schema_arrow.names)
            df = pq.read_table(p, columns=[c for c in s["usecols"] if c in have]).to_pandas()
        elif kind == "rawcat":
            df = pd.DataFrame(json.load(open(p, encoding="utf-8"))["files"])
        elif kind == "rds":
            import pyreadr
            df = list(pyreadr.read_r(p).values())[0]
            keep = s.get("usecols") or (ADMIN_COLS if name == "admin" else None)
            if keep:
                df = df[[c for c in df.columns if c in keep]]
        elif kind == "settlement":   # route-level files of the IM Settlement Maps tab (AFRO-Dash/input/<form id>.csv.gz), cleaned by tools/settlement_tools.py
            from settlement_tools import load_settlements
            df = load_settlements(os.path.dirname(p))
        elif kind == "admin":
            cache = os.path.join(self.here, "state", "admin_subset.pkl")
            if os.path.exists(cache) and os.path.getmtime(cache) > os.path.getmtime(p):
                df = pd.read_pickle(cache)
            else:
                df = pd.read_excel(p, usecols=lambda c: c in ADMIN_COLS, engine="openpyxl")
                os.makedirs(os.path.dirname(cache), exist_ok=True)
                df.to_pickle(cache)
        else:
            raise EngineError("unknown source kind " + kind)
        dc = s.get("date_col")
        if dc and dc in df.columns:
            df[".date"] = pd.to_datetime(df[dc], errors="coerce")
            df[".month"] = df[".date"].dt.strftime("%Y-%m")
        rdc = s.get("round_date")
        if rdc and rdc in df.columns:
            df[".rdate"] = pd.to_datetime(df[rdc], errors="coerce")
        self._cache[name] = (df, p)
        return self._cache[name]

    def rows_of(self, name):
        return len(self.load(name)[0])

    # ---------- description ----------
    def datasets(self, include_unavailable=False):
        out = []
        for k, s in self.ds.items():
            if not include_unavailable and not self.available(k):
                continue
            sem = self.sem.get("datasets", {}).get(s.get("semantic") or "", {})
            out.append({"dataset": k, "title": sem.get("title") or ("raw: " + k), "entity": s["entity"], "dimensions": list(s["dims"]) + (["month"] if s.get("date_col") else []), "metrics": list(s["metrics"]), "raw": bool(s.get("raw")), "available": self.available(k)})
        return out

    def describe(self, name):
        if name not in self.ds:
            raise EngineError("unknown dataset '%s'; datasets: %s" % (name, ", ".join(d["dataset"] for d in self.datasets())))
        if not self.available(name):
            raise EngineError("dataset '%s' is not available in this installation; available: %s" % (name, ", ".join(d["dataset"] for d in self.datasets())))
        s = self.ds[name]
        sem = self.sem.get("datasets", {}).get(s.get("semantic") or "", {})
        r = {"dataset": name, "title": sem.get("title"), "grain": sem.get("grain"), "entity": s["entity"], "raw": bool(s.get("raw")),
             "dimensions": list(s["dims"]) + (["month"] if s.get("date_col") else []),
             "latest_only_default": bool(s.get("latest", {}).get("default")), "latest_only_note": s.get("latest", {}).get("note"),
             "metrics": {k: {"unit": m["unit"], "definition": m["def"], "formula": m["formula"], **({"parameters": m["params"]} if m.get("params") else {})} for k, m in s["metrics"].items()},
             "caveats": sem.get("caveats", [])}
        return r

    # ---------- query ----------
    @staticmethod
    def _norm(v):
        return str(v).strip().lower()

    def _select(self, s, df, filters, months, latest_only, all_rows=False, latest_round=False):
        import pandas as pd
        notes = []
        lt = s.get("latest")
        if lt and (latest_only if latest_only is not None else (lt.get("default") and not all_rows)):
            df = df[df[lt["col"]].astype(str) == "True"]
            notes.append("latest report of each place and round only")
        applied = {}
        for k, v in (filters or {}).items():
            if v in (None, "", []):
                continue
            col = ".month" if k == "month" else s["dims"].get(k)
            if col is None or col not in df.columns:
                raise EngineError("'%s' is not a dimension of this dataset; dimensions: %s" % (k, ", ".join(s["dims"])))
            vals = v if isinstance(v, (list, tuple)) else [v]
            if pd.api.types.is_numeric_dtype(df[col]):
                nv = []
                for x in vals:
                    try:
                        nv.append(float(x))
                    except Exception:
                        raise EngineError("'%s' must be a number" % k)
                df = df[df[col].isin(nv)]
            else:
                want = {self._norm(x) for x in vals}
                df = df[df[col].astype(str).str.strip().str.lower().isin(want)]
            applied[k] = v
        if months:
            ms = months if isinstance(months, (list, tuple)) else [months]
            if all(isinstance(m, int) or str(m).isdigit() for m in ms):
                df = df[df[".date"].dt.month.isin([int(m) for m in ms])]
            else:
                df = df[df[".month"].isin([str(m) for m in ms])]
            applied["months"] = ms
        if latest_round:
            rk = s.get("round_key")
            if not rk:
                raise EngineError("this dataset has no rounds: latest_round is not available")
            dcol = ".rdate" if ".rdate" in df.columns else ".date"
            d = df[df[dcol].notna()]
            if len(d):
                row = d.loc[d[dcol].idxmax()]
                cols = [s["dims"][k] for k in rk]
                def norm(series):
                    return series.astype(str).str.strip().str.upper()
                sel = df
                for c in cols:
                    sel = sel[norm(sel[c]) == str(row[c]).strip().upper()]
                df = sel
                nm = ", ".join("%s %s" % (k, row[c]) for k, c in zip(rk, cols))
                a, b = df[dcol].min(), df[dcol].max()
                notes.append("latest round of this selection: %s (round started %s%s). A round is a campaign, a vaccine and a round label: labels such as RND3 repeat across campaigns, so never choose a round by its label" % (nm, a.strftime("%Y-%m-%d"), "" if a == b else " to " + b.strftime("%Y-%m-%d")))
                applied["latest_round"] = {k: self._plain(row[c]) for k, c in zip(rk, cols)}
        return df, applied, notes

    def _eval(self, expr, df, p, name):
        import numpy as np, pandas as pd
        return eval(expr, {"df": df, "pd": pd, "np": np, "p": p, "rows_of": self.rows_of}, {})

    @staticmethod
    def _plain(v):
        import numpy as np
        if isinstance(v, np.integer):
            return int(v)
        if isinstance(v, (np.floating, float)):
            return None if v != v else float(v)
        if hasattr(v, "isoformat"):
            return v.strftime("%Y-%m-%d") if hasattr(v, "strftime") else str(v)
        return v

    def query(self, dataset, metric, by=None, filters=None, months=None, latest_only=None, params=None, min_n=None, top_n=None, complete_lots_only=None, latest_round=False):
        import pandas as pd
        df0, path = self.load(dataset)
        s = self.ds[dataset]
        if metric not in s["metrics"]:
            raise EngineError("'%s' is not a metric of %s; metrics: %s" % (metric, dataset, ", ".join(s["metrics"])))
        m = s["metrics"][metric]
        p = dict(params or {})
        if complete_lots_only is not None:
            p["complete_lots_only"] = bool(complete_lots_only)
        for k, allowed in (m.get("params") or {}).items():
            if k not in p:
                raise EngineError("metric %s needs the parameter '%s' (one of: %s)" % (metric, k, ", ".join(allowed)))
            if p[k] not in allowed:
                raise EngineError("parameter %s=%s is not allowed; allowed: %s" % (k, p[k], ", ".join(allowed)))
        df, applied, notes = self._select(s, df0, filters, months, latest_only, bool(m.get("all_rows")), bool(latest_round))
        res = {"dataset": dataset, "metric": metric, "unit": m["unit"], "definition": m["def"], "formula": m["formula"], "filters": applied, "notes": notes,
               "rows_used": int(len(df)), "catalogue_version": self.version}
        if m.get("params"):
            res["parameters"] = {k: p[k] for k in m["params"]}
        if len(df) == 0:
            res.update({"value": None, "warnings": ["No rows match these filters."]})
            return res
        if ".date" in df.columns and df[".date"].notna().any():
            res["period"] = {"from": df[".date"].min().strftime("%Y-%m-%d"), "to": df[".date"].max().strftime("%Y-%m-%d")}
        full, _ = self.load(dataset)
        if ".date" in full.columns:
            res["data_as_of"] = full[".date"].max().strftime("%Y-%m-%d")
        if m["kind"] == "table":
            t = self._eval(m["expr"], df, p, dataset)
            res["table"] = [{k: self._plain(v) for k, v in r.items()} for r in t.to_dict("records")]
        elif by:
            col = ".month" if by == "month" else s["dims"].get(by)
            if col is None or col not in df.columns:
                raise EngineError("'%s' is not a dimension of this dataset; dimensions: %s" % (by, ", ".join(s["dims"])))
            rows = []
            for g, d in df.groupby(col, dropna=False):
                if min_n and len(d) < int(min_n):
                    continue
                try:
                    v = self._plain(self._eval(m["expr"], d, p, dataset))
                except Exception:
                    v = None
                gk = self._plain(g) if g == g else "(missing)"
                if isinstance(gk, float) and gk.is_integer():
                    gk = int(gk)
                rows.append({"group": gk, "value": v, "n": int(len(d))})
            ordered_by_key = by in ("year", "month", "round")
            text_values = any(isinstance(r["value"], str) for r in rows)   # a class per group (for example "high"): alphabetical, nothing to rank
            rows.sort(key=lambda r: (r["group"] if (ordered_by_key or text_values) else (-(r["value"] if r["value"] is not None else float("-inf")))))
            if top_n:
                rows = rows[: int(top_n)]
            res["table"] = rows
            res["by"] = by
            res["overall"] = self._plain(self._eval(m["expr"], df, p, dataset))
        else:
            res["value"] = self._plain(self._eval(m["expr"], df, p, dataset))
        res["warnings"] = self._warnings(s, metric, df, p, res)
        if latest_round:
            res["warnings"].append(res["notes"][-1] if res.get("notes") else "latest round")
        if s.get("latest") and m.get("all_rows") and latest_only is None:
            res["warnings"].append("This counts every report of the repository, repeated reports of the same place and round included; latest_only=true keeps only the last report of each place and round.")
        return res

    def _warnings(self, s, metric, df, p, res):
        out = []
        for w in s.get("warnings", []):
            if metric not in w["metrics"]:
                continue
            if w.get("only_if") and not eval(w["only_if"], {"p": p}):
                continue
            txt = w["text"]
            if "count" in w:
                try:
                    n = self._eval(w["count"], df, p, "")
                except Exception:
                    continue
                if n is None or n <= w.get("min_count", 0) and "min_count" not in w:
                    continue
                if "min_count" in w and n < w["min_count"]:
                    continue
                txt = txt.replace("{n}", "{:,}".format(int(n))).replace("{rows}", "{:,}".format(len(df)))
                if "%d" in txt:
                    txt = txt % int(n)
            out.append(txt)
        return out

    # ---------- picking (a figure out of a result, as the test sheet asks) ----------
    @staticmethod
    def pick(res, how):
        how = how or "value"
        if res.get("value") is None and "table" not in res:
            return None
        t = res.get("table")
        if how == "value":
            return res.get("value")
        if how == "top_reason":
            return t[0]["reason"]
        if how == "last_group":
            return t[-1]["group"]
        if how == "table":
            return {str(r["group"]): r["value"] for r in t}
        if how.startswith("row:"):
            k = how[4:]
            for r in t:
                if str(r["group"]).replace(".0", "") == k:
                    return r["value"]
            return None
        if how.startswith("head:"):
            return {str(r["group"]): r["value"] for r in t[: int(how[5:])]}
        return None

    def run_call(self, call):
        c = dict(call)
        filt = {k: c[k] for k in ("country", "year", "vaccine", "status", "performance", "block", "level", "state", "team_type", "form", "pipeline", "province") if k in c}
        extra = c.get("filters") or {}
        filt.update(extra)
        return self.query(c["dataset"], c["metric"], by=c.get("by"), filters=filt, months=c.get("months"), latest_only=c.get("latest_only"),
                          params=c.get("params"), min_n=c.get("min_n"), top_n=c.get("top_n"), complete_lots_only=c.get("complete_lots_only"), latest_round=c.get("latest_round", False))


def same(got, exp, tol, qid=""):
    if isinstance(exp, dict):
        if not isinstance(got, dict):
            return False
        return all(k in got and same(got[k], v, tol) for k, v in exp.items())
    if isinstance(exp, str):
        g = str(got)
        if qid == "lqas_10":
            import re
            return bool(re.search(exp.replace("r_", "", 1).lower(), re.sub("[- _]", "", g.lower()))) or g.lower() == "child absent"
        return exp.startswith(g) or g == exp or g.startswith(exp)
    try:
        return abs(float(got) - float(exp)) <= (tol or 0) + 1e-9
    except Exception:
        return False


def selftest(eng, qfile, only=None, verbose=True):
    spec = json.load(open(qfile, encoding="utf-8"))
    ok = bad = rule = gap = skipped = 0
    lines = []
    for q in spec["questions"]:
        if only and q["dataset"] not in only:
            continue
        tc = q.get("tool_call")
        if q["dataset"] in eng.ds and not eng.available(q["dataset"]):
            skipped += 1
            continue
        if not tc:
            if q.get("trap") or q["id"] in ("trap_03", "trap_04"):
                rule += 1
            else:
                gap += 1
                lines.append("  %-9s NO TOOL CALL (gap)" % q["id"])
            continue
        try:
            r = eng.run_call(tc)
            got = Engine.pick(r, tc.get("pick"))
            good = same(got, q["expected"], q.get("tolerance"), q["id"])
        except Exception as e:
            got, good = "error: %s" % e, False
        if good:
            ok += 1
        else:
            bad += 1
            lines.append("  %-9s FAIL got %s | expected %s" % (q["id"], str(got)[:90], str(q["expected"])[:90]))
    if verbose:
        print("\n".join(lines))
    print("%d pass, %d fail, %d rule only (refusal or correction), %d without a tool call%s" % (ok, bad, rule, gap, (", %d skipped (dataset not available here)" % skipped) if skipped else ""))
    return bad == 0 and gap == 0


def check(eng):
    problems = []
    for k, s in eng.ds.items():
        if not eng.available(k):
            print("skipped %s (no data file in this installation)" % k)
            continue
        try:
            df, p = eng.load(k)
        except Exception as e:
            problems.append("%s: cannot read (%s)" % (k, e))
            continue
        for d, c in s["dims"].items():
            if c not in df.columns:
                problems.append("%s: dimension %s -> column %s is not in the file" % (k, d, c))
        sem = s.get("semantic")
        if sem and sem not in eng.sem.get("datasets", {}):
            problems.append("%s: no dataset '%s' in semantic_layer.json" % (k, sem))
        for mk, m in s["metrics"].items():
            if m["kind"] == "table" or (m.get("params")):
                continue
            try:
                eng._eval(m["expr"], df.copy(), {}, k)
            except Exception as e:
                problems.append("%s.%s: expression fails (%s)" % (k, mk, str(e)[:100]))
    for p in problems:
        print("PROBLEM", p)
    print("%d dataset(s), %d metric(s), %d problem(s)" % (len(eng.ds), sum(len(s["metrics"]) for s in eng.ds.values()), len(problems)))
    return not problems


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("cmd", choices=["list", "describe", "query", "selftest", "check"])
    ap.add_argument("dataset", nargs="?")
    ap.add_argument("metric", nargs="?")
    ap.add_argument("--by")
    ap.add_argument("--filter", action="append", default=[], help="dimension=value (repeatable)")
    for k in ("country", "year", "vaccine", "status", "performance", "block", "level"):
        ap.add_argument("--" + k)
    ap.add_argument("--months", help="comma list: 1,2 or 2026-06")
    ap.add_argument("--all-reports", action="store_true", help="Form A and preparedness: keep every report, not only the latest of each place")
    ap.add_argument("--param", action="append", default=[], help="name=value (repeatable)")
    ap.add_argument("--min-n", type=int)
    ap.add_argument("--top-n", type=int)
    ap.add_argument("--questions", default=os.path.join(HERE, "catalog", "test_questions.json"))
    ap.add_argument("--only", default="", help="selftest: comma list of datasets")
    a = ap.parse_args()
    eng = Engine()
    try:
        if a.cmd == "list":
            for d in eng.datasets():
                print("%-26s %-14s dims: %s" % (d["dataset"], d["entity"], ", ".join(d["dimensions"])))
                print("    metrics: " + ", ".join(d["metrics"]))
        elif a.cmd == "describe":
            print(json.dumps(eng.describe(a.dataset), indent=1, ensure_ascii=False))
        elif a.cmd == "query":
            filt = dict(x.split("=", 1) for x in a.filter)
            for k in ("country", "year", "vaccine", "status", "performance", "block", "level"):
                if getattr(a, k):
                    filt[k] = getattr(a, k)
            ms = [x.strip() for x in a.months.split(",")] if a.months else None
            r = eng.query(a.dataset, a.metric, by=a.by, filters=filt, months=ms, latest_only=False if a.all_reports else None,
                          params=dict(x.split("=", 1) for x in a.param), min_n=a.min_n, top_n=a.top_n)
            print(json.dumps(r, indent=1, ensure_ascii=False, default=str))
        elif a.cmd == "selftest":
            sys.exit(0 if selftest(eng, a.questions, set(a.only.split(",")) if a.only else None) else 1)
        elif a.cmd == "check":
            sys.exit(0 if check(eng) else 1)
    except EngineError as e:
        print("ERROR:", e)
        sys.exit(2)


if __name__ == "__main__":
    main()

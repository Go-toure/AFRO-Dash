"""Maps of the catalogue figures: one engine result per place, drawn on the AFRO-Dash boundaries by make_map.R.   Author: Gorgui Ba Toure

Level of the map, chosen from the dataset: district by AFROGDB code when the dataset has geo_code (LQAS, admin, IM), else district or province by name inside ONE country
(form_a, preparedness, team_supervision, vaccine_accountability, social_mobilization), else the country. `level` overrides.
Needs R (sf, ggplot2, dplyr, jsonlite) and the boundary files of AFRO-Dash/input. Nothing is drawn from memory: the values are the engine's.
"""
import csv, glob, json, os, shutil, subprocess, tempfile

BOUND = "africa_districts_simplified.rds"
LEVELS = ("geo_code", "district", "province", "country")
BY = {  # level -> the dimension names that carry it, first match wins
    "geo_code": ("geo_code",), "district": ("district", "lga"), "province": ("province", "state", "region"), "country": ("country",)}


class MapError(Exception):
    pass


def rscript():
    p = shutil.which("Rscript")
    if p:
        return p
    for base in (os.environ.get("ProgramFiles", r"C:\Program Files"), os.environ.get("ProgramFiles(x86)", r"C:\Program Files (x86)")):
        f = sorted(glob.glob(os.path.join(base, "R", "R-*", "bin", "Rscript.exe")))
        if f:
            return f[-1]
    return None


def boundaries_dir(eng):
    for tag in ("@dash/input/" + BOUND, "@here/data/" + BOUND, "@data/AFRO-Dash/input/" + BOUND):
        p = eng._resolve(tag)
        if p:
            return os.path.dirname(p)
    raise MapError("the boundary files (%s and the province and country files) are not found in AFRO-Dash/input" % BOUND)


def pick_level(eng, dataset, level=None, country=None):
    dims = eng.ds[dataset]["dims"]
    if level:
        if level not in LEVELS:
            raise MapError("level must be one of: " + ", ".join(LEVELS))
        order = [level]
    else:
        order = ["geo_code", "district", "province", "country"]
    for lv in order:
        for d in BY[lv]:
            if d in dims:
                if lv in ("district", "province") and not country:
                    if level:
                        raise MapError("a map by place name needs one country (the same name exists in several countries): give `country`")
                    continue
                return lv, d
    raise MapError("this dataset cannot be mapped at level '%s'; its dimensions: %s" % (level or "district/province/country", ", ".join(dims)))


def render_map(eng, call, title=None, level=None, out_png=None):
    """call = engine call (dataset, metric, filters, latest_round...). Returns (png bytes, info dict with the engine result)."""
    R = rscript()
    if not R:
        raise MapError("R is not installed on this computer: maps need Rscript (sf, ggplot2)")
    call = dict(call)
    dataset = call["dataset"]
    country = call.get("country") or (call.get("filters") or {}).get("country")
    lv, dim = pick_level(eng, dataset, level, country)
    call["by"] = dim
    call.pop("top_n", None)
    res = eng.run_call(call)
    rows = [r for r in res.get("table", []) if r.get("value") is not None and r.get("group") != "(missing)"]
    if not rows:
        raise MapError("the result has no values by %s: nothing to draw" % dim)
    bdir = boundaries_dir(eng)
    tmp = tempfile.mkdtemp(prefix="sia_map_")
    cs = os.path.join(tmp, "values.csv")
    with open(cs, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["key", "value", "n"])
        for r in rows:
            w.writerow([r["group"], r["value"], r.get("n", 1)])
    out = out_png or os.path.join(tmp, "map.png")
    unit = res.get("unit", "")
    per = res.get("period") or {}
    sub = " · ".join(x for x in [res.get("dataset", ""), "by " + ("district" if lv == "geo_code" else lv), ("%s to %s" % (per.get("from"), per.get("to"))) if per else "", "{:,} rows".format(res.get("rows_used", 0))] if x)
    ttl = title or "%s%s" % (res["metric"].replace("_", " ").capitalize(), (" — " + str(country).title()) if country else "")
    p = subprocess.run([R, os.path.join(os.path.dirname(os.path.abspath(__file__)), "make_map.R"), bdir, cs, out, lv, ttl, sub, str(country or "-"), unit], capture_output=True, text=True, timeout=300)
    if p.returncode or not os.path.exists(out):
        lines = [x for x in (p.stderr or p.stdout).strip().splitlines() if x.strip() and "Execution halted" not in x and not x.startswith("Calls:")]
        raise MapError("the map could not be drawn: %s" % (" ".join(lines[-2:]) or "R failed")[:300])
    info = {"level": lv, "by": dim, "country": country, "result": res, "map_png": out}
    try:
        info["placed"] = json.load(open(out + ".json", encoding="utf-8"))
    except Exception:
        info["placed"] = {}
    return open(out, "rb").read(), info


def main(argv=None):
    """python tools/map_tools.py --dataset D --metric M [--country C] [--year Y] [--level geo_code|district|province|country] [--latest-round] [--filter dim=value] [--title T] [--out file.png]"""
    import argparse
    sys_path_fix()
    import catalog_engine
    ap = argparse.ArgumentParser(description=main.__doc__)
    ap.add_argument("--dataset", required=True); ap.add_argument("--metric", required=True)
    ap.add_argument("--country"); ap.add_argument("--year"); ap.add_argument("--level"); ap.add_argument("--vaccine"); ap.add_argument("--title")
    ap.add_argument("--latest-round", action="store_true"); ap.add_argument("--filter", action="append", default=[]); ap.add_argument("--out")
    a = ap.parse_args(argv)
    call = {"dataset": a.dataset, "metric": a.metric}
    for k in ("country", "year", "vaccine"):
        if getattr(a, k):
            call[k] = getattr(a, k)
    if a.latest_round:
        call["latest_round"] = True
    if a.filter:
        call["filters"] = dict(f.split("=", 1) for f in a.filter)
    out = a.out
    if not out:
        base = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "_agent_jobs", "outputs")
        if not os.path.isdir(os.path.dirname(base)):
            base = os.path.join(tempfile.gettempdir(), "sia_maps")
        os.makedirs(base, exist_ok=True)
        out = os.path.join(base, "map_%s_%s%s.png" % (a.dataset, a.metric, ("_" + a.country) if a.country else ""))
    try:
        _, info = render_map(catalog_engine.Engine(), call, a.title, a.level, out)
    except (MapError, catalog_engine.EngineError) as e:
        print("MAP FAILED:", e)
        return 1
    r = info["result"]
    print("MAP:", info["map_png"])
    print("level=%s metric=%s unit=%s rows=%s placed=%s/%s" % (info["level"], r["metric"], r["unit"], r["rows_used"], info["placed"].get("matched"), info["placed"].get("total")))
    for w in r.get("warnings", []):
        print("WARNING:", w)
    return 0


def sys_path_fix():
    import sys
    here = os.path.dirname(os.path.abspath(__file__))
    if here not in sys.path:
        sys.path.insert(0, here)


if __name__ == "__main__":
    import sys
    sys.exit(main())

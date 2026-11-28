"""Settlement information from the route-level files of the IM Settlement Maps tab (AFRO-Dash/input/<form id>.csv.gz).   Author: Gorgui Ba Toure

One row of those files = one monitor ROUTE that found at least one missed child (start and end GPS, households, reasons, total children missed). They are built from the raw IM
forms by AFRO-Dash/tools/build_settlement_inputs.py and hold counts only: no child, caregiver or monitor identity. Routes that found nothing are not in the files, so a place has no
coverage rate here: only where, when, how many and why children were found missed.

load_settlements(dir)          cleaned table, the same rules as im_settlement_module.R (text cleaning, date placeholder, GPS box, 100 children, 5 km)
search_places(df, name, ...)   candidate places for a name (spelling differences allowed), with their routes and children
profile(df, country, district, place, province=None)   everything known about one place: totals, visits by month and round, reasons, routes, district context, GPS
Used by the catalogue engine (dataset settlement) and the assistant (tool find_settlement).
"""
import difflib, glob, os, re, unicodedata

MAX_ROUTE_M = 5000
MAX_CHILDREN = 100
BOX = {
    "ALGERIA": (18.9, 37.2, -8.8, 12.1),
    "ANGOLA": (-18.1, -4.3, 11.6, 24.2),
    "BENIN": (6.0, 12.5, 0.7, 3.9),
    "BOTSWANA": (-27.0, -17.7, 19.9, 29.5),
    "BURKINA FASO": (9.3, 15.2, -5.6, 2.5),
    "BURUNDI": (-4.5, -2.3, 28.9, 30.9),
    "CAMEROON": (1.6, 13.2, 8.3, 16.3),
    "CENTRAL AFRICAN REPUBLIC": (2.1, 11.1, 14.3, 27.6),
    "CHAD": (7.3, 23.6, 13.4, 24.1),
    "CONGO": (-5.1, 3.8, 11.0, 18.7),
    "COTE D IVOIRE": (4.2, 10.8, -8.7, -2.4),
    "DEMOCRATIC REPUBLIC OF THE CONGO": (-13.6, 5.5, 12.0, 31.4),
    "ETHIOPIA": (3.3, 15.0, 32.9, 48.1),
    "GHANA": (4.5, 11.3, -3.4, 1.3),
    "GUINEA": (7.1, 12.8, -15.2, -7.6),
    "GUINEA-BISSAU": (10.8, 12.8, -16.8, -13.6),
    "KENYA": (-4.8, 5.1, 33.8, 42.0),
    "LIBERIA": (4.2, 8.6, -11.6, -7.3),
    "MADAGASCAR": (-25.7, -11.9, 43.1, 50.6),
    "MALAWI": (-17.2, -9.3, 32.6, 36.0),
    "MAURITANIA": (14.6, 27.4, -17.2, -4.7),
    "MOZAMBIQUE": (-27.0, -10.4, 30.1, 41.0),
    "NAMIBIA": (-29.1, -16.9, 11.6, 25.4),
    "NIGER": (11.6, 23.6, 0.0, 16.1),
    "NIGERIA": (4.2, 14.0, 2.6, 14.8),
    "RWANDA": (-3.0, -1.0, 28.8, 31.0),
    "SENEGAL": (12.2, 16.8, -17.6, -11.3),
    "SIERRA LEONE": (6.8, 10.1, -13.4, -10.2),
    "SOUTH SUDAN": (3.4, 12.3, 23.3, 35.9),
    "TOGO": (5.9, 11.2, -0.2, 1.9),
    "UGANDA": (-1.6, 4.3, 29.5, 35.1),
    "UNITED REPUBLIC OF TANZANIA": (-11.8, -0.9, 29.2, 40.5),
    "ZAMBIA": (-18.2, -8.1, 21.9, 33.8),
    "ZIMBABWE": (-22.5, -15.5, 25.1, 33.1)
}
AFRICA = (-35.0, 38.0, -20.0, 55.0)
REASONS = {"absent": "Absent", "nc": "Non-compliance", "not_visited": "Not visited", "asleep": "Asleep", "vacc_routine": "Vaccinated (routine)", "other": "Other"}
RAW_REASON = {"absent": "reason_absent", "nc": "reason_nc", "not_visited": "reason_notvisited", "asleep": "reason_asleep", "vacc_routine": "reason_vaccinatedroutine", "other": "reason_others"}


def _txt(s, underscores=True):
    s = s.astype("string").str.strip().str.upper()
    if underscores:
        s = s.str.replace("_", " ", regex=False)
    s = s.str.replace(r"\s+", " ", regex=True)
    return s.mask(s.isin(["", "NA", "N/A"]))


def norm(x):
    """Comparison key of a place name: no accents, no punctuation, upper case."""
    x = unicodedata.normalize("NFKD", str(x)).encode("ascii", "ignore").decode()
    return re.sub(r"[^A-Z0-9]+", " ", x.upper()).strip()


def _haversine(a1, o1, a2, o2):
    import numpy as np
    r = 6371000.0
    p1, p2 = np.radians(a1), np.radians(a2)
    d = np.sin((p2 - p1) / 2) ** 2 + np.cos(p1) * np.cos(p2) * np.sin(np.radians(o2 - o1) / 2) ** 2
    return 2 * r * np.arcsin(np.sqrt(d))


def settlement_files(folder):
    return sorted(f for f in glob.glob(os.path.join(folder, "*.csv.gz")) if re.fullmatch(r"\d+\.csv\.gz", os.path.basename(f)))


def _read_one(path):
    import numpy as np, pandas as pd
    d = pd.read_csv(path, dtype=str, keep_default_na=False, na_values=[""], low_memory=False)
    d.columns = [c.strip().lower() for c in d.columns]
    if not {"country", "lat", "lon", "lat_end", "lon_end"} <= set(d.columns):
        return None
    g = lambda c: d[c] if c in d.columns else pd.Series([None] * len(d), index=d.index, dtype="object")
    num = lambda c: pd.to_numeric(g(c), errors="coerce")
    o = pd.DataFrame({"file_id": os.path.basename(path).split(".")[0], "country": _txt(g("country")), "province": _txt(g("province")), "district": _txt(g("district")),
                      "facility": _txt(g("facility")), "settlement": _txt(g("settlement"))})
    o["place"] = o["settlement"].fillna(o["facility"])
    o["response"] = _txt(g("response"), False).fillna("NOT RECORDED")
    o["round"] = _txt(g("roundnumber"), False).str.replace(" ", "", regex=False).fillna("NOT RECORDED")
    mon = g("monitoring").astype("string").str.upper().str.replace(r"[^A-Z]", "", regex=True)
    o["monitoring"] = np.where(mon == "ENDPROCESS", "END", "IN")
    dt = pd.to_datetime(g("date_monitored").astype("string").str[:10], errors="coerce")
    dt = dt.mask(dt < pd.Timestamp("2018-01-01"))
    o["date"] = dt
    o["year"] = dt.dt.year
    for c in ("lat", "lon", "lat_end", "lon_end"):
        o[c] = num(c)
    o["children"] = num("total_child_missed")
    for k, rc in RAW_REASON.items():
        o[k] = num(rc)
    hh = [c for c in d.columns if re.fullmatch(r"household_\d+", c)]
    o["hh_missed"] = (d[hh].apply(pd.to_numeric, errors="coerce") > 0).sum(axis=1) if hh else float("nan")
    o["route_m"] = _haversine(o.lat, o.lon, o.lat_end, o.lon_end)
    ctry = o["country"].dropna().iloc[0] if o["country"].notna().any() else None
    b = BOX.get(ctry, AFRICA)
    inside = lambda la, lo: la.notna() & lo.notna() & (la >= b[0]) & (la <= b[1]) & (lo >= b[2]) & (lo <= b[3])
    ok_pos = inside(o.lat, o.lon) & inside(o.lat_end, o.lon_end)
    o["qc"] = np.where(~ok_pos, "Outside country / missing GPS", np.where(o.children.notna() & (o.children > MAX_CHILDREN), "Implausible child count (> %d)" % MAX_CHILDREN,
                       np.where(o.route_m.notna() & (o.route_m > MAX_ROUTE_M), "Long route (> 5 km)", "OK")))
    return o


_CACHE = {}


def load_settlements(folder):
    import pandas as pd
    files = settlement_files(folder)
    if not files:
        raise FileNotFoundError("no settlement files (<form id>.csv.gz) in " + folder)
    sig = tuple((f, os.path.getmtime(f)) for f in files)
    if _CACHE.get("sig") == sig:
        return _CACHE["df"]
    parts = [p for p in (_read_one(f) for f in files) if p is not None and len(p)]
    df = pd.concat(parts, ignore_index=True)
    df["place_key"] = df["place"].map(lambda v: norm(v) if isinstance(v, str) else "")
    _CACHE.update(sig=sig, df=df)
    return df


def _ok(df):
    return df[df.qc == "OK"]


def search_places(df, name, country=None, district=None, province=None, limit=12):
    """Candidate places for a name: exact name first, then names that start with or contain it, then close spellings. Routes and children counted on routes that pass the quality rules."""
    import pandas as pd
    key = norm(name)
    if not key:
        return []
    d = df
    for col, v in (("country", country), ("province", province), ("district", district)):
        if v:
            d = d[d[col].map(lambda x: norm(x) if isinstance(x, str) else "") == norm(v)]
    d = d[d.place_key != ""]
    names = pd.Series(d.place_key.unique())
    exact = names[names == key]
    starts = names[names.str.startswith(key) & (names != key)]
    has = names[names.str.contains(re.escape(key), regex=True) & ~names.str.startswith(key)]
    toks = [t for t in key.split() if len(t) > 2]
    tokhit = names[[all(t in n.split() for t in toks) and bool(toks) for n in names]] if toks else names[:0]
    close = pd.Series(difflib.get_close_matches(key, list(names), n=15, cutoff=0.82))
    order = []
    for rank, grp in enumerate((exact, starts, has, tokhit, close)):
        for n in grp:
            if n not in [o[1] for o in order]:
                order.append((rank, n))
    want = {n: r for r, n in order}
    sel = _ok(d[d.place_key.isin(want)])
    if sel.empty:
        return []
    g = sel.groupby(["country", "province", "district", "place", "place_key"], dropna=False).agg(
        routes=("children", "size"), children=("children", "sum"), first_visit=("date", "min"), last_visit=("date", "max")).reset_index()
    g["match"] = g.place_key.map(lambda n: ["exact name", "name starts with it", "name contains it", "all words found", "close spelling"][want[n]])
    g["_r"] = g.place_key.map(want)
    g = g.sort_values(["_r", "children"], ascending=[True, False]).head(int(limit))
    out = []
    for r in g.to_dict("records"):
        out.append({"country": r["country"], "province": r["province"], "district": r["district"], "place": r["place"], "routes_with_missed_children": int(r["routes"]),
                    "children_missed": int(r["children"]), "first_visit": _d(r["first_visit"]), "last_visit": _d(r["last_visit"]), "match": r["match"]})
    return out


def _d(v):
    return None if v is None or v != v else v.strftime("%Y-%m-%d")


def _num(v):
    return None if v is None or v != v else (int(v) if float(v).is_integer() else round(float(v), 2))


def place_rows(df, country, district, place, province=None):
    d = df[df.place_key == norm(place)]
    d = d[d.country.map(lambda x: norm(x) if isinstance(x, str) else "") == norm(country)]
    if district:
        d = d[d.district.map(lambda x: norm(x) if isinstance(x, str) else "") == norm(district)]
    if province:
        d = d[d.province.map(lambda x: norm(x) if isinstance(x, str) else "") == norm(province)]
    return d


def profile(df, country, district, place, province=None):
    """Aggregated facts about ONE place (all rows are monitor routes; nothing about a person)."""
    import pandas as pd
    d_all = place_rows(df, country, district, place, province)
    if d_all.empty:
        return None
    d = _ok(d_all)
    first = d_all.iloc[0]
    out = {"place": first["place"], "country": first["country"], "province": first["province"], "district": first["district"],
           "also_named_as_facility": sorted(set(d_all["facility"].dropna()) - {first["place"]})[:5],
           "routes_in_file": int(len(d_all)), "routes_used": int(len(d)),
           "routes_left_out": {k: int(v) for k, v in d_all[d_all.qc != "OK"].qc.value_counts().items()}}
    if d.empty:
        out["note"] = "every route of this place fails a quality rule (GPS outside the country, over 100 children on one route, or a route longer than 5 km)"
        return out
    out["children_missed"] = int(d.children.sum())
    out["households_with_missed_children"] = _num(d.hh_missed.sum())
    out["first_visit"], out["last_visit"] = _d(d.date.min()), _d(d.date.max())
    out["visit_days"] = int(d.date.nunique())
    r = {REASONS[k]: float(d[k].sum()) for k in REASONS}
    tot = sum(r.values())
    out["reasons"] = [{"reason": k, "children": int(v), "share_percent": round(100 * v / tot, 1) if tot else None} for k, v in sorted(r.items(), key=lambda kv: -kv[1]) if v > 0]
    out["reasons_note"] = "reason counts can be smaller than children_missed when a route did not record the reason"
    m = d.dropna(subset=["date"]).assign(month=lambda x: x.date.dt.strftime("%Y-%m")).groupby("month").agg(routes=("children", "size"), children=("children", "sum")).reset_index()
    out["by_month"] = [{"month": r_["month"], "routes": int(r_["routes"]), "children": int(r_["children"])} for r_ in m.to_dict("records")][-24:]
    c = d.groupby(["response", "round"], dropna=False).agg(routes=("children", "size"), children=("children", "sum"), first=("date", "min"), last=("date", "max")).reset_index().sort_values("first")
    out["by_campaign_round"] = [{"response": r_["response"], "round": r_["round"], "routes": int(r_["routes"]), "children": int(r_["children"]), "from": _d(r_["first"]), "to": _d(r_["last"])} for r_ in c.to_dict("records")][-20:]
    out["monitoring_type"] = {("in process" if k == "IN" else "end process"): int(v) for k, v in d.monitoring.value_counts().items()}
    g = d.dropna(subset=["lat", "lon"])
    if len(g):
        out["gps"] = {"centre_lat": round(float(g.lat.median()), 5), "centre_lon": round(float(g.lon.median()), 5),
                      "lat_range": [round(float(g.lat.min()), 5), round(float(g.lat.max()), 5)], "lon_range": [round(float(g.lon.min()), 5), round(float(g.lon.max()), 5)],
                      "median_route_length_m": _num(round(float(d.route_m.median()), 0))}
    # context: the district and the country
    dd = _ok(df[(df.country == first["country"]) & (df.district == first["district"])]) if isinstance(first["district"], str) else d
    by_place = dd.groupby("place").children.sum().sort_values(ascending=False)
    if len(by_place):
        pos = int(list(by_place.index).index(first["place"]) + 1) if first["place"] in by_place.index else None
        out["district_context"] = {"places_in_district_with_missed_children": int(len(by_place)), "children_missed_in_district": int(by_place.sum()),
                                   "rank_of_this_place_by_children_missed": pos, "share_of_district_percent": round(100 * float(d.children.sum()) / float(by_place.sum()), 1) if by_place.sum() else None,
                                   "top_places_in_district": [{"place": k, "children": int(v)} for k, v in by_place.head(5).items()]}
    recent = d.sort_values("date", ascending=False).head(12)
    def main_reason(r_):
        vals = {k: r_[k] for k in REASONS if r_[k] == r_[k] and r_[k] > 0}
        return REASONS[max(vals, key=vals.get)] if vals else None
    out["latest_routes"] = [{"date": _d(r_["date"]), "response": r_["response"], "round": r_["round"], "children_missed": _num(r_["children"]), "main_reason": main_reason(r_),
                             "route_length_m": _num(round(float(r_["route_m"]), 0)) if r_["route_m"] == r_["route_m"] else None} for r_ in recent.to_dict("records")]
    if first["district"] == "NOT RECORDED":
        out["note"] = "the district is not recorded in the form of this place, so the district context below groups every place of the country file with an unrecorded district"
    return out


def points_for_map(df, country, district, place, province=None):
    """Start points of the routes of one place (lat, lon, children, date) for the map."""
    d = _ok(place_rows(df, country, district, place, province)).dropna(subset=["lat", "lon"])
    return d[["lat", "lon", "children", "date"]].copy()


def render_place_map(df, country, district, place, out_png, boundaries_dir, rscript, province=None, title=None):
    """Returns (png path, info from R: the district and province of the GPS position). PNG of the district boundary with every route start of the place (size = children missed) and, in grey, the other places of the district."""
    import csv, subprocess, tempfile
    d = _ok(df[(df.country == country)]) if country else df
    if district:
        d = d[d.district == district]
    d = d.dropna(subset=["lat", "lon"])
    mine = d.place_key == norm(place)
    tmp = tempfile.mkdtemp(prefix="sia_place_")
    cs = os.path.join(tmp, "points.csv")
    with open(cs, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["lat", "lon", "children", "mine"])
        for r, m in zip(d.head(20000).itertuples(), mine.head(20000)):
            w.writerow([r.lat, r.lon, int(r.children) if r.children == r.children else 0, 1 if m else 0])
    here = os.path.dirname(os.path.abspath(__file__))
    p = subprocess.run([rscript, os.path.join(here, "make_places_map.R"), boundaries_dir, cs, out_png, str(country), str(district or "-"), title or place,
                        "%s, %s" % (district or "", country)], capture_output=True, text=True, timeout=300)
    if p.returncode or not os.path.exists(out_png):
        raise RuntimeError("the map could not be drawn: " + ((p.stderr or p.stdout).strip().splitlines() or ["R failed"])[-1][:300])
    try:
        import json
        info = json.load(open(out_png + ".json", encoding="utf-8"))
    except Exception:
        info = {}
    return out_png, info


def select_routes(df, country, province=None, district=None, year=None, months=None, latest_round=False, response=None, round_=None):
    """Routes that pass the quality rules for a selection; latest_round keeps the most recent campaign round of the selection (found by date; a round = response + round label)."""
    d = _ok(df)
    for col, v in (("country", country), ("province", province), ("district", district), ("response", response), ("round", round_)):
        if v:
            d = d[d[col].map(lambda x: norm(x) if isinstance(x, str) else "") == norm(v)]
    if year:
        d = d[d.year == int(year)]
    if months:
        ms = [str(m) for m in (months if isinstance(months, (list, tuple)) else [months])]
        d = d[d.date.dt.strftime("%Y-%m").isin(ms)] if any("-" in m for m in ms) else d[d.date.dt.month.isin([int(m) for m in ms])]
    info = {}
    d = d.dropna(subset=["lat", "lon"])
    if latest_round and d.date.notna().any():
        row = d.loc[d.date.idxmax()]
        d = d[(d.response == row["response"]) & (d["round"] == row["round"])]
        info["latest_round"] = {"response": row["response"], "round": row["round"], "first_route": _d(d.date.min()), "last_route": _d(d.date.max())}
    return d, info


def footprint_window(d, mine_mask, radius_m=None):
    """(xmin, xmax, ymin, ymax) of the map: a radius around the place, else the place's routes, else the central 99% of the routes, with a margin."""
    import numpy as np
    ref = d[mine_mask] if mine_mask is not None and mine_mask.any() else d
    lon0, lat0 = float(ref.lon.median()), float(ref.lat.median())
    mdeg_lat = 111320.0
    mdeg_lon = 111320.0 * float(np.cos(np.radians(lat0)))
    if radius_m:
        r = float(radius_m)
        return (lon0 - r / mdeg_lon, lon0 + r / mdeg_lon, lat0 - r / mdeg_lat, lat0 + r / mdeg_lat)
    lo_x, hi_x = ref.lon.quantile(0.005), ref.lon.quantile(0.995)
    lo_y, hi_y = ref.lat.quantile(0.005), ref.lat.quantile(0.995)
    px = max((hi_x - lo_x) * 0.06, 150 / mdeg_lon); py = max((hi_y - lo_y) * 0.06, 150 / mdeg_lat)
    return (float(lo_x - px), float(hi_x + px), float(lo_y - py), float(hi_y + py))


def footprint_meta(mine, country, title, subtitle, place=None, note=None):
    """The information card of the footprint map: where, which round, how many routes / children / households, the reasons."""
    def uniq(col):
        v = [str(x).title() for x in mine[col].dropna().unique() if str(x).strip() and str(x).upper() != "NOT RECORDED"] if col in mine else []
        return v
    def one(col, many):
        v = uniq(col)
        return v[0] if len(v) == 1 else ("%d %s" % (len(v), many) if v else None)
    info = [("Country", str(country).title()), ("Province", one("province", "provinces")), ("District", one("district", "districts"))]
    if place:
        fac, setl = one("facility", "facilities"), one("settlement", "settlements")
        info += [("Facility", fac), ("Settlement", setl if setl != fac else None)]
    info = [[k, v] for k, v in info if v]
    resp = sorted({str(x) for x in mine["response"].dropna() if str(x).strip()}) if "response" in mine else []
    rnd = sorted({str(x) for x in mine["round"].dropna() if str(x).strip()}) if "round" in mine else []
    lines = []
    if resp:
        lines.append("Response: " + (resp[0] if len(resp) == 1 else "%d responses" % len(resp)) + ((" | Round " + rnd[0]) if len(rnd) == 1 else ""))
    d = mine["date"].dropna() if "date" in mine else []
    if len(d):
        a, b = d.min().strftime("%d %b %Y"), d.max().strftime("%d %b %Y")
        lines.append("Date(s): " + (a if a == b else a + " - " + b))
    reasons = {REASONS[k]: float(mine[k].sum()) for k in REASONS if k in mine and mine[k].sum() > 0}
    hh = float(mine["hh_missed"].sum()) if "hh_missed" in mine and mine["hh_missed"].notna().any() else None
    return {"country": str(country), "title": title, "subtitle": subtitle, "info": info, "round_lines": lines, "reasons": reasons,
            "totals": {"routes": int(len(mine)), "children": float(mine["children"].sum()), "households": hh},
            "note": note or "Only routes that found at least one missed child are in the data. Routes with GPS outside the country, over 100 children or over 5 km are left out."}


def render_footprint(routes, mine_mask, window, out_png, boundaries_dir, rscript, country, title, subtitle, bg="auto", meta=None):
    """Draws the footprint map with make_footprint_map.R; returns (png path, info from R).
    bg: sat (Esri satellite photo, as the dashboard's settlement map), osmtile, none (district boundary), auto (= sat: the satellite photo is the default at every zoom)."""
    import csv, subprocess, tempfile, json
    tmp = tempfile.mkdtemp(prefix="sia_fp_")
    cs = os.path.join(tmp, "routes.csv")
    r = routes.copy(); r["mine"] = mine_mask.astype(int).values if hasattr(mine_mask, "values") else 1
    r[["lat", "lon", "lat_end", "lon_end", "children", "mine"]].head(30000).to_csv(cs, index=False)
    width_m = (window[1] - window[0]) * 111320.0
    if bg in (None, False, "auto"):
        bg = "sat"
    elif bg is True:
        bg = "sat"
    m = dict(meta or {}); m.setdefault("country", str(country)); m.setdefault("title", str(title)); m.setdefault("subtitle", str(subtitle))
    json.dump(m, open(cs + ".meta.json", "w", encoding="utf-8"), ensure_ascii=False)
    here = os.path.dirname(os.path.abspath(__file__))
    p = subprocess.run([rscript, os.path.join(here, "make_footprint_map.R"), boundaries_dir, cs, out_png, str(country), "-", "-"] + ["%.6f" % x for x in window] + [bg, "%.0f" % width_m],
                       capture_output=True, text=True, timeout=300)
    if p.returncode or not os.path.exists(out_png):
        raise RuntimeError("the map could not be drawn: " + ((p.stderr or p.stdout).strip().splitlines() or ["R failed"])[-1][:300])
    try:
        info = json.load(open(out_png + ".json", encoding="utf-8"))
    except Exception:
        info = {}
    return out_png, info

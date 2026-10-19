#!/usr/bin/env python3
"""
snapshot_stats.py -- raw ONA counts for the AFRO-Dash "Snapshot" landing page.

The IM and LQAS repositories are aggregated per district-round, so they cannot
tell how many ONA submissions, settlements or health facilities were visited.
This script reads the RAW per-form parquet files (one row = one ONA submission)
and writes one small CSV with, per source / country / year:

    submissions   distinct ONA submissions (_id), valid date, not a test record
    settlements   settlements visited
                    IM   : distinct (district, settlement name) where the form
                           recorded a name + one per submission that has no name
                           (an IM submission is one settlement visit; several
                           country forms do not capture the name)
                    LQAS : submissions that carry a Cluster flag (a cluster IS the
                           settlement/village sampled -- same rule the LQAS
                           pipeline uses for `number_clusters`)
    settlements_named / settlements_unnamed   the two parts of `settlements`
    facilities    distinct health facilities named (district + facility name,
                  names normalised; "other"/test/blank values ignored)

Every (country, year) row is accompanied by a (country, "ALL") row holding the
all-years DISTINCT counts, so the dashboard can show exact totals for any
country selection with year = All, and for any single year.

Usage
-----
  python tools/snapshot_stats.py --source IM   --raw-dir <IM repo>/data/raw
  python tools/snapshot_stats.py --source LQAS --raw-dir <LQAS repo>/data/raw
  (default output: <AFRO-Dash>/input/AFRO_snapshot_stats_<source>.csv)

Only needs pandas + pyarrow (already used by both pipelines).
"""
import argparse
import csv
import datetime as dt
import glob
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict

import pandas as pd
import pyarrow.parquet as pq

# --------------------------------------------------------------------------
# Country names exactly as used in the IM / LQAS repositories
# --------------------------------------------------------------------------
DRC = "DEMOCRATIC REPUBLIC OF THE CONGO"
TZA = "UNITED REPUBLIC OF TANZANIA"
CODE_TO_NAME = {
    "RDC": DRC, "DRC": DRC, "COD": DRC,
    "MOZ": "MOZAMBIQUE", "TNZ": TZA, "TZA": TZA, "ZAMBIA": "ZAMBIA", "ZMB": "ZAMBIA",
    "ETH": "ETHIOPIA", "ANG": "ANGOLA", "AGO": "ANGOLA", "CHAD": "CHAD", "TCD": "CHAD",
    "MLW": "MALAWI", "MWI": "MALAWI", "MDG": "MADAGASCAR", "NIG": "NIGER", "NER": "NIGER",
    "CAE": "CAMEROON", "CMR": "CAMEROON", "CAMEROON": "CAMEROON",
    "ZIM": "ZIMBABWE", "ZWE": "ZIMBABWE", "CIV": "COTE D IVOIRE",
    "KEN": "KENYA", "BURKINA_FASO": "BURKINA FASO", "BFA": "BURKINA FASO",
    "BENIN": "BENIN", "BEN": "BENIN", "GHA": "GHANA", "UGA": "UGANDA", "RWA": "RWANDA",
    "RCA": "CENTRAL AFRICAN REPUBLIC", "CAF": "CENTRAL AFRICAN REPUBLIC",
    "GUI": "GUINEA", "GIN": "GUINEA", "ALG": "ALGERIA", "DZA": "ALGERIA",
    "COG": "CONGO", "BUI": "BURUNDI", "BDI": "BURUNDI", "MAU": "MAURITANIA",
    "MRT": "MAURITANIA", "SIL": "SIERRA LEONE", "SLE": "SIERRA LEONE",
    "BWA": "BOTSWANA", "TOGO": "TOGO", "TGO": "TOGO", "LIB": "LIBERIA", "LBR": "LIBERIA",
    "NAM": "NAMIBIA", "SEN": "SENEGAL", "SSUD": "SOUTH SUDAN", "SSD": "SOUTH SUDAN",
    "GNB": "GUINEA-BISSAU", "NGA": "NIGERIA", "NIGERIA": "NIGERIA",
}
# Forms with no usable Country column: raw file id -> country
FORM_FALLBACK = {"272": "NIGERIA", "7178": "NIGERIA", "3583": "GHANA"}

SETTLEMENT_COLS = ["namesettlement", "settlement_name", "settlementname", "village_name",
                   "village", "settlement", "locality_name", "locality"]
FACILITY_COLS = ["facility", "other_facility", "facility_other", "health_facility"]
DISTRICT_COLS = ["district", "lgas", "wards", "ward", "sub_district"]
DATE_COLS = {"IM": ["date_monitored", "today", "_submission_time"],
             "LQAS": ["date_of_lqas", "today", "_submission_time"]}

EMPTY = {"", "na", "n/a", "nan", "none", "null", "-", "--", "0", "unknown", "test", "testing",
         "tst", "other", "others", "autre", "autres"}


def norm(v):
    """lower-case, strip accents, keep letters/digits only."""
    if v is None or (isinstance(v, float) and v != v):
        return ""
    s = unicodedata.normalize("NFKD", str(v))
    s = "".join(ch for ch in s if not unicodedata.combining(ch)).lower()
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def clean_series(s):
    """Vectorised norm() with the EMPTY / 'other...' / numeric-only rules applied."""
    s = s.astype("string").fillna("")
    s = s.str.normalize("NFKD").str.replace(r"[̀-ͯ]", "", regex=True).str.lower()
    s = s.str.replace(r"[^a-z0-9]+", " ", regex=True).str.strip()
    bad = s.isin(EMPTY) | s.str.match(r"^(other|others|autre|autres)(\s|$)") | s.str.match(r"^\d+$") | (s.str.len() < 2)
    return s.mask(bad, "")


def first_nonempty(df, cols):
    out = pd.Series("", index=df.index, dtype="string")
    for c in cols:
        if c in df:
            v = clean_series(df[c])
            out = out.mask((out == "") & (v != ""), v)
    return out


def parse_year(df, cols, today_year):
    year = pd.Series(pd.NA, index=df.index, dtype="Int64")
    for c in cols:
        if c in df:
            d = pd.to_datetime(df[c].astype("string").str[:10], format="%Y-%m-%d", errors="coerce")
            y = d.dt.year.astype("Int64")
            year = year.fillna(y)
    return year.where((year >= 2020) & (year <= today_year))


def read_form_country_map(path):
    m = {}
    if path and os.path.exists(path):
        with open(path, newline="", encoding="utf-8-sig") as fh:
            for r in csv.DictReader(fh):
                m[str(r["form_id"]).strip()] = r["country_name"].strip().upper()
    return m


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--source", required=True, choices=["IM", "LQAS"])
    ap.add_argument("--raw-dir", required=True)
    ap.add_argument("--out", default=None)
    ap.add_argument("--country-forms", default=None, help="IM: config/country_forms.csv (form_id -> country)")
    args = ap.parse_args()

    here = os.path.dirname(os.path.abspath(__file__))
    out = args.out or os.path.join(os.path.dirname(here), "input", f"AFRO_snapshot_stats_{args.source}.csv")
    cf = args.country_forms
    if cf is None and args.source == "IM":
        guess = os.path.join(os.path.dirname(os.path.abspath(args.raw_dir)), "..", "config", "country_forms.csv")
        cf = os.path.normpath(guess)
    form_country = read_form_country_map(cf)
    today_year = dt.date.today().year

    # acc[country][year] -> dict(sub=int, sett=set, fac=set)
    acc = defaultdict(lambda: defaultdict(lambda: {"sub": 0, "sett": set(), "fac": set()}))
    files = sorted(glob.glob(os.path.join(args.raw_dir, "*.parquet")))
    if not files:
        sys.exit(f"No parquet files in {args.raw_dir}")

    grand = Counter()
    for f in files:
        fid = os.path.basename(f)[:-len(".parquet")]
        pf = pq.ParquetFile(f)
        names = pf.schema_arrow.names
        low = {n.lower(): n for n in names}  # lower -> first exact name
        # every exact column whose lower-case name is a candidate (e.g. Settlement_name + settlement_name)
        def cols_for(cands):
            return [n for n in names if n.lower() in cands and "[" not in n]

        if args.source == "LQAS" and not ({"cluster", "date_of_lqas"} & set(low)):
            print(f"  skip {fid}: not an LQAS form (no Cluster/Date_of_LQAS)")
            continue

        want = set(["_id"])
        want.update(cols_for(set(SETTLEMENT_COLS)))
        want.update(cols_for(set(FACILITY_COLS)))
        want.update(cols_for(set(DISTRICT_COLS)))
        want.update(cols_for(set(DATE_COLS[args.source])))
        want.update(cols_for({"country", "cluster"}))
        want = [c for c in names if c in want]
        df = pf.read(columns=want).to_pandas()
        n_raw = len(df)

        if "_id" in df:
            df = df.drop_duplicates("_id")
        # country: IM -> form table; otherwise modal Country code in the file
        country = form_country.get(fid) if args.source == "IM" else None
        if not country and fid in FORM_FALLBACK:
            country = FORM_FALLBACK[fid]
        if not country and "Country" in df:
            vc = df["Country"].astype("string").str.strip().str.upper()
            vc = vc[vc.notna() & (vc != "")]
            if len(vc):
                code = vc.value_counts().index[0]
                country = CODE_TO_NAME.get(code)
                if country is None:
                    print(f"  !! {fid}: unmapped Country code {code!r} - file skipped")
                    continue
        if not country:
            print(f"  !! {fid}: cannot resolve country - file skipped")
            continue

        year = parse_year(df, [c for c in df.columns if c.lower() in DATE_COLS[args.source]], today_year)
        sett = first_nonempty(df, cols_for(set(SETTLEMENT_COLS)))
        fac = first_nonempty(df, [c for c in df.columns if c.lower() in ("facility", "other_facility", "facility_other", "health_facility")])
        dist = first_nonempty(df, [c for c in df.columns if c.lower() in DISTRICT_COLS])
        # test records
        raw_sett = pd.Series("", index=df.index, dtype="string")
        for c in cols_for(set(SETTLEMENT_COLS)):
            raw_sett = raw_sett.mask(raw_sett == "", df[c].astype("string").fillna("").str.strip().str.lower())
        is_test = raw_sett.isin(["test", "testing", "tst"])
        keep = year.notna() & ~is_test
        df, year, sett, fac, dist = df[keep], year[keep], sett[keep], fac[keep], dist[keep]

        has_cluster = pd.Series(True, index=df.index)
        if args.source == "LQAS":
            has_cluster = df["Cluster"].notna() if "Cluster" in df else pd.Series(False, index=df.index)

        skey = (dist + "|" + sett).where(sett != "")
        fkey = (dist + "|" + fac).where(fac != "")
        tmp = pd.DataFrame({"y": year.astype(int), "s": skey, "f": fkey, "c": has_cluster})
        for y, g in tmp.groupby("y"):
            a = acc[country][int(y)]
            a["sub"] += len(g)
            if args.source == "LQAS":
                a["sett_n"] = a.get("sett_n", 0) + int(g["c"].sum())
            else:
                a["sett"].update(g["s"].dropna().map(hash).tolist())
                a["unnamed"] = a.get("unnamed", 0) + int(g["s"].isna().sum())
            a["fac"].update(g["f"].dropna().map(hash).tolist())
        grand["files"] += 1
        grand["rows_raw"] += n_raw
        grand["rows_used"] += len(tmp)
        print(f"  {fid:>6} {country:<34} raw={n_raw:>8,} used={len(tmp):>8,}  settlements(named)={sett.ne('').mean():.0%} facilities(named)={fac.ne('').mean():.0%}")

    now = dt.datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S UTC")
    rows = []
    for country, years in sorted(acc.items()):
        all_sub = 0
        all_sett_n = 0
        all_unnamed = 0
        all_sett, all_fac = set(), set()
        for y, a in sorted(years.items()):
            if args.source == "LQAS":
                named, unnamed = a.get("sett_n", 0), 0
            else:
                named, unnamed = len(a["sett"]), a.get("unnamed", 0)
            rows.append([args.source, country, y, a["sub"], named + unnamed, len(a["fac"]), named, unnamed, now])
            all_sub += a["sub"]
            all_sett_n += a.get("sett_n", 0)
            all_unnamed += unnamed
            all_sett |= a["sett"]
            all_fac |= a["fac"]
        named_all = all_sett_n if args.source == "LQAS" else len(all_sett)
        rows.append([args.source, country, "ALL", all_sub, named_all + all_unnamed, len(all_fac),
                     named_all, all_unnamed, now])

    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["source", "country", "year", "submissions", "settlements", "facilities",
                    "settlements_named", "settlements_unnamed", "generated_at"])
        w.writerows(rows)
    tot = [r for r in rows if r[2] == "ALL"]
    print(f"\nWrote {out}")
    print(f"  files={grand['files']}  raw rows={grand['rows_raw']:,}  used={grand['rows_used']:,}")
    print(f"  countries={len(tot)}  submissions={sum(r[3] for r in tot):,}  "
          f"settlements={sum(r[4] for r in tot):,} (named {sum(r[6] for r in tot):,} + unnamed {sum(r[7] for r in tot):,})  "
          f"facilities={sum(r[5] for r in tot):,}")


if __name__ == "__main__":
    main()

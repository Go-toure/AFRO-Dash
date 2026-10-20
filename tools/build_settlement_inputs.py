#!/usr/bin/env python3
"""Build the settlement-level input files used by the AFRO-Dash "IM Settlement Maps" tab.

Replaces the old R function get_im_data(): one CSV per IM form id, one row per monitor route
(start/end GPS) with the households where children were missed.

    python tools/build_settlement_inputs.py                 # every known form
    python tools/build_settlement_inputs.py 4445 3550       # only these form ids
    python tools/build_settlement_inputs.py --raw-dir D:/im/raw --out-dir input

Reads  <raw-dir>/<form_id>.parquet   (written by the IM workflow fetch step)
Writes <out-dir>/<form_id>.csv.gz     (read by im_settlement_module.R)

Rules (same as the old script): InProcess monitoring only, households' "Diff_Present_Vacc_FM_HH" summed per
route, only routes with at least one missed child kept. Place names: the settlement column of the form
(or the health facility for forms where that is the place name), falling back to the facility.
"""
import argparse, os, re, sys, time
import numpy as np
import pandas as pd
import pyarrow.parquet as pq

DEFAULT_RAW = r"C:\Users\TOURE\Documents\im_workflow\data\raw"
HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_OUT = os.path.join(os.path.dirname(HERE), "input")

# form id -> country (used when the Country column is blank or an unknown code)
FORM_COUNTRY = {
    3550: "GHANA", 4344: "ZAMBIA", 4385: "BENIN", 4413: "TOGO", 4434: "CENTRAL AFRICAN REPUBLIC", 4445: "CHAD",
    4459: "BURKINA FASO", 4479: "COTE D IVOIRE", 4498: "DEMOCRATIC REPUBLIC OF THE CONGO", 4997: "CONGO",
    5212: "NIGER", 5297: "GUINEA", 5710: "CAMEROON", 5772: "SOUTH SUDAN", 5887: "LIBERIA", 6102: "SIERRA LEONE",
    6348: "SENEGAL", 6418: "KENYA", 6427: "MAURITANIA", 6767: "UGANDA", 6839: "ETHIOPIA", 7585: "GUINEA-BISSAU",
    7600: "MALAWI", 7612: "UNITED REPUBLIC OF TANZANIA", 7621: "MOZAMBIQUE", 7980: "ZIMBABWE", 8279: "MADAGASCAR",
    8587: "ALGERIA", 8832: "BOTSWANA", 9597: "BURUNDI", 9793: "RWANDA", 10267: "ANGOLA", 15780: "NAMIBIA",
}
# 3-letter / short codes used in the Country column
COUNTRY_CODES = {
    "GAM": "GAMBIA", "GHA": "GHANA", "ALG": "ALGERIA", "ETH": "ETHIOPIA", "ANG": "ANGOLA", "BEN": "BENIN",
    "BUI": "BURUNDI", "BURKINA_FASO": "BURKINA FASO", "BFA": "BURKINA FASO", "BWA": "BOTSWANA", "CHD": "CHAD",
    "CAE": "CAMEROON", "CIV": "COTE D IVOIRE", "COG": "CONGO", "GUI": "GUINEA", "KEN": "KENYA", "LIB": "LIBERIA",
    "MAL": "MALI", "MAU": "MAURITANIA", "MDG": "MADAGASCAR", "MLW": "MALAWI", "MOZ": "MOZAMBIQUE", "NIE": "NIGERIA",
    "NIG": "NIGER", "RCA": "CENTRAL AFRICAN REPUBLIC", "RDC": "DEMOCRATIC REPUBLIC OF THE CONGO",
    "DRC": "DEMOCRATIC REPUBLIC OF THE CONGO", "RWA": "RWANDA", "SEN": "SENEGAL", "SIL": "SIERRA LEONE",
    "SSUD": "SOUTH SUDAN", "TNZ": "UNITED REPUBLIC OF TANZANIA", "TOG": "TOGO", "UGA": "UGANDA", "ZIM": "ZIMBABWE",
    "ZMB": "ZAMBIA", "GNB": "GUINEA-BISSAU", "NAM": "NAMIBIA",
}
# forms where the old script used the health facility as the settlement name
FACILITY_IS_SETTLEMENT = {4434, 5710}
SETTLEMENT_COLS = ["Settlement_name", "Settlement_Name", "settlement_name", "SettlementName", "NameSettlement",
                   "Village", "Village_Name", "locality", "Locality", "locality_name"]
NA_TOKENS = {"", "n/a", "N/A", ".", "NA", "nan", "None", "<NA>"}
REASONS = {  # output column -> start of the group column name
    "Reason_Absent": "Tot_child_Absent_HH", "Reason_NC": "Tot_child_NC_HH", "Reason_NotVisited": "Tot_child_NotVisited_HH",
    "Reason_Asleep": "Tot_child_Asleep_HH", "Reason_VaccinatedRoutine": "Tot_child_VaccinatedRoutine",
    "Reason_Others": "Tot_child_Others_HH"}
NOT_RECORDED = "NOT RECORDED"
MAX_PER_HH = 50   # a household cell above this (or negative) is a data-entry error (IDs, phone numbers...) and is ignored


def sane(s: pd.Series) -> pd.Series:
    """numbers kept only when 0 <= value <= MAX_PER_HH; anything else becomes NaN"""
    return s.where((s >= 0) & (s <= MAX_PER_HH))


def clean_text(s: pd.Series, upper=True) -> pd.Series:
    s = s.astype("string").str.strip()
    s = s.mask(s.isin(NA_TOKENS))
    return s.str.upper() if upper else s


def num(s: pd.Series) -> pd.Series:
    return pd.to_numeric(s.astype("string").str.strip().mask(lambda x: x.isin(NA_TOKENS)), errors="coerce")


def process(fid: int, raw_dir: str) -> pd.DataFrame:
    path = os.path.join(raw_dir, f"{fid}.parquet")
    names = pq.ParquetFile(path).schema_arrow.names
    hh = {}
    for k in range(1, 11):
        for n in names:
            if re.fullmatch(rf"HH\[{k}\]/(HH/)?Diff_Present_Vacc_FM_HH", n):
                hh[k] = n
                break
    reason_cols = {r: [n for n in names if re.match(rf"^HH\[\d+\]/group\d+/{pat}", n)] for r, pat in REASONS.items()}
    base = ["Country", "Region", "District", "facility", "Response", "roundNumber", "date_monitored", "Type_Monitoring",
            "_GPS_hh_latitude", "_GPS_hh_longitude", "_GPS_hh_end_latitude", "_GPS_hh_end_longitude"]
    sett = [c for c in SETTLEMENT_COLS if c in names]
    want = [c for c in base + sett + list(hh.values()) + [c for v in reason_cols.values() for c in v] if c in names]
    for need in ("Type_Monitoring", "_GPS_hh_latitude", "_GPS_hh_longitude"):
        if need not in names:
            raise RuntimeError(f"column {need} not found in {fid}.parquet")
    df = pq.read_table(path, columns=want).to_pandas()
    df = df[df["Type_Monitoring"].astype("string").str.strip() == "InProcess"].reset_index(drop=True)
    n_inproc = len(df)

    def col(c):
        return df[c] if c in df.columns else pd.Series(pd.NA, index=df.index, dtype="string")

    # ---- country ----
    raw_c = clean_text(col("Country"))
    mapped = raw_c.map(lambda v: COUNTRY_CODES.get(v, v) if pd.notna(v) else v)
    default = FORM_COUNTRY.get(fid)
    known = set(COUNTRY_CODES.values()) | set(FORM_COUNTRY.values())
    country = mapped.where(mapped.isin(known), default if default else mapped)

    # ---- place names ----
    facility = clean_text(col("facility"))
    settlement = pd.Series(pd.NA, index=df.index, dtype="string")
    order = (["facility"] if fid in FACILITY_IS_SETTLEMENT else []) + sett + ([] if fid in FACILITY_IS_SETTLEMENT else ["facility"])
    for c in order:
        settlement = settlement.fillna(facility if c == "facility" else clean_text(col(c)))
    province = clean_text(col("Region")).fillna(clean_text(col("District")))
    district = clean_text(col("District"))
    out = pd.DataFrame({
        "COUNTRY": country,
        "PROVINCE": province.fillna(NOT_RECORDED),
        "DISTRICT": district.fillna(NOT_RECORDED),
        "Facility": facility.fillna(settlement).fillna(NOT_RECORDED),
        "Settlement": settlement.fillna(NOT_RECORDED),
        "Response": clean_text(col("Response"), upper=False).fillna(""),
        "roundNumber": clean_text(col("roundNumber"), upper=False).fillna(""),
        "date_monitored": col("date_monitored").astype("string").str.slice(0, 10),
        "lat": num(col("_GPS_hh_latitude")), "lon": num(col("_GPS_hh_longitude")),
        "lat_end": num(col("_GPS_hh_end_latitude")), "lon_end": num(col("_GPS_hh_end_longitude")),
    })
    invalid = pd.Series(0, index=df.index)
    for k in range(1, 11):
        raw = num(df[hh[k]]) if k in hh else pd.Series(np.nan, index=df.index)
        ok = sane(raw)
        invalid = invalid + (raw.notna() & ok.isna()).astype(int)
        out[f"Household_{k}"] = ok
    for r, cols in reason_cols.items():
        out[r] = sum((sane(num(df[c])).fillna(0) for c in cols), start=pd.Series(0.0, index=df.index)) if cols else 0.0
    out["Invalid_HH_cells"] = invalid
    keys = ["COUNTRY", "PROVINCE", "DISTRICT", "Facility", "Settlement", "Response", "roundNumber", "date_monitored",
            "lat", "lon", "lat_end", "lon_end"]
    vals = [f"Household_{k}" for k in range(1, 11)] + list(REASONS) + ["Invalid_HH_cells"]
    g = out.groupby(keys, dropna=False, sort=False)[vals].sum(min_count=0).reset_index()
    g["Total_child_missed"] = g[[f"Household_{k}" for k in range(1, 11)]].sum(axis=1)
    n_bad = int((g["Invalid_HH_cells"] > 0).sum())
    g = g[g["Total_child_missed"] >= 1].copy()
    for c in vals + ["Total_child_missed"]:
        g[c] = g[c].astype(float).round(0).astype("Int64") if c != "lat" else g[c]
    print(f"  {fid}: {n_inproc:,} in-process records -> {len(g):,} routes with >= 1 child missed "
          f"({int(g['Total_child_missed'].sum()):,} children), country {sorted(g['COUNTRY'].dropna().unique())[:3]}"
          + (f", {n_bad:,} routes had ignored out-of-range household values" if n_bad else ""))
    return g


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("forms", nargs="*", type=int, help="form ids (default: all known)")
    ap.add_argument("--raw-dir", default=DEFAULT_RAW)
    ap.add_argument("--out-dir", default=DEFAULT_OUT)
    a = ap.parse_args()
    ids = a.forms or sorted(FORM_COUNTRY)
    os.makedirs(a.out_dir, exist_ok=True)
    ok, failed = [], []
    for fid in ids:
        t0 = time.time()
        try:
            g = process(fid, a.raw_dir)
            g.to_csv(os.path.join(a.out_dir, f"{fid}.csv.gz"), index=False, compression="gzip")
            ok.append(fid)
        except Exception as e:  # keep going: one broken form must not stop the others
            print(f"  {fid}: FAILED - {e}")
            failed.append(fid)
        print(f"     {time.time() - t0:.0f}s")
    print(f"done: {len(ok)} written to {a.out_dir}" + (f", failed: {failed}" if failed else ""))
    return 1 if failed and not ok else 0


if __name__ == "__main__":
    sys.exit(main())

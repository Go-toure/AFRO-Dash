#!/usr/bin/env python3
"""Export the test questions for an assistant and grade its answers against the true answers of catalog/test_questions.json.   Author: Gorgui Ba Toure

  python tools/grade_assistant.py --export                       # writes catalog/assistant_test_sheet.csv (id, dataset, question, answer): paste each question into the assistant, paste its reply in 'answer'
  python tools/grade_assistant.py --export --all                 # every question, not only the datasets the assistant can reach
  python tools/grade_assistant.py --grade catalog/assistant_test_sheet.csv   # score, per dataset, with the failures listed
  python tools/grade_assistant.py --selftest                     # checks the grader itself (true answers score 100%, wrong ones 0%)

Rules: a number question passes when the reply contains a figure within the tolerance of the true answer (thousands separators, %, and the words thousand, million, billion are read);
a dictionary answer passes when every value of it is found; a text answer (a date, a name) passes when it is contained in the reply; a trap passes when the reply contains one word
of EACH group of 'must_mention' (the assistant corrected the premise). Questions on datasets the assistant cannot reach are 'declined' when no figure is given, 'answered' otherwise.
The grader reads the reply text only; it needs no access to the data.
"""
import argparse, csv, json, os, re, sys

_d = os.path.dirname(os.path.abspath(__file__))
HERE = _d if os.path.exists(os.path.join(_d, "catalog", "test_questions.json")) else os.path.dirname(_d)  # deployed (AFRO-Dash/assistant_py) or agent repository (tools/)
QF = os.path.join(HERE, "catalog", "test_questions.json")
SHEET = os.path.join(HERE, "catalog", "assistant_test_sheet.csv")
NUM = re.compile(r"(?<![\w.])-?(\d{1,3}(?:,\d{3})+(?:\.\d+)?|\d+(?:\.\d+)?)(?:\s?(thousand|million|billion|bn|m|k)\b)?", re.I)
SCALE = {"thousand": 1e3, "k": 1e3, "million": 1e6, "m": 1e6, "billion": 1e9, "bn": 1e9}


def numbers(text):
    out = []
    for m in NUM.finditer((text or "").replace("\u00a0", ",").replace("\u202f", ",").replace("\u2212", "-")):
        raw = m.group(0)
        scale = (m.group(2) or "").lower()
        num = m.group(1)
        # only a comma (or the no-break space of French text, turned into a comma) is a thousands separator: two figures separated by a space stay two figures
        try:
            v = float(num.replace(",", "").replace(" ", ""))
        except ValueError:
            continue
        neg = raw.lstrip().startswith("-")
        out.append((-v if neg else v) * SCALE.get(scale, 1))
    return out


MONTHS = {m: i + 1 for i, m in enumerate("january february march april may june july august september october november december".split())}
MONTHS.update({m[:3]: i for m, i in list(MONTHS.items())})


def dates(text):
    """Every date written in the reply, as YYYY-MM-DD (ISO, '23 October 2026', 'October 23, 2026', 23/10/2026)."""
    low = (text or "").lower()
    out = set(re.findall(r"\b(\d{4}-\d{2}-\d{2})", low))
    for d, m, y in re.findall(r"\b(\d{1,2})(?:st|nd|rd|th)?\s+(?:of\s+)?([a-z]{3,9})\.?,?\s+(\d{4})\b", low):
        if m in MONTHS:
            out.add("%s-%02d-%02d" % (y, MONTHS[m], int(d)))
    for m, d, y in re.findall(r"\b([a-z]{3,9})\.?\s+(\d{1,2})(?:st|nd|rd|th)?,?\s+(\d{4})\b", low):
        if m in MONTHS:
            out.add("%s-%02d-%02d" % (y, MONTHS[m], int(d)))
    for d, m, y in re.findall(r"\b(\d{1,2})/(\d{1,2})/(\d{4})\b", low):
        out.add("%s-%02d-%02d" % (y, int(m), int(d)))
    return out


def norm(s):
    return re.sub(r"[^a-z0-9]", "", (s or "").lower())


def close(found, expected, tol):
    rel = max(abs(expected) * 0.005, tol or 0) if abs(expected) >= 1000 else (tol or 0)
    if any(abs(f - expected) <= rel + 1e-9 for f in found):
        return True
    # a ratio such as 1.084 may be written as 108.4%
    return 0 < abs(expected) <= 2 and any(abs(f - expected * 100) <= (tol or 0) * 100 + 1e-9 for f in found)


def leaves(x):
    if isinstance(x, dict):
        for v in x.values():
            yield from leaves(v)
    else:
        yield x


def grade_one(q, reply, in_scope):
    low = (reply or "").lower()
    found = numbers(reply or "")
    exp = q.get("expected")
    if not in_scope:
        return "declined" if not found else "answered"
    if q.get("must_mention"):
        ok = all(any(w.lower() in low for w in grp) for grp in q["must_mention"])
        return "pass" if ok else "fail"
    if exp is None:
        return "skip"
    if isinstance(exp, (int, float)):
        return "pass" if close(found, float(exp), q.get("tolerance")) else "fail"
    if isinstance(exp, str):
        if re.match(r"^\d{4}-\d{2}-\d{2}", exp):
            return "pass" if exp[:10] in dates(reply) else "fail"
        wants = [exp] + list(q.get("accept", []))
        return "pass" if any(w.lower() in low or norm(w) in norm(reply) for w in wants) else "fail"
    vals = list(leaves(exp))
    hits = [(close(found, float(v), q.get("tolerance")) if isinstance(v, (int, float)) else str(v).lower() in low) for v in vals]
    return "pass" if all(hits) else "fail"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--export", action="store_true"); ap.add_argument("--all", action="store_true")
    ap.add_argument("--grade", default=""); ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--scope", default="assistant", choices=["assistant", "engine"], help="engine: every dataset of the Python engine counts as reachable (assistant_core), not only the R assistant datasets")
    a = ap.parse_args()
    spec = json.load(open(QF, encoding="utf-8"))
    scope = set(spec.get("engine_datasets" if a.scope == "engine" else "assistant_datasets", []))
    qs = spec["questions"]
    if a.export:
        rows = [q for q in qs if a.all or q["dataset"] in scope]
        with open(SHEET, "w", encoding="utf-8", newline="") as f:
            w = csv.writer(f); w.writerow(["id", "dataset", "question", "answer"])
            for q in rows:
                w.writerow([q["id"], q["dataset"], q["question"], ""])
        print("wrote %s: %d questions (as of %s)" % (SHEET, len(rows), spec.get("as_of")))
        return
    if a.selftest:
        good = {q["id"]: (" ".join(str(x) for x in leaves(q["expected"])) if q.get("expected") is not None else "") + " " + " ".join(w[0] for w in q.get("must_mention", [])) for q in qs}
        bad = {q["id"]: "The value is 7 and nothing else." for q in qs}
        for label, ans in (("true answers", good), ("wrong answers", bad)):
            res = [grade_one(q, ans[q["id"]], True) for q in qs if q["id"] in ans]
            print("%-14s pass %d of %d" % (label, res.count("pass"), len(res)))
        return
    if not a.grade:
        ap.print_help(); return
    ans = {r["id"]: r.get("answer", "") for r in csv.DictReader(open(a.grade, encoding="utf-8"))}
    by, fails, n_ans = {}, [], 0
    for q in qs:
        if q["id"] not in ans:
            continue
        reply = ans[q["id"]]
        if not reply.strip():
            continue
        n_ans += 1
        r = grade_one(q, reply, q["dataset"] in scope)
        by.setdefault(q["dataset"], {}).setdefault(r, 0)
        by[q["dataset"]][r] += 1
        if r in ("fail", "answered"):
            fails.append((q["id"], r, q.get("expected") if q.get("expected") is not None else q.get("trap", "")[:60], [round(x, 2) for x in numbers(reply)[:4]]))
    print("graded %d answers (data as of %s)\n" % (n_ans, spec.get("as_of")))
    tp = tf = 0
    for ds, c in sorted(by.items()):
        tp += c.get("pass", 0); tf += c.get("fail", 0)
        print("%-24s %s" % (ds, ", ".join("%s %d" % kv for kv in sorted(c.items()))))
    if tp + tf:
        print("\nin scope: %d of %d correct (%.0f%%)" % (tp, tp + tf, 100.0 * tp / (tp + tf)))
    for f in fails:
        print("  %-9s %-9s expected %s, figures in the reply %s" % f)


if __name__ == "__main__":
    main()

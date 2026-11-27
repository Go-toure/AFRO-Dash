#!/usr/bin/env python3
"""Run the test questions through the assistant core and write the answers for tools/grade_assistant.py.   Author: Gorgui Ba Toure

  python tools/run_assistant_eval.py --oracle                    # plumbing test, NO model: a scripted 'model' makes the tool call stored in each question and words the figure
  python tools/run_assistant_eval.py                             # a real model (needs: pip install anthropic, ANTHROPIC_API_KEY, SIA_CHAT_MODEL); every question is asked in a fresh conversation
  python tools/run_assistant_eval.py --only lqas,admin --limit 10
Writes catalog/assistant_eval_answers.csv (id, dataset, question, answer, tools) and grades it with grade_assistant.py. The oracle proves the engine, the tool plumbing and the grader work
together; only the real model measures the model's choice of tool and its wording.
"""
import argparse, csv, json, os, subprocess, sys, types

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from assistant_core import Assistant, trail  # noqa: E402
from catalog_engine import Engine, HERE  # noqa: E402


class OracleClient:
    """Scripted stand-in for the model: turn 1 calls the tool stored in the question, turn 2 words the picked figure."""
    def __init__(self, questions, engine):
        self.q = {x["question"]: x for x in questions}
        self.engine = engine
        self.messages = types.SimpleNamespace(create=self._create)

    def _create(self, model=None, max_tokens=None, system=None, tools=None, messages=None):
        first = messages[0]["content"]
        q = self.q[first]
        last = messages[-1]["content"]
        blk = types.SimpleNamespace
        if isinstance(last, str):  # first turn
            tc = q.get("tool_call")
            if not tc:
                return blk(content=[blk(type="text", text=" ".join(w for g in q.get("must_mention", []) for w in g[:1]) or "I cannot answer this from the catalogue.")])
            args = {k: v for k, v in tc.items() if k not in ("pick",)}
            return blk(content=[blk(type="tool_use", id="t1", name="query_data", input=args)])
        res = json.loads(last[0]["content"])
        tc = q["tool_call"]
        got = Engine.pick(res, tc.get("pick"))
        if isinstance(got, dict):
            txt = "; ".join("%s: %s" % (k, ("{:,.2f}".format(v) if isinstance(v, float) else "{:,}".format(v))) for k, v in got.items())
        elif isinstance(got, float):
            txt = "{:,.2f}".format(got)
        elif isinstance(got, int):
            txt = "{:,}".format(got)
        else:
            txt = str(got)
        w = " ".join(res.get("warnings", []))
        said = " ".join(wd for g in q.get("must_mention", []) for wd in g[:1])  # the oracle states the correction a trap asks for
        return blk(content=[blk(type="text", text="%s (%s, %s). %s %s" % (txt, res.get("unit"), res.get("dataset"), w, said))])


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--oracle", action="store_true")
    ap.add_argument("--only", default="")
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--out", default="", help="default: catalog/assistant_eval_answers.csv (real model) or catalog/assistant_eval_oracle.csv (--oracle), so a scripted run never overwrites a real one")
    a = ap.parse_args()
    a.out = a.out or os.path.join(HERE, "catalog", "assistant_eval_oracle.csv" if a.oracle else "assistant_eval_answers.csv")
    qs = json.load(open(os.path.join(HERE, "catalog", "test_questions.json"), encoding="utf-8"))["questions"]
    if a.only:
        qs = [q for q in qs if q["dataset"] in a.only.split(",")]
    if a.limit:
        qs = qs[: a.limit]
    eng = Engine()
    n0 = len(qs)
    qs = [q for q in qs if eng.available(q["dataset"])]  # questions on datasets that are not available in this installation are not asked
    if len(qs) < n0:
        print("%d question(s) skipped: their dataset is not available here (set SIA_DATA_ROOT)" % (n0 - len(qs)))
    bot = Assistant(engine=eng, client=OracleClient(qs, eng) if a.oracle else None)
    rows = []
    for q in qs:
        try:
            text, _, calls = bot.ask(q["question"])
        except Exception as e:
            text, calls = "ERROR: %s" % e, []
        rows.append({"id": q["id"], "dataset": q["dataset"], "question": q["question"], "answer": text.replace("\n", " "), "tools": " | ".join(trail(calls))})
        print("%-9s %s" % (q["id"], rows[-1]["tools"][:100]))
    with open(a.out, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["id", "dataset", "question", "answer", "tools"])
        w.writeheader()
        w.writerows(rows)
    print("answers written:", a.out)
    sys.stdout.flush()
    sys.exit(subprocess.call([sys.executable, os.path.join(os.path.dirname(os.path.abspath(__file__)), "grade_assistant.py"), "--grade", a.out, "--scope", "engine"]))


if __name__ == "__main__":
    main()

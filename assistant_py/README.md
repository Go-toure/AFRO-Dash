# SIA data assistant (Python)

A chat page where a language model answers questions on the SIA data **only through the catalogue engine**: it chooses a dataset, a metric, a grouping and filters; the engine runs a fixed metric and returns the figure with its unit, period, rows used and warnings. The model sees no records and writes no code. Rules: `catalog/ASK_RULES.md`.

This folder is generated from the agent repository (`SIA_data_operations_and_analytics`, `tools/sync_to_dash.py`); do not edit it here, edit there and sync.

## Run (local)

    cd assistant_py
    pip install -r requirements.txt
    set ANTHROPIC_API_KEY=...        (or put ANTHROPIC_API_KEY=... in .env next to app.py; .env is git-ignored)
    set SIA_CHAT_MODEL=...           (the model id your Anthropic account lists)
    shiny run app.py

## Data

LQAS and admin are read from `data/` (copies of `input/AFRO_LQAS_data_c.rds` and `AFRO_admin_data.rds`, refreshed by `sync_to_dash.py`). The other datasets (Form A, preparedness, vaccine accountability, social mobilization, team supervision, IM repository and the raw forms) are available only when `SIA_DATA_ROOT` points to the folder that holds the data repositories; otherwise they are left out of the lists and the model is told they are not available.

## Deploy (Posit Connect)

A second app next to the R dashboard: deploy the folder `assistant_py` as a Python Shiny app (`rsconnect write-manifest shiny --entrypoint app:app assistant_py`, then deploy from the repository path `assistant_py`). Set `ANTHROPIC_API_KEY` and `SIA_CHAT_MODEL` as environment variables of the content, never in a file. The page has no sign-in: restrict access in Connect if it must stay internal.

## Check

    python catalog_engine.py check       # every dataset available here is readable, every metric runs
    python catalog_engine.py selftest    # the test questions of the available datasets, no model
    python run_assistant_eval.py         # a real model (needs the key), graded by grade_assistant.py
    python run_assistant_eval.py --oracle   # scripted stand-in for the model (plumbing only)

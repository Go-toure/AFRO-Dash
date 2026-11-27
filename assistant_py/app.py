"""Internal chat page on the SIA data (Shiny for Python, native tool calling through the catalogue engine).   Author: Gorgui Ba Toure

Run on the owner's computer:   pip install -r requirements.txt   then   set ANTHROPIC_API_KEY=...   (and SIA_CHAT_MODEL=...)   then   shiny run app.py   (agent repository: shiny run chat_app/app.py)
The model answers only through the engine (tools/catalog_engine.py): aggregated figures with unit, period and warnings; it never sees a record and writes no code.
INTERNAL USE: the page has no sign-in; do not publish it publicly. The public AFRO-Dash assistant is a separate decision (workflows/18, section 3d).
"""
import asyncio, os, sys

from shiny import App, ui

APP = os.path.dirname(os.path.abspath(__file__))
for _d in (APP, os.path.join(os.path.dirname(APP), "tools")):  # next to the modules when deployed (AFRO-Dash/assistant_py), tools/ in the agent repository
    if os.path.exists(os.path.join(_d, "assistant_core.py")):
        sys.path.insert(0, _d)
        break
from assistant_core import Assistant, trail  # noqa: E402

app_ui = ui.page_fillable(
    ui.h4("SIA data assistant (internal)"),
    ui.p("Ask about LQAS, administrative coverage, Form A, preparedness, vaccine accountability, social mobilization, team supervision, independent monitoring or the raw forms. "
         "Every figure comes from the catalogue engine and is shown with its unit, period and warnings.", class_="text-muted"),
    ui.chat_ui("chat"),
    title="SIA data assistant",
)


def server(input, output, session):
    chat = ui.Chat(id="chat", messages=["Hello. Which figure do you need? For example: *What share of LQAS lots passed in 2026, by block?*"])
    state = {"bot": None, "history": []}

    def bot():
        if state["bot"] is None:
            state["bot"] = Assistant()
        return state["bot"]

    @chat.on_user_submit
    async def _():
        question = chat.user_input()
        try:
            reply, hist, calls = await asyncio.to_thread(bot().ask, question, state["history"])
        except SystemExit as e:
            await chat.append_message(str(e))
            return
        except Exception as e:
            await chat.append_message("The assistant could not answer: %s" % str(e)[:200])
            return
        state["history"] = hist[-20:]  # keep the conversation short
        t = trail(calls)
        await chat.append_message(reply + ("\n\n<details><summary>Data used</summary>\n\n" + "\n".join("- `%s`" % x for x in t) + "\n\n</details>" if t else ""))


app = App(app_ui, server)

"""Internal chat page on the SIA data (Shiny for Python, native tool calling through the catalogue engine).   Author: Gorgui Ba Toure

Run on the owner's computer:   pip install -r requirements.txt   then   set ANTHROPIC_API_KEY=...   (and SIA_CHAT_MODEL=...)   then   shiny run app.py   (agent repository: shiny run chat_app/app.py)
The model answers only through the engine (tools/catalog_engine.py): aggregated figures with unit, period and warnings; it never sees a record and writes no code.
INTERNAL USE: the page has no sign-in; do not publish it publicly. The public AFRO-Dash assistant is a separate decision (workflows/18, section 3d).
"""
import asyncio, os, sys, tempfile, uuid

from shiny import App, ui

APP = os.path.dirname(os.path.abspath(__file__))
for _d in (APP, os.path.join(os.path.dirname(APP), "tools")):  # next to the modules when deployed (AFRO-Dash/assistant_py), tools/ in the agent repository
    if os.path.exists(os.path.join(_d, "assistant_core.py")):
        sys.path.insert(0, _d)
        break
from assistant_core import Assistant, trail  # noqa: E402

CHARTS = tempfile.mkdtemp(prefix="sia_charts_")  # chart images are served as files: the chat component strips data: images

app_ui = ui.page_fillable(
    ui.h4("SIA data assistant (internal)"),
    ui.p("Ask about LQAS, administrative coverage, Form A, preparedness, vaccine accountability, social mobilization, team supervision, independent monitoring or the raw forms. "
         "Every figure comes from the catalogue engine and is shown with its unit, period and warnings.", class_="text-muted"),
    ui.chat_ui("chat"),
    title="SIA data assistant",
)


def trim(hist, keep=20):
    """Keep the last messages, starting at a question typed by the user: a tool result must never lose the tool call before it (the API refuses it)."""
    cut = max(0, len(hist) - keep)
    while cut < len(hist) and not (hist[cut].get("role") == "user" and isinstance(hist[cut].get("content"), str)):
        cut += 1
    return hist[cut:]


def server(input, output, session):
    hello = "Hello. Which figure do you need? For example: *What share of LQAS lots passed in 2026, by block?*"
    try:  # shinychat 0.7 and later: a greeting; the conversation is kept by this page, not by the chat component
        chat = ui.Chat(id="chat", greeting=hello, history=False)
    except TypeError:  # older shiny: startup messages
        chat = ui.Chat(id="chat", messages=[hello])
    state = {"bot": None, "history": []}

    def bot():
        if state["bot"] is None:
            state["bot"] = Assistant()
        return state["bot"]

    @chat.on_user_submit
    async def _():
        ui_in = chat.user_input()
        question = getattr(ui_in, "text", ui_in)  # newer shinychat returns (text, attachments), older ones the text
        try:
            reply, hist, calls = await asyncio.to_thread(bot().ask, question, state["history"])
        except SystemExit as e:
            await chat.append_message(str(e))
            return
        except Exception as e:
            await chat.append_message("The assistant could not answer: %s" % str(e)[:200])
            return
        state["history"] = trim(hist)  # keep the conversation short
        t = trail(calls)
        imgs = ""
        for b in getattr(bot(), "images", []):
            name = "c_%s.png" % uuid.uuid4().hex[:10]
            with open(os.path.join(CHARTS, name), "wb") as f:
                f.write(b)
            imgs += "\n\n![chart](charts/%s)" % name
        await chat.append_message(reply + imgs + ("\n\n<details><summary>Data used</summary>\n\n" + "\n".join("- `%s`" % x for x in t) + "\n\n</details>" if t else ""))


app = App(app_ui, server, static_assets={"/charts": CHARTS})

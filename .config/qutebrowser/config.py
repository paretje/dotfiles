#!/usr/bin/env python3
import logging
import sys

from PyQt6.QtCore import QUrl
from qutebrowser.api import interceptor

logger = logging.getLogger()

qute_pass = "spawn --userscript qute-pass --username-target secret --username-pattern 'login: (.+)' --dmenu-invocation 'wofi -dmenu' --no-insert-mode"


# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig()

# keep backend setting after restart from GUI
if "--backend" in sys.argv:
    c.backend = sys.argv[sys.argv.index("--backend") + 1]
else:
    c.backend = "webengine"

# reduce memory usage on low memory machines
try:
    import psutil
    if c.backend == "webengine" and psutil.virtual_memory().total < 3 * 1024 ** 3:
        c.qt.process_model = "process-per-site"
except ImportError:
    pass

c.url.start_pages = ["about:blank"]
c.url.default_page = "about:blank"
c.url.auto_search = "naive"
c.editor.command = ["x-terminal-emulator", "-e", "nvim", "-b", "-c", "set noeol", "{}"]
c.content.xss_auditing = True
c.content.default_encoding = "utf-8"
c.new_instance_open_target = "tab-silent"
c.auto_save.session = True
c.content.autoplay = False
c.content.headers.accept_language = "en-GB,en,nl-BE,nl"
c.content.tls.certificate_errors = "ask-block-thirdparty"
c.content.netrc_file = "~/.local/share/qutebrowser/netrc"
c.completion.cmd_history_max_items = 10000
c.completion.web_history.max_items = 10000
c.input.partial_timeout = 1000
c.tabs.background = True
c.tabs.last_close = "blank"
c.tabs.title.format = "{index}: {current_title} {audio}"
c.confirm_quit = ["multiple-tabs"]
c.scrolling.bar = "never"
c.downloads.location.directory = "~/downloads"
c.content.cache.size = 52428800
c.content.javascript.enabled = False
c.content.webgl = False
c.content.geolocation = False
c.content.cookies.accept = "no-3rdparty"
c.content.cookies.store = True
c.content.blocking.whitelist = []
c.hints.uppercase = True
c.hints.next_regexes = [
    r"\bnext\b",
    r"\bmore\b",
    r"\bnewer\b",
    r"\b[>→≫]\b",
    r"\b(>>|»)\b",
    r"\bcontinue\b",
    r"\bvolgende\b",
]
c.hints.prev_regexes = [
    r"\bprev(ious)?\b",
    r"\bback\b",
    r"\bolder\b",
    r"\b[<←≪]\b",
    r"\b(<<|«)\b",
    r"\bvorige\b",
]
c.fonts.web.size.default = 15
c.fonts.completion.entry = "8pt monospace"
c.fonts.completion.category = "bold 8pt monospace"
c.fonts.debug_console = "8pt monospace"
c.fonts.downloads = "8pt monospace"
c.fonts.keyhint = "8pt monospace"  # TODO: larger?
c.fonts.messages.error = "8pt monospace"
c.fonts.messages.info = "8pt monospace"
c.fonts.messages.warning = "8pt monospace"
c.fonts.prompts = "8pt sans-serif"
c.fonts.statusbar = "8pt monospace"
c.fonts.tabs.selected = "8pt monospace"
c.fonts.tabs.unselected = "8pt monospace"
c.qt.highdpi = True
c.spellcheck.languages = ["en-GB", "nl-NL"]

if c.backend == "webkit":
    c.content.cache.appcache = False
    c.hints.find_implementation = "javascript"
else:
    c.qt.args = [
        "ignore-gpu-blacklist",
        "enable-gpu-rasterization",
        "enable-zero-copy",
        "enable-strict-mixed-content-checking",
        "enable-features=WebRTCPipeWireCapturer",
        "enable-webrtc-pipewire-capturer",
    ]

c.url.searchengines["ddg"] = "https://duckduckgo.com/?kp=-2&k1=-1&kaj=m&q={}"
c.url.searchengines[
    "man"
] = "http://manpages.debian.org/cgi-bin/man.cgi?query={}&manpath=Debian+unstable+si"
c.url.searchengines["ghu"] = "https://github.com/{semiquoted}"
c.url.searchengines["taal"] = "https://taal.vrt.be/search/apachesolr_search/{}"
c.url.searchengines["dpkg"] = "https://packages.debian.org/search?keywords={}"
c.url.searchengines["ubuntu"] = "https://packages.ubuntu.com/search?keywords={}"
c.url.searchengines[
    "sp"
] = "https://www.startpage.com/do/search?prfh=lang_homepageEEEs/dawn/en/N1Nenable_post_methodEEE0N1Nconnect_to_serverEEEeuN1Ndisable_family_filterEEE1N1Ndisable_video_family_filterEEE1&query={}"
c.url.searchengines[
    "pydoc"
] = "https://docs.python.org/3/search.html?q={}&check_keywords=yes&area=default"
c.url.searchengines["wnl"] = "https://nl.wikipedia.org/w/index.php?search={}"
c.url.searchengines["wen"] = "https://en.wikipedia.org/w/index.php?search={}"
c.url.searchengines["woordenlijst"] = "http://woordenlijst.org/#/?q={}"
c.url.searchengines["gh"] = "https://github.com/search?q={}"
c.url.searchengines["tvdb"] = "https://www.thetvdb.com/search?query={}"
c.url.searchengines[
    "osub"
] = "https://www.opensubtitles.org/en/search2/sublanguageid-dut/moviename-{}"
c.url.searchengines["osm"] = "https://www.openstreetmap.org/search?query={}"
c.url.searchengines["arch"] = "https://wiki.archlinux.org/index.php?search={}"
c.url.searchengines["dbts"] = "https://bugs.debian.org/cgi-bin/bugreport.cgi?bug={}"
c.url.searchengines["pypi"] = "https://pypi.org/search/?q={}"
c.url.searchengines["dochub"] = "https://hub.docker.com/search?q={}&type=image"
c.url.searchengines["eco"] = "https://www.ecosia.org/search?q={}"
c.url.searchengines[
    "sqla"
] = "https://docs.sqlalchemy.org/en/latest/search.html?q={}&check_keywords=yes&area=default"
c.url.searchengines["tmdb"] = "https://www.themoviedb.org/search?query={}"
c.url.searchengines["yt"] = "https://yewtu.be/search?q={}"
c.url.searchengines["tts"] = "https://trythatsoap.com/search/?q={}"
c.url.searchengines['psn'] = "https://en.psprices.com/region-be/search/?q={}&platform=PS3"
c.url.searchengines['ebay'] = "https://www.benl.ebay.be/sch/i.html?_from=R40&_nkw={}&_sacat=0&_sop=15"
c.url.searchengines["DEFAULT"] = c.url.searchengines["sp"]

c.aliases["h"] = "help"
c.aliases["q"] = "close ;; session-delete default"
c.aliases["qa"] = "quit ;; session-delete default"

config.bind("t", "set-cmd-text -s :open -t")
config.bind("x", "tab-close")
config.bind("gt", "tab-next")
config.bind("gT", "tab-prev")
config.bind("f", "hint --add-history all current")
config.bind("F", "hint --add-history all tab-bg")
config.bind(";f", "hint --add-history all current")
config.bind(";F", "hint --add-history all tab-fg")
config.bind("af", "hint --add-history --rapid links tab-bg")
config.bind("yf", "hint links yank")
config.bind("X", "undo")
config.bind("p", "open -- {clipboard}")
config.bind("P", "open -t -- {clipboard}")
config.bind("[", "navigate prev")
config.bind("]", "navigate next")
config.bind("{", "navigate decrement")
config.bind("}", "navigate increment")
config.bind("d", "scroll-page 0 0.5")
config.bind("u", "scroll-page 0 -0.5")
config.bind("j", "scroll-px 0 40")
config.bind("k", "scroll-px 0 -40")
config.bind("gh", "home")
config.bind("s", "stop")
config.unbind(";i")
config.bind(";iw", "spawn --detach iceweasel {url}")
config.bind(";js", "config-cycle -p -u *://{url:host}/* content.javascript.enabled")
config.bind(";p", "spawn --userscript password_fill")
config.bind(";v", "spawn --detach mpv --ytdl-raw-options-append=netrc= {url}")
config.bind(";sv", "spawn --detach x-terminal-emulator -e pipe-viewer {url}")
config.bind(";sk", "spawn --userscript play_kodi")
config.bind(";a", "spawn --userscript play_mpc")
config.bind("gs", "view-source --edit")
config.bind("gf", "spawn --userscript openfeeds")
config.unbind(";o")
config.bind(";org", "spawn --verbose --userscript org_add")
config.bind("D", 'bookmark-del ;; message-info "Bookmark {url} deleted!"')
config.bind("zl", f"hint inputs -f ;; later 100 mode-enter normal ;; {qute_pass}")
config.bind("zul", f"hint inputs -f ;; later 100 mode-enter normal ;; {qute_pass} --username-only")
config.bind("zpl", f"hint inputs -f ;; later 100 mode-enter normal ;; {qute_pass} --password-only")
config.bind("zol", f"hint inputs -f ;; later 100 mode-enter normal ;; {qute_pass} --otp-only")
config.bind("zL", qute_pass)
config.bind("zuL", f"{qute_pass} --username-only")
config.bind("zpL", f"{qute_pass} --password-only")
config.bind("zoL", f"{qute_pass} --otp-only")
config.bind("<Ctrl-N>", "set-cmd-text -s :tab-select")
config.bind(",,", "spawn --userscript usermode")
config.unbind("<Ctrl-W>")
config.bind("<Ctrl-Shift-O>", "edit-text", mode="insert")
config.bind("<Ctrl-Shift-A>", "fake-key <Ctrl-A>", mode="insert")
config.bind("<Ctrl-A>", "fake-key <Home>", mode="insert")
config.bind("<Ctrl-E>", "fake-key <End>", mode="insert")
config.bind("<Ctrl-W>", "fake-key <Ctrl-Backspace>", mode="insert")
config.bind("<Ctrl-D>", "fake-key <Del>", mode="insert")
config.bind("<Ctrl-O>", "edit-text ;; mode-enter normal", mode="insert")
config.bind("<Up>", "command-history-prev", mode="command")
config.bind("<Down>", "command-history-next", mode="command")
config.bind("<Ctrl-Shift-D>", "completion-item-del", mode="command")
config.bind("<Ctrl-D>", "fake-key <Del>", mode="command")
config.bind("<Ctrl-W>", "rl-backward-kill-word", mode="command")
config.bind("<Alt-Backspace>", "rl-unix-word-rubout", mode="command")
config.bind(
    "<Ctrl-P>", "spawn --userscript password_fill_prompt", mode="prompt"
)


redirects = {
    "www.reddit.com": "old.reddit.com",
    "twitter.com": "nitter.cc",
    "www.youtube.com": "yewtu.be",
    "invidio.us": "yewtu.be",
    "invidious.13ad.de": "yewtu.be",
}


def unregister_existing_interceptor(func: interceptor.InterceptorType):
    interceptors = interceptor.interceptors._interceptors
    existing_func = None
    for reg_func in interceptors:
        if (
            reg_func.__name__ == func.__name__
            and reg_func.__module__ == func.__module__
        ):
            existing_func = reg_func
            break

    if existing_func is not None:
        interceptors.remove(existing_func)
    return func


@interceptor.register
@unregister_existing_interceptor
def intercept(info: interceptor.Request) -> None:
    if not info.request_url.scheme().startswith("http"):
        return

    # TODO: don't do host redirects for third party requests
    # third_party_request = info.first_party_url != info.request_url
    host = info.request_url.host()

    if host in redirects:
        new_url = QUrl(info.request_url)
        new_url.setHost(redirects[host])
        try:
            info.redirect(new_url)
        except interceptor.interceptors.RedirectException as ex:
            logger.error("Redirect failed (%s)", ex)

#!/usr/bin/env python3
import sys
import psutil

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

config.load_autoconfig()

# keep backend setting after restart from GUI
if '--backend' in sys.argv:
    c.backend = sys.argv[sys.argv.index('--backend')+1]
else:
    c.backend = "webengine"

# reduce memory usage on low memory machines
if c.backend == 'webengine' and psutil.virtual_memory().total < 3 * 1024 ** 3:
    c.qt.process_model = 'process-per-site'

c.url.start_pages = ["https://duckduckgo.com/html/?kl=be-nl&kp=-1"]
c.url.default_page = "about:blank"
c.url.auto_search = 'dns'
c.editor.command = ['urxvt', '-e', 'nvim', '-b', '-c', 'set noeol', '{}']
c.content.xss_auditing = True
c.content.default_encoding = "utf-8"
c.new_instance_open_target = 'tab-silent'
c.auto_save.session = True
c.content.autoplay = False
c.content.headers.accept_language = "en-GB,en,nl-BE,nl"
c.content.ssl_strict = True
c.content.netrc_file = "~/.local/share/qutebrowser/netrc"
c.completion.cmd_history_max_items = 10000
c.completion.web_history.max_items = 10000
c.input.partial_timeout = 1000
c.tabs.background = True
c.tabs.last_close = 'blank'
c.downloads.location.directory = "~/downloads"
c.content.cache.appcache = False
c.content.cache.size = 52428800
c.content.javascript.enabled = False
c.content.webgl = False
c.content.geolocation = False
c.content.cookies.store = False
c.content.host_blocking.lists = ["https://www.malwaredomainlist.com/hostslist/hosts.txt", "http://someonewhocares.org/hosts/hosts", "http://winhelp2002.mvps.org/hosts.zip", "http://malwaredomains.lehigh.edu/files/justdomains.zip", "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&mimetype=plaintext", "file:///var/cache/qutebrowser/ad_servers.txt"]
c.content.host_blocking.whitelist = []
c.hints.uppercase = True
c.hints.next_regexes = [r'\bnext\b', r'\bmore\b', r'\bnewer\b', r'\b[>→≫]\b', r'\b(>>|»)\b', r'\bcontinue\b', r'\bvolgende\b']
c.hints.prev_regexes = [r'\bprev(ious)?\b', r'\bback\b', r'\bolder\b', r'\b[<←≪]\b', r'\b(<<|«)\b', r'\bvorige\b']
c.hints.find_implementation = 'javascript'
c.fonts.web.size.default = 15
c.fonts.completion.entry = '8pt monospace'
c.fonts.completion.category = 'bold 8pt monospace'
c.fonts.debug_console = '8pt monospace'
c.fonts.downloads = '8pt monospace'
c.fonts.keyhint = '8pt monospace'  # TODO: larger?
c.fonts.messages.error = '8pt monospace'
c.fonts.messages.info = '8pt monospace'
c.fonts.messages.warning = '8pt monospace'
c.fonts.prompts = '8pt sans-serif'
c.fonts.statusbar = '8pt monospace'
c.fonts.tabs = '8pt monospace'
c.spellcheck.languages = ['en-GB']

c.url.searchengines['ddg'] = "https://duckduckgo.com/html/?kl=be-nl&kp=-1&q={}"
c.url.searchengines['man'] = "http://manpages.debian.org/cgi-bin/man.cgi?query={}&manpath=Debian+unstable+si"
c.url.searchengines['gh'] = "https://github.com/{}"
c.url.searchengines['taal'] = "https://taal.vrt.be/search/apachesolr_search/{}"
c.url.searchengines['dpkg'] = "https://packages.debian.org/search?keywords={}"
c.url.searchengines['DEFAULT'] = c.url.searchengines['ddg']

c.aliases['h'] = 'help'
c.aliases['q'] = 'close ;; session-delete default'
c.aliases['qa'] = 'quit ;; session-delete default'

config.bind('t', 'set-cmd-text -s :open -t')
config.bind('x', 'tab-close')
config.bind('gt', 'tab-next')
config.bind('gT', 'tab-prev')
config.bind('f', 'hint all current')
config.bind(';f', 'hint all current')
config.bind(';F', 'hint all tab-fg')
config.bind('yf', 'hint links yank')
config.bind('af', 'hint --rapid links tab-bg')
config.bind('X', 'undo')
config.bind('p', 'open -- {clipboard}')
config.bind('P', 'open -t -- {clipboard}')
config.bind('[', 'navigate prev')
config.bind(']', 'navigate next')
config.bind('{', 'navigate prev -t')
config.bind('}', 'navigate next -t')
config.bind('d', 'scroll-page 0 0.5')
config.bind('u', 'scroll-page 0 -0.5')
config.bind('gh', 'home')
config.bind('s', 'stop')
config.unbind(';i')
config.bind(';iw', 'spawn --detach iceweasel {url}')
config.bind(';js', 'config-cycle -p -u *://{url:host}/* content.javascript.enabled')
config.bind(';p', 'spawn --userscript password_fill')
config.bind(';v', 'spawn mpv {url}')
config.bind(';a', 'spawn --userscript play_mpc')
config.bind('gs', 'view-source --edit')
config.bind('gf', 'spawn --userscript openfeeds')
config.unbind(';o')
config.bind(';org', 'spawn --verbose --userscript org_add')
config.bind('D', 'bookmark-del ;; message-info "Bookmark {url} deleted!"')
config.bind(';sp', 'spawn --userscript password_store')
config.bind('g$', 'tab-focus -1')
config.bind('gi', 'hint inputs ;; fake-key -g a')
config.bind(';ip', 'spawn --userscript password_fill_insert')
config.bind('<Ctrl-N>', 'set-cmd-text -s :buffer')
config.bind(',,', 'spawn --userscript usermode')
config.unbind('<Ctrl-W>')
config.bind('<Ctrl-Shift-O>', 'open-editor', mode='insert')
config.bind('<Ctrl-Shift-A>', 'fake-key <Ctrl-A>', mode='insert')
config.bind('<Ctrl-A>', 'fake-key <Home>', mode='insert')
config.bind('<Ctrl-E>', 'fake-key <End>', mode='insert')
config.bind('<Ctrl-W>', 'fake-key <Ctrl-Backspace>', mode='insert')
config.bind('<Ctrl-D>', 'fake-key <Del>', mode='insert')
config.bind('<Ctrl-O>', 'open-editor ;; leave-mode', mode='insert')
config.bind('<Up>', 'command-history-prev', mode='command')
config.bind('<Down>', 'command-history-next', mode='command')
config.bind('<Shift-Tab>', 'completion-item-focus prev', mode='command')
config.bind('<Tab>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-Shift-D>', 'completion-item-del', mode='command')
config.bind('<Ctrl-D>', 'fake-key <Del>', mode='command')
config.bind('<Ctrl-W>', 'rl-backward-kill-word', mode='command')
config.bind('<Alt-Backspace>', 'rl-unix-word-rubout', mode='command')

config.bind('<Ctrl-P>', 'spawn --userscript password_fill_prompt_wrapper', mode='prompt')

from IPython import get_ipython
from prompt_toolkit.keys import Keys
from prompt_toolkit.key_binding.bindings.named_commands import get_by_name
from prompt_toolkit.filters import ViInsertMode, EmacsInsertMode

insert_mode = ViInsertMode() | EmacsInsertMode()

ip = get_ipython()
registry = ip.pt_cli.application.key_bindings_registry
registry.add_binding(Keys.ControlW, filter=insert_mode)(get_by_name('backward-kill-word'))

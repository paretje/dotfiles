# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

from ranger.api.commands import Command


class bulkrename(Command):
    """:bulkrename

    This command opens a list of selected files in an external editor.
    After you edit and save the file, it will generate a shell script
    which does bulk renaming according to the changes you did in the file.

    This shell script is opened in an editor for you to review.
    After you close it, it will be executed.
    """

    def execute(self):
        filenames = [f.relative_path for f in self.fm.thistab.get_selection()]
        # TODO: https://github.com/nimaipatel/brn
        # TODO: https://github.com/thameera/vimv
        self.fm.run(["vimv"] + filenames)

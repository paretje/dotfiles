# Copyright 2017 by Kevin Velghe <kevin@paretje.be>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import weechat

SCRIPT_NAME = 'hichan'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = "GPL3"
SCRIPT_AUTHOR = 'Kevin Velghe <kevin@paretje.be>'


def handle_msg(data, pbuffer, date, tags, displayed, highlight, prefix, message):
    if highlight:
        return weechat.WEECHAT_RC_OK

    buffer_type = weechat.buffer_get_string(pbuffer, "localvar_type")
    if buffer_type != 'channel':
        return weechat.WEECHAT_RC_OK

    my_nickname = "nick_" + weechat.buffer_get_string(pbuffer, "localvar_nick")
    if my_nickname in tags:
        return weechat.WEECHAT_RC_OK

    name = weechat.buffer_get_string(pbuffer, "name")
    hichans = weechat.config_get_plugin("channels").split(',')
    if name in hichans:
        short_name = weechat.buffer_get_string(pbuffer, "short_name")
        weechat.hook_signal_send('weechat_highlight', weechat.WEECHAT_HOOK_SIGNAL_STRING, '{}\t{}'.format(short_name, message))

    return weechat.WEECHAT_RC_OK

if __name__ == "__main__":
    weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
        "{} - A libnotify script for weechat".format(SCRIPT_NAME), "", "")

    print_hook = weechat.hook_print('', 'notify_message', '', 1, 'handle_msg', '')

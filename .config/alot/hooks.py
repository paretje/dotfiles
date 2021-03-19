import os
import re
import subprocess
import asyncio
import shlex
import magic
from alot.buffers import EnvelopeBuffer, SearchBuffer
from alot.commands import CommandCanceled
from alot.helper import mailto_to_envelope
from alot.settings.const import settings
from alot.settings.errors import NoMatchingAccount


def pre_buffer_focus(ui, dbm, buf):
    if isinstance(buf, SearchBuffer):
        buf.rebuild()


# TODO: async
# mark current message read at github
def github_mark_read(ui, msg=None):
    if msg is None:
        msg = ui.current_buffer.get_selected_message()
    msg = msg.get_email()

    if msg.is_multipart():
        msgtext = b""
        for msgpart in msg.get_payload():
            msgtext += msgpart.get_payload(decode=True)
    else:
        msgtext = msg.get_payload(decode=True)

    r = b'src="(https://github.com/notifications/beacon/.*.gif)"'
    beacons = re.findall(r, msgtext)

    if beacons:
        subprocess.Popen(['curl', '-s', beacons[0]], stdout=open(os.devnull, 'w'))
        ui.notify('removed from github notifications:\n %s' % beacons[0])
    else:
        ui.notify('no beacon found')


# automatically mark github notifications as read
async def post_search_select(ui, cmd, dbm):
    current_msg = ui.current_buffer.get_selected_message()
    if current_msg.get_author()[1] == 'notifications@github.com':
        last_msg = list(ui.current_buffer.messagetrees())[-1]._message
        if 'unread' in last_msg.get_tags():
            github_mark_read(ui, list(ui.current_buffer.messagetrees())[-1]._message)


# command to manually fetch mails
def getmails(ui):
    ui.notify("fetchinig email..")
    subprocess.call(['pkill', '-u', os.environ['USER'], '-USR1', 'maildaemon'])


def exit():
    subprocess.call(['notmuch-backup'])


def _sorted_func(func, key):
    def wrapper(*args, **kwargs):
        result = func(*args, **kwargs)
        return sorted(result, key=key)
    return wrapper


async def post_thread_save(ui, dbm, cmd):
    # we are only concerned when we saved a single focused attachment
    if cmd.all or not cmd.path:
        return

    if magic.from_file(cmd.path).endswith(' text, with CRLF line terminators'):
        if (await ui.choice("convert Windows text file?", select='yes', cancel='no')) == 'no':
            return
        process = await asyncio.create_subprocess_exec('dos2unix', cmd.path)
        await process.wait()
    if magic.from_file(cmd.path).startswith('ISO-8859 text'):
        if (await ui.choice("convert ISO-8859 text file?", select='yes', cancel='no')) == 'no':
            return
        process = await asyncio.create_subprocess_shell('iconv -f latin1 -t utf8 {0} | sponge {0}'.format(shlex.quote(cmd.path)))
        await process.wait()


def unsubscribe_list(ui):
    """
    Unsubscribe from a mailing list.

    This hook reads the 'List-Unsubscribe' header of a mail in thread mode,
    constructs a unsubsribe-mail according to any mailto-url it finds
    and opens the new mail in an envelope buffer.
    """
    msg = ui.current_buffer.get_selected_message()
    e = msg.get_email()
    uheader = e['List-Unsubscribe']
    dtheader = e.get('Delivered-To', None)

    if uheader is not None:
        mailto_match = re.search(r'<(mailto:\S*)>', uheader)
        if mailto_match is not None:
            envelope = mailto_to_envelope(mailto_match.group(1))
            if dtheader is not None:
                envelope['From'] = dtheader
                try:
                    envelope.account = settings.account_matching_address(dtheader)
                except NoMatchingAccount:
                    msg = 'Cannot compose mail - ' \
                          'no account found for `%s`' % dtheader
                    ui.notify(msg, priority='error')
                    raise CommandCanceled()
            ui.buffer_open(EnvelopeBuffer(ui, envelope))
    else:
        ui.notify('focussed mail contains no \'List-Unsubscribe\' header',
                  'error')

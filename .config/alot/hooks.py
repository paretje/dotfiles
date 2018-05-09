import alot
import os
import re
import urllib2
import subprocess


def _get_threadlines(threadlist):
    threadlines = threadlist.get_lines()
    for tlw in threadlines:
        yield tlw

    tlw = threadlist._get_next_item()
    while tlw:
        yield tlw
        tlw = threadlist._get_next_item()


def _save_focus(buf):
    buf.focused_thread = buf.get_selected_thread()
    if buf.focused_thread:
        buf.focused_thread = buf.focused_thread.get_thread_id()


def _restore_focus(buf):
    if not hasattr(buf, "focused_thread") or not buf.focused_thread:
        return

    for pos, tlw in enumerate(_get_threadlines(buf.threadlist)):
        if tlw.get_thread().get_thread_id() == buf.focused_thread:
            buf.body.set_focus(pos)
            break


def pre_buffer_focus(ui, dbm, buf):
    current = ui.current_buffer
    if isinstance(current, alot.buffers.SearchBuffer):
        _save_focus(current)

    if isinstance(buf, alot.buffers.SearchBuffer):
        buf.rebuild()


def post_buffer_focus(ui, dbm, buf, success):
    if success:
        _restore_focus(buf)


def pre_global_refresh(ui, dbm, cmd):
    _save_focus(ui.current_buffer)


def post_global_refresh(ui, dbm, cmd):
    _restore_focus(ui.current_buffer)


# mark current message read at github
def github_mark_read(ui, msg=None):
    if msg is None:
        msg = ui.current_buffer.get_selected_message()
    msg = msg.get_email()

    if msg.is_multipart():
        msgtext = ""
        for msgpart in msg.get_payload():
            msgtext += msgpart.get_payload(decode=True)
    else:
        msgtext = msg.get_payload(decode=True)

    r = r'src="(https://github.com/notifications/beacon/.*.gif)"'
    beacons = re.findall(r, msgtext)

    if beacons:
        subprocess.Popen(['curl', '-s', beacons[0]], stdout=open(os.devnull, 'w'))
        ui.notify('removed from github notifications:\n %s' % beacons[0])
    else:
        ui.notify('no beacon found')


# automatically mark github notifications as read
def post_search_select(ui, cmd, dbm):
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


from alot.commands import Command, registerCommand


@registerCommand('search', 'tagging',
                 arguments=[
                     (['tags'], {'help': 'space separated list of tags'})
                     ],
                 help='Reload all configuration files')
class TaggingCommand(Command):

    """Reload configuration."""

    def __init__(self, tags=u''):
        self.tagstring = tags

    def apply(self, ui):
        tagginglist = [t for t in self.tagstring.split(' ') if t]
        tags = []
        untags = []

        for tag in tagginglist:
            if tag[0] == '-':
                untags.append(tag[1:])
            else:
                tags.append(tag[1:] if tag[0] == '+' else tag)

        if tags:
            ui.apply_command(alot.commands.search.TagCommand(tags=' '.join(tags), action='add', flush=True))
        if untags:
            ui.apply_command(alot.commands.search.TagCommand(tags=' '.join(untags), action='remove', flush=True))

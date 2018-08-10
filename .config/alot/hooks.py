import alot
import os
import re
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


async def pre_global_refresh(ui, dbm, cmd):
    _save_focus(ui.current_buffer)


async def post_global_refresh(ui, dbm, cmd):
    _restore_focus(ui.current_buffer)


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


import notmuch
from alot.db.thread import Thread
from alot.db.message import Message


def _get_messages(self):
    """
    returns all messages in this thread as dict mapping all contained
    messages to their direct responses.
    :rtype: dict mapping :class:`~alot.db.message.Message` to a list of
            :class:`~alot.db.message.Message`.
    """
    if not self._messages:  # if not already cached
        query = self._dbman.query('thread:' + self._id)
        thread = next(query.search_threads())

        def accumulate(acc, msg):
            M = Message(self._dbman, msg, thread=self)
            acc[M] = []
            r = msg.get_replies()
            if r is not None:
                for m in r:
                    acc[M].append(accumulate(acc, m))
            return M

        self._messages = {}
        for m in sorted(thread.get_toplevel_messages(), key=notmuch.message.Message.get_date):
            self._toplevel_messages.append(accumulate(self._messages, m))
    return self._messages


Thread.get_messages = _get_messages

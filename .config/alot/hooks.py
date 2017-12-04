import alot
import re
import urllib2


def pre_buffer_focus(ui, dbm, buf):
    if buf.modename == 'search':
        buf.rebuild()


def pre_buffer_open(ui, dbm, buf):
    current = ui.current_buffer
    if isinstance(current, alot.buffers.SearchBuffer):
        current.focused_thread = current.get_selected_thread()  # save focus


def post_buffer_focus(ui, dbm, buf, success):
    if success and hasattr(buf, "focused_thread"):  # if buffer has saved focus
        tid = buf.focused_thread.get_thread_id()
        for pos, tlw in enumerate(buf.threadlist.get_lines()):
            if tlw.get_thread().get_thread_id() == tid:
                buf.body.set_focus(pos)
                break


def github_mark_read(ui):
    msg = ui.current_buffer.get_selected_message()
    msgtext = str(msg.get_email())
    r = r"img src='(https://github.com/notifications/beacon/.*.gif)'"
    beacons = re.findall(r, msgtext)
    if beacons:
        urllib2.urlopen(beacons[0])
        ui.notify('removed from github notifications:\n %s' % beacons[0])

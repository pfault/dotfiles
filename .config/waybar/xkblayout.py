#!/usr/bin/env -S python3 -u
# vi:syntax=python
import os
import signal

import i3ipc

sway_sock_path = os.environ["SWAYSOCK"]
sway = i3ipc.Connection(socket_path=sway_sock_path, auto_reconnect=False)

replacements = {
            "English": "us",
            "German": "de",
            "Colemak": "CMK",
        }


def short_layout(name):
    for k,v in replacements.items():
        name = name.replace(k, v)
    return name


def on_posix_signal(sig, frame):
    sway.main_quit()


def on_input_change(conn, event):
    if event.change != "xkb_layout":
        return
    print(short_layout(event.input.xkb_active_layout_name))


def on_shutdown_or_reload(conn, event):
    conn.main_quit()


if __name__=="__main__":
    signal.signal(signal.SIGHUP, on_posix_signal)
    signal.signal(signal.SIGINT, on_posix_signal)
    signal.signal(signal.SIGQUIT, on_posix_signal)
    signal.signal(signal.SIGTERM, on_posix_signal)

    inputs = sway.get_inputs()
    for input in inputs:
        print(short_layout(input.xkb_active_layout_name))
        if input.xkb_active_layout_name is not None:
            break

    sway.on(i3ipc.Event.INPUT, on_input_change)
    sway.on(i3ipc.Event.SHUTDOWN, on_shutdown_or_reload)
    sway.on(i3ipc.Event.SHUTDOWN_EXIT, on_shutdown_or_reload)
    sway.on(i3ipc.Event.SHUTDOWN_RESTART, on_shutdown_or_reload)
    sway.on(i3ipc.Event.BARCONFIG_UPDATE, on_shutdown_or_reload)
    sway.main()

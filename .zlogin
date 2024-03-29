#!/usr/bin/env zsh

[[ "$TTY" == /dev/tty* ]] || return 0

export $(systemctl --user show-environment)

export LIBSEAT_BACKEND=logind
export WLR_NO_HARDWARE_CURSORS=1
export GPG_TTY="$TTY"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
systemctl --user import-environment GPG_TTY SSH_AUTH_SOCK PATH

if [[ -z $DISPLAY && "$TTY" == "/dev/tty1" ]]; then
    ~/bin/setup-audio
    systemd-cat -t sway sway --unsupported-gpu
    systemctl --user stop graphical-session.target
    systemctl --user unset-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK
    exit
fi

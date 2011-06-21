#!/bin/sh

PID=`pgrep offlineimap`
PID1=`pgrep imapfilter`

[[ -n "$PID" ]] && exit 1
[[ -n "$PID1" ]] && exit 1

(imapfilter &>/dev/null && offlineimap -o -u Noninteractive.Quiet &>/dev/null && cat /home/pfault/.mutt/mailboxes | grep -oE '\"\+martin\.schrodi-f4n\.de\/[0-9A-Za-z\-]*\"' | xargs -i~~ echo "mailboxes \"~~\"" > /home/pfault/.mutt/martin.schrodi-f4n.de.mailboxes && cat .mutt/mailboxes | grep -oE '\"\+pfault-f4n\.de\/[0-9A-Za-z\-]*\"' | xargs -i~~ echo "mailboxes \"~~\"" > /home/pfault/.mutt/pfault-f4n.de.mailboxes)&
exit

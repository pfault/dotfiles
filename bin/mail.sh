basedir="`dirname $0`"
notmuch="/usr/bin/notmuch"
afew="/usr/bin/afew"

if [[ -f /usr/local/bin/notmuch ]]; then
  notmuch="/usr/local/bin/notmuch"
fi

sendmailcommand="$basedir/msmtp-runqueue.sh"

# Check connection status before sending mail
if ! ping -c1 www.google.com > /dev/null 2>&1; then
    # Ping could be firewalled ...
    # '-O -' will redirect the actual html to stdout and thus to /dev/null
    if ! wget -O - www.google.com > /dev/null 2>&1; then
        # Both tests failed. We are probably offline
        # (or google is offline, i.e. the end has come)
        exit 1;
    fi
fi

# We are online: So let's get mail going first
${sendmailcommand} 2>&1 >> ~/.msmtp.log

$afew -ta

$notmuch new --quiet

$notmuch tag +sent -- "folder:accountname/Sent and not tag:sent"
$notmuch tag -inbox +archive -- "folder:accountname/Archiv and not tag:archive"

$notmuch tag -unread +trash -- 'folder:accountname/Trash'

DISPLAY=:0.0 /usr/bin/notifymuch

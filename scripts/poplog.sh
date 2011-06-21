#!/bin/bash

# Usage: ./poplog.sh /var/log/yourlog
# pops a colored log with the last line of the log

export DISPLAY=":0.0"
export SYNTAXHIGHLIGHTFILE="/home/pfault/.config/awesome/awesome.outlang" # CHANGE ME

#Urgency
infoUrgency='low'
warningUrgency='normal'
errorUrgency='critical'
securityUrgency='critical'
 
#Popup time
infoPopupTime=5000
warningPopupTime=8000
errorPopupTime=11000
securityPopupTime=11000
 
#Icons
infoIcon='/usr/share/icons/gnome/32x32/status/dialog-information.png'
warningIcon='/usr/share/icons/gnome/32x32/status/dialog-warning.png'
errorIcon='/usr/share/icons/gnome/32x32/status/dialog-error.png'
securityIcon='/usr/share/icons/gnome/32x32/status/security-medium.png'

coloredLog=$(tail -n 1 $1 |                   \
  source-highlight --failsafe                 \
                   --src-lang=log             \
                   --style-file=default.style \
                   --outlang-def=${SYNTAXHIGHLIGHTFILE})




if [ -n "$coloredLog" ]; then
    #echo $coloredLog
    
    if $(echo $1|grep info); then messageType='info'; fi
    if $(echo $1|grep warn); then messageType='warning'; fi
    if $(echo $1|grep err); then messageType='error'; fi
    if $(echo $1|grep auth); then messageType='security'; fi
    if $(echo $1|grep access); then messageType='security';fi
    if $(echo $notification|grep 'UFW BLOCK INPUT'); then
        messageType='security'; fi
    if [ -z "$messageType" ]; then messageType='info'; fi
       
    case $messageType in
        info)
          urgency=$infoUrgency
          icon=$infoIcon
          popupTime=$infoPopupTime
          ;;
        warning)
          urgency=$warningUrgency
          icon=$warningIcon
          popupTime=$warningPopupTime
          ;;
        error)
          urgency=$errorUrgency
          icon=$errorIcon            
          popupTime=$errorPopupTime
          ;;
        security)
          urgency=$securityUrgency
          icon=$securityIcon        
          popupTime=$securityPopupTime
          ;;
        *)
          urgency=$errorUrgency
          icon=$errorIcon            
          popupTime=$errorPopupTime
          ;;
    esac

    DBUS_SESSION_BUS_ADDRESS=""
    pids=`pgrep -u pfault`
    for pid in $pids; do
            # find DBUS session bus for this session
            #   DBUS_SESSION_BUS_ADDRESS=`grep -z DBUS_SESSION_BUS_ADDRESS \
                    grep -z DBUS_SESSION_BUS_ADDRESS \
                        /proc/$pid/environ | sed -e 's/DBUS_SESSION_BUS_ADDRESS=//'
                echo
            done|sort -u|while read DBUS_SESSION_BUS_ADDRESS
        do  # use it
                export DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS
#                echo $DBUS_SESSION_BUS_ADDRESS > /tmp/dbusxxx
                notify-send -u $urgency -t $popupTime -i "$icon" "$1" "$coloredLog" 2> /tmp/notify-send-error
        done
fi

#!/bin/bash
shottype=("full" "area")
shotdest=("clipboard" "file" "upload")
action=""
if [[ " ${shottype[@]} " =~ " $1 " ]] && [[ " ${shotdest[@]} " =~ " $2 " ]]; then
  action="$1 $2"
else
  action="$(echo -e "full to clipboard (1)\narea to clipboard (2)\nfull to file (3)\narea to file (4)" | wofi -d -p "Screenshot" -k /dev/null)"
fi
case $action in
    full*clipboard*)
        grim - | wl-copy --type image/png
        ;;
    area*clipboard*)
        grim -g "$(slurp)" - | wl-copy --type image/png
        ;;
    full*file*)
        grim ~/Pictures/screenshots/$(date +'%s_grim.png')
        ;;
    area*file*)
        grim -g "$(slurp)" ~/Pictures/screenshots/$(date +'%s_grim.png')
        ;;
    *)
        exit 1
        ;;
esac

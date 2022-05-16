if [[ -z $DISPLAY && "$(tty)" == "/dev/tty1" ]]; then
	export LIBSEAT_BACKEND=logind
	export WLR_NO_HARDWARE_CURSORS=1
	#export WLR_DRM_NO_MODIFIERS=1
	/home/pfault/bin/setup-audio.sh
	exec sway --unsupported-gpu
fi

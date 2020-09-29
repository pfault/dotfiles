swaylock --screenshots --clock --indicator-idle-visible \
	-K -f \
	--grace 20 \
	--indicator-radius 100 \
	--indicator-thickness 7 \
	--ring-color 0000ff \
	--ring-clear-color cccccc \
	--ring-ver-color ffbf00 \
	--ring-wrong-color ff0000 \
	--key-hl-color 00ff00 \
	--text-color 00ff00 \
	--line-color 00000000 \
	--inside-color 00000088 \
	--separator-color 00000000 \
	--fade-in 0.1 \
	--effect-scale 0.5 --effect-blur 7x3 --effect-scale 2 \
	--effect-vignette 0.5:0.5 \
	--effect-compose "2,0;100%x1080;$HOME/.config/sway/lock.png" \
	"$@"

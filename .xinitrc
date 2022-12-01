#!/bin/sh

# merge in defaults and keymaps
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
userresources="$HOME/.Xresources"
usermodmap="$HOME/.xmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"
[ -f "$sysmodmap" ] && xmodmap "$sysmodmap"

# Set default cursor.
xsetroot -cursor_name left_ptr

# Root window time setting
while [ 1 -eq 1 ]
do
	xsetroot -name "$(gettime)"
	sleep 1m
done &

# Some custom configurations :3
xset r rate 300 50 &

xset s off &
xset -dpms &

dunst > /dev/null &
picom > /dev/null &
feh --no-fehbg --bg-fill "$HOME"/.local/share/backgrounds/wall &

exec dwm > /dev/null 2> /dev/null

#!/bin/sh

sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
userresources="$HOME/.Xresources"
usermodmap="$HOME/.Xmodmap"

# merge in defaults and keymaps

[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"
[ -f "$sysmodmap" ] && xmodmap "$sysmodmap"

# Set default cursor.
xsetroot -cursor_name left_ptr

while [ 1 -eq 1 ]
do
	xsetroot -name "$(gettime)"
	sleep 1m
done &

# Some custom configurations :3
setxkbmap -option caps:escape
remaps &
xset s off &
xset -dpms &
xscreensaver -no-splash > /dev/null &
dunst > /dev/null&
picom > /dev/null&
feh --bg-max "$HOME"/.local/share/backgrounds/wall

exec dwm > /dev/null 2> /dev/null

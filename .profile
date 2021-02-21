#!/bin/sh
# Profile file. Runs on login


export CC='gcc'
export EDITOR='mg'
export TERMINAL='st'
export BROWSER='firefox'


# shellcheck source=/dev/null
[ "$SHELL" = "/bin/bash" ] && . "$HOME/.bashrc"
# shellcheck source=/dev/null
[ "$SHELL" = "/bin/ksh" ] && . "$HOME/.kshrc"
# shellcheck source=/dev/null
[ "$SHELL" = "/bin/zsh" ] && . "$HOME/.zshrc"


[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.cargo/bin" ] && export PATH="$PATH:$HOME/.cargo/bin"
[ -d "$HOME/.cache" ]     && export XDG_CACHE_HOME="$HOME/.cache"
[ -d "$HOME/.config" ]    && export XDG_CONFIG_HOME="$HOME/.config"


export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export LESSHISTFILE="-"
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
export TERMINFO="$XDG_DATA_HOME"/terminfo
export WINEPREFIX="$HOME"/games/dummy
export WEECHAT_HOME="$XDG_CONFIG_HOME"/weechat

date

# COLORS
if [ "$TERM" = "linux" ]; then
    _SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
        echo -en "$i"
    done
    clear
fi

echo "+-----------------+"
echo "| Welcome back <3 |"
echo "+-----------------+"
echo

if [ "$(tty)" = "/dev/tty1" ] ; then
	pgrep -x Xorg || exec startx 2> /dev/null
fi

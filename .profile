#!/bin/sh
# Profile file. Runs on login


export CC='gcc'
export EDITOR='mg'
export TERMINAL='st'
export BROWSER='firefox'
export MPC_FORMAT='[%artist%[ "%album%"][ ##%track%] - ]%title%'
export DOTNET_CLI_TELEMETRY_OPTOUT=1

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

[ -d "$HOME/.local/share/info" ] &&
    export INFOPATH="$HOME/.local/share/info:$INFOPATH"



export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export LESSHISTFILE="-"
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
export TERMINFO="$XDG_DATA_HOME"/terminfo
export WINEPREFIX="$HOME"/games/dummy
export WEECHAT_HOME="$XDG_CONFIG_HOME"/weechat

emacs --fg-daemon &

date

if [ "$(tty)" = "/dev/tty2" ] ; then
    pgrep -x Xorg || exec startx 2> /dev/null
fi

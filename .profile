#!/bin/sh
# Profile file. Runs on login

# Source /etc/profile
[ -f /etc/profile ] && . /etc/profile

export CC='gcc'
export EDITOR='mg'
export TERMINAL='st'
export BROWSER='chromium'
export MPC_FORMAT='[%artist%[ "%album%"][ ##%track%] - ]%title%'
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# shellcheck source=/dev/null
[ $(basename "$SHELL") = "bash" ] && . "$HOME/.bashrc"
# shellcheck source=/dev/null
[ $(basename "$SHELL") = "ksh"  ] && . "$HOME/.kshrc"
# shellcheck source=/dev/null
[ $(basename "$SHELL") = "zsh"  ] && . "$HOME/.zshrc"

[ -d "$HOME/.local/bin" ]   && export PATH="$PATH:$HOME/.local/bin"
[ -d "$HOME/.cargo/bin" ]   && export PATH="$PATH:$HOME/.cargo/bin"
[ -d "$HOME/.roswell/bin" ] && export PATH="$PATH:$HOME/.roswell/bin"
[ -d "$HOME/.cache" ]       && export XDG_CACHE_HOME="$HOME/.cache"
[ -d "$HOME/.config" ]      && export XDG_CONFIG_HOME="$HOME/.config"

[ -d "$HOME/.local/share/info" ] &&
    export INFOPATH="$HOME/.local/share/info:$INFOPATH"

which go >/dev/null 2>/dev/null &&
    mkdir -p "$HOME/devel/go" &&
    export GOPATH="$HOME/devel/go" &&
    export PATH="$PATH:$GOPATH/bin"


export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export LESSHISTFILE="-"
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
export TERMINFO="$XDG_DATA_HOME"/terminfo
export WINEPREFIX="$HOME"/games/dummy
export WEECHAT_HOME="$XDG_CONFIG_HOME"/weechat

# wayland stuff
# export MOZ_ENABLE_WAYLAND=1
# export QT_QPA_PLATFORM=wayland
# export XDG_SESSION_TYPE=wayland

# river (wayland) keyboard stuff
export XKB_DEFAULT_LAYOUT="ro,ru"
export XKB_DEFAULT_OPTIONS="caps:escape,grp:alt_caps_toggle"

date

if [ "$(tty)" = "/dev/tty1" ] ; then
    exec startx
fi

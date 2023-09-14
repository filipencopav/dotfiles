#!/bin/sh
# Profile file. Runs on login


export CC='gcc'
export EDITOR='mg'
export TERMINAL='st'
export BROWSER='chromium'
export MPC_FORMAT='[%artist%[ "%album%"][ ##%track%] - ]%title%'
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# shellcheck source=/dev/null
[ "$SHELL" = "/bin/bash" ] && . "$HOME/.bashrc"
# shellcheck source=/dev/null
[ "$SHELL" = "/bin/ksh" ] && . "$HOME/.kshrc"
# shellcheck source=/dev/null
[ "$SHELL" = "/bin/zsh" ] && . "$HOME/.zshrc"

[ -d "$HOME/.local/bin" ]   && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.cargo/bin" ]   && export PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/.roswell/bin" ] && export PATH="$HOME/.roswell/bin:$PATH"
[ -d "$HOME/.cache" ]       && export XDG_CACHE_HOME="$HOME/.cache"
[ -d "$HOME/.config" ]      && export XDG_CONFIG_HOME="$HOME/.config"

[ -d "$HOME/.local/share/info" ] &&
    export INFOPATH="$HOME/.local/share/info:$INFOPATH"



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

#!/bin/sh
# Profile file. Runs on login

# Source /etc/profile
[ -f /etc/profile ] && . /etc/profile

# shellcheck source=/dev/null
[ $(basename "$SHELL") = "bash" ] && . "$HOME/.bashrc"
# shellcheck source=/dev/null
[ $(basename "$SHELL") = "ksh"  ] && . "$HOME/.kshrc"
# shellcheck source=/dev/null
[ $(basename "$SHELL") = "zsh"  ] && . "$HOME/.zshrc"

export $(run-parts /usr/lib/systemd/user-environment-generators | xargs -
0)

export USING_WAYLAND=yes
if [ "$USING_WAYLAND" == "yes" ]; then
    export MOZ_ENABLE_WAYLAND=1
    export QT_QPA_PLATFORM=wayland
    export XDG_SESSION_TYPE=wayland
    # https://wiki.archlinux.org/title/wayland#Requirements
    # > To force GBM as a backend, set the following environment variables:
    GBM_BACKEND=nvidia-drm
    __GLX_VENDOR_LIBRARY_NAME=nvidia
fi

# wayland keyboard stuff
export USING_GNOME="yes"
if [ "$USING_GNOME" == "no" ]; then
    export XKB_DEFAULT_LAYOUT="ro,ru"
    export XKB_DEFAULT_OPTIONS="caps:escape,grp:alt_caps_toggle"
fi

export START_XORG=no

date

if [ "$(tty)" = "/dev/tty1" ] && [ "$START_XORG" == "yes" ]; then
    exec startx
fi

#!/bin/bash
alias e="$EDITOR"

alias dotconf="git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# youtube downloader alias
alias youtube-dl="/usr/bin/yt-dlp -o '%(title)s.%(ext)s'"
alias audio-dl='yt-dlp -ic -x -f bestaudio/best'

# QOL basic system navigation commands
alias rm='rm -Iv'
alias mkdir='mkdir -pv'
alias grep='grep --color=auto'
alias ls='ls -hN --color=auto --group-directories-first'
alias ll='ls --color=auto -l -a'
alias 'update-grub'='grub-mkconfig -o /boot/grub/grub.cfg'

alias nvidia-settings="nvidia-settings --config=${XDG_CONFIG_HOME}/nvidia/settings"
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# devour aliases
alias mupdf='devour mupdf'
alias zathura='devour zathura'
alias feh='devour feh'
alias sxiv='devour sxiv'

# source sh-powerline script
. "$HOME/.bash-powerline.sh"

export ASDF_CONFIG_FILE="${XDG_CONFIG_HOME}/asdf/asdfrc"
export ASDF_DATA_DIR="${XDG_DATA_HOME}/asdf"
# https://asdf-vm.com/guide/getting-started.html
. /opt/asdf-vm/asdf.sh
export KERL_CONFIGURE_OPTIONS="--without-javac --with-odbc=/var/lib/pacman/local/unixodbc-$(pacman -Q unixodbc | cut -d' ' -f2)"

export HISTCONTROL=erasedups
export HISTIGNORE="clear:ls:history"
export HISTFILE="${XDG_STATE_HOME}/bash/history"
export MPC_FORMAT='[%artist%[ "%album%"][ ##%track%] - ]%title%'

complete -cf doas

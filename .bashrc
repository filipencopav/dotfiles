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

# devour aliases
alias mupdf='devour mupdf'
alias zathura='devour zathura'
alias feh='devour feh'
alias sxiv='devour sxiv'

# source sh-powerline script
source "$HOME/.bash-powerline.sh"

export HISTCONTROL=erasedups
export HISTIGNORE="clear:ls:history"

complete -cf doas

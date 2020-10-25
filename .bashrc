#!/bin/bash
set -o vi
bind "\C-l":"clear-screen"

# idk
alias e="$EDITOR"

# memes
alias 'pls'='sudo '
alias ':q'='exit'
alias ':wq'='exit'
alias mute='mpc pause'

# edit configuration files
alias bashrc="$EDITOR $HOME/.bashrc"
alias profile="$EDITOR $HOME/.profile"

alias kakrc="$EDITOR $HOME/.config/kak/kakrc"
alias visrc="$EDITOR $HOME/.config/vis/visrc.lua"
alias vimrc="$EDITOR $HOME/.vimrc"

alias config="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# youtube downloader alias
alias flac-dl='youtube-dl -ic -x --audio-format flac -o'
alias mp3-dl='youtube-dl -ic -x --audio-format mp3 -o'

# QOL basic system navigation commands
alias rm='rm -Iv'
alias mkdir='mkdir -pv'
alias grep='grep --color=auto'
alias ls='ls -hN --color=auto --group-directories-first'
alias ll='ls --color=auto -l -a'
alias 'update-grub'='grub-mkconfig -o /boot/grub/grub.cfg'

# coding
PROJECT_PATH="$HOME"/devel/rust/physac
alias proj="cd $PROJECT_PATH"
alias thinkdast="cd $HOME"/devel/java/ThinkDataStructures

# source sh-powerline script
source "$HOME/.bash-powerline.sh"

export HISTCONTROL=erasedups
export HISTIGNORE="clear:ls:history"

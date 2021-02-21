#!/bin/bash
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

alias kakrc="kak $HOME/.config/kak/kakrc"
alias visrc="vis $HOME/.config/vis/visrc.lua"
alias vimrc="vim $HOME/.vim/vimrc"
alias nvimrc="nvim $HOME/.config/nvim/init.vim"

alias config="/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME"

# youtube downloader alias
alias flac-dl='youtube-dl -ic -x --audio-format flac -o'
alias mp3-dl='youtube-dl -ic -x --audio-format mp3 -o'
alias audio-dl='youtube-dl -ic -x -f bestaudio/best'

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

# programming
PROJECT_PATH="$HOME"/devel/aerosmith
alias proj="cd $PROJECT_PATH"
alias thinkdast="cd $HOME"/devel/thinkdast

# source sh-powerline script
source "$HOME/.bash-powerline.sh"

export HISTCONTROL=erasedups
export HISTIGNORE="clear:ls:history"

complete -cf doas

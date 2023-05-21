#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias config='/usr/bin/git --git-dir=$HOME/.cfg --work-tree=$HOME'
PS1='[\u@\h \W]\$ '

export PATH=${PATH}:${HOME}/.config/emacs/bin
export TERMINAL=/usr/bin/alacritty
export EDITOR="/usr/bin/emacsclient -c"

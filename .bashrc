#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias config='/usr/bin/git --git-dir=$HOME/.cfg --work-tree=$HOME'
alias sgpt='OPENAI_API_KEY=$(pass shell-gpt-api-key) sgpt'

PS1='[\u@\h \W]\$ '

export PATH=${PATH}:${HOME}/.config/emacs/bin:${HOME}/.local/bin
export TERMINAL=/usr/bin/alacritty
export EDITOR="/usr/bin/emacsclient -c"
export OPENAI_API_KEY=$(pass shell-gpt-api-key)
export XDG_DATA_DIRS=${XDG_DATA_DIRS}:/var/lib/flatpak/exports/share:/home/rafael/.local/share/flatpak/exports/share

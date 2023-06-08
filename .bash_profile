#
# ~/.bash_profile
#

echo "entra" > /tmp/a.out

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi



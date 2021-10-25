#!/bin/zsh
# gnome-power-manager 
# gnome-volume-manager &
# gnome-settings-daemon &

[ -f $HOME/.profile ] && . $HOME/.profile

xmodmap $HOME/.Xmodmap
xsetroot -cursor_name left_ptr

# keynav &
if [ `hostname` = 'hypatia' ]; then
  source $HOME/.screenlayout/big-big-small.sh
fi
if [ `hostname` = 'diophantus' ]; then
  setxkbmap -option "compose:ralt"
fi
source $HOME/.fehbg

eval `ssh-agent`
urxvt -e ssh-add &

redshift -l 35.1:-92.4 &
# $HOME/src/TagTime/tagtimed.pl &

export TZ="America/Chicago"
$HOME/.local/bin/xmonad 

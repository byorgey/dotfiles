#!/bin/zsh
# gnome-power-manager 
# gnome-volume-manager &
# gnome-settings-daemon &

[ -f $HOME/.profile ] && . $HOME/.profile

xmodmap $HOME/.Xmodmap
xsetroot -cursor_name left_ptr

export PATH=$HOME/local/bin:/usr/local/bin:$PATH
export BROWSER=firefox

keynav &
if [ `hostname` = 'hypatia' ]; then
  source $HOME/.screenlayout/big-big-small.sh
fi
source $HOME/.fehbg

eval `ssh-agent`
urxvt -e ssh-add &


redshift -l 35.1:-92.4 &
# $HOME/src/TagTime/tagtimed.pl &
run_keybase

$HOME/.local/bin/xmonad 

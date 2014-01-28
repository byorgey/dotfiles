#!/bin/zsh
# gnome-power-manager 
# gnome-volume-manager &
# gnome-settings-daemon &

[ -f $HOME/.profile ] && . $HOME/.profile

xmodmap -e 'clear Lock'
xmodmap -e 'keysym Menu = Multi_key'
xmodmap -e 'remove Lock = Caps_Lock'
xmodmap -e 'keysym Caps_Lock = Escape'

xsetroot -cursor_name left_ptr

export PATH=$HOME/local/bin:/usr/local/bin:$PATH
export BROWSER=firefox

keynav &

if [ `hostname` = 'LVN513-9' ]; then
  xpmroot ~/images/lavender-field-2880x1024.png
  export PATH=/scratch/local/bin:$PATH
  /usr/local/condor/sbin/condor_kbdd -l $HOME/.kbdd.log -pidfile $HOME/.kbdd.pid
  xscreensaver &
else
  eval `cat $HOME/.fehbg`
fi

eval `ssh-agent`
xterm -e ssh-add &

xflux -z 19139
# $HOME/src/TagTime/tagtimed.pl &

$HOME/local/bin/xmonad 

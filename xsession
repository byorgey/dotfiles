# gnome-power-manager 
# gnome-volume-manager &
# gnome-settings-daemon &

eval `cat $HOME/.fehbg`  # set the background

xmodmap -e 'clear Lock'
xmodmap -e 'keysym Menu = Multi_key'
xmodmap -e 'remove Lock = Caps_Lock'
xmodmap -e 'keysym Caps_Lock = Escape'

xsetroot -cursor_name left_ptr

export PATH=$HOME/local/bin:/usr/local/bin:$PATH
# export OOO_FORCE_DESKTOP=gnome
# export AWT_TOOLKIT=MToolkit     # fix Java
export BROWSER=firefox

# gnome-panel &
$HOME/local/bin/keynav &

# ulimit -c unlimited

# xscreensaver &
# arbtt-capture &

eval `ssh-agent`
xterm -e ssh-add &

$HOME/src/TagTime/tagtimed.pl &

$HOME/local/bin/xmonad 

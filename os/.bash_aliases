# BACKLIGHT
alias lo='echo 812 | sudo tee /sys/class/backlight/intel_backlight/brightness'
alias mid='echo 2446 | sudo tee /sys/class/backlight/intel_backlight/brightness'
alias hi='echo 4296 | sudo tee /sys/class/backlight/intel_backlight/brightness'

# FILE MANAGEMENT
alias lsda='ls -lah --group-directories-first'
alias lsd='ls -lh --group-directories-first'

# SSH/VNC
alias share0='x11vnc -display :0'

# MONITOR
alias monitor43='xrandr --output VGA1 --mode 1280x1024 --output LVDS1 --off && bigfeh'
alias duo43='xrandr --output VGA1 --left-of LVDS1 && bigfeh'
alias monitor1610='xrandr --output HDMI1 --mode 1680x1050 --output LVDS1 --off'
alias laptopscreen='xrandr --auto'
alias bigscreen='xrandr --auto --output HDMI1 --left-of LVDS1 && bigfeh'

# Complex xrandr command
function duo {
    xrandr --output HDMI1 --right-of LVDS1
    xrandr --output LVDS1 --off
    xrandr --output HDMI1 --mode 1280x1024
    xrandr --output HDMI1 --right-of VGA1
    xrandr --output HDMI1 --rotate left
    xrandr --output VGA1 --mode 1280x1024
}

# APPLICATIONS
alias wcalc='wcalc -EE'
alias tmux='tmux -2'

# REDSHIFT
alias 3000='redshift -O 3000'
alias 3500='redshift -O 3500'
alias 4000='redshift -O 4000'
alias 4500='redshift -O 4500'
alias 5000='redshift -O 5000'

# NO CAPS
alias nocaps='setxkbmap -option ctrl:nocaps'


# i3
alias moveright='i3-msg move workspace to output right'
alias moveleft='i3-msg move workspace to output left'
alias nw='i3-msg move workspace '
alias bigfeh='feh --bg-scale ~/Pictures/Nebulacity.jpg'

# system
alias suspend='systemctl suspend'
alias off='sudo shutdown now'
alias reboot='sudo reboot'

alias p3='python3.5'

alias mnt-remote='sshfs s8z8@remote.ugrad.cs.ubc.ca:/home/s/s8z8/415/cs415a3_s8z8 /home/sergey/mnt/remote415/'

alias vim="/usr/local/bin/vim"

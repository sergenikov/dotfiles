#!/bin/bash

function laptop() {
  xrandr --auto
  #xrandr --output VGA1 --off
  #xrandr --output HDMI1 --off
}
echo "running laptop script"
laptop

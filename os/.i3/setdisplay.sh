#!/bin/bash

function external_off() {
  xrandr --output VGA1 --off --output HDMI1 --off --output LVDS1 --auto
}

echo "Running setdisplay script"
external_off

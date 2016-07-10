#!/bin/bash

if [ "$1" = "us" ]
then
    echo "switching to us"
    setxkbmap us
    xmodmap ~/.xmodmap
else
    echo "switching to ru"
    setxkbmap ru
    xmodmap ~/.xmodmap
fi

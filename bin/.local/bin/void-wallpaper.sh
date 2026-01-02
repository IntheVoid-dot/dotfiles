#!/usr/bin/env bash

swww-daemon &
sleep 1

swww img "$HOME/Pictures/Wallpapers/Void.jpeg" \
  --transition-type grow \
  --transition-pos 0.5,0.5 \
  --transition-step 90 \
  --transition-fps 60

#!/bin/sh

./vstream | ffmpeg -y -r 25 -f image2pipe -c:v png -i - vid.mp4

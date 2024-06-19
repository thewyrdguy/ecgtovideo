#!/bin/sh

./ecgtovideo | ffmpeg -y -r 25 -f image2pipe -c:v png -i - -an vid.mp4

#!/bin/sh

./ecgtovideo -r 30 | ffmpeg -y -r 30 -f image2pipe -c:v png -i - -an vid.mp4

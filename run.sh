#!/bin/sh

`dirname $0`/ecgtovideo -r 30 | ffmpeg -y -r 30 -f image2pipe -c:v png -i - -r 30 -an vid.mp4

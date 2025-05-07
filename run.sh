#!/bin/sh

if [ ! -r "$1" ]; then
       echo "no file $1"
       exit 1
fi

vid=`echo "$1" | sed -e 's/txt$/mp4/'`

echo "converting $1 to $vid"

`dirname $0`/ecgtovideo -r 30 < "$1" | ffmpeg -y -r 30 -f image2pipe -c:v png -i - -r 30 -an "$vid"

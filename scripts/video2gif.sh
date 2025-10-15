#!/bin/bash
# https://superuser.com/questions/1049606/reduce-generated-gif-size-using-ffmpeg

vid="$1"
fps=25           # frames per a second .
filename=$(basename -- "$vid")
filename="${filename%.*}"

filters="fps=$fps"

ffmpeg -i  "$vid"                                  \
       -vf "$filters,palettegen"                   \
       -y  palette.png                             &&
ffmpeg -i  "$vid"                                  \
       -i  palette.png                                \
       -lavfi "$filters [x]; [x][1:v] paletteuse"  \
       -y  "$filename".gif                              &&
rm palette.png

# Weirdly, generating a palette tripled the size of the GIF, although it greatly increased the quality over the default palette. I found the best way is to generate the GIF with ffmpeg as per usual (possibly with a better palette as in this answer) then just run it through an optimization tool (there's ones online too) that can make unchanging parts of the frame transparent or apply other optimizations. https://superuser.com/questions/1049606/reduce-generated-gif-size-using-ffmpeg#comment1636162_1049820

# I tried `gifsicle -0 -03 input.gif -o output.gif` but that shrink from 14M to 13M.
# ezgif.com shrinked to 9M, but probably scaled to smaller image.
# Probably I could use smaller resolution with `ffmped -s 800x600`.

# to convert webm to mp4 `ffmpeg -i input.webm -qscale 0 output.mp4`.
# https://stackoverflow.com/questions/12659383/ffmpeg-convert-webm-to-mp4-not-working/12659449#12659449

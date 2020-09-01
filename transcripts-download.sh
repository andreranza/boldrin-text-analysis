#!/bin/bash

# looping through the links to get the video transcript from Michele Boldrin YT channel

links=($(cut -f 2 -d , links.csv | grep youtube))

for link in ${links[@]}; do
	echo $link
	youtube-dl --write-auto-sub --sub-lang=it --skip-download $link
done
echo $?
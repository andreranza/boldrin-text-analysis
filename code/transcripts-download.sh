#!/bin/bash

# looping through the links to get the video transcript from Michele Boldrin YT channel
# when running this script, reiderect the ouput in transcripts/output/download-output.txt 
# with the -o option.

links=($(cut -f 2 -d , ../data/links.csv | grep youtube))

for link in ${links[@]}; do
	echo $link
	youtube-dl --write-auto-sub --sub-lang=it --skip-download $link 
done
echo $?
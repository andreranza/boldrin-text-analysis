#!/bin/bash

youtube-dl --write-auto-sub --sub-lang=it --skip-download -a ../data/links.txt > output/download-output.txt

echo $?
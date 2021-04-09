#!/bin/bash
# set -x

if [[ $# -ne 1 ]]; then
	echo "Usage: $0 <extractor file format>"
	echo "Extractor file format is:"
	echo "one entry per line, format is 'filename: <lineno start>,<lineno end> <#optional comment>'"
	exit 1
fi

IFS=$'\n'
for line in $(cat $1); do
	fname=$(echo $line | cut -f1 -d ':')
	range=$(echo $line | cut -f2 -d ' ')
	sed -n "${range}p" $fname
done

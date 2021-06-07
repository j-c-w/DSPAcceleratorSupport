#!/bin/bash
# set -x

if [[ $# -ne 1 ]]; then
	echo "Usage: $0 <extractor file format>"
	echo "Extractor file format is:"
	echo "one entry per line, format is 'filename: <lineno start>,<lineno end> <#optional comment>'"
	echo "or: INSERT: contents"
	exit 1
fi

IFS=$'\n'
for line in $(cat $1); do
	fname=$(echo $line | cut -f1 -d ':')
	range=$(echo $line | cut -f2 -d ' ')
	if [[ $fname == *INSERT* ]]; then
		contents=$(echo $line | cut -f2- -d:)
		echo "$contents"
	else
		sed -n "${range}p" $fname
	fi
done

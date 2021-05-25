#!/bin/bash

# Assumes that the inputs have been generated and
# things have been built.

files=$(find -name value_profiler)

for profiler in ${files[@]}; do
	pushd $(dirname $profiler)
	# Create outputs
	rm -rf value_profiles
	mkdir -p value_profiles

	# Get teh geneated input files.
	ifiles=$(find . -name "*.json")

	for file in ${ifiles[@]}; do
		eval ./$(basename $profiler) $file value_profiles/$file
	done
	popd
done

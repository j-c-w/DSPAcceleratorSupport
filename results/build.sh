#!/bin/bash
set -eu

# Generates all the inputs for every benchmark
makefiles=( $(find -name Makefile) )
# Only testing powers of two right now.

for file in "${makefiles[@]}"; do
	echo "Building $file"
	dname=$(dirname $file)
	execname=$(basename $file)

	pushd $dname
	make
	popd
done

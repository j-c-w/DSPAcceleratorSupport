#!/bin/bash
set -eu

# Generates all the inputs for every benchmark
generators=( $(find -name generate_inputs.sh) )
# Only testing powers of two right now.
MAX=10

for file in "${generators[@]}"; do
	dname=$(dirname $file)
	execname=$(basename $file)

	pushd $dname
	# Clear any old inputs
	rm -rf inputs
	for length in $(seq 1 $MAX); do
		echo $length
		eval ./$execname $(( 2**length ))
	done
	popd
done

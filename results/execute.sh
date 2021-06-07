#!/bin/bash

set -eu
set -x

if [[ $# -lt 1 ]]; then
	echo "Usage: $0 <accelerator executable name (e.g. accelerated_fftw)> [optional bmark name]"
	exit 1
fi

TIMES=2
if [[ $# -gt 1 ]]; then
	cd $2
fi
files=$(find -name $1)

for file in ${files[@]}; do
	execname=$(basename $file)

	pushd $(dirname $file)
	# Get all the input file sizes/names:
	pushd inputs
	ifiles=$(find . -name "*.json")
	popd
	# Make the new results folder.
	rfolder=${1}_results
	rm -rf $rfolder
	results=$(mkdir -p $rfolder)

	# Make the results folder ofr the original scripts
	orig_res_folder=${1}_orig_results
	rm -rf $orig_res_folder
	results=$(mkdir -p $orig_res_folder)
	echo "Results in $rfolder/${file}_out"
	echo "Time spent in accelerator in $rfolder/${file}_accelerator_time_out"

	for file in ${ifiles[@]}; do
		for t in $(seq 1 $TIMES); do
			# Get the time out from the executable.
			eval ./$execname inputs/$file > tmpout
			cat tmpout | grep -e '^Time:' | cut -d":" -f2- >> $rfolder/${file}_out
			cat tmpout | grep -e "^AccTime:" | cut -d":" -f2 >> $rfolder/${file}_acctime_out
		done

		for t in $(seq 1 $TIMES); do
			# Same thing but for the origingal
			eval ./original inputs/$file > tmpout
			cat tmpout | grep -e '^Time:' | cut -d":" -f2- >> $orig_res_folder/${file}_out
			cat tmpout | grep -e "^AccTime:" | cut -d":" -f2 >> $orig_res_folder/${file}_acctime_out
		done
	done
	popd
done

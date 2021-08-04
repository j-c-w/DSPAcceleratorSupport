#!/bin/bash
set -eu

# How many times to execute the FFT loop within the benchmark
export TIMES_IN_BENCHMARK=1000000

# Generates all the inputs for every benchmark
makefiles=( $(find -name Makefile) )
# Only testing powers of two right now.

parallel '(pushd $(dirname {}); make)' ::: "${makefiles[@]}"

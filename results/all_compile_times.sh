#!/bin/bash

# FFTW
./compile_time.sh ../benchmarks/Github/code/FINAL_LIST ../benchmarks/Accelerators/x86CompileSettings.json ../benchmarks/Accelerators/FFTW/FFTW_apispec.json
mv compile_times/compile_times compile_times/FFTW

# FFTA
./compile_time.sh ../benchmarks/Github/code/FINAL_LIST ../benchmarks/Accelerators/SHARCCompileSettings.json ../benchmarks/Accelerators/FFTA/FFTA_apispec.json
mv compile_times/compile_times compile_times/FFTA

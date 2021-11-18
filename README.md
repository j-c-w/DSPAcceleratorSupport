# FACC

Requirements: Nix. (see https://nixos.org/download.html)

Building:
- In the top-level directory, run `nix-shell`.  This should fetch all the dependencies.
- Build FACC: cd synth; make
- Build the FACC libraries: cd synth/libs/clib/; make
- [optional] Build the evaluation executables: cd benchmarks/; ./make.sh

Run FACC using main.byte, e.g. ./main.byte <compile settings file> <io specification> <api specification>.  examples of all three exist in the benchmarks directory.

Licensing:
All code in the synth/ directory is licensed under the Apache 2.0 license.

All code in benchmarks/ or results/ is licensed as it is in the originating repositories (specified for each).

Structure:
The high level structre of this project is:
synth: contains all the code to implement FACC, and external libraries to support synthesized programs.
benchmarks: contains target information (SHARC FFTA, NXP PowerQuad, FFTW, and TI FFTC) and benchmarks taken from Github and testsuites.  Input files for FACC come from here.
results/: contains outputs from runs of FACC on existing benchmarks and plotting scripts.

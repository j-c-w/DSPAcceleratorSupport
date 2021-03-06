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

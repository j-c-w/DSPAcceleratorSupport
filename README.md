# FACC

Requirements: Nix. (see https://nixos.org/download.html)

Building:
- In the top-level directory, run `nix-shell`.  This should fetch all the dependencies.
- Build FACC: cd synth; make
- Build the FACC libraries: cd synth/libs/clib/; make
- [optional] Build the evaluation executables: cd benchmarks/; ./make.sh

#Getting Weird Build Errors from OCaml where it looks like it can't find things?
Nix seems to do a poor job of hiding the system version of Ocaml.
You should uninstall whatever system version of ocaml is kicking around
(so that running ocaml/ocamlopt outside of nix-shell doesn't find anything).

#Running
Run FACC using main.byte, e.g. ./main.byte <compile settings file> <io specification> <api specification>.  examples of all three exist in the benchmarks directory.

Licensing:
All code in the synth/ directory is licensed under the Apache 2.0 license.

All code in benchmarks/ or results/ is licensed as it is in the originating repositories (specified for each).

Structure:
The high level structre of this project is:
synth: contains all the code to implement FACC, and external libraries to support synthesized programs.
benchmarks: contains target information (SHARC FFTA, NXP PowerQuad, FFTW, and TI FFTC) and benchmarks taken from Github and testsuites.  Input files for FACC come from here.
results/: contains outputs from runs of FACC on existing benchmarks and plotting scripts.

# Debugging:
If an example program is not working as expected, there are a few steps to debug:

1. See if FACC is generating the right candidates for it --- these are in synthethizer_temps by default --- look to see if the candidate you are looking for exists.  If it does, the --only-test <N> flag is helpful to debug behaviour on this candidate only.
2. If it is, check that your wrappers are correct for the program (the json_gen.byte program can help with this) --- you may also need value profiling that is not correctly setup
3. if it is not, check that range constraints are correct.


{pkgs ? import<nixos-22.11> {}}:

with pkgs;
mkShell {
	buildInputs = [
		bash ncurses
		cloc ocaml gdb valgrind splint linuxPackages-libre.perf gnumake ctags bc
		# Ocaml packages
		ocamlPackages.findlib ocamlPackages.ocamlbuild ocamlPackages.yojson
		ocamlPackages.core_kernel opam ocamlPackages.cmdliner
		ocamlPackages.ppx_expect ocamlPackages.parmap
		ocamlPackages.alcotest ocamlPackages.core
		ocamlPackages.mtime
		# C++ deps for the synnthesizer
		gcc10 nlohmann_json clang
		# Other indirect deps for the synthesizer.
		parallel
		# Deps for tests
		fftw fftwFloat blas mkl oneDNN
		# Deps for helper scripts
		python310 python310Packages.numpy python310Packages.matplotlib
		# Deps for other experiments
		(callPackage ./decard.nix {})
	];
	# Disable leak sanitizer
	LSAN_OPTIONS="detect_leaks=0";
	# Enable ocaml stack traces.
	OCAMLRUNPARAM = "b";
	SHELL_NAME = "FFTSynth";
	shellHook = ''
			# Don't use rosette
			# raco pkg install rosette
		'';
}

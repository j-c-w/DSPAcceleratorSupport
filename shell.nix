{pkgs ? import<nixpkgs> {}}:

with pkgs;
mkShell {
	buildInputs = [
		cloc ocaml gdb racket
		# Ocaml packages
		ocamlPackages.findlib ocamlPackages.ocamlbuild ocamlPackages.yojson
		ocamlPackages.core_kernel opam ocamlPackages.cmdliner
		ocamlPackages.ppx_expect ocamlPackages.parmap
		ocamlPackages.alcotest
		# C++ deps for the synnthesizer
		gcc10 nlohmann_json
		# Other indirect deps for the synthesizer.
		parallel
		# Deps for tests
		fftw
		# Deps for helper scripts
		python38 python38Packages.numpy
	];
	# Enable ocaml stack traces.
	OCAMLRUNPARAM = "b";
	SHELL_NAME = "FFTSynth";
	shellHook = ''
			raco pkg install rosette
		'';
}

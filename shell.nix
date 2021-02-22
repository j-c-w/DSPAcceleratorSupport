{pkgs ? import<nixpkgs> {}}:

with pkgs;
mkShell {
	buildInputs = [
		cloc ocaml
		# Ocaml packages
		ocamlPackages.findlib ocamlPackages.ocamlbuild ocamlPackages.yojson
		ocamlPackages.core_kernel opam ocamlPackages.cmdliner
		ocamlPackages.ppx_expect ocamlPackages.parmap
		# C++ deps for the synnthesizer
		gcc nlohmann_json
		# Other indirect deps for the synthesizer.
		parallel
	];
	# Enable ocaml stack traces.
	OCAMLRUNPARAM = "b";
	SHELL_NAME = "FFTSynth";
}

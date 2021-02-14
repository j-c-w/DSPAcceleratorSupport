{pkgs ? import<nixpkgs> {}}:

with pkgs;
mkShell {
	buildInputs = [ ocaml ocamlPackages.findlib ocamlPackages.ocamlbuild ocamlPackages.yojson ocamlPackages.core_kernel opam ocamlPackages.cmdliner cloc ocamlPackages.ppx_expect ];
	SHELL_NAME = "FFTSynth";
}

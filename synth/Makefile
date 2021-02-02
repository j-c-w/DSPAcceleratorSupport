all:
	ocamlbuild -just-plugin
	ocamlbuild main.native -pkg core_kernel -pkg yojson -pkg cmdliner

clean:
	rm -rf _build
	rm main.native

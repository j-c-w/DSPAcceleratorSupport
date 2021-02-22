all:
	ocamlbuild -just-plugin
	ocamlbuild main.native -pkg core_kernel -pkg yojson -pkg cmdliner -pkg parmap -cflag -g

clean:
	rm -rf _build
	rm main.native

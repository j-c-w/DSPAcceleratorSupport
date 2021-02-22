FLAGS = -pkg core_kernel -pkg yojson -pkg cmdliner -pkg parmap -cflag -g

all: main json_gen

plugin:
	ocamlbuild -just-plugin

main: plugin
	ocamlbuild main.native $(FLAGS)

json_gen: plugin
	ocamlbuild json_gen.native $(FLAGS)

clean:
	rm -rf _build
	rm main.native

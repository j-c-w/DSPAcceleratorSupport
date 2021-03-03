FLAGS = -pkg core_kernel -pkg yojson -pkg cmdliner -pkg parmap -pkg str -cflag -g -lflag -g

all: main json_gen

plugin:
	ocamlbuild -just-plugin

main: plugin
	ocamlbuild main.byte $(FLAGS)

json_gen: plugin
	ocamlbuild json_gen.native $(FLAGS)

clean:
	rm -rf _build
	rm main.native

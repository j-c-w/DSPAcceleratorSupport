FLAGS = -pkg core_kernel -pkg yojson -pkg cmdliner -pkg parmap -pkg str -cflag -g -lflag -g -r
INCLUDES = -I utilities/

all: main utilities

plugin:
	ocamlbuild -just-plugin

main: plugin
	ocamlbuild main.byte $(FLAGS)

utilities: plugin
	ocamlbuild utilities/json_gen.native $(FLAGS)
	ocamlbuild utilities/json_compare.native $(FLAGS)

clean:
	rm -rf _build
	rm main.native

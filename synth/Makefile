FLAGS = -pkg core_kernel -pkg yojson -pkg cmdliner -pkg parmap -pkg str -cflag -g -lflag -g -r
INCLUDES = -I utilities/
PARALLELISM = 4

all: main utilities

plugin:
	ocamlbuild -just-plugin

main: plugin
	ocamlbuild -j $(PARALLELISM) main.byte $(FLAGS)

utilities: plugin
	ocamlbuild -j $(PARALLELISM) utilities/json_gen.native $(FLAGS)
	ocamlbuild -j $(PARALLELISM) utilities/json_compare.native $(FLAGS)

clean:
	rm -rf _build
	rm main.native

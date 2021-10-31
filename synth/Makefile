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
	ocamlbuild -j $(PARALLELISM) utilities/injectivity_comparison.native $(FLAGS)
	ocamlbuild -j $(PARALLELISM) utilities/io_gen.native $(FLAGS)
	ocamlbuild -j $(PARALLELISM) utilities/constant_gen.native $(FLAGS)

test: plugin
	ocamlbuild -j $(PARALLELISM) tests/main_test.byte $(FLAGS) -pkg alcotest
	./main_test.byte

clean:
	rm -rf _build
	rm main.native
	rm main.byte

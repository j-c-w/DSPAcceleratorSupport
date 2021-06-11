TIMES_IN_BENCHMARK := $(shell v='$(TIMES_IN_BENCHMARK)'; echo "$${v%.*}")

all:
	g++ -DTIMES=${TIMES_IN_BENCHMARK} -Ofast -I ../../../synth/libs -g -fsanitize=address in_context_original.cpp -o original
	g++ -DTIMES=${TIMES_IN_BENCHMARK} -Wall -O0 -g -I ../../../benchmarks/Github/code/marton78_pffft/ -I ../../../synth/libs value_profiler.cpp -o value_profiler -fsanitize=undefined -fstack-protector -fsanitize=address
	g++ -lfftw3f -DTIMES=${TIMES_IN_BENCHMARK} -Ofast accelerated_fftw.cpp -L /usr/lib/x86_64-linux-gnu -I ../../../synth/libs -o accelerated_fftw

all:
	g++ -Ofast -lfftw3f -I ../../../synth/libs accelerated_fftw.cpp -o accelerated_fftw
	g++ -Ofast -I ../../../synth/libs ../../../benchmarks/Github/code/mborgerding_kissfft/wrapper.cpp -o original

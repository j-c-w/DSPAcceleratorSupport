all:
	g++ -lfftw3f -Ofast accelerated_fftw.cpp -L /usr/lib/x86_64-linux-gnu -I ../../../synth/libs -o accelerated_fftw
	g++ -Ofast -I ../../../synth/libs ../../../benchmarks/Github/code/cpuimage_FFTResampler/wrapper.cpp -o original

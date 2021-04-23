all:
	g++ -Ofast accelerated_fftw.cpp ~/Downloads/fftw-3.3.9/.libs/libfftw3.a -L /usr/lib/x86_64-linux-gnu -I ../../../synth/libs -o accelerated_fftw
	g++ -Ofast -I ../../../synth/libs ../../../benchmarks/Github/code/xiahouzouxin_fft/wrapper.cpp -o original

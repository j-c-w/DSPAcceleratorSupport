all:
	g++ -Ofast -I ../../../synth/libs ../../../benchmarks/Github/code/fakufaku_esp32-fft/wrapper.cpp -o original
	g++ -g -I ../../../benchmarks/Github/code/fakufaku_esp32-fft/ -I ../../../synth/libs value_profiler.cpp -o value_profiler
	g++ -Ofast accelerated_fftw.cpp -L /usr/lib/x86_64-linux-gnu -I ../../../synth/libs -o accelerated_fftw

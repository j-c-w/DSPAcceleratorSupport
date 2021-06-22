#!/bin/bash

python3 plot_ffta.py "fft_only" UnAcceleratedResults AcceleratedResults Github/cpuimage_FFTResampler/ffta_results Github/JodiTheTigger_meow_fft/ffta_results Github/marton78_pffft/ffta_results Github/mborgerding_kissfft/ffta_results Github/xiahouzouxin_fft/ffta_results
# DFT projects last: Github/jtfell_c-fft/ffta_results Github/liscio_fft/ffta_results 
python3 plot_ffta.py "all" UnAcceleratedResults AcceleratedResults Github/cpuimage_FFTResampler/ffta_results Github/JodiTheTigger_meow_fft/ffta_results Github/marton78_pffft/ffta_results Github/mborgerding_kissfft/ffta_results Github/xiahouzouxin_fft/ffta_results Github/jtfell_c-fft/ffta_results Github/liscio_fft/ffta_results 

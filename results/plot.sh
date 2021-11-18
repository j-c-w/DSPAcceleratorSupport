# Biggest --- require a log axis.
python plot_joint_graph.py all Github/gregfjohnson_fft Github/liscio_fft Github/jtfell_c-fft

# Next biggest, up to about 60x speedup.
python plot_joint_graph.py slow Github/cpuimage_FFTResampler/  Github/xiahouzouxin_fft/ Github/cpuimage_cpuFFT/ Github/xbarin02_uFFT/ Github/xbarin02_uFFT_2/

python plot_joint_graph.py faster Github/tasimon_FFT/ Github/mozanunal_SimpleDSP/ Github/marton78_pffft/ Github/dlinyj_fft/ Github/JodiTheTigger_meow_fft/ Github/cpuimage_StockhamFFT/ Github/akw0088_fft/ Github/mborgerding_kissfft/

# python plot_joint_graph.py fast_only Github/cpuimage_cpuFFT Github/cpuimage_FFTResampler Github/cpuimage_StockhamFFT Github/dlinyj_fft Github/JodiTheTigger_meow_fft Github/marton78_pffft Github/mborgerding_kissfft Github/mozanunal_SimpleDSP Github/tasimon_FFT Github/xbarin02_uFFT Github/xbarin02_uFFT_2 Github/xiahouzouxin_fft

# Plot the bar chart idl/programl/
python plot_success_comps.py CompileStatistics.dat

python plot_idl_graph.py ../benchmarks/Github/code/FFT_IDL_Check "Pattern 0"

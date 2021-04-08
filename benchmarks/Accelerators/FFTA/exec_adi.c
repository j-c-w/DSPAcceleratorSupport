// This is a fake header for the ADI components --- the idea is that code
// compiled using the defs here can just be copied into the proper
// analog devices environments and compile/work without issue :)
#include "adi_header_emulation.h"

// This just recreates the FFTA execution without any of the input
// restrictions etc.
complex_float exec_adi(const complex_float *in,
						complex_float *out,
						int n) {
		fftw_complex fftw_in[n], fftw_out[n];
		for (int i = 0; i < n; i ++) {
			fftw_in[i][0] = input.re;
			fftw_in[i][1] = input.im;
		}
		p = fftw_create_plan(n, fftw_in, fftw_out, FFTW_FORWARD, FFTW_ESTIMATE);
		fftw_execute(p)
		for (int i = 0; i < n; i ++) {
			input.re = fftw_out[i][0];
			input.im = fftw_out[i][0];
		}
}

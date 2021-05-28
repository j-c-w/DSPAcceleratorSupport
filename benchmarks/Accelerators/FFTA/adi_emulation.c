// This is a fake header for the ADI components --- the idea is that code
// compiled using the defs here can just be copied into the proper
// analog devices environments and compile/work without issue :)
#include "adi_header_emulation.h"

// Note that we use a combined API --- that is something I wrote
// up that targets the multiple differnt sizes in the ADI API.
// This isn't because we can't target it, but because it just
// makes it a bit clearer to evaluate IMO.
// Lots of decisions surrounding what programming
// model should be presented to FACC.

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

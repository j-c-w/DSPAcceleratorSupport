#include<fftw3.h>
#include<complex>
#include<iostream>

class _complex_double_ {
	public:
		double re;
		double im;

		_complex_double_ (float re, float im): re(re), im(im) {

		}
		_complex_double_ () {}

		void print() {
			std::cout << "CPX type is re:" << re << ", imag:" << im << std::endl;
		}
};

// simple call to FFTW --- we don't handle double array generation yet, but it has the same memory
// layout as this struct so that seems OK to just treat
// as the struct.
void fftw_example_api(_complex_double_ *api_in, _complex_double_ *api_out, int interface_len, int dir) {
	fftw_plan p;
	p = fftw_plan_dft_1d(interface_len, reinterpret_cast<fftw_complex*>(api_in), reinterpret_cast<fftw_complex*>(api_out), dir, FFTW_ESTIMATE);
	std::cout << "Starting execution!" << std::endl;;
    fftw_execute(p); /* repeat as needed */
	std::cout << "Finished execution" << std::endl;;
    fftw_destroy_plan(p);
}

/* Orignal skeleton is: 
Pre: SKELETON:

With the array index wrappers adi_acc_output,Annon
And (fromvars) []
Under dimensions [adi_acc_n = n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_n
And (fromvars) [n]
Under dimensions []
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,re
And (fromvars) [input, real]
Under dimensions [adi_acc_n = n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,im
And (fromvars) [input, imag]
Under dimensions [adi_acc_n = n]
With conversion function IdentityConversion
Post: SKELETON:

With the array index wrappers output,imag
And (fromvars) [adi_acc_output, im]
Under dimensions [n = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers output,real
And (fromvars) [adi_acc_output, re]
Under dimensions [n = adi_acc_n]
With conversion function IdentityConversion
*/


extern "C" {
#include "../../../benchmarks/Github/code/cpuimage_FFTResampler/self_contained_code.c"
}



extern "C" {
#include "../../../benchmarks/Accelerators/FFTA/adi_emulation.c"
}



#include<vector>
#include<nlohmann/json.hpp>
#include<fstream>
#include<iomanip>
#include<clib/synthesizer.h>
#include<time.h>
#include<iostream>
char *output_file; 
char *pre_accel_dump_file; // optional dump file. 
using json = nlohmann::json;
const char* __asan_default_options() { return "detect_leaks=0"; }


clock_t AcceleratorStart;
clock_t AcceleratorTotalNanos = 0;
void StartAcceleratorTimer() {
	AcceleratorStart = clock();
}

void StopAcceleratorTimer() {
	AcceleratorTotalNanos +=
		(clock()) - AcceleratorStart;
}

void write_output(cmplx * input, cmplx * output, int n) {

    json output_json;
std::vector<json> output_temp_33;
for (unsigned int i34 = 0; i34 < n; i34++) {
cmplx output_temp_35 = output[i34];
json output_temp_36;

output_temp_36["real"] = output_temp_35.real;

output_temp_36["imag"] = output_temp_35.imag;
output_temp_33.push_back(output_temp_36);
}
output_json["output"] = output_temp_33;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

void STB_FFT_accel_internal(cmplx * input,cmplx * output,int n) {
int adi_acc_n;;
	adi_acc_n = n;;
	complex_float adi_acc_output[adi_acc_n] __attribute((aligned(32)));;
	complex_float adi_acc_input[adi_acc_n] __attribute((aligned(32)));;
	for (int i7 = 0; i7 < adi_acc_n; i7++) {
		adi_acc_input[i7].re = input[i7].real;
	};
	for (int i8 = 0; i8 < adi_acc_n; i8++) {
		adi_acc_input[i8].im = input[i8].imag;
	};
	
if ((PRIM_EQUAL(adi_acc_n, 524288)) || ((PRIM_EQUAL(adi_acc_n, 262144)) || ((PRIM_EQUAL(adi_acc_n, 131072)) || ((PRIM_EQUAL(adi_acc_n, 65536)) || ((PRIM_EQUAL(adi_acc_n, 32768)) || ((PRIM_EQUAL(adi_acc_n, 16384)) || ((PRIM_EQUAL(adi_acc_n, 8192)) || ((PRIM_EQUAL(adi_acc_n, 4096)) || ((PRIM_EQUAL(adi_acc_n, 2048)) || ((PRIM_EQUAL(adi_acc_n, 1024)) || ((PRIM_EQUAL(adi_acc_n, 512)) || ((PRIM_EQUAL(adi_acc_n, 256)) || ((PRIM_EQUAL(adi_acc_n, 128)) || ((PRIM_EQUAL(adi_acc_n, 64)) || ((PRIM_EQUAL(adi_acc_n, 32)) || ((PRIM_EQUAL(adi_acc_n, 16)) || ((PRIM_EQUAL(adi_acc_n, 8)) || ((PRIM_EQUAL(adi_acc_n, 4)) || ((PRIM_EQUAL(adi_acc_n, 2)) || (PRIM_EQUAL(adi_acc_n, 1))))))))))))))))))))) {
StartAcceleratorTimer();;
	accel_cfft_wrapper(adi_acc_input, adi_acc_output, adi_acc_n);;
	StopAcceleratorTimer();;
	for (int i9 = 0; i9 < n; i9++) {
		output[i9].imag = adi_acc_output[i9].im;
	};
	for (int i10 = 0; i10 < n; i10++) {
		output[i10].real = adi_acc_output[i10].re;
	}
} else {
STB_FFT(input, output, n);
}
}
void STB_FFT_accel(cmplx * input, cmplx * output, int n) {
STB_FFT_accel_internal((cmplx *) input, (cmplx *) output, (int) n);
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<cmplx> input_vec;
for (auto& elem : input_json["input"]) {
float input_innerreal = elem["real"];
float input_innerimag = elem["imag"];
cmplx input_inner = { input_innerreal, input_innerimag};
input_vec.push_back(input_inner);
}
cmplx *input = &input_vec[0];
int n = input_json["n"];
cmplx output[n];
clock_t begin = clock();
for (int i = 0; i < TIMES; i ++) {
	STB_FFT_accel(input, output, n);
}
clock_t end = clock();
std::cout << "Time: " << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << "AccTime: " << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;
write_output(input, output, n);
}

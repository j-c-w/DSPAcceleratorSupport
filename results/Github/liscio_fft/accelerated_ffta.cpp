/* Orignal skeleton is: 
Pre: SKELETON:

With the array index wrappers adi_acc_output,Annon
And (fromvars) []
Under dimensions [adi_acc_n = N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_n
And (fromvars) [N]
Under dimensions []
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,re
And (fromvars) [x, im]
Under dimensions [adi_acc_n = N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,im
And (fromvars) [x, re]
Under dimensions [adi_acc_n = N]
With conversion function IdentityConversion
Post: SKELETON:

With the array index wrappers returnvar,Annon
And (fromvars) []
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers returnvar,im
And (fromvars) [adi_acc_output, re]
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers returnvar,re
And (fromvars) [adi_acc_output, im]
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion
*/


extern "C" {
#include "../../../benchmarks/Github/code/liscio_fft/self_contained_code.h"
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

void write_output(_complex_double_ * x, int N, _complex_double_ * returnvar) {

    json output_json;
std::vector<json> output_temp_33;
for (unsigned int i34 = 0; i34 < N; i34++) {
_complex_double_ output_temp_35 = returnvar[i34];
json output_temp_36;

output_temp_36["re"] = output_temp_35.re;

output_temp_36["im"] = output_temp_35.im;
output_temp_33.push_back(output_temp_36);
}
output_json["returnvar"] = output_temp_33;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

_complex_double_ * FFT_wrapper_accel_internal(_complex_double_ * x,int N) {
int adi_acc_n;;
	adi_acc_n = N;;
	complex_float adi_acc_output[adi_acc_n] __attribute((aligned(32)));;
	complex_float adi_acc_input[adi_acc_n] __attribute((aligned(32)));;
	for (int i14 = 0; i14 < adi_acc_n; i14++) {
		adi_acc_input[i14].re = x[i14].im;
	};
	for (int i15 = 0; i15 < adi_acc_n; i15++) {
		adi_acc_input[i15].im = x[i15].re;
	};
	
if ((PRIM_EQUAL(adi_acc_n, 524288)) || ((PRIM_EQUAL(adi_acc_n, 262144)) || ((PRIM_EQUAL(adi_acc_n, 131072)) || ((PRIM_EQUAL(adi_acc_n, 65536)) || ((PRIM_EQUAL(adi_acc_n, 32768)) || ((PRIM_EQUAL(adi_acc_n, 16384)) || ((PRIM_EQUAL(adi_acc_n, 8192)) || ((PRIM_EQUAL(adi_acc_n, 4096)) || ((PRIM_EQUAL(adi_acc_n, 2048)) || ((PRIM_EQUAL(adi_acc_n, 1024)) || ((PRIM_EQUAL(adi_acc_n, 512)) || ((PRIM_EQUAL(adi_acc_n, 256)) || ((PRIM_EQUAL(adi_acc_n, 128)) || ((PRIM_EQUAL(adi_acc_n, 64)) || ((PRIM_EQUAL(adi_acc_n, 32)) || ((PRIM_EQUAL(adi_acc_n, 16)) || ((PRIM_EQUAL(adi_acc_n, 8)) || ((PRIM_EQUAL(adi_acc_n, 4)) || ((PRIM_EQUAL(adi_acc_n, 2)) || (PRIM_EQUAL(adi_acc_n, 1))))))))))))))))))))) {
accel_cfft_wrapper(adi_acc_input, adi_acc_output, adi_acc_n);;
	_complex_double_* returnvar = (_complex_double_*) malloc (sizeof(_complex_double_)*N);;;
	for (int i17 = 0; i17 < N; i17++) {
		returnvar[i17].im = adi_acc_output[i17].re;
	};
	for (int i18 = 0; i18 < N; i18++) {
		returnvar[i18].re = adi_acc_output[i18].im;
	};
	
return returnvar;
} else {

return FFT_wrapper(x, N);;
}
}
_complex_double_ * FFT_wrapper_accel(_complex_double_ * x, int N) {
return (_complex_double_ *)FFT_wrapper_accel_internal((_complex_double_ *) x, (int) N);
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<_complex_double_> x_vec;
for (auto& elem : input_json["x"]) {
double x_innerre = elem["re"];
double x_innerim = elem["im"];
_complex_double_ x_inner = { x_innerre, x_innerim};
x_vec.push_back(x_inner);
}
_complex_double_ *x = &x_vec[0];
int N = input_json["N"];

for (int i = 0; i < TIMES; i ++) {
	_complex_double_ * returnvar = FFT_wrapper_accel(x, N);
}

write_output(x, N, returnvar);
}

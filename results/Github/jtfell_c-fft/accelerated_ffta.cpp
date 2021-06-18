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
And (fromvars) [x, re]
Under dimensions [adi_acc_n = N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,im
And (fromvars) [x, im]
Under dimensions [adi_acc_n = N]
With conversion function IdentityConversion
Post: SKELETON:

With the array index wrappers returnv,Annon
And (fromvars) []
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers returnv,im
And (fromvars) [adi_acc_output, im]
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers returnv,re
And (fromvars) [adi_acc_output, re]
Under dimensions [N = adi_acc_n]
With conversion function IdentityConversion
*/


extern "C" {
#include "../../benchmarks/Github/code/jtfell_c-fft/self_contained_code.c"
}



extern "C" {
#include "../../benchmarks/Accelerators/FFTA/adi_emulation.c"
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

void write_output(complex * x, int N, complex * returnv) {

    json output_json;
std::vector<json> output_temp_33;
for (unsigned int i34 = 0; i34 < N; i34++) {
complex output_temp_35 = returnv[i34];
json output_temp_36;

output_temp_36["re"] = output_temp_35.re;

output_temp_36["im"] = output_temp_35.im;
output_temp_33.push_back(output_temp_36);
}
output_json["returnv"] = output_temp_33;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

complex * DFT_naive_accel_internal(complex * x,int N) {
int adi_acc_n;;
	adi_acc_n = N;;
	complex_float adi_acc_output[adi_acc_n] __attribute((aligned(32)));;
	complex_float adi_acc_input[adi_acc_n] __attribute((aligned(32)));;
	for (int i8 = 0; i8 < adi_acc_n; i8++) {
		adi_acc_input[i8].re = x[i8].re;
	};
	for (int i9 = 0; i9 < adi_acc_n; i9++) {
		adi_acc_input[i9].im = x[i9].im;
	};
	
if ((PRIM_EQUAL(adi_acc_n, 524288)) || ((PRIM_EQUAL(adi_acc_n, 262144)) || ((PRIM_EQUAL(adi_acc_n, 131072)) || ((PRIM_EQUAL(adi_acc_n, 65536)) || ((PRIM_EQUAL(adi_acc_n, 32768)) || ((PRIM_EQUAL(adi_acc_n, 16384)) || ((PRIM_EQUAL(adi_acc_n, 8192)) || ((PRIM_EQUAL(adi_acc_n, 4096)) || ((PRIM_EQUAL(adi_acc_n, 2048)) || ((PRIM_EQUAL(adi_acc_n, 1024)) || ((PRIM_EQUAL(adi_acc_n, 512)) || ((PRIM_EQUAL(adi_acc_n, 256)) || ((PRIM_EQUAL(adi_acc_n, 128)) || ((PRIM_EQUAL(adi_acc_n, 64)) || ((PRIM_EQUAL(adi_acc_n, 32)) || ((PRIM_EQUAL(adi_acc_n, 16)) || ((PRIM_EQUAL(adi_acc_n, 8)) || ((PRIM_EQUAL(adi_acc_n, 4)) || ((PRIM_EQUAL(adi_acc_n, 2)) || (PRIM_EQUAL(adi_acc_n, 1))))))))))))))))))))) {
StartAcceleratorTimer();;
	accel_cfft_wrapper(adi_acc_input, adi_acc_output, adi_acc_n);;
	StopAcceleratorTimer();;
	complex* returnv = (complex*) malloc (sizeof(complex)*N);;;
	for (int i11 = 0; i11 < N; i11++) {
		returnv[i11].im = adi_acc_output[i11].im;
	};
	for (int i12 = 0; i12 < N; i12++) {
		returnv[i12].re = adi_acc_output[i12].re;
	};
	
return returnv;
} else {

return DFT_naive(x, N);;
}
}
complex * DFT_naive_accel(complex * x, int N) {
return (complex *)DFT_naive_accel_internal((complex *) x, (int) N);
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<complex> x_vec;
for (auto& elem : input_json["x"]) {
double x_innerre = elem["re"];
double x_innerim = elem["im"];
complex x_inner = { x_innerre, x_innerim};
x_vec.push_back(x_inner);
}
complex *x = &x_vec[0];
int N = input_json["N"];
clock_t begin = clock();
complex * returnv = DFT_naive_accel(x, N);
clock_t end = clock();
std::cout << "Time: " << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << "AccTime: " << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;
write_output(x, N, returnv);
}
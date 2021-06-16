/* Orignal skeleton is: 
Pre: SKELETON:

With the array index wrappers adi_acc_output,Annon
And (fromvars) []
Under dimensions [adi_acc_n = data.N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_n
And (fromvars) [data.N]
Under dimensions []
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,re
And (fromvars) [in, r]
Under dimensions [adi_acc_n = data.N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,im
And (fromvars) [in, j]
Under dimensions [adi_acc_n = data.N]
With conversion function IdentityConversion
Post: SKELETON:

With the array index wrappers out,j
And (fromvars) [adi_acc_output, im]
Under dimensions [data.N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers out,r
And (fromvars) [adi_acc_output, re]
Under dimensions [data.N = adi_acc_n]
With conversion function IdentityConversion
*/


extern "C" {
#include "../../../benchmarks/Github/code/JodiTheTigger_meow_fft/self_contained_code.c"
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

void write_output(Meow_FFT_Workset * data, Meow_FFT_Complex * in, Meow_FFT_Complex * out) {

    json output_json;
std::vector<json> output_temp_133;
for (unsigned int i134 = 0; i134 < data->N; i134++) {
Meow_FFT_Complex output_temp_135 = out[i134];
json output_temp_136;

output_temp_136["r"] = output_temp_135.r;

output_temp_136["j"] = output_temp_135.j;
output_temp_133.push_back(output_temp_136);
}
output_json["out"] = output_temp_133;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

void meow_fft_accel_internal(Meow_FFT_Workset * data,Meow_FFT_Complex * in,Meow_FFT_Complex * out) {
int adi_acc_n;;
	adi_acc_n = data->N;;
	complex_float adi_acc_output[adi_acc_n] __attribute((aligned(32)));;
	complex_float adi_acc_input[adi_acc_n] __attribute((aligned(32)));;
	for (int i47 = 0; i47 < adi_acc_n; i47++) {
		adi_acc_input[i47].re = in[i47].r;
	};
	for (int i48 = 0; i48 < adi_acc_n; i48++) {
		adi_acc_input[i48].im = in[i48].j;
	};
	
if ((PRIM_EQUAL(adi_acc_n, 524288)) || ((PRIM_EQUAL(adi_acc_n, 262144)) || ((PRIM_EQUAL(adi_acc_n, 131072)) || ((PRIM_EQUAL(adi_acc_n, 65536)) || ((PRIM_EQUAL(adi_acc_n, 32768)) || ((PRIM_EQUAL(adi_acc_n, 16384)) || ((PRIM_EQUAL(adi_acc_n, 8192)) || ((PRIM_EQUAL(adi_acc_n, 4096)) || ((PRIM_EQUAL(adi_acc_n, 2048)) || ((PRIM_EQUAL(adi_acc_n, 1024)) || ((PRIM_EQUAL(adi_acc_n, 512)) || ((PRIM_EQUAL(adi_acc_n, 256)) || ((PRIM_EQUAL(adi_acc_n, 128)) || ((PRIM_EQUAL(adi_acc_n, 64)) || ((PRIM_EQUAL(adi_acc_n, 32)) || ((PRIM_EQUAL(adi_acc_n, 16)) || ((PRIM_EQUAL(adi_acc_n, 8)) || ((PRIM_EQUAL(adi_acc_n, 4)) || ((PRIM_EQUAL(adi_acc_n, 2)) || (PRIM_EQUAL(adi_acc_n, 1))))))))))))))))))))) {
StartAcceleratorTimer();;
	accel_cfft_wrapper(adi_acc_input, adi_acc_output, adi_acc_n);;
	StopAcceleratorTimer();;
	for (int i49 = 0; i49 < data->N; i49++) {
		out[i49].j = adi_acc_output[i49].im;
	};
	for (int i50 = 0; i50 < data->N; i50++) {
		out[i50].r = adi_acc_output[i50].re;
	}
} else {
meow_fft(data, in, out);
}
}
void meow_fft_accel(Meow_FFT_Workset * data, Meow_FFT_Complex * in, Meow_FFT_Complex * out) {
meow_fft_accel_internal((Meow_FFT_Workset *) data, (Meow_FFT_Complex *) in, (Meow_FFT_Complex *) out);
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<Meow_FFT_Complex> in_vec;
for (auto& elem : input_json["in"]) {
float in_innerr = elem["r"];
float in_innerj = elem["j"];
Meow_FFT_Complex in_inner = { in_innerr, in_innerj};
in_vec.push_back(in_inner);
}
Meow_FFT_Complex *in = &in_vec[0];
size_t workset_bytes = meow_fft_generate_workset(n, NULL);
Meow_FFT_Workset* data =
	(Meow_FFT_Workset *) malloc(workset_bytes);
meow_fft_generate_workset(n, data);
Meow_FFT_Complex out[data->N];
clock_t begin = clock();
for (int i = 0; i < TIMES; i ++) {
	meow_fft_accel(data, in, out);
}
clock_t end = clock();
std::cout << "Time: " << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << "AccTime: " << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;
write_output(data, in, out);
}

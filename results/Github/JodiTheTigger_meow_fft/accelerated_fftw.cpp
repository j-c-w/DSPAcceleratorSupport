
#include "../../benchmarks/Github/code/JodiTheTigger_meow_fft/self_contained_code.c"


#include "../../benchmarks/Accelerators/FFTW/interface.hpp"
#include "complex"


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
std::vector<json> output_temp_405;
for (unsigned int i406 = 0; i406 < data->N; i406++) {
Meow_FFT_Complex output_temp_407 = out[i406];
json output_temp_408;

output_temp_408["r"] = output_temp_407.r;

output_temp_408["j"] = output_temp_407.j;
output_temp_405.push_back(output_temp_408);
}
output_json["out"] = output_temp_405;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

void meow_fft_accel_internal(Meow_FFT_Workset * data,Meow_FFT_Complex * in,Meow_FFT_Complex * out) {
short dir;;
	dir = -1;;
	int interface_len;;
	interface_len = data->N;;
	_complex_float_ api_out[interface_len];;
	_complex_float_ api_in[interface_len];;
	for (int i132 = 0; i132 < interface_len; i132++) {
		api_in[i132].re = in[i132].r;
	};
	for (int i133 = 0; i133 < interface_len; i133++) {
		api_in[i133].im = in[i133].j;
	};
	
if ((PRIM_EQUAL(dir, -1)) && ((PRIM_EQUAL(interface_len, 524288)) || ((PRIM_EQUAL(interface_len, 262144)) || ((PRIM_EQUAL(interface_len, 131072)) || ((PRIM_EQUAL(interface_len, 65536)) || ((PRIM_EQUAL(interface_len, 32768)) || ((PRIM_EQUAL(interface_len, 16384)) || ((PRIM_EQUAL(interface_len, 8192)) || ((PRIM_EQUAL(interface_len, 4096)) || ((PRIM_EQUAL(interface_len, 2048)) || ((PRIM_EQUAL(interface_len, 1024)) || ((PRIM_EQUAL(interface_len, 512)) || ((PRIM_EQUAL(interface_len, 256)) || ((PRIM_EQUAL(interface_len, 128)) || ((PRIM_EQUAL(interface_len, 64)) || ((PRIM_EQUAL(interface_len, 32)) || ((PRIM_EQUAL(interface_len, 16)) || ((PRIM_EQUAL(interface_len, 8)) || ((PRIM_EQUAL(interface_len, 4)) || ((PRIM_EQUAL(interface_len, 2)) || (PRIM_EQUAL(interface_len, 1)))))))))))))))))))))) {
StartAcceleratorTimer();;
	fftwf_example_api(api_in, api_out, interface_len, dir);;
	StopAcceleratorTimer();;
	for (int i134 = 0; i134 < data->N; i134++) {
		out[i134].r = api_out[i134].re;
	};
	for (int i135 = 0; i135 < data->N; i135++) {
		out[i135].j = api_out[i135].im;
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
int data_pointerN = input_json["data"]["N"];
std::vector<Meow_FFT_Complex> data_pointerwn_vec;
for (auto& elem : input_json["data"]["wn"]) {
float data_pointerwn_innerr = elem["r"];
float data_pointerwn_innerj = elem["j"];
Meow_FFT_Complex data_pointerwn_inner = { data_pointerwn_innerr, data_pointerwn_innerj};
data_pointerwn_vec.push_back(data_pointerwn_inner);
}
Meow_FFT_Complex *data_pointerwn = &data_pointerwn_vec[0];
std::vector<Meow_FFT_Complex> data_pointerwn_ordered_vec;
for (auto& elem : input_json["data"]["wn_ordered"]) {
float data_pointerwn_ordered_innerr = elem["r"];
float data_pointerwn_ordered_innerj = elem["j"];
Meow_FFT_Complex data_pointerwn_ordered_inner = { data_pointerwn_ordered_innerr, data_pointerwn_ordered_innerj};
data_pointerwn_ordered_vec.push_back(data_pointerwn_ordered_inner);
}
Meow_FFT_Complex *data_pointerwn_ordered = &data_pointerwn_ordered_vec[0];
unsigned int data_pointerstagescount = input_json["data"]["stages"]["count"];
std::vector<unsigned int> data_pointerstagesradix_vec;
for (auto& elem : input_json["data"]["stages"]["radix"]) {
unsigned int data_pointerstagesradix_inner = elem;
data_pointerstagesradix_vec.push_back(data_pointerstagesradix_inner);
}
unsigned int *data_pointerstagesradix = &data_pointerstagesradix_vec[0];
std::vector<unsigned int> data_pointerstagesremainder_vec;
for (auto& elem : input_json["data"]["stages"]["remainder"]) {
unsigned int data_pointerstagesremainder_inner = elem;
data_pointerstagesremainder_vec.push_back(data_pointerstagesremainder_inner);
}
unsigned int *data_pointerstagesremainder = &data_pointerstagesremainder_vec[0];
std::vector<unsigned int> data_pointerstagesoffsets_vec;
for (auto& elem : input_json["data"]["stages"]["offsets"]) {
unsigned int data_pointerstagesoffsets_inner = elem;
data_pointerstagesoffsets_vec.push_back(data_pointerstagesoffsets_inner);
}
unsigned int *data_pointerstagesoffsets = &data_pointerstagesoffsets_vec[0];
Meow_Fft_Stages data_pointerstages = { data_pointerstagescount, data_pointerstagesradix, data_pointerstagesremainder, data_pointerstagesoffsets};
Meow_FFT_Workset data_pointer = { data_pointerN, data_pointerwn, data_pointerwn_ordered, data_pointerstages};
Meow_FFT_Workset* data = &data_pointer;
std::vector<Meow_FFT_Complex> in_vec;
for (auto& elem : input_json["in"]) {
float in_innerr = elem["r"];
float in_innerj = elem["j"];
Meow_FFT_Complex in_inner = { in_innerr, in_innerj};
in_vec.push_back(in_inner);
}
Meow_FFT_Complex *in = &in_vec[0];
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

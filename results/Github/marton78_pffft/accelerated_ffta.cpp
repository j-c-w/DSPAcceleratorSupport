/* Orignal skeleton is: 
Pre: SKELETON:

With the array index wrappers adi_acc_output,Annon
And (fromvars) []
Under dimensions [adi_acc_n = setup.N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_n
And (fromvars) [setup.N]
Under dimensions []
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,re
And (fromvars) [input, f32_1]
Under dimensions [adi_acc_n = setup.N]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers adi_acc_input,im
And (fromvars) [input, f32_2]
Under dimensions [adi_acc_n = setup.N]
With conversion function IdentityConversion
Post: SKELETON:

With the array index wrappers output,f32_1
And (fromvars) [adi_acc_output, re]
Under dimensions [setup.N = adi_acc_n]
With conversion function IdentityConversion

>(new binding): 

With the array index wrappers output,f32_2
And (fromvars) [adi_acc_output, im]
Under dimensions [setup.N = adi_acc_n]
With conversion function IdentityConversion
*/


extern "C" {
#include "../../../benchmarks/Github/code/marton78_pffft/self_contained_code.c"
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

void write_output(PFFFT_Setup_Desugar * setup, float * input, float * output, float * work, int direction) {

    json output_json;
std::vector<json> output_temp_197;
for (unsigned int i198 = 0; i198 < setup->N; i198++) {
float output_temp_199 = output[i198];

output_temp_197.push_back(output_temp_199);
}
output_json["output"] = output_temp_197;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

void desugared_transform_ordered_accel_internal(PFFFT_Setup_Desugar * setup,facc_2xf32_t * input,facc_2xf32_t * output,facc_2xf32_t * work,int direction) {
int adi_acc_n;;
	adi_acc_n = setup->N;;
	complex_float adi_acc_output[adi_acc_n] __attribute((aligned(32)));;
	complex_float adi_acc_input[adi_acc_n] __attribute((aligned(32)));;
	for (int i82 = 0; i82 < adi_acc_n; i82++) {
		adi_acc_input[i82].re = input[i82].f32_1;
	};
	for (int i83 = 0; i83 < adi_acc_n; i83++) {
		adi_acc_input[i83].im = input[i83].f32_2;
	};
	
if ((PRIM_EQUAL(adi_acc_n, 524288)) || ((PRIM_EQUAL(adi_acc_n, 262144)) || ((PRIM_EQUAL(adi_acc_n, 131072)) || ((PRIM_EQUAL(adi_acc_n, 65536)) || ((PRIM_EQUAL(adi_acc_n, 32768)) || ((PRIM_EQUAL(adi_acc_n, 16384)) || ((PRIM_EQUAL(adi_acc_n, 8192)) || ((PRIM_EQUAL(adi_acc_n, 4096)) || ((PRIM_EQUAL(adi_acc_n, 2048)) || ((PRIM_EQUAL(adi_acc_n, 1024)) || ((PRIM_EQUAL(adi_acc_n, 512)) || ((PRIM_EQUAL(adi_acc_n, 256)) || ((PRIM_EQUAL(adi_acc_n, 128)) || ((PRIM_EQUAL(adi_acc_n, 64)) || ((PRIM_EQUAL(adi_acc_n, 32)) || ((PRIM_EQUAL(adi_acc_n, 16)) || ((PRIM_EQUAL(adi_acc_n, 8)) || ((PRIM_EQUAL(adi_acc_n, 4)) || ((PRIM_EQUAL(adi_acc_n, 2)) || (PRIM_EQUAL(adi_acc_n, 1))))))))))))))))))))) {
StartAcceleratorTimer();;
	accel_cfft_wrapper(adi_acc_input, adi_acc_output, adi_acc_n);;
	StopAcceleratorTimer();;
	for (int i84 = 0; i84 < setup->N; i84++) {
		output[i84].f32_1 = adi_acc_output[i84].re;
	};
	for (int i85 = 0; i85 < setup->N; i85++) {
		output[i85].f32_2 = adi_acc_output[i85].im;
	}
} else {
desugared_transform_ordered(setup, (float *)input, (float *)output, (float *)work, direction);
}
}
void desugared_transform_ordered_accel(PFFFT_Setup_Desugar * setup, float * input, float * output, float * work, int direction) {
desugared_transform_ordered_accel_internal((PFFFT_Setup_Desugar *) setup, (facc_2xf32_t *) input, (facc_2xf32_t *) output, (facc_2xf32_t *) work, (int) direction);
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<float> input_vec;
for (auto& elem : input_json["input"]) {
float input_inner = elem;
input_vec.push_back(input_inner);
}
float *input = &input_vec[0];
float work[n];
float output[n];
n /= 2;

clock_t begin = clock();
PFFFT_Setup *setup = pffft_new_setup(n, PFFFT_COMPLEX);

pffft_direction_t direction = PFFFT_FORWARD;
// This is just to achieve a compiler-internal representation
// of the struct without enums.
PFFFT_Setup_Desugar dsetup;
desugar_setup(setup, &dsetup);
clock_t begin = clock();
for (int i = 0; i < TIMES; i ++) {
	desugared_transform_ordered_accel(&dsetup, input, output, work, direction);
}
clock_t end = clock();
std::cout << "Time: " << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << "AccTime: " << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;
write_output(setup, input, output, work, direction);
}

#include<vector>
#include<nlohmann/json.hpp>
#include<fstream>
#include<iomanip>
#include<clib/synthesizer.h>
#include<time.h>
#include<iostream>
extern "C" {
#include "self_contained_code.c"
}
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

void write_output(COMPLEX * array, int n, COMPLEX * array) {

    json output_json;
std::vector<json> output_temp_1;
for (unsigned int i2 = 0; i2 < n; i2++) {
COMPLEX output_temp_3 = array[i2];
json output_temp_4;

output_temp_4["re"] = output_temp_3.re;

output_temp_4["im"] = output_temp_3.im;
output_temp_1.push_back(output_temp_4);
}
output_json["array"] = output_temp_1;
std::vector<json> output_temp_5;
for (unsigned int i6 = 0; i6 < n; i6++) {
COMPLEX output_temp_7 = array[i6];
json output_temp_8;

output_temp_8["re"] = output_temp_7.re;

output_temp_8["im"] = output_temp_7.im;
output_temp_5.push_back(output_temp_8);
}
output_json["array"] = output_temp_5;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}
int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<COMPLEX> array_vec;
for (auto& elem : input_json["array"]) {
float array_innerre = elem["re"];
float array_innerim = elem["im"];
COMPLEX array_inner = { array_innerre, array_innerim};
array_vec.push_back(array_inner);
}
COMPLEX *array = &array_vec[0];
int n = input_json["n"];
clock_t begin = clock();
COMPLEX *out;
for (int i = 0; i < TIMES; i ++) {
	out = fft_1d(array, n);
	// don't free since out == array.
}
clock_t end = clock();
std::cout << "Time: " << (double) (end - begin) / CLOCKS_PER_SEC << std::endl;
std::cout << "AccTime: " << (double) AcceleratorTotalNanos / CLOCKS_PER_SEC << std::endl;
write_output(array, n, array);
}

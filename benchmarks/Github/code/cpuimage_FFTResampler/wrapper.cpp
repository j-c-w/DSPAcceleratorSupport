#include<vector>
#include<nlohmann/json.hpp>
#include<fstream>
#include<iomanip>
#include<clib/synthesizer.h>
#include<chrono>
#include<iostream>
extern "C" {
#include "self_contained_code.c"
}
char *output_file; 
char *pre_accel_dump_file; // optional dump file. 
using json = nlohmann::json;
void write_output(cmplx * input, cmplx * output, int n) {

    json output_json;
std::vector<json> output_temp_1;
for (unsigned int i2 = 0; i2 < n; i2++) {
cmplx output_temp_3 = output[i2];
json output_temp_4;

output_temp_4["real"] = output_temp_3.real;

output_temp_4["imag"] = output_temp_3.imag;
output_temp_1.push_back(output_temp_4);
}
output_json["output"] = output_temp_1;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
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
std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
STB_FFT(input, output, n);
std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
std::cout << "Time: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin).count() << std::endl;
write_output(input, output, n);
}

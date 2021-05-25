// This is a modified version of the wrapper
// that value-profiles the benchmark suite
// in a couple of key parameters using
// a typical usecase pattern for the library.
#include<vector>
#include<nlohmann/json.hpp>
#include<fstream>
#include<iomanip>
#include<clib/synthesizer.h>
#include<chrono>
#include<iostream>
extern "C" {
#include "context_code.c"
}
char *output_file; 
char *pre_accel_dump_file; // optional dump file. 
using json = nlohmann::json;
void write_values(float * twiddle_factors, int n) {
    json output_json;
std::vector<json> output_temp_1;
for (unsigned int i2 = 0; i2 < 2 * n; i2++) {
float output_temp_3 = twiddle_factors[i2];

output_temp_1.push_back(output_temp_3);
}
std::cout << "Loop done" << std::endl;
output_json["twiddle_factors"] = output_temp_1;
output_json["n"] = n;
std::cout << "Writing" << std::endl;
std::ofstream out_str(output_file); 
std::cout << "Output created" << std::endl;
out_str << std::setw(4) << output_json << std::endl;
std::cout << "Writen" << std::endl;
}

int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 
	std::string inpname_str(inpname);
	std::cout<< inpname << std::endl;;

    std::ifstream ifs(inpname_str); 
    json input_json = json::parse(ifs);
std::vector<float> input_vec;
for (auto& elem : input_json["input"]) {
float input_inner = elem;
input_vec.push_back(input_inner);
}
float *input = &input_vec[0];
int n = input_json["n"];
float output[n];
std::cout << "Read inputs" << std::endl;
fft_config_t *config = fft_init(n, FFT_COMPLEX, FFT_FORWARD,  input, output);
std::cout << "created config" << config << std::endl;
std::cout << "created twiddles" << config->twiddle_factors << std::endl;
fft(input, output, config->twiddle_factors, n);
std::cout << "Computed fft" << std::endl;
write_values(config->twiddle_factors, n);
}

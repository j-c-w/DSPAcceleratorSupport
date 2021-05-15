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
void write_output(complex * x, int N) {

    json output_json;
std::vector<json> output_temp_1;
for (unsigned int i2 = 0; i2 < N; i2++) {
complex output_temp_3 = x[i2];
json output_temp_4;

output_temp_4["re"] = output_temp_3.re;

output_temp_4["im"] = output_temp_3.im;
output_temp_1.push_back(output_temp_4);
}
output_json["x"] = output_temp_1;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
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
std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
DFT_naive(x, N);
std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
std::cout << "Time: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin).count() << std::endl;
write_output(x, N);
}

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
void write_output(Meow_FFT_Workset * data, Meow_FFT_Complex * in, Meow_FFT_Complex * out) {

    json output_json;
std::vector<json> output_temp_1;
for (unsigned int i2 = 0; i2 < data->N; i2++) {
Meow_FFT_Complex output_temp_3 = out[i2];
json output_temp_4;

output_temp_4["r"] = output_temp_3.r;

output_temp_4["j"] = output_temp_3.j;
output_temp_1.push_back(output_temp_4);
}
output_json["out"] = output_temp_1;
std::ofstream out_str(output_file); 
out_str << std::setw(4) << output_json << std::endl;
}

int main(int argc, char **argv) {
    char *inpname = argv[1]; 
    output_file = argv[2]; 

    std::ifstream ifs(inpname); 
    json input_json = json::parse(ifs);
std::vector<Meow_FFT_Workset> data_vec;
for (auto& elem : input_json["data"]) {
int data_innerN = elem["N"];
std::vector<Meow_FFT_Complex> data_innerwn_vec;
for (auto& elem : elem["wn"]) {
float data_innerwn_innerr = elem["r"];
float data_innerwn_innerj = elem["j"];
Meow_FFT_Complex data_innerwn_inner = { data_innerwn_innerr, data_innerwn_innerj};
data_innerwn_vec.push_back(data_innerwn_inner);
}
Meow_FFT_Complex *data_innerwn = &data_innerwn_vec[0];
std::vector<Meow_FFT_Complex> data_innerwn_ordered_vec;
for (auto& elem : elem["wn_ordered"]) {
float data_innerwn_ordered_innerr = elem["r"];
float data_innerwn_ordered_innerj = elem["j"];
Meow_FFT_Complex data_innerwn_ordered_inner = { data_innerwn_ordered_innerr, data_innerwn_ordered_innerj};
data_innerwn_ordered_vec.push_back(data_innerwn_ordered_inner);
}
Meow_FFT_Complex *data_innerwn_ordered = &data_innerwn_ordered_vec[0];
unsigned int data_innerstagescount = elem["stages"]["count"];
std::vector<unsigned int> data_innerstagesradix_vec;
for (auto& elem : elem["stages"]["radix"]) {
unsigned int data_innerstagesradix_inner = elem;
data_innerstagesradix_vec.push_back(data_innerstagesradix_inner);
}
unsigned int *data_innerstagesradix = &data_innerstagesradix_vec[0];
std::vector<unsigned int> data_innerstagesremainder_vec;
for (auto& elem : elem["stages"]["remainder"]) {
unsigned int data_innerstagesremainder_inner = elem;
data_innerstagesremainder_vec.push_back(data_innerstagesremainder_inner);
}
unsigned int *data_innerstagesremainder = &data_innerstagesremainder_vec[0];
std::vector<unsigned int> data_innerstagesoffsets_vec;
for (auto& elem : elem["stages"]["offsets"]) {
unsigned int data_innerstagesoffsets_inner = elem;
data_innerstagesoffsets_vec.push_back(data_innerstagesoffsets_inner);
}
unsigned int *data_innerstagesoffsets = &data_innerstagesoffsets_vec[0];
Meow_Fft_Stages data_innerstages = { data_innerstagescount, data_innerstagesradix, data_innerstagesremainder, data_innerstagesoffsets};
Meow_FFT_Workset data_inner = { data_innerN, data_innerwn, data_innerwn_ordered, data_innerstages};
data_vec.push_back(data_inner);
}
Meow_FFT_Workset *data = &data_vec[0];
std::vector<Meow_FFT_Complex> in_vec;
for (auto& elem : input_json["in"]) {
float in_innerr = elem["r"];
float in_innerj = elem["j"];
Meow_FFT_Complex in_inner = { in_innerr, in_innerj};
in_vec.push_back(in_inner);
}
Meow_FFT_Complex *in = &in_vec[0];
Meow_FFT_Complex out[data->N];
std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
meow_fft(data, in, out);
std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
std::cout << "Time: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin).count() << std::endl;
write_output(data, in, out);
}

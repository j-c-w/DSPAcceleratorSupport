// A set of functions that are used by the synthesizer that
// I don't want to embed in OCaml.
// Not 100% clear that this is the best decision (as opposed
// to embedding implementations, since we could genericize
// the generation of those.

// Copied from
// https://www.geeksforgeeks.org/write-an-efficient-c-program-to-reverse-bits-of-a-number/
unsigned int reverseBits(unsigned int num)
{
    unsigned int  NO_OF_BITS = sizeof(num) * 8;
    unsigned int reverse_num = 0, i, temp;

    for (i = 0; i < NO_OF_BITS; i++)
    {
        temp = (num & (1 << i));
        if(temp)
            reverse_num |= (1 << ((NO_OF_BITS - 1) - i));
    }

    return reverse_num;
}

#define BIT_REVERSE(arr, len) 		   \
	for (int i = 0; i < len; i ++) {   \
		int reversed = reverseBits(i); \
		auto temp = arr[i];            \
		arr[i] = arr[reversed];        \
		arr[reversed] = temp;          \
	}


void inplace_bit_reversal_float(float *arr, int len) {
	BIT_REVERSE(arr, len)
}

void inplace_bit_reversal_double(double *arr, int len) {
	BIT_REVERSE(arr, len)
}

// TODO --- missing the other inplace_reversal functions.

#define ARRAY_NORM(arr, len) for (int i = 0; i < len; i ++) { arr[i] = arr[i] / len; }
#define ARRAY_DENORM(arr, len) for (int i = 0; i < len; i ++) { arr[i] = arr[i] * len; }

void inplace_normalize_float(float *arr, int len) {
	ARRAY_NORM(arr, len)
}

void inplace_normalize_double(double *arr, int len) {
	ARRAY_NORM(arr, len)
}

void inplace_denormalize_float(float *arr, int len) {
	ARRAY_DENORM(arr, len)
}

void inplace_denormalize_double(double *arr, int len) {
	ARRAY_DENORM(arr, len)
}

// A set of functions that are used by the synthesizer that
// I don't want to embed in OCaml.
// Not 100% clear that this is the best decision (as opposed
// to embedding implementations, since we could genericize
// the generation of those.

// Copied from
// https://www.geeksforgeeks.org/write-an-efficient-c-program-to-reverse-bits-of-a-number/
unsigned int reverseBits(unsigned int num, unsigned int no_bits);

// This doesn't use the post-index because it makes no sense
// to --- but it does make it semantically replacable
// with the other options here.
#define BIT_REVERSE(arr, postind, len) 		                 \
	for (unsigned int i = 0; i < len; i ++) {        \
		unsigned int reversed = reverseBits(i, len); \
		if (i < reversed) {                          \
			auto temp = arr[i];                      \
			arr[i] = arr[reversed];                  \
			arr[reversed] = temp;                    \
		}                                            \
	}

#define ARRAY_NORM(arr, postind, len) for (int i = 0; i < len; i ++) { arr[i]#postind = arr[i]#postind / len; }
#define ARRAY_DENORM(arr, postind, len) for (int i = 0; i < len; i ++) { arr[i]#postind = arr[i]#postind * len; }

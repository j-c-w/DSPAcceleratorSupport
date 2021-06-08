// See notes - this is to re-sugar the enum types without
// having to implement them in FACC>
// This is basically emulating what the compiler would do
// as it deconstructs the program.
typedef struct {
	int N;
	int Ncvec;
	int ifac1;
	int ifac2;
	int ifac3;
	int ifac4;
	int ifac5;
	int ifac6;
	int ifac7;
	int ifac8;
	int ifac9;
	int ifac10;
	int ifac11;
	int ifac12;
	int ifac13;
	int ifac14;
	int ifac15;
	int transform;
	v4sf *data;
	float *e;
	float *twiddle;
} PFFFT_Setup_Desugar;

void desugared_transform_ordered(PFFFT_Setup_Desugar *setup, const float *input, float *output, float *work, int direction) {
	struct PFFFT_Setup setup_struct = {
		setup->N, setup->Ncvec,
		setup->ifac1,
		setup->ifac2,
		setup->ifac3,
		setup->ifac4,
		setup->ifac5,
		setup->ifac6,
		setup->ifac7,
		setup->ifac8,
		setup->ifac9,
		setup->ifac10,
		setup->ifac11,
		setup->ifac12,
		setup->ifac13,
		setup->ifac14,
		setup->ifac15,
		(setup->transform == 0 ? PFFFT_REAL : PFFFT_COMPLEX),
		setup->data, setup->e, setup->twiddle
	};

	pffft_transform_ordered(&setup_struct, input, output, work, (direction == 0 ? PFFFT_FORWARD : PFFFT_BACKWARD));
}

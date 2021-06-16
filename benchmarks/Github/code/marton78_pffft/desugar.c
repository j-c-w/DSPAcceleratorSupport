// See notes - this is to re-sugar the enum types without
// having to implement them in FACC>
// This is basically emulating what the compiler would do
// as it deconstructs the program.
typedef struct {
	int N;
	int Ncvec;
	int *ifac;
	int transform;
	v4sf *data;
	float *e;
	float *twiddle;
} PFFFT_Setup_Desugar;

void desugared_transform_ordered(PFFFT_Setup_Desugar *setup, const float *input, float *output, float *work, int direction) {
	struct PFFFT_Setup setup_struct = {
		setup->N, setup->Ncvec,
		setup->ifac[1],
		setup->ifac[2],
		setup->ifac[3],
		setup->ifac[4],
		setup->ifac[5],
		setup->ifac[6],
		setup->ifac[7],
		setup->ifac[8],
		setup->ifac[9],
		setup->ifac[10],
		setup->ifac[11],
		setup->ifac[12],
		setup->ifac[13],
		setup->ifac[14],
		setup->ifac[15],
		(setup->transform == 0 ? PFFFT_REAL : PFFFT_COMPLEX),
		setup->data, setup->e, setup->twiddle
	};

	pffft_transform_ordered(&setup_struct, input, output, work, (direction == 0 ? PFFFT_FORWARD : PFFFT_BACKWARD));
}

void desugar_setup(PFFFT_Setup *orig, PFFFT_Setup_Desugar *new) {
	new->N = orig->N;
	new->Ncvec = orign->Ncvec;
	new->ifac = origin ->ifac;
	new->transform = (orig->transform == PFFFT_REAL ? 0 : 1);
	new->data = orig->data;
	new->e = orig->e;
	new->twiddle = orig->twiddle;
}

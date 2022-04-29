#include "biteopt/biteopt.h"

extern "C" int minimize( const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M = 1, const int attc = 10,
	const int stopc = 0, biteopt_rng rf = 0, void* rdata = 0 )
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

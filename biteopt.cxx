#include "biteopt/biteopt.h"

extern "C" int biteopt_minimize_wrapper(const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M, const int attc,
	const int stopc, biteopt_rng rf, void* rdata)
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

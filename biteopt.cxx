#include "biteopt/biteopt.h"

// TOOD: expose all parameters
// TODO: skip steps without position change

extern "C" CBiteOptMinimize* minimize_new(const int N, const biteopt_func f, const double* lb, const double* ub, const int M)
{
	CBiteOptMinimize* opt = new CBiteOptMinimize();
	opt->N = N;
	opt->f = f;
	opt->lb = lb;
	opt->ub = ub;

	opt->updateDims(N, M);

	return opt;
}
extern "C" void minimize_init(CBiteOptMinimize* opt, CBiteRnd* rnd)
{
	opt->init(*rnd);
}
extern "C" void minimize_step(CBiteOptMinimize* opt, CBiteRnd* rnd, double* x)
{
	opt->optimize(*rnd);

	memcpy(x, opt->getBestParams(), opt->N * sizeof(double));
}

extern "C" CBiteRnd* rng_new(const biteopt_rng rf)
{
	CBiteRnd* rnd = new CBiteRnd();

	rnd->init(1, rf);

	return rnd;
}

extern "C" int biteopt_minimize_wrapper(const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M, const int attc,
	const int stopc, biteopt_rng rf, void* rdata)
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

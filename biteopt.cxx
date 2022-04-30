#include <stdio.h>

#include "biteopt/biteopt.h"

// TODO: const modifiers?
// TOOD: expose all parameters
// TODO: skip steps without position change?

extern "C" CBiteRnd* rnd_new() { printf("rnd_new\n"); return new CBiteRnd(); }
extern "C" void rnd_free(CBiteRnd* rnd) { printf("rnd_free\n"); delete rnd; }
extern "C" void rnd_init(CBiteRnd* rnd, int seed, biteopt_rng rf, void* rdata) { rnd->init(0, rf, rdata); }

extern "C" CBiteOptMinimize* opt_new() { printf("opt_new\n"); return new CBiteOptMinimize(); }
extern "C" void opt_free(CBiteOptMinimize* opt) { printf("opt_free\n"); delete opt; }
extern "C" void opt_set(CBiteOptMinimize* opt, int N, biteopt_func f, void* data, double* lb, double* ub)
{
	opt->N = N;
	opt->f = f;
	opt->data = data;
	opt->lb = lb;
	opt->ub = ub;
}
extern "C" void opt_dims(CBiteOptMinimize* opt, int N, int M) { opt->updateDims(N, M); }
extern "C" void opt_init(CBiteOptMinimize* opt, CBiteRnd* rnd) { opt->init(*rnd); }
extern "C" int opt_step(CBiteOptMinimize* opt, CBiteRnd* rnd) { printf("opt_step\n"); return opt->optimize(*rnd); }
// TODO: can we just return getBestParams? it gets copied in haskell anyways
extern "C" void opt_best(CBiteOptMinimize* opt, double* x) { memcpy(x, opt->getBestParams(), opt->N * sizeof(double)); }

extern "C" int biteopt_minimize_wrapper(const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M, const int attc,
	const int stopc, biteopt_rng rf, void* rdata)
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

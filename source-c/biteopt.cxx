#include "biteopt.h"

extern "C" CBiteRnd* rnd_new() { return new CBiteRnd(); }
extern "C" void rnd_free(CBiteRnd* rnd) { delete rnd; }
extern "C" void rnd_init(CBiteRnd* rnd, int seed, biteopt_rng rf, void* rdata) { rnd->init(seed, rf, rdata); }

extern "C" CBiteOptMinimize* opt_new() { return new CBiteOptMinimize(); }
extern "C" void opt_free(CBiteOptMinimize* opt) { delete opt; }
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
extern "C" int opt_step(CBiteOptMinimize* opt, CBiteRnd* rnd) { return opt->optimize(*rnd); }
extern "C" const double* opt_best(CBiteOptMinimize* opt) { return opt->getBestParams(); }

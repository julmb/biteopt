#include "biteopt/biteopt.h"

// TODO: public inheritance?
// TODO: const modifiers?
class Minimizer : CBiteOptMinimize
{
	CBiteRnd rnd;

	public: Minimizer(int N, biteopt_func f, void* data, double* lb, double* ub, int M, biteopt_rng rf)
	{
		this->N = N;
		this->f = f;
		this->data = data;
		this->lb = lb;
		this->ub = ub;
		
		updateDims(N, M);

		rnd.init(0, rf);

		init(rnd);
	}

	public: int step() { return optimize(rnd); }
	public: void best(double* x) { memcpy(x, getBestParams(), N * sizeof(double)); }
};

// TOOD: expose all parameters
// TODO: skip steps without position change

extern "C" Minimizer* minimizer_new(int N, biteopt_func f, void* data, double* lb, double* ub, int M, biteopt_rng rf)
{
	return new Minimizer(N, f, data, lb, ub, M, rf);
}
extern "C" void minimizer_free(Minimizer* minimizer) { delete minimizer; }
extern "C" int minimizer_step(Minimizer* minimizer) { return minimizer->step(); }
extern "C" void minimizer_best(Minimizer* minimizer, double* x) { minimizer->best(x); }

extern "C" int biteopt_minimize_wrapper(const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M, const int attc,
	const int stopc, biteopt_rng rf, void* rdata)
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

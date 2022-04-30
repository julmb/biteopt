#include "biteopt/biteopt.h"

// TODO: public inheritance?
// TODO: const modifiers?
class Minimize : CBiteOptMinimize
{
	CBiteRnd rnd;

	public: Minimize(int N, biteopt_func f, double* lb, double* ub, int M, biteopt_rng rf)
	{
		this->N = N;
		this->f = f;
		this->data = NULL;
		this->lb = lb;
		this->ub = ub;
		
		updateDims(N, M);

		// TODO: try seed = 0
		rnd.init(1, rf);

		init(rnd);
	}

	public: int step() { return optimize(rnd); }
	public: void best(double* x) { memcpy(x, getBestParams(), N * sizeof(double)); }
};

// TOOD: expose all parameters
// TODO: skip steps without position change

extern "C" Minimize* minimize_new(int N, biteopt_func f, double* lb, double* ub, int M, biteopt_rng rf)
{
	return new Minimize(N, f, lb, ub, M, rf);
}
extern "C" int minimize_step(Minimize* minimize)
{
	return minimize->step();
}
extern "C" void minimize_best(Minimize* minimize, double* x)
{
	minimize->best(x);
}

extern "C" int biteopt_minimize_wrapper(const int N, biteopt_func f, void* data,
	const double* lb, const double* ub, double* x, double* minf,
	const int iter, const int M, const int attc,
	const int stopc, biteopt_rng rf, void* rdata)
{
	return biteopt_minimize(N, f, data, lb, ub, x, minf, iter, M, attc, stopc, rf, rdata);
}

// May (1976) logistic map  x_{n+1} = r * x_n * (1 - x_n).
// A pure floating-point recurrence exhibiting sensitive dependence on initial
// conditions (chaos for r ~ 3.57..4). Run for a runtime number of iterations
// so it stays a loop with a double-typed phi rather than unrolling.
extern "C" double logistic(double r, double x0, int iters) {
    double x = x0;
    for (int i = 0; i < iters; i++) {
        x = r * x * (1.0 - x);
    }
    return x;
}

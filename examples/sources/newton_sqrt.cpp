// Babylonian / Heron / Newton iteration for sqrt(a):
//   x_{n+1} = (x_n + a/x_n) / 2
// Implemented WITHOUT the llvm.sqrt intrinsic (so it stays inside the subset)
// and as a real loop over a runtime iteration count. A non-positive input is
// guarded, giving an early-return branch that exercises fcmp + control flow.
extern "C" double newton_sqrt(double a, int iters) {
    if (a <= 0.0) return 0.0;
    double x = a;                 // initial guess
    for (int i = 0; i < iters; i++) {
        x = 0.5 * (x + a / x);
    }
    return x;
}

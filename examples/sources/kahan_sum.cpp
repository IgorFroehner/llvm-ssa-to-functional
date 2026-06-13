// Kahan (1965) compensated summation of the harmonic series
//   sum_{k=1}^{n} 1/k.
// The running compensation term c recovers low-order bits otherwise lost to
// rounding, so the result is far more accurate than a naive accumulator.
// Exercises an int->double conversion (the (double) k cast) inside a loop;
// since k is provably >= 1, clang emits uitofp (not sitofp).
extern "C" double kahan_harmonic(int n) {
    double sum = 0.0;
    double c   = 0.0;            // running compensation
    for (int k = 1; k <= n; k++) {
        double y = 1.0 / (double) k - c;
        double t = sum + y;
        c   = (t - sum) - y;     // recovers the part of y lost in (sum + y)
        sum = t;
    }
    return sum;
}

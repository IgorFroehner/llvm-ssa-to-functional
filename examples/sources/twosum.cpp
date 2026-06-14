// Knuth / Moller TwoSum (an "error-free transformation"): for s = a + b it
// recovers the exact rounding error err such that a + b == s + err in exact
// arithmetic, using only floating additions/subtractions. Foundational
// primitive of compensated-arithmetic algorithms (cf. Kahan summation).
extern "C" double twosum_err(double a, double b) {
    double s   = a + b;
    double bb  = s - a;
    double err = (a - (s - bb)) + (b - bb);
    return err;
}

// Rump (1988): a rational/polynomial expression whose naive floating-point
// evaluation is catastrophically wrong. At (a, b) = (77617, 33096) the exact
// value is approx -0.827396, but float/double arithmetic gives wildly wrong
// results (often the wrong sign and magnitude) due to cancellation.
// Pure scalar +, -, *, / — no intrinsics.
extern "C" double rump(double a, double b) {
    double b2 = b * b;
    double b4 = b2 * b2;
    double b6 = b4 * b2;
    double b8 = b4 * b4;
    double a2 = a * a;
    return 333.75 * b6
         + a2 * (11.0 * a2 * b2 - b6 - 121.0 * b4 - 2.0)
         + 5.5 * b8
         + a / (2.0 * b);
}

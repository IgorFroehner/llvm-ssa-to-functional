// Muller's recurrence (J.-M. Muller, Elementary Functions):
//   E_{n+1} = 108 - (815 - 1500/E_{n-1}) / E_n,   E0 = 4, E1 = 4.25
// The exact limit is 5, but in floating point the iteration is drawn to the
// repulsive fixed point 100. A two-state (E_{n-1}, E_n) recurrence: exercises
// double-typed phi nodes carried around a loop.
extern "C" double muller(int n) {
    double prev = 4.0;     // E_{n-1}
    double cur  = 4.25;    // E_n
    for (int i = 0; i < n; i++) {
        double next = 108.0 - (815.0 - 1500.0 / prev) / cur;
        prev = cur;
        cur  = next;
    }
    return cur;
}

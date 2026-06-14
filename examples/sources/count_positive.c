// Each `> 0` is an i1; using the results as integers forces clang to emit
// `zext i1 ... to i32` before the adds -- the Bool->int boundary coercion that
// the i1-aware translation must reintroduce (docs/roadmap/10-boolean-types.md).
int count_positive(int a, int b, int c) { return (a > 0) + (b > 0) + (c > 0); }

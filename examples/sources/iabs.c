// abs(x) is lowered to the llvm.abs.i32 intrinsic at -O1.
int iabs(int x) { return x < 0 ? -x : x; }

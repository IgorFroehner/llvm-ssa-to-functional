// clang lowers these ternaries to llvm.smin.i32 / llvm.smax.i32 at -O1.
int imin(int a, int b) { return a < b ? a : b; }
int imax(int a, int b) { return a > b ? a : b; }
